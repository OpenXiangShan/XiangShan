/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._
import xiangshan._
import huancun.AliasKey
import xiangshan.cache._
import utils._


abstract class ICacheMissUnitModule(implicit p: Parameters) extends XSModule
  with HasICacheParameters

abstract class ICacheMissUnitBundle(implicit p: Parameters) extends XSBundle
  with HasICacheParameters

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle
{
    val paddr      = UInt(PAddrBits.W)
    val vaddr      = UInt(VAddrBits.W)
    val waymask   = UInt(nWays.W)
    val coh       = new ClientMetadata
    //val release   = ValidUndirectioned(new ReleaseReq)

    def getVirSetIdx = get_idx(vaddr)
    def getPhyTag    = get_phy_tag(paddr)
}


class ICacheMissResp(implicit p: Parameters) extends ICacheBundle
{
    val data     = UInt(blockBits.W)
}

class ICacheMissBundle(implicit p: Parameters) extends ICacheBundle{
    val req       =   Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp      =   Vec(2,ValidIO(new ICacheMissResp))
    val flush     =   Input(Bool())
}


class ICacheMissEntry(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends ICacheMissUnitModule
  with MemoryOpConstants
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Ceil(nMissEntries).W))

    val req = Flipped(DecoupledIO(new ICacheMissReq))
    val resp = ValidIO(new ICacheMissResp)

    //tilelink channel
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write = DecoupledIO(new ICacheDataWriteBundle)

//    val release    =  DecoupledIO(new ReleaseReq)
//    val victimInfor = Output(new ICacheVictimInfor())
//    val probeMerge  = Input(new ICacheVictimInfor)
//    val probeMergeFix = Output(Bool())

  })

  /** default value for control signals */
  io.resp := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  val s_idle :: s_send_release :: s_send_mem_aquire :: s_wait_mem_grant :: s_write_back :: s_send_grant_ack :: s_wait_resp :: Nil = Enum(7)
  val state = RegInit(s_idle)

  /** control logic transformation */
  //request register
  val req = Reg(new ICacheMissReq)
  val req_idx = req.getVirSetIdx //virtual index
  val req_tag = req.getPhyTag //physical tag
  val req_waymask = req.waymask

//  val victim_tag = get_phy_tag(req.release.bits.addr)
//  val victim_idx = req_idx

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

//  val needMergeProbe = (io.probeMerge.valid && state =/= s_idle && io.probeMerge.ptag === victim_tag && io.probeMerge.vidx === victim_idx)
//
//  //change release into a ProbeAck
//  //WARNING: no change to param, default TtoN
//  when(needMergeProbe && state === s_send_release){
//    io.release.bits.voluntary := false.B
//    io.release.bits.hasData   := true.B
//  }
//
//  io.probeMergeFix := needMergeProbe && state === s_send_release

  //cacheline register
  //refullCycles: 8 for 64-bit bus bus and 2 for 256-bit
  val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
  val respDataReg = Reg(Vec(refillCycles, UInt(beatBits.W)))

  //initial
  io.resp.bits := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare


  io.req.ready := (state === s_idle)
  io.mem_acquire.valid := (state === s_send_mem_aquire) //&& !io.flush

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire())
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))
  val is_grant = RegEnable(edge.isRequest(io.mem_grant.bits), io.mem_grant.fire())

  val neddSendAck = RegInit(false.B)

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.fire()) {
        readBeatCnt := 0.U
        state := s_send_mem_aquire
        req := io.req.bits
      }
    }

//    is(s_send_release){
//      when(io.release.fire()){
//        state := s_send_mem_aquire
//      }
//    }
    // memory request
    is(s_send_mem_aquire) {
      when(io.mem_acquire.fire()) {
        state := s_wait_mem_grant
      }
    }

    is(s_wait_mem_grant) {
      when(edge.hasData(io.mem_grant.bits)) {
        when(io.mem_grant.fire()) {
          readBeatCnt := readBeatCnt + 1.U
          respDataReg(readBeatCnt) := io.mem_grant.bits.data
          grant_param := io.mem_grant.bits.param
          when(readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
            neddSendAck := edge.isResponse(io.mem_grant.bits)
          }
        }
      }
    }

    is(s_write_back) {
      state := Mux(io.meta_write.fire() && io.data_write.fire(), Mux(neddSendAck, s_send_grant_ack, s_wait_resp), s_write_back)
    }

    is(s_send_grant_ack) {
      when(io.mem_finish.fire()) {
        state := s_wait_resp
      }
    }

    is(s_wait_resp) {
      io.resp.bits.data := respDataReg.asUInt
      when(io.resp.fire()) {
        state := s_idle
      }
    }
  }

  /** refill write and meta write */

  /** update coh meta */
  def missCohGen(param: UInt): UInt = {
    MuxLookup(param, Nothing, Seq(
      toB -> Branch,
      toT -> Trunk))
  }

  val miss_new_coh = ClientMetadata(missCohGen(grant_param))

  io.meta_write.valid := (state === s_write_back)
  io.meta_write.bits.generate(tag = req_tag, coh = miss_new_coh, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0))

  io.data_write.valid := (state === s_write_back)
  io.data_write.bits.generate(data = respDataReg.asUInt, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0))

//  io.release.valid := req.release.valid && (state === s_send_release)
//  io.release.bits  := req.release.bits
//
//  io.victimInfor.valid   := state =/= s_idle
//  io.victimInfor.ptag    := req_tag
//  io.victimInfor.vidx    := req_idx

  /** Tilelink request for next level cache/memory */
  val missCoh    = ClientMetadata(Nothing)
  val grow_param = missCoh.onAccess(M_XRD)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U,
    growPermissions = grow_param
  )._2
  io.mem_acquire.bits := acquireBlock
  // resolve cache alias by L2
  io.mem_acquire.bits.user.lift(AliasKey).foreach(_ := req.vaddr(13, 12))
  require(nSets <= 256) // icache size should not be more than 128KB

  /** Grant ACK */
  io.mem_finish.valid := (state === s_send_grant_ack) && is_grant
  io.mem_finish.bits := grantack

  //resp to ifu
  io.resp.valid := state === s_wait_resp

  XSPerfAccumulate(
    "entryPenalty" + Integer.toString(id, 10),
    BoolStopWatch(
      start = io.req.fire(),
      stop = io.resp.valid,
      startHighPriority = true)
  )
  XSPerfAccumulate("entryReq" + Integer.toString(id, 10), io.req.fire())

}


class ICacheMissUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheMissUnitModule
{
  val io = IO(new Bundle{
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, ValidIO(new ICacheMissResp))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = DecoupledIO(new TLBundleE(edge.bundle))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

//    val release     = DecoupledIO(new ReleaseReq)

//    val victimInfor = Vec(2, Output(new ICacheVictimInfor()))
//    val probeMerge  = Flipped(ValidIO(new ICacheVictimInfor))
  })
  // assign default values to output signals
  io.mem_grant.ready := false.B

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  PortNumber))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  PortNumber))
  //val release_arb    = Module(new Arbiter(new ReleaseReq,  PortNumber))

  io.mem_grant.ready := true.B

//  val probeMergeFix = VecInit(Seq.fill(2)(WireInit(false.B)))
//
//  val probeMerge = RegInit(0.U.asTypeOf(new ICacheVictimInfor))
//  when(io.probeMerge.valid){
//    probeMerge.ptag   := io.probeMerge.bits.ptag
//    probeMerge.vidx   := io.probeMerge.bits.vidx
//    probeMerge.valid  := true.B
//  }
//
//  when(probeMergeFix.reduce(_||_)){
//    probeMerge.valid := false.B
//  }

  val entries = (0 until 2) map { i =>
    val entry = Module(new ICacheMissEntry(edge, i))

    entry.io.id := i.U

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

//    io.victimInfor(i)  := entry.io.victimInfor
//    entry.io.probeMerge := probeMerge
//
//    probeMergeFix(i) := entry.io.probeMergeFix

    // entry resp
    meta_write_arb.io.in(i)     <>  entry.io.meta_write
    refill_arb.io.in(i)         <>  entry.io.data_write
    //release_arb.io.in(i)        <>  entry.io.release

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.resp(i) <> entry.io.resp

    XSPerfAccumulate(
      "entryPenalty" + Integer.toString(i, 10),
      BoolStopWatch(
        start = entry.io.req.fire(),
        stop = entry.io.resp.fire(),
        startHighPriority = true)
    )
    XSPerfAccumulate("entryReq" + Integer.toString(i, 10), entry.io.req.fire())

    entry
  }

  TLArbiter.lowest(edge, io.mem_acquire, entries.map(_.io.mem_acquire):_*)
  TLArbiter.lowest(edge, io.mem_finish,  entries.map(_.io.mem_finish):_*)

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out
  //io.release        <> release_arb.io.out

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_refill_way_" + Integer.toString(w, 10),  entries(0).io.meta_write.valid && OHToUInt(entries(0).io.meta_write.bits.waymask)  === w.U)
    XSPerfAccumulate("line_1_refill_way_" + Integer.toString(w, 10),  entries(1).io.meta_write.valid && OHToUInt(entries(1).io.meta_write.bits.waymask)  === w.U)
  }

}



