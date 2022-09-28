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
import huancun.{AliasKey, DirtyKey}
import xiangshan.cache._
import utils._
import difftest._


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

    def getVirSetIdx = get_idx(vaddr)
    def getPhyTag    = get_phy_tag(paddr)
}


class ICacheMissResp(implicit p: Parameters) extends ICacheBundle
{
    val data     = UInt(blockBits.W)
    val corrupt  = Bool()
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
    val id = Input(UInt(log2Ceil(PortNumber).W))

    val req = Flipped(DecoupledIO(new ICacheMissReq))
    val resp = ValidIO(new ICacheMissResp)

    //tilelink channel
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write = DecoupledIO(new ICacheDataWriteBundle)

    val toPrefetch    = ValidIO(UInt(PAddrBits.W))

  })

  /** default value for control signals */
  io.resp := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  val s_idle  :: s_send_mem_aquire :: s_wait_mem_grant :: s_write_back :: s_send_grant_ack :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)
  /** control logic transformation */
  //request register
  val req = Reg(new ICacheMissReq)
  val req_idx = req.getVirSetIdx //virtual index
  val req_tag = req.getPhyTag //physical tag
  val req_waymask = req.waymask
  val req_corrupt = RegInit(false.B)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  //cacheline register
  val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
  val respDataReg = Reg(Vec(refillCycles, UInt(beatBits.W)))

  //initial
  io.resp.bits := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  io.req.ready := (state === s_idle)
  io.mem_acquire.valid := (state === s_send_mem_aquire)

  io.toPrefetch.valid := (state =/= s_idle)
  io.toPrefetch.bits  :=  addrAlign(req.paddr, blockBytes, PAddrBits)

  val grantack = RegEnable(edge.GrantAck(io.mem_grant.bits), io.mem_grant.fire())
  val grant_param = Reg(UInt(TLPermissions.bdWidth.W))
  val is_dirty = RegInit(false.B)
  val is_grant = RegEnable(edge.isRequest(io.mem_grant.bits), io.mem_grant.fire())

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.fire()) {
        readBeatCnt := 0.U
        state := s_send_mem_aquire
        req := io.req.bits
      }
    }

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
          req_corrupt := io.mem_grant.bits.corrupt
          when(readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
          }
        }
      }
    }


    is(s_write_back) {
      state := Mux(io.meta_write.fire() && io.data_write.fire(), s_wait_resp, s_write_back)
    }

    is(s_wait_resp) {
      io.resp.bits.data := respDataReg.asUInt
      io.resp.bits.corrupt := req_corrupt
      when(io.resp.fire()) {
        state := s_idle
      }
    }
  }

  /** refill write and meta write */
  val missCoh    = ClientMetadata(Nothing)
  val grow_param = missCoh.onAccess(M_XRD)._2
  val acquireBlock = edge.AcquireBlock(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U,
    growPermissions = grow_param
  )._2

  val GetBlock = edge.Get(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U
  )._2

  io.mem_acquire.bits := GetBlock
  // resolve cache alias by L2
//  io.mem_acquire.bits.user.lift(AliasKey).foreach(_ := req.vaddr(13, 12))
  require(nSets <= 256) // icache size should not be more than 128KB

  /** Grant ACK */
  io.mem_finish.valid := false.B//(state === s_send_grant_ack) && is_grant
  io.mem_finish.bits := grantack

  //resp to ifu
  io.resp.valid := state === s_wait_resp
  /** update coh meta */
  def missCohGen(param: UInt, dirty: Bool): UInt = {
    MuxLookup(Cat(param, dirty), Nothing, Seq(
      Cat(toB, false.B) -> Branch,
      Cat(toB, true.B)  -> Branch,
      Cat(toT, false.B) -> Trunk,
      Cat(toT, true.B)  -> Dirty))
  }

  val miss_new_coh = ClientMetadata(ClientStates.Branch)

  io.meta_write.valid := (state === s_write_back)
  io.meta_write.bits.generate(tag = req_tag, coh = miss_new_coh, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0))

  io.data_write.valid := (state === s_write_back)
  io.data_write.bits.generate(data = respDataReg.asUInt,
                              idx  = req_idx,
                              waymask = req_waymask,
                              bankIdx = req_idx(0),
                              paddr = req.paddr)

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
    val hartId      = Input(UInt(8.W))
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, ValidIO(new ICacheMissResp))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish  = DecoupledIO(new TLBundleE(edge.bundle))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val prefetch_req          =  Flipped(DecoupledIO(new PIQReq))
    val prefetch_check        =  Vec(PortNumber,ValidIO(UInt(PAddrBits.W)))


  })
  // assign default values to output signals
  io.mem_grant.ready := false.B

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  PortNumber))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  PortNumber))

  io.mem_grant.ready := true.B

  val entries = (0 until PortNumber) map { i =>
    val entry = Module(new ICacheMissEntry(edge, i))

    entry.io.id := i.U

    // entry req
    entry.io.req.valid := io.req(i).valid
    entry.io.req.bits  := io.req(i).bits
    io.req(i).ready    := entry.io.req.ready

    // entry resp
    meta_write_arb.io.in(i)     <>  entry.io.meta_write
    refill_arb.io.in(i)         <>  entry.io.data_write

    entry.io.mem_grant.valid := false.B
    entry.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      entry.io.mem_grant <> io.mem_grant
    }

    io.resp(i) <> entry.io.resp
    io.prefetch_check(i) <> entry.io.toPrefetch

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

  val alloc = Wire(UInt(log2Ceil(nPrefetchEntries).W))

  val prefEntries = (PortNumber until PortNumber + nPrefetchEntries) map { i =>
    val prefetchEntry = Module(new IPrefetchEntry(edge, PortNumber))

    prefetchEntry.io.mem_hint_ack.valid := false.B
    prefetchEntry.io.mem_hint_ack.bits := DontCare

    when(io.mem_grant.bits.source === PortNumber.U) {
      prefetchEntry.io.mem_hint_ack <> io.mem_grant
    }

    prefetchEntry.io.req.valid := io.prefetch_req.valid && ((i-PortNumber).U === alloc)
    prefetchEntry.io.req.bits  := io.prefetch_req.bits

    prefetchEntry.io.id := i.U

    prefetchEntry
  }
  alloc := PriorityEncoder(prefEntries.map(_.io.req.ready))
  io.prefetch_req.ready := ParallelOR(prefEntries.map(_.io.req.ready))
  val tl_a_chanel = entries.map(_.io.mem_acquire) ++ prefEntries.map(_.io.mem_hint)
  TLArbiter.lowest(edge, io.mem_acquire, tl_a_chanel:_*)

  TLArbiter.lowest(edge, io.mem_finish,  entries.map(_.io.mem_finish):_*)

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out

  if (env.EnableDifftest) {
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := io.hartId
    difftest.io.cacheid := 0.U
    difftest.io.valid := refill_arb.io.out.valid
    difftest.io.addr := refill_arb.io.out.bits.paddr
    difftest.io.data := refill_arb.io.out.bits.data.asTypeOf(difftest.io.data)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_refill_way_" + Integer.toString(w, 10),  entries(0).io.meta_write.valid && OHToUInt(entries(0).io.meta_write.bits.waymask)  === w.U)
    XSPerfAccumulate("line_1_refill_way_" + Integer.toString(w, 10),  entries(1).io.meta_write.valid && OHToUInt(entries(1).io.meta_write.bits.waymask)  === w.U)
  }

}



