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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._
import xiangshan._
import xiangshan.cache._
import utils._
import utility._
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

    val meta_write = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write = DecoupledIO(new ICacheDataWriteBundle)

    val ongoing_req    = Output(new FilterInfo)
    val fencei = Input(Bool())
  })

  /** default value for control signals */
  io.resp := DontCare
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.meta_write.bits := DontCare
  io.data_write.bits := DontCare

  val s_idle  :: s_send_mem_aquire :: s_wait_mem_grant :: s_write_back :: s_wait_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)
  /** control logic transformation */
  //request register
  val req = Reg(new ICacheMissReq)
  val req_idx = req.getVirSetIdx //virtual index
  val req_tag = req.getPhyTag //physical tag
  val req_waymask = req.waymask
  val req_corrupt = RegInit(false.B)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  val needflush_r = RegInit(false.B)
  when (state === s_idle) { needflush_r := false.B }
  when (state =/= s_idle && io.fencei) { needflush_r := true.B }
  val needflush = needflush_r | io.fencei

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

  io.ongoing_req.valid := (state =/= s_idle)
  io.ongoing_req.paddr :=  addrAlign(req.paddr, blockBytes, PAddrBits)

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.fire) {
        readBeatCnt := 0.U
        state := s_send_mem_aquire
        req := io.req.bits
      }
    }

    // memory request
    is(s_send_mem_aquire) {
      when(io.mem_acquire.fire) {
        state := s_wait_mem_grant
      }
    }

    is(s_wait_mem_grant) {
      when(edge.hasData(io.mem_grant.bits)) {
        when(io.mem_grant.fire) {
          readBeatCnt := readBeatCnt + 1.U
          respDataReg(readBeatCnt) := io.mem_grant.bits.data
          req_corrupt := io.mem_grant.bits.corrupt // TODO: seems has bug
          when(readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
          }
        }
      }
    }

    is(s_write_back) {
      state := Mux(io.meta_write.fire && io.data_write.fire || needflush, s_wait_resp, s_write_back)
    }

    is(s_wait_resp) {
      io.resp.bits.data := respDataReg.asUInt
      io.resp.bits.corrupt := req_corrupt
      when(io.resp.fire) {
        state := s_idle
      }
    }
  }

  /** refill write and meta write */

  val getBlock = edge.Get(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U
  )._2

  io.mem_acquire.bits := getBlock // getBlock
  // req source
  io.mem_acquire.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUInst.id.U)
  require(nSets <= 256) // icache size should not be more than 128KB

  //resp to ifu
  io.resp.valid := state === s_wait_resp

  io.meta_write.valid := (state === s_write_back && !needflush)
  io.meta_write.bits.generate(tag = req_tag, idx = req_idx, waymask = req_waymask, bankIdx = req_idx(0))

  io.data_write.valid := (state === s_write_back && !needflush)
  io.data_write.bits.generate(data = respDataReg.asUInt,
                              idx  = req_idx,
                              waymask = req_waymask,
                              bankIdx = req_idx(0),
                              paddr = req.paddr)

  XSPerfAccumulate(
    "entryPenalty" + Integer.toString(id, 10),
    BoolStopWatch(
      start = io.req.fire,
      stop = io.resp.valid,
      startHighPriority = true)
  )
  XSPerfAccumulate("entryReq" + Integer.toString(id, 10), io.req.fire)

  // Statistics on the latency distribution of MSHR
  val cntLatency = RegInit(0.U(32.W))
  cntLatency := Mux(io.mem_acquire.fire, 1.U, cntLatency + 1.U)
  // the condition is same as the transition from s_wait_mem_grant to s_write_back
  val cntEnable = (state === s_wait_mem_grant) && edge.hasData(io.mem_grant.bits) &&
                  io.mem_grant.fire && (readBeatCnt === (refillCycles - 1).U)
  XSPerfHistogram("icache_mshr_latency_" + id.toString(), cntLatency, cntEnable, 0, 300, 10, right_strict = true)
}


class ICacheMissUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheMissUnitModule
{
  val io = IO(new Bundle{
    val hartId      = Input(UInt(8.W))
    val req         = Vec(2, Flipped(DecoupledIO(new ICacheMissReq)))
    val resp        = Vec(2, ValidIO(new ICacheMissResp))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    val fdip_acquire = Flipped(DecoupledIO(new TLBundleA(edge.bundle)))
    val fdip_grant   = DecoupledIO(new TLBundleD(edge.bundle))

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val ICacheMissUnitInfo = new ICacheMissUnitInfo
    val fencei = Input(Bool())
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
    io.ICacheMissUnitInfo.mshr(i) <> entry.io.ongoing_req
    entry.io.fencei := io.fencei
//    XSPerfAccumulate(
//      "entryPenalty" + Integer.toString(i, 10),
//      BoolStopWatch(
//        start = entry.io.req.fire,
//        stop = entry.io.resp.fire,
//        startHighPriority = true)
//    )
//    XSPerfAccumulate("entryReq" + Integer.toString(i, 10), entry.io.req.fire)

    entry
  }

  io.fdip_grant.valid := false.B
  io.fdip_grant.bits  := DontCare
  when (io.mem_grant.bits.source === PortNumber.U) {
    io.fdip_grant <> io.mem_grant
  }

  /**
    ******************************************************************************
    * Register 2 cycle meta write info for IPrefetchPipe filter
    ******************************************************************************
    */
  val meta_write_buffer = InitQueue(new FilterInfo, size = 2)
  meta_write_buffer(0).valid := io.meta_write.fire
  meta_write_buffer(0).paddr := io.data_write.bits.paddr
  meta_write_buffer(1)       := meta_write_buffer(0)
  (0 until 2).foreach (i => {
    io.ICacheMissUnitInfo.recentWrite(i) := meta_write_buffer(i)
  })

  val tl_a_chanel = entries.map(_.io.mem_acquire) :+ io.fdip_acquire
  TLArbiter.lowest(edge, io.mem_acquire, tl_a_chanel:_*)

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index := 0.U
    difftest.valid := refill_arb.io.out.valid
    difftest.addr := refill_arb.io.out.bits.paddr
    difftest.data := refill_arb.io.out.bits.data.asTypeOf(difftest.data)
    difftest.idtfr := DontCare
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_refill_way_" + Integer.toString(w, 10),  entries(0).io.meta_write.valid && OHToUInt(entries(0).io.meta_write.bits.waymask)  === w.U)
    XSPerfAccumulate("line_1_refill_way_" + Integer.toString(w, 10),  entries(1).io.meta_write.valid && OHToUInt(entries(1).io.meta_write.bits.waymask)  === w.U)
  }

}
