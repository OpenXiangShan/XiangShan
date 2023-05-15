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

    val ongoing_req    = ValidIO(UInt(PAddrBits.W))
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
  io.ongoing_req.bits  :=  addrAlign(req.paddr, blockBytes, PAddrBits)

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
          req_corrupt := io.mem_grant.bits.corrupt // TODO: seems has bug
          when(readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
          }
        }
      }
    }

    is(s_write_back) {
      state := Mux(io.meta_write.fire() && io.data_write.fire() || needflush, s_wait_resp, s_write_back)
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

  val getBlock = edge.Get(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits),
    lgSize = (log2Up(cacheParams.blockBytes)).U
  )._2

  io.mem_acquire.bits := getBlock // getBlock
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

    val meta_write  = DecoupledIO(new ICacheMetaWriteBundle)
    val data_write  = DecoupledIO(new ICacheDataWriteBundle)

    val prefetch_req          =  Flipped(DecoupledIO(new PIQReq))
    val mshr_info             =  Vec(totalMSHRNum,ValidIO(UInt(PAddrBits.W)))
    val freePIQEntry          =  Output(UInt(log2Ceil(nPrefetchEntries).W))

    val fencei = Input(Bool())

    val piq_write_ipbuffer = ValidIO(new IPFBufferWrite)

    val to_main_pipe = Vec(nPrefetchEntries, new PIQToMainPipe)
  })
  // assign default values to output signals
  io.mem_grant.ready := false.B

  val meta_write_arb = Module(new Arbiter(new ICacheMetaWriteBundle,  PortNumber))
  val refill_arb     = Module(new Arbiter(new ICacheDataWriteBundle,  PortNumber))
  val ipf_write_arb  = Module(new Arbiter(new IPFBufferWrite,  nPrefetchEntries))

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
    io.mshr_info(i) <> entry.io.ongoing_req
    entry.io.fencei := io.fencei
//    XSPerfAccumulate(
//      "entryPenalty" + Integer.toString(i, 10),
//      BoolStopWatch(
//        start = entry.io.req.fire(),
//        stop = entry.io.resp.fire(),
//        startHighPriority = true)
//    )
//    XSPerfAccumulate("entryReq" + Integer.toString(i, 10), entry.io.req.fire())

    entry
  }

  val alloc = Wire(UInt(log2Ceil(nPrefetchEntries).W))
  val toMainPipe = io.to_main_pipe.map(_.info)

  val prefEntries = (PortNumber until PortNumber + nPrefetchEntries) map { i =>
    val prefetchEntry = Module(new PIQEntry(edge, i))

    prefetchEntry.io.mem_grant.valid := false.B
    prefetchEntry.io.mem_grant.bits := DontCare
    prefetchEntry.io.fencei := io.fencei

    ipf_write_arb.io.in(i - PortNumber) <> prefetchEntry.io.piq_write_ipbuffer

    when(io.mem_grant.bits.source === i.U) {
      prefetchEntry.io.mem_grant <> io.mem_grant
    }

    prefetchEntry.io.req.valid := io.prefetch_req.valid && ((i-PortNumber).U === alloc)
    prefetchEntry.io.req.bits  := io.prefetch_req.bits

    prefetchEntry.io.id := i.U

    io.mshr_info(i) := prefetchEntry.io.ongoing_req

    prefetchEntry
  }

  alloc := PriorityEncoder(prefEntries.map(_.io.req.ready))
  io.prefetch_req.ready := ParallelOR(prefEntries.map(_.io.req.ready))
  io.freePIQEntry := PriorityEncoder(prefEntries.map(_.io.req.ready))
  (0 until nPrefetchEntries).foreach(i => toMainPipe(i) <> prefEntries(i).io.prefetch_entry_data)
  val tl_a_chanel = entries.map(_.io.mem_acquire) ++ prefEntries.map(_.io.mem_acquire)
  TLArbiter.lowest(edge, io.mem_acquire, tl_a_chanel:_*)

  io.meta_write     <> meta_write_arb.io.out
  io.data_write     <> refill_arb.io.out

  io.piq_write_ipbuffer.valid := ipf_write_arb.io.out.valid
  io.piq_write_ipbuffer.bits  := ipf_write_arb.io.out.bits
  ipf_write_arb.io.out.ready := true.B

  XSPerfAccumulate("refill_ipf_num", io.piq_write_ipbuffer.fire)

  if (env.EnableDifftest) {
    val diffipfrefill = Module(new DifftestRefillEvent)
    diffipfrefill.io.clock := clock
    diffipfrefill.io.coreid := io.hartId
    diffipfrefill.io.cacheid := 3.U
    diffipfrefill.io.valid := ipf_write_arb.io.out.valid
    diffipfrefill.io.addr := ipf_write_arb.io.out.bits.meta.paddr
    diffipfrefill.io.data := ipf_write_arb.io.out.bits.data.asTypeOf(diffipfrefill.io.data)
  }

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



