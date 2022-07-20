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

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import chisel3.internal.naming.chiselName
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMP, PMPChecker, PMPReqBundle, PMPRespBundle}
import xiangshan.backend.fu.util.HasCSRConst

class L2TLB()(implicit p: Parameters) extends LazyModule with HasPtwConst {

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw",
      sourceId = IdRange(0, MemReqWidth)
    ))
  )))

  lazy val module = new L2TLBImp(this)
}

@chiselName
class L2TLBImp(outer: L2TLB)(implicit p: Parameters) extends PtwModule(outer) with HasCSRConst with HasPerfEvents {

  val (mem, edge) = outer.node.out.head

  val io = IO(new L2TLBIO)
  val difftestIO = IO(new Bundle() {
    val ptwResp = Output(Bool())
    val ptwAddr = Output(UInt(64.W))
    val ptwData = Output(Vec(4, UInt(64.W)))
  })

  /* Ptw processes multiple requests
   * Divide Ptw procedure into two stages: cache access ; mem access if cache miss
   *           miss queue itlb       dtlb
   *               |       |         |
   *               ------arbiter------
   *                            |
   *                    l1 - l2 - l3 - sp
   *                            |
   *          -------------------------------------------
   *    miss  |  queue                                  | hit
   *    [][][][][][]                                    |
   *          |                                         |
   *    state machine accessing mem                     |
   *          |                                         |
   *          ---------------arbiter---------------------
   *                 |                    |
   *                itlb                 dtlb
   */

  difftestIO <> DontCare

  val sfence = DelayN(io.sfence, 2)
  val csr    = DelayN(io.csr.tlb, 2)
  val satp   = csr.satp
  val priv   = csr.priv
  val flush  = sfence.valid || csr.satp.changed

  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(2)(Module(new PMPChecker(lgMaxSize = 3, sameCycle = true)).io))
  pmp.io.distribute_csr := io.csr.distribute_csr
  pmp_check.foreach(_.check_env.apply(ModeS, pmp.io.pmp, pmp.io.pma))

  val missQueue = Module(new L2TlbMissQueue)
  val cache = Module(new PtwCache)
  val ptw = Module(new PTW)
  val llptw = Module(new LLPTW)
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bSourceWidth.W)
  }, if (l2tlbParams.enablePrefetch) 3 else 2))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwResp, 3)).io)
  val outArbCachePort = 0
  val outArbFsmPort = 1
  val outArbMqPort = 2

  // NOTE: when cache out but miss and ptw doesnt accept,
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))
  arb1.io.out.ready := arb2.io.in(1).ready

  val InArbMissQueuePort = 0
  val InArbTlbPort = 1
  val InArbPrefetchPort = 2
  block_decoupled(missQueue.io.out, arb2.io.in(InArbMissQueuePort), !ptw.io.req.ready)
  arb2.io.in(InArbTlbPort).valid := arb1.io.out.valid
  arb2.io.in(InArbTlbPort).bits.vpn := arb1.io.out.bits.vpn
  arb2.io.in(InArbTlbPort).bits.source := arb1.io.chosen
  if (l2tlbParams.enablePrefetch) {
    val prefetch = Module(new L2TlbPrefetch())
    val recv = cache.io.resp
    // NOTE: 1. prefetch doesn't gen prefetch 2. req from mq doesn't gen prefetch
    // NOTE: 1. miss req gen prefetch 2. hit but prefetched gen prefetch
    prefetch.io.in.valid := recv.fire() && !from_pre(recv.bits.req_info.source) && (!recv.bits.hit  ||
      recv.bits.prefetch) && recv.bits.isFirst
    prefetch.io.in.bits.vpn := recv.bits.req_info.vpn
    prefetch.io.sfence := sfence
    prefetch.io.csr := csr
    arb2.io.in(InArbPrefetchPort) <> prefetch.io.out
  }
  arb2.io.out.ready := cache.io.req.ready

  val LLPTWARB_CACHE=0
  val LLPTWARB_PTW=1
  val llptw_arb = Module(new Arbiter(new LLPTWInBundle, 2))
  llptw_arb.io.in(LLPTWARB_CACHE).valid := cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.toFsm.l2Hit
  llptw_arb.io.in(LLPTWARB_CACHE).bits.req_info := cache.io.resp.bits.req_info
  llptw_arb.io.in(LLPTWARB_CACHE).bits.ppn := cache.io.resp.bits.toFsm.ppn
  llptw_arb.io.in(LLPTWARB_PTW) <> ptw.io.llptw
  llptw.io.in <> llptw_arb.io.out
  llptw.io.sfence := sfence
  llptw.io.csr := csr

  cache.io.req.valid := arb2.io.out.valid
  cache.io.req.bits.req_info.vpn := arb2.io.out.bits.vpn
  cache.io.req.bits.req_info.source := arb2.io.out.bits.source
  cache.io.req.bits.isFirst := arb2.io.chosen =/= InArbMissQueuePort.U
  cache.io.sfence := sfence
  cache.io.csr := csr
  cache.io.resp.ready := Mux(cache.io.resp.bits.hit,
    outReady(cache.io.resp.bits.req_info.source, outArbCachePort),
    Mux(cache.io.resp.bits.toFsm.l2Hit, llptw_arb.io.in(LLPTWARB_CACHE).ready,
    missQueue.io.in.ready || ptw.io.req.ready))

  missQueue.io.in.valid := cache.io.resp.valid && !cache.io.resp.bits.hit &&
    !cache.io.resp.bits.toFsm.l2Hit && !from_pre(cache.io.resp.bits.req_info.source) && !ptw.io.req.ready
  missQueue.io.in.bits := cache.io.resp.bits.req_info
  missQueue.io.sfence  := sfence
  missQueue.io.csr := csr

  // NOTE: missQueue req has higher priority
  ptw.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.toFsm.l2Hit
  ptw.io.req.bits.req_info := cache.io.resp.bits.req_info
  ptw.io.req.bits.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  ptw.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  ptw.io.csr := csr
  ptw.io.sfence := sfence
  ptw.io.resp.ready := outReady(ptw.io.resp.bits.source, outArbFsmPort)


  // mem req
  def blockBytes_align(addr: UInt) = {
    Cat(addr(PAddrBits - 1, log2Up(l2tlbParams.blockBytes)), 0.U(log2Up(l2tlbParams.blockBytes).W))
  }
  def addr_low_from_vpn(vpn: UInt) = {
    vpn(log2Ceil(l2tlbParams.blockBytes)-log2Ceil(XLEN/8)-1, 0)
  }
  def addr_low_from_paddr(paddr: UInt) = {
    paddr(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
  }
  def from_missqueue(id: UInt) = {
    (id =/= l2tlbParams.llptwsize.U)
  }
  val waiting_resp = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  val flush_latch = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  for (i <- waiting_resp.indices) {
    assert(!flush_latch(i) || waiting_resp(i)) // when sfence_latch wait for mem resp, waiting_resp should be true
  }

  val llptw_out = llptw.io.out
  val llptw_mem = llptw.io.mem
  llptw_mem.req_mask := waiting_resp.take(l2tlbParams.llptwsize)
  ptw.io.mem.mask := waiting_resp.last

  val mem_arb = Module(new Arbiter(new L2TlbMemReqBundle(), 2))
  mem_arb.io.in(0) <> ptw.io.mem.req
  mem_arb.io.in(1) <> llptw_mem.req
  mem_arb.io.out.ready := mem.a.ready && !flush

  // assert, should not send mem access at same addr for twice.
  val last_resp_vpn = RegEnable(cache.io.refill.bits.req_info.vpn, cache.io.refill.valid)
  val last_resp_level = RegEnable(cache.io.refill.bits.level, cache.io.refill.valid)
  val last_resp_v = RegInit(false.B)
  val last_has_invalid = !Cat(cache.io.refill.bits.ptes.asTypeOf(Vec(blockBits/XLEN, UInt(XLEN.W))).map(a => a(0))).andR
  when (cache.io.refill.valid && !last_has_invalid) { last_resp_v := true.B }
  when (flush) { last_resp_v := false.B }
  XSError(last_resp_v && cache.io.refill.valid &&
    (cache.io.refill.bits.req_info.vpn === last_resp_vpn) &&
    (cache.io.refill.bits.level === last_resp_level),
    "l2tlb should not access mem at same addr for twice")

  val req_addr_low = Reg(Vec(MemReqWidth, UInt((log2Up(l2tlbParams.blockBytes)-log2Up(XLEN/8)).W)))

  when (llptw.io.in.fire()) {
    // when enq miss queue, set the req_addr_low to receive the mem resp data part
    req_addr_low(llptw_mem.enq_ptr) := addr_low_from_vpn(llptw.io.in.bits.req_info.vpn)
  }
  when (mem_arb.io.out.fire()) {
    req_addr_low(mem_arb.io.out.bits.id) := addr_low_from_paddr(mem_arb.io.out.bits.addr)
    waiting_resp(mem_arb.io.out.bits.id) := true.B
  }
  // mem read
  val memRead =  edge.Get(
    fromSource = mem_arb.io.out.bits.id,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = blockBytes_align(mem_arb.io.out.bits.addr),
    lgSize     = log2Up(l2tlbParams.blockBytes).U
  )._2
  mem.a.bits := memRead
  mem.a.valid := mem_arb.io.out.valid && !flush
  mem.d.ready := true.B
  // mem -> data buffer
  val refill_data = Reg(Vec(blockBits / l1BusDataWidth, UInt(l1BusDataWidth.W)))
  val refill_helper = edge.firstlastHelper(mem.d.bits, mem.d.fire())
  val mem_resp_done = refill_helper._3
  val mem_resp_from_mq = from_missqueue(mem.d.bits.source)
  when (mem.d.valid) {
    assert(mem.d.bits.source <= l2tlbParams.llptwsize.U)
    refill_data(refill_helper._4) := mem.d.bits.data
  }
  // save only one pte for each id
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte = VecInit((0 until MemReqWidth).map(i =>
    if (i == l2tlbParams.llptwsize) {DataHoldBypass(get_part(refill_data, req_addr_low(i)), RegNext(mem_resp_done && !mem_resp_from_mq)) }
    else { DataHoldBypass(get_part(refill_data, req_addr_low(i)), llptw_mem.buffer_it(i)) }
  ))

  // mem -> miss queue
  llptw_mem.resp.valid := mem_resp_done && mem_resp_from_mq
  llptw_mem.resp.bits.id := mem.d.bits.source
  // mem -> ptw
  ptw.io.mem.req.ready := mem.a.ready
  ptw.io.mem.resp.valid := mem_resp_done && !mem_resp_from_mq
  ptw.io.mem.resp.bits := resp_pte.last
  // mem -> cache
  val refill_from_mq = RegNext(mem_resp_from_mq)
  cache.io.refill.valid := RegNext(mem_resp_done && !flush && !flush_latch(mem.d.bits.source))
  cache.io.refill.bits.ptes := refill_data.asUInt
  cache.io.refill.bits.req_info  := Mux(refill_from_mq, llptw_mem.refill, ptw.io.refill.req_info)
  cache.io.refill.bits.level := Mux(refill_from_mq, 2.U, RegEnable(ptw.io.refill.level, 0.U, ptw.io.mem.req.fire()))
  cache.io.refill.bits.addr_low := RegNext(req_addr_low(mem.d.bits.source))

  // pmp
  pmp_check(0).req <> ptw.io.pmp.req
  ptw.io.pmp.resp <> pmp_check(0).resp
  pmp_check(1).req <> llptw.io.pmp.req
  llptw.io.pmp.resp <> pmp_check(1).resp

  llptw_out.ready := outReady(llptw_out.bits.req_info.source, outArbMqPort)
  for (i <- 0 until PtwWidth) {
    outArb(i).in(outArbCachePort).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.req_info.source===i.U
    outArb(i).in(outArbCachePort).bits.entry := cache.io.resp.bits.toTlb
    outArb(i).in(outArbCachePort).bits.pf := !cache.io.resp.bits.toTlb.v
    outArb(i).in(outArbCachePort).bits.af := false.B
    outArb(i).in(outArbFsmPort).valid := ptw.io.resp.valid && ptw.io.resp.bits.source===i.U
    outArb(i).in(outArbFsmPort).bits := ptw.io.resp.bits.resp
    outArb(i).in(outArbMqPort).valid := llptw_out.valid && llptw_out.bits.req_info.source===i.U
    outArb(i).in(outArbMqPort).bits := pte_to_ptwResp(resp_pte(llptw_out.bits.id), llptw_out.bits.req_info.vpn, llptw_out.bits.af, true)
  }

  // io.tlb.map(_.resp) <> outArb.map(_.out)
  io.tlb.map(_.resp).zip(outArb.map(_.out)).map{
    case (resp, out) => resp <> out
  }

  // sfence
  when (flush) {
    for (i <- 0 until MemReqWidth) {
      when (waiting_resp(i)) {
        flush_latch(i) := true.B
      }
    }
  }
  // mem -> control signal
  // waiting_resp and sfence_latch will be reset when mem_resp_done
  when (mem_resp_done) {
    waiting_resp(mem.d.bits.source) := false.B
    flush_latch(mem.d.bits.source) := false.B
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  def get_part(data: Vec[UInt], index: UInt): UInt = {
    val inner_data = data.asTypeOf(Vec(data.getWidth / XLEN, UInt(XLEN.W)))
    inner_data(index)
  }

  def pte_to_ptwResp(pte: UInt, vpn: UInt, af: Bool, af_first: Boolean) : PtwResp = {
    val pte_in = pte.asTypeOf(new PteBundle())
    val ptw_resp = Wire(new PtwResp())
    ptw_resp.entry.ppn := pte_in.ppn
    ptw_resp.entry.level.map(_ := 2.U)
    ptw_resp.entry.perm.map(_ := pte_in.getPerm())
    ptw_resp.entry.tag := vpn
    ptw_resp.pf := (if (af_first) !af else true.B) && pte_in.isPf(2.U)
    ptw_resp.af := (if (!af_first) pte_in.isPf(2.U) else true.B) && af
    ptw_resp.entry.v := !ptw_resp.pf
    ptw_resp.entry.prefetch := DontCare
    ptw_resp.entry.asid := satp.asid
    ptw_resp
  }

  def outReady(source: UInt, port: Int): Bool = {
    MuxLookup(source, true.B,
      (0 until PtwWidth).map(i => i.U -> outArb(i).in(port).ready))
  }

  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[sfence] ${sfence}\n")
  XSDebug(p"[io.csr.tlb] ${io.csr.tlb}\n")

  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"req_count${i}", io.tlb(i).req(0).fire())
    XSPerfAccumulate(s"req_blocked_count_${i}", io.tlb(i).req(0).valid && !io.tlb(i).req(0).ready)
  }
  XSPerfAccumulate(s"req_blocked_by_mq", arb1.io.out.valid && missQueue.io.out.valid)
  for (i <- 0 until (MemReqWidth + 1)) {
    XSPerfAccumulate(s"mem_req_util${i}", PopCount(waiting_resp) === i.U)
  }
  XSPerfAccumulate("mem_cycle", PopCount(waiting_resp) =/= 0.U)
  XSPerfAccumulate("mem_count", mem.a.fire())

  // print configs
  println(s"${l2tlbParams.name}: a ptw, a llptw with size ${l2tlbParams.llptwsize}, miss queue size ${MissQueueSize} l1:${l2tlbParams.l1Size} fa l2: nSets ${l2tlbParams.l2nSets} nWays ${l2tlbParams.l2nWays} l3: ${l2tlbParams.l3nSets} nWays ${l2tlbParams.l3nWays} blockBytes:${l2tlbParams.blockBytes}")

  // time out assert
  for (i <- 0 until MemReqWidth) {
    TimeOutAssert(waiting_resp(i), timeOutThreshold, s"ptw mem resp time out wait_resp${i}")
    TimeOutAssert(flush_latch(i), timeOutThreshold, s"ptw mem resp time out flush_latch${i}")
  }


  val perfEvents  = Seq(llptw, cache, ptw).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class PTEHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val satp   = IO(Input(UInt(64.W)))
  val vpn    = IO(Input(UInt(64.W)))
  val pte    = IO(Output(UInt(64.W)))
  val level  = IO(Output(UInt(8.W)))
  val pf     = IO(Output(UInt(8.W)))
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TLBIO)

  for (i <- 0 until PtwWidth) {
    io.tlb(i).req(0).ready := true.B

    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.enable := io.tlb(i).req(0).valid
    helper.satp := io.csr.tlb.satp.ppn
    helper.vpn := io.tlb(i).req(0).bits.vpn
    val pte = helper.pte.asTypeOf(new PteBundle)
    val level = helper.level
    val pf = helper.pf

    io.tlb(i).resp.valid := RegNext(io.tlb(i).req(0).valid)
    assert(!io.tlb(i).resp.valid || io.tlb(i).resp.ready)
    io.tlb(i).resp.bits.entry.tag := RegNext(io.tlb(i).req(0).bits.vpn)
    io.tlb(i).resp.bits.entry.ppn := pte.ppn
    io.tlb(i).resp.bits.entry.perm.map(_ := pte.getPerm())
    io.tlb(i).resp.bits.entry.level.map(_ := level)
    io.tlb(i).resp.bits.pf := pf
    io.tlb(i).resp.bits.af := DontCare // TODO: implement it
  }
}

class L2TLBWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val useSoftPTW = coreParams.softPTW
  val node = if (!useSoftPTW) TLIdentityNode() else null
  val ptw = if (!useSoftPTW) LazyModule(new L2TLB()) else null
  if (!useSoftPTW) {
    node := ptw.node
  }

  lazy val module = new LazyModuleImp(this) with HasPerfEvents {
    val io = IO(new L2TLBIO)
    val perfEvents = if (useSoftPTW) {
      val fake_ptw = Module(new FakePTW())
      io <> fake_ptw.io
      Seq()
    }
    else {
        io <> ptw.module.io
        ptw.module.getPerfEvents
    }
    generatePerfEvent()
  }
}
