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
import utility._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMP, PMPChecker, PMPReqBundle, PMPRespBundle}
import xiangshan.backend.fu.util.HasCSRConst
import utility.ChiselDB
import difftest._

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

  val sfence_tmp = DelayN(io.sfence, 1)
  val csr_tmp    = DelayN(io.csr.tlb, 1)
  val sfence_dup = Seq.fill(8)(RegNext(sfence_tmp))
  val csr_dup = Seq.fill(7)(RegNext(csr_tmp))
  val satp   = csr_dup(0).satp
  val priv   = csr_dup(0).priv
  val flush  = sfence_dup(0).valid || satp.changed

  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(2)(Module(new PMPChecker(lgMaxSize = 3, sameCycle = true)).io))
  pmp.io.distribute_csr := io.csr.distribute_csr
  pmp_check.foreach(_.check_env.apply(ModeS, pmp.io.pmp, pmp.io.pma))

  val missQueue = Module(new L2TlbMissQueue)
  val cache = Module(new PtwCache)
  val ptw = Module(new PTW)
  val llptw = Module(new LLPTW)
  val blockmq = Module(new BlockHelper(3))
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bSourceWidth.W)
  }, if (l2tlbParams.enablePrefetch) 4 else 3))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwSectorResp, 1)).io)
  val mergeArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwMergeResp, 3)).io)
  val outArbCachePort = 0
  val outArbFsmPort = 1
  val outArbMqPort = 2

  // arb2 input port
  val InArbPTWPort = 0
  val InArbMissQueuePort = 1
  val InArbTlbPort = 2
  val InArbPrefetchPort = 3
  // NOTE: when cache out but miss and ptw doesnt accept,
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))
  arb1.io.out.ready := arb2.io.in(InArbTlbPort).ready

  arb2.io.in(InArbPTWPort).valid := ptw.io.llptw.valid
  arb2.io.in(InArbPTWPort).bits.vpn := ptw.io.llptw.bits.req_info.vpn
  arb2.io.in(InArbPTWPort).bits.source := ptw.io.llptw.bits.req_info.source
  ptw.io.llptw.ready := arb2.io.in(InArbPTWPort).ready
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
    prefetch.io.sfence := sfence_dup(0)
    prefetch.io.csr := csr_dup(0)
    arb2.io.in(InArbPrefetchPort) <> prefetch.io.out

    val L2TlbPrefetchTable = ChiselDB.createTable("L2TlbPrefetch_hart" + p(XSCoreParamsKey).HartId.toString, new L2TlbPrefetchDB)
    val L2TlbPrefetchDB = Wire(new L2TlbPrefetchDB)
    L2TlbPrefetchDB.vpn := prefetch.io.out.bits.vpn
    L2TlbPrefetchTable.log(L2TlbPrefetchDB, prefetch.io.out.fire, "L2TlbPrefetch", clock, reset)
  }
  arb2.io.out.ready := cache.io.req.ready


  val mq_arb = Module(new Arbiter(new L2TlbInnerBundle, 2))
  mq_arb.io.in(0).valid := cache.io.resp.valid && !cache.io.resp.bits.hit &&
    (!cache.io.resp.bits.toFsm.l2Hit || cache.io.resp.bits.bypassed) &&
    !from_pre(cache.io.resp.bits.req_info.source) &&
    (cache.io.resp.bits.bypassed || cache.io.resp.bits.isFirst || !ptw.io.req.ready)
  mq_arb.io.in(0).bits :=  cache.io.resp.bits.req_info
  mq_arb.io.in(1) <> llptw.io.cache
  missQueue.io.in <> mq_arb.io.out
  missQueue.io.sfence  := sfence_dup(6)
  missQueue.io.csr := csr_dup(5)

  blockmq.io.start := missQueue.io.out.fire
  blockmq.io.enable := ptw.io.req.fire()

  llptw.io.in.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.toFsm.l2Hit && !cache.io.resp.bits.bypassed
  llptw.io.in.bits.req_info := cache.io.resp.bits.req_info
  llptw.io.in.bits.ppn := cache.io.resp.bits.toFsm.ppn
  llptw.io.sfence := sfence_dup(1)
  llptw.io.csr := csr_dup(1)

  cache.io.req.valid := arb2.io.out.valid
  cache.io.req.bits.req_info.vpn := arb2.io.out.bits.vpn
  cache.io.req.bits.req_info.source := arb2.io.out.bits.source
  cache.io.req.bits.isFirst := arb2.io.chosen =/= InArbMissQueuePort.U
  cache.io.req.bits.bypassed.map(_ := false.B)
  cache.io.sfence := sfence_dup(2)
  cache.io.csr := csr_dup(2)
  cache.io.sfence_dup.zip(sfence_dup.drop(2).take(4)).map(s => s._1 := s._2)
  cache.io.csr_dup.zip(csr_dup.drop(2).take(3)).map(c => c._1 := c._2)
  cache.io.resp.ready := Mux(cache.io.resp.bits.hit,
    outReady(cache.io.resp.bits.req_info.source, outArbCachePort),
    Mux(cache.io.resp.bits.toFsm.l2Hit && !cache.io.resp.bits.bypassed, llptw.io.in.ready,
    Mux(cache.io.resp.bits.bypassed || cache.io.resp.bits.isFirst, mq_arb.io.in(0).ready, mq_arb.io.in(0).ready || ptw.io.req.ready)))

  // NOTE: missQueue req has higher priority
  ptw.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.toFsm.l2Hit &&
    !cache.io.resp.bits.bypassed &&
    !cache.io.resp.bits.isFirst
  ptw.io.req.bits.req_info := cache.io.resp.bits.req_info
  ptw.io.req.bits.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  ptw.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  ptw.io.sfence := sfence_dup(7)
  ptw.io.csr := csr_dup(6)
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
  val last_resp_vpn = RegEnable(cache.io.refill.bits.req_info_dup(0).vpn, cache.io.refill.valid)
  val last_resp_level = RegEnable(cache.io.refill.bits.level_dup(0), cache.io.refill.valid)
  val last_resp_v = RegInit(false.B)
  val last_has_invalid = !Cat(cache.io.refill.bits.ptes.asTypeOf(Vec(blockBits/XLEN, UInt(XLEN.W))).map(a => a(0))).andR || cache.io.refill.bits.sel_pte_dup(0).asTypeOf(new PteBundle).isAf()
  when (cache.io.refill.valid) { last_resp_v := !last_has_invalid}
  when (flush) { last_resp_v := false.B }
  XSError(last_resp_v && cache.io.refill.valid &&
    (cache.io.refill.bits.req_info_dup(0).vpn === last_resp_vpn) &&
    (cache.io.refill.bits.level_dup(0) === last_resp_level),
    "l2tlb should not access mem at same addr for twice")
  // ATTENTION: this may wronngly assert when: a ptes is l2, last part is valid,
  // but the current part is invalid, so one more mem access happened
  // If this happened, remove the assert.

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
  // refill_data_tmp is the wire fork of refill_data, but one cycle earlier
  val refill_data_tmp = WireInit(refill_data)
  refill_data_tmp(refill_helper._4) := mem.d.bits.data

  // save only one pte for each id
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte = VecInit((0 until MemReqWidth).map(i =>
    if (i == l2tlbParams.llptwsize) {RegEnable(get_part(refill_data_tmp, req_addr_low(i)), mem_resp_done && !mem_resp_from_mq) }
    else { DataHoldBypass(get_part(refill_data, req_addr_low(i)), llptw_mem.buffer_it(i)) }
    // llptw could not use refill_data_tmp, because enq bypass's result works at next cycle
  ))

  // save only one pte for each id, for sector tlb
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte_sector = VecInit((0 until MemReqWidth).map(i =>
    if (i == l2tlbParams.llptwsize) {RegEnable(refill_data_tmp, mem_resp_done && !mem_resp_from_mq) }
    else { DataHoldBypass(refill_data, llptw_mem.buffer_it(i)) }
    // llptw could not use refill_data_tmp, because enq bypass's result works at next cycle
  ))

  // mem -> miss queue
  llptw_mem.resp.valid := mem_resp_done && mem_resp_from_mq
  llptw_mem.resp.bits.id := DataHoldBypass(mem.d.bits.source, mem.d.valid)
  // mem -> ptw
  ptw.io.mem.req.ready := mem.a.ready
  ptw.io.mem.resp.valid := mem_resp_done && !mem_resp_from_mq
  ptw.io.mem.resp.bits := resp_pte.last
  // mem -> cache
  val refill_from_mq = mem_resp_from_mq
  val refill_level = Mux(refill_from_mq, 2.U, RegEnable(ptw.io.refill.level, init = 0.U, ptw.io.mem.req.fire()))
  val refill_valid = mem_resp_done && !flush && !flush_latch(mem.d.bits.source)

  cache.io.refill.valid := RegNext(refill_valid, false.B)
  cache.io.refill.bits.ptes := refill_data.asUInt
  cache.io.refill.bits.req_info_dup.map(_ := RegEnable(Mux(refill_from_mq, llptw_mem.refill, ptw.io.refill.req_info), refill_valid))
  cache.io.refill.bits.level_dup.map(_ := RegEnable(refill_level, refill_valid))
  cache.io.refill.bits.levelOH(refill_level, refill_valid)
  cache.io.refill.bits.sel_pte_dup.map(_ := RegNext(sel_data(refill_data_tmp.asUInt, req_addr_low(mem.d.bits.source))))

  if (env.EnableDifftest) {
    val difftest_ptw_addr = RegInit(VecInit(Seq.fill(MemReqWidth)(0.U(PAddrBits.W))))
    when (mem.a.valid) {
      difftest_ptw_addr(mem.a.bits.source) := mem.a.bits.address
    }

    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := p(XSCoreParamsKey).HartId.asUInt
    difftest.io.cacheid := 2.U
    difftest.io.valid := cache.io.refill.valid
    difftest.io.addr := difftest_ptw_addr(RegNext(mem.d.bits.source))
    difftest.io.data := refill_data.asTypeOf(difftest.io.data)
  }

  if (env.EnableDifftest) {
    for (i <- 0 until PtwWidth) {
      val difftest = Module(new DifftestL2TLBEvent)
      difftest.io.clock := clock
      difftest.io.coreid := p(XSCoreParamsKey).HartId.asUInt
      difftest.io.valid := io.tlb(i).resp.fire && !io.tlb(i).resp.bits.af
      difftest.io.index := i.U
      difftest.io.satp := io.csr.tlb.satp.ppn
      difftest.io.vpn := Cat(io.tlb(i).resp.bits.entry.tag, 0.U(sectortlbwidth.W))
      for (j <- 0 until tlbcontiguous) {
        difftest.io.ppn(j) := Cat(io.tlb(i).resp.bits.entry.ppn, io.tlb(i).resp.bits.ppn_low(j))
        difftest.io.valididx(j) := io.tlb(i).resp.bits.valididx(j)
      }
      difftest.io.perm := io.tlb(i).resp.bits.entry.perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      difftest.io.level := io.tlb(i).resp.bits.entry.level.getOrElse(0.U.asUInt)
      difftest.io.pf := io.tlb(i).resp.bits.pf
    }
  }

  // pmp
  pmp_check(0).req <> ptw.io.pmp.req
  ptw.io.pmp.resp <> pmp_check(0).resp
  pmp_check(1).req <> llptw.io.pmp.req
  llptw.io.pmp.resp <> pmp_check(1).resp

  llptw_out.ready := outReady(llptw_out.bits.req_info.source, outArbMqPort)
  for (i <- 0 until PtwWidth) {
    mergeArb(i).in(outArbCachePort).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.req_info.source===i.U
    mergeArb(i).in(outArbCachePort).bits := cache.io.resp.bits.toTlb
    mergeArb(i).in(outArbFsmPort).valid := ptw.io.resp.valid && ptw.io.resp.bits.source===i.U
    mergeArb(i).in(outArbFsmPort).bits := ptw.io.resp.bits.resp
    mergeArb(i).in(outArbMqPort).valid := llptw_out.valid && llptw_out.bits.req_info.source===i.U
    mergeArb(i).in(outArbMqPort).bits := contiguous_pte_to_merge_ptwResp(resp_pte_sector(llptw_out.bits.id).asUInt, llptw_out.bits.req_info.vpn, llptw_out.bits.af, true)
    mergeArb(i).out.ready := outArb(i).in(0).ready
  }

  for (i <- 0 until PtwWidth) {
    outArb(i).in(0).valid := mergeArb(i).out.valid
    outArb(i).in(0).bits := merge_ptwResp_to_sector_ptwResp(mergeArb(i).out.bits)
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
    ptw_resp.af := (if (!af_first) pte_in.isPf(2.U) else true.B) && (af || pte_in.isAf())
    ptw_resp.entry.v := !ptw_resp.pf
    ptw_resp.entry.prefetch := DontCare
    ptw_resp.entry.asid := satp.asid
    ptw_resp
  }

  def contiguous_pte_to_merge_ptwResp(pte: UInt, vpn: UInt, af: Bool, af_first: Boolean, not_super: Boolean = true) : PtwMergeResp = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val ptw_merge_resp = Wire(new PtwMergeResp())
    for (i <- 0 until tlbcontiguous) {
      val pte_in = pte(64 * i + 63, 64 * i).asTypeOf(new PteBundle())
      val ptw_resp = Wire(new PtwMergeEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true))
      ptw_resp.ppn := pte_in.ppn(ppnLen - 1, sectortlbwidth)
      ptw_resp.ppn_low := pte_in.ppn(sectortlbwidth - 1, 0)
      ptw_resp.level.map(_ := 2.U)
      ptw_resp.perm.map(_ := pte_in.getPerm())
      ptw_resp.tag := vpn(vpnLen - 1, sectortlbwidth)
      ptw_resp.pf := (if (af_first) !af else true.B) && pte_in.isPf(2.U)
      ptw_resp.af := (if (!af_first) pte_in.isPf(2.U) else true.B) && (af || pte_in.isAf())
      ptw_resp.v := !ptw_resp.pf
      ptw_resp.prefetch := DontCare
      ptw_resp.asid := satp.asid
      ptw_merge_resp.entry(i) := ptw_resp
    }
    ptw_merge_resp.pteidx := UIntToOH(vpn(sectortlbwidth - 1, 0)).asBools
    ptw_merge_resp.not_super := not_super.B
    ptw_merge_resp
  }

  def merge_ptwResp_to_sector_ptwResp(pte: PtwMergeResp) : PtwSectorResp = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val ptw_sector_resp = Wire(new PtwSectorResp)
    ptw_sector_resp.entry.tag := pte.entry(OHToUInt(pte.pteidx)).tag
    ptw_sector_resp.entry.asid := pte.entry(OHToUInt(pte.pteidx)).asid
    ptw_sector_resp.entry.ppn := pte.entry(OHToUInt(pte.pteidx)).ppn
    ptw_sector_resp.entry.perm.map(_ := pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)))
    ptw_sector_resp.entry.level.map(_ := pte.entry(OHToUInt(pte.pteidx)).level.getOrElse(0.U(2.W)))
    ptw_sector_resp.entry.prefetch := pte.entry(OHToUInt(pte.pteidx)).prefetch
    ptw_sector_resp.entry.v := pte.entry(OHToUInt(pte.pteidx)).v
    ptw_sector_resp.af := pte.entry(OHToUInt(pte.pteidx)).af
    ptw_sector_resp.pf := pte.entry(OHToUInt(pte.pteidx)).pf
    ptw_sector_resp.addr_low := OHToUInt(pte.pteidx)
    for (i <- 0 until tlbcontiguous) {
      val ppn_equal = pte.entry(i).ppn === pte.entry(OHToUInt(pte.pteidx)).ppn
      val perm_equal = pte.entry(i).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt === pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      val v_equal = pte.entry(i).v === pte.entry(OHToUInt(pte.pteidx)).v
      val af_equal = pte.entry(i).af === pte.entry(OHToUInt(pte.pteidx)).af
      val pf_equal = pte.entry(i).pf === pte.entry(OHToUInt(pte.pteidx)).pf
      ptw_sector_resp.valididx(i) := (ppn_equal && perm_equal && v_equal && af_equal && pf_equal) || !pte.not_super
      ptw_sector_resp.ppn_low(i) := pte.entry(i).ppn_low
    }
    ptw_sector_resp.valididx(OHToUInt(pte.pteidx)) := true.B
    ptw_sector_resp
  }

  def outReady(source: UInt, port: Int): Bool = {
    MuxLookup(source, true.B,
      (0 until PtwWidth).map(i => i.U -> mergeArb(i).in(port).ready))
  }

  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[sfence] ${io.sfence}\n")
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
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"llptw_ppn_af${i}", mergeArb(i).in(outArbMqPort).valid && mergeArb(i).in(outArbMqPort).bits.entry(OHToUInt(mergeArb(i).in(outArbMqPort).bits.pteidx)).af && !llptw_out.bits.af)
    XSPerfAccumulate(s"access_fault${i}", io.tlb(i).resp.fire && io.tlb(i).resp.bits.af)
  }

  // print configs
  println(s"${l2tlbParams.name}: a ptw, a llptw with size ${l2tlbParams.llptwsize}, miss queue size ${MissQueueSize} l1:${l2tlbParams.l1Size} fa l2: nSets ${l2tlbParams.l2nSets} nWays ${l2tlbParams.l2nWays} l3: ${l2tlbParams.l3nSets} nWays ${l2tlbParams.l3nWays} blockBytes:${l2tlbParams.blockBytes}")

  // time out assert
  for (i <- 0 until MemReqWidth) {
    TimeOutAssert(waiting_resp(i), timeOutThreshold, s"ptw mem resp time out wait_resp${i}")
    TimeOutAssert(flush_latch(i), timeOutThreshold, s"ptw mem resp time out flush_latch${i}")
  }


  val perfEvents  = Seq(llptw, cache, ptw).flatMap(_.getPerfEvents)
  generatePerfEvent()

  val L1TlbTable = ChiselDB.createTable("L1Tlb_hart" + p(XSCoreParamsKey).HartId.toString, new L1TlbDB)
  val ITlbReqDB, DTlbReqDB, ITlbRespDB, DTlbRespDB = Wire(new L1TlbDB)
  ITlbReqDB.vpn := io.tlb(0).req(0).bits.vpn
  DTlbReqDB.vpn := io.tlb(1).req(0).bits.vpn
  ITlbRespDB.vpn := io.tlb(0).resp.bits.entry.tag
  DTlbRespDB.vpn := io.tlb(1).resp.bits.entry.tag
  L1TlbTable.log(ITlbReqDB, io.tlb(0).req(0).fire, "ITlbReq", clock, reset)
  L1TlbTable.log(DTlbReqDB, io.tlb(1).req(0).fire, "DTlbReq", clock, reset)
  L1TlbTable.log(ITlbRespDB, io.tlb(0).resp.fire, "ITlbResp", clock, reset)
  L1TlbTable.log(DTlbRespDB, io.tlb(1).resp.fire, "DTlbResp", clock, reset)

  val PageCacheTable = ChiselDB.createTable("PageCache_hart" + p(XSCoreParamsKey).HartId.toString, new PageCacheDB)
  val PageCacheDB = Wire(new PageCacheDB)
  PageCacheDB.vpn := Cat(cache.io.resp.bits.toTlb.entry(0).tag, OHToUInt(cache.io.resp.bits.toTlb.pteidx))
  PageCacheDB.source := cache.io.resp.bits.req_info.source
  PageCacheDB.bypassed := cache.io.resp.bits.bypassed
  PageCacheDB.is_first := cache.io.resp.bits.isFirst
  PageCacheDB.prefetched := cache.io.resp.bits.toTlb.entry(0).prefetch
  PageCacheDB.prefetch := cache.io.resp.bits.prefetch
  PageCacheDB.l2Hit := cache.io.resp.bits.toFsm.l2Hit
  PageCacheDB.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  PageCacheDB.hit := cache.io.resp.bits.hit
  PageCacheTable.log(PageCacheDB, cache.io.resp.fire, "PageCache", clock, reset)

  val PTWTable = ChiselDB.createTable("PTW_hart" + p(XSCoreParamsKey).HartId.toString, new PTWDB)
  val PTWReqDB, PTWRespDB, LLPTWReqDB, LLPTWRespDB = Wire(new PTWDB)
  PTWReqDB.vpn := ptw.io.req.bits.req_info.vpn
  PTWReqDB.source := ptw.io.req.bits.req_info.source
  PTWRespDB.vpn := ptw.io.refill.req_info.vpn
  PTWRespDB.source := ptw.io.refill.req_info.source
  LLPTWReqDB.vpn := llptw.io.in.bits.req_info.vpn
  LLPTWReqDB.source := llptw.io.in.bits.req_info.source
  LLPTWRespDB.vpn := llptw.io.mem.refill.vpn
  LLPTWRespDB.source := llptw.io.mem.refill.source
  PTWTable.log(PTWReqDB, ptw.io.req.fire, "PTWReq", clock, reset)
  PTWTable.log(PTWRespDB, ptw.io.mem.resp.fire, "PTWResp", clock, reset)
  PTWTable.log(LLPTWReqDB, llptw.io.in.fire, "LLPTWReq", clock, reset)
  PTWTable.log(LLPTWRespDB, llptw.io.mem.resp.fire, "LLPTWResp", clock, reset)

  val L2TlbMissQueueTable = ChiselDB.createTable("L2TlbMissQueue_hart" + p(XSCoreParamsKey).HartId.toString, new L2TlbMissQueueDB)
  val L2TlbMissQueueInDB, L2TlbMissQueueOutDB = Wire(new L2TlbMissQueueDB)
  L2TlbMissQueueInDB.vpn := missQueue.io.in.bits.vpn
  L2TlbMissQueueOutDB.vpn := missQueue.io.out.bits.vpn
  L2TlbMissQueueTable.log(L2TlbMissQueueInDB, missQueue.io.in.fire, "L2TlbMissQueueIn", clock, reset)
  L2TlbMissQueueTable.log(L2TlbMissQueueOutDB, missQueue.io.out.fire, "L2TlbMissQueueOut", clock, reset)
}

/** BlockHelper, block missqueue, not to send too many req to cache
 *  Parameter:
 *    enable: enable BlockHelper, mq should not send too many reqs
 *    start: when miss queue out fire and need, block miss queue's out
 *    block: block miss queue's out
 *    latency: last missqueue out's cache access latency
 */
class BlockHelper(latency: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val start = Input(Bool())
    val block = Output(Bool())
  })

  val count = RegInit(0.U(log2Ceil(latency).W))
  val valid = RegInit(false.B)
  val work = RegInit(true.B)

  io.block := valid

  when (io.start && work) { valid := true.B }
  when (valid) { count := count + 1.U }
  when (count === (latency.U) || io.enable) {
    valid := false.B
    work := io.enable
    count := 0.U
  }
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

class PTWDelayN[T <: Data](gen: T, n: Int, flush: Bool) extends Module {
  val io = IO(new Bundle() {
    val in = Input(gen)
    val out = Output(gen)
    val ptwflush = Input(flush.cloneType)
  })
  val out = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  val t = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  out(0) := io.in
  if (n == 1) {
    io.out := out(0)
  } else {
    when (io.ptwflush) {
      for (i <- 0 until n) {
        t(i) := 0.U.asTypeOf(gen)
        out(i) := 0.U.asTypeOf(gen)
      }
      io.out := 0.U.asTypeOf(gen)
    } .otherwise {
      for (i <- 1 until n) {
        t(i-1) := out(i-1)
        out(i) := t(i-1)
      }
      io.out := out(n-1)
    }
  }
}

object PTWDelayN {
  def apply[T <: Data](in: T, n: Int, flush: Bool): T = {
    val delay = Module(new PTWDelayN(in.cloneType, n, flush))
    delay.io.in := in
    delay.io.ptwflush := flush
    delay.io.out
  }
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TLBIO)
  val flush = VecInit(Seq.fill(PtwWidth)(false.B))
  flush(0) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed, itlbParams.fenceDelay)
  flush(1) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed, ldtlbParams.fenceDelay)
  for (i <- 0 until PtwWidth) {
    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.satp := io.csr.tlb.satp.ppn

    if (coreParams.softPTWDelay == 1) {
      helper.enable := io.tlb(i).req(0).fire
      helper.vpn := io.tlb(i).req(0).bits.vpn
    } else {
      helper.enable := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay - 1, flush(i))
      helper.vpn := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay - 1, flush(i))
    }

    val pte = helper.pte.asTypeOf(new PteBundle)
    val level = helper.level
    val pf = helper.pf
    val empty = RegInit(true.B)
    when (io.tlb(i).req(0).fire) {
      empty := false.B
    } .elsewhen (io.tlb(i).resp.fire || flush(i)) {
      empty := true.B
    }

    io.tlb(i).req(0).ready := empty || io.tlb(i).resp.fire
    io.tlb(i).resp.valid := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay, flush(i))
    assert(!io.tlb(i).resp.valid || io.tlb(i).resp.ready)
    io.tlb(i).resp.bits.entry.tag := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay, flush(i))
    io.tlb(i).resp.bits.entry.ppn := pte.ppn
    io.tlb(i).resp.bits.entry.perm.map(_ := pte.getPerm())
    io.tlb(i).resp.bits.entry.level.map(_ := level)
    io.tlb(i).resp.bits.pf := pf
    io.tlb(i).resp.bits.af := DontCare // TODO: implement it
    io.tlb(i).resp.bits.entry.v := !pf
    io.tlb(i).resp.bits.entry.prefetch := DontCare
    io.tlb(i).resp.bits.entry.asid := io.csr.tlb.satp.asid
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
