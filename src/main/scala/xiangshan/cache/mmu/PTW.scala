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

class PTW()(implicit p: Parameters) extends LazyModule with HasXSParameter {

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw",
      sourceId = IdRange(0, l2tlbParams.missQueueSize + 1)
    ))
  )))

  lazy val module = new PTWImp(this)
}

@chiselName
class PTWImp(outer: PTW)(implicit p: Parameters) extends PtwModule(outer) {

  val (mem, edge) = outer.node.out.head

  val io = IO(new PtwIO)
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

  val sfence = RegNext(io.sfence)
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv

  val missQueue = Module(new L2TlbMissQueue)
  val cache = Module(new PtwCache)
  val fsm = Module(new PtwFsm)
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
  }, 2))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwResp, 3)).io)
  val outArbFsmPort = 1
  val outArbMqPort = 2

  // NOTE: when cache out but miss and fsm doesnt accept,
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))
  arb1.io.out.ready := arb2.io.in(1).ready

  arb2.io.in(0) <> missQueue.io.cache
  arb2.io.in(1).valid := arb1.io.out.valid
  arb2.io.in(1).bits.vpn := arb1.io.out.bits.vpn
  arb2.io.in(1).bits.source := arb1.io.chosen
  arb2.io.out.ready := cache.io.req.ready

  cache.io.req.valid := arb2.io.out.valid
  cache.io.req.bits.vpn := arb2.io.out.bits.vpn
  cache.io.req.bits.source := arb2.io.out.bits.source
  cache.io.req.bits.isReplay := arb2.io.chosen === 0.U
  cache.io.sfence := sfence
  cache.io.resp.ready := Mux(cache.io.resp.bits.hit, true.B, missQueue.io.in.ready || fsm.io.req.ready)

  val mq_in_arb = Module(new Arbiter(new L2TlbMQInBundle, 2))
  mq_in_arb.io.in(0).valid := cache.io.resp.valid && !cache.io.resp.bits.hit && (cache.io.resp.bits.toFsm.l2Hit || !fsm.io.req.ready)
  mq_in_arb.io.in(0).bits.vpn := cache.io.resp.bits.vpn
  mq_in_arb.io.in(0).bits.source := cache.io.resp.bits.source
  mq_in_arb.io.in(0).bits.l3.valid := cache.io.resp.bits.toFsm.l2Hit
  mq_in_arb.io.in(0).bits.l3.bits := cache.io.resp.bits.toFsm.ppn
  mq_in_arb.io.in(1) <> fsm.io.mq
  missQueue.io.in <> mq_in_arb.io.out
  missQueue.io.sfence  := sfence
  missQueue.io.fsm_done := fsm.io.req.ready

  // NOTE: missQueue req has higher priority
  fsm.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.toFsm.l2Hit
  fsm.io.req.bits.source := cache.io.resp.bits.source
  fsm.io.req.bits.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  fsm.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  fsm.io.req.bits.vpn := cache.io.resp.bits.vpn
  fsm.io.csr := csr
  fsm.io.sfence := sfence
  fsm.io.resp.ready := MuxLookup(fsm.io.resp.bits.source, false.B,
    (0 until PtwWidth).map(i => i.U -> outArb(i).in(outArbFsmPort).ready))

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
    (id =/= MSHRSize.U)
  }
  val waiting_resp = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  val sfence_latch = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  for (i <- waiting_resp.indices) {
    assert(!sfence_latch(i) || waiting_resp(i)) // when sfence_latch wait for mem resp, waiting_resp should be true
  }

  val mq_out = missQueue.io.out
  val mq_mem = missQueue.io.mem
  mq_mem.req_mask := waiting_resp.take(MSHRSize)
  fsm.io.mem.mask := waiting_resp.last

  val mem_arb = Module(new Arbiter(new L2TlbMemReqBundle(), 2))
  mem_arb.io.in(0) <> fsm.io.mem.req
  mem_arb.io.in(1) <> mq_mem.req
  mem_arb.io.out.ready := mem.a.ready

  val req_addr_low = Reg(Vec(MemReqWidth, UInt((log2Up(l2tlbParams.blockBytes)-log2Up(XLEN/8)).W)))

  when (missQueue.io.in.fire()) {
    // when enq miss queue, set the req_addr_low to receive the mem resp data part
    req_addr_low(mq_mem.enq_ptr) := addr_low_from_vpn(missQueue.io.in.bits.vpn)
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
  mem.a.valid := mem_arb.io.out.valid
  mem.d.ready := true.B
  // mem -> data buffer
  val refill_data = Reg(Vec(blockBits / l1BusDataWidth, UInt(l1BusDataWidth.W)))
  val refill_helper = edge.firstlastHelper(mem.d.bits, mem.d.fire())
  val mem_resp_done = refill_helper._3
  val mem_resp_from_mq = from_missqueue(mem.d.bits.source)
  when (mem.d.valid) {
    assert(mem.d.bits.source <= MSHRSize.U)
    refill_data(refill_helper._4) := mem.d.bits.data
  }
  // save only one pte for each id
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte = VecInit((0 until MemReqWidth).map(i =>
    if (i == MSHRSize) {DataHoldBypass(get_part(refill_data, req_addr_low(i)), RegNext(mem_resp_done && !mem_resp_from_mq)) }
    else { DataHoldBypass(get_part(refill_data, req_addr_low(i)), mq_mem.buffer_it(i)) }
  ))
  // mem -> control signal
  when (mem_resp_done) {
    waiting_resp(mem.d.bits.source) := false.B
    sfence_latch(mem.d.bits.source) := false.B
  }
  // mem -> miss queue
  mq_mem.resp.valid := mem_resp_done && mem_resp_from_mq
  mq_mem.resp.bits.id := mem.d.bits.source
  // mem -> fsm
  fsm.io.mem.req.ready := mem.a.ready
  fsm.io.mem.resp.valid := mem_resp_done && !mem_resp_from_mq
  fsm.io.mem.resp.bits := resp_pte.last
  // mem -> cache
  val refill_from_mq = RegNext(mem_resp_from_mq)
  cache.io.refill.valid := RegNext(mem_resp_done && !io.sfence.valid && !sfence_latch(mem.d.bits.source))
  cache.io.refill.bits.ptes := refill_data.asUInt
  cache.io.refill.bits.vpn  := Mux(refill_from_mq, mq_mem.refill_vpn, fsm.io.refill.vpn)
  cache.io.refill.bits.level := Mux(refill_from_mq, 2.U, RegEnable(fsm.io.refill.level, init = 0.U, fsm.io.mem.req.fire()))
  cache.io.refill.bits.addr_low := req_addr_low(RegNext(mem.d.bits.source))


  mq_out.ready := MuxLookup(missQueue.io.out.bits.source, false.B,
    (0 until PtwWidth).map(i => i.U -> outArb(i).in(outArbMqPort).ready))
  for (i <- 0 until PtwWidth) {
    outArb(i).in(0).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.source===i.U
    outArb(i).in(0).bits.entry := cache.io.resp.bits.toTlb
    outArb(i).in(0).bits.pf := false.B
    outArb(i).in(outArbFsmPort).valid := fsm.io.resp.valid && fsm.io.resp.bits.source===i.U
    outArb(i).in(outArbFsmPort).bits := fsm.io.resp.bits.resp
    outArb(i).in(outArbMqPort).valid := mq_out.valid && mq_out.bits.source===i.U
    outArb(i).in(outArbMqPort).bits := pte_to_ptwResp(resp_pte(mq_out.bits.id), mq_out.bits.vpn)
  }

  // io.tlb.map(_.resp) <> outArb.map(_.out)
  io.tlb.map(_.resp).zip(outArb.map(_.out)).map{
    case (resp, out) => resp <> out
  }

  // sfence
  when (io.sfence.valid) {
    for (i <- 0 until MemReqWidth) {
      when ((waiting_resp(i) && !(mem_resp_done && mem.d.bits.source =/= i.U)) ||
        (mem.a.fire() && mem_arb.io.out.bits.id === i.U)) {
        sfence_latch(i) := true.B
      }
    }
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

  def pte_to_ptwResp(pte: UInt, vpn: UInt) : PtwResp = {
    val pte_in = pte.asTypeOf(new PteBundle())
    val ptw_resp = Wire(new PtwResp())
    ptw_resp.entry.ppn := pte_in.ppn
    ptw_resp.entry.level.map(_ := 2.U)
    ptw_resp.entry.perm.map(_ := pte_in.getPerm())
    ptw_resp.entry.tag := vpn
    ptw_resp.pf := pte_in.isPf(2.U)
    ptw_resp
  }

  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[io.sfence] ${io.sfence}\n")
  XSDebug(p"[io.csr] ${io.csr}\n")

  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"req_count${i}", io.tlb(i).req(0).fire())
    XSPerfAccumulate(s"req_blocked_count_${i}", io.tlb(i).req(0).valid && !io.tlb(i).req(0).ready)
  }
  XSPerfAccumulate(s"req_blocked_by_mq", arb1.io.out.valid && missQueue.io.cache.valid)
  XSPerfAccumulate(s"replay_again", cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.isReplay && !fsm.io.req.ready)
  XSPerfAccumulate(s"into_fsm_no_replay", cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.isReplay && fsm.io.req.ready)
  for (i <- 0 until (MemReqWidth + 1)) {
    XSPerfAccumulate(s"mem_req_util${i}", PopCount(waiting_resp) === i.U)
  }
  XSPerfAccumulate("mem_cycle", PopCount(waiting_resp) =/= 0.U)
  XSPerfAccumulate("mem_count", mem.a.fire())

  // print configs
  println(s"${l2tlbParams.name}: one ptw, miss queue size ${l2tlbParams.missQueueSize} l1:${l2tlbParams.l1Size} fa l2: nSets ${l2tlbParams.l2nSets} nWays ${l2tlbParams.l2nWays} l3: ${l2tlbParams.l3nSets} nWays ${l2tlbParams.l3nWays} blockBytes:${l2tlbParams.blockBytes}")

  // time out assert
  for (i <- 0 until MSHRSize + 1) {
    TimeOutAssert(waiting_resp(i), timeOutThreshold, s"ptw mem resp time out wait_resp${i}")
    TimeOutAssert(sfence_latch(i), timeOutThreshold, s"ptw mem resp time out sfence_latch${i}")
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

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwIO)

  for (i <- 0 until PtwWidth) {
    io.tlb(i).req(0).ready := true.B

    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.enable := io.tlb(i).req(0).valid
    helper.satp := io.csr.satp.ppn
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
  }
}

class PTWWrapper()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  val node = if (!useFakePTW) TLIdentityNode() else null
  val ptw = if (!useFakePTW) LazyModule(new PTW()) else null
  if (!useFakePTW) {
    node := ptw.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new PtwIO)
    if (useFakePTW) {
      val fake_ptw = Module(new FakePTW())
      io <> fake_ptw.io
    }
    else {
      io <> ptw.module.io
    }
  }
}
