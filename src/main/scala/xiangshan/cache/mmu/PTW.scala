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
import chisel3.util._
import Chisel.BlackBox
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

class PTW()(implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw"
    ))
  )))

  lazy val module = new PTWImp(this)
}

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

  val missQueue = Module(new PtwMissQueue)
  val cache = Module(new PtwCache)
  val fsm = Module(new PtwFsm)
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
  }, 2))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwResp, 2)).io)

  // NOTE: when cache out but miss and fsm doesnt accept,
  val blockNewReq = false.B
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))
  arb1.io.out.ready := arb2.io.in(1).ready && !blockNewReq

  val blockMissQueue = !fsm.io.req.ready
  block_decoupled(missQueue.io.out, arb2.io.in(0), blockMissQueue)
  arb2.io.in(1).valid := arb1.io.out.valid && !blockNewReq
  arb2.io.in(1).bits.vpn := arb1.io.out.bits.vpn
  arb2.io.in(1).bits.source := arb1.io.chosen
  arb2.io.out.ready := cache.io.req.ready

  cache.io.req.valid := arb2.io.out.valid
  cache.io.req.bits.vpn := arb2.io.out.bits.vpn
  cache.io.req.bits.source := arb2.io.out.bits.source
  cache.io.req.bits.isReplay := arb2.io.chosen === 0.U
  cache.io.sfence := sfence
  cache.io.refuseRefill := fsm.io.sfenceLatch
  cache.io.resp.ready := Mux(cache.io.resp.bits.hit, true.B, missQueue.io.in.ready || fsm.io.req.ready)

  missQueue.io.in.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && !fsm.io.req.ready
  missQueue.io.in.bits.vpn := cache.io.resp.bits.vpn
  missQueue.io.in.bits.source := cache.io.resp.bits.source
  missQueue.io.sfence  := sfence

  // NOTE: missQueue req has higher priority
  fsm.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit
  fsm.io.req.bits.source := cache.io.resp.bits.source
  fsm.io.req.bits.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  fsm.io.req.bits.l2Hit := cache.io.resp.bits.toFsm.l2Hit
  fsm.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  fsm.io.req.bits.vpn := cache.io.resp.bits.vpn
  fsm.io.csr := csr
  fsm.io.sfence := sfence
  fsm.io.resp.ready := MuxLookup(fsm.io.resp.bits.source, false.B,
    (0 until PtwWidth).map(i => i.U -> outArb(i).in(1).ready))

  // mem
  val memRead =  edge.Get(
    fromSource = 0.U/*id*/,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = Cat(fsm.io.mem.req.bits.addr(PAddrBits - 1, log2Up(l2tlbParams.blockBytes)), 0.U(log2Up(l2tlbParams.blockBytes).W)),
    lgSize     = log2Up(l2tlbParams.blockBytes).U
  )._2
  mem.a.bits := memRead
  mem.a.valid := fsm.io.mem.req.valid
  mem.d.ready := true.B

  val refill_data = Reg(Vec(blockBits / l1BusDataWidth, UInt(l1BusDataWidth.W)))
  val refill_helper = edge.firstlastHelper(mem.d.bits, mem.d.fire())
  val refill_valid = RegNext(refill_helper._3)
  when (mem.d.valid) {
    refill_data(refill_helper._4) := mem.d.bits.data
  }

  def get_part(data: Vec[UInt], index: UInt): UInt = {
    val inner_data = data.asTypeOf(Vec(data.getWidth / XLEN, UInt(XLEN.W)))
    inner_data(index)
  }

  val req_addr_low = fsm.io.mem.req.bits.addr(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
  fsm.io.mem.req.ready := mem.a.ready
  fsm.io.mem.resp.valid := refill_valid
  fsm.io.mem.resp.bits := get_part(refill_data, req_addr_low)
  cache.io.refill.valid := refill_valid
  cache.io.refill.bits.ptes := refill_data.asUInt
  cache.io.refill.bits.vpn  := fsm.io.refill.vpn
  cache.io.refill.bits.level := fsm.io.refill.level
  cache.io.refill.bits.memAddr := fsm.io.refill.memAddr

  for (i <- 0 until PtwWidth) {
    outArb(i).in(0).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.source===i.U
    outArb(i).in(0).bits.entry := cache.io.resp.bits.toTlb
    outArb(i).in(0).bits.pf := false.B
    outArb(i).in(1).valid := fsm.io.resp.valid && fsm.io.resp.bits.source===i.U
    outArb(i).in(1).bits := fsm.io.resp.bits.resp
  }

  // io.tlb.map(_.resp) <> outArb.map(_.out)
  io.tlb.map(_.resp).zip(outArb.map(_.out)).map{
    case (resp, out) => resp <> out
  }
  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
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
  XSPerfAccumulate(s"req_blocked_by_mq", arb1.io.out.valid && missQueue.io.out.valid)
  XSPerfAccumulate(s"replay_again", cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.isReplay && !fsm.io.req.ready)
  XSPerfAccumulate(s"into_fsm_no_replay", cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.isReplay && fsm.io.req.ready)

  // print configs
  println(s"${l2tlbParams.name}: one ptw, miss queue size ${l2tlbParams.missQueueSize} l1:${l2tlbParams.l1Size} fa l2: nSets ${l2tlbParams.l2nSets} nWays ${l2tlbParams.l2nWays} l3: ${l2tlbParams.l3nSets} nWays ${l2tlbParams.l3nWays} blockBytes:${l2tlbParams.blockBytes}")
}

class PTEHelper() extends BlackBox {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val enable = Input(Bool())
    val satp   = Input(UInt(64.W))
    val vpn    = Input(UInt(64.W))
    val pte    = Output(UInt(64.W))
    val level  = Output(UInt(8.W))
    val pf     = Output(UInt(8.W))
  })
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwIO)

  for (i <- 0 until PtwWidth) {
    io.tlb(i).req(0).ready := true.B

    val helper = Module(new PTEHelper())
    helper.io.clock := clock
    helper.io.enable := io.tlb(i).req(0).valid
    helper.io.satp := io.csr.satp.ppn
    helper.io.vpn := io.tlb(i).req(0).bits.vpn
    val pte = helper.io.pte.asTypeOf(new PteBundle)
    val level = helper.io.level
    val pf = helper.io.pf

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
