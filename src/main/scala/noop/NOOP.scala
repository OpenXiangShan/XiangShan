package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

trait HasNOOPParameter {
  val XLEN = 64
  val HasMExtension = true
  val HasDiv = true
  val HasIcache = true
  val HasDcache = true
  val AddrBits = 32
  val AddrBytes = AddrBits / 8
  val DataBits = XLEN
  val DataBytes = DataBits / 8
}

abstract class NOOPModule extends Module with HasNOOPParameter with HasExceptionNO
abstract class NOOPBundle extends Bundle with HasNOOPParameter

case class NOOPConfig (
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = false
)

object AddressSpace {
  // (start, size)
  def mmio = List((0x0000000040000000L, 0x0000000010000000L))
  def dram = (0x0000000080000000L, 0x0000000010000000L)

  //def isMMIO(addr: UInt) = mmio.map(range => ((addr & ~((range._2 - 1).U(32.W))) === range._1.U)).reduce(_ || _)
  def isMMIO(addr: UInt) = addr(31,28) === "h4".U
}

class NOOP(implicit val p: NOOPConfig) extends NOOPModule {
  val io = IO(new Bundle {
    val imem = new SimpleBusC
    val dmem = new SimpleBusC
    val mmio = new SimpleBusUC
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU)
  val wbu = Module(new WBU)

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

  pipelineConnect2(ifu.io.out, idu.io.in, ifu.io.flushVec(0))
  PipelineConnect(idu.io.out, isu.io.in, isu.io.out.fire(), ifu.io.flushVec(1))
  PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire(), ifu.io.flushVec(2))
  PipelineConnect(exu.io.out, wbu.io.in, true.B, ifu.io.flushVec(3))
  isu.io.flush := ifu.io.flushVec(2)
  exu.io.flush := ifu.io.flushVec(3)

  Debug() {
    printf("------------------------ TIMER: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), idu:(%d,%d), isu:(%d,%d), exu:(%d,%d), wbu: (%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      idu.io.in.valid, idu.io.in.ready, isu.io.in.valid, isu.io.in.ready,
      exu.io.in.valid, exu.io.in.ready, wbu.io.in.valid, wbu.io.in.ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr, ifu.io.out.bits.pnpc) }
    when (idu.io.in.valid) { printf("IDU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in.bits.pc, idu.io.in.bits.instr, idu.io.in.bits.pnpc) }
    when (isu.io.in.valid) { printf("ISU: pc = 0x%x, pnpc = 0x%x\n", isu.io.in.bits.cf.pc, isu.io.in.bits.cf.pnpc) }
    when (exu.io.in.valid) { printf("EXU: pc = 0x%x, pnpc = 0x%x\n", exu.io.in.bits.cf.pc, exu.io.in.bits.cf.pnpc) }
    when (wbu.io.in.valid) { printf("WBU: pc = 0x%x\n", wbu.io.in.bits.decode.cf.pc) }
  }

  isu.io.wb <> wbu.io.wb
  ifu.io.redirect <> wbu.io.redirect
  // forward
  isu.io.forward <> exu.io.forward

  val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
  
/*
  val iptw = Module(new Ptw(name = "iptw", userBits = AddrBits*2))
  //iptw.io.satp := exu.io.satp//"h8000000000087fbe".U//"h80087fbe".U
  iptw.io.exu <> exu.io.tlb
  iptw.io.flush := ifu.io.flushVec(0) | ifu.io.bpFlush
  iptw.io.in <> ifu.io.imem
  val ptwWork = exu.io.tlb.satp(63,60) =/= 0.U || true.B
  val inCacheFlush = Mux(ptwWork, Fill(2,false.B), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))
  io.imem <> Cache(iptw.io.out, mmioXbar.io.in(0), inCacheFlush)(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2))

  val dptw = Module(new Ptw(name = "dptw"))
  //dptw.io.satp := exu.io.satp//"h8000000000087fbe".U//"h80087fbe".U
  dptw.io.exu   <> exu.io.tlb
  dptw.io.flush := false.B
  dptw.io.in <> exu.io.dmem
  io.dmem <> Cache(dptw.io.out, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

  io.mmio <> mmioXbar.io.out
*/
  val itlb = Module(new TLB()(TLBConfig(name = "itlb", userBits = AddrBits*2)))
  val itran = Module(new TLBIOTran(userBits = AddrBits*2, name = "itran"))
  val icache = Module(new Cache()(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2)))
  val itlbXbar = Module(new SimpleBusCrossbarNto1Special(2, userBits = AddrBits*2, name = "itlbXbar"))
  itlb.io.exu <> exu.io.tlb
  itlb.io.flush := Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush)
  itlb.io.in.req <> ifu.io.imem.req
  itran.io.in.req <> itlb.io.in.resp
  ifu.io.imem.resp <> itran.io.in.resp
  
  icache.io.in <> itran.io.out
  icache.io.flush := Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush)
  mmioXbar.io.in(0) <> icache.io.mmio

  itlbXbar.io.flush := false.B//ifu.io.flushVec(0) | ifu.io.bpFlush
  itlbXbar.io.in(0) <> itlb.io.mem
  itlbXbar.io.in(1) <> icache.io.out.mem
  io.imem.mem <> itlbXbar.io.out
  io.imem.coh <> icache.io.out.coh
  //itlbXbar.io.in(1) <> itlb.io.mem
  //io.imem <> Cache(/*itlbXbar.io.out*/itran.io.out, mmioXbar.io.in(0), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))(
  //  CacheConfig(ro = true, name = "icache", userBits = AddrBits*2))

  val dtlb = Module(new TLB()(TLBConfig(name = "dtlb")))
  val dtran = Module(new TLBIOTran(name = "dtran"))
  dtlb.io.exu <> exu.io.tlb
  dtlb.io.flush := "b00".U //flush must be wrong
  dtlb.io.in.req <> exu.io.dmem.req
  dtran.io.in.req <> dtlb.io.in.resp
  exu.io.dmem.resp <> dtran.io.in.resp
  val dtlbXbar = Module(new SimpleBusCrossbarNto1(2))
  dtlbXbar.io.in(0) <> dtran.io.out
  dtlbXbar.io.in(1) <> dtlb.io.mem
  io.dmem <> Cache(dtlbXbar.io.out, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

  io.mmio <> mmioXbar.io.out

/*
  io.imem <> Cache(ifu.io.imem, mmioXbar.io.in(0), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2))
  io.dmem <> Cache(exu.io.dmem, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
  io.mmio <> mmioXbar.io.out
*/
}
