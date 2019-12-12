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
  val HasCExtension = true
  val HasDiv = true
  val HasIcache = true
  val HasDcache = true
  val EnableStoreQueue = false
  val AddrBits = 64 // AddrBits is used in some cases
  val VAddrBits = 39 // VAddrBits is Virtual Memory addr bits
  val PAddrBits = 32 // PAddrBits is Phyical Memory addr bits
  val AddrBytes = AddrBits / 8 // unused
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
    val frontend = Flipped(new SimpleBusUC)
  })

  val ifu  = Module(new IFU)
  val idu1 = Module(new IDU1)
  val idu2 = Module(new IDU2)
  val isu  = Module(new ISU)
  val exu  = Module(new EXU)
  val wbu  = Module(new WBU)

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 4, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

  pipelineConnect2(ifu.io.out, idu1.io.in, ifu.io.flushVec(0))
  PipelineConnect(idu1.io.out, idu2.io.in, idu2.io.out.fire(), ifu.io.flushVec(1))
  PipelineConnect(idu2.io.out, isu.io.in, isu.io.out.fire(), ifu.io.flushVec(1))
  PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire(), ifu.io.flushVec(2))
  PipelineConnect(exu.io.out, wbu.io.in, true.B, ifu.io.flushVec(3))
  idu1.io.flush := ifu.io.flushVec(1)
  idu2.io.flush := ifu.io.flushVec(1)
  isu.io.flush := ifu.io.flushVec(2)
  exu.io.flush := ifu.io.flushVec(3)

  Debug() {
    printf("------------------------ TIMER: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), idu1:(%d,%d), idu2:(%d,%d), isu:(%d,%d), exu:(%d,%d), wbu: (%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      idu1.io.in.valid, idu1.io.in.ready, idu2.io.in.valid, idu2.io.in.ready, isu.io.in.valid, isu.io.in.ready,
      exu.io.in.valid, exu.io.in.ready, wbu.io.in.valid, wbu.io.in.ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr, ifu.io.out.bits.pnpc)} ; 
    when (idu1.io.in.valid) { printf("ID1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu1.io.in.bits.pc, idu1.io.in.bits.instr, idu1.io.in.bits.pnpc) }
    when (idu2.io.in.valid) { printf("ID2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu2.io.in.bits.pc, idu2.io.in.bits.instr, idu2.io.in.bits.pnpc) }
    when (isu.io.in.valid)  { printf("ISU: pc = 0x%x, pnpc = 0x%x\n", isu.io.in.bits.cf.pc, isu.io.in.bits.cf.pnpc)} ;
    when (exu.io.in.valid)  { printf("EXU: pc = 0x%x, pnpc = 0x%x\n", exu.io.in.bits.cf.pc, exu.io.in.bits.cf.pnpc)} ;
    when (wbu.io.in.valid)  { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x\n", wbu.io.in.bits.decode.cf.pc, wbu.io.in.bits.decode.ctrl.rfWen, wbu.io.in.bits.decode.ctrl.rfDest, wbu.io.wb.rfData, wbu.io.in.bits.decode.ctrl.fuType )}
    // when (io.in.valid) { printf("TIMER: %d WBU: pc = 0x%x wen %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits.decode.cf.pc, io.wb.rfWen, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO) }
    
    // printf(p"IFUO: redirectIO:${ifu.io.out.bits.redirect}\n") ; printf("IFUO: exceptionVec: %x\n", ifu.io.out.bits.exceptionVec.asUInt)} 
    // printf(p"IDUO: redirectIO:${idu.io.out.bits.cf.redirect} redirectIOC:${idu.io.redirect}\n") ; printf("IDUO: exceptionVec:%x\n", idu.io.out.bits.cf.exceptionVec.asUInt)}
    // printf(p"ISUO: ${isu.io.out.bits.cf.redirect}\n") ; printf("ISUO: exceptionVec:%x\n", isu.io.out.bits.cf.exceptionVec.asUInt)}
    when (exu.io.out.bits.decode.cf.redirect.valid) { printf("EXUO: redirect valid:%d target:%x\n", exu.io.out.bits.decode.cf.redirect.valid, exu.io.out.bits.decode.cf.redirect.target) }
    // when (wbu.io.in.valid) { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x commits(0):%x commits(1):%x commits(3):%x\n", wbu.io.in.bits.decode.cf.pc, wbu.io.in.bits.decode.ctrl.rfWen, wbu.io.in.bits.decode.ctrl.rfDest, wbu.io.wb.rfData, wbu.io.in.bits.decode.ctrl.fuType, wbu.io.in.bits.commits(0), wbu.io.in.bits.commits(1), wbu.io.in.bits.commits(3)) }
    
  }

  isu.io.wb <> wbu.io.wb
  ifu.io.redirect <> wbu.io.redirect
  // forward
  isu.io.forward <> exu.io.forward

  val mmioXbar = Module(new SimpleBusCrossbarNto1(if (HasDcache) 2 else 3))
  val dmemXbar = Module(new SimpleBusCrossbarNto1(4))

  val itlb = TLB(in = ifu.io.imem, mem = dmemXbar.io.in(1), flush = ifu.io.flushVec(0) | ifu.io.bpFlush, csrMMU = exu.io.memMMU.imem)(TLBConfig(name = "itlb", userBits = VAddrBits*2 + 4, totalEntry = 4))
  ifu.io.ipf := itlb.io.ipf
  io.imem <> Cache(in = itlb.io.out, mmio = mmioXbar.io.in.take(1), flush = Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush), empty = itlb.io.cacheEmpty)(
    CacheConfig(ro = true, name = "icache", userBits = VAddrBits*2 + 4))
  
  val dtlb = TLB(in = exu.io.dmem, mem = dmemXbar.io.in(2), flush = false.B, csrMMU = exu.io.memMMU.dmem)(TLBConfig(name = "dtlb", totalEntry = 64))
  dmemXbar.io.in(0) <> dtlb.io.out
  io.dmem <> Cache(in = dmemXbar.io.out, mmio = mmioXbar.io.in.drop(1), flush = "b00".U, empty = dtlb.io.cacheEmpty, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

  // Make DMA access through L1 DCache to keep coherence
  dmemXbar.io.in(3) <> io.frontend

  io.mmio <> mmioXbar.io.out
}
