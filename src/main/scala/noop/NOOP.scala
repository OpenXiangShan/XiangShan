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
  val AddrBits = 64//32 //TODO: fix by lemover-zhangzifei 32->64
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
  idu.io.flush := ifu.io.flushVec(1)
  isu.io.flush := ifu.io.flushVec(2)
  exu.io.flush := ifu.io.flushVec(3)

  Debug() {
    printf("------------------------ TIMER: %d ------------------------\n", GTimer())
    printf("flush = %b, ifu:(%d,%d), idu:(%d,%d), isu:(%d,%d), exu:(%d,%d), wbu: (%d,%d)\n",
      ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      idu.io.in.valid, idu.io.in.ready, isu.io.in.valid, isu.io.in.ready,
      exu.io.in.valid, exu.io.in.ready, wbu.io.in.valid, wbu.io.in.ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr, ifu.io.out.bits.pnpc)} ; 
    when (idu.io.in.valid)  { printf("IBF: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.in.bits.pc, idu.io.in.bits.instr, idu.io.in.bits.pnpc)} ; 
    when (idu.io.out.valid) { printf("IDU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu.io.out.bits.cf.pc, idu.io.out.bits.cf.instr, idu.io.out.bits.cf.pnpc)} ; 
    when (isu.io.in.valid)  { printf("ISU: pc = 0x%x, pnpc = 0x%x\n", isu.io.in.bits.cf.pc, isu.io.in.bits.cf.pnpc)} ;
    when (exu.io.in.valid)  { printf("EXU: pc = 0x%x, pnpc = 0x%x\n", exu.io.in.bits.cf.pc, exu.io.in.bits.cf.pnpc)} ;
    when (wbu.io.in.valid)  { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x\n", wbu.io.in.bits.decode.cf.pc, wbu.io.in.bits.decode.ctrl.rfWen, wbu.io.in.bits.decode.ctrl.rfDest, wbu.io.wb.rfData, wbu.io.in.bits.decode.ctrl.fuType )}
    // when (io.in.valid) { printf("TIMER: %d WBU: pc = 0x%x wen %x wdata %x mmio %x intrNO %x\n", GTimer(), io.in.bits.decode.cf.pc, io.wb.rfWen, io.wb.rfData, io.in.bits.isMMIO, io.in.bits.intrNO) }
    
    // printf(p"IFUO: redirectIO:${ifu.io.out.bits.redirect}\n") ; printf("IFUO: exceptionVec: %x\n", ifu.io.out.bits.exceptionVec.asUInt)} 
    // printf(p"IDUO: redirectIO:${idu.io.out.bits.cf.redirect} redirectIOC:${idu.io.redirect}\n") ; printf("IDUO: exceptionVec:%x\n", idu.io.out.bits.cf.exceptionVec.asUInt)}
    // printf(p"ISUO: ${isu.io.out.bits.cf.redirect}\n") ; printf("ISUO: exceptionVec:%x\n", isu.io.out.bits.cf.exceptionVec.asUInt)}
    // printf(p"EXUO: ${exu.io.out.bits.decode.cf.redirect}\n") ; printf("EXUO: exceptionVecIn:%x\n", exu.io.in.bits.cf.exceptionVec.asUInt)}
    // when (wbu.io.in.valid) { printf("WBU: pc = 0x%x rfWen:%d rfDest:%d rfData:%x Futype:%x commits(0):%x commits(1):%x commits(3):%x\n", wbu.io.in.bits.decode.cf.pc, wbu.io.in.bits.decode.ctrl.rfWen, wbu.io.in.bits.decode.ctrl.rfDest, wbu.io.wb.rfData, wbu.io.in.bits.decode.ctrl.fuType, wbu.io.in.bits.commits(0), wbu.io.in.bits.commits(1), wbu.io.in.bits.commits(3)) }
    
  }

  isu.io.wb <> wbu.io.wb
  ifu.io.redirect <> wbu.io.redirect
  ifu.io.redirectRVC <> idu.io.redirect
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
  val itlb = Module(new TLB()(TLBConfig(name = "itlb", userBits = AddrBits*2 + 4 + 1)))
  val itran = Module(new TLBIOTran(userBits = AddrBits*2 + 4 + 1, name = "itran"))
  itlb.io.exu <> exu.io.tlb
  itlb.io.csrMMU <> exu.io.memMMU.imem
  itlb.io.flush := Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush)
  itlb.io.in.req <> ifu.io.imem.req
  itran.io.in.req <> itlb.io.in.resp
  ifu.io.imem.resp <> itran.io.in.resp
  io.imem <> Cache(itran.io.out, mmioXbar.io.in(0), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2 + 4 + 1))

  val dtlb = Module(new TLB()(TLBConfig(name = "dtlb")))
  val dtran = Module(new TLBIOTran(name = "dtran"))
  dtlb.io.exu <> exu.io.tlb
  dtlb.io.csrMMU <> exu.io.memMMU.dmem
  dtlb.io.flush := "b00".U //flush must be wrong
  dtlb.io.in.req <> exu.io.dmem.req
  dtran.io.in.req <> dtlb.io.in.resp
  exu.io.dmem.resp <> dtran.io.in.resp
  val tlbXbar = Module(new SimpleBusCrossbarNto1Special(3, name = "tlbXbar"))
  tlbXbar.io.in(0) <> dtran.io.out
  tlbXbar.io.in(1) <> dtlb.io.mem
  tlbXbar.io.in(2) <> itlb.io.mem
  io.dmem <> Cache(tlbXbar.io.out, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))

  io.mmio <> mmioXbar.io.out

/*
  io.imem <> Cache(ifu.io.imem, mmioXbar.io.in(0), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2 + 4)) // userBits = AddrBits + BrIdxBits
  io.dmem <> Cache(exu.io.dmem, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
  io.mmio <> mmioXbar.io.out
*/
}
