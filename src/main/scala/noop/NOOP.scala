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
  val HasL2cache = true
	val HasPrefetch = false
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
		val prefetchReq = Decoupled(new SimpleBusReqBundle)
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
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr, ifu.io.out.bits.pnpc) }
    when (idu1.io.in.valid) { printf("ID1: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu1.io.in.bits.pc, idu1.io.in.bits.instr, idu1.io.in.bits.pnpc) }
    when (idu2.io.in.valid) { printf("ID2: pc = 0x%x, instr = 0x%x, pnpc = 0x%x\n", idu2.io.in.bits.pc, idu2.io.in.bits.instr, idu2.io.in.bits.pnpc) }
    when (isu.io.in.valid) { printf("ISU: pc = 0x%x, pnpc = 0x%x\n", isu.io.in.bits.cf.pc, isu.io.in.bits.cf.pnpc) }
    when (exu.io.in.valid) { printf("EXU: pc = 0x%x, pnpc = 0x%x\n", exu.io.in.bits.cf.pc, exu.io.in.bits.cf.pnpc) }
		when (wbu.io.in.valid) { printf("WBU: pc = 0x%x\n", wbu.io.in.bits.decode.cf.pc) }
  }

  isu.io.wb <> wbu.io.wb
  ifu.io.redirect <> wbu.io.redirect
  // forward
  isu.io.forward <> exu.io.forward

  val mmioXbar = Module(new SimpleBusCrossbarNto1(2))
  io.imem <> Cache(ifu.io.imem, mmioXbar.io.in(0), Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush))(
    CacheConfig(ro = true, name = "icache", userBits = AddrBits*2 + 4)) // userBits = AddrBits + BrIdxBits
  io.dmem <> Cache(exu.io.dmem, mmioXbar.io.in(1), "b00".U, enable = HasDcache)(CacheConfig(ro = false, name = "dcache"))
	io.prefetchReq.bits := exu.io.dmem.req.bits
	io.prefetchReq.valid := exu.io.dmem.req.valid
  io.mmio <> mmioXbar.io.out
}
