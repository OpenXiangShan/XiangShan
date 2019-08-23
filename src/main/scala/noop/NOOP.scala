package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus.{SimpleBus, SimpleBusCrossbar}
import bus.axi4._
import utils._

trait NOOPConfig {
  val HasIcache = true
  val HasDcache = true
  val HasMExtension = true
  val HasDiv = true
  val debug = false
}

object AddressSpace {
  // (start, size)
  def mmio = List((0x40000000L, 0x10000000L))
  def dram = (0x80000000L, 0x10000000L)

  //def isMMIO(addr: UInt) = mmio.map(range => ((addr & ~((range._2 - 1).U(32.W))) === range._1.U)).reduce(_ || _)
  def isMMIO(addr: UInt) = addr(31,28) === "h4".U
}

class NOOP(hasPerfCnt: Boolean = false) extends Module with NOOPConfig with HasCSRConst with HasFuType {
  val io = IO(new Bundle {
    val imem = new AXI4
    val dmem = new AXI4
    val mmio = new SimpleBus
    val difftest = new DiffTestIO
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU(hasPerfCnt))
  val wbu = Module(new WBU)

  ifu.io.bpu1Update := exu.io.bpu1Update

  io.imem <> (if (HasIcache) {
    val icache = Module(new Cache(ro = true, name = "icache", userBits = 32))
    icache.io.in <> ifu.io.imem
    icache.io.flush := Fill(2, ifu.io.flushVec(0) | ifu.io.bpFlush)
    ifu.io.pc := icache.io.addr
    icache.io.mem
  } else { ifu.io.imem.toAXI4() })

  def pipelineConnect2[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T],
    isFlush: Bool, entries: Int = 2, pipe: Boolean = false) = {
    right <> FlushableQueue(left, isFlush,  entries = entries, pipe = pipe)
  }

  pipelineConnect2(ifu.io.out, idu.io.in, ifu.io.flushVec(0))
  PipelineConnect(idu.io.out, isu.io.in, isu.io.out.fire(), ifu.io.flushVec(1))
  PipelineConnect(isu.io.out, exu.io.in, exu.io.out.fire(), ifu.io.flushVec(2))
  PipelineConnect(exu.io.out, wbu.io.in, true.B, ifu.io.flushVec(3))
  isu.io.flush := ifu.io.flushVec(2)
  exu.io.flush := ifu.io.flushVec(3)

  if (debug) {
    printf("%d: flush = %b, ifu:(%d,%d), idu:(%d,%d), isu:(%d,%d), exu:(%d,%d), wbu: (%d,%d)\n",
      GTimer(), ifu.io.flushVec.asUInt, ifu.io.out.valid, ifu.io.out.ready,
      idu.io.in.valid, idu.io.in.ready, isu.io.in.valid, isu.io.in.ready,
      exu.io.in.valid, exu.io.in.ready, wbu.io.in.valid, wbu.io.in.ready)
    when (ifu.io.out.valid) { printf("IFU: pc = 0x%x, instr = 0x%x\n", ifu.io.out.bits.pc, ifu.io.out.bits.instr) }
    when (idu.io.in.valid) { printf("IDU: pc = 0x%x, instr = 0x%x\n", idu.io.in.bits.pc, idu.io.in.bits.instr) }
    when (isu.io.in.valid) { printf("ISU: pc = 0x%x\n", isu.io.in.bits.pc) }
    when (exu.io.in.valid) { printf("EXU: pc = 0x%x\n", exu.io.in.bits.pc) }
    when (wbu.io.in.valid) { printf("WBU: pc = 0x%x\n", wbu.io.in.bits.pc) }
  }

  isu.io.wb <> wbu.io.wb
  ifu.io.br <> wbu.io.brOut
  // forward
  isu.io.forward <> exu.io.forward
  exu.io.wbData := wbu.io.wb.rfWdata

  io.dmem <> (if (HasDcache) {
    val dcache = Module(new Cache(ro = false, name = "dcache"))
    dcache.io.in <> exu.io.dmem
    dcache.io.flush := Fill(2, false.B)
    dcache.io.mem
  } else { exu.io.dmem.toAXI4() })
  io.mmio <> exu.io.mmio

  // monitor
  val mon = Module(new Monitor)
  val nooptrap = exu.io.in.bits.ctrl.isNoopTrap && exu.io.in.valid
  val cycleCnt = WireInit(0.U(32.W))
  val instrCnt = WireInit(0.U(32.W))
  mon.io.clk := clock
  mon.io.isNoopTrap := nooptrap
  mon.io.reset := reset.asBool
  mon.io.trapCode := exu.io.in.bits.data.src1
  mon.io.trapPC := exu.io.in.bits.pc
  mon.io.cycleCnt := cycleCnt
  mon.io.instrCnt := instrCnt

  BoringUtils.addSink(cycleCnt, "simCycleCnt")
  BoringUtils.addSink(instrCnt, "simInstrCnt")
  BoringUtils.addSource(nooptrap, "nooptrap")

  // difftest
  // latch writeback signal to let register files and pc update
  io.difftest.commit := RegNext(wbu.io.writeback)
  isu.io.difftestRegs.zipWithIndex.map { case(r, i) => io.difftest.r(i) := r }
  io.difftest.thisPC := RegNext(wbu.io.in.bits.pc)
  io.difftest.isMMIO := RegNext(wbu.io.in.bits.isMMIO)
}
