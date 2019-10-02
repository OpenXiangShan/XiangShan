package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class RegFile {
  val rf = Mem(32, UInt(32.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data }
}

class ScoreBoard {
  val busy = RegInit(0.U(32.W))
  def isBusy(idx: UInt): Bool = busy(idx)
  def mask(idx: UInt) = (1.U(32.W) << idx)(31, 0)
  def update(setMask: UInt, clearMask: UInt) = {
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit
    // when reg(i) is written and issued at the same cycle.
    busy := Cat(((busy & ~clearMask) | setMask)(31, 1), 0.U(1.W))
  }
}

class ISU(implicit val p: NOOPConfig) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DecodeIO))
    val out = Decoupled(new DecodeIO)
    val wb = Flipped(new WriteBackIO)
    val flush = Input(Bool())
    val forward = Flipped(new ForwardIO)
  })

  io.out.bits := DontCare
  val rfSrc1 = io.in.bits.ctrl.rfSrc1
  val rfSrc2 = io.in.bits.ctrl.rfSrc2
  val rfDest = io.in.bits.ctrl.rfDest

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen

  val forwardRfWen = io.forward.wb.rfWen && io.forward.valid
  val dontForward = (io.forward.fuType =/= FuType.alu) && (io.forward.fuType =/= FuType.lsu)
  val src1DependEX = isDepend(rfSrc1, io.forward.wb.rfDest, forwardRfWen)
  val src2DependEX = isDepend(rfSrc2, io.forward.wb.rfDest, forwardRfWen)
  val src1DependWB = isDepend(rfSrc1, io.wb.rfDest, io.wb.rfWen)
  val src2DependWB = isDepend(rfSrc2, io.wb.rfDest, io.wb.rfWen)

  val src1ForwardNextCycle = src1DependEX && !dontForward
  val src2ForwardNextCycle = src2DependEX && !dontForward
  val src1Forward = src1DependWB && Mux(dontForward, !src1DependEX, true.B)
  val src2Forward = src2DependWB && Mux(dontForward, !src2DependEX, true.B)

  val sb = new ScoreBoard
  val src1Ready = !sb.isBusy(rfSrc1) || src1ForwardNextCycle || src1Forward
  val src2Ready = !sb.isBusy(rfSrc2) || src2ForwardNextCycle || src2Forward
  io.out.valid := io.in.valid && src1Ready && src2Ready

  val rf = new RegFile
  io.out.bits.data.src1 := Mux1H(List(
    (io.in.bits.ctrl.src1Type === SrcType.pc) -> io.in.bits.cf.pc,
    src1ForwardNextCycle -> io.forward  .wb.rfData,
    (src1Forward && !src1ForwardNextCycle) -> io.wb.rfData,
    ((io.in.bits.ctrl.src1Type =/= SrcType.pc) && !src1ForwardNextCycle && !src1Forward) -> rf.read(rfSrc1)
  ))
  io.out.bits.data.src2 := Mux1H(List(
    (io.in.bits.ctrl.src2Type =/= SrcType.reg) -> io.in.bits.data.imm,
    src2ForwardNextCycle -> io.forward.wb.rfData,
    (src2Forward && !src2ForwardNextCycle) -> io.wb.rfData,
    ((io.in.bits.ctrl.src2Type === SrcType.reg) && !src2ForwardNextCycle && !src2Forward) -> rf.read(rfSrc2)
  ))
  io.out.bits.data.imm  := io.in.bits.data.imm

  io.out.bits.cf <> io.in.bits.cf
  io.out.bits.ctrl := io.in.bits.ctrl
  io.out.bits.ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits.ctrl.isSrc2Forward := src2ForwardNextCycle

  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfData) }

  val wbClearMask = Mux(io.wb.rfWen && !isDepend(io.wb.rfDest, io.forward.wb.rfDest, forwardRfWen), sb.mask(io.wb.rfDest), 0.U(32.W))
  val isuFireSetMask = Mux(io.out.fire(), sb.mask(rfDest), 0.U)
  when (io.flush) { sb.update(0.U, "hffffffff".U) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in.ready := !io.in.valid || io.out.fire()

  // read after write
  BoringUtils.addSource(io.in.valid && !io.out.valid, "perfCntCondMrawStall")
  BoringUtils.addSource(io.out.valid && !io.out.fire(), "perfCntCondMexuBusy")

  if (!p.FPGAPlatform) {
    BoringUtils.addSource(VecInit((0 to 31).map(i => rf.read(i.U))), "difftestRegs")
  }
}
