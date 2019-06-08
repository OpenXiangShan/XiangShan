package noop

import chisel3._
import chisel3.util._

import utils._

class RegFile {
  val rf = Mem(32, UInt(32.W))
  def read(addr: UInt) : UInt = Mux(addr === 0.U, 0.U, rf(addr))
  def write(addr: UInt, data: UInt) = { rf(addr) := data }
}

class ScoreBoard {
  val busy = RegInit(0.U(32.W))
  def isBusy(idx: UInt): Bool = busy(idx)
  def setMask(idx: UInt) = (1.U(32.W) << idx)(31, 0)
  def clearMask(idx: UInt) = ~setMask(idx)
  def update(setMask: UInt, clearMask: UInt) = {
    // When clearMask(i) and setMask(i) are both set, setMask(i) wins.
    // This can correctly record the busy bit
    // when reg(i) is written and issued at the same cycle.
    busy := Cat(((busy & ~clearMask) | setMask)(31, 1), 0.U(1.W))
  }
}

class ISU extends Module with HasSrcType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new PcCtrlDataIO))
    val out = Decoupled(new PcCtrlDataIO)
    val wb = Flipped(new WriteBackIO)
    val flush = Input(Bool())
    val forward = Flipped(new ForwardIO)
    val difftestRegs = Output(Vec(32, UInt(32.W)))
    val rawStall = Output(Bool())
    val exuBusy = Output(Bool())
  })

  io.out.bits := DontCare

  // make non-register addressing to zero, since sb.isBusy(0) === false.B
  val rfSrc1 = Mux(io.in.bits.ctrl.src1Type === Src1Pc, 0.U, io.in.bits.ctrl.rfSrc1)
  val rfSrc2 = Mux(io.in.bits.ctrl.src2Type === Src2Reg, io.in.bits.ctrl.rfSrc2, 0.U)
  val rfDest = Mux(io.in.bits.ctrl.rfWen, io.in.bits.ctrl.rfDest, 0.U)

  def isDepend(rfSrc: UInt, rfDest: UInt, wen: Bool): Bool = (rfSrc =/= 0.U) && (rfSrc === rfDest) && wen

  val forwardRfWen = io.forward.rfWen && io.forward.valid
  val dontForward = false.B
  val src1DependEX = isDepend(rfSrc1, io.forward.rfDest, forwardRfWen)
  val src2DependEX = isDepend(rfSrc2, io.forward.rfDest, forwardRfWen)
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
  val rs1Data = Mux(src1Forward, io.wb.rfWdata, rf.read(rfSrc1))
  val rs2Data = Mux(src2Forward, io.wb.rfWdata, rf.read(rfSrc2))
  io.out.bits.data.src1 := Mux(io.in.bits.ctrl.src1Type === Src1Pc, io.in.bits.pc, rs1Data)
  io.out.bits.data.src2 := Mux(io.in.bits.ctrl.src2Type === Src2Reg, rs2Data, io.in.bits.data.imm)
  io.out.bits.data.imm  := io.in.bits.data.imm
  io.out.bits.data.dest := DontCare

  (io.out.bits.ctrl, io.in.bits.ctrl) match { case (o, i) =>
    o.fuType := i.fuType
    o.fuOpType := i.fuOpType
    o.rfWen := i.rfWen
    o.rfDest := i.rfDest
    o.isInvOpcode := i.isInvOpcode
    o.isNoopTrap := i.isNoopTrap
  }
  io.out.bits.pc := io.in.bits.pc
  io.out.bits.ctrl.isSrc1Forward := src1ForwardNextCycle
  io.out.bits.ctrl.isSrc2Forward := src2ForwardNextCycle

  when (io.wb.rfWen) { rf.write(io.wb.rfDest, io.wb.rfWdata) }

  val wbClearMask = Mux(io.wb.rfWen && (rfDest === io.wb.rfDest), sb.clearMask(io.wb.rfDest), "hffffffff".U)
  val isuFireSetMask = Mux(io.out.fire(), sb.setMask(rfDest), 0.U)
  when (io.flush && io.forward.rfWen) { sb.update(0.U, "hffffffff".U) }
  .otherwise { sb.update(isuFireSetMask, wbClearMask) }

  io.in.ready := !io.in.valid || io.out.fire()

  // read after write
  io.rawStall := io.in.valid && !io.out.valid
  io.exuBusy := io.out.valid && !io.out.fire()

  io.difftestRegs.zipWithIndex.map{ case (r, i) => r := rf.read(i.U) }
}
