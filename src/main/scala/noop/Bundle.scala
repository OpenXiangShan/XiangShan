package noop

import chisel3._
import chisel3.util._

class CtrlPathIO extends Bundle
    with HasDecodeConst {
  val src1Type = Output(UInt(Src1TypeWidth))
  val src2Type = Output(UInt(Src2TypeWidth))
  val fuType = Output(UInt(FuTypeWidth))
  val fuOpType = Output(UInt(FuOpTypeWidth))
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isInvOpcode = Output(Bool())
  val isNoopTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
}

class DataPathIO extends Bundle {
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
  val imm  = Output(UInt(32.W))
  val dest = Output(UInt(32.W))
}

class PcInstrIO extends Bundle {
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(32.W))
}

class PcCtrlDataIO extends Bundle {
  val pc = Output(UInt(32.W))
  val ctrl = new CtrlPathIO
  val data = new DataPathIO
}

class WriteBackIO extends Bundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfWdata = Output(UInt(32.W))
}

class BranchIO extends Bundle {
  val isTaken = Output(Bool())
  val target = Output(UInt(32.W))
}

class FunctionUnitIO extends Bundle with HasDecodeConst {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(32.W))
    val src2 = Output(UInt(32.W))
    val func = Output(UInt(FuOpTypeWidth))
  }))
  val out = Decoupled(Output(UInt(32.W)))
}

class ForwardIO extends Bundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val fire = Output(Bool())
}
