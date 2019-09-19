package noop

import chisel3._
import chisel3.util._

class CtrlSignalIO extends Bundle {
  val src1Type = Output(SrcType())
  val src2Type = Output(SrcType())
  val fuType = Output(FuType())
  val fuOpType = Output(FuOpType())
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isInvOpcode = Output(Bool())
  val isNoopTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
}

class DataSrcIO extends Bundle {
  val src1 = Output(UInt(64.W))
  val src2 = Output(UInt(64.W))
  val imm  = Output(UInt(64.W))
}

class RedirectIO extends Bundle {
  val target = Output(UInt(64.W))
  val valid = Output(Bool())
}

class CtrlFlowIO extends Bundle {
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(64.W))
  val pnpc = Output(UInt(64.W))
  val redirect = new RedirectIO
}

class DecodeIO extends Bundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

class WriteBackIO extends Bundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfData = Output(UInt(64.W))
}

class CommitIO extends Bundle {
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val commits = Output(Vec(FuType.num, UInt(64.W)))
}

class FunctionUnitIO extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(64.W))
    val src2 = Output(UInt(64.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(64.W)))
}

class ForwardIO extends Bundle {
  val valid = Output(Bool())
  val wb = new WriteBackIO
  val fuType = Output(FuType())
}
