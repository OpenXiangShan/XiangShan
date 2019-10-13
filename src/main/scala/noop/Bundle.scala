package noop

import chisel3._
import chisel3.util._

class CtrlSignalIO extends NOOPBundle {
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

class DataSrcIO extends NOOPBundle {
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val imm  = Output(UInt(XLEN.W))
}

class RedirectIO extends NOOPBundle {
  val target = Output(UInt(AddrBits.W))
  val brIdx = Output(UInt(3.W)) // for RVC
  val valid = Output(Bool())
}

class IRIDCtrlFlowIO extends NOOPBundle {
  val instr = Output(UInt(64.W))
  val pc = Output(UInt(AddrBits.W))
  val pnpc = Output(UInt(AddrBits.W))
  val brIdx = Output(UInt(3.W))
  val redirect = new RedirectIO
}

class CtrlFlowIO extends NOOPBundle {
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(AddrBits.W))
  val pnpc = Output(UInt(AddrBits.W))
  val brIdx = Output(UInt(2.W))
  val redirect = new RedirectIO
}

class DecodeIO extends NOOPBundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

class WriteBackIO extends NOOPBundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfData = Output(UInt(XLEN.W))
}

class CommitIO extends NOOPBundle {
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val commits = Output(Vec(FuType.num, UInt(XLEN.W)))
}

class FunctionUnitIO extends NOOPBundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
}

class ForwardIO extends NOOPBundle {
  val valid = Output(Bool())
  val wb = new WriteBackIO
  val fuType = Output(FuType())
}
