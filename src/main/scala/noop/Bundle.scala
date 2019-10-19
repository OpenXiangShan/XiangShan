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
  val valid = Output(Bool())
}

class CtrlFlowIO extends NOOPBundle {
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(AddrBits.W))
  val pnpc = Output(UInt(AddrBits.W))
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
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
  val intrNO = Output(UInt(XLEN.W))
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

class TLBExuIO extends NOOPBundle with tlbConst{
  val satp = Output(UInt(XLEN.W))
  val sfence = new Bundle {
    val valid = Output(Bool())
    val asid  = Output(UInt(tlbAsidLen.W))
    val vaddr = Output(UInt(XLEN.W))
  }

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, satp: UInt) = {//func no use here for just sfence.vma only
    this.sfence.valid := valid
    this.sfence.vaddr := src1
    this.sfence.asid  := src2(tlbAsidLen-1,0)
    this.satp := satp
  }
}