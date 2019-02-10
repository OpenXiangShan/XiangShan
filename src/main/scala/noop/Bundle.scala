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
}

class DataPathIO extends Bundle {
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
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

class MemAddrBundle extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(3.W))
}

class MemDataBundle(val dataBits: Int = 32) extends Bundle {
  val data = Output(UInt(dataBits.W))
}

class MemIO(val dataBits: Int = 32) extends Bundle {
  val a = Valid(new MemAddrBundle)
  val r = Flipped(Valid(new MemDataBundle(dataBits)))
  val w = Valid(new MemDataBundle(dataBits))
}
