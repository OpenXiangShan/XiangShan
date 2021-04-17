package difftest

import chisel3._
import chisel3.util._


trait DifftestParameter {
}

trait DifftestWithClock {
  val clock  = Input(Clock())
}

trait DifftestWithCoreid {
  val coreid = Input(UInt(8.W))
}

trait DifftestWithIndex {
  val index = Input(UInt(8.W))
}

abstract class DifftestBundle extends Bundle
  with DifftestParameter
  with DifftestWithClock
  with DifftestWithCoreid

class DiffArchEventIO extends DifftestBundle {
  val intrNO = Input(UInt(32.W))
  val cause = Input(UInt(32.W))
  val exceptionPC = Input(UInt(64.W))
}

class DiffInstrCommitIO extends DifftestBundle with DifftestWithIndex {
  val valid    = Input(Bool())
  val pc       = Input(UInt(64.W))
  val instr    = Input(UInt(32.W))
  val skip     = Input(Bool())
  val isRVC    = Input(Bool())
  val scFailed = Input(Bool())
  val wen      = Input(Bool())
  val wdata    = Input(UInt(64.W))
  val wdest    = Input(UInt(8.W))
}

class DiffTrapEventIO extends DifftestBundle {
  val valid    = Input(Bool())
  val code     = Input(UInt(3.W))
  val pc       = Input(UInt(64.W))
  val cycleCnt = Input(UInt(64.W))
  val instrCnt = Input(UInt(64.W))
}

class DiffCSRStateIO extends DifftestBundle {
  val priviledgeMode = Input(UInt(2.W))
  val mstatus = Input(UInt(64.W))
  val sstatus = Input(UInt(64.W))
  val mepc = Input(UInt(64.W))
  val sepc = Input(UInt(64.W))
  val mtval = Input(UInt(64.W))
  val stval = Input(UInt(64.W))
  val mtvec = Input(UInt(64.W))
  val stvec = Input(UInt(64.W))
  val mcause = Input(UInt(64.W))
  val scause = Input(UInt(64.W))
  val satp = Input(UInt(64.W))
  val mip = Input(UInt(64.W))
  val mie = Input(UInt(64.W))
  val mscratch = Input(UInt(64.W))
  val sscratch = Input(UInt(64.W))
  val mideleg = Input(UInt(64.W))
  val medeleg = Input(UInt(64.W))
}

class DiffArchIntRegStateIO extends DifftestBundle {
  val gpr = Input(Vec(32, UInt(64.W)))
}

class DiffArchFpRegStateIO extends DifftestBundle {
  val fpr  = Input(Vec(32, UInt(64.W)))
}

class DiffSbufferEventIO extends DifftestBundle {
  val sbufferResp = Input(Bool())
  val sbufferAddr = Input(UInt(64.W))
  val sbufferData = Input(Vec(64, UInt(8.W)))
  val sbufferMask = Input(UInt(64.W))
}

class DiffStoreEventIO extends DifftestBundle with DifftestWithIndex {
  val valid       = Input(Bool())
  val storeAddr   = Input(UInt(64.W))
  val storeData   = Input(UInt(64.W))
  val storeMask   = Input(UInt(8.W))
}

class DiffLoadEventIO extends DifftestBundle with DifftestWithIndex {
  val valid  = Input(Bool())
  val paddr  = Input(UInt(64.W))
  val opType = Input(UInt(8.W))
  val fuType = Input(UInt(8.W))
}

class DiffAtomicEventIO extends DifftestBundle {
  val atomicResp = Input(Bool())
  val atomicAddr = Input(UInt(64.W))
  val atomicData = Input(UInt(64.W))
  val atomicMask = Input(UInt(8.W))
  val atomicFuop = Input(UInt(8.W))
  val atomicOut  = Input(UInt(64.W))
}

class DiffPtwEventIO extends DifftestBundle {
  val ptwResp = Input(Bool())
  val ptwAddr = Input(UInt(64.W))
  val ptwData = Input(Vec(4, UInt(64.W)))
}

class DifftestArchEvent extends BlackBox {
  val io = IO(new DiffArchEventIO)
}

class DifftestInstrCommit extends BlackBox {
  val io = IO(new DiffInstrCommitIO)
}

class DifftestTrapEvent extends BlackBox {
  val io = IO(new DiffTrapEventIO)
}

class DifftestCSRState extends BlackBox {
  val io = IO(new DiffCSRStateIO)
}

class DifftestArchIntRegState extends BlackBox {
  val io = IO(new DiffArchIntRegStateIO)
}

class DifftestArchFpRegState extends BlackBox {
  val io = IO(new DiffArchFpRegStateIO)
}

class DifftestSbufferEvent extends BlackBox {
  val io = IO(new DiffSbufferEventIO)
}

class DifftestStoreEvent extends BlackBox {
  val io = IO(new DiffStoreEventIO)
}

class DifftestLoadEvent extends BlackBox {
  val io = IO(new DiffLoadEventIO)
}

class DifftestAtomicEvent extends BlackBox {
  val io = IO(new DiffAtomicEventIO)
}

class DifftestPtwEvent extends BlackBox {
  val io = IO(new DiffPtwEventIO)
}
