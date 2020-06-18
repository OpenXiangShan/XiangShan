package xiangshan

import chisel3._
import chisel3.util._

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket extends XSBundle {
  val instrs = Vec(FetchWidth, UInt(32.W))
  val mask = UInt(FetchWidth.W)
  val pc = UInt(VAddrBits.W) // the pc of first inst in the fetch group
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val exceptionVec = Vec(16, Bool())
  val intrVec = Vec(12, Bool())
  val isRVC = Bool()
  val isBr = Bool()
}

// Decode DecodeWidth insts at Decode Stage
class CtrlSignals extends XSBundle {
  val src1Type, src2Type, src3Type = SrcType()
  val lsrc1, lsrc2, lsrc3 = UInt(5.W)
  val ldest = UInt(5.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val isXSTrap = Bool()
  val noSpecExec = Bool()  // This inst can not be speculated
  val isBlocked  = Bool()  // This inst requires pipeline to be blocked
}

class CfCtrl extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
  val brMask = UInt(BrqSize.W)
  val brTag = UInt(BrTagWidth.W)
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp extends CfCtrl {

  val psrc1, psrc2, psrc3, pdest, old_pdest = UInt(PhyRegIdxWidth.W)
  val src1State, src2State, src3State = SrcState()

  val roqIdx = UInt(RoqIdxWidth.W)
}

class Redirect extends XSBundle {
  val target = UInt(VAddrBits.W)
  val brTag = UInt(BrTagWidth.W)
}

class Dp1ToDp2IO extends XSBundle {
  val intDqToDp2 = Vec(IntDqDeqWidth, DecoupledIO(new MicroOp))
  val fpDqToDp2 = Vec(FpDqDeqWidth, DecoupledIO(new MicroOp))
  val lsDqToDp2 = Vec(LsDqDeqWidth, DecoupledIO(new MicroOp))
}


class ExuInput extends XSBundle {
  val uop = new MicroOp
  val src1, src2, src3 = UInt(XLEN.W)
  val isRVF = Bool()
}

class ExuOutput extends XSBundle {
  val uop = new MicroOp
  val data = UInt(XLEN.W)
}

class ExuIO extends XSBundle {
  val in = Flipped(DecoupledIO(new ExuInput))
  val out = DecoupledIO(new ExuOutput)
}

class RoqCommit extends XSBundle {
  val uop = new MicroOp
}

class FrontendToBackendIO extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  // from backend
  val redirect = Flipped(ValidIO(new Redirect))
  val commits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit))) // update branch pred
}