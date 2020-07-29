package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import xiangshan.backend.brq.BrqPtr
import xiangshan.backend.rename.FreeListPtr

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket extends XSBundle {
  val instrs = Vec(FetchWidth, UInt(32.W))
  val mask = UInt((FetchWidth*2).W)
  val pc = UInt(VAddrBits.W) // the pc of first inst in the fetch group
  val pnpc = Vec(FetchWidth*2, UInt(VAddrBits.W))
  val hist = Vec(FetchWidth*2, UInt(HistoryLength.W))
  // val btbVictimWay = UInt(log2Up(BtbWays).W)
  val predCtr = Vec(FetchWidth*2, UInt(2.W))
  val btbHit = Vec(FetchWidth*2, Bool())
  val tageMeta = Vec(FetchWidth*2, (new TageMeta))
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
}


class ValidUndirectioned[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen.asInstanceOf[T]
  override def cloneType = new ValidUndirectioned(gen).asInstanceOf[this.type]
}

object ValidUndirectioned {
  def apply[T <: Data](gen: T) = {
    new ValidUndirectioned[T](gen)
  }
}

class TageMeta extends XSBundle {
//  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
//  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
}

// Branch prediction result from BPU Stage1 & 3
class BranchPrediction extends XSBundle {
  val redirect = Bool()

  // mask off all the instrs after the first redirect instr
  val instrValid = Vec(FetchWidth*2, Bool())
  // target of the first redirect instr in a fetch package
  val target = UInt(VAddrBits.W)
  val lateJump = Bool()
  // save these info in brq!
  // global history of each valid(or uncancelled) instruction, excluding branch's own prediction result
  val hist = Vec(FetchWidth*2, UInt(HistoryLength.W))
  // victim way when updating btb
  // val btbVictimWay = UInt(log2Up(BtbWays).W)
  // 2-bit saturated counter 
  val predCtr = Vec(FetchWidth*2, UInt(2.W))
  val btbHit = Vec(FetchWidth*2, Bool())
  // tage meta info
  val tageMeta = Vec(FetchWidth*2, (new TageMeta))
  // ras checkpoint, only used in Stage3
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
}

// Save predecode info in icache
class Predecode extends XSBundle {
  val mask = UInt((FetchWidth*2).W)
  val isRVC = Vec(FetchWidth*2, Bool())
  val fuTypes = Vec(FetchWidth*2, FuType())
  val fuOpTypes = Vec(FetchWidth*2, FuOpType())
}


class BranchUpdateInfo extends XSBundle {
  val fetchOffset = UInt(log2Up(FetchWidth * 4).W)
  val pnpc = UInt(VAddrBits.W)
  val brTarget = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val btbPredCtr = UInt(2.W)
  val btbHit = Bool()
  val tageMeta = new TageMeta
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
  val taken = Bool()
  val fetchIdx = UInt(log2Up(FetchWidth*2).W)
  val btbType = UInt(2.W)
  val isRVC = Bool()
  val isBr = Bool()
  val isMisPred = Bool()
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val exceptionVec = Vec(16, Bool())
  val intrVec = Vec(12, Bool())
  val brUpdate = new BranchUpdateInfo
  val crossPageIPFFix = Bool()
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
  val isRVF = Bool()
  val imm = UInt(XLEN.W)
}

class CfCtrl extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
  val brTag = new BrqPtr
}

trait HasRoqIdx { this: HasXSParameter =>
  val roqIdx = UInt(RoqIdxWidth.W)
  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && Mux(
      this.roqIdx.head(1) === redirect.bits.roqIdx.head(1),
      this.roqIdx.tail(1) > redirect.bits.roqIdx.tail(1),
      this.roqIdx.tail(1) < redirect.bits.roqIdx.tail(1)
    )
  }
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp extends CfCtrl with HasRoqIdx {
  val psrc1, psrc2, psrc3, pdest, old_pdest = UInt(PhyRegIdxWidth.W)
  val src1State, src2State, src3State = SrcState()
}

class Redirect extends XSBundle with HasRoqIdx {
  val isException = Bool()
  val isMisPred = Bool()
  val isReplay = Bool()
  val pc = UInt(VAddrBits.W)
  val target = UInt(VAddrBits.W)
  val brTag = new BrqPtr
}

class Dp1ToDp2IO extends XSBundle {
  val intDqToDp2 = Vec(IntDqDeqWidth, DecoupledIO(new MicroOp))
  val fpDqToDp2 = Vec(FpDqDeqWidth, DecoupledIO(new MicroOp))
  val lsDqToDp2 = Vec(LsDqDeqWidth, DecoupledIO(new MicroOp))
}

class DebugBundle extends XSBundle{
  val isMMIO = Bool()
}

class ExuInput extends XSBundle {
  val uop = new MicroOp
  val src1, src2, src3 = UInt(XLEN.W)
}

class ExuOutput extends XSBundle {
  val uop = new MicroOp
  val data = UInt(XLEN.W)
  val redirectValid = Bool()
  val redirect = new Redirect
  val brUpdate = new BranchUpdateInfo
  val debug = new DebugBundle
}

class ExuIO extends XSBundle {
  val in = Flipped(DecoupledIO(new ExuInput))
  val redirect = Flipped(ValidIO(new Redirect))
  val out = DecoupledIO(new ExuOutput)
  // for csr
  val exception = Flipped(ValidIO(new MicroOp))
  // for Lsu
  val dmem = new SimpleBusUC
  val scommit = Input(UInt(3.W))
}

class RoqCommit extends XSBundle {
  val uop = new MicroOp
  val isWalk = Bool()
}

class FrontendToBackendIO extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  // from backend
  val redirect = Flipped(ValidIO(new Redirect))
  val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
}
