package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import xiangshan.backend.brq.BrqPtr
import xiangshan.backend.rename.FreeListPtr
import xiangshan.frontend.PreDecodeInfo

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val mask = UInt(PredictWidth.W)
  // val pc = UInt(VAddrBits.W)
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val pnpc = Vec(PredictWidth, UInt(VAddrBits.W))
  val brInfo = Vec(PredictWidth, new BranchInfo)
  val pd = Vec(PredictWidth, new PreDecodeInfo)
}

class ValidUndirectioned[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen.cloneType.asInstanceOf[T]
  override def cloneType = new ValidUndirectioned(gen).asInstanceOf[this.type]
}

object ValidUndirectioned {
  def apply[T <: Data](gen: T) = {
    new ValidUndirectioned[T](gen)
  }
}

class TageMeta extends XSBundle {
  def TageNTables = 6
  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
}

class BranchPrediction extends XSBundle {
  val redirect = Bool()
  val taken = Bool()
  val jmpIdx = UInt(log2Up(PredictWidth).W)
  val hasNotTakenBrs = Bool()
  val target = UInt(VAddrBits.W)
  val saveHalfRVI = Bool()
}

class BranchInfo extends XSBundle {
  val ubtbWriteWay = UInt(log2Up(UBtbWays).W)
  val ubtbHits = Bool()
  val btbWriteWay = UInt(log2Up(BtbWays).W)
  val btbHitJal = Bool()
  val bimCtr = UInt(2.W)
  val histPtr = UInt(log2Up(ExtHistoryLength).W)
  val tageMeta = new TageMeta
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
  val fetchIdx = UInt(log2Up(PredictWidth).W)

  val debug_ubtb_cycle = UInt(64.W)
  val debug_btb_cycle = UInt(64.W)
  val debug_tage_cycle = UInt(64.W)

  def apply(histPtr: UInt, tageMeta: TageMeta, rasSp: UInt, rasTopCtr: UInt) = {
    this.histPtr := histPtr
    this.tageMeta := tageMeta
    this.rasSp := rasSp
    this.rasTopCtr := rasTopCtr
    this.asUInt
  }
  def size = 0.U.asTypeOf(this).getWidth
  def fromUInt(x: UInt) = x.asTypeOf(this)
}

class Predecode extends XSBundle {
  val isFetchpcEqualFirstpc = Bool()
  val mask = UInt((FetchWidth*2).W)
  val pd = Vec(FetchWidth*2, (new PreDecodeInfo))
}

class BranchUpdateInfo extends XSBundle {
  // from backend
  val pc = UInt(VAddrBits.W)
  val pnpc = UInt(VAddrBits.W)
  val target = UInt(VAddrBits.W)
  val brTarget = UInt(VAddrBits.W)
  val taken = Bool()
  val fetchIdx = UInt(log2Up(FetchWidth*2).W)
  val isMisPred = Bool()

  // frontend -> backend -> frontend
  val pd = new PreDecodeInfo
  val brInfo = new BranchInfo
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
