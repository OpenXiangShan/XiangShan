package xiangshan

import chisel3._
import chisel3.util._
import bus.simplebus._
import xiangshan.backend.brq.BrqPtr
import xiangshan.backend.rename.FreeListPtr
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.HasBPUParameter

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val mask = UInt(PredictWidth.W)
  // val pc = UInt(VAddrBits.W)
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val pnpc = Vec(PredictWidth, UInt(VAddrBits.W))
  val brInfo = Vec(PredictWidth, new BranchInfo)
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val ipf = Bool()
  val crossPageIPFFix = Bool()
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

class BranchInfo extends XSBundle with HasBPUParameter {
  val ubtbWriteWay = UInt(log2Up(UBtbWays).W)
  val ubtbHits = Bool()
  val btbWriteWay = UInt(log2Up(BtbWays).W)
  val btbHitJal = Bool()
  val bimCtr = UInt(2.W)
  val histPtr = UInt(log2Up(ExtHistoryLength).W)
  val tageMeta = new TageMeta
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
  val rasToqAddr = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(PredictWidth).W)

  val debug_ubtb_cycle = if (BPUDebug) UInt(64.W) else UInt(0.W)
  val debug_btb_cycle  = if (BPUDebug) UInt(64.W) else UInt(0.W)
  val debug_tage_cycle = if (BPUDebug) UInt(64.W) else UInt(0.W)

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
  val flushPipe  = Bool()  // This inst will flush all the pipe when commit, like exception but can commit
  val isRVF = Bool()
  val imm = UInt(XLEN.W)
  val commitType = CommitType()
}

class CfCtrl extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
  val brTag = new BrqPtr
}

trait HasRoqIdx { this: HasXSParameter =>
  val roqIdx = UInt(RoqIdxWidth.W)

  def isAfter(thatIdx: UInt): Bool = {
    Mux(
      this.roqIdx.head(1) === thatIdx.head(1),
      this.roqIdx.tail(1) > thatIdx.tail(1),
      this.roqIdx.tail(1) < thatIdx.tail(1)
    )
  }

  def isAfter[ T<: HasRoqIdx ](that: T): Bool = {
    isAfter(that.roqIdx)
  }

  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && (redirect.bits.isException || redirect.bits.isFlushPipe || this.isAfter(redirect.bits.roqIdx)) // TODO: need check by JiaWei
  }
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp extends CfCtrl with HasRoqIdx {
  val psrc1, psrc2, psrc3, pdest, old_pdest = UInt(PhyRegIdxWidth.W)
  val src1State, src2State, src3State = SrcState()
  val lsroqIdx = UInt(LsroqIdxWidth.W)
  val diffTestDebugLrScValid = Bool()
}

class Redirect extends XSBundle with HasRoqIdx {
  val isException = Bool()
  val isMisPred = Bool()
  val isReplay = Bool()
  val isFlushPipe = Bool()
  val pc = UInt(VAddrBits.W)
  val target = UInt(VAddrBits.W)
  val brTag = new BrqPtr
}

class Dp1ToDp2IO extends XSBundle {
  val intDqToDp2 = Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
  val fpDqToDp2 = Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp))
  val lsDqToDp2 = Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp))
}

class ReplayPregReq extends XSBundle {
  // NOTE: set isInt and isFp both to 'false' when invalid
  val isInt = Bool()
  val isFp = Bool()
  val preg = UInt(PhyRegIdxWidth.W)
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
  val mcommit = Input(UInt(3.W))
}

class RoqCommit extends XSBundle {
  val uop = new MicroOp
  val isWalk = Bool()
}

class TlbFeedback extends XSBundle with HasRoqIdx{
  val hit = Bool()
}

class FrontendToBackendIO extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  // from backend
  val redirect = Flipped(ValidIO(new Redirect))
  val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
}

class TlbCsrBundle extends XSBundle {
  val satp = new Bundle {
    val mode = UInt(4.W) // TODO: may change number to parameter
    val asid = UInt(16.W)
    val ppn  = UInt(44.W) // just use PAddrBits - 3 - vpnnLen
  }
  val priv = new Bundle {
    val mxr = Bool()
    val sum = Bool()
    val imode = UInt(2.W)
    val dmode = UInt(2.W)
  }

  override def toPrintable: Printable = {
    p"Satp mode:0x${Hexadecimal(satp.mode)} asid:0x${Hexadecimal(satp.asid)} ppn:0x${Hexadecimal(satp.ppn)} " + 
    p"Priv mxr:${priv.mxr} sum:${priv.sum} imode:${priv.imode} dmode:${priv.dmode}"
  }
}

class SfenceBundle extends XSBundle {
  val valid = Bool()
  val bits = new Bundle {
    val rs1 = Bool()
    val rs2 = Bool()
    val addr = UInt(VAddrBits.W)
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}"
  }
}
