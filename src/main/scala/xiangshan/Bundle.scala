package xiangshan

import chisel3._
import chisel3.util._
import xiangshan.backend.SelImm
import xiangshan.backend.brq.BrqPtr
import xiangshan.backend.fu.fpu.Fflags
import xiangshan.backend.rename.FreeListPtr
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.decode.XDecode
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.HasBPUParameter
import xiangshan.frontend.HasTageParameter
import xiangshan.frontend.HasIFUConst
import xiangshan.frontend.GlobalHistory
import utils._
import scala.math.max

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val mask = UInt(PredictWidth.W)
  val pdmask = UInt(PredictWidth.W)
  // val pc = UInt(VAddrBits.W)
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val pnpc = Vec(PredictWidth, UInt(VAddrBits.W))
  val bpuMeta = Vec(PredictWidth, new BpuMeta)
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val predTaken = Bool()
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

class SCMeta(val useSC: Boolean) extends XSBundle with HasTageParameter {
  def maxVal = 8 * ((1 << TageCtrBits) - 1) + SCTableInfo.map{case (_,cb,_) => (1 << cb) - 1}.reduce(_+_)
  def minVal = -(8 * (1 << TageCtrBits) + SCTableInfo.map{case (_,cb,_) => 1 << cb}.reduce(_+_))
  def sumCtrBits = max(log2Ceil(-minVal), log2Ceil(maxVal+1)) + 1
  val tageTaken = if (useSC) Bool() else UInt(0.W)
  val scUsed    = if (useSC) Bool() else UInt(0.W)
  val scPred    = if (useSC) Bool() else UInt(0.W)
  // Suppose ctrbits of all tables are identical
  val ctrs      = if (useSC) Vec(SCNTables, SInt(SCCtrBits.W)) else Vec(SCNTables, SInt(0.W))
  val sumAbs    = if (useSC) UInt(sumCtrBits.W) else UInt(0.W)
}

class TageMeta extends XSBundle with HasTageParameter {
  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val taken = Bool()
  val scMeta = new SCMeta(EnableSC)
}

class BranchPrediction extends XSBundle with HasIFUConst {
  // val redirect = Bool()
  val takens = UInt(PredictWidth.W)
  // val jmpIdx = UInt(log2Up(PredictWidth).W)
  val brMask = UInt(PredictWidth.W)
  val jalMask = UInt(PredictWidth.W)
  val targets = Vec(PredictWidth, UInt(VAddrBits.W))

  // marks the last 2 bytes of this fetch packet
  // val endsAtTheEndOfFirstBank = Bool()
  // val endsAtTheEndOfLastBank = Bool()

  // half RVI could only start at the end of a bank
  val firstBankHasHalfRVI = Bool()
  val lastBankHasHalfRVI = Bool()

  // assumes that only one of the two conditions could be true
  def lastHalfRVIMask = Cat(lastBankHasHalfRVI.asUInt, 0.U((bankWidth-1).W), firstBankHasHalfRVI.asUInt, 0.U((bankWidth-1).W))

  def lastHalfRVIClearMask = ~lastHalfRVIMask
  // is taken from half RVI
  def lastHalfRVITaken = (takens(bankWidth-1) && firstBankHasHalfRVI) || (takens(PredictWidth-1) && lastBankHasHalfRVI)

  def lastHalfRVIIdx = Mux(firstBankHasHalfRVI, (bankWidth-1).U, (PredictWidth-1).U)
  // should not be used if not lastHalfRVITaken
  def lastHalfRVITarget = Mux(firstBankHasHalfRVI, targets(bankWidth-1), targets(PredictWidth-1))
  
  def realTakens  = takens  & lastHalfRVIClearMask
  def realBrMask  = brMask  & lastHalfRVIClearMask
  def realJalMask = jalMask & lastHalfRVIClearMask

  def brNotTakens = ~takens & realBrMask
  def sawNotTakenBr = VecInit((0 until PredictWidth).map(i =>
                       (if (i == 0) false.B else ParallelORR(brNotTakens(i-1,0)))))
  // def hasNotTakenBrs = (brNotTakens & LowerMaskFromLowest(realTakens)).orR
  def unmaskedJmpIdx = ParallelPriorityEncoder(takens)
  // if not taken before the half RVI inst
  def saveHalfRVI = (firstBankHasHalfRVI && !(ParallelORR(takens(bankWidth-2,0)))) ||
  (lastBankHasHalfRVI && !(ParallelORR(takens(PredictWidth-2,0))))
  // could get PredictWidth-1 when only the first bank is valid
  def jmpIdx = ParallelPriorityEncoder(realTakens)
  // only used when taken
  def target = ParallelPriorityMux(realTakens, targets)
  def taken = ParallelORR(realTakens)
  def takenOnBr = taken && ParallelPriorityMux(realTakens, realBrMask.asBools)
  def hasNotTakenBrs = Mux(taken, ParallelPriorityMux(realTakens, sawNotTakenBr), ParallelORR(brNotTakens))
}

class BpuMeta extends XSBundle with HasBPUParameter {
  val ubtbWriteWay = UInt(log2Up(UBtbWays).W)
  val ubtbHits = Bool()
  val btbWriteWay = UInt(log2Up(BtbWays).W)
  val btbHitJal = Bool()
  val bimCtr = UInt(2.W)
  val tageMeta = new TageMeta
  val rasSp = UInt(log2Up(RasSize).W)
  val rasTopCtr = UInt(8.W)
  val rasToqAddr = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(PredictWidth).W)
  val specCnt = UInt(10.W)
  // for global history
  val predTaken = Bool()
  val hist = new GlobalHistory
  val predHist = new GlobalHistory
  val sawNotTakenBranch = Bool()

  val debug_ubtb_cycle = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)
  val debug_btb_cycle  = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)
  val debug_tage_cycle = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)

  // def apply(histPtr: UInt, tageMeta: TageMeta, rasSp: UInt, rasTopCtr: UInt) = {
  //   this.histPtr := histPtr
  //   this.tageMeta := tageMeta
  //   this.rasSp := rasSp
  //   this.rasTopCtr := rasTopCtr
  //   this.asUInt
  // }
  def size = 0.U.asTypeOf(this).getWidth
  def fromUInt(x: UInt) = x.asTypeOf(this)
}

class Predecode extends XSBundle with HasIFUConst {
  val hasLastHalfRVI = Bool()
  val mask = UInt(PredictWidth.W)
  val lastHalf = UInt(nBanksInPacket.W)
  val pd = Vec(PredictWidth, (new PreDecodeInfo))
}

class CfiUpdateInfo extends XSBundle {
  // from backend
  val pc = UInt(VAddrBits.W)
  val pnpc = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(PredictWidth).W)
  // frontend -> backend -> frontend
  val pd = new PreDecodeInfo
  val bpuMeta = new BpuMeta

  // need pipeline update
  val target = UInt(VAddrBits.W)
  val brTarget = UInt(VAddrBits.W)
  val taken = Bool()
  val isMisPred = Bool()
  val brTag = new BrqPtr
  val isReplay = Bool()
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val exceptionVec = Vec(16, Bool())
  val intrVec = Vec(12, Bool())
  val brUpdate = new CfiUpdateInfo
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
  val noSpecExec = Bool()  // wait forward
  val blockBackward  = Bool()  // block backward
  val flushPipe  = Bool()  // This inst will flush all the pipe when commit, like exception but can commit
  val isRVF = Bool()
  val selImm = SelImm()
  val imm = UInt(XLEN.W)
  val commitType = CommitType()

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decodeDefault, table)
    val signals =
      Seq(src1Type, src2Type, src3Type, fuType, fuOpType, rfWen, fpWen, 
          isXSTrap, noSpecExec, blockBackward, flushPipe, isRVF, selImm)
    signals zip decoder map { case(s, d) => s := d }
    commitType := DontCare
    this
  }
}

class CfCtrl extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
  val brTag = new BrqPtr
}

class LSIdx extends XSBundle {
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp extends CfCtrl {
  val psrc1, psrc2, psrc3, pdest, old_pdest = UInt(PhyRegIdxWidth.W)
  val src1State, src2State, src3State = SrcState()
  val roqIdx = new RoqPtr
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val diffTestDebugLrScValid = Bool()
}

class Redirect extends XSBundle {
  val roqIdx = new RoqPtr
  val level = RedirectLevel()
  val interrupt = Bool()
  val pc = UInt(VAddrBits.W)
  val target = UInt(VAddrBits.W)
  val brTag = new BrqPtr

  def isUnconditional() = RedirectLevel.isUnconditional(level)
  def flushItself() = RedirectLevel.flushItself(level)
  def isException() = RedirectLevel.isException(level)
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
  val src1, src2, src3 = UInt((XLEN+1).W)
}

class ExuOutput extends XSBundle {
  val uop = new MicroOp
  val data = UInt((XLEN+1).W)
  val fflags  = new Fflags
  val redirectValid = Bool()
  val redirect = new Redirect
  val brUpdate = new CfiUpdateInfo
  val debug = new DebugBundle
}

class ExternalInterruptIO extends XSBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
}

class CSRSpecialIO extends XSBundle {
  val exception = Flipped(ValidIO(new MicroOp))
  val isInterrupt = Input(Bool())
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val trapTarget = Output(UInt(VAddrBits.W))
  val externalInterrupt = new ExternalInterruptIO
  val interrupt = Output(Bool())
}

class RoqCommitInfo extends XSBundle {
  val ldest = UInt(5.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val commitType = CommitType()
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr

  // these should be optimized for synthesis verilog
  val pc = UInt(VAddrBits.W)
}

class RoqCommitIO extends XSBundle {
  val isWalk = Output(Bool())
  val valid = Vec(CommitWidth, Output(Bool()))
  val info = Vec(CommitWidth, Output(new RoqCommitInfo))

  def hasWalkInstr = isWalk && valid.asUInt.orR
  def hasCommitInstr = !isWalk && valid.asUInt.orR
}

class TlbFeedback extends XSBundle {
  val roqIdx = new RoqPtr
  val hit = Bool()
}

class FrontendToBackendIO extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  // from backend
  val redirect = Flipped(ValidIO(UInt(VAddrBits.W)))
  // val cfiUpdateInfo = Flipped(ValidIO(new CfiUpdateInfo))
  val cfiUpdateInfo = Flipped(ValidIO(new CfiUpdateInfo))
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
