/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan

import chisel3._
import chisel3.util._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.decode.{ImmUnion, XDecode}
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.frontend.PreDecodeInfoForDebug
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.HasBPUParameter
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.HasTageParameter
import xiangshan.frontend.HasSCParameter
import xiangshan.frontend.HasIFUConst
import xiangshan.frontend.GlobalHistory
import xiangshan.frontend.RASEntry
import xiangshan.frontend.BPUCtrl
import utils._

import scala.math.max
import Chisel.experimental.chiselName
import chipsalliance.rocketchip.config.Parameters
import xiangshan.backend.ftq.FtqPtr

// Fetch FetchWidth x 32-bit insts from Icache
class FetchPacket(implicit p: Parameters) extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val mask = UInt(PredictWidth.W)
  val pdmask = UInt(PredictWidth.W)
  // val pc = UInt(VAddrBits.W)
  val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  val foldpc = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val pred_taken = UInt(PredictWidth.W)
  val ftqPtr = new FtqPtr
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

object RSFeedbackType {
  val tlbMiss = 0.U(2.W)
  val mshrFull = 1.U(2.W)
  val dataInvalid = 2.U(2.W)

  def apply() = UInt(2.W)
}

class SCMeta(val useSC: Boolean)(implicit p: Parameters) extends XSBundle with HasSCParameter {
  val tageTaken = if (useSC) Bool() else UInt(0.W)
  val scUsed = if (useSC) Bool() else UInt(0.W)
  val scPred = if (useSC) Bool() else UInt(0.W)
  // Suppose ctrbits of all tables are identical
  val ctrs = if (useSC) Vec(SCNTables, SInt(SCCtrBits.W)) else Vec(SCNTables, SInt(0.W))
}

class TageMeta(implicit p: Parameters) extends XSBundle with HasTageParameter {
  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val taken = Bool()
  val scMeta = new SCMeta(EnableSC)
}

@chiselName
class BranchPrediction(implicit p: Parameters) extends XSBundle with HasIFUConst {
  // val redirect = Bool()
  val takens = UInt(PredictWidth.W)
  // val jmpIdx = UInt(log2Up(PredictWidth).W)
  val brMask = UInt(PredictWidth.W)
  val jalMask = UInt(PredictWidth.W)
  val targets = Vec(PredictWidth, UInt(VAddrBits.W))

  // half RVI could only start at the end of a packet
  val hasHalfRVI = Bool()

  def brNotTakens = (~takens & brMask)

  def sawNotTakenBr = VecInit((0 until PredictWidth).map(i =>
    (if (i == 0) false.B else ParallelORR(brNotTakens(i - 1, 0)))))

  // if not taken before the half RVI inst
  def saveHalfRVI = hasHalfRVI && !(ParallelORR(takens(PredictWidth - 2, 0)))

  // could get PredictWidth-1 when only the first bank is valid
  def jmpIdx = ParallelPriorityEncoder(takens)

  // only used when taken
  def target = {
    val generator = new PriorityMuxGenerator[UInt]
    generator.register(takens.asBools, targets, List.fill(PredictWidth)(None))
    generator()
  }

  def taken = ParallelORR(takens)

  def takenOnBr = taken && ParallelPriorityMux(takens, brMask.asBools)

  def hasNotTakenBrs = Mux(taken, ParallelPriorityMux(takens, sawNotTakenBr), ParallelORR(brNotTakens))
}

class PredictorAnswer(implicit p: Parameters) extends XSBundle {
  val hit    = if (!env.FPGAPlatform) Bool() else UInt(0.W)
  val taken  = if (!env.FPGAPlatform) Bool() else UInt(0.W)
  val target = if (!env.FPGAPlatform) UInt(VAddrBits.W) else UInt(0.W)
}

class BpuMeta(implicit p: Parameters) extends XSBundle with HasBPUParameter {
  val btbWriteWay = UInt(log2Up(BtbWays).W)
  val btbHit = Bool()
  val bimCtr = UInt(2.W)
  val tageMeta = new TageMeta
  // for global history

  val debug_ubtb_cycle = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)
  val debug_btb_cycle = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)
  val debug_tage_cycle = if (EnableBPUTimeRecord) UInt(64.W) else UInt(0.W)

  val predictor = if (BPUDebug) UInt(log2Up(4).W) else UInt(0.W) // Mark which component this prediction comes from {ubtb, btb, tage, loopPredictor}

  val ubtbHit = if (BPUDebug) UInt(1.W) else UInt(0.W)

  val ubtbAns = new PredictorAnswer
  val btbAns = new PredictorAnswer
  val tageAns = new PredictorAnswer
  val rasAns = new PredictorAnswer
  val loopAns = new PredictorAnswer

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

class Predecode(implicit p: Parameters) extends XSBundle with HasIFUConst {
  val hasLastHalfRVI = Bool()
  val mask = UInt(PredictWidth.W)
  val lastHalf = Bool()
  val pd = Vec(PredictWidth, (new PreDecodeInfo))
}

class CfiUpdateInfo(implicit p: Parameters) extends XSBundle with HasBPUParameter {
  // from backend
  val pc = UInt(VAddrBits.W)
  // frontend -> backend -> frontend
  val pd = new PreDecodeInfo
  val rasSp = UInt(log2Up(RasSize).W)
  val rasEntry = new RASEntry
  val hist = new GlobalHistory
  val predHist = new GlobalHistory
  val specCnt = Vec(PredictWidth, UInt(10.W))
  // need pipeline update
  val sawNotTakenBranch = Bool()
  val predTaken = Bool()
  val target = UInt(VAddrBits.W)
  val taken = Bool()
  val isMisPred = Bool()
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow(implicit p: Parameters) extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val exceptionVec = ExceptionVec()
  val intrVec = Vec(12, Bool())
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val crossPageIPFFix = Bool()
  val storeSetHit = Bool() // inst has been allocated an store set
  val loadWaitBit = Bool() // load inst should not be executed until all former store addr calcuated
  val ssid = UInt(SSIDWidth.W)
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
}

class FtqEntry(implicit p: Parameters) extends XSBundle {
  // fetch pc, pc of each inst could be generated by concatenation
  val ftqPC = UInt(VAddrBits.W)
  val lastPacketPC = ValidUndirectioned(UInt(VAddrBits.W))
  // prediction metas
  val hist = new GlobalHistory
  val predHist = new GlobalHistory
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry()
  val specCnt = Vec(PredictWidth, UInt(10.W))
  val metas = Vec(PredictWidth, new BpuMeta)

  val cfiIsCall, cfiIsRet, cfiIsJalr, cfiIsRVC = Bool()
  val rvc_mask = Vec(PredictWidth, Bool())
  val br_mask = Vec(PredictWidth, Bool())
  val cfiIndex = ValidUndirectioned(UInt(log2Up(PredictWidth).W))
  val valids = Vec(PredictWidth, Bool())

  // backend update
  val mispred = Vec(PredictWidth, Bool())
  val target = UInt(VAddrBits.W)

  // For perf counters
  val pd = Vec(PredictWidth, new PreDecodeInfoForDebug(!env.FPGAPlatform))

  def takens = VecInit((0 until PredictWidth).map(i => cfiIndex.valid && cfiIndex.bits === i.U))
  def hasLastPrev = lastPacketPC.valid

  override def toPrintable: Printable = {
    p"ftqPC: ${Hexadecimal(ftqPC)} lastPacketPC: ${Hexadecimal(lastPacketPC.bits)} hasLastPrev:$hasLastPrev " +
      p"rasSp:$rasSp specCnt:$specCnt brmask:${Binary(Cat(br_mask))} rvcmask:${Binary(Cat(rvc_mask))} " +
      p"valids:${Binary(valids.asUInt())} cfi valid: ${cfiIndex.valid} " +
      p"cfi index: ${cfiIndex.bits} isCall:$cfiIsCall isRet:$cfiIsRet isJalr:$cfiIsJalr, isRvc:$cfiIsRVC " +
      p"mispred:${Binary(Cat(mispred))} target:${Hexadecimal(target)}\n"
  }

}


class FPUCtrlSignals(implicit p: Parameters) extends XSBundle {
  val isAddSub = Bool() // swap23
  val typeTagIn = UInt(2.W)
  val typeTagOut = UInt(2.W)
  val fromInt = Bool()
  val wflags = Bool()
  val fpWen = Bool()
  val fmaCmd = UInt(2.W)
  val div = Bool()
  val sqrt = Bool()
  val fcvt = Bool()
  val typ = UInt(2.W)
  val fmt = UInt(2.W)
  val ren3 = Bool() //TODO: remove SrcType.fp
  val rm = UInt(3.W)
}

// Decode DecodeWidth insts at Decode Stage
class CtrlSignals(implicit p: Parameters) extends XSBundle {
  val srcType = Vec(3, SrcType())
  val lsrc = Vec(3, UInt(5.W))
  val ldest = UInt(5.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val isXSTrap = Bool()
  val noSpecExec = Bool() // wait forward
  val blockBackward = Bool() // block backward
  val flushPipe = Bool() // This inst will flush all the pipe when commit, like exception but can commit
  val isRVF = Bool()
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val commitType = CommitType()
  val fpu = new FPUCtrlSignals
  val isMove = Bool()
  val singleStep = Bool()

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decodeDefault, table)
    val signals =
      Seq(srcType(0), srcType(1), srcType(2), fuType, fuOpType, rfWen, fpWen,
        isXSTrap, noSpecExec, blockBackward, flushPipe, isRVF, selImm)
    signals zip decoder map { case (s, d) => s := d }
    commitType := DontCare
    this
  }
}

class CfCtrl(implicit p: Parameters) extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
}

class PerfDebugInfo(implicit p: Parameters) extends XSBundle {
  val eliminatedMove = Bool()
  // val fetchTime = UInt(64.W)
  val renameTime = UInt(64.W)
  val dispatchTime = UInt(64.W)
  val issueTime = UInt(64.W)
  val writebackTime = UInt(64.W)
  // val commitTime = UInt(64.W)
}

// Separate LSQ
class LSIdx(implicit p: Parameters) extends XSBundle {
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp(implicit p: Parameters) extends CfCtrl {
  val srcState = Vec(3, SrcState())
  val psrc = Vec(3, UInt(PhyRegIdxWidth.W))
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
  val roqIdx = new RoqPtr
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val diffTestDebugLrScValid = Bool()
  val eliminatedMove = Bool()
  val debugInfo = new PerfDebugInfo
  def needRfRPort(index: Int, rfType: Int, ignoreState: Boolean = true) : Bool = {
    (index, rfType) match {
      case (0, 0) => ctrl.srcType(0) === SrcType.reg && ctrl.lsrc(0) =/= 0.U && (srcState(0) === SrcState.rdy || ignoreState.B)
      case (1, 0) => ctrl.srcType(1) === SrcType.reg && ctrl.lsrc(1) =/= 0.U && (srcState(1) === SrcState.rdy || ignoreState.B)
      case (0, 1) => ctrl.srcType(0) === SrcType.fp && (srcState(0) === SrcState.rdy || ignoreState.B)
      case (1, 1) => ctrl.srcType(1) === SrcType.fp && (srcState(1) === SrcState.rdy || ignoreState.B)
      case (2, 1) => ctrl.srcType(2) === SrcType.fp && (srcState(2) === SrcState.rdy || ignoreState.B)
      case _ => false.B
    }
  }
  def srcIsReady: Vec[Bool] = {
    VecInit(ctrl.srcType.zip(srcState).map{ case (t, s) => SrcType.isPcImm(t) || s === SrcState.rdy })
  }
  def doWriteIntRf: Bool = ctrl.rfWen && ctrl.ldest =/= 0.U
  def doWriteFpRf: Bool = ctrl.fpWen
}

class MicroOpRbExt(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val flag = UInt(1.W)
}

class Redirect(implicit p: Parameters) extends XSBundle {
  val roqIdx = new RoqPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfiUpdate = new CfiUpdateInfo

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset = UInt(log2Up(PredictWidth).W)

  // def isUnconditional() = RedirectLevel.isUnconditional(level)
  def flushItself() = RedirectLevel.flushItself(level)
  // def isException() = RedirectLevel.isException(level)
}

class Dp1ToDp2IO(implicit p: Parameters) extends XSBundle {
  val intDqToDp2 = Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
  val fpDqToDp2 = Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp))
  val lsDqToDp2 = Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp))
}

class ReplayPregReq(implicit p: Parameters) extends XSBundle {
  // NOTE: set isInt and isFp both to 'false' when invalid
  val isInt = Bool()
  val isFp = Bool()
  val preg = UInt(PhyRegIdxWidth.W)
}

class DebugBundle(implicit p: Parameters) extends XSBundle {
  val isMMIO = Bool()
  val isPerfCnt = Bool()
  val paddr = UInt(PAddrBits.W)
}

class ExuInput(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val src = Vec(3, UInt((XLEN + 1).W))
}

class ExuOutput(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val data = UInt((XLEN + 1).W)
  val fflags = UInt(5.W)
  val redirectValid = Bool()
  val redirect = new Redirect
  val debug = new DebugBundle
}

class ExternalInterruptIO(implicit p: Parameters) extends XSBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
  val debug = Input(Bool())
}

class CSRSpecialIO(implicit p: Parameters) extends XSBundle {
  val exception = Flipped(ValidIO(new MicroOp))
  val isInterrupt = Input(Bool())
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val trapTarget = Output(UInt(VAddrBits.W))
  val externalInterrupt = new ExternalInterruptIO
  val interrupt = Output(Bool())
}

class ExceptionInfo(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val isInterrupt = Bool()
}

class RoqCommitInfo(implicit p: Parameters) extends XSBundle {
  val ldest = UInt(5.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val wflags = Bool()
  val commitType = CommitType()
  val eliminatedMove = Bool()
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)

  // these should be optimized for synthesis verilog
  val pc = UInt(VAddrBits.W)
}

class RoqCommitIO(implicit p: Parameters) extends XSBundle {
  val isWalk = Output(Bool())
  val valid = Vec(CommitWidth, Output(Bool()))
  val info = Vec(CommitWidth, Output(new RoqCommitInfo))

  def hasWalkInstr = isWalk && valid.asUInt.orR

  def hasCommitInstr = !isWalk && valid.asUInt.orR
}

class RSFeedback(implicit p: Parameters) extends XSBundle {
  val rsIdx = UInt(log2Up(IssQueSize).W)
  val hit = Bool()
  val flushState = Bool()
  val sourceType = RSFeedbackType()
}

class FrontendToBackendIO(implicit p: Parameters) extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val fetchInfo = DecoupledIO(new FtqEntry)
  // from backend
  val redirect_cfiUpdate = Flipped(ValidIO(new Redirect))
  val commit_cfiUpdate = Flipped(ValidIO(new FtqEntry))
  val ftqEnqPtr = Input(new FtqPtr)
  val ftqLeftOne = Input(Bool())
}

class TlbCsrBundle(implicit p: Parameters) extends XSBundle {
  val satp = new Bundle {
    val mode = UInt(4.W) // TODO: may change number to parameter
    val asid = UInt(16.W)
    val ppn = UInt(44.W) // just use PAddrBits - 3 - vpnnLen
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

class SfenceBundle(implicit p: Parameters) extends XSBundle {
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

// Bundle for load violation predictor updating
class MemPredUpdateReq(implicit p: Parameters) extends XSBundle  {
  val valid = Bool()

  // wait table update
  val waddr = UInt(MemPredPCWidth.W)
  val wdata = Bool() // true.B by default

  // store set update
  // by default, ldpc/stpc should be xor folded
  val ldpc = UInt(MemPredPCWidth.W)
  val stpc = UInt(MemPredPCWidth.W)
}

class CustomCSRCtrlIO(implicit p: Parameters) extends XSBundle {
  // Prefetcher
  val l1plus_pf_enable = Output(Bool())
  val l2_pf_enable = Output(Bool())
  // Labeled XiangShan
  val dsid = Output(UInt(8.W)) // TODO: DsidWidth as parameter
  // Load violation predictor
  val lvpred_disable = Output(Bool())
  val no_spec_load = Output(Bool())
  val waittable_timeout = Output(UInt(5.W))
  // Branch predictor
  val bp_ctrl = Output(new BPUCtrl)
  // Memory Block
  val sbuffer_threshold = Output(UInt(4.W))
  // Rename
  val move_elim_enable = Output(Bool())
}
