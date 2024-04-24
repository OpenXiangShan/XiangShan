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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import utility._
import utils._
import xiangshan.backend.decode.{ImmUnion, XDecode}
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend._
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.backend.Bundles.{DynInst, UopIdx}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.frontend.{AllAheadFoldedHistoryOldestBits, AllFoldedHistories, BPUCtrl, CGHPtr, FtqPtr, FtqToCtrlIO}
import xiangshan.frontend.{Ftq_Redirect_SRAMEntry, HasBPUParameter, IfuToBackendIO, PreDecodeInfo, RASPtr}
import xiangshan.cache.HasDCacheParameters
import utility._

import org.chipsalliance.cde.config.Parameters
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util.experimental.decode.EspressoMinimizer
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.fu.PMPEntry
import xiangshan.frontend.Ftq_Redirect_SRAMEntry
import xiangshan.frontend.AllFoldedHistories
import xiangshan.frontend.AllAheadFoldedHistoryOldestBits
import xiangshan.frontend.RASPtr
import xiangshan.backend.rob.RobBundles.RobCommitEntryBundle

class ValidUndirectioned[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen.cloneType.asInstanceOf[T]

}

object ValidUndirectioned {
  def apply[T <: Data](gen: T) = {
    new ValidUndirectioned[T](gen)
  }
}

object RSFeedbackType {
  val lrqFull         = 0.U(4.W)
  val tlbMiss         = 1.U(4.W)
  val mshrFull        = 2.U(4.W)
  val dataInvalid     = 3.U(4.W)
  val bankConflict    = 4.U(4.W)
  val ldVioCheckRedo  = 5.U(4.W)
  val feedbackInvalid = 7.U(4.W)
  val issueSuccess    = 8.U(4.W)
  val rfArbitFail     = 9.U(4.W)
  val fuIdle          = 10.U(4.W)
  val fuBusy          = 11.U(4.W)
  val fuUncertain     = 12.U(4.W)

  val allTypes = 16
  def apply() = UInt(4.W)

  def isStageSuccess(feedbackType: UInt) = {
    feedbackType === issueSuccess
  }

  def isBlocked(feedbackType: UInt) = {
    feedbackType === rfArbitFail || feedbackType === fuBusy || feedbackType >= lrqFull && feedbackType <= feedbackInvalid
  }
}

class PredictorAnswer(implicit p: Parameters) extends XSBundle {
  val hit    = if (!env.FPGAPlatform) Bool() else UInt(0.W)
  val taken  = if (!env.FPGAPlatform) Bool() else UInt(0.W)
  val target = if (!env.FPGAPlatform) UInt(VAddrBits.W) else UInt(0.W)
}

class CfiUpdateInfo(implicit p: Parameters) extends XSBundle with HasBPUParameter {
  // from backend
  val pc = UInt(VAddrBits.W)
  // frontend -> backend -> frontend
  val pd = new PreDecodeInfo
  val ssp = UInt(log2Up(RasSize).W)
  val sctr = UInt(log2Up(RasCtrSize).W)
  val TOSW = new RASPtr
  val TOSR = new RASPtr
  val NOS = new RASPtr
  val topAddr = UInt(VAddrBits.W)
  // val hist = new ShiftingGlobalHistory
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val ghr = UInt(UbtbGHRLength.W)
  val histPtr = new CGHPtr
  val specCnt = Vec(numBr, UInt(10.W))
  // need pipeline update
  val br_hit = Bool() // if in ftb entry
  val jr_hit = Bool() // if in ftb entry
  val sc_hit = Bool() // if used in ftb entry, invalid if !br_hit
  val predTaken = Bool()
  val target = UInt(VAddrBits.W)
  val taken = Bool()
  val isMisPred = Bool()
  val shift = UInt((log2Ceil(numBr)+1).W)
  val addIntoHist = Bool()

  def fromFtqRedirectSram(entry: Ftq_Redirect_SRAMEntry) = {
    // this.hist := entry.ghist
    this.folded_hist := entry.folded_hist
    this.lastBrNumOH := entry.lastBrNumOH
    this.afhob := entry.afhob
    this.histPtr := entry.histPtr
    this.ssp := entry.ssp
    this.sctr := entry.sctr
    this.TOSW := entry.TOSW
    this.TOSR := entry.TOSR
    this.NOS := entry.NOS
    this.topAddr := entry.topAddr
    this
  }
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow(implicit p: Parameters) extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  // Todo: remove this
  val gpaddr = UInt(GPAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val exceptionVec = ExceptionVec()
  val trigger = new TriggerCf
  val pd = new PreDecodeInfo
  val pred_taken = Bool()
  val crossPageIPFFix = Bool()
  val storeSetHit = Bool() // inst has been allocated an store set
  val waitForRobIdx = new RobPtr // store set predicted previous store robIdx
  // Load wait is needed
  // load inst will not be executed until former store (predicted by mdp) addr calcuated
  val loadWaitBit = Bool()
  // If (loadWaitBit && loadWaitStrict), strict load wait is needed
  // load inst will not be executed until ALL former store addr calcuated
  val loadWaitStrict = Bool()
  val ssid = UInt(SSIDWidth.W)
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
}


class FPUCtrlSignals(implicit p: Parameters) extends XSBundle {
  val isAddSub = Bool() // swap23
  val typeTagIn = UInt(1.W)
  val typeTagOut = UInt(1.W)
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
  val debug_globalID = UInt(XLEN.W)
  val srcType = Vec(4, SrcType())
  val lsrc = Vec(4, UInt(6.W))
  val ldest = UInt(6.W)
  val fuType = FuType()
  val fuOpType = FuOpType()
  val rfWen = Bool()
  val fpWen = Bool()
  val vecWen = Bool()
  val isXSTrap = Bool()
  val noSpecExec = Bool() // wait forward
  val blockBackward = Bool() // block backward
  val flushPipe = Bool() // This inst will flush all the pipe when commit, like exception but can commit
  val uopSplitType = UopSplitType()
  val selImm = SelImm()
  val imm = UInt(32.W)
  val commitType = CommitType()
  val fpu = new FPUCtrlSignals
  val uopIdx = UopIdx()
  val isMove = Bool()
  val vm = Bool()
  val singleStep = Bool()
  // This inst will flush all the pipe when it is the oldest inst in ROB,
  // then replay from this inst itself
  val replayInst = Bool()
  val canRobCompress = Bool()

  private def allSignals = srcType.take(3) ++ Seq(fuType, fuOpType, rfWen, fpWen, vecWen,
    isXSTrap, noSpecExec, blockBackward, flushPipe, canRobCompress, uopSplitType, selImm)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): CtrlSignals = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decodeDefault, table, EspressoMinimizer)
    allSignals zip decoder foreach { case (s, d) => s := d }
    commitType := DontCare
    this
  }

  def decode(bit: List[BitPat]): CtrlSignals = {
    allSignals.zip(bit.map(bitPatToUInt(_))).foreach{ case (s, d) => s := d }
    this
  }

  def isWFI: Bool = fuType === FuType.csr.U && fuOpType === CSROpType.wfi
  def isSoftPrefetch: Bool = {
    fuType === FuType.alu.U && fuOpType === ALUOpType.or && selImm === SelImm.IMM_I && ldest === 0.U
  }
  def needWriteRf: Bool = (rfWen && ldest =/= 0.U) || fpWen || vecWen
  def isHyperInst: Bool = {
    fuType === FuType.ldu.U && LSUOpType.isHlv(fuOpType) || fuType === FuType.stu.U && LSUOpType.isHsv(fuOpType)
  }
}

class CfCtrl(implicit p: Parameters) extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
}

class PerfDebugInfo(implicit p: Parameters) extends XSBundle {
  val eliminatedMove = Bool()
  // val fetchTime = UInt(XLEN.W)
  val renameTime = UInt(XLEN.W)
  val dispatchTime = UInt(XLEN.W)
  val enqRsTime = UInt(XLEN.W)
  val selectTime = UInt(XLEN.W)
  val issueTime = UInt(XLEN.W)
  val writebackTime = UInt(XLEN.W)
  // val commitTime = UInt(XLEN.W)
  val runahead_checkpoint_id = UInt(XLEN.W)
  val tlbFirstReqTime = UInt(XLEN.W)
  val tlbRespTime = UInt(XLEN.W) // when getting hit result (including delay in L2TLB hit)
}

// Separate LSQ
class LSIdx(implicit p: Parameters) extends XSBundle {
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
}

// CfCtrl -> MicroOp at Rename Stage
class MicroOp(implicit p: Parameters) extends CfCtrl {
  val srcState = Vec(4, SrcState())
  val psrc = Vec(4, UInt(PhyRegIdxWidth.W))
  val pdest = UInt(PhyRegIdxWidth.W)
  val robIdx = new RobPtr
  val instrSize = UInt(log2Ceil(RenameWidth + 1).W)
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val eliminatedMove = Bool()
  val snapshot = Bool()
  val debugInfo = new PerfDebugInfo
  def needRfRPort(index: Int, isFp: Boolean, ignoreState: Boolean = true) : Bool = {
    val stateReady = srcState(index) === SrcState.rdy || ignoreState.B
    val readReg = if (isFp) {
      ctrl.srcType(index) === SrcType.fp
    } else {
      ctrl.srcType(index) === SrcType.reg && ctrl.lsrc(index) =/= 0.U
    }
    readReg && stateReady
  }
  def srcIsReady: Vec[Bool] = {
    VecInit(ctrl.srcType.zip(srcState).map{ case (t, s) => SrcType.isPcOrImm(t) || s === SrcState.rdy })
  }
  def clearExceptions(
    exceptionBits: Seq[Int] = Seq(),
    flushPipe: Boolean = false,
    replayInst: Boolean = false
  ): MicroOp = {
    cf.exceptionVec.zipWithIndex.filterNot(x => exceptionBits.contains(x._2)).foreach(_._1 := false.B)
    if (!flushPipe) { ctrl.flushPipe := false.B }
    if (!replayInst) { ctrl.replayInst := false.B }
    this
  }
}

class XSBundleWithMicroOp(implicit p: Parameters) extends XSBundle {
  val uop = new DynInst
}

class MicroOpRbExt(implicit p: Parameters) extends XSBundleWithMicroOp {
  val flag = UInt(1.W)
}

class Redirect(implicit p: Parameters) extends XSBundle {
  val isRVC = Bool()
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfiUpdate = new CfiUpdateInfo

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset = UInt(log2Up(PredictWidth).W)

  val debug_runahead_checkpoint_id = UInt(64.W)
  val debugIsCtrl = Bool()
  val debugIsMemVio = Bool()

  def flushItself() = RedirectLevel.flushItself(level)
}

class ResetPregStateReq(implicit p: Parameters) extends XSBundle {
  // NOTE: set isInt and isFp both to 'false' when invalid
  val isInt = Bool()
  val isFp = Bool()
  val preg = UInt(PhyRegIdxWidth.W)
}

class DebugBundle(implicit p: Parameters) extends XSBundle {
  val isMMIO = Bool()
  val isPerfCnt = Bool()
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  /* add L/S inst info in EXU */
  // val L1toL2TlbLatency = UInt(XLEN.W)
  // val levelTlbHit = UInt(2.W)
}

class ExternalInterruptIO(implicit p: Parameters) extends XSBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
  val seip = Input(Bool())
  val debug = Input(Bool())
}

class CSRSpecialIO(implicit p: Parameters) extends XSBundle {
  val exception = Flipped(ValidIO(new DynInst))
  val isInterrupt = Input(Bool())
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val trapTarget = Output(UInt(VAddrBits.W))
  val externalInterrupt = new ExternalInterruptIO
  val interrupt = Output(Bool())
}

class DiffCommitIO(implicit p: Parameters) extends XSBundle {
  val isCommit = Bool()
  val commitValid = Vec(CommitWidth * MaxUopSize, Bool())

  val info = Vec(CommitWidth * MaxUopSize, new RabCommitInfo)
}

class RobCommitInfo(implicit p: Parameters) extends RobCommitEntryBundle

class RobCommitIO(implicit p: Parameters) extends XSBundle {
  val isCommit = Bool()
  val commitValid = Vec(CommitWidth, Bool())

  val isWalk = Bool()
  // valid bits optimized for walk
  val walkValid = Vec(CommitWidth, Bool())

  val info = Vec(CommitWidth, new RobCommitInfo)
  val robIdx = Vec(CommitWidth, new RobPtr)

  def hasWalkInstr: Bool = isWalk && walkValid.asUInt.orR
  def hasCommitInstr: Bool = isCommit && commitValid.asUInt.orR
}

class RabCommitInfo(implicit p: Parameters) extends XSBundle {
  val ldest = UInt(6.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val vecWen = Bool()
  val isMove = Bool()
}

class RabCommitIO(implicit p: Parameters) extends XSBundle {
  val isCommit = Bool()
  val commitValid = Vec(RabCommitWidth, Bool())

  val isWalk = Bool()
  // valid bits optimized for walk
  val walkValid = Vec(RabCommitWidth, Bool())

  val info = Vec(RabCommitWidth, new RabCommitInfo)
  val robIdx = OptionWrapper(!env.FPGAPlatform, Vec(RabCommitWidth, new RobPtr))

  def hasWalkInstr: Bool = isWalk && walkValid.asUInt.orR
  def hasCommitInstr: Bool = isCommit && commitValid.asUInt.orR
}

class SnapshotPort(implicit p: Parameters) extends XSBundle {
  val snptEnq = Bool()
  val snptDeq = Bool()
  val useSnpt = Bool()
  val snptSelect = UInt(log2Ceil(RenameSnapshotNum).W)
  val flushVec = Vec(RenameSnapshotNum, Bool())
}

class RSFeedback(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val hit = Bool()
  val flushState = Bool()
  val sourceType = RSFeedbackType()
  val dataInvalidSqIdx = new SqPtr
}

class MemRSFeedbackIO(implicit p: Parameters) extends XSBundle {
  // Note: you need to update in implicit Parameters p before imp MemRSFeedbackIO
  // for instance: MemRSFeedbackIO()(updateP)
  val feedbackSlow = ValidIO(new RSFeedback()) // dcache miss queue full, dtlb miss
  val feedbackFast = ValidIO(new RSFeedback()) // bank conflict
}

class LoadCancelIO(implicit p: Parameters) extends XSBundle {
  val ld1Cancel = Bool()
  val ld2Cancel = Bool()
}

class FrontendToCtrlIO(implicit p: Parameters) extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val stallReason = new StallReasonIO(DecodeWidth)
  val fromFtq = new FtqToCtrlIO
  val fromIfu = new IfuToBackendIO
  // from backend
  val toFtq = Flipped(new CtrlToFtqIO)
  val canAccept = Input(Bool())
}

class SatpStruct(implicit p: Parameters) extends XSBundle {
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbSatpBundle(implicit p: Parameters) extends SatpStruct {
  val changed = Bool()

  def apply(satp_value: UInt): Unit = {
    require(satp_value.getWidth == XLEN)
    val sa = satp_value.asTypeOf(new SatpStruct)
    mode := sa.mode
    asid := sa.asid
    ppn := Cat(0.U((44-PAddrBits).W), sa.ppn(PAddrBits-1, 0)).asUInt
    changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
  }
}

class TlbCsrBundle(implicit p: Parameters) extends XSBundle {
  val satp = new TlbSatpBundle()
  val vsatp = new TlbSatpBundle()
  val hgatp = new TlbSatpBundle()
  val priv = new Bundle {
    val mxr = Bool()
    val sum = Bool()
    val vmxr = Bool()
    val vsum = Bool()
    val virt = Bool()
    val spvp = UInt(1.W)
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
    val id = UInt((AsidLength).W) // asid or vmid
    val flushPipe = Bool()
    val hv = Bool()
    val hg = Bool()
  }

  override def toPrintable: Printable = {
    p"valid:0x${Hexadecimal(valid)} rs1:${bits.rs1} rs2:${bits.rs2} addr:${Hexadecimal(bits.addr)}, flushPipe:${bits.flushPipe}"
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
  val l1I_pf_enable = Output(Bool())
  val l2_pf_enable = Output(Bool())
  val l1D_pf_enable = Output(Bool())
  val l1D_pf_train_on_hit = Output(Bool())
  val l1D_pf_enable_agt = Output(Bool())
  val l1D_pf_enable_pht = Output(Bool())
  val l1D_pf_active_threshold = Output(UInt(4.W))
  val l1D_pf_active_stride = Output(UInt(6.W))
  val l1D_pf_enable_stride = Output(Bool())
  val l2_pf_store_only = Output(Bool())
  // ICache
  val icache_parity_enable = Output(Bool())
  // Labeled XiangShan
  val dsid = Output(UInt(8.W)) // TODO: DsidWidth as parameter
  // Load violation predictor
  val lvpred_disable = Output(Bool())
  val no_spec_load = Output(Bool())
  val storeset_wait_store = Output(Bool())
  val storeset_no_fast_wakeup = Output(Bool())
  val lvpred_timeout = Output(UInt(5.W))
  // Branch predictor
  val bp_ctrl = Output(new BPUCtrl)
  // Memory Block
  val sbuffer_threshold = Output(UInt(4.W))
  val ldld_vio_check_enable = Output(Bool())
  val soft_prefetch_enable = Output(Bool())
  val cache_error_enable = Output(Bool())
  val uncache_write_outstanding_enable = Output(Bool())
  // Rename
  val fusion_enable = Output(Bool())
  val wfi_enable = Output(Bool())
  // Decode
  val svinval_enable = Output(Bool())

  // distribute csr write signal
  val distribute_csr = new DistributedCSRIO()
  // TODO: move it to a new bundle, since single step is not a custom control signal
  val singlestep = Output(Bool())
  val frontend_trigger = new FrontendTdataDistributeIO()
  val mem_trigger = new MemTdataDistributeIO()
  // Virtualization Mode
  val virtMode = Output(Bool())
}

class DistributedCSRIO(implicit p: Parameters) extends XSBundle {
  // CSR has been written by csr inst, copies of csr should be updated
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
}

class DistributedCSRUpdateReq(implicit p: Parameters) extends XSBundle {
  // Request csr to be updated
  //
  // Note that this request will ONLY update CSR Module it self,
  // copies of csr will NOT be updated, use it with care!
  //
  // For each cycle, no more than 1 DistributedCSRUpdateReq is valid
  val w = ValidIO(new Bundle {
    val addr = Output(UInt(12.W))
    val data = Output(UInt(XLEN.W))
  })
  def apply(valid: Bool, addr: UInt, data: UInt, src_description: String) = {
    when(valid){
      w.bits.addr := addr
      w.bits.data := data
    }
    println("Distributed CSR update req registered for " + src_description)
  }
}

class L1CacheErrorInfo(implicit p: Parameters) extends XSBundle {
  // L1CacheErrorInfo is also used to encode customized CACHE_ERROR CSR
  val source = Output(new Bundle() {
    val tag = Bool() // l1 tag array
    val data = Bool() // l1 data array
    val l2 = Bool()
  })
  val opType = Output(new Bundle() {
    val fetch = Bool()
    val load = Bool()
    val store = Bool()
    val probe = Bool()
    val release = Bool()
    val atom = Bool()
  })
  val paddr = Output(UInt(PAddrBits.W))

  // report error and paddr to beu
  // bus error unit will receive error info iff ecc_error.valid
  val report_to_beu = Output(Bool())

  // there is an valid error
  // l1 cache error will always be report to CACHE_ERROR csr
  val valid = Output(Bool())

  def toL1BusErrorUnitInfo(): L1BusErrorUnitInfo = {
    val beu_info = Wire(new L1BusErrorUnitInfo)
    beu_info.ecc_error.valid := report_to_beu
    beu_info.ecc_error.bits := paddr
    beu_info
  }
}

class TriggerCf(implicit p: Parameters) extends XSBundle {
  // frontend
  val frontendHit       = Vec(TriggerNum, Bool()) // en && hit
  val frontendCanFire   = Vec(TriggerNum, Bool())
  // backend
  val backendHit        = Vec(TriggerNum, Bool())
  val backendCanFire    = Vec(TriggerNum, Bool())

  // Two situations not allowed:
  // 1. load data comparison
  // 2. store chaining with store
  def getFrontendCanFire = frontendCanFire.reduce(_ || _)
  def getBackendCanFire = backendCanFire.reduce(_ || _)
  def canFire = getFrontendCanFire || getBackendCanFire
  def clear(): Unit = {
    frontendHit.foreach(_ := false.B)
    frontendCanFire.foreach(_ := false.B)
    backendHit.foreach(_ := false.B)
    backendCanFire.foreach(_ := false.B)
  }
}

// these 3 bundles help distribute trigger control signals from CSR
// to Frontend, Load and Store.
class FrontendTdataDistributeIO(implicit p: Parameters) extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class MemTdataDistributeIO(implicit p: Parameters) extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
}

class MatchTriggerIO(implicit p: Parameters) extends XSBundle {
  val matchType = Output(UInt(2.W))
  val select = Output(Bool())
  val timing = Output(Bool())
  val action = Output(Bool())
  val chain = Output(Bool())
  val execute = Output(Bool())
  val store = Output(Bool())
  val load = Output(Bool())
  val tdata2 = Output(UInt(64.W))
}

class StallReasonIO(width: Int) extends Bundle {
  val reason = Output(Vec(width, UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))
  val backReason = Flipped(Valid(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))
}

// custom l2 - l1 interface
class L2ToL1Hint(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val sourceId = UInt(log2Up(cfg.nMissEntries).W)    // tilelink sourceID -> mshr id
  val isKeyword = Bool()                             // miss entry keyword -> L1 load queue replay
}

