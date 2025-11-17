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
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util.experimental.decode.EspressoMinimizer

import utility._
import utils._

import org.chipsalliance.cde.config.Parameters

import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuCtrl
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ftq.FtqToCtrlIO
import xiangshan.frontend.FrontendRedirect

import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.decode.XDecode
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.NewCSR.Mcontrol6
import xiangshan.backend.fu.NewCSR.Tdata1Bundle
import xiangshan.backend.fu.NewCSR.Tdata2Bundle
import xiangshan.backend.rob.RobBundles.RobCommitEntryBundle
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.HasDCacheParameters

import xiangshan.mem.LqPtr
import xiangshan.mem.SqPtr
import xiangshan.mem.prefetch.PrefetchCtrl

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

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow(implicit p: Parameters) extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val foldpc = UInt(MemPredPCWidth.W)
  val exceptionVec = ExceptionVec()
  val backendException = Bool()
  val trigger = TriggerAction()
  val isRvc = Bool()
  val fixedTaken = Bool()
  val predTaken  = Bool()
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
  val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  val isLastInFtqEntry = Bool()
  val debug_seqNum = InstSeqNum()
}


class FPUCtrlSignals(implicit p: Parameters) extends XSBundle {
  val typeTagOut = UInt(2.W) // H S D
  val wflags = Bool()
  val typ = UInt(2.W)
  val fmt = UInt(2.W)
  val rm = UInt(3.W)
}

// Decode DecodeWidth insts at Decode Stage
class CtrlSignals(implicit p: Parameters) extends XSBundle {
  // val debug_globalID = UInt(XLEN.W)
  val srcType = Vec(4, SrcType())
  val lsrc = Vec(4, UInt(LogicRegsWidth.W))
  val ldest = UInt(LogicRegsWidth.W)
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
  def needWriteRf: Bool = rfWen || fpWen || vecWen
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

class Redirect(implicit p: Parameters) extends FrontendRedirect {
  // for frontend
  val level:       Bool = Bool()
  val backendIGPF: Bool = Bool() // instruction guest page fault
  val backendIPF: Bool = Bool() // instruction page fault
  val backendIAF: Bool = Bool() // instruction access fault

  def hasBackendFault: Bool = backendIGPF || backendIPF || backendIAF

  // for backend and memory
  val robIdx = new RobPtr
  val interrupt: Bool = Bool()
  val isMisPred: Bool = Bool()

  val fullTarget: UInt = UInt(XLEN.W) // only used for tval storage in backend

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset: UInt = UInt(FetchBlockInstOffsetWidth.W)
  val stIsRVC  = Bool()

  val debug_runahead_checkpoint_id = UInt(64.W)
  val debugIsCtrl = Bool()
  val debugIsMemVio = Bool()

  def flushItself() = RedirectLevel.flushItself(level)

  def getPcOffset() = {
    val ftqOffset = (this.ftqOffset << instOffsetBits).asUInt
    val rvcOffset = Mux(this.isRVC, 0.U, 2.U)
    val thisPcOffset = SignExt(ftqOffset -& rvcOffset, VAddrBits)
    thisPcOffset
  }
  def getStPcOffset() = {
    val ftqOffset = (this.stFtqOffset << instOffsetBits).asUInt
    val rvcOffset = Mux(this.stIsRVC, 0.U, 2.U)
    val thisPcOffset = SignExt(ftqOffset -& rvcOffset, VAddrBits)
    thisPcOffset
  }

  def getNextPcOffset() = {
    val ftqOffset = (this.ftqOffset << instOffsetBits).asUInt
    val nextPcOffset = ftqOffset +& 2.U
    nextPcOffset
  }
}

object Redirect extends HasCircularQueuePtrHelper {

  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
}

class Resolve(implicit p: Parameters) extends XSBundle {
  val ftqIdx: FtqPtr = new FtqPtr
  val ftqOffset: UInt = UInt(FetchBlockInstOffsetWidth.W)
  val pc: PrunedAddr = PrunedAddr(VAddrBits)
  val target: PrunedAddr = PrunedAddr(VAddrBits)
  val taken: Bool = Bool()
  val mispredict: Bool = Bool()
  val attribute: BranchAttribute = new BranchAttribute
  val debug_isRVC: Option[Bool] = Option.when(!env.FPGAPlatform)(Bool())
}

class CallRetCommit(implicit p: Parameters) extends XSBundle {
  val rasAction: UInt = BranchAttribute.RasAction()
  val ftqPtr: FtqPtr = new FtqPtr
}

class ResetPregStateReq(implicit p: Parameters) extends XSBundle {
  // NOTE: set isInt and isFp both to 'false' when invalid
  val isInt = Bool()
  val isFp = Bool()
  val isVec = Bool()
  val isV0 = Bool()
  val isVl = Bool()
  val preg = UInt(PhyRegIdxWidth.W)
}

class DebugBundle(implicit p: Parameters) extends XSBundle {
  val isMMIO = Bool()
  val isNCIO = Bool()
  val isPerfCnt = Bool()
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)

  def isSkipDiff: Bool = isMMIO || isNCIO || isPerfCnt
  /* add L/S inst info in EXU */
  // val L1toL2TlbLatency = UInt(XLEN.W)
  // val levelTlbHit = UInt(2.W)
}

class SoftIfetchPrefetchBundle(implicit p: Parameters) extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
}

class ExternalInterruptIO(implicit p: Parameters) extends XSBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
  val seip = Input(Bool())
  val debug = Input(Bool())
  val nmi = new NonmaskableInterruptIO()
}

class NonmaskableInterruptIO() extends Bundle {
  val nmi_31 = Input(Bool())
  val nmi_43 = Input(Bool())
  // reserve for other nmi type
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
  val ldest = UInt(LogicRegsWidth.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val vecWen = Bool()
  val v0Wen = Bool()
  val vlWen = Bool()
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

class RSFeedback(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val hit = Bool()
  val flushState = Bool()
  val sourceType = RSFeedbackType()
  val dataInvalidSqIdx = new SqPtr
  val sqIdx = new SqPtr
  val lqIdx = new LqPtr
}

class MemRSFeedbackIO(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
  // Note: you need to update in implicit Parameters p before imp MemRSFeedbackIO
  // for instance: MemRSFeedbackIO()(updateP)
  val feedbackSlow = ValidIO(new RSFeedback(isVector)) // dcache miss queue full, dtlb miss
  val feedbackFast = ValidIO(new RSFeedback(isVector)) // bank conflict
}

class LoadCancelIO(implicit p: Parameters) extends XSBundle {
  val ld1Cancel = Bool()
  val ld2Cancel = Bool()
}

class WfiReqBundle extends Bundle {
  val wfiReq = Output(Bool())
  val wfiSafe = Input(Bool())
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

  val wfi = Flipped(new WfiReqBundle)
}

class SatpStruct(implicit p: Parameters) extends XSBundle {
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbSatpBundle(implicit p: Parameters) extends SatpStruct {
  val changed = Bool()

  // Todo: remove it
  def apply(satp_value: UInt): Unit = {
    require(satp_value.getWidth == XLEN)
    val sa = satp_value.asTypeOf(new SatpStruct)
    mode := sa.mode
    asid := sa.asid
    ppn := sa.ppn
    changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
  }
}

class HgatpStruct(implicit p: Parameters) extends XSBundle {
  val mode = UInt(4.W)
  val vmid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbHgatpBundle(implicit p: Parameters) extends HgatpStruct {
  val changed = Bool()

  // Todo: remove it
  def apply(hgatp_value: UInt): Unit = {
    require(hgatp_value.getWidth == XLEN)
    val sa = hgatp_value.asTypeOf(new HgatpStruct)
    mode := sa.mode
    vmid := sa.vmid
    ppn := sa.ppn
    changed := DataChanged(sa.vmid) // when ppn is changed, software need do the flush
  }
}

// add mbmc csr
class MbmcStruct(implicit p: Parameters) extends XSBundle {
  val KEYIDEN = UInt(1.W)
  val BME = UInt(1.W)
  val CMODE = UInt(1.W)
  val BCLEAR = UInt(1.W)
  val BMA = UInt(58.W)
}

class TlbMbmcBundle(implicit p: Parameters) extends MbmcStruct {
  def apply(mbmc_value: UInt): Unit = {
    require(mbmc_value.getWidth == XLEN)
    val mc = mbmc_value.asTypeOf(new MbmcStruct)
    KEYIDEN := mc.KEYIDEN
    BME := mc.BME
    CMODE := mc.CMODE
    BCLEAR := mc.BCLEAR
    BMA := mc.BMA
  }
}

class TlbCsrBundle(implicit p: Parameters) extends XSBundle {
  val satp = new TlbSatpBundle()
  val vsatp = new TlbSatpBundle()
  val hgatp = new TlbHgatpBundle()
  val mbmc = new TlbMbmcBundle()
  val priv = new Bundle {
    val mxr = Bool()
    val sum = Bool()
    val vmxr = Bool()
    val vsum = Bool()
    val virt = Bool()
    val virt_changed = Bool()
    val spvp = UInt(1.W)
    val imode = UInt(2.W)
    val dmode = UInt(2.W)
  }
  val mPBMTE = Bool()
  val hPBMTE = Bool()
  val pmm = new Bundle {
    val mseccfg = UInt(2.W)
    val menvcfg = UInt(2.W)
    val henvcfg = UInt(2.W)
    val hstatus = UInt(2.W)
    val senvcfg = UInt(2.W)
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
  val pf_ctrl = Output(new PrefetchCtrl)
  // Load violation predictor
  val lvpred_disable = Output(Bool())
  val no_spec_load = Output(Bool())
  val storeset_wait_store = Output(Bool())
  val storeset_no_fast_wakeup = Output(Bool())
  val lvpred_timeout = Output(UInt(5.W))
  // Branch predictor
  val bp_ctrl = Output(new BpuCtrl)
  // Memory Block
  val sbuffer_threshold = Output(UInt(4.W))
  val ldld_vio_check_enable = Output(Bool())
  val soft_prefetch_enable = Output(Bool())
  val cache_error_enable = Output(Bool())
  val uncache_write_outstanding_enable = Output(Bool())
  val hd_misalign_st_enable = Output(Bool())
  val hd_misalign_ld_enable = Output(Bool())
  val power_down_enable = Output(Bool())
  val flush_l2_enable = Output(Bool())
  // Rename
  val fusion_enable = Output(Bool())
  val wfi_enable = Output(Bool())

  // distribute csr write signal
  val distribute_csr = new DistributedCSRIO()
  // TODO: move it to a new bundle, since single step is not a custom control signal
  val singlestep = Output(Bool())
  val frontend_trigger = new FrontendTdataDistributeIO()
  val mem_trigger = new MemTdataDistributeIO()
  // Virtualization Mode
  val virtMode = Output(Bool())
  // xstatus.fs field is off
  val fsIsOff = Output(Bool())
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

class AddrTransType(implicit p: Parameters) extends XSBundle {
  val bare, sv39, sv39x4, sv48, sv48x4 = Bool()

  def checkAccessFault(target: UInt): Bool = bare && target(XLEN - 1, PAddrBits).orR
  def checkPageFault(target: UInt): Bool =
    sv39 && target(XLEN - 1, 39) =/= VecInit.fill(XLEN - 39)(target(38)).asUInt ||
    sv48 && target(XLEN - 1, 48) =/= VecInit.fill(XLEN - 48)(target(47)).asUInt
  def checkGuestPageFault(target: UInt): Bool =
    sv39x4 && target(XLEN - 1, 41).orR || sv48x4 && target(XLEN - 1, 50).orR

  def shouldBeSext: Bool = sv39 || sv48
  def shouldBeZext: Bool = bare || sv39x4 || sv48x4
}

object AddrTransType {
  def apply(bare: Boolean = false,
            sv39: Boolean = false,
            sv39x4: Boolean = false,
            sv48: Boolean = false,
            sv48x4: Boolean = false)(implicit p: Parameters): AddrTransType =
    (new AddrTransType).Lit(_.bare -> bare.B,
                            _.sv39 -> sv39.B,
                            _.sv39x4 -> sv39x4.B,
                            _.sv48 -> sv48.B,
                            _.sv48x4 -> sv48x4.B)

  def apply(bare: Bool, sv39: Bool, sv39x4: Bool, sv48: Bool, sv48x4: Bool)(implicit p: Parameters): AddrTransType = {
    val addrTransType = Wire(new AddrTransType)
    addrTransType.bare := bare
    addrTransType.sv39 := sv39
    addrTransType.sv39x4 := sv39x4
    addrTransType.sv48 := sv48
    addrTransType.sv48x4 := sv48x4
    addrTransType
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

  def toL1BusErrorUnitInfo(valid: Bool): L1BusErrorUnitInfo = {
    val beu_info = Wire(new L1BusErrorUnitInfo)
    beu_info.ecc_error.valid := valid && report_to_beu
    beu_info.ecc_error.bits := paddr
    beu_info
  }
}

object TriggerAction extends NamedUInt(4) {
  // Put breakpoint Exception gererated by trigger in ExceptionVec[3].
  def BreakpointExp = 0.U(width.W)  // raise breakpoint exception
  def DebugMode     = 1.U(width.W)  // enter debug mode
  def TraceOn       = 2.U(width.W)
  def TraceOff      = 3.U(width.W)
  def TraceNotify   = 4.U(width.W)
  def None          = 15.U(width.W) // use triggerAction = 15.U to express that action is None;

  def isExp(action: UInt)   = action === BreakpointExp
  def isDmode(action: UInt) = action === DebugMode
  def isNone(action: UInt)  = action === None
}

// these 3 bundles help distribute trigger control signals from CSR
// to Frontend, Load and Store.
class FrontendTdataDistributeIO(implicit p: Parameters) extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
  val debugMode = Output(Bool())
  val triggerCanRaiseBpExp = Output(Bool())
}

class MemTdataDistributeIO(implicit p: Parameters) extends XSBundle {
  val tUpdate = ValidIO(new Bundle {
    val addr = Output(UInt(log2Up(TriggerNum).W))
    val tdata = new MatchTriggerIO
  })
  val tEnableVec: Vec[Bool] = Output(Vec(TriggerNum, Bool()))
  val debugMode = Output(Bool())
  val triggerCanRaiseBpExp  = Output(Bool())
}

class MatchTriggerIO(implicit p: Parameters) extends XSBundle {
  val matchType = Output(UInt(2.W))
  val select    = Output(Bool())
  val timing    = Output(Bool())
  val action    = Output(TriggerAction())
  val chain     = Output(Bool())
  val execute   = Output(Bool())
  val store     = Output(Bool())
  val load      = Output(Bool())
  val tdata2    = Output(UInt(64.W))

  def GenTdataDistribute(tdata1: Tdata1Bundle, tdata2: Tdata2Bundle): MatchTriggerIO = {
    val mcontrol6 = Wire(new Mcontrol6)
    mcontrol6 := tdata1.DATA.asUInt
    this.matchType := mcontrol6.MATCH.asUInt
    this.select    := mcontrol6.SELECT.asBool
    this.timing    := false.B
    this.action    := mcontrol6.ACTION.asUInt
    this.chain     := mcontrol6.CHAIN.asBool
    this.execute   := mcontrol6.EXECUTE.asBool
    this.load      := mcontrol6.LOAD.asBool
    this.store     := mcontrol6.STORE.asBool
    this.tdata2    := tdata2.asUInt
    this
  }
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

class TopDownInfo(implicit p: Parameters) extends XSBundle {
  val lqEmpty = Input(Bool())
  val sqEmpty = Input(Bool())
  val l1Miss = Input(Bool())
  val noUopsIssued = Output(Bool())
  val l2TopMiss = Input(new TopDownFromL2Top)
}

class TopDownFromL2Top(implicit p: Parameters) extends XSBundle {
  val l2Miss = Bool()
  val l3Miss = Bool()
}

class UopTopDown(implicit p: Parameters) extends XSBundle {
  val uopsIssued = Output(Bool())
  val uopsIssuedCnt = Output(UInt((log2Up(p(XSCoreParamsKey).backendParams.allIssueParams.size)).W))
  val noStoreIssued = Output(Bool())
}

class LowPowerIO(implicit p: Parameters) extends Bundle {
  /* i_*: SoC -> CPU   o_*: CPU -> SoC */
  val o_cpu_no_op = Output(Bool()) 
  //physical power down 
  val i_cpu_pwrdown_req_n = Input(Bool())
  val o_cpu_pwrdown_ack_n = Output(Bool())
  // power on/off sequence control for Core iso/rst
  val i_cpu_iso_en= Input(Bool())
  val i_cpu_sw_rst_n = Input(Bool())
}
