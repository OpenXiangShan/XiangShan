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
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.decode.{ImmUnion, XDecode}
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.HasBPUParameter
import xiangshan.frontend.{AllFoldedHistories, CircularGlobalHistory, GlobalHistory, ShiftingGlobalHistory}
import xiangshan.frontend.RASEntry
import xiangshan.frontend.BPUCtrl
import xiangshan.frontend.FtqPtr
import xiangshan.frontend.CGHPtr
import xiangshan.frontend.FtqRead
import xiangshan.frontend.FtqToCtrlIO
import utils._

import scala.math.max
import Chisel.experimental.chiselName
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.BitPat.bitPatToUInt
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.fu.PMPEntry
import xiangshan.frontend.Ftq_Redirect_SRAMEntry
import xiangshan.frontend.AllFoldedHistories
import xiangshan.frontend.AllAheadFoldedHistoryOldestBits

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
  val tlbMiss = 0.U(3.W)
  val mshrFull = 1.U(3.W)
  val dataInvalid = 2.U(3.W)
  val bankConflict = 3.U(3.W)
  val ldVioCheckRedo = 4.U(3.W)

  def apply() = UInt(3.W)
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
  val rasSp = UInt(log2Up(RasSize).W)
  val rasEntry = new RASEntry
  // val hist = new ShiftingGlobalHistory
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val ghr = UInt(UbtbGHRLength.W)
  val histPtr = new CGHPtr
  val specCnt = Vec(numBr, UInt(10.W))
  // need pipeline update
  val br_hit = Bool()
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
    this.rasSp := entry.rasSp
    this.rasEntry := entry.rasEntry
    this
  }
}

// Dequeue DecodeWidth insts from Ibuffer
class CtrlFlow(implicit p: Parameters) extends XSBundle {
  val instr = UInt(32.W)
  val pc = UInt(VAddrBits.W)
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
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val commitType = CommitType()
  val fpu = new FPUCtrlSignals
  val isMove = Bool()
  val singleStep = Bool()
  // This inst will flush all the pipe when it is the oldest inst in ROB,
  // then replay from this inst itself
  val replayInst = Bool()

  private def allSignals = srcType ++ Seq(fuType, fuOpType, rfWen, fpWen,
    isXSTrap, noSpecExec, blockBackward, flushPipe, selImm)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]): CtrlSignals = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, XDecode.decodeDefault, table)
    allSignals zip decoder foreach { case (s, d) => s := d }
    commitType := DontCare
    this
  }

  def decode(bit: List[BitPat]): CtrlSignals = {
    allSignals.zip(bit.map(bitPatToUInt(_))).foreach{ case (s, d) => s := d }
    this
  }

  def isWFI: Bool = fuType === FuType.csr && fuOpType === CSROpType.wfi
  def isSoftPrefetch: Bool = {
    fuType === FuType.alu && fuOpType === ALUOpType.or && selImm === SelImm.IMM_I && ldest === 0.U
  }
}

class CfCtrl(implicit p: Parameters) extends XSBundle {
  val cf = new CtrlFlow
  val ctrl = new CtrlSignals
}

class PerfDebugInfo(implicit p: Parameters) extends XSBundle {
  val eliminatedMove = Bool()
  // val fetchTime = UInt(64.W)
  val renameTime = UInt(XLEN.W)
  val dispatchTime = UInt(XLEN.W)
  val enqRsTime = UInt(XLEN.W)
  val selectTime = UInt(XLEN.W)
  val issueTime = UInt(XLEN.W)
  val writebackTime = UInt(XLEN.W)
  // val commitTime = UInt(64.W)
  val runahead_checkpoint_id = UInt(64.W)
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
  val robIdx = new RobPtr
  val lqIdx = new LqPtr
  val sqIdx = new SqPtr
  val eliminatedMove = Bool()
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
  // Assume only the LUI instruction is decoded with IMM_U in ALU.
  def isLUI: Bool = ctrl.selImm === SelImm.IMM_U && ctrl.fuType === FuType.alu
  // This MicroOp is used to wakeup another uop (the successor: (psrc, srcType).
  def wakeup(successor: Seq[(UInt, UInt)], exuCfg: ExuConfig): Seq[(Bool, Bool)] = {
    successor.map{ case (src, srcType) =>
      val pdestMatch = pdest === src
      // For state: no need to check whether src is x0/imm/pc because they are always ready.
      val rfStateMatch = if (exuCfg.readIntRf) ctrl.rfWen else false.B
      val fpMatch = if (exuCfg.readFpRf) ctrl.fpWen else false.B
      val bothIntFp = exuCfg.readIntRf && exuCfg.readFpRf
      val bothStateMatch = Mux(SrcType.regIsFp(srcType), fpMatch, rfStateMatch)
      val stateCond = pdestMatch && (if (bothIntFp) bothStateMatch else rfStateMatch || fpMatch)
      // For data: types are matched and int pdest is not $zero.
      val rfDataMatch = if (exuCfg.readIntRf) ctrl.rfWen && src =/= 0.U else false.B
      val dataCond = pdestMatch && (rfDataMatch && SrcType.isReg(srcType) || fpMatch && SrcType.isFp(srcType))
      (stateCond, dataCond)
    }
  }
  // This MicroOp is used to wakeup another uop (the successor: MicroOp).
  def wakeup(successor: MicroOp, exuCfg: ExuConfig): Seq[(Bool, Bool)] = {
    wakeup(successor.psrc.zip(successor.ctrl.srcType), exuCfg)
  }
  def isJump: Bool = FuType.isJumpExu(ctrl.fuType)
}

class MicroOpRbExt(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val flag = UInt(1.W)
}

class Redirect(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfiUpdate = new CfiUpdateInfo

  val stFtqIdx = new FtqPtr // for load violation predict
  val stFtqOffset = UInt(log2Up(PredictWidth).W)

  val debug_runahead_checkpoint_id = UInt(64.W)

  // def isUnconditional() = RedirectLevel.isUnconditional(level)
  def flushItself() = RedirectLevel.flushItself(level)
  // def isException() = RedirectLevel.isException(level)
}

class Dp1ToDp2IO(implicit p: Parameters) extends XSBundle {
  val intDqToDp2 = Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
  val fpDqToDp2 = Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp))
  val lsDqToDp2 = Vec(dpParams.LsDqDeqWidth, DecoupledIO(new MicroOp))
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
}

class ExuInput(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val src = Vec(3, UInt(XLEN.W))
}

class ExuOutput(implicit p: Parameters) extends XSBundle {
  val uop = new MicroOp
  val data = UInt(XLEN.W)
  val fflags = UInt(5.W)
  val redirectValid = Bool()
  val redirect = new Redirect
  val debug = new DebugBundle
}

class ExternalInterruptIO(implicit p: Parameters) extends XSBundle {
  val mtip = Input(Bool())
  val msip = Input(Bool())
  val meip = Input(Bool())
  val seip = Input(Bool())
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

class RobDispatchData(implicit p: Parameters) extends XSBundle {
  val ldest = UInt(5.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val wflags = Bool()
  val commitType = CommitType()
  val pdest = UInt(PhyRegIdxWidth.W)
  val old_pdest = UInt(PhyRegIdxWidth.W)
  val ftqIdx = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
}

class RobCommitInfo(implicit p: Parameters) extends RobDispatchData {
  // these should be optimized for synthesis verilog
  val pc = UInt(VAddrBits.W)

  def connectDispatchData(data: RobDispatchData) {
    ldest := data.ldest
    rfWen := data.rfWen
    fpWen := data.fpWen
    wflags := data.wflags
    commitType := data.commitType
    pdest := data.pdest
    old_pdest := data.old_pdest
    ftqIdx := data.ftqIdx
    ftqOffset := data.ftqOffset
  }
}

class RobCommitIO(implicit p: Parameters) extends XSBundle {
  val isCommit = Output(Bool())
  val commitValid = Vec(CommitWidth, Output(Bool()))

  val isWalk = Output(Bool())
  // valid bits optimized for walk
  val walkValid = Vec(CommitWidth, Output(Bool()))

  val info = Vec(CommitWidth, Output(new RobCommitInfo))

  def hasWalkInstr: Bool = isWalk && walkValid.asUInt.orR
  def hasCommitInstr: Bool = isCommit && commitValid.asUInt.orR
}

class RSFeedback(implicit p: Parameters) extends XSBundle {
  val rsIdx = UInt(log2Up(IssQueSize).W)
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
  val rsIdx = Input(UInt(log2Up(IssQueSize).W))
  val isFirstIssue = Input(Bool())
}

class FrontendToCtrlIO(implicit p: Parameters) extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val fromFtq = new FtqToCtrlIO
  // from backend
  val toFtq = Flipped(new CtrlToFtqIO)
}

class SatpStruct extends Bundle {
  val mode = UInt(4.W)
  val asid = UInt(16.W)
  val ppn  = UInt(44.W)
}

class TlbCsrBundle(implicit p: Parameters) extends XSBundle {
  val satp = new Bundle {
    val changed = Bool()
    val mode = UInt(4.W) // TODO: may change number to parameter
    val asid = UInt(16.W)
    val ppn = UInt(44.W) // just use PAddrBits - 3 - vpnnLen

    def apply(satp_value: UInt): Unit = {
      require(satp_value.getWidth == XLEN)
      val sa = satp_value.asTypeOf(new SatpStruct)
      mode := sa.mode
      asid := sa.asid
      ppn := sa.ppn
      changed := DataChanged(sa.asid) // when ppn is changed, software need do the flush
    }
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
    val asid = UInt(AsidLength.W)
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
  val l1I_pf_enable = Output(Bool())
  val l2_pf_enable = Output(Bool())
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
  // Rename
  val move_elim_enable = Output(Bool())
  // Decode
  val svinval_enable = Output(Bool())

  // distribute csr write signal
  val distribute_csr = new DistributedCSRIO()

  val singlestep = Output(Bool())
  val frontend_trigger = new FrontendTdataDistributeIO()
  val mem_trigger = new MemTdataDistributeIO()
  val trigger_enable = Output(Vec(10, Bool()))
}

class DistributedCSRIO(implicit p: Parameters) extends XSBundle {
  // CSR has been writen by csr inst, copies of csr should be updated
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

/* TODO how to trigger on next inst?
1. If hit is determined at frontend, then set a "next instr" trap at dispatch like singlestep
2. If it is determined at Load(meaning it must be "hit after", then it must not be a jump. So we can let it commit and set
xret csr to pc + 4/ + 2
2.5 The problem is to let it commit. This is the real TODO
3. If it is load and hit before just treat it as regular load exception
 */

// This bundle carries trigger hit info along the pipeline
// Now there are 10 triggers divided into 5 groups of 2
// These groups are
// (if if) (store store) (load loid) (if store) (if load)

// Triggers in the same group can chain, meaning that they only
// fire is both triggers in the group matches (the triggerHitVec bit is asserted)
// Chaining of trigger No. (2i) and (2i+1) is indicated by triggerChainVec(i)
// Timing of 0 means trap at current inst, 1 means trap at next inst
// Chaining and timing and the validness of a trigger is controlled by csr
// In two chained triggers, if they have different timing, both won't fire
//class TriggerCf (implicit p: Parameters) extends XSBundle {
//  val triggerHitVec = Vec(10, Bool())
//  val triggerTiming = Vec(10, Bool())
//  val triggerChainVec = Vec(5, Bool())
//}

class TriggerCf(implicit p: Parameters) extends XSBundle {
  // frontend
  val frontendHit = Vec(4, Bool())
//  val frontendTiming = Vec(4, Bool())
//  val frontendHitNext = Vec(4, Bool())

//  val frontendException = Bool()
  // backend
  val backendEn = Vec(2, Bool()) // Hit(6) && chain(4) , Hit(8) && chain(4)
  val backendHit = Vec(6, Bool())
//  val backendTiming = Vec(6, Bool()) // trigger enable fro chain

  // Two situations not allowed:
  // 1. load data comparison
  // 2. store chaining with store
  def getHitFrontend = frontendHit.reduce(_ || _)
  def getHitBackend = backendHit.reduce(_ || _)
  def hit = getHitFrontend || getHitBackend
  def clear(): Unit = {
    frontendHit.foreach(_ := false.B)
    backendEn.foreach(_ := false.B)
    backendHit.foreach(_ := false.B)
  }
}

// these 3 bundles help distribute trigger control signals from CSR
// to Frontend, Load and Store.
class FrontendTdataDistributeIO(implicit p: Parameters)  extends XSBundle {
    val t = Valid(new Bundle {
      val addr = Output(UInt(2.W))
      val tdata = new MatchTriggerIO
    })
  }

class MemTdataDistributeIO(implicit p: Parameters)  extends XSBundle {
  val t = Valid(new Bundle {
    val addr = Output(UInt(3.W))
    val tdata = new MatchTriggerIO
  })
}

class MatchTriggerIO(implicit p: Parameters) extends XSBundle {
  val matchType = Output(UInt(2.W))
  val select = Output(Bool())
  val timing = Output(Bool())
  val action = Output(Bool())
  val chain = Output(Bool())
  val tdata2 = Output(UInt(64.W))
}
