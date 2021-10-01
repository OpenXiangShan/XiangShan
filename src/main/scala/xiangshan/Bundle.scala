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
import xiangshan.frontend.GlobalHistory
import xiangshan.frontend.RASEntry
import xiangshan.frontend.BPUCtrl
import xiangshan.frontend.FtqPtr
import xiangshan.frontend.FtqRead
import xiangshan.frontend.FtqToCtrlIO
import utils._

import scala.math.max
import Chisel.experimental.chiselName
import chipsalliance.rocketchip.config.Parameters
import chisel3.util.BitPat.bitPatToUInt
import xiangshan.frontend.Ftq_Redirect_SRAMEntry

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
  val hist = new GlobalHistory
  val phist = UInt(PathHistoryLength.W)
  val specCnt = Vec(numBr, UInt(10.W))
  val phNewBit = Bool()
  // need pipeline update
  val br_hit = Bool()
  val predTaken = Bool()
  val target = UInt(VAddrBits.W)
  val taken = Bool()
  val isMisPred = Bool()
  val shift = UInt((log2Ceil(numBr)+1).W)
  val addIntoHist = Bool()

  def fromFtqRedirectSram(entry: Ftq_Redirect_SRAMEntry) = {
    this.hist := entry.ghist
    this.phist := entry.phist
    this.phNewBit := entry.phNewBit
    this.rasSp := entry.rasSp
    this.rasEntry := entry.rasEntry
    this.specCnt := entry.specCnt
    this
  }
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
  // This inst will flush all the pipe when it is the oldest inst in ROB,
  // then replay from this inst itself
  val replayInst = Bool()
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
  val isRVF = Bool()
  val selImm = SelImm()
  val imm = UInt(ImmUnion.maxLen.W)
  val commitType = CommitType()
  val fpu = new FPUCtrlSignals
  val isMove = Bool()
  val singleStep = Bool()
  val isFused = UInt(3.W)
  // This inst will flush all the pipe when it is the oldest inst in ROB,
  // then replay from this inst itself
  val replayInst = Bool()

  private def allSignals = srcType ++ Seq(fuType, fuOpType, rfWen, fpWen,
    isXSTrap, noSpecExec, blockBackward, flushPipe, isRVF, selImm)

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
    VecInit(ctrl.srcType.zip(srcState).map{ case (t, s) => SrcType.isPcOrImm(t) || s === SrcState.rdy })
  }
  def doWriteIntRf: Bool = ctrl.rfWen && ctrl.ldest =/= 0.U
  def doWriteFpRf: Bool = ctrl.fpWen
  def clearExceptions(): MicroOp = {
    cf.exceptionVec.map(_ := false.B)
    ctrl.replayInst := false.B
    ctrl.flushPipe := false.B
    this
  }
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

class RobCommitInfo(implicit p: Parameters) extends XSBundle {
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
  val isFused = UInt(3.W)

  // these should be optimized for synthesis verilog
  val pc = UInt(VAddrBits.W)
}

class RobCommitIO(implicit p: Parameters) extends XSBundle {
  val isWalk = Output(Bool())
  val valid = Vec(CommitWidth, Output(Bool()))
  val info = Vec(CommitWidth, Output(new RobCommitInfo))

  def hasWalkInstr = isWalk && valid.asUInt.orR

  def hasCommitInstr = !isWalk && valid.asUInt.orR
}

class RSFeedback(implicit p: Parameters) extends XSBundle {
  val rsIdx = UInt(log2Up(IssQueSize).W)
  val hit = Bool()
  val flushState = Bool()
  val sourceType = RSFeedbackType()
}

class FrontendToCtrlIO(implicit p: Parameters) extends XSBundle {
  // to backend end
  val cfVec = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val fromFtq = new FtqToCtrlIO
  // from backend
  val toFtq = Flipped(new CtrlToFtqIO)
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
