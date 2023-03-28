package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.rename.BusyTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, IssueQueueWakeUpBundle}
import xiangshan.v2backend.dispatch.{Dispatch2Iq, Dispatch2IqImp}
import xiangshan.v2backend.issue._
import xiangshan._

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val numIntStateWrite = backendParams.numIntWb
  val numVfStateWrite = backendParams.numVfWb

  val dispatch2Iq = LazyModule(new Dispatch2Iq(params))
  val issueQueue = params.issueBlockParams.map(x => LazyModule(new IssueQueue(x).suggestName(x.getIQName)))

  lazy val module = params.schdType match {
    case IntScheduler() => new SchedulerArithImp(this)(params, p)
    case MemScheduler() => new SchedulerMemImp(this)(params, p)
    case VfScheduler() => new SchedulerArithImp(this)(params, p)
    case _ => null
  }
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromCtrlBlock = new Bundle {
    val pcVec = Input(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
    val targetVec = Input(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
    val flush = Flipped(ValidIO(new Redirect))
  }
  val fromDispatch = new Bundle {
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val uops =  Vec(params.numUopIn, Flipped(DecoupledIO(new DynInst)))
  }
  val writeback = Vec(params.numWakeupFromWB, Flipped(ValidIO(new IssueQueueWakeUpBundle(params.pregIdxWidth))))
  val toDataPath: MixedVec[MixedVec[DecoupledIO[Bundles.IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))

  val memIO = if (params.isMemSchd) Some(new Bundle {
    val feedbackIO = Flipped(Vec(params.StaCnt, new MemRSFeedbackIO))
    val lsqEnqIO = Flipped(new LsqEnqIO)
  }) else None
  val fromMem = if (params.isMemSchd) Some(new Bundle {
    val stIssuePtr = Input(new SqPtr())
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
    // from lsq
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }) else None
  val toMem = if (params.isMemSchd) Some(new Bundle {
    val loadFastMatch = Output(Vec(params.LduCnt, new IssueQueueLoadBundle))
  }) else None
}

abstract class SchedulerImpBase(wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends LazyModuleImp(wrapper)
    with HasXSParameter
{
  val io = IO(new SchedulerIO())

  // alias
  private val schdType = params.schdType
  private val (numRfRead, numRfWrite) = params.numRfReadWrite.getOrElse((0, 0))
  private val numPregs = params.numPregs

  // Modules
  val dispatch2Iq: Dispatch2IqImp = wrapper.dispatch2Iq.module
  val issueQueues: Seq[IssueQueueImp] = wrapper.issueQueue.map(_.module)

  // BusyTable Modules
  val intBusyTable = schdType match {
    case IntScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numIntStateRead, wrapper.numIntStateWrite)))
    case _ => None
  }

  val vfBusyTable = schdType match {
    case VfScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numFpStateRead, wrapper.numVfStateWrite)))
    case _ => None
  }

  println(s"[Scheduler] intBusyTable: ${intBusyTable}, vfBusyTable: ${vfBusyTable}")

  dispatch2Iq.io match { case dp2iq =>
    dp2iq.redirect <> io.fromCtrlBlock.flush
    dp2iq.in <> io.fromDispatch.uops
    dp2iq.readIntState.foreach(_ <> intBusyTable.get.io.read)
    dp2iq.readFpState.foreach(_ <> vfBusyTable.get.io.read)
  }

  dontTouch(dispatch2Iq.io.out)

  intBusyTable match {
    case Some(bt) =>
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isInt
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs.zipWithIndex.foreach { case (wb, i) =>
        wb.valid := io.writeback(i).valid && io.writeback(i).bits.rfWen
        wb.bits := io.writeback(i).bits.pdest
      }
    case None =>
  }

  vfBusyTable match {
    case Some(bt) =>
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isFp
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs.zipWithIndex.foreach { case (wb, i) =>
        wb.valid := io.writeback(i).valid && (io.writeback(i).bits.fpWen || io.writeback(i).bits.vecWen)
        wb.bits := io.writeback(i).bits.pdest
      }
    case None =>
  }

  io.toDataPath.zipWithIndex.foreach { case (toDp, i) =>
    toDp <> issueQueues(i).io.deq
  }
}

class SchedulerArithImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
{
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeup <> io.writeback
    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid
      deqResp.bits.success := io.toDataPath(i)(j).ready // Todo: remove it
      deqResp.bits.respType := Mux(io.toDataPath(i)(j).ready, RSFeedbackType.readRfSuccess, 0.U)
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
    }
    dontTouch(iq.io.enq)
  }

  val iqJumpBundleVec: Seq[IssueQueueJumpBundle] = issueQueues.map {
    case imp: IssueQueueIntImp => imp.io.enqJmp
    case _ => None
  }.filter(_.nonEmpty).flatMap(_.get)
  println(s"[Scheduler] iqJumpBundleVec: ${iqJumpBundleVec}")

  iqJumpBundleVec.zip(io.fromCtrlBlock.pcVec zip io.fromCtrlBlock.targetVec).foreach { case (iqJmp, (pc, target)) =>
    iqJmp.pc := pc
    iqJmp.target := target
  }
}

class SchedulerMemImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
{
  val memAddrIQs = issueQueues.filter(iq => iq.params.StdCnt == 0)
  val stAddrIQs = issueQueues.filter(iq => iq.params.StaCnt > 0) // included in memAddrIQs
  val stDataIQs = issueQueues.filter(iq => iq.params.StdCnt > 0)
  require(memAddrIQs.nonEmpty && stDataIQs.nonEmpty)

  memAddrIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeup <> io.writeback

    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid
      deqResp.bits.success := io.toDataPath(i)(j).ready // Todo: remove it
      deqResp.bits.respType := Mux(io.toDataPath(i)(j).ready, RSFeedbackType.readRfSuccess, 0.U)
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
    }
  }

  require(stAddrIQs.size == stDataIQs.size, s"number of store address IQs(${stAddrIQs.size}) " +
    s"should be equal to number of data IQs(${stDataIQs})")
  stDataIQs.zip(stAddrIQs).zipWithIndex.foreach { case ((stdIQ, staIQ), i) =>
    stdIQ.io.flush <> io.fromCtrlBlock.flush

    stdIQ.io.enq.zip(staIQ.io.enq).foreach { case (stdIQEnq, staIQEnq) =>
      stdIQEnq.valid := staIQEnq.valid
      stdIQEnq.bits  := staIQEnq.bits
      stdIQEnq.bits.srcState(0) := staIQEnq.bits.srcState(1)
      stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(1)
      stdIQEnq.bits.sqIdx := staIQEnq.bits.sqIdx
    }
    stdIQ.io.wakeup <> io.writeback

    stdIQ.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := stdIQ.io.deq(j).valid
      deqResp.bits.success := io.toDataPath(i)(j).ready // Todo: remove it
      deqResp.bits.respType := Mux(io.toDataPath(i)(j).ready, RSFeedbackType.readRfSuccess, 0.U)
      deqResp.bits.addrOH := stdIQ.io.deq(j).bits.addrOH
    }
  }

  val iqMemBundleVec = stAddrIQs.map {
    case imp: IssueQueueMemAddrImp => imp.io.memIO
    case _ => None
  }.filter(_.nonEmpty).map(_.get)
  println(s"[Scheduler] iqMemBundleVec: ${iqMemBundleVec}")

  val lsqEnqCtrl = Module(new LsqEnqCtrl)

  lsqEnqCtrl.io.redirect <> io.fromCtrlBlock.flush
  lsqEnqCtrl.io.enq <> dispatch2Iq.io.enqLsqIO.get
  lsqEnqCtrl.io.lcommit := io.fromMem.get.lcommit
  lsqEnqCtrl.io.scommit := io.fromMem.get.scommit
  lsqEnqCtrl.io.lqCancelCnt := io.fromMem.get.lqCancelCnt
  lsqEnqCtrl.io.sqCancelCnt := io.fromMem.get.sqCancelCnt
  io.memIO.get.lsqEnqIO <> lsqEnqCtrl.io.enqLsq
  require(io.memIO.get.feedbackIO.size == iqMemBundleVec.map(_.feedbackIO.size).sum,
    s"[SchedulerMemImp] io.memIO.feedbackIO.size(${io.memIO.get.feedbackIO.size}) " +
      s"should be equal to sum of memIQ.io.feedbackIO.size(${iqMemBundleVec.map(_.feedbackIO.size).sum})")

  val memIQFeedbackIO: Seq[MemRSFeedbackIO] = iqMemBundleVec.flatMap(_.feedbackIO)
  io.memIO.get.feedbackIO <> memIQFeedbackIO
//  dontTouch(io)
}
