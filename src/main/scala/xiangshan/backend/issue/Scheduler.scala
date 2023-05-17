package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.backend.Bundles
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.backend.rename.BusyTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, MemWaitUpdateReq, SqPtr}
import xiangshan.backend.Bundles.{DynInst, IssueQueueWakeUpBundle}

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
  val wbFuBusyTable = new Bundle{
    val fuBusyTableWrite = MixedVec(params.issueBlockParams.map(x => x.genFuBusyTableWriteBundle))
    val fuBusyTableRead = MixedVec(params.issueBlockParams.map(x => Input(x.genFuBusyTableReadBundle)))
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
  val intWriteBack = MixedVec(Vec(backendParams.intPregParams.numWrite,
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))
  val vfWriteBack = MixedVec(Vec(backendParams.vfPregParams.numWrite,
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))
  val toDataPath: MixedVec[MixedVec[DecoupledIO[Bundles.IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))
  val fromDataPath: MixedVec[MixedVec[Bundles.OGRespBundle]] = MixedVec(params.issueBlockParams.map(x => Flipped(x.genOGRespBundle)))

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
    case VfScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numVfStateRead, wrapper.numVfStateWrite)))
    case _ => None
  }

  dispatch2Iq.io match { case dp2iq =>
    dp2iq.redirect <> io.fromCtrlBlock.flush
    dp2iq.in <> io.fromDispatch.uops
    dp2iq.readIntState.foreach(_ <> intBusyTable.get.io.read)
    dp2iq.readVfState.foreach(_ <> vfBusyTable.get.io.read)
  }

  intBusyTable match {
    case Some(bt) =>
      bt.io.allocPregs.zip(io.fromDispatch.allocPregs).foreach { case (btAllocPregs, dpAllocPregs) =>
        btAllocPregs.valid := dpAllocPregs.isInt
        btAllocPregs.bits := dpAllocPregs.preg
      }
      bt.io.wbPregs.zipWithIndex.foreach { case (wb, i) =>
        wb.valid := io.intWriteBack(i).wen && io.intWriteBack(i).intWen
        wb.bits := io.intWriteBack(i).addr
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
        wb.valid := io.vfWriteBack(i).wen && (io.vfWriteBack(i).fpWen || io.vfWriteBack(i).vecWen)
        wb.bits := io.vfWriteBack(i).addr
      }
    case None =>
  }

  val wakeupFromWBVec = Wire(Vec(params.numWakeupFromWB, ValidIO(new IssueQueueWakeUpBundle(params.pregIdxWidth))))
  val writeback = params.schdType match {
    case IntScheduler() => io.intWriteBack
    case MemScheduler() => io.intWriteBack ++ io.vfWriteBack
    case VfScheduler() => io.vfWriteBack
    case _ => Seq()
  }
  wakeupFromWBVec.zip(writeback).foreach { case (sink, source) =>
    sink.valid := source.wen
    sink.bits.rfWen := source.intWen
    sink.bits.fpWen := source.fpWen
    sink.bits.vecWen := source.vecWen
    sink.bits.pdest := source.addr
  }

  io.toDataPath.zipWithIndex.foreach { case (toDp, i) =>
    toDp <> issueQueues(i).io.deq
  }
}

class SchedulerArithImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
{
  println(s"[SchedulerArithImp] " +
    s"has intBusyTable: ${intBusyTable.nonEmpty}, " +
    s"has vfBusyTable: ${vfBusyTable.nonEmpty}")

  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeup := wakeupFromWBVec
    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      deqResp.bits.success := false.B
      deqResp.bits.respType := RSFeedbackType.issueSuccess
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
      deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
      deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.respType := RSFeedbackType.issueSuccess
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
    }
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      og0Resp.bits.success := false.B // Todo: remove it
      og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      og0Resp.bits.addrOH := io.fromDataPath(i)(j).og0resp.bits.addrOH
      og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
      og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      og1Resp.bits.success := false.B
      og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      og1Resp.bits.addrOH := io.fromDataPath(i)(j).og1resp.bits.addrOH
      og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
      og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
    }
    iq.io.wbBusyRead := io.wbFuBusyTable.fuBusyTableRead(i)
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
  println(s"[SchedulerMemImp] " +
    s"has intBusyTable: ${intBusyTable.nonEmpty}, " +
    s"has vfBusyTable: ${vfBusyTable.nonEmpty}")

  val memAddrIQs = issueQueues.filter(iq => iq.params.StdCnt == 0)
  val stAddrIQs = issueQueues.filter(iq => iq.params.StaCnt > 0) // included in memAddrIQs
  val stDataIQs = issueQueues.filter(iq => iq.params.StdCnt > 0)
  require(memAddrIQs.nonEmpty && stDataIQs.nonEmpty)

  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      deqResp.bits.success := false.B
      deqResp.bits.respType := RSFeedbackType.issueSuccess
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
      deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
      deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.respType := RSFeedbackType.issueSuccess
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
    }
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      og0Resp.bits.success := false.B // Todo: remove it
      og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      og0Resp.bits.addrOH := io.fromDataPath(i)(j).og0resp.bits.addrOH
      og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
      og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      og1Resp.bits.success := false.B
      og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      og1Resp.bits.addrOH := io.fromDataPath(i)(j).og1resp.bits.addrOH
      og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
      og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType

      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      io.wbFuBusyTable.fuBusyTableWrite(i)(j).og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
    }
    iq.io.wbBusyRead := io.wbFuBusyTable.fuBusyTableRead(i)
  }

  memAddrIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeup := wakeupFromWBVec
  }


  dispatch2Iq.io.out(1).zip(stAddrIQs(0).io.enq).zip(stDataIQs(0).io.enq).foreach{ case((di, staIQ), stdIQ) =>
    val isAllReady = staIQ.ready && stdIQ.ready
    di.ready := isAllReady
    staIQ.valid := di.valid && isAllReady
    stdIQ.valid := di.valid && isAllReady
  }

  require(stAddrIQs.size == stDataIQs.size, s"number of store address IQs(${stAddrIQs.size}) " +
    s"should be equal to number of data IQs(${stDataIQs})")
  stDataIQs.zip(stAddrIQs).zipWithIndex.foreach { case ((stdIQ, staIQ), i) =>
    stdIQ.io.flush <> io.fromCtrlBlock.flush

    stdIQ.io.enq.zip(staIQ.io.enq).foreach { case (stdIQEnq, staIQEnq) =>
      stdIQEnq.bits  := staIQEnq.bits
      // Store data reuses store addr src(1) in dispatch2iq
      // [dispatch2iq] --src*------src*(0)--> [staIQ]
      //                       \
      //                        ---src*(1)--> [stdIQ]
      // Since the src(1) of sta is easier to get, stdIQEnq.bits.src*(0) is assigned to staIQEnq.bits.src*(1)
      // instead of dispatch2Iq.io.out(x).bits.src*(1)
      stdIQEnq.bits.srcState(0) := staIQEnq.bits.srcState(1)
      stdIQEnq.bits.srcType(0) := staIQEnq.bits.srcType(1)
      stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(1)
      stdIQEnq.bits.sqIdx := staIQEnq.bits.sqIdx
    }
    stdIQ.io.wakeup := wakeupFromWBVec
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
}
