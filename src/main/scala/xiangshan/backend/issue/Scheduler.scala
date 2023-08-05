package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig.{IntData, VAddrData, VecData}
import xiangshan.backend.datapath.WbConfig.{IntWB, VfWB}
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.backend.rename.BusyTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, MemWaitUpdateReq, SqPtr}

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val numIntStateWrite = backendParams.numPregWb(IntData())
  val numVfStateWrite = backendParams.numPregWb(VecData())

  val dispatch2Iq = LazyModule(new Dispatch2Iq(params))
  val issueQueue = params.issueBlockParams.map(x => LazyModule(new IssueQueue(x).suggestName(x.getIQName)))

  lazy val module = params.schdType match {
    case IntScheduler() => new SchedulerArithImp(this)(params, p)
    case MemScheduler() => new SchedulerMemImp(this)(params, p)
    case VfScheduler() => new SchedulerArithImp(this)(params, p)
    case _ => null
  }
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends Bundle {
  // params alias
  private val backendParams = params.backendParam
  private val LoadQueueSize = p(XSCoreParamsKey).VirtualLoadQueueSize
  private val RenameWidth = p(XSCoreParamsKey).RenameWidth
  private val CommitWidth = p(XSCoreParamsKey).CommitWidth
  private val EnsbufferWidth = p(XSCoreParamsKey).EnsbufferWidth
  private val StoreQueueSize = p(XSCoreParamsKey).StoreQueueSize

  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromWbFuBusyTable = new Bundle{
    val fuBusyTableRead = MixedVec(params.issueBlockParams.map(x => Input(x.genWbFuBusyTableReadBundle)))
  }
  val wbFuBusyTable = MixedVec(params.issueBlockParams.map(x => Output(x.genWbFuBusyTableWriteBundle)))

  val fromCtrlBlock = new Bundle {
    val pcVec = Input(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
    val targetVec = Input(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
    val flush = Flipped(ValidIO(new Redirect))
  }
  val fromDispatch = new Bundle {
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val uops =  Vec(params.numUopIn, Flipped(DecoupledIO(new DynInst)))
  }
  val intWriteBack = MixedVec(Vec(backendParams.numPregWb(IntData()),
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))
  val vfWriteBack = MixedVec(Vec(backendParams.numPregWb(VecData()),
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))
  val toDataPath: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))
  val toDataPathAfterDelay: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))
  val fromCancelNetwork = Flipped(MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle)))

  val fromSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpInValidBundle)
  }

  val toSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpOutValidBundle
  }

  val fromDataPath = new Bundle {
    val resp: MixedVec[MixedVec[OGRespBundle]] = MixedVec(params.issueBlockParams.map(x => Flipped(x.genOGRespBundle)))
    val og0Cancel = Input(ExuVec(backendParams.numExu))
    // Todo: remove this after no cancel signal from og1
    val og1Cancel = Input(ExuVec(backendParams.numExu))
    // just be compatible to old code
    def apply(i: Int)(j: Int) = resp(i)(j)
  }


  val memIO = if (params.isMemSchd) Some(new Bundle {
    val lsqEnqIO = Flipped(new LsqEnqIO)
  }) else None
  val fromMem = if (params.isMemSchd) Some(new Bundle {
    val ldaFeedback = Flipped(Vec(params.LduCnt, new MemRSFeedbackIO))
    val staFeedback = Flipped(Vec(params.StaCnt, new MemRSFeedbackIO))
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
  private val iqWakeUpInMap: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    io.fromSchedulers.wakeupVec.map(x => (x.bits.exuIdx, x)).toMap
  private val schdType = params.schdType

  // Modules
  val dispatch2Iq: Dispatch2IqImp = wrapper.dispatch2Iq.module
  val issueQueues: Seq[IssueQueueImp] = wrapper.issueQueue.map(_.module)

  // BusyTable Modules
  val intBusyTable = schdType match {
    case IntScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numIntStateRead, wrapper.numIntStateWrite, IntPhyRegs)))
    case _ => None
  }

  val vfBusyTable = schdType match {
    case VfScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numVfStateRead, wrapper.numVfStateWrite, VfPhyRegs)))
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

  val wakeupFromWBVec = Wire(params.genWBWakeUpSinkValidBundle)
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

  // Connect bundles having the same wakeup source
  issueQueues.zipWithIndex.foreach { case(iq, i) =>
    iq.io.wakeupFromIQ.foreach { wakeUp =>
      wakeUp := iqWakeUpInMap(wakeUp.bits.exuIdx)
    }
    iq.io.og0Cancel := io.fromDataPath.og0Cancel
    iq.io.og1Cancel := io.fromDataPath.og1Cancel
    iq.io.fromCancelNetwork <> io.fromCancelNetwork(i)
  }

  private val iqWakeUpOutMap: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    issueQueues.flatMap(_.io.wakeupToIQ)
      .map(x => (x.bits.exuIdx, x))
      .toMap

  // Connect bundles having the same wakeup source
  io.toSchedulers.wakeupVec.foreach { wakeUp =>
    wakeUp := iqWakeUpOutMap(wakeUp.bits.exuIdx)
  }

  io.toDataPath.zipWithIndex.foreach { case (toDp, i) =>
    toDp <> issueQueues(i).io.deq
  }
  io.toDataPathAfterDelay.zipWithIndex.foreach { case (toDpDy, i) =>
    toDpDy <> issueQueues(i).io.deqDelay
  }

  println(s"[Scheduler] io.fromSchedulers.wakeupVec: ${io.fromSchedulers.wakeupVec.map(x => backendParams.getExuName(x.bits.exuIdx))}")
  println(s"[Scheduler] iqWakeUpInKeys: ${iqWakeUpInMap.keys}")

  println(s"[Scheduler] iqWakeUpOutKeys: ${iqWakeUpOutMap.keys}")
  println(s"[Scheduler] io.toSchedulers.wakeupVec: ${io.toSchedulers.wakeupVec.map(x => backendParams.getExuName(x.bits.exuIdx))}")
}

class SchedulerArithImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
{
//  dontTouch(io.vfWbFuBusyTable)
  println(s"[SchedulerArithImp] " +
    s"has intBusyTable: ${intBusyTable.nonEmpty}, " +
    s"has vfBusyTable: ${vfBusyTable.nonEmpty}")

  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeupFromWB := wakeupFromWBVec
    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      deqResp.bits.respType := RSFeedbackType.issueSuccess
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
      deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
      deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType

    }
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      og0Resp.bits.addrOH := io.fromDataPath(i)(j).og0resp.bits.addrOH
      og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
      og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType

    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      og1Resp.bits.addrOH := io.fromDataPath(i)(j).og1resp.bits.addrOH
      og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
      og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType

    }

    iq.io.wbBusyTableRead := io.fromWbFuBusyTable.fuBusyTableRead(i)
    io.wbFuBusyTable(i) := iq.io.wbBusyTableWrite
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
  val ldAddrIQs = issueQueues.filter(iq => iq.params.LduCnt > 0)
  val stDataIQs = issueQueues.filter(iq => iq.params.StdCnt > 0)
  require(memAddrIQs.nonEmpty && stDataIQs.nonEmpty)

  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.deqResp.zipWithIndex.foreach { case (deqResp, j) =>
      deqResp.valid := iq.io.deq(j).valid && io.toDataPath(i)(j).ready
      deqResp.bits.respType := RSFeedbackType.issueSuccess
      deqResp.bits.addrOH := iq.io.deq(j).bits.addrOH
      deqResp.bits.rfWen := iq.io.deq(j).bits.common.rfWen.getOrElse(false.B)
      deqResp.bits.fuType := iq.io.deq(j).bits.common.fuType

    }
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp.valid := io.fromDataPath(i)(j).og0resp.valid
      og0Resp.bits.respType := io.fromDataPath(i)(j).og0resp.bits.respType
      og0Resp.bits.addrOH := io.fromDataPath(i)(j).og0resp.bits.addrOH
      og0Resp.bits.rfWen := io.fromDataPath(i)(j).og0resp.bits.rfWen
      og0Resp.bits.fuType := io.fromDataPath(i)(j).og0resp.bits.fuType

    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp.valid := io.fromDataPath(i)(j).og1resp.valid
      og1Resp.bits.respType := io.fromDataPath(i)(j).og1resp.bits.respType
      og1Resp.bits.addrOH := io.fromDataPath(i)(j).og1resp.bits.addrOH
      og1Resp.bits.rfWen := io.fromDataPath(i)(j).og1resp.bits.rfWen
      og1Resp.bits.fuType := io.fromDataPath(i)(j).og1resp.bits.fuType

    }
    iq.io.wbBusyTableRead := io.fromWbFuBusyTable.fuBusyTableRead(i)
    io.wbFuBusyTable(i) := iq.io.wbBusyTableWrite
  }

  memAddrIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeupFromWB := wakeupFromWBVec
  }

  ldAddrIQs.foreach {
    case imp: IssueQueueMemAddrImp =>
      imp.io.memIO.get.feedbackIO <> io.fromMem.get.ldaFeedback
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
    case _ =>
  }

  stAddrIQs.foreach {
    case imp: IssueQueueMemAddrImp => imp.io.memIO.get.feedbackIO <> io.fromMem.get.staFeedback
    case _ =>
  }

  private val staIdxSeq = issueQueues.filter(iq => iq.params.StaCnt > 0).map(iq => iq.params.idxInSchBlk)

  for ((idxInSchBlk, i) <- staIdxSeq.zipWithIndex) {
    dispatch2Iq.io.out(idxInSchBlk).zip(stAddrIQs(i).io.enq).zip(stDataIQs(i).io.enq).foreach{ case((di, staIQ), stdIQ) =>
      val isAllReady = staIQ.ready && stdIQ.ready
      di.ready := isAllReady
      staIQ.valid := di.valid && isAllReady
      stdIQ.valid := di.valid && isAllReady
    }
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
    stdIQ.io.wakeupFromWB := wakeupFromWBVec
  }

  val lsqEnqCtrl = Module(new LsqEnqCtrl)

  lsqEnqCtrl.io.redirect <> io.fromCtrlBlock.flush
  lsqEnqCtrl.io.enq <> dispatch2Iq.io.enqLsqIO.get
  lsqEnqCtrl.io.lcommit := io.fromMem.get.lcommit
  lsqEnqCtrl.io.scommit := io.fromMem.get.scommit
  lsqEnqCtrl.io.lqCancelCnt := io.fromMem.get.lqCancelCnt
  lsqEnqCtrl.io.sqCancelCnt := io.fromMem.get.sqCancelCnt
  io.memIO.get.lsqEnqIO <> lsqEnqCtrl.io.enqLsq
}
