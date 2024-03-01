package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig.{IntData, VAddrData, VecData}
import xiangshan.backend.datapath.WbConfig.{IntWB, VfWB}
import xiangshan.backend.fu.FuType
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.backend.rename.BusyTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, MemWaitUpdateReq, SqPtr, LqPtr}

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val numIntStateWrite = backendParams.numPregWb(IntData())
  val numVfStateWrite = backendParams.numPregWb(VecData())

  val dispatch2Iq = LazyModule(new Dispatch2Iq(params))
  val issueQueue = params.issueBlockParams.map(x => LazyModule(new IssueQueue(x).suggestName(x.getIQName)))

  lazy val module: SchedulerImpBase = params.schdType match {
    case IntScheduler() => new SchedulerArithImp(this)(params, p)
    case MemScheduler() => new SchedulerMemImp(this)(params, p)
    case VfScheduler() => new SchedulerArithImp(this)(params, p)
    case _ => null
  }
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends XSBundle {
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize

  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromWbFuBusyTable = new Bundle{
    val fuBusyTableRead = MixedVec(params.issueBlockParams.map(x => Input(x.genWbFuBusyTableReadBundle)))
  }
  val wbFuBusyTable = MixedVec(params.issueBlockParams.map(x => Output(x.genWbFuBusyTableWriteBundle)))
  val IQValidNumVec = Output(MixedVec(backendParams.genIQValidNumBundle))

  val fromCtrlBlock = new Bundle {
    val pcVec = Input(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
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
  val toDataPathAfterDelay: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))

  val fromSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpInValidBundle)
  }

  val toSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpOutValidBundle
  }

  val fromDataPath = new Bundle {
    val resp: MixedVec[MixedVec[OGRespBundle]] = MixedVec(params.issueBlockParams.map(x => Flipped(x.genOGRespBundle)))
    val og0Cancel = Input(ExuOH(backendParams.numExu))
    // Todo: remove this after no cancel signal from og1
    val og1Cancel = Input(ExuOH(backendParams.numExu))
    val cancelToBusyTable = Vec(backendParams.numExu, Flipped(ValidIO(new CancelSignal)))
    // just be compatible to old code
    def apply(i: Int)(j: Int) = resp(i)(j)
  }

  val loadFinalIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.LdExuCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))
  val memAddrIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.LdExuCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))

  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val memIO = if (params.isMemSchd) Some(new Bundle {
    val lsqEnqIO = Flipped(new LsqEnqIO)
  }) else None
  val fromMem = if (params.isMemSchd) Some(new Bundle {
    val ldaFeedback = Flipped(Vec(params.LduCnt, new MemRSFeedbackIO))
    val staFeedback = Flipped(Vec(params.StaCnt, new MemRSFeedbackIO))
    val hyuFeedback = Flipped(Vec(params.HyuCnt, new MemRSFeedbackIO))
    val stIssuePtr = Input(new SqPtr())
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
    val wakeup = Vec(params.LdExuCnt, Flipped(Valid(new DynInst)))
    val lqDeqPtr = Input(new LqPtr)
    val sqDeqPtr = Input(new SqPtr)
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
  if (params.isIntSchd) {
    dispatch2Iq.io.IQValidNumVec.get := io.IQValidNumVec
    io.IQValidNumVec := MixedVecInit(issueQueues.map(_.io.validCntDeqVec))
  }
  else io.IQValidNumVec := 0.U.asTypeOf(io.IQValidNumVec)

  // valid count
  dispatch2Iq.io.iqValidCnt := issueQueues.filter(_.params.StdCnt == 0).map(_.io.status.validCnt)

  // BusyTable Modules
  val intBusyTable = schdType match {
    case IntScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numIntStateRead, wrapper.numIntStateWrite, IntPhyRegs, IntWB())))
    case _ => None
  }

  val vfBusyTable = schdType match {
    case VfScheduler() | MemScheduler() => Some(Module(new BusyTable(dispatch2Iq.numVfStateRead, wrapper.numVfStateWrite, VfPhyRegs, VfWB())))
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
      bt.io.wakeUp := io.fromSchedulers.wakeupVec
      bt.io.cancel := io.fromDataPath.cancelToBusyTable
      bt.io.ldCancel := io.ldCancel
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
      bt.io.wakeUp := io.fromSchedulers.wakeupVec
      bt.io.cancel := io.fromDataPath.cancelToBusyTable
      bt.io.ldCancel := io.ldCancel
    case None =>
  }

  val wakeupFromIntWBVec = Wire(params.genIntWBWakeUpSinkValidBundle)
  val wakeupFromVfWBVec = Wire(params.genVfWBWakeUpSinkValidBundle)

  wakeupFromIntWBVec.zip(io.intWriteBack).foreach { case (sink, source) =>
    sink.valid := source.wen
    sink.bits.rfWen := source.intWen
    sink.bits.fpWen := source.fpWen
    sink.bits.vecWen := source.vecWen
    sink.bits.pdest := source.addr
  }

  wakeupFromVfWBVec.zip(io.vfWriteBack).foreach { case (sink, source) =>
    sink.valid := source.wen
    sink.bits.rfWen := source.intWen
    sink.bits.fpWen := source.fpWen
    sink.bits.vecWen := source.vecWen
    sink.bits.pdest := source.addr
  }

  // Connect bundles having the same wakeup source
  issueQueues.zipWithIndex.foreach { case(iq, i) =>
    iq.io.wakeupFromIQ.foreach { wakeUp =>
      val wakeUpIn = iqWakeUpInMap(wakeUp.bits.exuIdx)
      val exuIdx = wakeUp.bits.exuIdx
      println(s"[Backend] Connect wakeup exuIdx ${exuIdx}")
      connectSamePort(wakeUp,wakeUpIn)
      backendParams.connectWakeup(exuIdx)
      if (backendParams.isCopyPdest(exuIdx)) {
        println(s"[Backend] exuIdx ${exuIdx} use pdestCopy ${backendParams.getCopyPdestIndex(exuIdx)}")
        wakeUp.bits.pdest := wakeUpIn.bits.pdestCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.rfWenCopy.nonEmpty) wakeUp.bits.rfWen := wakeUpIn.bits.rfWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.fpWenCopy.nonEmpty) wakeUp.bits.fpWen := wakeUpIn.bits.fpWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.vecWenCopy.nonEmpty) wakeUp.bits.vecWen := wakeUpIn.bits.vecWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.loadDependencyCopy.nonEmpty) wakeUp.bits.loadDependency := wakeUpIn.bits.loadDependencyCopy.get(backendParams.getCopyPdestIndex(exuIdx))
      }
      if (iq.params.numIntSrc == 0) wakeUp.bits.rfWen := false.B
      if (iq.params.numFpSrc == 0)  wakeUp.bits.fpWen := false.B
      if (iq.params.numVfSrc == 0)  wakeUp.bits.vecWen := false.B
    }
    iq.io.og0Cancel := io.fromDataPath.og0Cancel
    iq.io.og1Cancel := io.fromDataPath.og1Cancel
    iq.io.ldCancel := io.ldCancel
  }

  private val iqWakeUpOutMap: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    issueQueues.flatMap(_.io.wakeupToIQ)
      .map(x => (x.bits.exuIdx, x))
      .toMap

  // Connect bundles having the same wakeup source
  io.toSchedulers.wakeupVec.foreach { wakeUp =>
    wakeUp := iqWakeUpOutMap(wakeUp.bits.exuIdx)
  }

  io.toDataPathAfterDelay.zipWithIndex.foreach { case (toDpDy, i) =>
    toDpDy <> issueQueues(i).io.deqDelay
  }

  // Response
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp := io.fromDataPath(i)(j).og0resp
    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp := io.fromDataPath(i)(j).og1resp
    }
    iq.io.finalIssueResp.foreach(_.zipWithIndex.foreach { case (finalIssueResp, j) =>
      if (io.loadFinalIssueResp(i).isDefinedAt(j)) {
        finalIssueResp := io.loadFinalIssueResp(i)(j)
      } else {
        finalIssueResp := 0.U.asTypeOf(finalIssueResp)
      }
    })
    iq.io.memAddrIssueResp.foreach(_.zipWithIndex.foreach { case (memAddrIssueResp, j) =>
      if (io.memAddrIssueResp(i).isDefinedAt(j)) {
        memAddrIssueResp := io.memAddrIssueResp(i)(j)
      } else {
        memAddrIssueResp := 0.U.asTypeOf(memAddrIssueResp)
      }
    })
    iq.io.wbBusyTableRead := io.fromWbFuBusyTable.fuBusyTableRead(i)
    io.wbFuBusyTable(i) := iq.io.wbBusyTableWrite
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
    val intWBIQ = params.schdType match {
      case IntScheduler() => wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1)
      case VfScheduler() => wakeupFromVfWBVec
      case _ => null
    }
    iq.io.wakeupFromWB.zip(intWBIQ).foreach{ case (sink, source) => sink := source}
  }
}

// FIXME: Vector mem instructions may not be handled properly!
class SchedulerMemImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
{
  println(s"[SchedulerMemImp] " +
    s"has intBusyTable: ${intBusyTable.nonEmpty}, " +
    s"has vfBusyTable: ${vfBusyTable.nonEmpty}")

  val memAddrIQs = issueQueues.filter(_.params.isMemAddrIQ)
  val stAddrIQs = issueQueues.filter(iq => iq.params.StaCnt > 0) // included in memAddrIQs
  val ldAddrIQs = issueQueues.filter(iq => iq.params.LduCnt > 0)
  val stDataIQs = issueQueues.filter(iq => iq.params.StdCnt > 0)
  val vecMemIQs = issueQueues.filter(_.params.isVecMemIQ)
  val (hyuIQs, hyuIQIdxs) = issueQueues.zipWithIndex.filter(_._1.params.HyuCnt > 0).unzip

  println(s"[SchedulerMemImp] memAddrIQs.size: ${memAddrIQs.size}, enq.size: ${memAddrIQs.map(_.io.enq.size).sum}")
  println(s"[SchedulerMemImp] stAddrIQs.size:  ${stAddrIQs.size }, enq.size: ${stAddrIQs.map(_.io.enq.size).sum}")
  println(s"[SchedulerMemImp] ldAddrIQs.size:  ${ldAddrIQs.size }, enq.size: ${ldAddrIQs.map(_.io.enq.size).sum}")
  println(s"[SchedulerMemImp] stDataIQs.size:  ${stDataIQs.size }, enq.size: ${stDataIQs.map(_.io.enq.size).sum}")
  println(s"[SchedulerMemImp] hyuIQs.size:     ${hyuIQs.size    }, enq.size: ${hyuIQs.map(_.io.enq.size).sum}")
  require(memAddrIQs.nonEmpty && stDataIQs.nonEmpty)

  io.toMem.get.loadFastMatch := 0.U.asTypeOf(io.toMem.get.loadFastMatch) // TODO: is still needed?

  private val loadWakeUp = issueQueues.filter(_.params.LdExuCnt > 0).map(_.asInstanceOf[IssueQueueMemAddrImp].io.memIO.get.loadWakeUp).flatten
  require(loadWakeUp.length == io.fromMem.get.wakeup.length)
  loadWakeUp.zip(io.fromMem.get.wakeup).foreach(x => x._1 := x._2)

  memAddrIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.enq <> dispatch2Iq.io.out(i)
    iq.io.wakeupFromWB.zip(wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1) ++ wakeupFromVfWBVec).foreach{ case (sink, source) => sink := source}
  }

  ldAddrIQs.zipWithIndex.foreach {
    case (imp: IssueQueueMemAddrImp, i) =>
      imp.io.memIO.get.feedbackIO.head := 0.U.asTypeOf(imp.io.memIO.get.feedbackIO.head)
      imp.io.memIO.get.checkWait.stIssuePtr := io.fromMem.get.stIssuePtr
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
    case _ =>
  }

  stAddrIQs.zipWithIndex.foreach {
    case (imp: IssueQueueMemAddrImp, i) =>
      imp.io.memIO.get.feedbackIO.head := io.fromMem.get.staFeedback(i)
      imp.io.memIO.get.checkWait.stIssuePtr := io.fromMem.get.stIssuePtr
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
    case _ =>
  }

  hyuIQs.zip(hyuIQIdxs).foreach {
    case (imp: IssueQueueMemAddrImp, idx) =>
      imp.io.memIO.get.feedbackIO.head := io.fromMem.get.hyuFeedback.head
      imp.io.memIO.get.feedbackIO(1) := 0.U.asTypeOf(imp.io.memIO.get.feedbackIO(1))
      imp.io.memIO.get.checkWait.stIssuePtr := io.fromMem.get.stIssuePtr
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
      // TODO: refactor ditry code
      imp.io.deqDelay(1).ready := false.B
      io.toDataPathAfterDelay(idx)(1).valid := false.B
      io.toDataPathAfterDelay(idx)(1).bits := 0.U.asTypeOf(io.toDataPathAfterDelay(idx)(1).bits)
    case _ =>
  }

  private val staIdxSeq = (stAddrIQs).map(iq => iq.params.idxInSchBlk)
  private val hyaIdxSeq = (hyuIQs).map(iq => iq.params.idxInSchBlk)

  println(s"[SchedulerMemImp] sta iq idx in memSchdBlock: $staIdxSeq")
  println(s"[SchedulerMemImp] hya iq idx in memSchdBlock: $hyaIdxSeq")

  private val staEnqs = stAddrIQs.map(_.io.enq).flatten
  private val stdEnqs = stDataIQs.map(_.io.enq).flatten.take(staEnqs.size)
  private val hyaEnqs = hyuIQs.map(_.io.enq).flatten
  private val hydEnqs = stDataIQs.map(_.io.enq).flatten.drop(staEnqs.size)

  require(staEnqs.size == stdEnqs.size, s"number of enq ports of store address IQs(${staEnqs.size}) " +
  s"should be equal to number of enq ports of store data IQs(${stdEnqs.size})")

  require(hyaEnqs.size == hydEnqs.size, s"number of enq ports of hybrid address IQs(${hyaEnqs.size}) " +
  s"should be equal to number of enq ports of hybrid data IQs(${hydEnqs.size})")

  for ((idxInSchBlk, i) <- staIdxSeq.zipWithIndex) {
    dispatch2Iq.io.out(idxInSchBlk).zip(staEnqs).zip(stdEnqs).foreach{ case((dp, staIQ), stdIQ) =>
      val isAllReady = staIQ.ready && stdIQ.ready
      dp.ready := isAllReady
      staIQ.valid := dp.valid && isAllReady
      stdIQ.valid := dp.valid && isAllReady && FuType.isStore(dp.bits.fuType)
    }
  }

  for ((idxInSchBlk, i) <- hyaIdxSeq.zipWithIndex) {
    dispatch2Iq.io.out(idxInSchBlk).zip(hyaEnqs).zip(hydEnqs).foreach{ case((dp, hyaIQ), hydIQ) =>
      val isAllReady = hyaIQ.ready && hydIQ.ready
      dp.ready := isAllReady
      hyaIQ.valid := dp.valid && isAllReady
      hydIQ.valid := dp.valid && isAllReady && FuType.FuTypeOrR(dp.bits.fuType, FuType.stu, FuType.mou)
    }
  }

  stDataIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.wakeupFromWB.zip(wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++ wakeupFromVfWBVec).foreach{ case (sink, source) => sink := source}
  }

  (stdEnqs ++ hydEnqs).zip(staEnqs ++ hyaEnqs).zipWithIndex.foreach { case ((stdIQEnq, staIQEnq), i) =>
    stdIQEnq.bits  := staIQEnq.bits
    // Store data reuses store addr src(1) in dispatch2iq
    // [dispatch2iq] --src*------src*(0)--> [staIQ|hyaIQ]
    //                       \
    //                        ---src*(1)--> [stdIQ]
    // Since the src(1) of sta is easier to get, stdIQEnq.bits.src*(0) is assigned to staIQEnq.bits.src*(1)
    // instead of dispatch2Iq.io.out(x).bits.src*(1)
    val stdIdx = 1
    stdIQEnq.bits.srcState(0) := staIQEnq.bits.srcState(stdIdx)
    stdIQEnq.bits.srcLoadDependency(0) := staIQEnq.bits.srcLoadDependency(1)
      stdIQEnq.bits.srcType(0) := staIQEnq.bits.srcType(stdIdx)
    stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(stdIdx)
    stdIQEnq.bits.sqIdx := staIQEnq.bits.sqIdx
  }

  vecMemIQs.foreach {
    case imp: IssueQueueVecMemImp =>
      imp.io.memIO.get.sqDeqPtr.foreach(_ := io.fromMem.get.sqDeqPtr)
      imp.io.memIO.get.lqDeqPtr.foreach(_ := io.fromMem.get.lqDeqPtr)
      // not used
      imp.io.memIO.get.feedbackIO := 0.U.asTypeOf(imp.io.memIO.get.feedbackIO)
      // maybe not used
      imp.io.memIO.get.checkWait.stIssuePtr := io.fromMem.get.stIssuePtr
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
      imp.io.wakeupFromWB.zip(wakeupFromIntWBVec.zipWithIndex.filter(x => imp.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++ wakeupFromVfWBVec).foreach{ case (sink, source) => sink := source}

    case _ =>
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
