package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasPerfEvents
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.backend.datapath.WbConfig.V0WB
import xiangshan.backend.regfile.VlPregParams
import xiangshan.backend.regcache.RegCacheTagTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, SqPtr, LqPtr}
import xiangshan.mem.Bundles.MemWaitUpdateReqBundle

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class FpScheduler() extends SchedulerType
case class MemScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType

class Scheduler(val params: SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val numIntStateWrite = backendParams.numPregWb(IntData())
  val numFpStateWrite = backendParams.numPregWb(FpData())
  val numVfStateWrite = backendParams.numPregWb(VecData())
  val numV0StateWrite = backendParams.numPregWb(V0Data())
  val numVlStateWrite = backendParams.numPregWb(VlData())

  val issueQueue = params.issueBlockParams.map(x => LazyModule(new IssueQueue(x).suggestName(x.getIQName)))

  lazy val module: SchedulerImpBase = params.schdType match {
    case IntScheduler() => new SchedulerArithImp(this)(params, p)
    case FpScheduler()  => new SchedulerArithImp(this)(params, p)
    case MemScheduler() => new SchedulerMemImp(this)(params, p)
    case VfScheduler() => new SchedulerArithImp(this)(params, p)
    case _ => null
  }
}

class SchedulerIO()(implicit params: SchdBlockParams, p: Parameters) extends XSBundle {
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize
  val fromDispatchUopNum = params.issueBlockParams.filter(x => x.StdCnt == 0).map(_.numEnq).sum
  val allIssueParams = params.issueBlockParams.filter(_.StdCnt == 0)
  val IssueQueueDeqSum = allIssueParams.map(_.numDeq).sum
  val maxIQSize = allIssueParams.map(_.numEntries).max
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val fromWbFuBusyTable = new Bundle{
    val fuBusyTableRead = MixedVec(params.issueBlockParams.map(x => Input(x.genWbFuBusyTableReadBundle)))
  }
  val wbFuBusyTable = MixedVec(params.issueBlockParams.map(x => Output(x.genWbFuBusyTableWriteBundle)))
  val IQValidNumVec = Output(Vec(IssueQueueDeqSum, UInt((maxIQSize).U.getWidth.W)))

  val fromCtrlBlock = new Bundle {
    val flush = Flipped(ValidIO(new Redirect))
  }
  val fromDispatch = new Bundle {
    val uops =  Vec(fromDispatchUopNum, Flipped(DecoupledIO(new DynInst)))
  }
  val intWriteBack = MixedVec(Vec(backendParams.numPregWb(IntData()),
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))
  val fpWriteBack = MixedVec(Vec(backendParams.numPregWb(FpData()),
    new RfWritePortWithConfig(backendParams.fpPregParams.dataCfg, backendParams.fpPregParams.addrWidth)))
  val vfWriteBack = MixedVec(Vec(backendParams.numPregWb(VecData()),
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))
  val v0WriteBack = MixedVec(Vec(backendParams.numPregWb(V0Data()),
    new RfWritePortWithConfig(backendParams.v0PregParams.dataCfg, backendParams.v0PregParams.addrWidth)))
  val vlWriteBack = MixedVec(Vec(backendParams.numPregWb(VlData()),
    new RfWritePortWithConfig(backendParams.vlPregParams.dataCfg, backendParams.vlPregParams.addrWidth)))
  val intWriteBackDelayed = MixedVec(Vec(backendParams.numPregWb(IntData()),
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))
  val fpWriteBackDelayed = MixedVec(Vec(backendParams.numPregWb(FpData()),
    new RfWritePortWithConfig(backendParams.fpPregParams.dataCfg, backendParams.fpPregParams.addrWidth)))
  val vfWriteBackDelayed = MixedVec(Vec(backendParams.numPregWb(VecData()),
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))
  val v0WriteBackDelayed = MixedVec(Vec(backendParams.numPregWb(V0Data()),
    new RfWritePortWithConfig(backendParams.v0PregParams.dataCfg, backendParams.v0PregParams.addrWidth)))
  val vlWriteBackDelayed = MixedVec(Vec(backendParams.numPregWb(VlData()),
    new RfWritePortWithConfig(backendParams.vlPregParams.dataCfg, backendParams.vlPregParams.addrWidth)))
  val toDataPathAfterDelay: MixedVec[MixedVec[DecoupledIO[IssueQueueIssueBundle]]] = MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle))

  val vlWriteBackInfo = new Bundle {
    val vlFromIntIsZero  = Input(Bool())
    val vlFromIntIsVlmax = Input(Bool())
    val vlFromVfIsZero   = Input(Bool())
    val vlFromVfIsVlmax  = Input(Bool())
  }

  val fromSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpInValidBundle)
    val wakeupVecDelayed: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpInValidBundle)
  }

  val toSchedulers = new Bundle {
    val wakeupVec: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpOutValidBundle
  }

  val fromDataPath = new Bundle {
    val resp: MixedVec[MixedVec[OGRespBundle]] = MixedVec(params.issueBlockParams.map(x => Flipped(x.genOGRespBundle)))
    val og0Cancel = Input(ExuVec())
    // Todo: remove this after no cancel signal from og1
    val og1Cancel = Input(ExuVec())
    // replace RCIdx to Wakeup Queue
    val replaceRCIdx = OptionWrapper(params.needWriteRegCache, Vec(params.numWriteRegCache, Input(UInt(RegCacheIdxWidth.W))))
    // just be compatible to old code
    def apply(i: Int)(j: Int) = resp(i)(j)
  }

  val loadFinalIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.LdExuCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))
  val vecLoadFinalIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.VlduCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))
  val memAddrIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.LdExuCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))
  val vecLoadIssueResp = MixedVec(params.issueBlockParams.map(x => MixedVec(Vec(x.VlduCnt, Flipped(ValidIO(new IssueQueueDeqRespBundle()(p, x)))))))

  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))

  val fromMem = if (params.isMemSchd) Some(new Bundle {
    val ldaFeedback = Flipped(Vec(params.LduCnt, new MemRSFeedbackIO))
    val staFeedback = Flipped(Vec(params.StaCnt, new MemRSFeedbackIO))
    val hyuFeedback = Flipped(Vec(params.HyuCnt, new MemRSFeedbackIO))
    val vstuFeedback = Flipped(Vec(params.VstuCnt, new MemRSFeedbackIO(isVector = true)))
    val vlduFeedback = Flipped(Vec(params.VlduCnt, new MemRSFeedbackIO(isVector = true)))
    val stIssuePtr = Input(new SqPtr())
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
    val wakeup = Vec(params.LdExuCnt, Flipped(Valid(new DynInst)))
    val lqDeqPtr = Input(new LqPtr)
    val sqDeqPtr = Input(new SqPtr)
    // from lsq
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReqBundle)
  }) else None
  val toMem = if (params.isMemSchd) Some(new Bundle {
    val loadFastMatch = Output(Vec(params.LduCnt, new IssueQueueLoadBundle))
  }) else None
  val fromOg2Resp = if(params.needOg2Resp) Some(MixedVec(params.issueBlockParams.filter(_.needOg2Resp).map(x => Flipped(x.genOG2RespBundle)))) else None
}

abstract class SchedulerImpBase(wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends LazyModuleImp(wrapper)
    with HasXSParameter
{
  val io = IO(new SchedulerIO())

  // alias
  private val iqWakeUpInMap: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    io.fromSchedulers.wakeupVec.map(x => (x.bits.exuIdx, x)).toMap
  private val iqWakeUpInMapDelayed: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    io.fromSchedulers.wakeupVecDelayed.map(x => (x.bits.exuIdx, x)).toMap
  private val schdType = params.schdType

  // Modules
  val issueQueues: Seq[IssueQueueImp] = wrapper.issueQueue.map(_.module)

  io.IQValidNumVec := issueQueues.filter(_.params.StdCnt == 0).map(_.io.validCntDeqVec).flatten
  val wakeupFromIntWBVec = Wire(params.genIntWBWakeUpSinkValidBundle)
  val wakeupFromFpWBVec = Wire(params.genFpWBWakeUpSinkValidBundle)
  val wakeupFromVfWBVec = Wire(params.genVfWBWakeUpSinkValidBundle)
  val wakeupFromV0WBVec = Wire(params.genV0WBWakeUpSinkValidBundle)
  val wakeupFromVlWBVec = Wire(params.genVlWBWakeUpSinkValidBundle)
  val wakeupFromIntWBVecDelayed = Wire(params.genIntWBWakeUpSinkValidBundle)
  val wakeupFromFpWBVecDelayed = Wire(params.genFpWBWakeUpSinkValidBundle)
  val wakeupFromVfWBVecDelayed = Wire(params.genVfWBWakeUpSinkValidBundle)
  val wakeupFromV0WBVecDelayed = Wire(params.genV0WBWakeUpSinkValidBundle)
  val wakeupFromVlWBVecDelayed = Wire(params.genVlWBWakeUpSinkValidBundle)

  val wakeupFromWBVec = Seq(wakeupFromIntWBVec, wakeupFromFpWBVec, wakeupFromVfWBVec, wakeupFromV0WBVec, wakeupFromVlWBVec)
  val allWriteBack = Seq(io.intWriteBack, io.fpWriteBack, io.vfWriteBack, io.v0WriteBack, io.vlWriteBack)
  wakeupFromWBVec.zip(allWriteBack).map{ case (sinks, sources) =>
    sinks.zip(sources).map{ case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
  }

  val wakeupFromWBVecDelayed = Seq(wakeupFromIntWBVecDelayed, wakeupFromFpWBVecDelayed, wakeupFromVfWBVecDelayed, wakeupFromV0WBVecDelayed, wakeupFromVlWBVecDelayed)
  val allWriteBackDelayed = Seq(io.intWriteBackDelayed, io.fpWriteBackDelayed, io.vfWriteBackDelayed, io.v0WriteBackDelayed, io.vlWriteBackDelayed)
  wakeupFromWBVecDelayed.zip(allWriteBackDelayed).map { case (sinks, sources) =>
    sinks.zip(sources).map { case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
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
        if (wakeUpIn.bits.v0WenCopy.nonEmpty) wakeUp.bits.v0Wen := wakeUpIn.bits.v0WenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.vlWenCopy.nonEmpty) wakeUp.bits.vlWen := wakeUpIn.bits.vlWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.loadDependencyCopy.nonEmpty) wakeUp.bits.loadDependency := wakeUpIn.bits.loadDependencyCopy.get(backendParams.getCopyPdestIndex(exuIdx))
      }
      if (iq.params.numIntSrc == 0) wakeUp.bits.rfWen := false.B
      if (iq.params.numFpSrc == 0)  wakeUp.bits.fpWen := false.B
      if (iq.params.numVfSrc == 0)  wakeUp.bits.vecWen := false.B
      if (iq.params.numV0Src == 0)  wakeUp.bits.v0Wen := false.B
      if (iq.params.numVlSrc == 0)  wakeUp.bits.vlWen := false.B
    }
    iq.io.wakeupFromIQDelayed.foreach { wakeUp =>
      val wakeUpIn = iqWakeUpInMapDelayed(wakeUp.bits.exuIdx)
      connectSamePort(wakeUp, wakeUpIn)
      if (iq.params.numIntSrc == 0) wakeUp.bits.rfWen := false.B
      if (iq.params.numFpSrc == 0) wakeUp.bits.fpWen := false.B
      if (iq.params.numVfSrc == 0) wakeUp.bits.vecWen := false.B
      if (iq.params.numV0Src == 0) wakeUp.bits.v0Wen := false.B
      if (iq.params.numVlSrc == 0) wakeUp.bits.vlWen := false.B
    }
    iq.io.og0Cancel := io.fromDataPath.og0Cancel
    iq.io.og1Cancel := io.fromDataPath.og1Cancel
    if (iq.params.needLoadDependency)
      iq.io.ldCancel := io.ldCancel
    else
      iq.io.ldCancel := 0.U.asTypeOf(io.ldCancel)
  }

  // connect the vl writeback informatino to the issue queues
  issueQueues.zipWithIndex.foreach { case(iq, i) =>
    iq.io.vlFromIntIsVlmax := io.vlWriteBackInfo.vlFromIntIsVlmax
    iq.io.vlFromIntIsZero := io.vlWriteBackInfo.vlFromIntIsZero
    iq.io.vlFromVfIsVlmax := io.vlWriteBackInfo.vlFromVfIsVlmax
    iq.io.vlFromVfIsZero := io.vlWriteBackInfo.vlFromVfIsZero
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
      if (io.loadFinalIssueResp(i).isDefinedAt(j) && iq.params.isLdAddrIQ) {
        finalIssueResp := io.loadFinalIssueResp(i)(j)
      } else if (io.vecLoadFinalIssueResp(i).isDefinedAt(j) && iq.params.isVecLduIQ) {
        finalIssueResp := io.vecLoadFinalIssueResp(i)(j)
      }
      else {
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
    iq.io.vecLoadIssueResp.foreach(_.zipWithIndex.foreach { case (resp, deqIdx) =>
      resp := io.vecLoadIssueResp(i)(deqIdx)
    })
    iq.io.wbBusyTableRead := io.fromWbFuBusyTable.fuBusyTableRead(i)
    io.wbFuBusyTable(i) := iq.io.wbBusyTableWrite
    iq.io.replaceRCIdx.foreach(x => x := 0.U.asTypeOf(x))
  }
  if (params.needOg2Resp) {
    issueQueues.filter(_.params.needOg2Resp).zip(io.fromOg2Resp.get).foreach{ case (iq, og2RespVec) =>
      iq.io.og2Resp.get.zip(og2RespVec).foreach{ case (iqOg2Resp, og2Resp) =>
        iqOg2Resp := og2Resp
      }
    }
  }

  // Connect each replace RCIdx to IQ
  if (params.needWriteRegCache) {
    val iqReplaceRCIdxVec = issueQueues.filter(_.params.needWriteRegCache).flatMap{ iq =>
      iq.params.allExuParams.zip(iq.io.replaceRCIdx.get).filter(_._1.needWriteRegCache).map(_._2)
    }
    iqReplaceRCIdxVec.zip(io.fromDataPath.replaceRCIdx.get).foreach{ case (iq, in) =>
      iq := in
    }

    println(s"[Scheduler] numWriteRegCache: ${params.numWriteRegCache}")
    println(s"[Scheduler] iqReplaceRCIdxVec: ${iqReplaceRCIdxVec.size}")
  }

  // perfEvent
  val lastCycleIqEnqFireVec    = RegNext(VecInit(issueQueues.map(_.io.enq.map(_.fire)).flatten))
  val lastCycleIqFullVec       = RegNext(VecInit(issueQueues.map(_.io.enq.head.ready)))

  val issueQueueFullVecPerf = issueQueues.zip(lastCycleIqFullVec)map{ case (iq, full) => (iq.params.getIQName + s"_full", full) }
  val basePerfEvents = Seq(
    ("issueQueue_enq_fire_cnt",  PopCount(lastCycleIqEnqFireVec)                    )
  )  ++ issueQueueFullVecPerf

  println(s"[Scheduler] io.fromSchedulers.wakeupVec: ${io.fromSchedulers.wakeupVec.map(x => backendParams.getExuName(x.bits.exuIdx))}")
  println(s"[Scheduler] iqWakeUpInKeys: ${iqWakeUpInMap.keys}")

  println(s"[Scheduler] iqWakeUpOutKeys: ${iqWakeUpOutMap.keys}")
  println(s"[Scheduler] io.toSchedulers.wakeupVec: ${io.toSchedulers.wakeupVec.map(x => backendParams.getExuName(x.bits.exuIdx))}")
}

class SchedulerArithImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
    with HasPerfEvents
{
  val issueQueuesUopIn = issueQueues.map(_.io.enq).flatten
  issueQueuesUopIn.zip(io.fromDispatch.uops).map(x => x._1 <> x._2)
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    if (!iq.params.needLoadDependency) {
      iq.io.enq.map(x => x.bits.srcLoadDependency := 0.U.asTypeOf(x.bits.srcLoadDependency))
    }
    val intWBIQ = params.schdType match {
      case IntScheduler() => wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1)
      case FpScheduler() => wakeupFromFpWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1)
      case VfScheduler() => (wakeupFromVfWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
                             wakeupFromV0WBVec.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1) ++
                             wakeupFromVlWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1))
      case _ => null
    }
    val intWBIQDelayed = params.schdType match {
      case IntScheduler() => wakeupFromIntWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1)
      case FpScheduler() => wakeupFromFpWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1)
      case VfScheduler() => (wakeupFromVfWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
                             wakeupFromV0WBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1) ++
                             wakeupFromVlWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1))
      case _ => null
    }
    iq.io.wakeupFromWB.zip(intWBIQ).foreach{ case (sink, source) => sink := source}
    iq.io.wakeupFromWBDelayed.zip(intWBIQDelayed).foreach{ case (sink, source) => sink := source}
  }

  val perfEvents = basePerfEvents
  generatePerfEvent()
}

// FIXME: Vector mem instructions may not be handled properly!
class SchedulerMemImp(override val wrapper: Scheduler)(implicit params: SchdBlockParams, p: Parameters)
  extends SchedulerImpBase(wrapper)
    with HasXSParameter
    with HasPerfEvents
{

  val issueQueuesUopIn = issueQueues.filter(_.params.StdCnt == 0).map(_.io.enq).flatten
  issueQueuesUopIn.zip(io.fromDispatch.uops).map(x => x._1 <> x._2)
  val noStdExuParams = params.issueBlockParams.map(x => Seq.fill(x.numEnq)(x.exuBlockParams)).flatten.filter{x => x.map(!_.hasStdFu).reduce(_ && _)}
  val staIdx = noStdExuParams.zipWithIndex.filter{x => x._1.map(_.hasStoreAddrFu).reduce(_ || _)}.map(_._2)
  val staReady = issueQueues.filter(iq => iq.params.StaCnt > 0).map(_.io.enq.map(_.ready)).flatten
  val stdReady = issueQueues.filter(iq => iq.params.StdCnt > 0).map(_.io.enq.map(_.ready)).flatten
  staIdx.zipWithIndex.map{ case (sta, i) => {
    io.fromDispatch.uops(sta).ready := staReady(i) && stdReady(i)
  }}
  issueQueues.filter(iq => iq.params.StaCnt > 0).map(_.io.enq).flatten.zipWithIndex.map{ case (iq, idx) =>
    iq.valid := io.fromDispatch.uops(staIdx(idx)).valid && !io.fromDispatch.uops(staIdx(idx)).bits.isDropAmocasSta
  }
  val staValidFromDispatch = staIdx.map(idx => io.fromDispatch.uops(idx).valid)
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
    if (!iq.params.needLoadDependency) {
      iq.io.enq.map(x => x.bits.srcLoadDependency := 0.U.asTypeOf(x.bits.srcLoadDependency))
    }
    iq.io.wakeupFromWB.zip(
      wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromFpWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromVfWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromV0WBVec.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromVlWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1)
    ).foreach{ case (sink, source) => sink := source}
    iq.io.wakeupFromWBDelayed.zip(
      wakeupFromIntWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromFpWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromVfWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromV0WBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1) ++
      wakeupFromVlWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1)
    ).foreach { case (sink, source) => sink := source }
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

  stDataIQs.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush <> io.fromCtrlBlock.flush
    iq.io.wakeupFromWB.zip(
      wakeupFromIntWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromFpWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromVfWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromV0WBVec.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromVlWBVec.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq
    ).foreach{ case (sink, source) => sink := source}
    iq.io.wakeupFromWBDelayed.zip(
      wakeupFromIntWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromFpWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromVfWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromV0WBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
      wakeupFromVlWBVecDelayed.zipWithIndex.filter(x => iq.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq
    ).foreach { case (sink, source) => sink := source }
    // here disable fp load fast wakeup to std, and no FEX wakeup to std
    iq.io.wakeupFromIQ.map(_.bits.fpWen := false.B)
  }

  (stdEnqs ++ hydEnqs).zip(staEnqs ++ hyaEnqs).zipWithIndex.foreach { case ((stdIQEnq, staIQEnq), i) =>
    stdIQEnq.valid := staValidFromDispatch(i)
    stdIQEnq.bits  := staIQEnq.bits
    // Store data reuses store addr src(1) in dispatch2iq
    // [dispatch2iq] --src*------src*(0)--> [staIQ|hyaIQ]
    //                       \
    //                        ---src*(1)--> [stdIQ]
    // Since the src(1) of sta is easier to get, stdIQEnq.bits.src*(0) is assigned to staIQEnq.bits.src*(1)
    // instead of dispatch2Iq.io.out(x).bits.src*(1)
    val stdIdx = 1
    stdIQEnq.bits.srcState(0) := staIQEnq.bits.srcState(stdIdx)
    stdIQEnq.bits.srcLoadDependency(0) := staIQEnq.bits.srcLoadDependency(stdIdx)
    stdIQEnq.bits.srcType(0) := staIQEnq.bits.srcType(stdIdx)
    stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(stdIdx)
    stdIQEnq.bits.sqIdx := staIQEnq.bits.sqIdx
    stdIQEnq.bits.useRegCache(0) := staIQEnq.bits.useRegCache(stdIdx)
    stdIQEnq.bits.regCacheIdx(0) := staIQEnq.bits.regCacheIdx(stdIdx)
  }

  vecMemIQs.foreach {
    case imp: IssueQueueVecMemImp =>
      imp.io.memIO.get.sqDeqPtr.foreach(_ := io.fromMem.get.sqDeqPtr)
      imp.io.memIO.get.lqDeqPtr.foreach(_ := io.fromMem.get.lqDeqPtr)
      // not used
      //imp.io.memIO.get.feedbackIO.head := io.fromMem.get.vstuFeedback.head // only vector store replay
      // maybe not used
      imp.io.memIO.get.checkWait.stIssuePtr := io.fromMem.get.stIssuePtr
      imp.io.memIO.get.checkWait.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
      imp.io.wakeupFromWB.zip(
        wakeupFromIntWBVec.zipWithIndex.filter(x => imp.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromFpWBVec.zipWithIndex.filter(x => imp.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromVfWBVec.zipWithIndex.filter(x => imp.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromV0WBVec.zipWithIndex.filter(x => imp.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromVlWBVec.zipWithIndex.filter(x => imp.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq
      ).foreach{ case (sink, source) => sink := source}
      imp.io.wakeupFromWBDelayed.zip(
        wakeupFromIntWBVecDelayed.zipWithIndex.filter(x => imp.params.needWakeupFromIntWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromFpWBVecDelayed.zipWithIndex.filter(x => imp.params.needWakeupFromFpWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromVfWBVecDelayed.zipWithIndex.filter(x => imp.params.needWakeupFromVfWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromV0WBVecDelayed.zipWithIndex.filter(x => imp.params.needWakeupFromV0WBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq ++
        wakeupFromVlWBVecDelayed.zipWithIndex.filter(x => imp.params.needWakeupFromVlWBPort.keys.toSeq.contains(x._2)).map(_._1).toSeq
      ).foreach { case (sink, source) => sink := source }

    case _ =>
  }
  val vecMemFeedbackIO: Seq[MemRSFeedbackIO] = vecMemIQs.map {
    case imp: IssueQueueVecMemImp =>
      imp.io.memIO.get.feedbackIO
  }.flatten
  assert(vecMemFeedbackIO.size == io.fromMem.get.vstuFeedback.size, "vecMemFeedback size dont match!")
  vecMemFeedbackIO.zip(io.fromMem.get.vstuFeedback).foreach{
    case (sink, source) =>
      sink := source
  }

  val perfEvents = basePerfEvents
  generatePerfEvent()
}
