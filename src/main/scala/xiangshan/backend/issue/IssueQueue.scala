package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.FuConfig._
import xiangshan.mem.{LqPtr, SqPtr}
import utility.PerfCCT

class IssueQueueIO()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  // Inputs
  val flush = Flipped(ValidIO(new Redirect))
  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new RegionInUop(params))))

  val og0Resp = Vec(params.numDeq, Flipped(new IssueQueueRespBundle))
  val og1Resp = Vec(params.numDeq, Flipped(new IssueQueueRespBundle))
  val og2Resp = Option.when(params.needOg2Resp)(Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val s0Resp = Option.when(params.needS0Resp)(Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val s2Resp = Option.when(params.needS2Resp)(Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  // Vec Mem Resp, uncertain
  val snResp = Option.when(params.needSnResp)(Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val wbBusyTableRead = Input(params.genWbFuBusyTableReadBundle)
  val wbBusyTableWrite = Output(params.genWbFuBusyTableWriteBundle)
  val wakeupFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeupFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val wakeupFromExu: Option[MixedVec[DecoupledIO[IssueQueueIQWakeUpBundle]]] = Option.when(params.needUncertainWakeupFromExu)(Flipped(params.genExuWakeUpOutValidBundle))
  val wakeupFromI2F: Option[ValidIO[IssueQueueIQWakeUpBundle]] = Option.when(params.needWakeupFromI2F)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxI2F, params.backendParam))))
  val wakeupFromF2I: Option[ValidIO[IssueQueueIQWakeUpBundle]] = Option.when(params.needWakeupFromF2I)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxF2I, params.backendParam))))
  val wakeupFromWBDelayed: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeupFromIQDelayed: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val vlFromIntIsZero = Input(Bool())
  val vlFromIntIsVlmax = Input(Bool())
  val vlFromVfIsZero = Input(Bool())
  val vlFromVfIsVlmax = Input(Bool())
  val og0Cancel = Input(ExuVec())
  val og1Cancel = Input(ExuVec())
  val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, Flipped(new LoadCancelIO))
  val replaceRCIdx = Option.when(params.needWriteRegCache)(Vec(params.numDeq, Input(UInt(RegCacheIdxWidth.W))))
  // memAddrIQ
  val memIO = Option.when(params.isMemAddrIQ)(new IssueQueueMemBundle)

  // Outputs
  val wakeupToIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpSourceValidBundle
  val validCntDeqVec = Output(Vec(params.numDeq,UInt(params.numEntries.U.getWidth.W)))
  // perf counter
  val validVec = Output(Vec(params.numEntries, Bool()))
  val issuedVec = Output(Vec(params.numEntries, Bool()))
  val fuTypeVec = Output(Vec(params.numEntries, FuType()))
  val canIssueVec = Output(Vec(params.numEntries, Bool()))
  val srcReadyVec = Output(Vec(params.numEntries, Bool()))

  val deqDelay: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle// = deq.cloneType
  val deqOg1Payload: MixedVec[Og1Payload] = params.genIssueDeqOg1PayloadBundle
  def allWakeUp = wakeupFromWB ++ wakeupFromIQ
}

class IssueQueueImp(implicit p: Parameters, params: IssueBlockParams) extends XSModule with HasXSParameter {

  override def desiredName: String = s"${params.getIQName}"

  println(s"[IssueQueueImp] ${params.getIQName} wakeupFromWB(${io.wakeupFromWB.size}), " +
    s"wakeup exu in(${params.wakeUpInExuSources.size}): ${params.wakeUpInExuSources.map(_.name).mkString("{",",","}")}, " +
    s"wakeup exu out(${params.wakeUpOutExuSources.size}): ${params.wakeUpOutExuSources.map(_.name).mkString("{",",","}")}, " +
    s"numEntries: ${params.numEntries}, numRegSrc: ${params.numRegSrc}, " +
    s"numEnq: ${params.numEnq}, numSimp: ${params.numSimp}, numComp: ${params.numComp}, numDeq: ${params.numDeq}, " +
    s"isAllSimp: ${params.isAllSimp}, isAllComp: ${params.isAllComp}")

  require(params.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  require(params.numEnq <= 2, "IssueQueue has not supported more than 2 enq ports")
  require(params.numSimp == 0 || params.numSimp >= params.numEnq, "numSimp should be 0 or at least not less than numEnq")
  require(params.numComp == 0 || params.numComp >= params.numEnq, "numComp should be 0 or at least not less than numEnq")
  val param: IssueBlockParams = params
  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs)
  val wakeupFuLatencySeqs : Seq[Seq[(FuType.OHType, Int)]] = params.exuBlockParams.map(x => x.wakeUpFuLatencyMap.toSeq.sortBy(_._2))

  println(s"[IssueQueueImp] ${params.getIQName} fuLatencySeqs: ${wakeupFuLatencySeqs}")
  if (params.hasIQWakeUp) {
    val exuSourcesEncodeString = params.wakeUpSourceExuIdx.map(x => 1 << x).reduce(_ + _).toBinaryString
    println(s"[IssueQueueImp] ${params.getIQName} exuSourcesWidth: ${ExuSource().value.getWidth}, " +
      s"exuSourcesEncodeMask: ${"0" * (p(XSCoreParamsKey).backendParams.numExu - exuSourcesEncodeString.length) + exuSourcesEncodeString}")
  }

  lazy val io = IO(new IssueQueueIO())

  io.enq.foreach { case enq =>
    enq.bits.debug.foreach(x => PerfCCT.updateInstPos(x.debug_seqNum, PerfCCT.InstPos.AtIssueQue.id.U, enq.valid, clock, reset))
  }

  // Modules
  val entries = Module(new Entries)
  val fuBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.latencyValMax > 0)(Module(new FuBusyTableWrite(x.fuLatencyMap(param.aluDeqNeedPickJump)))) }
  val fuBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.latencyValMax > 0)(Module(new FuBusyTableRead(x.fuLatencyMap(param.aluDeqNeedPickJump)))) }
  val intWbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.intLatencyCertain)(Module(new FuBusyTableWrite(x.intFuLatencyMap))) }
  val intWbBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.intLatencyCertain)(Module(new FuBusyTableRead(x.intFuLatencyMap))) }
  val fpWbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.fpLatencyCertain)(Module(new FuBusyTableWrite(x.fpFuLatencyMap))) }
  val fpWbBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.fpLatencyCertain)(Module(new FuBusyTableRead(x.fpFuLatencyMap))) }
  val vfWbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.vfLatencyCertain)(Module(new FuBusyTableWrite(x.vfFuLatencyMap))) }
  val vfWbBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.vfLatencyCertain)(Module(new FuBusyTableRead(x.vfFuLatencyMap))) }
  val v0WbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.v0LatencyCertain)(Module(new FuBusyTableWrite(x.v0FuLatencyMap))) }
  val v0WbBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.v0LatencyCertain)(Module(new FuBusyTableRead(x.v0FuLatencyMap))) }
  val vlWbBusyTableWrite = params.exuBlockParams.map { case x => Option.when(x.vlLatencyCertain)(Module(new FuBusyTableWrite(x.vlFuLatencyMap))) }
  val vlWbBusyTableRead = params.exuBlockParams.map { case x => Option.when(x.vlLatencyCertain)(Module(new FuBusyTableRead(x.vlFuLatencyMap))) }

  class WakeupQueueFlush extends Bundle {
    val redirect = ValidIO(new Redirect)
    val ldCancel = Vec(backendParams.LduCnt + backendParams.HyuCnt, new LoadCancelIO)
    val og0Fail = Output(Bool())
    val og1Fail = Output(Bool())
  }

  private def flushFunc(exuInput: ExuInput, flush: WakeupQueueFlush, stage: Int): Bool = {
    val redirectFlush = exuInput.robIdx.needFlush(flush.redirect)
    val loadDependencyFlush = LoadShouldCancel(exuInput.loadDependency, flush.ldCancel)
    val ogFailFlush = stage match {
      case 1 => flush.og0Fail
      case 2 => flush.og1Fail
      case _ => false.B
    }
    redirectFlush || loadDependencyFlush || ogFailFlush
  }

  private def modificationFunc(exuInput: ExuInput): ExuInput = {
    val newExuInput = WireDefault(exuInput)
    newExuInput.loadDependency match {
      case Some(deps) => deps.zip(exuInput.loadDependency.get).foreach(x => x._1 := x._2 << 1)
      case None =>
    }
    newExuInput
  }

  private def lastConnectFunc(exuInput: ExuInput, newInput: ExuInput): ExuInput = {
    val lastExuInput = WireDefault(exuInput)
    val newExuInput = WireDefault(newInput)
    newExuInput.elements.foreach { case (name, data) =>
      if (lastExuInput.elements.contains(name)) {
        data := lastExuInput.elements(name)
      }
    }
    if (newExuInput.pdestCopy.nonEmpty && !lastExuInput.pdestCopy.nonEmpty) {
      newExuInput.pdestCopy.get.foreach(_ := lastExuInput.pdest)
    }
    if (newExuInput.rfWenCopy.nonEmpty && !lastExuInput.rfWenCopy.nonEmpty) {
      newExuInput.rfWenCopy.get.foreach(_ := lastExuInput.rfWen.get)
    }
    if (newExuInput.fpWenCopy.nonEmpty && !lastExuInput.fpWenCopy.nonEmpty) {
      newExuInput.fpWenCopy.get.foreach(_ := lastExuInput.fpWen.get)
    }
    if (newExuInput.vecWenCopy.nonEmpty && !lastExuInput.vecWenCopy.nonEmpty) {
      newExuInput.vecWenCopy.get.foreach(_ := lastExuInput.vecWen.get)
    }
    if (newExuInput.v0WenCopy.nonEmpty && !lastExuInput.v0WenCopy.nonEmpty) {
      newExuInput.v0WenCopy.get.foreach(_ := lastExuInput.v0Wen.get)
    }
    if (newExuInput.vlWenCopy.nonEmpty && !lastExuInput.vlWenCopy.nonEmpty) {
      newExuInput.vlWenCopy.get.foreach(_ := lastExuInput.vlWen.get)
    }
    if (newExuInput.loadDependencyCopy.nonEmpty && !lastExuInput.loadDependencyCopy.nonEmpty) {
      newExuInput.loadDependencyCopy.get.foreach(_ := lastExuInput.loadDependency.get)
    }
    newExuInput
  }

  val wakeUpQueues: Seq[Option[MultiWakeupQueue[ExuInput, WakeupQueueFlush]]] = params.exuBlockParams.map { x => Option.when(x.isIQWakeUpSource && !x.hasLoadExu)(Module(
    new MultiWakeupQueue(x, new ExuInput(x), new ExuInput(x, x.copyWakeupOut, x.copyNum), new WakeupQueueFlush, x.wakeUpFuLatancySet, flushFunc, modificationFunc, lastConnectFunc)
  ))}
  val deqBeforeDly = Wire(params.genIssueDecoupledBundle)

  val intWbBusyTableIn = io.wbBusyTableRead.map(_.intWbBusyTable)
  val fpWbBusyTableIn = io.wbBusyTableRead.map(_.fpWbBusyTable)
  val vfWbBusyTableIn = io.wbBusyTableRead.map(_.vfWbBusyTable)
  val v0WbBusyTableIn = io.wbBusyTableRead.map(_.v0WbBusyTable)
  val vlWbBusyTableIn = io.wbBusyTableRead.map(_.vlWbBusyTable)

  val intWbBusyTableOut = io.wbBusyTableWrite.map(_.intWbBusyTable)
  val fpWbBusyTableOut = io.wbBusyTableWrite.map(_.fpWbBusyTable)
  val vfWbBusyTableOut = io.wbBusyTableWrite.map(_.vfWbBusyTable)
  val v0WbBusyTableOut = io.wbBusyTableWrite.map(_.v0WbBusyTable)
  val vlWbBusyTableOut = io.wbBusyTableWrite.map(_.vlWbBusyTable)

  val intDeqRespSetOut = io.wbBusyTableWrite.map(_.intDeqRespSet)
  val fpDeqRespSetOut = io.wbBusyTableWrite.map(_.fpDeqRespSet)
  val vfDeqRespSetOut = io.wbBusyTableWrite.map(_.vfDeqRespSet)
  val v0DeqRespSetOut = io.wbBusyTableWrite.map(_.v0DeqRespSet)
  val vlDeqRespSetOut = io.wbBusyTableWrite.map(_.vlDeqRespSet)

  val fuBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val intWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val fpWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val vfWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val v0WbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val vlWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val s0_enqValidVec = io.enq.map(_.valid)
  val s0_enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val s0_enqNotFlush = !io.flush.valid
  val s0_enqBits = WireInit(VecInit(io.enq.map(_.bits)))
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush) //enqValid && notFlush && enqReady


  val finalDeqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  dontTouch(finalDeqSelValidVec)
  val finalDeqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val validVec = VecInit(entries.io.valid.asBools)
  val issuedVec = VecInit(entries.io.issued.asBools)
  val requestForTrans = VecInit(validVec.zip(issuedVec).map(x => x._1 && !x._2))
  val canIssueVec = VecInit(entries.io.canIssue.asBools)
  val rfWenVec = VecInit(entries.io.rfWen.asBools)
  val srcReadyVec = VecInit(entries.io.srcReady.asBools)
  io.validVec := validVec
  io.issuedVec := issuedVec
  io.canIssueVec := canIssueVec
  io.srcReadyVec := srcReadyVec
  dontTouch(canIssueVec)
  val deqFirstIssueVec = entries.io.isFirstIssue

  val dataSources: Vec[Vec[DataSource]] = entries.io.dataSources
  val finalDataSources: Vec[Vec[DataSource]] = VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, dataSources)))
  val loadDependency: Vec[Vec[UInt]] = entries.io.loadDependency
  val finalLoadDependency: IndexedSeq[Vec[UInt]] = VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, loadDependency)))
  // (entryIdx)(srcIdx)
  val exuSources: Option[Vec[Vec[ExuSource]]] = entries.io.exuSources
  // (deqIdx)(srcIdx)
  val finalExuSources: Option[Vec[Vec[ExuSource]]] = exuSources.map(x => VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, x))))

  val fuTypeVec = Wire(Vec(params.numEntries, FuType()))
  io.fuTypeVec := fuTypeVec
  val deqEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val canIssueMergeAllBusy = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val deqCanIssue = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  //deq
  val enqEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt(params.numEnq.W))))
  val simpEntryOldestSel = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq + params.numEnq, ValidIO(UInt(params.numSimp.W)))))
  val compEntryOldestSel = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq, ValidIO(UInt(params.numComp.W)))))
  val othersEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt((params.numEntries - params.numEnq).W))))
  val deqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val deqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val cancelDeqVec = Wire(Vec(params.numDeq, Bool()))

  val subDeqSelValidVec = Option.when(params.deqFuSame)(Wire(Vec(params.numDeq, Bool())))
  val subDeqSelOHVec = Option.when(params.deqFuSame)(Wire(Vec(params.numDeq, UInt(params.numEntries.W))))
  val subDeqRequest = Option.when(params.deqFuSame)(Wire(UInt(params.numEntries.W)))

  //trans
  val simpEntryEnqSelVec = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numEnq, UInt(params.numSimp.W))))
  val compEntryEnqSelVec = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numEnq, UInt(params.numComp.W))))
  val othersEntryEnqSelVec = Option.when(params.isAllComp || params.isAllSimp)(Wire(Vec(params.numEnq, UInt((params.numEntries - params.numEnq).W))))
  val simpAgeDetectRequest = Option.when(params.hasCompAndSimp)(Wire(Vec(params.numDeq + params.numEnq, UInt(params.numSimp.W))))
  simpAgeDetectRequest.foreach(_ := 0.U.asTypeOf(simpAgeDetectRequest.get))

  // when vf exu (with og2) wake up int/mem iq (without og2), the wakeup signals should delay 1 cycle
  // as vf exu's min latency is 1, we do not need consider og0cancel
  val wakeupFromIQ = Wire(chiselTypeOf(io.wakeupFromIQ))
  wakeupFromIQ.zip(io.wakeupFromIQ).foreach { case (w, w_src) =>
    if (!params.inVfSchd && params.readVfRf && params.hasWakeupFromVf && w_src.bits.params.isVfExeUnit) {
      val noCancel = !LoadShouldCancel(Some(w_src.bits.loadDependency), io.ldCancel)
      w := RegNext(Mux(noCancel, w_src, 0.U.asTypeOf(w)))
      w.bits.loadDependency.zip(w_src.bits.loadDependency).foreach{ case (ld, ld_src) => ld := RegNext(Mux(noCancel, ld_src << 1, 0.U.asTypeOf(ld))) }
    } else {
      w := w_src
    }
  }
  val wakeupFromIQDelayed = Wire(chiselTypeOf(io.wakeupFromIQDelayed))
  wakeupFromIQDelayed.zip(io.wakeupFromIQDelayed).foreach { case (w, w_src) =>
    if (!params.inVfSchd && params.readVfRf && params.hasWakeupFromVf && w_src.bits.params.isVfExeUnit) {
      val noCancel = !LoadShouldCancel(Some(w_src.bits.loadDependency), io.ldCancel)
      w := RegNext(Mux(noCancel, w_src, 0.U.asTypeOf(w)))
      w.bits.loadDependency.zip(w_src.bits.loadDependency).foreach { case (ld, ld_src) => ld := RegNext(Mux(noCancel, ld_src << 1, 0.U.asTypeOf(ld))) }
    } else {
      w := w_src
    }
  }

  /**
    * Connection of [[entries]]
    */
  entries.io match { case entriesIO: EntriesIO =>
    entriesIO.flush                                             := io.flush
    entriesIO.enq.zipWithIndex.foreach { case (enq, enqIdx) =>
      enq.valid                                                 := s0_doEnqSelValidVec(enqIdx)
      enq.bits.status.robIdx                                    := s0_enqBits(enqIdx).robIdx
      enq.bits.status.fuType                                    := IQFuType.readFuType(VecInit(s0_enqBits(enqIdx).fuType.asBools), params.getFuCfgs.map(_.fuType))
      val numLsrc = s0_enqBits(enqIdx).srcType.size.min(enq.bits.status.srcStatus.map(_.srcType).size)
      for(j <- 0 until numLsrc) {
        enq.bits.status.srcStatus(j).psrc                       := s0_enqBits(enqIdx).psrc(j)
        enq.bits.status.srcStatus(j).srcType                    := s0_enqBits(enqIdx).srcType(j)
        enq.bits.status.srcStatus(j).srcState                   := (if (j < 3) {
                                                                      Mux(SrcType.isVp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U),
                                                                          SrcState.rdy,
                                                                          s0_enqBits(enqIdx).srcState(j))
                                                                    } else {
                                                                      s0_enqBits(enqIdx).srcState(j)
                                                                    })
        enq.bits.status.srcStatus(j).dataSources.value          := (if (j < 3) {
                                                                      MuxCase(DataSource.reg, Seq(
                                                                        (SrcType.isXp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U)) -> DataSource.zero,
                                                                        SrcType.isNotReg(s0_enqBits(enqIdx).srcType(j))                                       -> DataSource.imm,
                                                                        (SrcType.isVp(s0_enqBits(enqIdx).srcType(j)) && (s0_enqBits(enqIdx).psrc(j) === 0.U)) -> DataSource.v0,
                                                                      ))
                                                                    } else {
                                                                      MuxCase(DataSource.reg, Seq(
                                                                        SrcType.isNotReg(s0_enqBits(enqIdx).srcType(j))  -> DataSource.imm,
                                                                      ))
                                                                    })
        enq.bits.status.srcStatus(j).srcLoadDependency          := VecInit(s0_enqBits(enqIdx).srcLoadDependency(j).map(x => x << 1))
        enq.bits.status.srcStatus(j).exuSources.foreach(_       := 0.U.asTypeOf(ExuSource()))
        enq.bits.status.srcStatus(j).useRegCache.foreach(_      := s0_enqBits(enqIdx).useRegCache(j))
        enq.bits.status.srcStatus(j).regCacheIdx.foreach(_      := s0_enqBits(enqIdx).regCacheIdx(j))
      }
      enq.bits.status.srcStatusVl.foreach {
        vlSrcStatus =>
          vlSrcStatus.srcState                                  := s0_enqBits(enqIdx).srcStateVl.get
          vlSrcStatus.psrc                                      := s0_enqBits(enqIdx).psrcVl.get
          vlSrcStatus.dataSource.value                          := DataSource.reg // Todo: update when support vl wake up
      }
      enq.bits.status.blocked                                   := false.B
      enq.bits.status.issued                                    := false.B
      enq.bits.status.firstIssue                                := false.B
      enq.bits.status.issueTimer                                := 0.U
      enq.bits.status.deqPortIdx                                := 0.U
      connectSamePort(enq.bits.payload, s0_enqBits(enqIdx))
      // for IssueQueueSta imm width is not 32
      enq.bits.payload.imm.foreach(_                            := s0_enqBits(enqIdx).imm.get)
      connectSamePort(enq.bits.payload.og1Payload, enq.bits.payload)
    }
    entriesIO.og0Resp                                           := io.og0Resp
    entriesIO.og1Resp                                           := io.og1Resp
    entriesIO.og2Resp.foreach(_                                 := io.og2Resp.get)
    entriesIO.s0Resp.foreach(_                                  := io.s0Resp.get)
    entriesIO.s2Resp.foreach(_                                  := io.s2Resp.get)
    entriesIO.snResp.foreach(_                                  := io.snResp.get)
    entriesIO.snResp.foreach(x => dontTouch(x.head.sqIdx.get))
    for(deqIdx <- 0 until params.numDeq) {
      entriesIO.deqReady(deqIdx)                                := deqBeforeDly(deqIdx).ready
      entriesIO.deqSelOH(deqIdx).valid                          := deqSelValidVec(deqIdx)
      if (params.aluDeqNeedPickJump && (deqIdx == 1)) {
        val needCancelDeq1 = deqEntryVec(0).valid && FuType.isJump(deqEntryVec(0).bits.payload.fuType)
        entriesIO.deqSelOH(deqIdx).valid                        := deqSelValidVec(deqIdx) && !needCancelDeq1
      }
      entriesIO.deqSelOH(deqIdx).bits                           := deqSelOHVec(deqIdx)
      entriesIO.enqEntryOldestSel(deqIdx)                       := enqEntryOldestSel(deqIdx)
      entriesIO.simpEntryOldestSel.foreach(_(deqIdx)            := simpEntryOldestSel.get(deqIdx))
      entriesIO.compEntryOldestSel.foreach(_(deqIdx)            := compEntryOldestSel.get(deqIdx))
      entriesIO.othersEntryOldestSel.foreach(_(deqIdx)          := othersEntryOldestSel(deqIdx))
      entriesIO.enqEntryOldestSelDelay(deqIdx)                  := RegNext(enqEntryOldestSel(deqIdx))
      entriesIO.simpEntryOldestSelDelay.foreach(_(deqIdx)       := RegNext(simpEntryOldestSel.get(deqIdx)))
      entriesIO.compEntryOldestSelDelay.foreach(_(deqIdx)       := RegNext(compEntryOldestSel.get(deqIdx)))
      entriesIO.othersEntryOldestSelDelay.foreach(_(deqIdx)     := RegNext(othersEntryOldestSel(deqIdx)))
      entriesIO.subDeqRequest.foreach(_(deqIdx)                 := subDeqRequest.get)
      entriesIO.subDeqSelOH.foreach(_(deqIdx)                   := subDeqSelOHVec.get(deqIdx))
    }
    entriesIO.wakeUpFromWB                                      := 0.U.asTypeOf(io.wakeupFromWB)
    entriesIO.wakeUpFromIQ                                      := wakeupFromIQ
    entriesIO.wakeUpFromWBDelayed                               := 0.U.asTypeOf(io.wakeupFromWBDelayed)
    println(s"[issueQueue] name = ${params.getIQName}")
    val wakeupFromWBExuName = io.wakeupFromWB.map(x => x.bits.exuIndices).map(i => i.map(backendParams.allExuParams(_).name))
    println(s"[issueQueue] wakeupFromWBExuName = ${wakeupFromWBExuName}")
    val vecExuIndices = params.backendParam.allExuParams.filter(x => x.isVfExeUnit || x.isMemExeUnit && x.needVecWen).map(_.exuIdx)
    println(s"[issueQueue] vecExuIndices = ${vecExuIndices}")
    val vecWBIndices = io.wakeupFromWB.zipWithIndex.filter(x => x._1.bits.exuIndices.intersect(vecExuIndices).nonEmpty).map(_._2)
    println(s"[issueQueue] vecWBIndices = ${vecWBIndices}")
    vecWBIndices.map{ case i =>
      entriesIO.wakeUpFromWB(i) := io.wakeupFromWB(i)
      entriesIO.wakeUpFromWBDelayed(i) := io.wakeupFromWBDelayed(i)
    }
    if (params.inVfSchd){
      entriesIO.wakeUpFromWB                                    := io.wakeupFromWB
      entriesIO.wakeUpFromWBDelayed                             := io.wakeupFromWBDelayed
    }
    entriesIO.wakeUpFromIQDelayed                               := wakeupFromIQDelayed
    entriesIO.vlFromIntIsZero                                   := io.vlFromIntIsZero
    entriesIO.vlFromIntIsVlmax                                  := io.vlFromIntIsVlmax
    entriesIO.vlFromVfIsZero                                    := io.vlFromVfIsZero
    entriesIO.vlFromVfIsVlmax                                   := io.vlFromVfIsVlmax
    entriesIO.og0Cancel                                         := io.og0Cancel
    entriesIO.og1Cancel                                         := io.og1Cancel
    entriesIO.ldCancel                                          := io.ldCancel
    entriesIO.simpEntryDeqSelVec.foreach(_                      := VecInit(simpEntryOldestSel.get.takeRight(params.numEnq).map(_.bits)))
    if (params.isVecMemIQ){
      entries.io.enq.zipWithIndex.map{ case(enqData, i) =>
        val enqStatus = enqData.bits.status
        enqStatus.vecMem.get.sqIdx := s0_enqBits(i).sqIdx.get
        enqStatus.vecMem.get.lqIdx := s0_enqBits(i).lqIdx.get
        // MemAddrIQ also handle vector insts
        enqStatus.vecMem.get.numLsElem := s0_enqBits(i).numLsElem.get

        val isFirstLoad = s0_enqBits(i).lqIdx.get <= io.memIO.get.lqDeqPtr.get
        val isVleff = s0_enqBits(i).vpu.get.isVleff
        enqStatus.blocked := !isFirstLoad && isVleff
      }
    }
    entries.io.vecMemIn.foreach(_.sqDeqPtr := io.memIO.get.sqDeqPtr.get)
    entries.io.vecMemIn.foreach(_.lqDeqPtr := io.memIO.get.lqDeqPtr.get)
    //output
    fuTypeVec                                                   := entriesIO.fuType
    deqEntryVec                                                 := entriesIO.deqEntry
    cancelDeqVec                                                := entriesIO.cancelDeqVec
    simpEntryEnqSelVec.foreach(_                                := entriesIO.simpEntryEnqSelVec.get)
    compEntryEnqSelVec.foreach(_                                := entriesIO.compEntryEnqSelVec.get)
    othersEntryEnqSelVec.foreach(_                              := entriesIO.othersEntryEnqSelVec.get)
  }


  s0_enqSelValidVec := s0_enqValidVec.zip(io.enq).map{ case (enqValid, enq) => enqValid && enq.ready}

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeVec.map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType))
    ).reverse)
  }

  protected val deqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    // alu and jmp are not same deq port, alu select intWen uop (include auipc and jalr)
    val aluDeqNeedPickJump = param.aluDeqNeedPickJump && fuCfgs.contains(AluCfg)
    val bjuDeqPickCond = param.aluDeqNeedPickJump && fuCfgs.contains(JmpCfg)
    if (aluDeqNeedPickJump) println(s"${param.getIQName} need select intWen uop")
    fuTypeVec.zip(rfWenVec).map { case (fuType, rfWen) =>
      val bjuDeqCanAccept = FuType.isBrh(fuType) || FuType.isJump(fuType) && !rfWen
      // alu and csr in same exeunit, but csr's rfwen may be false.B
      // not (branch || jump && !rfWen)
      val aluDeqCanAccept = !bjuDeqCanAccept
      val deqCanAccept = if (aluDeqNeedPickJump) aluDeqCanAccept
                         else if (bjuDeqPickCond) bjuDeqCanAccept
                         else FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType))
      deqCanAccept
    }
  }

  canIssueMergeAllBusy.zipWithIndex.foreach { case (merge, i) =>
    val mergeFuBusy = {
      if (fuBusyTableWrite(i).nonEmpty) canIssueVec.asUInt & (~fuBusyTableMask(i)).asUInt
      else canIssueVec.asUInt
    }
    val mergeIntWbBusy = {
      if (intWbBusyTableRead(i).nonEmpty) mergeFuBusy & (~intWbBusyTableMask(i)).asUInt
      else mergeFuBusy
    }
    val mergefpWbBusy = {
      if (fpWbBusyTableRead(i).nonEmpty) mergeIntWbBusy & (~fpWbBusyTableMask(i)).asUInt
      else mergeIntWbBusy
    }
    val mergeVfWbBusy = {
      if (vfWbBusyTableRead(i).nonEmpty) mergefpWbBusy & (~vfWbBusyTableMask(i)).asUInt
      else mergefpWbBusy
    }
    val mergeV0WbBusy = {
      if (v0WbBusyTableRead(i).nonEmpty) mergeVfWbBusy & (~v0WbBusyTableMask(i)).asUInt
      else mergeVfWbBusy
    }
    val mergeVlWbBusy = {
      if (vlWbBusyTableRead(i).nonEmpty) mergeV0WbBusy & (~vlWbBusyTableMask(i)).asUInt
      else  mergeV0WbBusy
    }
    merge := mergeVlWbBusy
  }

  deqCanIssue.zipWithIndex.foreach { case (req, i) =>
    req := canIssueMergeAllBusy(i) & VecInit(deqCanAcceptVec(i)).asUInt
  }
  dontTouch(fuTypeVec)
  dontTouch(canIssueMergeAllBusy)
  dontTouch(deqCanIssue)

  if (params.numDeq == 2) {
    require(params.deqFuSame || params.deqFuDiff, "The 2 deq ports need to be identical or completely different")
  }

  if (params.numDeq == 2 && params.deqFuSame) {
    val subDeqPolicy = Module(new DeqPolicy())

    enqEntryOldestSel := DontCare

    if (params.isAllComp || params.isAllSimp) {
      othersEntryOldestSel(0) := AgeDetector(numEntries = params.numEntries - params.numEnq,
        enq = othersEntryEnqSelVec.get,
        canIssue = canIssueVec.asUInt(params.numEntries-1, params.numEnq)
      )
      othersEntryOldestSel(1) := DontCare

      subDeqPolicy.io.request := subDeqRequest.get
      subDeqSelValidVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.bits)
    }
    else {
      simpAgeDetectRequest.get(0) := canIssueVec.asUInt(params.numEnq + params.numSimp - 1, params.numEnq)
      simpAgeDetectRequest.get(1) := DontCare
      simpAgeDetectRequest.get(params.numDeq) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt
      if (params.numEnq == 2) {
        simpAgeDetectRequest.get(params.numDeq + 1) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt & (~simpEntryOldestSel.get(params.numDeq).bits).asUInt
      }

      simpEntryOldestSel.get := AgeDetector(numEntries = params.numSimp,
        enq = simpEntryEnqSelVec.get,
        canIssue = simpAgeDetectRequest.get
      )

      compEntryOldestSel.get(0) := AgeDetector(numEntries = params.numComp,
        enq = compEntryEnqSelVec.get,
        canIssue = canIssueVec.asUInt(params.numEntries - 1, params.numEnq + params.numSimp)
      )
      compEntryOldestSel.get(1) := DontCare

      othersEntryOldestSel(0).valid := compEntryOldestSel.get(0).valid || simpEntryOldestSel.get(0).valid
      othersEntryOldestSel(0).bits := Cat(
        compEntryOldestSel.get(0).bits,
        Fill(params.numSimp, !compEntryOldestSel.get(0).valid) & simpEntryOldestSel.get(0).bits,
      )
      othersEntryOldestSel(1) := DontCare

      subDeqPolicy.io.request := Reverse(subDeqRequest.get)
      subDeqSelValidVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => Reverse(oh.bits))
    }

    subDeqRequest.get := canIssueVec.asUInt & (~Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W))).asUInt

    deqSelValidVec(0) := othersEntryOldestSel(0).valid || subDeqSelValidVec.get(1)
    deqSelValidVec(1) := subDeqSelValidVec.get(0)
    deqSelOHVec(0) := Mux(othersEntryOldestSel(0).valid,
                          Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W)),
                          subDeqSelOHVec.get(1)) & canIssueMergeAllBusy(0)
    deqSelOHVec(1) := subDeqSelOHVec.get(0) & canIssueMergeAllBusy(1)

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach { case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid && deqOH.orR
      selOH := deqOH
    }
  }
  else {
    enqEntryOldestSel := NewAgeDetector(numEntries = params.numEnq,
      enq = VecInit(s0_doEnqSelValidVec),
      canIssue = VecInit(deqCanIssue.map(_(params.numEnq - 1, 0)))
    )

    if (params.isAllComp || params.isAllSimp) {
      othersEntryOldestSel := AgeDetector(numEntries = params.numEntries - params.numEnq,
        enq = othersEntryEnqSelVec.get,
        canIssue = VecInit(deqCanIssue.map(_(params.numEntries - 1, params.numEnq)))
      )

      deqSelValidVec.zip(deqSelOHVec).zipWithIndex.foreach { case ((selValid, selOH), i) =>
        if (params.exuBlockParams(i).fuConfigs.contains(FuConfig.FakeHystaCfg)) {
          selValid := false.B
          selOH := 0.U.asTypeOf(selOH)
        } else {
          selValid := othersEntryOldestSel(i).valid || enqEntryOldestSel(i).valid
          selOH := Cat(othersEntryOldestSel(i).bits, Fill(params.numEnq, !othersEntryOldestSel(i).valid) & enqEntryOldestSel(i).bits)
        }
      }
    }
    else {
      othersEntryOldestSel := DontCare

      deqCanIssue.zipWithIndex.foreach { case (req, i) =>
        simpAgeDetectRequest.get(i) := req(params.numEnq + params.numSimp - 1, params.numEnq)
      }
      simpAgeDetectRequest.get(params.numDeq) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt
      if (params.numEnq == 2) {
        simpAgeDetectRequest.get(params.numDeq + 1) := VecInit(requestForTrans.drop(params.numEnq).take(params.numSimp)).asUInt & (~simpEntryOldestSel.get(params.numDeq).bits).asUInt
      }

      simpEntryOldestSel.get := AgeDetector(numEntries = params.numSimp,
        enq = simpEntryEnqSelVec.get,
        canIssue = simpAgeDetectRequest.get
      )

      compEntryOldestSel.get := AgeDetector(numEntries = params.numComp,
        enq = compEntryEnqSelVec.get,
        canIssue = VecInit(deqCanIssue.map(_(params.numEntries - 1, params.numEnq + params.numSimp)))
      )

      deqSelValidVec.zip(deqSelOHVec).zipWithIndex.foreach { case ((selValid, selOH), i) =>
        if (params.exuBlockParams(i).fuConfigs.contains(FuConfig.FakeHystaCfg)) {
          selValid := false.B
          selOH := 0.U.asTypeOf(selOH)
        } else {
          selValid := compEntryOldestSel.get(i).valid || simpEntryOldestSel.get(i).valid || enqEntryOldestSel(i).valid
          selOH := Cat(
            compEntryOldestSel.get(i).bits,
            Fill(params.numSimp, !compEntryOldestSel.get(i).valid) & simpEntryOldestSel.get(i).bits,
            Fill(params.numEnq, !compEntryOldestSel.get(i).valid && !simpEntryOldestSel.get(i).valid) & enqEntryOldestSel(i).bits
          )
        }
      }
    }

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach { case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid
      selOH := deqOH
    }
  }

  val toBusyTableDeqResp = Wire(Vec(params.numDeq, ValidIO(new IssueQueueRespBundle)))

  toBusyTableDeqResp.zipWithIndex.foreach { case (deqResp, i) =>
    deqResp.valid := deqBeforeDly(i).valid
    deqResp.bits.failed := false.B
    deqResp.bits.finalSuccess := false.B
    deqResp.bits.fuType := deqBeforeDly(i).bits.fuType
    deqResp.bits.sqIdx.foreach(_ := 0.U.asTypeOf(new SqPtr))
    deqResp.bits.lqIdx.foreach(_ := 0.U.asTypeOf(new LqPtr))
  }

  //fuBusyTable
  fuBusyTableWrite.zip(fuBusyTableRead).zipWithIndex.foreach { case ((busyTableWrite: Option[FuBusyTableWrite], busyTableRead: Option[FuBusyTableRead]), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val btrd = busyTableRead.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      btrd.io.in.fuBusyTable := btwr.io.out.fuBusyTable
      btrd.io.in.fuTypeRegVec := fuTypeVec
      fuBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      fuBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  //wbfuBusyTable write
  intWbBusyTableWrite.zip(intWbBusyTableOut).zip(intDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.rfWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  fpWbBusyTableWrite.zip(fpWbBusyTableOut).zip(fpDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.fpWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  vfWbBusyTableWrite.zip(vfWbBusyTableOut).zip(vfDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.vecWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  v0WbBusyTableWrite.zip(v0WbBusyTableOut).zip(v0DeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.v0Wen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  vlWbBusyTableWrite.zip(vlWbBusyTableOut).zip(vlDeqRespSetOut).zipWithIndex.foreach { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
    if (busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val bt = busyTable.get
      val dq = deqResp.get
      btwr.io.in.deqResp := toBusyTableDeqResp(i)
      btwr.io.in.deqResp.valid := toBusyTableDeqResp(i).valid && deqBeforeDly(i).bits.vlWen.getOrElse(false.B)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      bt := btwr.io.out.fuBusyTable
      dq := btwr.io.out.deqRespSet
    }
  }

  //wbfuBusyTable read
  intWbBusyTableRead.zip(intWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if(busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      intWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      intWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }
  fpWbBusyTableRead.zip(fpWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      fpWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      fpWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }
  vfWbBusyTableRead.zip(vfWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      vfWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      vfWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }
  v0WbBusyTableRead.zip(v0WbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      v0WbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      v0WbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }
  vlWbBusyTableRead.zip(vlWbBusyTableIn).zipWithIndex.foreach { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeVec
      vlWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      vlWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  io.wakeupFromExu.foreach(x => x.map(_.ready := true.B))
  wakeUpQueues.zipWithIndex.foreach { case (wakeUpQueueOption, i) =>
    wakeUpQueueOption.foreach {
      wakeUpQueue =>
        val flush = Wire(new WakeupQueueFlush)
        flush.redirect := io.flush
        flush.ldCancel := io.ldCancel
        flush.og0Fail := io.og0Resp(i).failed
        flush.og1Fail := io.og1Resp(i).failed
        wakeUpQueue.io.flush := flush
        if (params.exuBlockParams(i).needUncertainWakeup){
          val wakeupFromExu = io.wakeupFromExu.get(i)
          wakeUpQueue.io.enq.bits.uop := 0.U.asTypeOf(wakeUpQueue.io.enq.bits.uop)
          // int schduler uncertain has high priority
          if (params.inIntSchd){
            wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid && !FuType.isUncertain(deqBeforeDly(i).bits.fuType) || wakeupFromExu.valid
            // loadDependency only from deqBeforeDly
            wakeUpQueue.io.enq.bits.uop.loadDependency.foreach(x => x := Mux(wakeupFromExu.valid, 0.U.asTypeOf(x), deqBeforeDly(i).bits.loadDependency.get))
            wakeUpQueue.io.enq.bits.uop.rfWen.foreach(x => x := Mux(wakeupFromExu.valid, wakeupFromExu.bits.rfWen, deqBeforeDly(i).bits.rfWen.get))
            wakeUpQueue.io.enq.bits.uop.fpWen.foreach(x => x := Mux(wakeupFromExu.valid, wakeupFromExu.bits.fpWen, deqBeforeDly(i).bits.fpWen.get))
            wakeUpQueue.io.enq.bits.uop.vecWen.foreach(x => x := Mux(wakeupFromExu.valid, wakeupFromExu.bits.vecWen, deqBeforeDly(i).bits.vecWen.get))
            wakeUpQueue.io.enq.bits.uop.v0Wen.foreach(x => x := Mux(wakeupFromExu.valid, wakeupFromExu.bits.v0Wen, deqBeforeDly(i).bits.v0Wen.get))
            wakeUpQueue.io.enq.bits.uop.vlWen.foreach(x => x := Mux(wakeupFromExu.valid, wakeupFromExu.bits.vlWen, deqBeforeDly(i).bits.vlWen.get))
            wakeUpQueue.io.enq.bits.uop.pdest := Mux(wakeupFromExu.valid, wakeupFromExu.bits.pdest, deqBeforeDly(i).bits.pdest)
            wakeUpQueue.io.enq.bits.uop.fuType := Mux(wakeupFromExu.valid, FuType.div.U, deqBeforeDly(i).bits.fuType)
            wakeUpQueue.io.enq.bits.lat := Mux(wakeupFromExu.valid, 0.U, getDeqLat(i, deqBeforeDly(i).bits.fuType))
            wakeUpQueue.io.enq.bits.uop.is0Lat.foreach(_ := Mux(wakeupFromExu.valid, false.B, getDeqLat(i, deqBeforeDly(i).bits.fuType) === 0.U))
            // wakeupFromExu's valid need after flush
            wakeUpQueue.io.enq.bits.uop.robIdx := Mux(wakeupFromExu.valid, 0.U.asTypeOf(wakeUpQueue.io.enq.bits.uop.robIdx), deqBeforeDly(i).bits.robIdx)
          }
          else if (params.inFpSchd){
            wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid && !FuType.isUncertain(deqBeforeDly(i).bits.fuType)
            wakeupFromExu.ready := wakeUpQueue.io.enqAppend.ready
            // loadDependency only from deqBeforeDly
            wakeUpQueue.io.enq.bits.uop.loadDependency.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.loadDependency.get, 0.U.asTypeOf(x)))
            wakeUpQueue.io.enq.bits.uop.rfWen.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.rfWen.get, wakeupFromExu.bits.rfWen))
            wakeUpQueue.io.enq.bits.uop.fpWen.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.fpWen.get, wakeupFromExu.bits.fpWen))
            wakeUpQueue.io.enq.bits.uop.vecWen.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.vecWen.get, wakeupFromExu.bits.vecWen))
            wakeUpQueue.io.enq.bits.uop.v0Wen.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.v0Wen.get, wakeupFromExu.bits.v0Wen))
            wakeUpQueue.io.enq.bits.uop.vlWen.foreach(x => x := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.vlWen.get, wakeupFromExu.bits.vlWen))
            wakeUpQueue.io.enq.bits.uop.pdest := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.pdest, wakeupFromExu.bits.pdest)
            wakeUpQueue.io.enq.bits.uop.fuType := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.fuType, FuType.fDivSqrt.U)
            wakeUpQueue.io.enq.bits.lat := Mux(deqBeforeDly(i).valid, getDeqLat(i, deqBeforeDly(i).bits.fuType), 0.U)
            // fp don't have 0 lat fu
            // wakeupFromExu's valid need after flush
            wakeUpQueue.io.enq.bits.uop.robIdx := Mux(deqBeforeDly(i).valid, deqBeforeDly(i).bits.robIdx, 0.U.asTypeOf(wakeUpQueue.io.enq.bits.uop.robIdx))
          }
        }
        else{
          wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid
          wakeUpQueue.io.enq.bits.uop := 0.U.asTypeOf(wakeUpQueue.io.enq.bits.uop)
          connectSamePort(wakeUpQueue.io.enq.bits.uop, deqBeforeDly(i).bits)
          wakeUpQueue.io.enq.bits.uop.pdestCopy.foreach(_ := 0.U)
          wakeUpQueue.io.enq.bits.lat := getDeqLat(i, deqBeforeDly(i).bits.fuType)
          wakeUpQueue.io.enq.bits.uop.is0Lat.foreach( _ := getDeqLat(i, deqBeforeDly(i).bits.fuType) === 0.U)
          // int i2f wakeup fstore from fpRegion, so there is not need fpWen
          if (params.inIntSchd) {
            wakeUpQueue.io.enq.bits.uop.fpWen.foreach(_ := false.B)
          }
          else if (params.inFpSchd) {
            wakeUpQueue.io.enq.bits.uop.rfWen.foreach(_ := false.B)
          }
        }
        wakeUpQueue.io.enqAppend.valid := false.B
        wakeUpQueue.io.enqAppend.bits := 0.U.asTypeOf(wakeUpQueue.io.enqAppend.bits)
        if (params.exuBlockParams(i).needDataFromI2F) {
          wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid && deqBeforeDly(i).bits.fpWen.get
          val wakeupFromI2F = io.wakeupFromI2F.get
          wakeUpQueue.io.enqAppend.valid := wakeupFromI2F.valid
          wakeUpQueue.io.enqAppend.bits.uop.fpWen.foreach(x => x := wakeupFromI2F.bits.fpWen)
          wakeUpQueue.io.enqAppend.bits.uop.pdest := wakeupFromI2F.bits.pdest
          wakeUpQueue.io.enqAppend.bits.uop.is0Lat.foreach(_ := false.B)
          wakeUpQueue.io.enqAppend.bits.lat := 0.U
        }
        else if (params.exuBlockParams(i).needDataFromF2I) {
          wakeUpQueue.io.enq.valid := deqBeforeDly(i).valid && deqBeforeDly(i).bits.rfWen.get
          val wakeupFromF2I = io.wakeupFromF2I.get
          wakeUpQueue.io.enqAppend.valid := wakeupFromF2I.valid
          wakeUpQueue.io.enqAppend.bits.uop.rfWen.foreach(x => x := wakeupFromF2I.bits.rfWen)
          wakeUpQueue.io.enqAppend.bits.uop.pdest := wakeupFromF2I.bits.pdest
          wakeUpQueue.io.enqAppend.bits.uop.is0Lat.foreach(_ := false.B)
          wakeUpQueue.io.enqAppend.bits.lat := 0.U
        }
        else if (params.exuBlockParams(i).fuConfigs.contains(FuConfig.FdivCfg)) {
          val wakeupFromExu = io.wakeupFromExu.get(i)
          wakeUpQueue.io.enqAppend.valid := wakeupFromExu.valid
          wakeUpQueue.io.enqAppend.bits.uop.fpWen.foreach(x => x := wakeupFromExu.bits.fpWen)
          wakeUpQueue.io.enqAppend.bits.uop.pdest := wakeupFromExu.bits.pdest
          wakeUpQueue.io.enqAppend.bits.uop.is0Lat.foreach(_ := false.B)
          wakeUpQueue.io.enqAppend.bits.lat := 0.U
        }
    }
  }

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i) && !cancelDeqVec(i)
    // bju deq valid
    if (params.aluDeqNeedPickJump && (i ==1)) {
      deq.valid := finalDeqSelValidVec(i) && !cancelDeqVec(i) || entries.io.aluDeqSelectJump.get && deqBeforeDly(0).valid
    }
    deq.bits.isFirstIssue := deqFirstIssueVec(i)
    deq.bits.iqIdx    := OHToUInt(finalDeqSelOHVec(i))
    deq.bits.fuType   := IQFuType.readFuType(deqEntryVec(i).bits.status.fuType, params.getFuCfgs.map(_.fuType)).asUInt
    deq.bits.rfWen.foreach(_ := deqEntryVec(i).bits.payload.rfWen.get)
    deq.bits.fpWen.foreach(_ := deqEntryVec(i).bits.payload.fpWen.get)
    deq.bits.vecWen.foreach(_ := deqEntryVec(i).bits.payload.vecWen.get)
    deq.bits.v0Wen.foreach(_ := deqEntryVec(i).bits.payload.v0Wen.get)
    deq.bits.vlWen.foreach(_ := deqEntryVec(i).bits.payload.vlWen.get)
    deq.bits.flushPipe.foreach(_ := false.B)
    deq.bits.pdest := deqEntryVec(i).bits.payload.pdest
    deq.bits.pdestVl.foreach(_ := deqEntryVec(i).bits.payload.pdestVl.get)
    deq.bits.robIdx := deqEntryVec(i).bits.status.robIdx

    require(deq.bits.dataSources.size <= finalDataSources(i).size)
    deq.bits.dataSources.zip(finalDataSources(i)).foreach { case (sink, source) => sink := source}
    deq.bits.exuSources.foreach(_.zip(finalExuSources.get(i)).foreach { case (sink, source) => sink := source})
    deq.bits.loadDependency.foreach(_.zip(finalLoadDependency(i)).foreach { case (sink, source) => sink := source})
    // when alu select jump uop, src0's dataSource change to imm
    if (params.aluDeqNeedPickJump && (i == 0)) {
      val src0IsReadReg = finalDataSources(i)(0).readReg
      deq.bits.dataSources(0).value := Mux(entries.io.aluDeqSelectJump.get && src0IsReadReg, DataSource.imm, finalDataSources(i)(0).value)
    }
    else if (params.aluDeqNeedPickJump && (i == 1)) {
      // assign jump uop form alu deq
      when(entries.io.aluDeqSelectJump.get) {
        deq.bits.dataSources := finalDataSources(0)
        deq.bits.exuSources.foreach(_ := deqBeforeDly(0).bits.exuSources.get)
        deq.bits.loadDependency.foreach(_ := deqBeforeDly(0).bits.loadDependency.get)
      }
    }

    deq.bits.rf.zip(deqEntryVec(i).bits.status.srcStatus.map(_.psrc)).zip(deqEntryVec(i).bits.status.srcStatus.map(_.srcType)).foreach { case ((rf, psrc), srcType) =>
      // psrc in status array can be pregIdx of IntRegFile or VfRegFile
      rf.foreach(_.addr := psrc)
      rf.foreach(_.srcType := srcType)
      rf.foreach(_.robIdx := deqEntryVec(i).bits.status.robIdx)
      rf.foreach(_.issueValid := deqEntryVec(i).valid)
    }
    deq.bits.rfVl lazyZip deqEntryVec(i).bits.status.srcStatusVl.map(_.psrc) foreach {
      case (rf, psrc) =>
        rf.addr := psrc
        rf.srcType := SrcType.vp // this is vl
        rf.robIdx := deqEntryVec(i).bits.status.robIdx
        rf.issueValid := deqEntryVec(i).valid
    }
    deq.bits.srcType.zip(deqEntryVec(i).bits.status.srcStatus.map(_.srcType)).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.rcIdx.foreach(_ := deqEntryVec(i).bits.status.srcStatus.map(_.regCacheIdx.get))
    deq.bits.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr.get)
    deq.bits.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset.get)
    deq.bits.perfDebugInfo.foreach(_ := deqEntryVec(i).bits.payload.debug.get.perfDebugInfo)
    deq.bits.debug_seqNum.foreach(_ := deqEntryVec(i).bits.payload.debug.get.debug_seqNum)
    deq.bits.perfDebugInfo.foreach(_.selectTime := GTimer())
    deq.bits.perfDebugInfo.foreach(_.issueTime := GTimer() + 1.U)
  }

  val deqDelay = Reg(params.genIssueValidBundle)
  deqDelay.zip(deqBeforeDly).zipWithIndex.foreach { case ((deqDly, deq), i) =>
    deqDly.valid := deq.valid
    when(validVec.asUInt.orR) {
      deqDly.bits := deq.bits
    }
    // deqBeforeDly.ready is always true
    deq.ready := true.B
    // for int scheduler fdiv has high priority than alu
    if (params.inIntSchd && (deqFuCfgs(i).contains(DivCfg) || deqFuCfgs(i).contains(CsrCfg))) {
      // div and csr need wakeupFromExu
      io.wakeupFromExu.foreach(x => {
        deq.ready := !x.head.valid
        deqDly.valid := deq.valid && !x.head.valid
      })
    }
    if (params.aluDeqNeedPickJump && deqFuCfgs(i).contains(JmpCfg)) {
      io.wakeupFromExu.foreach(x => {
        deq.ready := !entries.io.aluDeqSelectJump.get
        deqDly.valid := deq.valid && !(entries.io.aluDeqSelectJump.get && x.head.valid)
      })
    }
  }
  io.deqDelay.zip(deqDelay).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
  }
  io.deqOg1Payload := entries.io.deqOg1Payload
  if(backendParams.debugEn) {
    dontTouch(deqDelay)
    dontTouch(io.deqDelay)
    dontTouch(deqBeforeDly)
  }
  io.wakeupToIQ.zipWithIndex.foreach { case (wakeup, i) =>
    if (wakeUpQueues(i).nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits)
      wakeup.bits.rcDest.foreach(_ := io.replaceRCIdx.get(i))
    } else if (param.isLdAddrIQ)  {
      // for load IQ
      val loadWakeUpIter = io.memIO.get.loadWakeUp.iterator
      io.wakeupToIQ.zip(params.exuBlockParams).zipWithIndex.foreach { case ((wakeup, param), i) =>
        val loadWakeUp = loadWakeUpIter.next()
        wakeup.valid := RegNext(loadWakeUp.valid)
        wakeup.bits.rfWen   := (param.writeIntRf).B && RegNext(loadWakeUp.bits.rfWen && loadWakeUp.valid)
        wakeup.bits.fpWen   := (param.writeFpRf ).B && RegNext(loadWakeUp.bits.fpWen && loadWakeUp.valid)
        wakeup.bits.vecWen  := (param.writeVecRf).B && RegNext(loadWakeUp.bits.vecWen && loadWakeUp.valid)
        wakeup.bits.v0Wen   := (param.writeV0Rf ).B && RegNext(loadWakeUp.bits.v0Wen && loadWakeUp.valid)
        wakeup.bits.vlWen   := (param.writeVlRf ).B && RegNext(loadWakeUp.bits.vlWen && loadWakeUp.valid)
        wakeup.bits.pdest   := RegNext(loadWakeUp.bits.pdest)
        wakeup.bits.pdestVl := 0.U
        wakeup.bits.rcDest.foreach(_ := io.replaceRCIdx.get(i))
        wakeup.bits.loadDependency.foreach(_ := 0.U) // this is correct for load only
        wakeup.bits.is0Lat := 0.U
        wakeup.bits.rfWenCopy.foreach(_.foreach(_  := (param.writeIntRf).B && RegNext(loadWakeUp.bits.rfWen && loadWakeUp.valid)))
        wakeup.bits.fpWenCopy.foreach(_.foreach(_  := (param.writeFpRf ).B && RegNext(loadWakeUp.bits.fpWen && loadWakeUp.valid)))
        wakeup.bits.vecWenCopy.foreach(_.foreach(_ := (param.writeVecRf).B && RegNext(loadWakeUp.bits.vecWen && loadWakeUp.valid)))
        wakeup.bits.v0WenCopy.foreach(_.foreach(_  := (param.writeV0Rf ).B && RegNext(loadWakeUp.bits.v0Wen && loadWakeUp.valid)))
        wakeup.bits.vlWenCopy.foreach(_.foreach(_  := (param.writeVlRf ).B && RegNext(loadWakeUp.bits.vlWen && loadWakeUp.valid)))
        wakeup.bits.pdestCopy.foreach(_.foreach(_  := RegNext(loadWakeUp.bits.pdest)))
        wakeup.bits.loadDependencyCopy.foreach(x => x := 0.U.asTypeOf(x)) // this is correct for load only

      }
    }
  }

  // Todo: better counter implementation
  private val enqHasValid = validVec.take(params.numEnq).reduce(_ | _)
  private val enqHasIssued = validVec.zip(issuedVec).take(params.numEnq).map(x => x._1 & x._2).reduce(_ | _)
  private val enqEntryValidCnt = PopCount(validVec.take(params.numEnq))
  private val othersValidCnt = PopCount(validVec.drop(params.numEnq))
  private val enqEntryValidCntDeq0 = PopCount(
    validVec.take(params.numEnq).zip(deqCanAcceptVec(0).take(params.numEnq)).map { case (a, b) => a && b }
  )
  private val othersValidCntDeq0 = PopCount(
    validVec.drop(params.numEnq).zip(deqCanAcceptVec(0).drop(params.numEnq)).map { case (a, b) => a && b }
  )
  private val enqEntryValidCntDeq1 = PopCount(
    validVec.take(params.numEnq).zip(deqCanAcceptVec.last.take(params.numEnq)).map { case (a, b) => a && b }
  )
  private val othersValidCntDeq1 = PopCount(
    validVec.drop(params.numEnq).zip(deqCanAcceptVec.last.drop(params.numEnq)).map { case (a, b) => a && b }
  )
  protected val deqCanAcceptVecEnq: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    io.enq.map(_.bits.fuType).map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType)))
  }
  protected val enqValidCntDeq0 = PopCount(io.enq.map(_.fire).zip(deqCanAcceptVecEnq(0)).map { case (a, b) => a && b })
  protected val enqValidCntDeq1 = PopCount(io.enq.map(_.fire).zip(deqCanAcceptVecEnq.last).map { case (a, b) => a && b })
  io.validCntDeqVec.head := RegNext(enqEntryValidCntDeq0 +& othersValidCntDeq0 - io.deqDelay.head.fire) // validCntDeqVec(0)
  io.validCntDeqVec.last := RegNext(enqEntryValidCntDeq1 +& othersValidCntDeq1 - io.deqDelay.last.fire) // validCntDeqVec(1)
  private val othersLeftOneCaseVec = Wire(Vec(params.numEntries - params.numEnq, UInt((params.numEntries - params.numEnq).W)))
  othersLeftOneCaseVec.zipWithIndex.foreach { case (leftone, i) =>
    leftone := ~(1.U((params.numEntries - params.numEnq).W) << i)
  }
  private val othersLeftOne = othersLeftOneCaseVec.map(_ === VecInit(validVec.drop(params.numEnq)).asUInt).reduce(_ | _)
  private val othersCanotIn = Wire(Bool())
  othersCanotIn := othersLeftOne || validVec.drop(params.numEnq).reduce(_ & _)
  // if has simp Entry, othersCanotIn will be simpCanotIn
  if (params.numSimp > 0) {
    val simpLeftOneCaseVec = Wire(Vec(params.numSimp, UInt((params.numSimp).W)))
    simpLeftOneCaseVec.zipWithIndex.foreach { case (leftone, i) =>
      leftone := ~(1.U((params.numSimp).W) << i)
    }
    val simpLeftOne = simpLeftOneCaseVec.map(_ === VecInit(validVec.drop(params.numEnq).take(params.numSimp)).asUInt).reduce(_ | _)
    val simpCanotIn = simpLeftOne || validVec.drop(params.numEnq).take(params.numSimp).reduce(_ & _)
    othersCanotIn := simpCanotIn
  }
  io.enq.foreach(_.ready := (!othersCanotIn || !enqHasValid) && !enqHasIssued)

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    Mux(FuType.isUncertain(fuType),
      1.U,
      Mux1H(wakeupFuLatencySeqs(deqPortIdx) map { case (k, v) => (fuType(k.id), v.U) })
    )
  }

  // issue perf counter
  // enq count
  XSPerfAccumulate("enq_valid_cnt", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_fire_cnt", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_alu_fire_cnt", PopCount(io.enq.map { case enq => enq.fire && FuType.isAlu(enq.bits.fuType) }))
  XSPerfAccumulate("enq_brh_fire_cnt", PopCount(io.enq.map { case enq => enq.fire && FuType.isBrh(enq.bits.fuType) }))
  XSPerfAccumulate("deqDelay0_fire_cnt", PopCount(io.deqDelay.head.fire))
  XSPerfAccumulate("deqDelay1_fire_cnt", PopCount(io.deqDelay.last.fire))
  // valid count
  XSPerfHistogram("enq_entry_valid_cnt", enqEntryValidCnt, true.B, 0, params.numEnq + 1)
  XSPerfHistogram("other_entry_valid_cnt", othersValidCnt, true.B, 0, params.numEntries - params.numEnq + 1)
  XSPerfHistogram("valid_cnt", PopCount(validVec), true.B, 0, params.numEntries + 1)
  // only split when more than 1 func type
  if (params.getFuCfgs.size > 0) {
    for (t <- FuType.functionNameMap.keys) {
      val fuName = FuType.functionNameMap(t)
      if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
        XSPerfHistogram(s"valid_cnt_hist_futype_${fuName}", PopCount(validVec.zip(fuTypeVec).map { case (v, fu) => v && fu === t.U }), true.B, 0, params.numEntries, 1)
      }
    }
  }
  // ready instr count
  private val readyEntriesCnt = PopCount(validVec.zip(canIssueVec).map(x => x._1 && x._2))
  XSPerfHistogram("ready_cnt", readyEntriesCnt, true.B, 0, params.numEntries + 1)
  // only split when more than 1 func type
  if (params.getFuCfgs.size > 0) {
    for (t <- FuType.functionNameMap.keys) {
      val fuName = FuType.functionNameMap(t)
      if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
        XSPerfHistogram(s"ready_cnt_hist_futype_${fuName}", PopCount(validVec.zip(canIssueVec).zip(fuTypeVec).map { case ((v, c), fu) => v && c && fu === t.U }), true.B, 0, params.numEntries, 1)
      }
    }
  }

  // deq instr count
  XSPerfAccumulate("issue_instr_pre_count", PopCount(deqBeforeDly.map(_.valid)))
  XSPerfHistogram("issue_instr_pre_count_hist", PopCount(deqBeforeDly.map(_.valid)), true.B, 0, params.numDeq + 1, 1)
  XSPerfAccumulate("issue_instr_count", PopCount(io.deqDelay.map(_.valid)))
  XSPerfHistogram("issue_instr_count_hist", PopCount(io.deqDelay.map(_.valid)), true.B, 0, params.numDeq + 1, 1)

  // deq instr data source count
  XSPerfAccumulate("issue_datasource_reg", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_bypass", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_forward", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_noreg", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _))

  XSPerfHistogram("issue_datasource_reg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_bypass_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_forward_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_noreg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)

  // deq instr data source count for each futype
  for (t <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(t)
    if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
      XSPerfAccumulate(s"issue_datasource_reg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_bypass_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_forward_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_noreg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _))

      XSPerfHistogram(s"issue_datasource_reg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_bypass_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_forward_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_noreg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.fuType === t.U })
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
    }
  }
}

class IssueQueueMemBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
  // load wakeup
  val loadWakeUp = Input(Vec(params.LdExuCnt, ValidIO(new MemWakeUpBundle)))
  // vector
  val sqDeqPtr = Option.when(params.isVecMemIQ)(Input(new SqPtr))
  val lqDeqPtr = Option.when(params.isVecMemIQ)(Input(new LqPtr))
}
