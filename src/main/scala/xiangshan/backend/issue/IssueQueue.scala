package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{GTimer, HasCircularQueuePtrHelper, SelectOne}
import utils._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.{ImmUnion, Imm_LUI_LOAD}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.backend.datapath.NewPipelineConnect

class IssueQueue(params: IssueBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  implicit val iqParams = params
  lazy val module: IssueQueueImp = iqParams.schdType match {
    case IntScheduler() => new IssueQueueIntImp(this)
    case VfScheduler() => new IssueQueueVfImp(this)
    case MemScheduler() => if (iqParams.StdCnt == 0) new IssueQueueMemAddrImp(this)
      else new IssueQueueIntImp(this)
    case _ => null
  }
}

class IssueQueueStatusBundle(numEnq: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val leftVec = Output(Vec(numEnq + 1, Bool()))
}

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueBlockParams) extends EntryDeqRespBundle

class IssueQueueIO()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  // Inputs
  val flush = Flipped(ValidIO(new Redirect))
  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val og0Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og1Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val finalIssueResp = OptionWrapper(params.LduCnt > 0, Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle))))
  val wbBusyTableRead = Input(params.genWbFuBusyTableReadBundle())
  val wbBusyTableWrite = Output(params.genWbFuBusyTableWriteBundle())
  val wakeupFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeupFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel = Input(ExuVec(backendParams.numExu))
  val og1Cancel = Input(ExuVec(backendParams.numExu))
  val ldCancel = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  val finalBlock = Vec(params.numExu, Input(Bool()))

  // Outputs
  val wakeupToIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpSourceValidBundle
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  // val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))

  val deqDelay: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle// = deq.cloneType
  def allWakeUp = wakeupFromWB ++ wakeupFromIQ
}

class IssueQueueImp(override val wrapper: IssueQueue)(implicit p: Parameters, val params: IssueBlockParams)
  extends LazyModuleImp(wrapper)
  with HasXSParameter {

  println(s"[IssueQueueImp] ${params.getIQName} wakeupFromWB(${io.wakeupFromWB.size}), " +
    s"wakeup exu in(${params.wakeUpInExuSources.size}): ${params.wakeUpInExuSources.map(_.name).mkString("{",",","}")}, " +
    s"wakeup exu out(${params.wakeUpOutExuSources.size}): ${params.wakeUpOutExuSources.map(_.name).mkString("{",",","}")}, " +
    s"numEntries: ${params.numEntries}, numRegSrc: ${params.numRegSrc}")

  require(params.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs)
  val allDeqFuCfgs  : Seq[FuConfig] = params.exuBlockParams.flatMap(_.fuConfigs)
  val fuCfgsCnt     : Map[FuConfig, Int] = allDeqFuCfgs.groupBy(x => x).map { case (cfg, cfgSeq) => (cfg, cfgSeq.length) }
  val commonFuCfgs  : Seq[FuConfig] = fuCfgsCnt.filter(_._2 > 1).keys.toSeq
  val fuLatencyMaps : Seq[Map[FuType.OHType, Int]] = params.exuBlockParams.map(x => x.fuLatencyMap)

  println(s"[IssueQueueImp] ${params.getIQName} fuLatencyMaps: ${fuLatencyMaps}")
  println(s"[IssueQueueImp] ${params.getIQName} commonFuCfgs: ${commonFuCfgs.map(_.name)}")
  lazy val io = IO(new IssueQueueIO())
  // Modules

  val entries = Module(new Entries)
  val fuBusyTableWrite = params.exuBlockParams.map { case x => OptionWrapper(x.latencyValMax > 0, Module(new FuBusyTableWrite(x.fuLatencyMap))) }
  val fuBusyTableRead = params.exuBlockParams.map { case x => OptionWrapper(x.latencyValMax > 0, Module(new FuBusyTableRead(x.fuLatencyMap))) }
  val intWbBusyTableWrite = params.exuBlockParams.map { case x => OptionWrapper(x.intLatencyCertain, Module(new FuBusyTableWrite(x.intFuLatencyMap))) }
  val intWbBusyTableRead = params.exuBlockParams.map { case x => OptionWrapper(x.intLatencyCertain, Module(new FuBusyTableRead(x.intFuLatencyMap))) }
  val vfWbBusyTableWrite = params.exuBlockParams.map { case x => OptionWrapper(x.vfLatencyCertain, Module(new FuBusyTableWrite(x.vfFuLatencyMap))) }
  val vfWbBusyTableRead = params.exuBlockParams.map { case x => OptionWrapper(x.vfLatencyCertain, Module(new FuBusyTableRead(x.vfFuLatencyMap))) }

  class WakeupQueueFlush extends Bundle {
    val redirect = ValidIO(new Redirect)
    val ldCancel = Vec(backendParams.LduCnt, new LoadCancelIO)
    val og0Fail = Output(Bool())
    val og1Fail = Output(Bool())
    val finalFail = Output(Bool())
  }

  private def flushFunc(exuInput: ExuInput, flush: WakeupQueueFlush, stage: Int): Bool = {
    val redirectFlush = exuInput.robIdx.needFlush(flush.redirect)
    val loadDependencyFlush = LoadShouldCancel(exuInput.loadDependency, flush.ldCancel)
    val ogFailFlush = stage match {
      case 1 => flush.og0Fail
      case 2 => flush.og1Fail
      case 3 => flush.finalFail
      case _ => false.B
    }
    redirectFlush || loadDependencyFlush || ogFailFlush
  }

  private def modificationFunc(exuInput: ExuInput, newInput: ExuInput): ExuInput = {
    val lastExuInput = WireDefault(exuInput)
    val newExuInput = WireDefault(newInput)
    newExuInput.elements.foreach{ case (name, data) =>
      if (lastExuInput.elements.contains(name)){
        data := lastExuInput.elements(name)
      }
    }
    newExuInput.loadDependency match {
      case Some(deps) => deps.zip(exuInput.loadDependency.get).foreach(x => x._1 := x._2 << 1)
      case None =>
    }
    if (newExuInput.pdestCopy.nonEmpty && !lastExuInput.pdestCopy.nonEmpty) {
      newExuInput.pdestCopy.get.foreach(_ := lastExuInput.pdest)
    }
    newExuInput
  }

  val wakeUpQueues: Seq[Option[MultiWakeupQueue[ExuInput, WakeupQueueFlush]]] = params.exuBlockParams.map { x => OptionWrapper(x.isIQWakeUpSource, Module(
    new MultiWakeupQueue(new ExuInput(x), new ExuInput(x, x.copyPdest, x.iqWakeUpSourcePairs.size / x.copyDistance), new WakeupQueueFlush, x.fuLatancySet, flushFunc, modificationFunc)
  ))}
  val deqBeforeDly = Wire(params.genIssueDecoupledBundle)

  val intWbBusyTableIn = io.wbBusyTableRead.map(_.intWbBusyTable)
  val vfWbBusyTableIn = io.wbBusyTableRead.map(_.vfWbBusyTable)
  val intWbBusyTableOut = io.wbBusyTableWrite.map(_.intWbBusyTable)
  val vfWbBusyTableOut = io.wbBusyTableWrite.map(_.vfWbBusyTable)
  val intDeqRespSetOut = io.wbBusyTableWrite.map(_.intDeqRespSet)
  val vfDeqRespSetOut = io.wbBusyTableWrite.map(_.vfDeqRespSet)
  val fuBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val intWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val vfWbBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val s0_enqValidVec = io.enq.map(_.valid)
  val s0_enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val s0_enqNotFlush = !io.flush.valid
  val s0_enqBits = WireInit(VecInit(io.enq.map(_.bits)))
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush) //enqValid && notFlush && enqReady


  val finalDeqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val finalDeqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val validVec = VecInit(entries.io.valid.asBools)
  val canIssueVec = VecInit(entries.io.canIssue.asBools)
  val clearVec = VecInit(entries.io.clear.asBools)
  val deqFirstIssueVec = VecInit(entries.io.deq.map(_.isFirstIssue))

  val dataSources: Vec[Vec[DataSource]] = entries.io.dataSources
  val finalDataSources: Vec[Vec[DataSource]] = VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, dataSources)))
  // (entryIdx)(srcIdx)(exuIdx)
  val wakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = entries.io.srcWakeUpL1ExuOH
  val srcTimer: Option[Vec[Vec[UInt]]] = entries.io.srcTimer

  // (deqIdx)(srcIdx)(exuIdx)
  val finalWakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = wakeUpL1ExuOH.map(x => VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, x))))
  val finalSrcTimer = srcTimer.map(x => VecInit(finalDeqSelOHVec.map(oh => Mux1H(oh, x))))

  val fuTypeVec = Wire(Vec(params.numEntries, FuType()))
  val transEntryDeqVec = Wire(Vec(params.numEnq, ValidIO(new EntryBundle)))
  val deqEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val transSelVec = Wire(Vec(params.numEnq, UInt((params.numEntries-params.numEnq).W)))
  val canIssueMergeAllBusy = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val deqCanIssue = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))

  val enqEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt(params.numEnq.W))))
  val othersEntryOldestSel = Wire(Vec(params.numDeq, ValidIO(UInt((params.numEntries - params.numEnq).W))))
  val deqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val deqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val cancelDeqVec = Wire(Vec(params.numDeq, Bool()))

  val subDeqSelValidVec = OptionWrapper(params.deqFuSame, Wire(Vec(params.numDeq, Bool())))
  val subDeqSelOHVec = OptionWrapper(params.deqFuSame, Wire(Vec(params.numDeq, UInt(params.numEntries.W))))
  val subDeqRequest = OptionWrapper(params.deqFuSame, Wire(UInt(params.numEntries.W)))

  /**
    * Connection of [[entries]]
    */
  entries.io match { case entriesIO: EntriesIO =>
    entriesIO.flush <> io.flush
    entriesIO.wakeUpFromWB := io.wakeupFromWB
    entriesIO.wakeUpFromIQ := io.wakeupFromIQ
    entriesIO.og0Cancel := io.og0Cancel
    entriesIO.og1Cancel := io.og1Cancel
    entriesIO.ldCancel := io.ldCancel
    entriesIO.enq.zipWithIndex.foreach { case (enq: ValidIO[EntryBundle], i) =>
      enq.valid := s0_doEnqSelValidVec(i)
      val numLsrc = s0_enqBits(i).srcType.size.min(enq.bits.status.srcType.size)
      for (j <- 0 until numLsrc) {
        enq.bits.status.srcState(j) := s0_enqBits(i).srcState(j) & !LoadShouldCancel(Some(s0_enqBits(i).srcLoadDependency(j)), io.ldCancel)
        enq.bits.status.psrc(j) := s0_enqBits(i).psrc(j)
        enq.bits.status.srcType(j) := s0_enqBits(i).srcType(j)
        enq.bits.status.dataSources(j).value := DataSource.reg
        enq.bits.payload.debugInfo.enqRsTime := GTimer()
      }
      enq.bits.status.fuType := s0_enqBits(i).fuType
      enq.bits.status.robIdx := s0_enqBits(i).robIdx
      enq.bits.status.issueTimer := "b10".U
      enq.bits.status.deqPortIdx := 0.U
      enq.bits.status.issued := false.B
      enq.bits.status.firstIssue := false.B
      enq.bits.status.blocked := false.B

      if (params.hasIQWakeUp) {
        enq.bits.status.srcWakeUpL1ExuOH.get := 0.U.asTypeOf(enq.bits.status.srcWakeUpL1ExuOH.get)
        enq.bits.status.srcTimer.get := 0.U.asTypeOf(enq.bits.status.srcTimer.get)
        enq.bits.status.srcLoadDependency.foreach(_.zipWithIndex.foreach {
          case (dep, srcIdx) =>
            dep := VecInit(s0_enqBits(i).srcLoadDependency(srcIdx).map(x => x(x.getWidth - 2, 0) << 1))
        })
      }
      if (params.inIntSchd && params.AluCnt > 0) {
        // dirty code for lui+addi(w) fusion
        val isLuiAddiFusion = s0_enqBits(i).isLUI32
        val luiImm = Cat(s0_enqBits(i).lsrc(1), s0_enqBits(i).lsrc(0), s0_enqBits(i).imm(ImmUnion.maxLen - 1, 0))
        enq.bits.imm.foreach(_ := Mux(isLuiAddiFusion, ImmUnion.LUI32.toImm32(luiImm), s0_enqBits(i).imm))
      }
      else if (params.inMemSchd && params.LduCnt > 0) {
        // dirty code for fused_lui_load
        val isLuiLoadFusion = SrcType.isNotReg(s0_enqBits(i).srcType(0)) && FuType.isLoad(s0_enqBits(i).fuType)
        enq.bits.imm.foreach(_ := Mux(isLuiLoadFusion, Imm_LUI_LOAD().getLuiImm(s0_enqBits(i)), s0_enqBits(i).imm))
      }
      else {
        enq.bits.imm.foreach(_ := s0_enqBits(i).imm)
      }
      enq.bits.payload := s0_enqBits(i)
    }
    entriesIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.enqEntryOldestSel := enqEntryOldestSel(i)
      deq.othersEntryOldestSel := othersEntryOldestSel(i)
      deq.subDeqRequest.foreach(_ := subDeqRequest.get)
      deq.subDeqSelOH.foreach(_ := subDeqSelOHVec.get(i))
      deq.deqReady := deqBeforeDly(i).ready
      deq.deqSelOH.valid := deqSelValidVec(i)
      deq.deqSelOH.bits := deqSelOHVec(i)
    }
    entriesIO.og0Resp.zipWithIndex.foreach { case (og0Resp, i) =>
      og0Resp.valid := io.og0Resp(i).valid
      og0Resp.bits.robIdx := io.og0Resp(i).bits.robIdx
      og0Resp.bits.dataInvalidSqIdx := io.og0Resp(i).bits.dataInvalidSqIdx
      og0Resp.bits.respType := io.og0Resp(i).bits.respType
      og0Resp.bits.rfWen := io.og0Resp(i).bits.rfWen
      og0Resp.bits.fuType := io.og0Resp(i).bits.fuType
    }
    entriesIO.og1Resp.zipWithIndex.foreach { case (og1Resp, i) =>
      og1Resp.valid := io.og1Resp(i).valid
      og1Resp.bits.robIdx := io.og1Resp(i).bits.robIdx
      og1Resp.bits.dataInvalidSqIdx := io.og1Resp(i).bits.dataInvalidSqIdx
      og1Resp.bits.respType := io.og1Resp(i).bits.respType
      og1Resp.bits.rfWen := io.og1Resp(i).bits.rfWen
      og1Resp.bits.fuType := io.og1Resp(i).bits.fuType
    }
    entriesIO.finalIssueResp.foreach(_.zipWithIndex.foreach { case (finalIssueResp, i) =>
      finalIssueResp := io.finalIssueResp.get(i)
    })
    transEntryDeqVec := entriesIO.transEntryDeqVec
    deqEntryVec := entriesIO.deq.map(_.deqEntry)
    fuTypeVec := entriesIO.fuType
    cancelDeqVec := entriesIO.cancelDeqVec
    transSelVec := entriesIO.transSelVec
  }


  s0_enqSelValidVec := s0_enqValidVec.zip(io.enq).map{ case (enqValid, enq) => enqValid && enq.ready}

  protected val commonAccept: UInt = Cat(fuTypeVec.map(fuType =>
    FuType.FuTypeOrR(fuType, commonFuCfgs.map(_.fuType))
  ).reverse)

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeVec.map(fuType => 
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType))
    ).reverse)
  }

  protected val deqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    fuTypeVec.map(fuType =>
      FuType.FuTypeOrR(fuType, fuCfgs.map(_.fuType)))
  }

  canIssueMergeAllBusy.zipWithIndex.foreach { case (merge, i) => 
    val mergeFuBusy = {
      if (fuBusyTableWrite(i).nonEmpty) canIssueVec.asUInt & (~fuBusyTableMask(i))
      else canIssueVec.asUInt
    }
    val mergeIntWbBusy = {
      if (intWbBusyTableRead(i).nonEmpty) mergeFuBusy & (~intWbBusyTableMask(i))
      else mergeFuBusy
    }
    val mergeVfWbBusy = {
      if (vfWbBusyTableRead(i).nonEmpty) mergeIntWbBusy & (~vfWbBusyTableMask(i))
      else mergeIntWbBusy
    }
    merge := mergeVfWbBusy
  }

  deqCanIssue.zipWithIndex.foreach { case (req, i) =>
    req := canIssueMergeAllBusy(i) & VecInit(deqCanAcceptVec(i)).asUInt
  }

  if (params.numDeq == 2) {
    require(params.deqFuSame || params.deqFuDiff, "The 2 deq ports need to be identical or completely different")
  }

  if (params.numDeq == 2 && params.deqFuSame) {
    enqEntryOldestSel := DontCare

    othersEntryOldestSel(0) := AgeDetector(numEntries = params.numEntries - params.numEnq,
      enq = VecInit(transEntryDeqVec.zip(transSelVec).map{ case (transEntry, transSel) => Fill(params.numEntries-params.numEnq, transEntry.valid) & transSel }),
      canIssue = canIssueVec.asUInt(params.numEntries-1, params.numEnq)
    )
    othersEntryOldestSel(1) := DontCare

    subDeqRequest.get := canIssueVec.asUInt & ~Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W))

    val subDeqPolicy = Module(new DeqPolicy())
    subDeqPolicy.io.request := subDeqRequest.get
    subDeqSelValidVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.valid)
    subDeqSelOHVec.get := subDeqPolicy.io.deqSelOHVec.map(oh => oh.bits)

    deqSelValidVec(0) := othersEntryOldestSel(0).valid || subDeqSelValidVec.get(1)
    deqSelValidVec(1) := subDeqSelValidVec.get(0)
    deqSelOHVec(0) := Mux(othersEntryOldestSel(0).valid, 
                          Cat(othersEntryOldestSel(0).bits, 0.U((params.numEnq).W)), 
                          subDeqSelOHVec.get(1)) & canIssueMergeAllBusy(0)
    deqSelOHVec(1) := subDeqSelOHVec.get(0) & canIssueMergeAllBusy(1)

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach { case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid && deqOH.orR && deqBeforeDly(i).ready
      selOH := deqOH
    }
  }
  else {
    enqEntryOldestSel := NewAgeDetector(numEntries = params.numEnq,
      enq = VecInit(s0_doEnqSelValidVec),
      canIssue = VecInit(deqCanIssue.map(_(params.numEnq-1, 0)))
    )

    othersEntryOldestSel := AgeDetector(numEntries = params.numEntries - params.numEnq,
      enq = VecInit(transEntryDeqVec.zip(transSelVec).map{ case (transEntry, transSel) => Fill(params.numEntries-params.numEnq, transEntry.valid) & transSel }),
      canIssue = VecInit(deqCanIssue.map(_(params.numEntries-1, params.numEnq)))
    )

    deqSelValidVec.zip(deqSelOHVec).zipWithIndex.foreach { case ((selValid, selOH), i) =>
      selValid := othersEntryOldestSel(i).valid || enqEntryOldestSel(i).valid
      selOH := Cat(othersEntryOldestSel(i).bits, Fill(params.numEnq, enqEntryOldestSel(i).valid && !othersEntryOldestSel(i).valid) & enqEntryOldestSel(i).bits)
    }

    finalDeqSelValidVec.zip(finalDeqSelOHVec).zip(deqSelValidVec).zip(deqSelOHVec).zipWithIndex.foreach { case ((((selValid, selOH), deqValid), deqOH), i) =>
      selValid := deqValid && deqBeforeDly(i).ready
      selOH := deqOH
    }
  }

  val toBusyTableDeqResp = Wire(Vec(params.numDeq, ValidIO(new IssueQueueDeqRespBundle)))

  toBusyTableDeqResp.zipWithIndex.foreach { case (deqResp, i) =>
    deqResp.valid := finalDeqSelValidVec(i)
    deqResp.bits.respType := RSFeedbackType.issueSuccess
    deqResp.bits.robIdx := DontCare
    deqResp.bits.dataInvalidSqIdx := DontCare
    deqResp.bits.rfWen := DontCare
    deqResp.bits.fuType := deqBeforeDly(i).bits.common.fuType
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

  wakeUpQueues.zipWithIndex.foreach { case (wakeUpQueueOption, i) =>
    val og0RespEach = io.og0Resp(i)
    val og1RespEach = io.og1Resp(i)
    wakeUpQueueOption.foreach {
      wakeUpQueue =>
        val flush = Wire(new WakeupQueueFlush)
        flush.redirect := io.flush
        flush.ldCancel := io.ldCancel
        flush.og0Fail := io.og0Resp(i).valid && RSFeedbackType.isBlocked(io.og0Resp(i).bits.respType)
        flush.og1Fail := io.og1Resp(i).valid && RSFeedbackType.isBlocked(io.og1Resp(i).bits.respType)
        flush.finalFail := io.finalBlock(i)
        wakeUpQueue.io.flush := flush
        wakeUpQueue.io.enq.valid := deqBeforeDly(i).fire && {
          if (deqBeforeDly(i).bits.common.rfWen.isDefined)
            deqBeforeDly(i).bits.common.rfWen.get && deqBeforeDly(i).bits.common.pdest =/= 0.U
          else
            true.B
        }
        wakeUpQueue.io.enq.bits.uop :<= deqBeforeDly(i).bits.common
        wakeUpQueue.io.enq.bits.uop.pdestCopy.foreach(_ := 0.U)
        wakeUpQueue.io.enq.bits.lat := getDeqLat(i, deqBeforeDly(i).bits.common.fuType)
        wakeUpQueue.io.og0IssueFail := flush.og0Fail
        wakeUpQueue.io.og1IssueFail := flush.og1Fail
    }
  }

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i) && !cancelDeqVec(i)
    deq.bits.addrOH          := finalDeqSelOHVec(i)
    deq.bits.common.isFirstIssue := deqFirstIssueVec(i)
    deq.bits.common.iqIdx    := OHToUInt(finalDeqSelOHVec(i))
    deq.bits.common.fuType   := deqEntryVec(i).bits.status.fuType
    deq.bits.common.fuOpType := deqEntryVec(i).bits.payload.fuOpType
    deq.bits.common.rfWen.foreach(_ := deqEntryVec(i).bits.payload.rfWen)
    deq.bits.common.fpWen.foreach(_ := deqEntryVec(i).bits.payload.fpWen)
    deq.bits.common.vecWen.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
    deq.bits.common.flushPipe.foreach(_ := deqEntryVec(i).bits.payload.flushPipe)
    deq.bits.common.pdest := deqEntryVec(i).bits.payload.pdest
    deq.bits.common.robIdx := deqEntryVec(i).bits.status.robIdx
    deq.bits.common.dataSources := finalDataSources(i)
    deq.bits.common.l1ExuVec.foreach(_ := finalWakeUpL1ExuOH.get(i))
    deq.bits.common.srcTimer.foreach(_ := finalSrcTimer.get(i))
    deq.bits.common.loadDependency.foreach(_ := deqEntryVec(i).bits.status.mergedLoadDependency.get)
    deq.bits.common.deqPortIdx.foreach(_ := i.U)
    deq.bits.common.src := DontCare

    deq.bits.rf.zip(deqEntryVec(i).bits.status.psrc).zip(deqEntryVec(i).bits.status.srcType).foreach { case ((rf, psrc), srcType) =>
      // psrc in status array can be pregIdx of IntRegFile or VfRegFile
      rf.foreach(_.addr := psrc)
      rf.foreach(_.srcType := srcType)
    }
    deq.bits.srcType.zip(deqEntryVec(i).bits.status.srcType).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.immType := deqEntryVec(i).bits.payload.selImm
    deq.bits.common.imm := deqEntryVec(i).bits.imm.getOrElse(0.U)

    deq.bits.common.perfDebugInfo := deqEntryVec(i).bits.payload.debugInfo
    deq.bits.common.perfDebugInfo.selectTime := GTimer()
    deq.bits.common.perfDebugInfo.issueTime := GTimer() + 1.U
  }

  private val deqShift = WireDefault(deqBeforeDly)
  deqShift.zip(deqBeforeDly).foreach {
    case (shifted, original) =>
      original.ready := shifted.ready // this will not cause combinational loop
      shifted.bits.common.loadDependency.foreach(
        _ := original.bits.common.loadDependency.get.map(_ << 1)
      )
  }
  io.deqDelay.zip(deqShift).foreach { case (deqDly, deq) =>
    NewPipelineConnect(
      deq, deqDly, deqDly.valid,
      false.B,
      Option("Scheduler2DataPathPipe")
    )
  }
  if(backendParams.debugEn) {
    dontTouch(io.deqDelay)
  }
  io.wakeupToIQ.zipWithIndex.foreach { case (wakeup, i) =>
    if (wakeUpQueues(i).nonEmpty && finalWakeUpL1ExuOH.nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits, finalWakeUpL1ExuOH.get(i))
      wakeup.bits.loadDependency := wakeUpQueues(i).get.io.deq.bits.loadDependency.getOrElse(0.U.asTypeOf(wakeup.bits.loadDependency))
      wakeup.bits.is0Lat := getDeqLat(i, wakeUpQueues(i).get.io.deq.bits.fuType) === 0.U
    } else if (wakeUpQueues(i).nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits)
      wakeup.bits.loadDependency := wakeUpQueues(i).get.io.deq.bits.loadDependency.getOrElse(0.U.asTypeOf(wakeup.bits.loadDependency))
      wakeup.bits.is0Lat := getDeqLat(i, wakeUpQueues(i).get.io.deq.bits.fuType) === 0.U
    } else {
      wakeup.valid := false.B
      wakeup.bits := 0.U.asTypeOf(wakeup.bits)
      wakeup.bits.is0Lat :=  0.U
    }
    if(wakeup.bits.pdestCopy.nonEmpty){
      wakeup.bits.pdestCopy.get := wakeUpQueues(i).get.io.deq.bits.pdestCopy.get
    }
  }

  // Todo: better counter implementation
  private val enqHasValid = validVec.take(params.numEnq).reduce(_ | _)
  private val enqEntryValidCnt = PopCount(validVec.take(params.numEnq))
  private val othersValidCnt = PopCount(validVec.drop(params.numEnq))
  io.status.leftVec(0) := validVec.drop(params.numEnq).reduce(_ & _)
  for (i <- 0 until params.numEnq) {
    io.status.leftVec(i + 1) := othersValidCnt === (params.numEntries - params.numEnq - (i + 1)).U
  }
  private val othersLeftOneCaseVec = Wire(Vec(params.numEntries - params.numEnq, UInt((params.numEntries - params.numEnq).W)))
  othersLeftOneCaseVec.zipWithIndex.foreach { case (leftone, i) =>
    leftone := ~(1.U((params.numEntries - params.numEnq).W) << i)
  }
  private val othersLeftOne = othersLeftOneCaseVec.map(_ === VecInit(validVec.drop(params.numEnq)).asUInt).reduce(_ | _)
  private val othersCanotIn = othersLeftOne || validVec.drop(params.numEnq).reduce(_ & _)

  io.enq.foreach(_.ready := !othersCanotIn || !enqHasValid)
  io.status.empty := !Cat(validVec).orR
  io.status.full := othersCanotIn

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    val fuLatUIntMaps: Map[Int, UInt] = fuLatencyMaps(deqPortIdx).map { case (k, v) => (k.id, v.U) }
    val lat = WireInit(Mux1H(fuLatUIntMaps.keys.map(fuType(_)).toSeq, fuLatUIntMaps.values.toSeq))
    dontTouch(lat)
  }

  // issue perf counter
  // enq count
  XSPerfAccumulate("enq_valid_cnt", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_fire_cnt", PopCount(io.enq.map(_.fire)))
  // valid count
  XSPerfHistogram("enq_entry_valid_cnt", enqEntryValidCnt, true.B, 0, params.numEnq + 1)
  XSPerfHistogram("other_entry_valid_cnt", othersValidCnt, true.B, 0, params.numEntries - params.numEnq + 1)
  XSPerfHistogram("valid_cnt", PopCount(validVec), true.B, 0, params.numEntries + 1)
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
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_bypass", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_forward", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_noreg", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))

  XSPerfHistogram("issue_datasource_reg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_bypass_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_forward_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_noreg_hist", deqBeforeDly.map{ deq =>
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)

  // deq instr data source count for each futype
  for (t <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(t)
    if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
      XSPerfAccumulate(s"issue_datasource_reg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_bypass_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_forward_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_noreg_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))

      XSPerfHistogram(s"issue_datasource_reg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_bypass_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_forward_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_noreg_hist_futype_${fuName}", deqBeforeDly.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
    }
  }

  // cancel instr count
  if (params.hasIQWakeUp) {
    val cancelVec: Vec[Bool] = entries.io.cancel.get
    XSPerfAccumulate("cancel_instr_count", PopCount(validVec.zip(cancelVec).map(x => x._1 & x._2)))
    XSPerfHistogram("cancel_instr_hist", PopCount(validVec.zip(cancelVec).map(x => x._1 & x._2)), true.B, 0, params.numEntries, 1)
    for (t <- FuType.functionNameMap.keys) {
      val fuName = FuType.functionNameMap(t)
      if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
        XSPerfAccumulate(s"cancel_instr_count_futype_${fuName}", PopCount(validVec.zip(cancelVec).zip(fuTypeVec).map{ case ((x, y), fu) => x & y & fu === t.U }))
        XSPerfHistogram(s"cancel_instr_hist_futype_${fuName}", PopCount(validVec.zip(cancelVec).zip(fuTypeVec).map{ case ((x, y), fu) => x & y & fu === t.U }), true.B, 0, params.numEntries, 1)
      }
    }
  }
}

class IssueQueueJumpBundle extends Bundle {
  val pc = UInt(VAddrData().dataWidth.W)
}

class IssueQueueLoadBundle(implicit p: Parameters) extends XSBundle {
  val fastMatch = UInt(backendParams.LduCnt.W)
  val fastImm = UInt(12.W)
}

class IssueQueueIntIO()(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO

class IssueQueueIntImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  io.suggestName("none")
  override lazy val io = IO(new IssueQueueIntIO).suggestName("io")

  deqBeforeDly.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.pc.foreach(_ := deqEntryVec(i).bits.payload.pc)
    deq.bits.common.preDecode.foreach(_ := deqEntryVec(i).bits.payload.preDecodeInfo)
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
    deq.bits.common.predictInfo.foreach(x => {
      x.target := DontCare
      x.taken := deqEntryVec(i).bits.payload.pred_taken
    })
    // for std
    deq.bits.common.sqIdx.foreach(_ := deqEntryVec(i).bits.payload.sqIdx)
    // for i2f
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
  }}
}

class IssueQueueVfImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  s0_enqBits.foreach{ x =>
    x.srcType(3) := SrcType.vp // v0: mask src
    x.srcType(4) := SrcType.vp // vl&vtype
  }
  deqBeforeDly.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
    deq.bits.common.vpu.foreach(_ := deqEntryVec(i).bits.payload.vpu)
    deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
  }}
}

class IssueQueueMemBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
  val feedbackIO = Flipped(Vec(params.numDeq, new MemRSFeedbackIO))
  val checkWait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
  val loadFastMatch = Output(Vec(params.LduCnt, new IssueQueueLoadBundle))
}

class IssueQueueMemIO(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO {
  val memIO = Some(new IssueQueueMemBundle)
}

class IssueQueueMemAddrImp(override val wrapper: IssueQueue)(implicit p: Parameters, params: IssueBlockParams)
  extends IssueQueueImp(wrapper) with HasCircularQueuePtrHelper {

  require(params.StdCnt == 0 && (params.LduCnt + params.StaCnt + params.VlduCnt) > 0, "IssueQueueMemAddrImp can only be instance of MemAddr IQ")

  io.suggestName("none")
  override lazy val io = IO(new IssueQueueMemIO).suggestName("io")
  private val memIO = io.memIO.get

  memIO.loadFastMatch := 0.U.asTypeOf(memIO.loadFastMatch) // TODO: is still needed?

  for (i <- io.enq.indices) {
    s0_enqBits(i).loadWaitBit := false.B
  }

  for (i <- entries.io.enq.indices) {
    entries.io.enq(i).bits.status match { case enqData =>
      enqData.blocked := false.B // s0_enqBits(i).loadWaitBit
      enqData.mem.get.strictWait := s0_enqBits(i).loadWaitStrict
      enqData.mem.get.waitForStd := false.B
      enqData.mem.get.waitForRobIdx := s0_enqBits(i).waitForRobIdx
      enqData.mem.get.waitForSqIdx := 0.U.asTypeOf(enqData.mem.get.waitForSqIdx) // generated by sq, will be updated later
      enqData.mem.get.sqIdx := s0_enqBits(i).sqIdx
    }

    entries.io.fromMem.get.slowResp.zipWithIndex.foreach { case (slowResp, i) =>
      slowResp.valid                 := memIO.feedbackIO(i).feedbackSlow.valid
      slowResp.bits.robIdx           := memIO.feedbackIO(i).feedbackSlow.bits.robIdx
      slowResp.bits.respType         := Mux(memIO.feedbackIO(i).feedbackSlow.bits.hit, RSFeedbackType.fuIdle, RSFeedbackType.feedbackInvalid)
      slowResp.bits.dataInvalidSqIdx := memIO.feedbackIO(i).feedbackSlow.bits.dataInvalidSqIdx
      slowResp.bits.rfWen := DontCare
      slowResp.bits.fuType := DontCare
    }

    entries.io.fromMem.get.fastResp.zipWithIndex.foreach { case (fastResp, i) =>
      fastResp.valid                 := memIO.feedbackIO(i).feedbackFast.valid
      fastResp.bits.robIdx           := memIO.feedbackIO(i).feedbackFast.bits.robIdx
      fastResp.bits.respType         := memIO.feedbackIO(i).feedbackFast.bits.sourceType
      fastResp.bits.dataInvalidSqIdx := 0.U.asTypeOf(fastResp.bits.dataInvalidSqIdx)
      fastResp.bits.rfWen := DontCare
      fastResp.bits.fuType := DontCare
    }

    entries.io.fromMem.get.memWaitUpdateReq := memIO.checkWait.memWaitUpdateReq
    entries.io.fromMem.get.stIssuePtr := memIO.checkWait.stIssuePtr
  }

  deqBeforeDly.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.sqIdx.get := deqEntryVec(i).bits.payload.sqIdx
    deq.bits.common.lqIdx.get := deqEntryVec(i).bits.payload.lqIdx
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
  }
}