package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{GTimer, HasCircularQueuePtrHelper}
import utils._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.{ImmUnion, Imm_LUI_LOAD}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr, LqPtr}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.datapath.NewPipelineConnect

class IssueQueue(params: IssueBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  implicit val iqParams = params
  lazy val module: IssueQueueImp = iqParams.schdType match {
    case IntScheduler() => new IssueQueueIntImp(this)
    case VfScheduler() => new IssueQueueVfImp(this)
    case MemScheduler() =>
      if (iqParams.StdCnt == 0 && !iqParams.isVecMemIQ) new IssueQueueMemAddrImp(this)
      else if (iqParams.isVecMemIQ) new IssueQueueVecMemImp(this)
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

  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
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

  // Outputs
  val deq: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle
  val wakeupToIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpSourceValidBundle
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  // val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))

  val fromCancelNetwork = Flipped(params.genIssueDecoupledBundle)
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
  dontTouch(io.deq)
  dontTouch(io.deqResp)
  // Modules

  val entries = Module(new Entries)
  val subDeqPolicies  = deqFuCfgs.map(x => if (x.nonEmpty) Some(Module(new DeqPolicy())) else None)
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

  val wakeUpQueues: Seq[Option[MultiWakeupQueue[ExuInput, WakeupQueueFlush]]] = params.exuBlockParams.map { x => OptionWrapper(x.isIQWakeUpSource, Module(
    new MultiWakeupQueue(new ExuInput(x), new WakeupQueueFlush, x.fuLatancySet, flushFunc, modificationFunc)
  ))}

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


  // One deq port only need one special deq policy
  val subDeqSelValidVec: Seq[Option[Vec[Bool]]] = subDeqPolicies.map(_.map(_ => Wire(Vec(params.numDeq, Bool()))))
  val subDeqSelOHVec: Seq[Option[Vec[UInt]]] = subDeqPolicies.map(_.map(_ => Wire(Vec(params.numDeq, UInt(params.numEntries.W)))))

  val finalDeqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val finalDeqSelOHVec    = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val finalDeqOH: IndexedSeq[UInt] = (finalDeqSelValidVec zip finalDeqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }
  val finalDeqMask: UInt = finalDeqOH.reduce(_ | _)

  val deqRespVec = io.deqResp

  val validVec = VecInit(entries.io.valid.asBools)
  val canIssueVec = VecInit(entries.io.canIssue.asBools)
  val clearVec = VecInit(entries.io.clear.asBools)
  val deqFirstIssueVec = VecInit(entries.io.deq.map(_.isFirstIssue))

  val dataSources: Vec[Vec[DataSource]] = entries.io.dataSources
  val finalDataSources: Vec[Vec[DataSource]] = VecInit(finalDeqOH.map(oh => Mux1H(oh, dataSources)))
  // (entryIdx)(srcIdx)(exuIdx)
  val wakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = entries.io.srcWakeUpL1ExuOH
  val srcTimer: Option[Vec[Vec[UInt]]] = entries.io.srcTimer

  // (deqIdx)(srcIdx)(exuIdx)
  val finalWakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = wakeUpL1ExuOH.map(x => VecInit(finalDeqOH.map(oh => Mux1H(oh, x))))
  val finalSrcTimer = srcTimer.map(x => VecInit(finalDeqOH.map(oh => Mux1H(oh, x))))

  val wakeupEnqSrcStateBypassFromWB: Vec[Vec[UInt]] = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  val wakeupEnqSrcStateBypassFromIQ: Vec[Vec[UInt]] = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  val srcWakeUpEnqByIQMatrix = Wire(Vec(params.numEnq, Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))))

  val shiftedWakeupLoadDependencyByIQVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  shiftedWakeupLoadDependencyByIQVec
    .zip(io.wakeupFromIQ.map(_.bits.loadDependency))
    .zip(params.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
          dep := (originalDep << 1).asUInt | 1.U
        else
          dep := originalDep << 1
    }
  }

  for (i <- io.enq.indices) {
    for (j <- s0_enqBits(i).srcType.indices) {
      wakeupEnqSrcStateBypassFromWB(i)(j) := Cat(
        io.wakeupFromWB.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head).toSeq
      ).orR
    }
  }

  for (i <- io.enq.indices) {
    val numLsrc = s0_enqBits(i).srcType.size.min(entries.io.enq(i).bits.status.srcType.size)
    for (j <- s0_enqBits(i).srcType.indices) {
      val ldTransCancel = if (params.numWakeupFromIQ > 0 && j < numLsrc) Mux(
        srcWakeUpEnqByIQMatrix(i)(j).asUInt.orR,
        Mux1H(srcWakeUpEnqByIQMatrix(i)(j), io.wakeupFromIQ.map(_.bits.loadDependency).map(dep => LoadShouldCancel(Some(dep), io.ldCancel)).toSeq),
        false.B
      ) else false.B
      wakeupEnqSrcStateBypassFromIQ(i)(j) := Cat(
        io.wakeupFromIQ.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head).toSeq
      ).orR && !ldTransCancel
    }
  }

  srcWakeUpEnqByIQMatrix.zipWithIndex.foreach { case (wakeups: Vec[Vec[Bool]], i) =>
    if (io.wakeupFromIQ.isEmpty) {
      wakeups := 0.U.asTypeOf(wakeups)
    } else {
      val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.wakeupFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
        bundle.bits.wakeUp(s0_enqBits(i).psrc.take(params.numRegSrc) zip s0_enqBits(i).srcType.take(params.numRegSrc), bundle.valid)
      ).toIndexedSeq.transpose
      wakeups := wakeupVec.map(x => VecInit(x))
    }
  }

  val fuTypeVec = Wire(Vec(params.numEntries, FuType()))
  val transEntryDeqVec = Wire(Vec(params.numEnq, ValidIO(new EntryBundle)))
  val deqEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val transSelVec = Wire(Vec(params.numEnq, UInt((params.numEntries-params.numEnq).W)))

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
      for(j <- 0 until numLsrc) {
        enq.bits.status.srcState(j) := s0_enqBits(i).srcState(j) |
                                       wakeupEnqSrcStateBypassFromWB(i)(j) |
                                       wakeupEnqSrcStateBypassFromIQ(i)(j)
        enq.bits.status.psrc(j) := s0_enqBits(i).psrc(j)
        enq.bits.status.srcType(j) := s0_enqBits(i).srcType(j)
        enq.bits.status.dataSources(j).value := Mux(wakeupEnqSrcStateBypassFromIQ(i)(j).asBool, DataSource.forward, s0_enqBits(i).dataSource(j).value)
        enq.bits.payload.debugInfo.enqRsTime := GTimer()
      }
      enq.bits.status.fuType := s0_enqBits(i).fuType
      enq.bits.status.robIdx := s0_enqBits(i).robIdx
      enq.bits.status.uopIdx.foreach(_ := s0_enqBits(i).uopIdx)
      enq.bits.status.issueTimer := "b11".U
      enq.bits.status.deqPortIdx := 0.U
      enq.bits.status.issued := false.B
      enq.bits.status.firstIssue := false.B
      enq.bits.status.blocked := false.B
      enq.bits.status.srcWakeUpL1ExuOH match {
        case Some(value) => value.zip(srcWakeUpEnqByIQMatrix(i)).zipWithIndex.foreach {
          case ((exuOH, wakeUpByIQOH), srcIdx) =>
            when(wakeUpByIQOH.asUInt.orR) {
              exuOH := Mux1H(wakeUpByIQOH, io.wakeupFromIQ.map(x => MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W)).toSeq).asBools
            }.otherwise {
              exuOH := s0_enqBits(i).l1ExuOH(srcIdx)
            }
        }
        case None =>
      }
      enq.bits.status.srcTimer match {
        case Some(value) => value.zip(srcWakeUpEnqByIQMatrix(i)).zipWithIndex.foreach {
          case ((timer, wakeUpByIQOH), srcIdx) =>
            when(wakeUpByIQOH.asUInt.orR) {
              timer := 1.U.asTypeOf(timer)
            }.otherwise {
              timer := Mux(s0_enqBits(i).dataSource(srcIdx).value === DataSource.bypass, 2.U.asTypeOf(timer), 0.U.asTypeOf(timer))
            }
        }
        case None =>
      }
      enq.bits.status.srcLoadDependency.foreach(_.zip(srcWakeUpEnqByIQMatrix(i)).zipWithIndex.foreach {
        case ((dep, wakeUpByIQOH), srcIdx) =>
          dep := Mux(wakeUpByIQOH.asUInt.orR, Mux1H(wakeUpByIQOH, shiftedWakeupLoadDependencyByIQVec), 0.U.asTypeOf(dep))
      })
      enq.bits.imm := s0_enqBits(i).imm
      enq.bits.payload := s0_enqBits(i)
    }
    entriesIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.deqSelOH.valid := finalDeqSelValidVec(i)
      deq.deqSelOH.bits := finalDeqSelOHVec(i)
    }
    entriesIO.deqResp.zipWithIndex.foreach { case (deqResp, i) =>
      deqResp.valid := io.deqResp(i).valid
      deqResp.bits.robIdx := io.deqResp(i).bits.robIdx
      deqResp.bits.dataInvalidSqIdx := io.deqResp(i).bits.dataInvalidSqIdx
      deqResp.bits.respType := io.deqResp(i).bits.respType
      deqResp.bits.rfWen := io.deqResp(i).bits.rfWen
      deqResp.bits.fuType := io.deqResp(i).bits.fuType
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
    deqEntryVec := entriesIO.deqEntry
    fuTypeVec := entriesIO.fuType
    transSelVec := entriesIO.transSelVec
  }


  s0_enqSelValidVec := s0_enqValidVec.zip(io.enq).map{ case (enqValid, enq) => enqValid && enq.ready}

  protected val commonAccept: UInt = Cat(fuTypeVec.map(fuType =>
    Cat(commonFuCfgs.map(_.fuType.U === fuType)).orR
  ).reverse)

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeVec.map(fuType => Cat(fuCfgs.map(_.fuType.U === fuType)).orR).reverse).asUInt
  }

  protected val deqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    fuTypeVec.map(fuType =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR) // C+E0    C+E1
  }

  subDeqPolicies.zipWithIndex.foreach { case (dpOption: Option[DeqPolicy], i) =>
    if (dpOption.nonEmpty) {
      val dp = dpOption.get
      dp.io.request             := canIssueVec.asUInt & VecInit(deqCanAcceptVec(i)).asUInt & (~fuBusyTableMask(i)).asUInt & (~intWbBusyTableMask(i)).asUInt & (~vfWbBusyTableMask(i)).asUInt
      subDeqSelValidVec(i).get  := dp.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec(i).get     := dp.io.deqSelOHVec.map(oh => oh.bits)
    }
  }

  protected val enqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    io.enq.map(_.bits.fuType).map(fuType =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR) // C+E0    C+E1
  }

  protected val transCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    transEntryDeqVec.map(_.bits.status.fuType).zip(transEntryDeqVec.map(_.valid)).map{ case (fuType, valid) =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR && valid }
  }

  val enqEntryOldest = (0 until params.numDeq).map {
    case deqIdx =>
      NewAgeDetector(numEntries = params.numEnq,
        enq = VecInit(enqCanAcceptVec(deqIdx).zip(s0_doEnqSelValidVec).map{ case (doCanAccept, valid) => doCanAccept && valid }),
        clear = VecInit(clearVec.take(params.numEnq)),
        canIssue = VecInit(canIssueVec.take(params.numEnq)).asUInt & ((~fuBusyTableMask(deqIdx)).asUInt & (~intWbBusyTableMask(deqIdx)).asUInt & (~vfWbBusyTableMask(deqIdx)).asUInt)(params.numEnq-1, 0)
      )
  }

  val othersEntryOldest = (0 until params.numDeq).map {
    case deqIdx =>
      AgeDetector(numEntries = params.numEntries - params.numEnq,
        enq = VecInit(transCanAcceptVec(deqIdx).zip(transSelVec).map{ case(doCanAccept, transSel) => Mux(doCanAccept, transSel, 0.U)}),
        deq = VecInit(clearVec.drop(params.numEnq)).asUInt,
        canIssue = VecInit(canIssueVec.drop(params.numEnq)).asUInt & ((~fuBusyTableMask(deqIdx)).asUInt & (~intWbBusyTableMask(deqIdx)).asUInt & (~vfWbBusyTableMask(deqIdx)).asUInt)(params.numEntries-1, params.numEnq)
      )
  }

  finalDeqSelValidVec.head := othersEntryOldest.head.valid || enqEntryOldest.head.valid || subDeqSelValidVec.head.getOrElse(Seq(false.B)).head
  finalDeqSelOHVec.head := Mux(othersEntryOldest.head.valid, Cat(othersEntryOldest.head.bits, 0.U((params.numEnq).W)),
                            Mux(enqEntryOldest.head.valid, Cat(0.U((params.numEntries-params.numEnq).W), enqEntryOldest.head.bits),
                              subDeqSelOHVec.head.getOrElse(Seq(0.U)).head))

  if (params.numDeq == 2) {
    val chooseOthersOldest = othersEntryOldest(1).valid && Cat(othersEntryOldest(1).bits, 0.U((params.numEnq).W)) =/= finalDeqSelOHVec.head
    val chooseEnqOldest = enqEntryOldest(1).valid && Cat(0.U((params.numEntries-params.numEnq).W), enqEntryOldest(1).bits) =/= finalDeqSelOHVec.head
    val choose1stSub = subDeqSelOHVec(1).getOrElse(Seq(0.U)).head =/= finalDeqSelOHVec.head

    finalDeqSelValidVec(1) := MuxCase(subDeqSelValidVec(1).getOrElse(Seq(false.B)).last, Seq(
      (chooseOthersOldest) -> othersEntryOldest(1).valid,
      (chooseEnqOldest) -> enqEntryOldest(1).valid,
      (choose1stSub) -> subDeqSelValidVec(1).getOrElse(Seq(false.B)).head)
    )
    finalDeqSelOHVec(1) := MuxCase(subDeqSelOHVec(1).getOrElse(Seq(0.U)).last, Seq(
      (chooseOthersOldest) -> Cat(othersEntryOldest(1).bits, 0.U((params.numEnq).W)),
      (chooseEnqOldest) -> Cat(0.U((params.numEntries-params.numEnq).W), enqEntryOldest(1).bits),
      (choose1stSub) -> subDeqSelOHVec(1).getOrElse(Seq(0.U)).head)
    )
  }

  //fuBusyTable
  fuBusyTableWrite.zip(fuBusyTableRead).zipWithIndex.foreach { case ((busyTableWrite: Option[FuBusyTableWrite], busyTableRead: Option[FuBusyTableRead]), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val btrd = busyTableRead.get
      btwr.io.in.deqResp := io.deqResp(i)
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
      btwr.io.in.deqResp := io.deqResp(i)
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
      btwr.io.in.deqResp := io.deqResp(i)
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
        wakeUpQueue.io.flush := flush
        wakeUpQueue.io.enq.valid := io.deq(i).fire && !io.deq(i).bits.common.needCancel(io.og0Cancel, io.og1Cancel) && {
          io.deq(i).bits.common.rfWen.getOrElse(false.B) && io.deq(i).bits.common.pdest =/= 0.U ||
          io.deq(i).bits.common.fpWen.getOrElse(false.B) ||
          io.deq(i).bits.common.vecWen.getOrElse(false.B)
        }
        wakeUpQueue.io.enq.bits.uop := io.deq(i).bits.common
        wakeUpQueue.io.enq.bits.lat := getDeqLat(i, io.deq(i).bits.common.fuType)
        wakeUpQueue.io.og0IssueFail := flush.og0Fail
        wakeUpQueue.io.og1IssueFail := flush.og1Fail
    }
  }

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i)
    deq.bits.addrOH          := finalDeqSelOHVec(i)
    deq.bits.common.isFirstIssue := deqFirstIssueVec(i)
    deq.bits.common.iqIdx    := OHToUInt(finalDeqSelOHVec(i))
    deq.bits.common.fuType   := deqEntryVec(i).bits.payload.fuType
    deq.bits.common.fuOpType := deqEntryVec(i).bits.payload.fuOpType
    deq.bits.common.rfWen.foreach(_ := deqEntryVec(i).bits.payload.rfWen)
    deq.bits.common.fpWen.foreach(_ := deqEntryVec(i).bits.payload.fpWen)
    deq.bits.common.vecWen.foreach(_ := deqEntryVec(i).bits.payload.vecWen)
    deq.bits.common.flushPipe.foreach(_ := deqEntryVec(i).bits.payload.flushPipe)
    deq.bits.common.pdest := deqEntryVec(i).bits.payload.pdest
    deq.bits.common.robIdx := deqEntryVec(i).bits.payload.robIdx
    deq.bits.common.imm := deqEntryVec(i).bits.imm
    deq.bits.common.dataSources.zip(finalDataSources(i)).zipWithIndex.foreach {
      case ((sink, source), srcIdx) =>
        sink.value := Mux(
          SrcType.isXp(deqEntryVec(i).bits.payload.srcType(srcIdx)) && deqEntryVec(i).bits.payload.psrc(srcIdx) === 0.U,
          DataSource.none,
          source.value
        )
    }
    if(params.hasIQWakeUp) {
      deq.bits.common.l1ExuVec := finalWakeUpL1ExuOH.get(i)
    } else {
      deq.bits.common.l1ExuVec := deqEntryVec(i).bits.payload.l1ExuOH.take(deq.bits.common.l1ExuVec.length)
    }
    deq.bits.common.srcTimer.foreach(_ := finalSrcTimer.get(i))
    deq.bits.common.loadDependency.foreach(_ := deqEntryVec(i).bits.status.mergedLoadDependency.get)
    deq.bits.common.deqPortIdx.foreach(_ := i.U)
    deq.bits.common.src := DontCare

    deq.bits.rf.zip(deqEntryVec(i).bits.payload.psrc).foreach { case (rf, psrc) =>
      rf.foreach(_.addr := psrc) // psrc in payload array can be pregIdx of IntRegFile or VfRegFile
    }
    deq.bits.rf.zip(deqEntryVec(i).bits.payload.srcType).foreach { case (rf, srcType) =>
      rf.foreach(_.srcType := srcType) // psrc in payload array can be pregIdx of IntRegFile or VfRegFile
    }
    deq.bits.srcType.zip(deqEntryVec(i).bits.payload.srcType).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.immType := deqEntryVec(i).bits.payload.selImm

    // dirty code for lui+addi(w) fusion
    when (deqEntryVec(i).bits.payload.isLUI32) {
      val lui_imm = Cat(deqEntryVec(i).bits.payload.lsrc(1), deqEntryVec(i).bits.payload.lsrc(0), deqEntryVec(i).bits.imm(ImmUnion.maxLen - 1, 0))
      deq.bits.common.imm := ImmUnion.LUI32.toImm32(lui_imm)
    }

    // dirty code for fused_lui_load
    when (SrcType.isImm(deqEntryVec(i).bits.payload.srcType(0)) && deqEntryVec(i).bits.payload.fuType === FuType.ldu.U) {
      deq.bits.common.imm := Imm_LUI_LOAD().getLuiImm(deqEntryVec(i).bits.payload)
    }

    deq.bits.common.perfDebugInfo := deqEntryVec(i).bits.payload.debugInfo
    deq.bits.common.perfDebugInfo.selectTime := GTimer()
    deq.bits.common.perfDebugInfo.issueTime := GTimer() + 1.U
  }

  private val ldCancels = io.fromCancelNetwork.map(in =>
    LoadShouldCancel(in.bits.common.loadDependency, io.ldCancel)
  )
  private val fromCancelNetworkShift = WireDefault(io.fromCancelNetwork)
  fromCancelNetworkShift.zip(io.fromCancelNetwork).foreach {
    case (shifted, original) =>
      original.ready := shifted.ready // this will not cause combinational loop
      shifted.bits.common.loadDependency.foreach(
        _ := original.bits.common.loadDependency.get.map(_ << 1)
      )
  }
  io.deqDelay.zip(fromCancelNetworkShift).zip(ldCancels).foreach { case ((deqDly, deq), ldCancel) =>
    NewPipelineConnect(
      deq, deqDly, deqDly.valid,
      deq.bits.common.robIdx.needFlush(io.flush) || ldCancel,
      Option("Scheduler2DataPathPipe")
    )
  }
  dontTouch(io.deqDelay)
  io.wakeupToIQ.zipWithIndex.foreach { case (wakeup, i) =>
    if (wakeUpQueues(i).nonEmpty && finalWakeUpL1ExuOH.nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits, finalWakeUpL1ExuOH.get(i))
      wakeup.bits.loadDependency := wakeUpQueues(i).get.io.deq.bits.loadDependency.getOrElse(0.U.asTypeOf(wakeup.bits.loadDependency))
    } else if (wakeUpQueues(i).nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits)
      wakeup.bits.loadDependency := wakeUpQueues(i).get.io.deq.bits.loadDependency.getOrElse(0.U.asTypeOf(wakeup.bits.loadDependency))
    } else {
      wakeup.valid := false.B
      wakeup.bits := 0.U.asTypeOf(wakeup.bits)
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
  io.enq.foreach(_.ready := !Cat(io.status.leftVec).orR || !enqHasValid) // Todo: more efficient implementation
  io.status.empty := !Cat(validVec).orR
  io.status.full := Cat(io.status.leftVec).orR

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    Mux1H(fuLatencyMaps(deqPortIdx) map { case (k, v) => (k.U === fuType, v.U) })
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
  XSPerfAccumulate("issue_instr_pre_count", PopCount(io.deq.map(_.valid)))
  XSPerfHistogram("issue_instr_pre_count_hist", PopCount(io.deq.map(_.valid)), true.B, 0, params.numDeq + 1, 1)
  XSPerfAccumulate("issue_instr_count", PopCount(io.deqDelay.map(_.valid)))
  XSPerfHistogram("issue_instr_count_hist", PopCount(io.deqDelay.map(_.valid)), true.B, 0, params.numDeq + 1, 1)

  // deq instr data source count
  XSPerfAccumulate("issue_datasource_reg", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_bypass", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_forward", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))
  XSPerfAccumulate("issue_datasource_noreg", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _))

  XSPerfHistogram("issue_datasource_reg_hist", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) })
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_bypass_hist", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_forward_hist", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
  XSPerfHistogram("issue_datasource_noreg_hist", io.deq.map{ deq => 
    PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) }) 
  }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)

  // deq instr data source count for each futype
  for (t <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(t)
    if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _)) {
      XSPerfAccumulate(s"issue_datasource_reg_futype_${fuName}", io.deq.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_bypass_futype_${fuName}", io.deq.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_forward_futype_${fuName}", io.deq.map{ deq =>
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))
      XSPerfAccumulate(s"issue_datasource_noreg_futype_${fuName}", io.deq.map{ deq => 
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _))

      XSPerfHistogram(s"issue_datasource_reg_hist_futype_${fuName}", io.deq.map{ deq => 
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.reg && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_bypass_hist_futype_${fuName}", io.deq.map{ deq => 
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.bypass && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_forward_hist_futype_${fuName}", io.deq.map{ deq => 
        PopCount(deq.bits.common.dataSources.zipWithIndex.map{ case (ds, j) => deq.valid && ds.value === DataSource.forward && !SrcType.isNotReg(deq.bits.srcType(j)) && deq.bits.common.fuType === t.U }) 
      }.reduce(_ +& _), true.B, 0, params.numDeq * params.numRegSrc + 1, 1)
      XSPerfHistogram(s"issue_datasource_noreg_hist_futype_${fuName}", io.deq.map{ deq => 
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

  if(params.needPc) {
    entries.io.enq.zipWithIndex.foreach { case (entriesEnq, i) =>
      entriesEnq.bits.status.pc.foreach(_ := io.enq(i).bits.pc)
    }
  }

  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.pc.foreach(_ := deqEntryVec(i).bits.status.pc.get)
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
  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
    deq.bits.common.vpu.foreach(_ := deqEntryVec(i).bits.payload.vpu)
    deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
    deq.bits.common.vpu.foreach(_.lastUop := deqEntryVec(i).bits.payload.lastUop)
  }}
}

class IssueQueueMemBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
  val feedbackIO = Flipped(Vec(params.numDeq, new MemRSFeedbackIO))
  val checkWait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
  val loadFastMatch = Output(Vec(params.LduCnt, new IssueQueueLoadBundle))

  // vector
  val sqDeqPtr = OptionWrapper(params.isVecMemIQ, Input(new SqPtr))
  val lqDeqPtr = OptionWrapper(params.isVecMemIQ, Input(new LqPtr))
}

class IssueQueueMemIO(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO {
  val memIO = Some(new IssueQueueMemBundle)
}

class IssueQueueMemAddrImp(override val wrapper: IssueQueue)(implicit p: Parameters, params: IssueBlockParams)
  extends IssueQueueImp(wrapper) with HasCircularQueuePtrHelper {

  require(params.StdCnt == 0 && (params.LduCnt + params.StaCnt) > 0, "IssueQueueMemAddrImp can only be instance of MemAddr IQ")

  io.suggestName("none")
  override lazy val io = IO(new IssueQueueMemIO).suggestName("io")
  private val memIO = io.memIO.get

  memIO.loadFastMatch := 0.U.asTypeOf(memIO.loadFastMatch) // TODO: is still needed?

  for (i <- io.enq.indices) {
    s0_enqBits(i).loadWaitBit := false.B
    // when have vpu
    if (params.VlduCnt > 0 || params.VstuCnt > 0) {
      s0_enqBits(i).srcType(3) := SrcType.vp // v0: mask src
      s0_enqBits(i).srcType(4) := SrcType.vp // vl&vtype
    }
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

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.sqIdx.get := deqEntryVec(i).bits.payload.sqIdx
    deq.bits.common.lqIdx.get := deqEntryVec(i).bits.payload.lqIdx
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
    // when have vpu
    if (params.VlduCnt > 0 || params.VstuCnt > 0) {
      deq.bits.common.vpu.foreach(_ := deqEntryVec(i).bits.payload.vpu)
      deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
    }
  }
}

class IssueQueueVecMemImp(override val wrapper: IssueQueue)(implicit p: Parameters, params: IssueBlockParams)
  extends IssueQueueImp(wrapper) with HasCircularQueuePtrHelper {

  require((params.VstdCnt + params.VlduCnt + params.VstaCnt) > 0, "IssueQueueVecMemImp can only be instance of VecMem IQ")

  io.suggestName("none")
  override lazy val io = IO(new IssueQueueMemIO).suggestName("io")
  private val memIO = io.memIO.get

  def selectOldUop(robIdx: Seq[RobPtr], uopIdx: Seq[UInt], valid: Seq[Bool]): Vec[Bool] = {
    val compareVec = (0 until robIdx.length).map(i => (0 until i).map(j => isAfter(robIdx(j), robIdx(i)) || (robIdx(j).value === robIdx(i).value && uopIdx(i) < uopIdx(j))))
    val resultOnehot = VecInit((0 until robIdx.length).map(i => Cat((0 until robIdx.length).map(j =>
      (if (j < i) !valid(j) || compareVec(i)(j)
      else if (j == i) valid(i)
      else !valid(j) || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }

  val robIdxVec = entries.io.robIdx.get
  val uopIdxVec = entries.io.uopIdx.get
  val allEntryOldestOH = selectOldUop(robIdxVec, uopIdxVec, validVec)

  finalDeqSelValidVec.head := (allEntryOldestOH.asUInt & canIssueVec.asUInt).orR
  finalDeqSelOHVec.head := allEntryOldestOH.asUInt & canIssueVec.asUInt

  if (params.isVecMemAddrIQ) {
    s0_enqBits.foreach{ x =>
      x.srcType(3) := SrcType.vp // v0: mask src
      x.srcType(4) := SrcType.vp // vl&vtype
    }

    for (i <- io.enq.indices) {
      val blockNotReleased = isAfter(io.enq(i).bits.sqIdx, memIO.checkWait.stIssuePtr)
      val storeAddrWaitForIsIssuing = VecInit((0 until StorePipelineWidth).map(i => {
        memIO.checkWait.memWaitUpdateReq.staIssue(i).valid &&
          memIO.checkWait.memWaitUpdateReq.staIssue(i).bits.uop.robIdx.value === io.enq(i).bits.waitForRobIdx.value
      })).asUInt.orR && !io.enq(i).bits.loadWaitStrict // is waiting for store addr ready
      s0_enqBits(i).loadWaitBit := io.enq(i).bits.loadWaitBit && !storeAddrWaitForIsIssuing && blockNotReleased
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
  }

  for (i <- entries.io.enq.indices) {
    entries.io.enq(i).bits.status match { case enqData =>
      enqData.vecMem.get.sqIdx := s0_enqBits(i).sqIdx
      enqData.vecMem.get.lqIdx := s0_enqBits(i).lqIdx
    }
  }

  entries.io.fromLsq.get.sqDeqPtr := memIO.sqDeqPtr.get
  entries.io.fromLsq.get.lqDeqPtr := memIO.lqDeqPtr.get

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.sqIdx.foreach(_ := deqEntryVec(i).bits.payload.sqIdx)
    deq.bits.common.lqIdx.foreach(_ := deqEntryVec(i).bits.payload.lqIdx)
    if (params.isVecLdAddrIQ) {
      deq.bits.common.ftqIdx.get := deqEntryVec(i).bits.payload.ftqPtr
      deq.bits.common.ftqOffset.get := deqEntryVec(i).bits.payload.ftqOffset
    }
    deq.bits.common.fpu.foreach(_ := deqEntryVec(i).bits.payload.fpu)
    deq.bits.common.vpu.foreach(_ := deqEntryVec(i).bits.payload.vpu)
    deq.bits.common.vpu.foreach(_.vuopIdx := deqEntryVec(i).bits.payload.uopIdx)
    deq.bits.common.vpu.foreach(_.lastUop := deqEntryVec(i).bits.payload.lastUop)
  }
}