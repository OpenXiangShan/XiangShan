package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasCircularQueuePtrHelper
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
  implicit val iqParams = params
  lazy val module = iqParams.schdType match {
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

  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og0Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og1Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val wbBusyTableRead = Input(params.genWbFuBusyTableReadBundle())
  val wbBusyTableWrite = Output(params.genWbFuBusyTableWriteBundle())
  val wakeupFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeupFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel = Input(ExuVec(backendParams.numExu))
  val og1Cancel = Input(ExuVec(backendParams.numExu))

  // Outputs
  val deq: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle
  val wakeupToIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = params.genIQWakeUpSourceValidBundle
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))

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
  val fuLatencyMaps : Seq[Map[Int, Int]] = params.exuBlockParams.map(x => x.fuLatencyMap)

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

  val wakeUpQueues: Seq[Option[MultiWakeupQueue[ExuInput, ValidIO[Redirect]]]] = params.exuBlockParams.map { x => OptionWrapper(x.isIQWakeUpSource, Module(
    new MultiWakeupQueue(
      new ExuInput(x),
      ValidIO(new Redirect) ,
      x.fuLatancySet,
      (exuInput: ExuInput, flush: ValidIO[Redirect]) => exuInput.robIdx.needFlush(flush)
    )
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
  for (i <- io.enq.indices) {
    for (j <- s0_enqBits(i).srcType.indices) {
      wakeupEnqSrcStateBypassFromWB(i)(j) := Cat(
        io.wakeupFromWB.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head)
      ).orR
    }
  }

  val wakeupEnqSrcStateBypassFromIQ: Vec[Vec[UInt]] = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  for (i <- io.enq.indices) {
    for (j <- s0_enqBits(i).srcType.indices) {
      wakeupEnqSrcStateBypassFromIQ(i)(j) := Cat(
        io.wakeupFromIQ.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head)
      ).orR
    }
  }
  val srcWakeUpEnqByIQMatrix = Wire(Vec(params.numEnq, Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))))
  srcWakeUpEnqByIQMatrix.zipWithIndex.foreach { case (wakeups: Vec[Vec[Bool]], i) =>
    if (io.wakeupFromIQ.isEmpty) {
      wakeups := 0.U.asTypeOf(wakeups)
    } else {
      val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.wakeupFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
        bundle.bits.wakeUp(s0_enqBits(i).psrc.take(params.numRegSrc) zip s0_enqBits(i).srcType.take(params.numRegSrc), bundle.valid)
      ).transpose
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
    entriesIO.enq.zipWithIndex.foreach { case (enq: ValidIO[EntryBundle], i) =>
      enq.valid := s0_doEnqSelValidVec(i)
      val numLsrc = s0_enqBits(i).srcType.size.min(enq.bits.status.srcType.size)
      for(j <-0 until numLsrc) {
        enq.bits.status.srcState(j) := s0_enqBits(i).srcState(j) |
                                       wakeupEnqSrcStateBypassFromWB(i)(j) |
                                       wakeupEnqSrcStateBypassFromIQ(i)(j)
        enq.bits.status.psrc(j) := s0_enqBits(i).psrc(j)
        enq.bits.status.srcType(j) := s0_enqBits(i).srcType(j)
        enq.bits.status.dataSources(j).value := Mux(wakeupEnqSrcStateBypassFromIQ(i)(j).asBool, DataSource.forward, DataSource.reg)
      }
      enq.bits.status.fuType := s0_enqBits(i).fuType
      enq.bits.status.robIdx := s0_enqBits(i).robIdx
      enq.bits.status.issueTimer := "b11".U
      enq.bits.status.deqPortIdx := 0.U
      enq.bits.status.issued := false.B
      enq.bits.status.firstIssue := false.B
      enq.bits.status.blocked := false.B
      enq.bits.status.srcWakeUpL1ExuOH match {
        case Some(value) => value.zip(srcWakeUpEnqByIQMatrix(i)).zipWithIndex.foreach {
          case ((exuOH, wakeUpByIQOH), srcIdx) =>
            when(wakeUpByIQOH.asUInt.orR) {
              exuOH := Mux1H(wakeUpByIQOH, io.wakeupFromIQ.map(x => MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W))).asBools
            }.otherwise {
              exuOH := 0.U.asTypeOf(exuOH)
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
              timer := 0.U.asTypeOf(timer)
            }
        }
        case None =>
      }
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
    wakeUpQueueOption.foreach {
      wakeUpQueue =>
        wakeUpQueue.io.flush := io.flush
        wakeUpQueue.io.enq.valid := io.deq(i).fire && {
          if (io.deq(i).bits.common.rfWen.isDefined)
            io.deq(i).bits.common.rfWen.get && io.deq(i).bits.common.pdest =/= 0.U
          else
            true.B
        }
        wakeUpQueue.io.enq.bits.uop := io.deq(i).bits.common
        wakeUpQueue.io.enq.bits.lat := getDeqLat(i, io.deq(i).bits.common.fuType)
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
    deq.bits.common.l1ExuVec.foreach(_ := finalWakeUpL1ExuOH.get(i))
    deq.bits.common.srcTimer.foreach(_ := finalSrcTimer.get(i))

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
  }
  io.deqDelay.zip(io.fromCancelNetwork).foreach{ case(deqDly, deq) =>
    NewPipelineConnect(
      deq, deqDly, deqDly.valid,
      deq.bits.common.robIdx.needFlush(io.flush),
      Option("Scheduler2DataPathPipe")
    )
  }
  dontTouch(io.deqDelay)
  io.wakeupToIQ.zipWithIndex.foreach { case (wakeup, i) =>
    if (wakeUpQueues(i).nonEmpty && finalWakeUpL1ExuOH.nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits, finalWakeUpL1ExuOH.get(i))
    } else if (wakeUpQueues(i).nonEmpty) {
      wakeup.valid := wakeUpQueues(i).get.io.deq.valid
      wakeup.bits.fromExuInput(wakeUpQueues(i).get.io.deq.bits)
    } else {
      wakeup.valid := false.B
      wakeup.bits := 0.U.asTypeOf(wakeup.bits.cloneType)
    }
  }

  // Todo: better counter implementation
  private val enqHasValid = validVec.take(params.numEnq).reduce(_ | _)
  private val othersValidCnt = PopCount(validVec.drop(params.numEnq))
  io.status.leftVec(0) := validVec.drop(params.numEnq).reduce(_ & _)
  for (i <- 0 until params.numEnq) {
    io.status.leftVec(i + 1) := othersValidCnt === (params.numEntries - params.numEnq - (i + 1)).U
  }
  io.enq.foreach(_.ready := !Cat(io.status.leftVec).orR || !enqHasValid) // Todo: more efficient implementation

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    val fuLatUIntMaps: Map[UInt, UInt] = fuLatencyMaps(deqPortIdx).map { case (k, v) => (k.U, v.U) }
    val lat = WireInit(Mux1H(fuLatUIntMaps.keys.map(_ === fuType).toSeq, fuLatUIntMaps.values.toSeq))
    dontTouch(lat)
  }
}

class IssueQueueJumpBundle extends Bundle {
  val pc = UInt(VAddrData().dataWidth.W)
  val target = UInt(VAddrData().dataWidth.W)
}

class IssueQueueLoadBundle(implicit p: Parameters) extends XSBundle {
  val fastMatch = UInt(backendParams.LduCnt.W)
  val fastImm = UInt(12.W)
}

class IssueQueueIntIO()(implicit p: Parameters, params: IssueBlockParams) extends IssueQueueIO {
  val enqJmp = if(params.numPcReadPort > 0) Some(Input(Vec(params.numPcReadPort, new IssueQueueJumpBundle))) else None
}

class IssueQueueIntImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  io.suggestName("none")
  override lazy val io = IO(new IssueQueueIntIO).suggestName("io")

  if(params.needPc) {
    entries.io.enq.zipWithIndex.foreach { case (entriesEnq, i) =>
      entriesEnq.bits.status.pc.foreach(_ := io.enq(i).bits.pc)
      entriesEnq.bits.status.target.foreach(_ := io.enqJmp.get(i).target)
    }
  }

  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.jmp.foreach((deqJmp: IssueQueueJumpBundle) => {
      deqJmp.pc := deqEntryVec(i).bits.status.pc.get
      deqJmp.target := deqEntryVec(i).bits.status.target.get
    })
    deq.bits.common.preDecode.foreach(_ := deqEntryVec(i).bits.payload.preDecodeInfo)
    deq.bits.common.ftqIdx.foreach(_ := deqEntryVec(i).bits.payload.ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := deqEntryVec(i).bits.payload.ftqOffset)
    deq.bits.common.predictInfo.foreach(x => {
      x.target := deqEntryVec(i).bits.status.target.get
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

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.sqIdx.get := deqEntryVec(i).bits.payload.sqIdx
    deq.bits.common.lqIdx.get := deqEntryVec(i).bits.payload.lqIdx
    if (params.isLdAddrIQ) {
      deq.bits.common.ftqIdx.get := deqEntryVec(i).bits.payload.ftqPtr
      deq.bits.common.ftqOffset.get := deqEntryVec(i).bits.payload.ftqOffset
    }
  }
}