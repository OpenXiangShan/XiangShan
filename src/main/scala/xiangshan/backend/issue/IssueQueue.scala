package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasCircularQueuePtrHelper
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}

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

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueBlockParams) extends StatusArrayDeqRespBundle

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
  val statusArray   = Module(StatusArray(p, params))
  val immArray      = Module(new DataArray(UInt(XLEN.W), params.numDeq, params.numEnq, params.numEntries))
  val payloadArray  = Module(new DataArray(Output(new DynInst), params.numDeq, params.numEnq, params.numEntries))
  val enqPolicy     = Module(new EnqPolicy)
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
  val s0_enqSelOHVec = Wire(Vec(params.numEnq, UInt(params.numEntries.W)))
  val s0_enqNotFlush = !io.flush.valid
  val s0_enqBits = WireInit(VecInit(io.enq.map(_.bits)))
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush)
  val s0_doEnqOH: Vec[UInt] = VecInit((s0_doEnqSelValidVec zip s0_enqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  })

  val s0_enqImmValidVec = io.enq.map(enq => enq.valid)
  val s0_enqImmVec = VecInit(io.enq.map(_.bits.imm))

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

  val validVec = VecInit(statusArray.io.valid.asBools)
  val canIssueVec = VecInit(statusArray.io.canIssue.asBools)
  val clearVec = VecInit(statusArray.io.clear.asBools)
  val deqFirstIssueVec = VecInit(statusArray.io.deq.map(_.isFirstIssue))

  val dataSources: Vec[Vec[DataSource]] = statusArray.io.dataSources
  val finalDataSources: Vec[Vec[DataSource]] = VecInit(finalDeqOH.map(oh => Mux1H(oh, dataSources)))
  // (entryIdx)(srcIdx)(exuIdx)
  val wakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = statusArray.io.srcWakeUpL1ExuOH
  val srcTimer: Option[Vec[Vec[UInt]]] = statusArray.io.srcTimer

  // (deqIdx)(srcIdx)(exuIdx)
  val finalWakeUpL1ExuOH: Option[Vec[Vec[Vec[Bool]]]] = wakeUpL1ExuOH.map(x => VecInit(finalDeqOH.map(oh => Mux1H(oh, x))))
  val finalSrcTimer = srcTimer.map(x => VecInit(finalDeqOH.map(oh => Mux1H(oh, x))))

  val wakeupEnqSrcStateBypass = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  for (i <- io.enq.indices) {
    for (j <- s0_enqBits(i).srcType.indices) {
      wakeupEnqSrcStateBypass(i)(j) := Cat(
        io.wakeupFromWB.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head)
      ).orR
    }
  }

  /**
    * Connection of [[statusArray]]
    */
  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.flush  <> io.flush
    statusArrayIO.wakeUpFromIQ := io.wakeupFromIQ
    statusArrayIO.og0Cancel := io.og0Cancel
    statusArrayIO.og1Cancel := io.og1Cancel
    statusArrayIO.wakeUpFromWB := io.wakeupFromWB
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      enq.valid                 := s0_doEnqSelValidVec(i)
      enq.bits.addrOH           := s0_enqSelOHVec(i)
      val numLSrc = s0_enqBits(i).srcType.size.min(enq.bits.data.srcType.size)
      for (j <- 0 until numLSrc) {
        enq.bits.data.srcState(j) := s0_enqBits(i).srcState(j) | wakeupEnqSrcStateBypass(i)(j)
        enq.bits.data.psrc(j)     := s0_enqBits(i).psrc(j)
        enq.bits.data.srcType(j)  := s0_enqBits(i).srcType(j)
      }
      enq.bits.data.robIdx      := s0_enqBits(i).robIdx
      enq.bits.data.issued      := false.B
      enq.bits.data.firstIssue  := false.B
      enq.bits.data.blocked     := false.B
      enq.bits.data.dataSources.foreach(_.value := DataSource.reg)
      enq.bits.data.srcWakeUpL1ExuOH match {
        case Some(value) => value := 0.U.asTypeOf(value)
        case None =>
      }
      enq.bits.data.srcTimer match {
        case Some(value) => value := 0.U.asTypeOf(value)
        case None =>
      }
    }
    statusArrayIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.deqSelOH.valid  := finalDeqSelValidVec(i)
      deq.deqSelOH.bits   := finalDeqSelOHVec(i)
    }
    statusArrayIO.deqResp.zipWithIndex.foreach { case (deqResp, i) =>
      deqResp.valid      := io.deqResp(i).valid
      deqResp.bits.addrOH := io.deqResp(i).bits.addrOH
      deqResp.bits.dataInvalidSqIdx := io.deqResp(i).bits.dataInvalidSqIdx
      deqResp.bits.respType := io.deqResp(i).bits.respType
      deqResp.bits.rfWen := io.deqResp(i).bits.rfWen
      deqResp.bits.fuType := io.deqResp(i).bits.fuType
    }
    statusArrayIO.og0Resp.zipWithIndex.foreach { case (og0Resp, i) =>
      og0Resp.valid := io.og0Resp(i).valid
      og0Resp.bits.addrOH := io.og0Resp(i).bits.addrOH
      og0Resp.bits.dataInvalidSqIdx := io.og0Resp(i).bits.dataInvalidSqIdx
      og0Resp.bits.respType := io.og0Resp(i).bits.respType
      og0Resp.bits.rfWen := io.og0Resp(i).bits.rfWen
      og0Resp.bits.fuType := io.og0Resp(i).bits.fuType
    }
    statusArrayIO.og1Resp.zipWithIndex.foreach { case (og1Resp, i) =>
      og1Resp.valid := io.og1Resp(i).valid
      og1Resp.bits.addrOH := io.og1Resp(i).bits.addrOH
      og1Resp.bits.dataInvalidSqIdx := io.og1Resp(i).bits.dataInvalidSqIdx
      og1Resp.bits.respType := io.og1Resp(i).bits.respType
      og1Resp.bits.rfWen := io.og1Resp(i).bits.rfWen
      og1Resp.bits.fuType := io.og1Resp(i).bits.fuType
    }
  }

  /**
    * Connection of [[immArray]]
    */
  val immArrayRdataVec = immArray.io.read.map(_.data)
  immArray.io match { case immArrayIO: DataArrayIO[UInt] =>
    immArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := s0_doEnqSelValidVec(i) && s0_enqImmValidVec(i)
      w.addr := s0_enqSelOHVec(i)
      w.data := s0_enqImmVec(i)
    }
    immArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := finalDeqOH(i)
    }
  }

  /**
    * Connection of [[payloadArray]]
    */
  val payloadArrayRdata = Wire(Vec(params.numDeq, Output(new DynInst)))
  payloadArray.io match { case payloadArrayIO: DataArrayIO[DynInst] =>
    payloadArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := s0_doEnqSelValidVec(i)
      w.addr := s0_enqSelOHVec(i)
      w.data := s0_enqBits(i)
    }
    payloadArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := finalDeqOH(i)
      payloadArrayRdata(i) := r.data
    }
  }

  val fuTypeRegVec = Reg(Vec(params.numEntries, FuType()))
  val fuTypeNextVec = WireInit(fuTypeRegVec)
  fuTypeRegVec := fuTypeNextVec

  s0_doEnqSelValidVec.zip(s0_enqSelOHVec).zipWithIndex.foreach { case ((valid, oh), i) =>
    when (valid) {
      fuTypeNextVec(OHToUInt(oh)) := s0_enqBits(i).fuType
    }
  }

  enqPolicy match { case ep =>
    ep.io.valid     := validVec.asUInt
    s0_enqSelValidVec  := ep.io.enqSelOHVec.map(oh => oh.valid).zip(s0_enqValidVec).zip(io.enq).map { case((sel, enqValid), enq) => enqValid && sel && enq.ready}
    s0_enqSelOHVec     := ep.io.enqSelOHVec.map(oh => oh.bits)
  }

  protected val commonAccept: UInt = Cat(fuTypeRegVec.map(fuType =>
    Cat(commonFuCfgs.map(_.fuType.U === fuType)).orR
  ).reverse)

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeRegVec.map(fuType => Cat(fuCfgs.map(_.fuType.U === fuType)).orR).reverse).asUInt
  }

  protected val deqCanAcceptVec: Seq[IndexedSeq[Bool]] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    fuTypeRegVec.map(fuType =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR) // C+E0    C+E1
  }

  subDeqPolicies.zipWithIndex.map { case (dpOption: Option[DeqPolicy], i) =>
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

  val ageDetectorEnqVec: Vec[Vec[UInt]] = WireInit(VecInit(Seq.fill(params.numDeq)(VecInit(Seq.fill(params.numEnq)(0.U(params.numEntries.W))))))

  ageDetectorEnqVec.zip(enqCanAcceptVec) foreach {
    case (ageDetectorEnq, enqCanAccept) =>
      ageDetectorEnq := enqCanAccept.zip(s0_doEnqOH).map {
        case (enqCanAccept, s0_doEnqOH) => Mux(enqCanAccept, s0_doEnqOH, 0.U)
      }
  }

  val oldestSelVec = (0 until params.numDeq).map {
    case deqIdx =>
      AgeDetector(numEntries = params.numEntries,
        enq = ageDetectorEnqVec(deqIdx),
        deq = clearVec.asUInt,
        canIssue = canIssueVec.asUInt & (~fuBusyTableMask(deqIdx)).asUInt & (~intWbBusyTableMask(deqIdx)).asUInt & (~vfWbBusyTableMask(deqIdx)).asUInt)
  }

  finalDeqSelValidVec.head := oldestSelVec.head.valid || subDeqSelValidVec.head.getOrElse(Seq(false.B)).head
  finalDeqSelOHVec.head := Mux(oldestSelVec.head.valid, oldestSelVec.head.bits, subDeqSelOHVec.head.getOrElse(Seq(0.U)).head)

  if (params.numDeq == 2) {
    val chooseOldest = oldestSelVec(1).valid && oldestSelVec(1).bits =/= finalDeqSelOHVec.head
    val choose1stSub = subDeqSelOHVec(1).getOrElse(Seq(0.U)).head =/= finalDeqSelOHVec.head

    finalDeqSelValidVec(1) := MuxCase(subDeqSelValidVec(1).getOrElse(Seq(false.B)).last, Seq(
      (chooseOldest) -> oldestSelVec(1).valid,
      (choose1stSub) -> subDeqSelValidVec(1).getOrElse(Seq(false.B)).head)
    )
    finalDeqSelOHVec(1) := MuxCase(subDeqSelOHVec(1).getOrElse(Seq(0.U)).last, Seq(
      (chooseOldest) -> oldestSelVec(1).bits,
      (choose1stSub) -> subDeqSelOHVec(1).getOrElse(Seq(0.U)).head)
    )
  }

  //fuBusyTable
  fuBusyTableWrite.zip(fuBusyTableRead).zipWithIndex.map { case ((busyTableWrite: Option[FuBusyTableWrite], busyTableRead: Option[FuBusyTableRead]), i) =>
    if(busyTableWrite.nonEmpty) {
      val btwr = busyTableWrite.get
      val btrd = busyTableRead.get
      btwr.io.in.deqResp := io.deqResp(i)
      btwr.io.in.og0Resp := io.og0Resp(i)
      btwr.io.in.og1Resp := io.og1Resp(i)
      btrd.io.in.fuBusyTable := btwr.io.out.fuBusyTable
      btrd.io.in.fuTypeRegVec := fuTypeRegVec
      fuBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      fuBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }

  //wbfuBusyTable write
  intWbBusyTableWrite.zip(intWbBusyTableOut).zip(intDeqRespSetOut).zipWithIndex.map { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
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

  vfWbBusyTableWrite.zip(vfWbBusyTableOut).zip(vfDeqRespSetOut).zipWithIndex.map { case (((busyTableWrite: Option[FuBusyTableWrite], busyTable: Option[UInt]), deqResp), i) =>
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
  intWbBusyTableRead.zip(intWbBusyTableIn).zipWithIndex.map { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if(busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeRegVec
      intWbBusyTableMask(i) := btrd.io.out.fuBusyTableMask
    }
    else {
      intWbBusyTableMask(i) := 0.U(params.numEntries.W)
    }
  }
  vfWbBusyTableRead.zip(vfWbBusyTableIn).zipWithIndex.map { case ((busyTableRead: Option[FuBusyTableRead], busyTable: Option[UInt]), i) =>
    if (busyTableRead.nonEmpty) {
      val btrd = busyTableRead.get
      val bt = busyTable.get
      btrd.io.in.fuBusyTable := bt
      btrd.io.in.fuTypeRegVec := fuTypeRegVec
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
    deq.bits.common.fuType   := payloadArrayRdata(i).fuType
    deq.bits.common.fuOpType := payloadArrayRdata(i).fuOpType
    deq.bits.common.rfWen.foreach(_ := payloadArrayRdata(i).rfWen)
    deq.bits.common.fpWen.foreach(_ := payloadArrayRdata(i).fpWen)
    deq.bits.common.vecWen.foreach(_ := payloadArrayRdata(i).vecWen)
    deq.bits.common.flushPipe.foreach(_ := payloadArrayRdata(i).flushPipe)
    deq.bits.common.pdest := payloadArrayRdata(i).pdest
    deq.bits.common.robIdx := payloadArrayRdata(i).robIdx
    deq.bits.common.imm := immArrayRdataVec(i)
    deq.bits.common.dataSources.zip(finalDataSources(i)).zipWithIndex.foreach {
      case ((sink, source), srcIdx) =>
        sink.value := Mux(
          SrcType.isXp(payloadArrayRdata(i).srcType(srcIdx)) && payloadArrayRdata(i).psrc(srcIdx) === 0.U,
          DataSource.none,
          source.value
        )
    }
    deq.bits.common.l1ExuVec.foreach(_ := finalWakeUpL1ExuOH.get(i))
    deq.bits.common.srcTimer.foreach(_ := finalSrcTimer.get(i))

    deq.bits.rf.zip(payloadArrayRdata(i).psrc).foreach { case (rf, psrc) =>
      rf.foreach(_.addr := psrc) // psrc in payload array can be pregIdx of IntRegFile or VfRegFile
    }
    deq.bits.rf.zip(payloadArrayRdata(i).srcType).foreach { case (rf, srcType) =>
      rf.foreach(_.srcType := srcType) // psrc in payload array can be pregIdx of IntRegFile or VfRegFile
    }
    deq.bits.srcType.zip(payloadArrayRdata(i).srcType).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.immType := payloadArrayRdata(i).selImm
  }

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
  private val validCnt = PopCount(validVec)
  private val enqSelCnt = PopCount(s0_doEnqSelValidVec)
  private val validCntNext = validCnt + enqSelCnt
  io.status.full := validVec.asUInt.andR
  io.status.empty := !validVec.asUInt.orR
  io.status.leftVec(0) := io.status.full
  for (i <- 0 until params.numEnq) {
    io.status.leftVec(i + 1) := validCnt === (params.numEntries - (i + 1)).U
  }
  io.statusNext.full := validCntNext === params.numEntries.U
  io.statusNext.empty := validCntNext === 0.U // always false now
  io.statusNext.leftVec(0) := io.statusNext.full
  for (i <- 0 until params.numEnq) {
    io.statusNext.leftVec(i + 1) := validCntNext === (params.numEntries - (i + 1)).U
  }
  io.enq.foreach(_.ready := !Cat(io.status.leftVec).orR) // Todo: more efficient implementation

  protected def getDeqLat(deqPortIdx: Int, fuType: UInt) : UInt = {
    val fuLatUIntMaps: Map[UInt, UInt] = fuLatencyMaps(deqPortIdx).map { case (k, v) => (k.U, v.U) }
    val lat = Mux1H(fuLatUIntMaps.keys.map(_ === fuType).toSeq, fuLatUIntMaps.values.toSeq)
    dontTouch(lat)
    // ParallelLookUp(fuType, fuLatencyMaps(deqPortIdx).map { case (k, v) => (k.U, v.U) }.toSeq)
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
  val pcArray: Option[DataArray[UInt]] = if(params.needPc) Some(Module(
    new DataArray(UInt(VAddrData().dataWidth.W), params.numDeq, params.numEnq, params.numEntries)
  )) else None
  val targetArray: Option[DataArray[UInt]] = if(params.needPc) Some(Module(
    new DataArray(UInt(VAddrData().dataWidth.W), params.numDeq, params.numEnq, params.numEntries)
  )) else None

  if (pcArray.nonEmpty) {
    val pcArrayIO = pcArray.get.io
    pcArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := finalDeqSelOHVec(i)
    }
    pcArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := s0_doEnqSelValidVec(i)
      w.addr := s0_enqSelOHVec(i)
      w.data := io.enq(i).bits.pc
    }
  }

  if (targetArray.nonEmpty) {
    val arrayIO = targetArray.get.io
    arrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := finalDeqSelOHVec(i)
    }
    arrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := s0_doEnqSelValidVec(i)
      w.addr := s0_enqSelOHVec(i)
      w.data := io.enqJmp.get(i).target
    }
  }

  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.jmp.foreach((deqJmp: IssueQueueJumpBundle) => {
      deqJmp.pc := pcArray.get.io.read(i).data
      deqJmp.target := targetArray.get.io.read(i).data
    })
    deq.bits.common.preDecode.foreach(_ := payloadArrayRdata(i).preDecodeInfo)
    deq.bits.common.ftqIdx.foreach(_ := payloadArrayRdata(i).ftqPtr)
    deq.bits.common.ftqOffset.foreach(_ := payloadArrayRdata(i).ftqOffset)
    deq.bits.common.predictInfo.foreach(x => {
      x.target := targetArray.get.io.read(i).data
      x.taken := payloadArrayRdata(i).pred_taken
    })
    // for std
    deq.bits.common.sqIdx.foreach(_ := payloadArrayRdata(i).sqIdx)
    // for i2f
    deq.bits.common.fpu.foreach(_ := payloadArrayRdata(i).fpu)
  }}
}

class IssueQueueVfImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      val numLSrc = s0_enqBits(i).srcType.size min enq.bits.data.srcType.size
      val numPSrc = s0_enqBits(i).srcState.size min enq.bits.data.srcState.size

      for (j <- 0 until numPSrc) {
        enq.bits.data.srcState(j) := s0_enqBits(i).srcState(j) | wakeupEnqSrcStateBypass(i)(j)
        enq.bits.data.psrc(j)     := s0_enqBits(i).psrc(j)
      }

      for (j <- 0 until numLSrc) {
        enq.bits.data.srcType(j) := s0_enqBits(i).srcType(j)
      }
      if (enq.bits.data.srcType.isDefinedAt(3)) enq.bits.data.srcType(3) := SrcType.vp // v0: mask src
      if (enq.bits.data.srcType.isDefinedAt(4)) enq.bits.data.srcType(4) := SrcType.vp // vl&vtype
    }
  }
  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.fpu.foreach(_ := payloadArrayRdata(i).fpu)
    deq.bits.common.vpu.foreach(_ := payloadArrayRdata(i).vpu)
    deq.bits.common.vpu.foreach(_.vuopIdx := payloadArrayRdata(i).uopIdx)
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

  for (i <- statusArray.io.enq.indices) {
    statusArray.io.enq(i).bits.data match { case enqData =>
      enqData.blocked := false.B // s0_enqBits(i).loadWaitBit
      enqData.mem.get.strictWait := s0_enqBits(i).loadWaitStrict
      enqData.mem.get.waitForStd := false.B
      enqData.mem.get.waitForRobIdx := s0_enqBits(i).waitForRobIdx
      enqData.mem.get.waitForSqIdx := 0.U.asTypeOf(enqData.mem.get.waitForSqIdx) // generated by sq, will be updated later
      enqData.mem.get.sqIdx := s0_enqBits(i).sqIdx
    }

    statusArray.io.fromMem.get.slowResp.zipWithIndex.foreach { case (slowResp, i) =>
      slowResp.valid                 := memIO.feedbackIO(i).feedbackSlow.valid
      slowResp.bits.addrOH           := UIntToOH(memIO.feedbackIO(i).feedbackSlow.bits.rsIdx)
      slowResp.bits.respType         := Mux(memIO.feedbackIO(i).feedbackSlow.bits.hit, RSFeedbackType.fuIdle, RSFeedbackType.feedbackInvalid)
      slowResp.bits.dataInvalidSqIdx := memIO.feedbackIO(i).feedbackSlow.bits.dataInvalidSqIdx
      slowResp.bits.rfWen := DontCare
      slowResp.bits.fuType := DontCare
    }

    statusArray.io.fromMem.get.fastResp.zipWithIndex.foreach { case (fastResp, i) =>
      fastResp.valid                 := memIO.feedbackIO(i).feedbackFast.valid
      fastResp.bits.addrOH           := UIntToOH(memIO.feedbackIO(i).feedbackFast.bits.rsIdx)
      fastResp.bits.respType         := memIO.feedbackIO(i).feedbackFast.bits.sourceType
      fastResp.bits.dataInvalidSqIdx := 0.U.asTypeOf(fastResp.bits.dataInvalidSqIdx)
      fastResp.bits.rfWen := DontCare
      fastResp.bits.fuType := DontCare
    }

    statusArray.io.fromMem.get.memWaitUpdateReq := memIO.checkWait.memWaitUpdateReq
    statusArray.io.fromMem.get.stIssuePtr := memIO.checkWait.stIssuePtr
  }

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.bits.common.sqIdx.get := payloadArrayRdata(i).sqIdx
    deq.bits.common.lqIdx.get := payloadArrayRdata(i).lqIdx
    if (params.isLdAddrIQ) {
      deq.bits.common.ftqIdx.get := payloadArrayRdata(i).ftqPtr
      deq.bits.common.ftqOffset.get := payloadArrayRdata(i).ftqOffset
    }
  }
}