package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, IssueQueueIssueBundle, IssueQueueWakeUpBundle}
import xiangshan.v2backend._
import xiangshan.{HasXSParameter, MemRSFeedbackIO, Redirect, SrcState, SrcType, XSBundle}

case class IssueQueueParams(
  val exuParams          : Seq[ExeUnitParams],
  var numEntries         : Int,
  var numEnq             : Int,
  var numDeq             : Int,
  var numSrc             : Int,
  var dataBits           : Int,
  var pregBits           : Int,
  var numWakeupFromWB    : Int,
  var schdType           : SchedulerType = NoScheduler(),
  var numWakeupFromIQ    : Int = 0,
  var numWakeupFromOthers: Int = 0,
  var vaddrBits          : Int = 39,
) {
  require(numWakeupFromWB > 0 && numWakeupFromIQ >= 0 && numWakeupFromOthers >= 0)
  require(numEnq > 0 && numDeq > 0)

  def hasBrhFu = exuParams.map(_.hasBrhFu).reduce(_ || _)
  def hasJmpFu = exuParams.map(_.hasJmpFu).reduce(_ || _)
  def hasLoadFu = exuParams.map(_.hasLoadFu).reduce(_ || _)
  def hasStoreFu = exuParams.map(_.hasStoreFu).reduce(_ || _)
  def hasLoadStore = hasLoadFu || hasStoreFu
  def hasRedirectOut = hasBrhFu || hasJmpFu
  def numAllWakeup: Int = numWakeupFromWB + numWakeupFromIQ + numWakeupFromOthers
}

object DummyIQParams {
  def apply()(implicit p: Parameters): IssueBlockParams = {
    SchdBlockParams.dummyIntParams().issueBlockParams(0)
  }
}

class IssueQueue(params: IssueBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  implicit val iqParams = params

  lazy val module = iqParams.schdType match {
    case IntScheduler() => new IssueQueueIntImp(this)
    case VfScheduler() => new IssueQueueImp(this)
    case _ => new IssueQueueImp(this)
  }
}

class IssueQueueStatusBundle(numEnq: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val leftVec = Output(Vec(numEnq + 1, Bool()))
}

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueBlockParams) extends StatusArrayDeqRespBundle

class IssueQueueIO()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))

  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val deq: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle
  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val wakeup = Vec(params.numWakeupFromWB, Flipped(ValidIO(new IssueQueueWakeUpBundle(params.pregBits))))
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))
  // Todo: wake up bundle
}

class IssueQueueImp(override val wrapper: IssueQueue)(implicit p: Parameters, val params: IssueBlockParams)
  extends LazyModuleImp(wrapper)
  with HasXSParameter {

  require(params.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs)
  val allDeqFuCfgs  : Seq[FuConfig] = params.exuBlockParams.flatMap(_.fuConfigs)
  val fuCfgsCnt     : Map[FuConfig, Int] = allDeqFuCfgs.groupBy(x => x).map { case (cfg, cfgSeq) => (cfg, cfgSeq.length) }
  val commonFuCfgs  : Seq[FuConfig] = fuCfgsCnt.filter(_._2 > 1).keys.toSeq
  val specialFuCfgs : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs.filterNot(commonFuCfgs.contains))
  println(s"commonFuCfgs: ${commonFuCfgs.map(_.name)}")
  println(s"specialFuCfgs: ${specialFuCfgs.map(_.map(_.name))}")
  lazy val io = IO(new IssueQueueIO())
  dontTouch(io.deq)
  dontTouch(io.deqResp)
  // Modules
  val statusArray   = Module(new StatusArray)
  val immArray      = Module(new DataArray(UInt(XLEN.W), params.numDeq, params.numEnq, params.numEntries))
  val payloadArray  = Module(new DataArray(Output(new DynInst), params.numDeq, params.numEnq, params.numEntries))
  val enqPolicy     = Module(new EnqPolicy)
  val mainDeqPolicy = Module(new DeqPolicy)
  val subDeqPolicies  = specialFuCfgs.map(x => if (x.nonEmpty) Some(Module(new DeqPolicy())) else None)

  // Wires
  val s0_enqValidVec = io.enq.map(_.valid)
  val s0_enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val s0_enqSelOHVec = Wire(Vec(params.numEnq, UInt(params.numEntries.W)))
  val s0_enqNotFlush = !io.flush.valid
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush)
  val s0_doEnqOH: IndexedSeq[UInt] = (s0_doEnqSelValidVec zip s0_enqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }

  val s0_enqImmValidVec = io.enq.map(enq => enq.valid)
  val s0_enqImmVec = VecInit(io.enq.map(_.bits.imm))

  val mainDeqSelValidVec = Wire(Vec(params.numDeq, Bool()))
  val mainDeqSelOHVec = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val mainDeqOH: IndexedSeq[UInt] = (mainDeqSelValidVec zip mainDeqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }
  val mainDeqMask: UInt = mainDeqOH.reduce(_ | _)
  val finalDeqSelValidVec : Vec[Bool] = WireInit(mainDeqSelValidVec)
  val finalDeqSelOHVec    : Vec[UInt] = WireInit(mainDeqSelOHVec)
  val finalDeqOH: IndexedSeq[UInt] = (finalDeqSelValidVec zip finalDeqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }
  val finalDeqMask: UInt = finalDeqOH.reduce(_ | _)

  val deqRespVec = io.deqResp

  val validVec = VecInit(statusArray.io.valid.asBools)
  val canIssueVec = VecInit(statusArray.io.canIssue.asBools)
  val clearVec = VecInit(statusArray.io.clear.asBools)
  val deqFirstIssueVec = VecInit(statusArray.io.deq.map(_.isFirstIssue))

  val wakeupEnqSrcStateBypass = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  for (i <- io.enq.indices) {
    for (j <- io.enq(i).bits.srcType.indices) {
      wakeupEnqSrcStateBypass(i)(j) := Cat(
        io.wakeup.map(x => x.valid && x.bits.wakeUp(Seq((io.enq(i).bits.psrc(j), io.enq(i).bits.srcType(j)))).head)
      ).orR
    }
  }

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.flush  <> io.flush
    statusArrayIO.wakeup <> io.wakeup
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      enq.valid                 := s0_doEnqSelValidVec(i)
      enq.bits.addrOH           := s0_enqSelOHVec(i)
      val numLSrc = io.enq(i).bits.srcType.size.min(enq.bits.data.srcType.size)
      for (j <- 0 until numLSrc) {
        enq.bits.data.srcState(j) := io.enq(i).bits.srcState(j) | wakeupEnqSrcStateBypass(i)(j)
        enq.bits.data.psrc(j)     := io.enq(i).bits.psrc(j)
        enq.bits.data.srcType(j)  := io.enq(i).bits.srcType(j)
      }
      enq.bits.data.robIdx      := io.enq(i).bits.robIdx
      enq.bits.data.ready       := false.B
      enq.bits.data.issued      := false.B
      enq.bits.data.firstIssue  := false.B
      enq.bits.data.blocked     := false.B
    }
    statusArrayIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.deqSelOH.valid  := finalDeqSelValidVec(i)
      deq.deqSelOH.bits   := finalDeqSelOHVec(i)
      deq.resp.valid      := io.deqResp(i).valid
      deq.resp.bits.addrOH := io.deqResp(i).bits.addrOH
      deq.resp.bits.success := io.deqResp(i).bits.success
      deq.resp.bits.dataInvalidSqIdx := 0.U.asTypeOf(deq.resp.bits.dataInvalidSqIdx)
      deq.resp.bits.respType := 0.U.asTypeOf(deq.resp.bits.respType)
    }
  }

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

  val payloadArrayRdata = Wire(Vec(params.numDeq, Output(new DynInst)))
  payloadArray.io match { case payloadArrayIO: DataArrayIO[DynInst] =>
    payloadArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := s0_doEnqSelValidVec(i)
      w.addr := s0_enqSelOHVec(i)
      w.data := io.enq(i).bits
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
      fuTypeNextVec(OHToUInt(oh)) := io.enq(i).bits.fuType
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

  mainDeqPolicy match { case dp =>
    dp.io.request       := canIssueVec.asUInt & commonAccept
    mainDeqSelValidVec  := dp.io.deqSelOHVec.map(oh => oh.valid)
    mainDeqSelOHVec     := dp.io.deqSelOHVec.map(oh => oh.bits)
  }

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeRegVec.map(fuType => Cat(fuCfgs.map(_.fuType.U === fuType)).orR).reverse).asUInt
  }

  protected val specialCanAcceptVec: Seq[IndexedSeq[Bool]] = specialFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    fuTypeRegVec.map(fuType =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR)
  }

  // One deq port only need one special deq policy
  val subDeqSelValidVec: Seq[Option[Vec[Bool]]] = subDeqPolicies.map(_.map(_ => Wire(Vec(params.numDeq, Bool()))))
  val subDeqSelOHVec: Seq[Option[Vec[UInt]]] = subDeqPolicies.map(_.map(_ => Wire(Vec(params.numDeq, UInt(params.numEntries.W)))))

  subDeqPolicies.zipWithIndex.map { case (dpOption: Option[DeqPolicy], i) =>
    dpOption.zip(specialCanAcceptVec).map { case (dp: DeqPolicy, canAccept: IndexedSeq[Bool]) =>
      dp.io.request             := canIssueVec.asUInt & VecInit(canAccept).asUInt
      subDeqSelValidVec(i).get  := dp.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec(i).get     := dp.io.deqSelOHVec.map(oh => oh.bits)
    }
  }

  val mainDeqCanAccept: IndexedSeq[Bool] = (mainDeqOH zip canAcceptVec).map { case (deqOH, accept) =>
    (deqOH & accept).orR
  }

  subDeqPolicies.zipWithIndex.foreach {
    case (x: Option[DeqPolicy], i) =>
      x.map(_ => {
        // If the inst selected by mainDeqPolicy cannot be accepted, use specialDeqPolicy instead
        when (!mainDeqCanAccept(i)) {
          finalDeqSelValidVec(i)  := subDeqSelValidVec(i).get.head
          finalDeqSelOHVec(i)     := subDeqSelOHVec(i).get.head
        }
      })
  }

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i)
    deq.bits.addrOH          := finalDeqSelOHVec(i)
    deq.bits.common.fuType   := payloadArrayRdata(i).fuType
    deq.bits.common.fuOpType := payloadArrayRdata(i).fuOpType
    deq.bits.common.rfWen.foreach(_ := payloadArrayRdata(i).rfWen)
    deq.bits.common.fpWen.foreach(_ := payloadArrayRdata(i).fpWen)
    deq.bits.common.vecWen.foreach(_ := payloadArrayRdata(i).vecWen)
    deq.bits.common.flushPipe.foreach(_ := payloadArrayRdata(i).flushPipe)
    deq.bits.common.pdest := payloadArrayRdata(i).pdest
    deq.bits.common.robIdx := payloadArrayRdata(i).robIdx
    deq.bits.common.imm := immArrayRdataVec(i)
    deq.bits.rf.zip(payloadArrayRdata(i).psrc).foreach { case (rf, psrc) =>
      rf.foreach(_.addr := psrc) // psrc in payload array can be pregIdx of IntRegFile or VfRegFile
    }
    deq.bits.srcType.zip(payloadArrayRdata(i).srcType).foreach { case (sink, source) =>
      sink := source
    }
    deq.bits.immType := payloadArrayRdata(i).selImm
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
}

class IssueQueueJumpBundle extends Bundle {
  val pc = UInt(VAddrData().dataWidth.W)
  val target = UInt(VAddrData().dataWidth.W)
}

class IssueQueueMemBundle()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val feedback = Vec(params.numDeq, new MemRSFeedbackIO)
  val checkwait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
//    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput(params.exuParams))))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
}

class IssueQueueLoadBundle()(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
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
//      w.data := io.enqJmp.get(i).pc
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
  }}
}

class IssueQueueVfImp(override val wrapper: IssueQueue)(implicit p: Parameters, iqParams: IssueBlockParams)
  extends IssueQueueImp(wrapper)
{
  private val numPSrc = 5 // Todo: imm
  private val numLSrc = 3 // Todo: imm

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      for (j <- 0 until numPSrc) {
        enq.bits.data.srcState(j) := io.enq(i).bits.srcState(j)
        enq.bits.data.psrc(j)     := io.enq(i).bits.psrc(j)
      }
      for (j <- 0 until numLSrc) {
        enq.bits.data.srcType(j) := io.enq(i).bits.srcType(j)
      }
      enq.bits.data.srcType(3) := SrcType.vp // v0: mask src
      enq.bits.data.srcType(4) := SrcType.vp // vl&vtype
    }
  }
}

