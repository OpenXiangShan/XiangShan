package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, IssueQueueIssueBundle, IssueQueueWakeUpBundle}
import xiangshan.v2backend._
import xiangshan.{HasXSParameter, MemRSFeedbackIO, Redirect, XSBundle}

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

  def genIssueBundle: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = {
    MixedVec(this.exuParams.map(x => DecoupledIO(new IssueQueueIssueBundle(x, dataBits, pregBits, vaddrBits))))
  }
}

object DummyIQParams {
  def apply(): IssueQueueParams = {
    SchdBlockParams.dummyIntParams().issueBlockParams(0).genIqParams
  }
}

class IssueQueue(params: IssueQueueParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  implicit val iqParams = params

  lazy val module = iqParams.schdType match {
    case IntScheduler() => new IssueQueueIntImp(this)
    case _ => new IssueQueueImp(this)
  }
}

class IssueQueueStatusBundle(numEnq: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val leftVec = Output(Vec(numEnq + 1, Bool()))
}

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueQueueParams) extends StatusArrayDeqRespBundle

class IssueQueueIO()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))

  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val deq = params.genIssueBundle
  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val wakeup = Vec(params.numWakeupFromWB, Flipped(ValidIO(new IssueQueueWakeUpBundle)))
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))
  // Todo: wake up bundle
}

class IssueQueueImp(outer: IssueQueue)(implicit p: Parameters, val params: IssueQueueParams) extends LazyModuleImp(outer) with HasXSParameter{

  require(params.exuParams.length <= 2, "IssueQueue has not supported more than 2 deq ports")
  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuParams.map(_.fuConfigs)
  val allDeqFuCfgs  : Seq[FuConfig] = params.exuParams.flatMap(_.fuConfigs)
  val fuCfgsCnt     : Map[FuConfig, Int] = allDeqFuCfgs.groupBy(x => x).map { case (cfg, cfgSeq) => (cfg, cfgSeq.length) }
  val commonFuCfgs  : Seq[FuConfig] = fuCfgsCnt.filter(_._2 > 1).keys.toSeq
  val specialFuCfgs : Seq[Seq[FuConfig]] = params.exuParams.map(_.fuConfigs.filterNot(commonFuCfgs.contains))

  lazy val io = IO(new IssueQueueIO())

  // Modules
  val statusArray   = Module(new StatusArray)
  val immArray      = Module(new DataArray(UInt(XLEN.W), params.numDeq, params.numEnq, params.numEntries))
  val payloadArray  = Module(new DataArray(Output(new DynInst), params.numDeq, params.numEnq, params.numEntries))
  val enqPolicy     = Module(new EnqPolicy)
  val mainDeqPolicy = Module(new DeqPolicy)
  val subDeqPolicies  = specialFuCfgs.map(x => if (x.nonEmpty) Some(Module(new DeqPolicy())) else None)

  // Wires
  val enqValidVec = io.enq.map(_.valid)
  val enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val enqSelOHVec = Wire(Vec(params.numEnq, UInt(params.numEntries.W)))
  val enqOH: IndexedSeq[UInt] = (enqSelValidVec zip enqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }
  val enqMask: UInt = enqOH.reduce(_ | _)

  val enqImmValidVec = io.enq.map(enq => enq.valid && enq.bits.imm.valid)
  val enqImmVec = VecInit(io.enq.map(_.bits.imm.bits))

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

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.flush  <> io.flush
    statusArrayIO.wakeup <> io.wakeup
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      enq.valid                 := enqSelValidVec(i)
      enq.bits.addrOH           := enqSelOHVec(i)
      val numSrcMin = enq.bits.data.srcState.length.min(io.enq(i).bits.srcState.length)
      for (j <- 0 until numSrcMin) {
        enq.bits.data.srcState(j) := io.enq(i).bits.srcState(j)
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
      deq.resp            := deqRespVec(i)
    }
  }

  val immArrayRdataVec = immArray.io.read.map(_.data)
  immArray.io match { case immArrayIO: DataArrayIO[UInt] =>
    immArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := enqSelValidVec(i) && enqImmValidVec(i)
      w.addr := enqSelOHVec(i)
      w.data := enqImmVec(i)
    }
    immArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := Mux(finalDeqSelValidVec(i), finalDeqSelOHVec(i), 0.U)
    }
  }

  val payloadArrayRdata = Wire(Vec(params.numDeq, Output(new DynInst)))
  payloadArray.io match { case payloadArrayIO: DataArrayIO[DynInst] =>
    payloadArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := enqSelValidVec(i)
      w.addr := enqSelOHVec(i)
      w.data := io.enq(i).bits
    }
    payloadArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := finalDeqSelOHVec(i)
      payloadArrayRdata(i) := r.data
    }
  }

  val fuTypeRegVec = Reg(Vec(params.numEntries, FuType()))
  val fuTypeNextVec = WireInit(fuTypeRegVec)
  fuTypeRegVec := fuTypeNextVec

  enqSelValidVec.zip(enqSelOHVec).zipWithIndex.foreach { case ((valid, oh), i) =>
    when (valid) {
      fuTypeNextVec(OHToUInt(oh)) := io.enq(i).bits.fuType
    }
  }

  enqPolicy match { case ep =>
    ep.io.valid     := validVec.asUInt
    enqSelValidVec  := ep.io.enqSelOHVec.map(oh => oh.valid)
    enqSelOHVec     := ep.io.enqSelOHVec.map(oh => oh.bits)
  }

  mainDeqPolicy match { case dp =>
    dp.io.request       := canIssueVec.asUInt
    mainDeqSelValidVec  := dp.io.deqSelOHVec.map(oh => oh.valid)
    mainDeqSelOHVec     := dp.io.deqSelOHVec.map(oh => oh.bits)
  }

  // if deq port can accept the uop
  protected val canAcceptVec: Seq[UInt] = deqFuCfgs.map { fuCfgs: Seq[FuConfig] =>
    Cat(fuTypeRegVec.map(fuType =>
      Cat(fuCfgs.map(_.fuType.U === fuType)).asUInt.orR)).asUInt
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

  subDeqPolicies.zipWithIndex.map{case (x, i) => x.map(_ => {
    when (!mainDeqCanAccept(i)) {
      finalDeqSelValidVec(i)  := subDeqSelValidVec(i).get.head
      finalDeqSelOHVec(i)     := subDeqSelOHVec(i).get.head
    }
  })}

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.valid                := finalDeqSelValidVec(i)
    deq.bits.common.fuType   := payloadArrayRdata(i).fuType
    deq.bits.common.fuOpType := payloadArrayRdata(i).fuOpType
    deq.bits.common.rfWen    := payloadArrayRdata(i).rfWen
    deq.bits.common.fpWen    := payloadArrayRdata(i).fpWen
    deq.bits.common.vecWen   := payloadArrayRdata(i).vecWen
    deq.bits.rf.zip(payloadArrayRdata(i).psrc).foreach { case (rf, psrc) =>
      rf.addr := psrc
    }
  }

  // Todo: better counter implementation
  private val validCnt = PopCount(validVec)
  private val enqSelCnt = PopCount(enqSelValidVec)
  private val validCntNext = validCnt + enqSelCnt
  io.status.full := validVec.asUInt.andR
  io.status.empty := !validVec.asUInt.orR
  io.status.leftVec(0) := io.status.full
  for (i <- 0 until params.numEnq) {
    io.status.leftVec(i + 1) := validCnt === (i + 1).U
  }
  io.statusNext.full := validCntNext === params.numEntries.U
  io.statusNext.empty := validCntNext === 0.U // always false now
  io.statusNext.leftVec(0) := io.statusNext.full
  for (i <- 0 until params.numEnq) {
    io.statusNext.leftVec(i + 1) := validCntNext === (i + 1).U
  }

}

class IssueQueueJumpBundle(vaddrBits: Int) extends Bundle {
  val pc = Input(UInt(vaddrBits.W))
  val target = Input(UInt(vaddrBits.W))
}

class IssueQueueMemBundle()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val feedback = Vec(params.numDeq, new MemRSFeedbackIO)
  val checkwait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
//    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput(params.exuParams))))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
}

class IssueQueueLoadBundle()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val fastMatch = UInt(exuParameters.LduCnt.W)
  val fastImm = UInt(12.W)
}

class IssueQueueIntIO()(implicit p: Parameters, params: IssueQueueParams) extends IssueQueueIO {
  val jmp = if (params.hasJmpFu) Some(new IssueQueueJumpBundle(VAddrBits)) else None
}

class IssueQueueIntImp(outer: IssueQueue)(implicit p: Parameters, iqParams: IssueQueueParams)
  extends IssueQueueImp(outer)
{
  io.suggestName("none")
  override lazy val io = IO(new IssueQueueIntIO).suggestName("io")

  val pcMem = Reg(Vec(params.numEntries, UInt(VAddrBits.W)))
  val targetMem = Reg(Vec(params.numEntries, UInt(VAddrBits.W)))
  if (io.jmp.nonEmpty) {
    val jmp = io.jmp.get
    for (i <- 0 until params.numEntries) {
      when(enqMask(i)) {
        pcMem(i) := jmp.pc
        targetMem(i) := jmp.target
      }
    }
  }

  val pcRead = Wire(Vec(params.numDeq, UInt(VAddrBits.W)))
  val targetRead = Wire(Vec(params.numDeq, UInt(VAddrBits.W)))

  for (i <- 0 until params.numDeq) {
    pcRead(i) := Mux1H(finalDeqOH(i), pcMem)
    targetRead(i) := Mux1H(finalDeqOH(i), targetMem)
  }
  io.deq.zipWithIndex.foreach{ case (deq, i) =>
    deq.bits.jmp.foreach((deqJmp: IssueQueueJumpBundle) => {
      deqJmp.pc := pcRead(i)
      deqJmp.target := targetRead(i)
    }
  )}
}
