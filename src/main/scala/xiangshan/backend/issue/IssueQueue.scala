package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasCircularQueuePtrHelper
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.backend.Bundles.{DynInst, IssueQueueIssueBundle, IssueQueueWakeUpBundle}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.exu.ExeUnitParams

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
  val flush = Flipped(ValidIO(new Redirect))

  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val deq: MixedVec[DecoupledIO[IssueQueueIssueBundle]] = params.genIssueDecoupledBundle
  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og0Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val og1Resp = Vec(params.numDeq, Flipped(ValidIO(new IssueQueueDeqRespBundle)))
  val wbBusyRead = Input(params.genFuBusyTableReadBundle)
  val wakeup = Vec(params.numWakeupFromWB, Flipped(ValidIO(new IssueQueueWakeUpBundle(params.pregBits))))
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))
  // Todo: wake up bundle
}

class IssueQueueImp(override val wrapper: IssueQueue)(implicit p: Parameters, val params: IssueBlockParams)
  extends LazyModuleImp(wrapper)
  with HasXSParameter {

  println(s"[IssueQueueImp] ${params.getIQName} wakeupFromWB: ${params.numWakeupFromWB}, " +
    s"numEntries: ${params.numEntries}, numRegSrc: ${params.numRegSrc}")

  require(params.numExu <= 2, "IssueQueue has not supported more than 2 deq ports")
  val deqFuCfgs     : Seq[Seq[FuConfig]] = params.exuBlockParams.map(_.fuConfigs)
  val fuLatencyMaps :  Seq[Option[Seq[(Int, Int)]]]  = params.exuBlockParams.map(x => x.fuLatencyMap)
  val latencyValMaxs: Seq[Option[Int]] = params.exuBlockParams.map(x => x.latencyValMax)
  val allDeqFuCfgs: Seq[FuConfig] = params.exuBlockParams.flatMap(_.fuConfigs)
  val fuCfgsCnt     : Map[FuConfig, Int] = allDeqFuCfgs.groupBy(x => x).map { case (cfg, cfgSeq) => (cfg, cfgSeq.length) }
  val commonFuCfgs  : Seq[FuConfig] = fuCfgsCnt.filter(_._2 > 1).keys.toSeq
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
  val fuBusyTable = latencyValMaxs.map { case y => if (y.getOrElse(0)>0) Some(Reg(UInt(y.getOrElse(1).W))) else None }

  // Wires
  val resps = params.schdType match {
    case IntScheduler() => Seq(io.deqResp, io.og0Resp, io.og1Resp)
    case MemScheduler() => Seq(io.deqResp, io.og1Resp)
    case VfScheduler() => Seq(io.deqResp, io.og1Resp)
    case _ => null
  }
  val fuBusyTableMask = Wire(Vec(params.numDeq, UInt(params.numEntries.W)))
  val s0_enqValidVec = io.enq.map(_.valid)
  val s0_enqSelValidVec = Wire(Vec(params.numEnq, Bool()))
  val s0_enqSelOHVec = Wire(Vec(params.numEnq, UInt(params.numEntries.W)))
  val s0_enqNotFlush = !io.flush.valid
  val s0_enqBits = WireInit(VecInit(io.enq.map(_.bits)))
  val s0_doEnqSelValidVec = s0_enqSelValidVec.map(_ && s0_enqNotFlush)
  val s0_doEnqOH: IndexedSeq[UInt] = (s0_doEnqSelValidVec zip s0_enqSelOHVec).map { case (valid, oh) =>
    Mux(valid, oh, 0.U)
  }

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

  val wakeupEnqSrcStateBypass = Wire(Vec(io.enq.size, Vec(io.enq.head.bits.srcType.size, SrcState())))
  for (i <- io.enq.indices) {
    for (j <- s0_enqBits(i).srcType.indices) {
      wakeupEnqSrcStateBypass(i)(j) := Cat(
        io.wakeup.map(x => x.bits.wakeUp(Seq((s0_enqBits(i).psrc(j), s0_enqBits(i).srcType(j))), x.valid).head)
      ).orR
    }
  }

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.flush  <> io.flush
    statusArrayIO.wakeup <> io.wakeup
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
      enq.bits.data.ready       := false.B
      enq.bits.data.issued      := false.B
      enq.bits.data.firstIssue  := false.B
      enq.bits.data.blocked     := false.B
    }
    statusArrayIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.deqSelOH.valid  := finalDeqSelValidVec(i)
      deq.deqSelOH.bits   := finalDeqSelOHVec(i)
    }
    statusArrayIO.deqResp.zipWithIndex.foreach { case (deqResp, i) =>
      deqResp.valid      := io.deqResp(i).valid
      deqResp.bits.addrOH := io.deqResp(i).bits.addrOH
      deqResp.bits.success := io.deqResp(i).bits.success
      deqResp.bits.dataInvalidSqIdx := io.deqResp(i).bits.dataInvalidSqIdx
      deqResp.bits.respType := io.deqResp(i).bits.respType
      deqResp.bits.rfWen := io.deqResp(i).bits.rfWen
      deqResp.bits.fuType := io.deqResp(i).bits.fuType
    }
    statusArrayIO.og0Resp.zipWithIndex.foreach { case (og0Resp, i) =>
      og0Resp.valid := io.og0Resp(i).valid
      og0Resp.bits.addrOH := io.og0Resp(i).bits.addrOH
      og0Resp.bits.success := io.og0Resp(i).bits.success
      og0Resp.bits.dataInvalidSqIdx := io.og0Resp(i).bits.dataInvalidSqIdx
      og0Resp.bits.respType := io.og0Resp(i).bits.respType
      og0Resp.bits.rfWen := io.og0Resp(i).bits.rfWen
      og0Resp.bits.fuType := io.og0Resp(i).bits.fuType
    }
    statusArrayIO.og1Resp.zipWithIndex.foreach { case (og1Resp, i) =>
      og1Resp.valid := io.og1Resp(i).valid
      og1Resp.bits.addrOH := io.og1Resp(i).bits.addrOH
      og1Resp.bits.success := io.og1Resp(i).bits.success
      og1Resp.bits.dataInvalidSqIdx := io.og1Resp(i).bits.dataInvalidSqIdx
      og1Resp.bits.respType := io.og1Resp(i).bits.respType
      og1Resp.bits.rfWen := io.og1Resp(i).bits.rfWen
      og1Resp.bits.fuType := io.og1Resp(i).bits.fuType
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
      dp.io.request             := canIssueVec.asUInt & VecInit(deqCanAcceptVec(i)).asUInt & (~fuBusyTableMask(i)).asUInt()
      subDeqSelValidVec(i).get  := dp.io.deqSelOHVec.map(oh => oh.valid)
      subDeqSelOHVec(i).get     := dp.io.deqSelOHVec.map(oh => oh.bits)
    }
  }

  finalDeqSelValidVec(0) := subDeqSelValidVec(0).getOrElse(Seq(0.U)).head
  finalDeqSelOHVec(0)    := subDeqSelOHVec(0).getOrElse(Seq(0.U)).head
  if(params.numDeq == 2){
    val isSame = subDeqSelOHVec(0).getOrElse(Seq(0.U)).head === subDeqSelOHVec(1).getOrElse(Seq(0.U)).head
    finalDeqSelValidVec(1) := Mux(isSame,
                                  subDeqSelValidVec(1).getOrElse(Seq(0.U)).last,
                                  subDeqSelValidVec(1).getOrElse(Seq(0.U)).head)
    finalDeqSelOHVec(1)    := Mux(isSame,
                                  subDeqSelOHVec(1).getOrElse(Seq(0.U)).last,
                                  subDeqSelOHVec(1).getOrElse(Seq(0.U)).head)
  }

  // fuBusyTable write
  for (i <- 0 until params.numDeq){
    if (fuBusyTable(i).nonEmpty) {
      val isLatencyNumVec = Mux(resps(0)(i).valid && resps(0)(i).bits.respType === RSFeedbackType.issueSuccess,
        Cat((0 until latencyValMaxs(i).get).map { case num =>
          val latencyNumFuType = fuLatencyMaps(i).get.filter(_._2 == num+1).map(_._1) // futype with latency equal to num+1
          val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(io.deqResp(i).bits.addrOH)) === futype.U)).asUInt().orR() // The latency of the deq inst is Num
          isLatencyNum
        }),
        0.U
      ) // |  when N cycle is 2 latency, N+1 cycle could not 1 latency
      val isLNumVecOg0 = WireInit(~(0.U.asTypeOf(isLatencyNumVec)))
      isLNumVecOg0 := Mux(resps(1)(i).valid && (resps(1)(i).bits.respType === RSFeedbackType.rfArbitFail || resps(1)(i).bits.respType === RSFeedbackType.fuBusy),
        ~(Cat(Cat((0 until latencyValMaxs(i).get).map { case num =>
          val latencyNumFuType = fuLatencyMaps(i).get.filter(_._2 == num+1).map(_._1) // futype with latency equal to num+1
          val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(io.og0Resp(i).bits.addrOH)) === futype.U)).asUInt().orR() // The latency of the deq inst is Num
          isLatencyNum
        }), 0.U(1.W))),
        ~(0.U.asTypeOf(isLatencyNumVec))
        // & ~
      )
      val isLNumVecOg1 = WireInit(~(0.U.asTypeOf(isLatencyNumVec)))
      if(resps.length == 3){
        isLNumVecOg1 := Mux(resps(2)(i).valid && resps(2)(i).bits.respType === RSFeedbackType.fuBusy,
          ~(Cat(Cat((0 until latencyValMaxs(i).get).map { case num =>
            val latencyNumFuType = fuLatencyMaps(i).get.filter(_._2 == num+1).map(_._1) // futype with latency equal to num+1
            val isLatencyNum = Cat(latencyNumFuType.map(futype => fuTypeRegVec(OHToUInt(io.og1Resp(i).bits.addrOH)) === futype.U)).asUInt().orR() // The latency of the deq inst is Num
            isLatencyNum
          }), 0.U(2.W))),
          ~(0.U.asTypeOf(isLatencyNumVec))
        )
        // & ~
      }

      fuBusyTable(i).get := ((fuBusyTable(i).get << 1.U).asUInt() | isLatencyNumVec) & isLNumVecOg0.asUInt() & isLNumVecOg1.asUInt()
    }
  }
  // fuBusyTable read
  for (i <- 0 until params.numDeq){
    if(fuBusyTable(i).nonEmpty){
      val isReadLatencyNumVec2 = fuBusyTable(i).get.asBools().reverse.zipWithIndex.map { case (en, idx) =>
        val isLatencyNumVec = WireInit(0.U(params.numEntries.W))
        when(en) {
          isLatencyNumVec := VecInit(fuTypeRegVec.map { case futype =>
            val latencyNumFuType = fuLatencyMaps(i).get.filter(_._2 == idx).map(_._1)
            val isLatencyNum = Cat(latencyNumFuType.map(_.U === futype)).asUInt().orR()
            isLatencyNum
          }).asUInt()
        }
        isLatencyNumVec
      }
      val isWBReadLatencyNumVec2 = io.wbBusyRead(i).asBools().reverse.zipWithIndex.map { case (en, idx) =>
        val isLatencyNumVec = WireInit(0.U(params.numEntries.W))
        when(en) {
          isLatencyNumVec := VecInit(fuTypeRegVec.map { case futype =>
            val latencyNumFuType = fuLatencyMaps(i).get.filter(_._2 == idx).map(_._1)
            val isLatencyNum = Cat(latencyNumFuType.map(_.U === futype)).asUInt().orR()
            isLatencyNum
          }).asUInt()
        }
        isLatencyNumVec
      }
      if ( latencyValMaxs(i).get > 1 ){
        fuBusyTableMask(i) := isReadLatencyNumVec2.reduce(_ | _) | isWBReadLatencyNumVec2.reduce(_ | _)
      }else{
        fuBusyTableMask(i) := isReadLatencyNumVec2.head | isWBReadLatencyNumVec2.head
      }
    } else {
      fuBusyTableMask(i) := 0.U(params.numEntries.W)
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
      for (j <- 0 until numLSrc) {
        enq.bits.data.srcState(j) := s0_enqBits(i).srcState(j) | wakeupEnqSrcStateBypass(i)(j)
        enq.bits.data.psrc(j)     := s0_enqBits(i).psrc(j)
        enq.bits.data.srcType(j) := s0_enqBits(i).srcType(j)
      }
      // enq.bits.data.srcType(3) := SrcType.vp // v0: mask src
      // enq.bits.data.srcType(4) := SrcType.vp // vl&vtype
    }
  }
  io.deq.zipWithIndex.foreach{ case (deq, i) => {
    deq.bits.common.fpu.get := payloadArrayRdata(i).fpu
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

  require(params.StdCnt == 0 && (params.LduCnt + params.StaCnt) > 0, "IssueQueueMemAddrImp can only be instance of MemAddr IQ")

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
      enqData.blocked := s0_enqBits(i).loadWaitBit
      enqData.mem.get.strictWait := s0_enqBits(i).loadWaitStrict
      enqData.mem.get.waitForStd := false.B
      enqData.mem.get.waitForRobIdx := s0_enqBits(i).waitForRobIdx
      enqData.mem.get.waitForSqIdx := 0.U.asTypeOf(enqData.mem.get.waitForSqIdx) // generated by sq, will be updated later
      enqData.mem.get.sqIdx := s0_enqBits(i).sqIdx
    }

    statusArray.io.deqResp.zipWithIndex.foreach { case (deqResp, i) =>
      deqResp.valid        := io.deqResp(i).valid
      deqResp.bits.addrOH  := io.deqResp(i).bits.addrOH
      deqResp.bits.success := io.deqResp(i).bits.success
      deqResp.bits.dataInvalidSqIdx := io.deqResp(i).bits.dataInvalidSqIdx
      deqResp.bits.respType := io.deqResp(i).bits.respType
      deqResp.bits.rfWen := io.deqResp(i).bits.rfWen
      deqResp.bits.fuType := io.deqResp(i).bits.fuType
    }

    statusArray.io.og0Resp.zipWithIndex.foreach { case (og0Resp, i) =>
      og0Resp.valid := io.og0Resp(i).valid
      og0Resp.bits.addrOH := io.og0Resp(i).bits.addrOH
      og0Resp.bits.success := io.og0Resp(i).bits.success
      og0Resp.bits.dataInvalidSqIdx := io.og0Resp(i).bits.dataInvalidSqIdx
      og0Resp.bits.respType := io.og0Resp(i).bits.respType
      og0Resp.bits.rfWen := io.og0Resp(i).bits.rfWen
      og0Resp.bits.fuType := io.og0Resp(i).bits.fuType
    }
    statusArray.io.og1Resp.zipWithIndex.foreach { case (og1Resp, i) =>
      og1Resp.valid := io.og1Resp(i).valid
      og1Resp.bits.addrOH := io.og1Resp(i).bits.addrOH
      og1Resp.bits.success := io.og1Resp(i).bits.success
      og1Resp.bits.dataInvalidSqIdx := io.og1Resp(i).bits.dataInvalidSqIdx
      og1Resp.bits.respType := io.og1Resp(i).bits.respType
      og1Resp.bits.rfWen := io.og1Resp(i).bits.rfWen
      og1Resp.bits.fuType := io.og1Resp(i).bits.fuType
    }

    statusArray.io.fromMem.get.slowResp.zipWithIndex.foreach { case (slowResp, i) =>
      slowResp.valid                 := memIO.feedbackIO(i).feedbackSlow.valid
      slowResp.bits.addrOH           := UIntToOH(memIO.feedbackIO(i).feedbackSlow.bits.rsIdx)
      slowResp.bits.success          := memIO.feedbackIO(i).feedbackSlow.bits.hit
      slowResp.bits.respType         := Mux(memIO.feedbackIO(i).feedbackSlow.bits.hit, 0.U, RSFeedbackType.feedbackInvalid)
      slowResp.bits.dataInvalidSqIdx := memIO.feedbackIO(i).feedbackSlow.bits.dataInvalidSqIdx
      slowResp.bits.rfWen := DontCare
      slowResp.bits.fuType := DontCare
    }

    statusArray.io.fromMem.get.fastResp.zipWithIndex.foreach { case (fastResp, i) =>
      fastResp.valid                 := memIO.feedbackIO(i).feedbackFast.valid
      fastResp.bits.addrOH           := UIntToOH(memIO.feedbackIO(i).feedbackFast.bits.rsIdx)
      fastResp.bits.success          := false.B
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