package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils.{MathUtils, OptionWrapper, XSError}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Utils.NOnes
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{LqPtr, MemWaitUpdateReq, SqPtr}
import xiangshan.backend.issue.EntryBundles._

class Entries(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  override def desiredName: String = params.getEntryName

  private val EnqEntryNum         = params.numEnq
  private val OthersEntryNum      = params.numEntries - params.numEnq
  val io = IO(new EntriesIO)

  // only memAddrIQ use it
  val memEtyResps: MixedVec[ValidIO[EntryDeqRespBundle]] = {
    if (params.isLdAddrIQ && !params.isStAddrIQ)
      MixedVecInit(io.og0Resp ++ io.og1Resp ++ io.finalIssueResp.get)
    else if (params.isLdAddrIQ && params.isStAddrIQ || params.isHyAddrIQ)
      MixedVecInit(io.og0Resp ++ io.og1Resp ++ io.finalIssueResp.get ++ io.fromMem.get.fastResp ++ io.fromMem.get.slowResp)
    else if (params.isMemAddrIQ)
      MixedVecInit(io.og0Resp ++ io.og1Resp ++ io.fromMem.get.fastResp ++ io.fromMem.get.slowResp)
    else MixedVecInit(Seq())
  }

  val resps: Vec[Vec[ValidIO[EntryDeqRespBundle]]] = VecInit(io.og0Resp, io.og1Resp, 0.U.asTypeOf(io.og0Resp), 0.U.asTypeOf(io.og0Resp))

  //Module
  val enqEntries          = Seq.fill(EnqEntryNum)(Module(EnqEntry(p, params)))
  val othersEntries       = Seq.fill(OthersEntryNum)(Module(OthersEntry(p, params)))
  val transPolicy         = Module(new EnqPolicy)

  //Wire
  //entries status
  val entries             = Wire(Vec(params.numEntries, ValidIO(new EntryBundle)))
  val robIdxVec           = Wire(Vec(params.numEntries, new RobPtr))
  val validVec            = Wire(Vec(params.numEntries, Bool()))
  val canIssueVec         = Wire(Vec(params.numEntries, Bool()))
  val fuTypeVec           = Wire(Vec(params.numEntries, FuType()))
  val isFirstIssueVec     = Wire(Vec(params.numEntries, Bool()))
  val issueTimerVec       = Wire(Vec(params.numEntries, UInt(2.W)))
  //src status
  val dataSourceVec       = Wire(Vec(params.numEntries, Vec(params.numRegSrc, DataSource())))
  val srcTimerVec         = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, UInt(3.W)))))
  val srcWakeUpL1ExuOHVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, ExuVec()))))
  //deq sel
  val deqSelVec           = Wire(Vec(params.numEntries, Bool()))
  val issueRespVec        = Wire(Vec(params.numEntries, ValidIO(new EntryDeqRespBundle)))
  val deqPortIdxWriteVec  = Wire(Vec(params.numEntries, UInt(1.W)))
  val deqPortIdxReadVec   = Wire(Vec(params.numEntries, UInt(1.W)))
  //trans sel
  val transSelVec         = Wire(Vec(EnqEntryNum, Vec(OthersEntryNum, Bool())))
  val transEntryDeqVec    = Wire(Vec(EnqEntryNum, ValidIO(new EntryBundle)))
  val transEntryEnqVec    = Wire(Vec(OthersEntryNum, ValidIO(new EntryBundle)))
  //debug
  val cancelVec           = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Bool())))
  //og0Cancel bypass
  val cancelByOg0Vec      = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Bool())))
  val uopIdxVec           = OptionWrapper(params.isVecMemIQ, Wire(Vec(params.numEntries, UopIdx())))

  val canTrans            = Wire(Bool())
  val enqReadyOthersVec   = Wire(Vec(OthersEntryNum, Bool()))
  val enqTransSelVec      = Wire(Vec(EnqEntryNum, Valid(UInt(OthersEntryNum.W))))

  io.transEntryDeqVec := transEntryDeqVec

  //enqEntries
  enqEntries.zipWithIndex.foreach { case (enqEntry, entryIdx) =>
    enqEntry.io.commonIn.enq                  := io.enq(entryIdx)
    enqEntry.io.commonIn.transSel             := canTrans && enqTransSelVec(entryIdx).valid
    EntriesConnect(enqEntry.io.commonIn, enqEntry.io.commonOut, entryIdx)
    enqEntry.io.enqDelayWakeUpFromWB          := RegNext(io.wakeUpFromWB)
    enqEntry.io.enqDelayWakeUpFromIQ          := RegNext(io.wakeUpFromIQ)
    enqEntry.io.enqDelayOg0Cancel             := RegNext(io.og0Cancel.asUInt)
    enqEntry.io.enqDelayLdCancel              := RegNext(io.ldCancel)
    transEntryDeqVec(entryIdx)                := enqEntry.io.transEntry
    // TODO: move it into EntriesConnect
    if (params.isVecMemIQ) {
      enqEntry.io.commonIn.fromLsq.get.sqDeqPtr := io.vecMemIn.get.sqDeqPtr
      enqEntry.io.commonIn.fromLsq.get.lqDeqPtr := io.vecMemIn.get.lqDeqPtr
    }
  }
  //othersEntries
  othersEntries.zipWithIndex.foreach { case (othersEntry, entryIdx) =>
    othersEntry.io.commonIn.enq               := transEntryEnqVec(entryIdx)
    EntriesConnect(othersEntry.io.commonIn, othersEntry.io.commonOut, entryIdx + EnqEntryNum)
    othersEntry.io.commonIn.transSel          := transSelVec.map(x => x(entryIdx)).reduce(_ | _)
    enqReadyOthersVec(entryIdx)               := othersEntry.io.enqReady
    if (params.isVecMemIQ) {
      othersEntry.io.commonIn.fromLsq.get.sqDeqPtr := io.vecMemIn.get.sqDeqPtr
      othersEntry.io.commonIn.fromLsq.get.lqDeqPtr := io.vecMemIn.get.lqDeqPtr
    }
  }


  deqSelVec.zip(deqPortIdxWriteVec).zipWithIndex.foreach { case ((deqSel, deqPortIdxWrite), i) =>
    val deqVec = io.deqSelOH.zip(io.deqReady).map(x => x._1.valid && x._1.bits(i) && x._2)
    deqPortIdxWrite := OHToUInt(deqVec)
    deqSel := deqVec.reduce(_ | _)
  }


  //transPolicy
  transPolicy.io.canEnq := enqReadyOthersVec.asUInt
  canTrans := PopCount(validVec.take(EnqEntryNum)) <= PopCount(enqReadyOthersVec)
  enqTransSelVec(0).valid := transPolicy.io.enqSelOHVec(0).valid
  enqTransSelVec(0).bits := transPolicy.io.enqSelOHVec(0).bits
  if (params.numEnq == 2) {
    enqTransSelVec(1).valid := Mux(!validVec(0), transPolicy.io.enqSelOHVec(0).valid, transPolicy.io.enqSelOHVec(1).valid)
    enqTransSelVec(1).bits := Mux(!validVec(0), transPolicy.io.enqSelOHVec(0).bits, transPolicy.io.enqSelOHVec(1).bits)
  }

  transSelVec.zip(enqTransSelVec).zipWithIndex.foreach { case ((selBools, selOH), enqIdx) =>
    selBools.zipWithIndex.foreach { case (selBool, othersIdx) =>
      selBool := canTrans && validVec(enqIdx) && selOH.valid && selOH.bits(othersIdx)
    }
  }

  //transEntryEnq
  transEntryEnqVec.zipWithIndex.foreach { case (transEntryEnq, othersIdx) =>
    val transEnqHit = transSelVec.map(x => x(othersIdx))
    transEntryEnq := Mux1H(transEnqHit, transEntryDeqVec)
  }
  if(backendParams.debugEn) {
    dontTouch(transEntryEnqVec)
  }

  //issueRespVec
  if (params.isVecMemIQ) {
    // vector memory IQ
    issueRespVec.zip(robIdxVec).zip(uopIdxVec.get).foreach { case ((issueResp, robIdx), uopIdx) =>
      val hitRespsVec = VecInit(resps.flatten.map(x =>
        x.valid && x.bits.robIdx === robIdx && x.bits.uopIdx.get === uopIdx
      ))
      issueResp.valid := hitRespsVec.reduce(_ | _)
      issueResp.bits := Mux1H(hitRespsVec, resps.flatten.map(_.bits))
    }
  } else if (params.isMemAddrIQ) {
    // scalar memory IQ
    issueRespVec.zip(robIdxVec).foreach { case (issueResp, robIdx) =>
      val hitRespsVec = VecInit(memEtyResps.map(x => x.valid && (x.bits.robIdx === robIdx)).toSeq)
      issueResp.valid := hitRespsVec.reduce(_ | _)
      issueResp.bits := Mux1H(hitRespsVec, memEtyResps.map(_.bits).toSeq)
    }
  }
  else {
    issueRespVec.zip(issueTimerVec).zip(deqPortIdxReadVec).foreach { case ((issueResp, issueTimer), deqPortIdx) =>
      val Resp = resps(issueTimer)(deqPortIdx)
      issueResp := Resp
    }
  }

  //deq
  val enqEntryOldest = io.enqEntryOldestSel.map { x =>
    Mux1H(x.bits, entries.take(EnqEntryNum))
  }
  val enqEntryOldestCancel = io.enqEntryOldestSel.map { x =>
    Mux1H(x.bits, cancelByOg0Vec.getOrElse(VecInit(Seq.fill(params.numEntries)(false.B))).take(EnqEntryNum))
  }
  val othersEntryOldest = io.othersEntryOldestSel.map { x =>
    Mux1H(x.bits, entries.drop(EnqEntryNum))
  }
  val othersEntryOldestCancel = io.othersEntryOldestSel.map { x =>
    Mux1H(x.bits, cancelByOg0Vec.getOrElse(VecInit(Seq.fill(params.numEntries)(false.B))).drop(EnqEntryNum))
  }

  if (params.deqFuSame) {
    val subDeqPolicyEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
    val subDeqPolicyValidVec = Wire(Vec(params.numDeq, Bool()))
    val subDeqPolicyCancelByOg0Vec = Wire(Vec(params.numDeq, Bool()))

    subDeqPolicyEntryVec(0) := PriorityMux(io.subDeqRequest.get(0), entries)
    subDeqPolicyEntryVec(1) := PriorityMux(Reverse(io.subDeqRequest.get(0)), entries.reverse)
    subDeqPolicyValidVec(0) := PopCount(io.subDeqRequest.get(0)) >= 1.U
    subDeqPolicyValidVec(1) := PopCount(io.subDeqRequest.get(0)) >= 2.U
    subDeqPolicyCancelByOg0Vec(0) := PriorityMux(io.subDeqRequest.get(0), cancelByOg0Vec.getOrElse(VecInit(Seq.fill(params.numEntries)(false.B))))
    subDeqPolicyCancelByOg0Vec(1) := PriorityMux(Reverse(io.subDeqRequest.get(0)), cancelByOg0Vec.getOrElse(VecInit(Seq.fill(params.numEntries)(false.B))).reverse)

    io.deqEntry(0) := Mux(io.othersEntryOldestSel(0).valid, othersEntryOldest(0), subDeqPolicyEntryVec(1))
    io.deqEntry(1) := subDeqPolicyEntryVec(0)
    io.cancelDeqVec(0) := Mux(io.othersEntryOldestSel(0).valid, othersEntryOldestCancel(0), subDeqPolicyCancelByOg0Vec(1))
    io.cancelDeqVec(1) := subDeqPolicyCancelByOg0Vec(0)

    when (subDeqPolicyValidVec(0)) {
      assert(Mux1H(io.subDeqSelOH.get(0), entries).bits.status.robIdx === subDeqPolicyEntryVec(0).bits.status.robIdx, "subDeqSelOH(0) is not the same\n")
    }
    when (subDeqPolicyValidVec(1)) {
      assert(Mux1H(io.subDeqSelOH.get(1), entries).bits.status.robIdx === subDeqPolicyEntryVec(1).bits.status.robIdx, "subDeqSelOH(1) is not the same\n")
    }
  }
  else {
    io.othersEntryOldestSel.zipWithIndex.foreach { case (x, i) =>
      io.deqEntry(i) := Mux(x.valid, othersEntryOldest(i), enqEntryOldest(i))
      io.cancelDeqVec(i) := Mux(x.valid, othersEntryOldestCancel(i), enqEntryOldestCancel(i))
    }
  }

  if (params.hasIQWakeUp) {
    cancelByOg0Vec.get.zip(srcWakeUpL1ExuOHVec.get).zip(srcTimerVec.get).foreach{ case ((cancelByOg0: Bool, l1ExuOH: Vec[Vec[Bool]]), srcTimer: Vec[UInt]) =>
      cancelByOg0 := l1ExuOH.zip(srcTimer).map {
        case(exuOH, srcTimer) =>
          (exuOH.asUInt & io.og0Cancel.asUInt).orR && srcTimer === 1.U
      }.reduce(_ | _)
    }
  }

  io.valid                      := validVec.asUInt
  io.canIssue                   := canIssueVec.asUInt
  io.fuType                     := fuTypeVec
  io.dataSources                := dataSourceVec
  io.srcWakeUpL1ExuOH.foreach(_ := srcWakeUpL1ExuOHVec.get.map(x => VecInit(x.map(_.asUInt))))
  io.srcTimer.foreach(_         := srcTimerVec.get)
  io.isFirstIssue.zipWithIndex.foreach{ case (isFirstIssue, deqIdx) =>
    isFirstIssue                := io.deqSelOH(deqIdx).valid && Mux1H(io.deqSelOH(deqIdx).bits, isFirstIssueVec)
  }
  io.transSelVec.zip(transSelVec).foreach { case (sink, source) =>
    sink                        := source.asUInt
  }
  io.robIdx.foreach(_           := robIdxVec)
  io.uopIdx.foreach(_           := uopIdxVec.get)
  io.rsFeedback                 := 0.U.asTypeOf(io.rsFeedback)  //should be removed
  io.cancel.foreach(_           := cancelVec.get)               //for debug

  def EntriesConnect(in: CommonInBundle, out: CommonOutBundle, entryIdx: Int) = {
    in.flush                    := io.flush
    in.wakeUpFromWB             := io.wakeUpFromWB
    in.wakeUpFromIQ             := io.wakeUpFromIQ
    in.og0Cancel                := io.og0Cancel
    in.og1Cancel                := io.og1Cancel
    in.ldCancel                 := io.ldCancel
    in.deqSel                   := deqSelVec(entryIdx)
    in.deqPortIdxWrite          := deqPortIdxWriteVec(entryIdx)
    in.issueResp                := issueRespVec(entryIdx)
    if (params.isMemAddrIQ) {
      in.fromMem.get.stIssuePtr := io.fromMem.get.stIssuePtr
      in.fromMem.get.memWaitUpdateReq := io.fromMem.get.memWaitUpdateReq
    }
    if (params.isVecMemIQ) {
      in.fromLsq.get.sqDeqPtr := io.vecMemIn.get.sqDeqPtr
      in.fromLsq.get.lqDeqPtr := io.vecMemIn.get.lqDeqPtr
    }
    validVec(entryIdx)          := out.valid
    canIssueVec(entryIdx)       := out.canIssue
    fuTypeVec(entryIdx)         := out.fuType
    robIdxVec(entryIdx)         := out.robIdx
    dataSourceVec(entryIdx)     := out.dataSource
    isFirstIssueVec(entryIdx)   := out.isFirstIssue
    entries(entryIdx)           := out.entry
    deqPortIdxReadVec(entryIdx) := out.deqPortIdxRead
    issueTimerVec(entryIdx)     := out.issueTimerRead
    if (params.hasIQWakeUp) {
      srcWakeUpL1ExuOHVec.get(entryIdx) := out.srcWakeUpL1ExuOH.get
      srcTimerVec.get(entryIdx) := out.srcTimer.get
      cancelVec.get(entryIdx)   := out.cancel.get
    }
    if (params.isVecMemIQ) {
      uopIdxVec.get(entryIdx)   := out.uopIdx.get
    }
  }
}

class EntriesIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val flush               = Flipped(ValidIO(new Redirect))
  //enq
  val enq                 = Vec(params.numEnq, Flipped(ValidIO(new EntryBundle)))
  val og0Resp             = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
  val og1Resp             = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
  val finalIssueResp      = OptionWrapper(params.LdExuCnt > 0, Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle))))
  //deq sel
  val deqReady            = Vec(params.numDeq, Input(Bool()))
  val deqSelOH            = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
  val enqEntryOldestSel   = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEnq.W))))
  val othersEntryOldestSel= Vec(params.numDeq, Flipped(ValidIO(UInt((params.numEntries - params.numEnq).W))))
  val subDeqRequest       = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  val subDeqSelOH         = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  // wakeup
  val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel           = Input(ExuOH(backendParams.numExu))
  val og1Cancel           = Input(ExuOH(backendParams.numExu))
  val ldCancel            = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
  //trans output
  val transEntryDeqVec    = Vec(params.numEnq, ValidIO(new EntryBundle))
  val transSelVec         = Output(Vec(params.numEnq, UInt((params.numEntries - params.numEnq).W)))
  //entries status
  val valid               = Output(UInt(params.numEntries.W))
  val canIssue            = Output(UInt(params.numEntries.W))
  val fuType              = Vec(params.numEntries, Output(FuType()))
  val dataSources         = Vec(params.numEntries, Vec(params.numRegSrc, Output(DataSource())))
  val srcWakeUpL1ExuOH    = OptionWrapper(params.hasIQWakeUp, Vec(params.numEntries, Vec(params.numRegSrc, Output(ExuOH()))))
  val srcTimer            = OptionWrapper(params.hasIQWakeUp, Vec(params.numEntries, Vec(params.numRegSrc, Output(UInt(3.W)))))
  //deq status
  val isFirstIssue        = Vec(params.numDeq, Output(Bool()))
  val deqEntry            = Vec(params.numDeq, ValidIO(new EntryBundle))
  val cancelDeqVec        = Vec(params.numDeq, Output(Bool()))
  // mem only
  val fromMem = if (params.isMemAddrIQ) Some(new Bundle {
    val stIssuePtr        = Input(new SqPtr)
    val memWaitUpdateReq  = Flipped(new MemWaitUpdateReq)
    val slowResp          = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
    val fastResp          = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
  }) else None
  val vecMemIn = OptionWrapper(params.isVecMemIQ, new Bundle {
    val sqDeqPtr = Input(new SqPtr)
    val lqDeqPtr = Input(new LqPtr)
  })

  val robIdx = OptionWrapper(params.isVecMemIQ, Output(Vec(params.numEntries, new RobPtr)))
  val uopIdx = OptionWrapper(params.isVecMemIQ, Output(Vec(params.numEntries, UopIdx())))

  val rsFeedback          = Output(Vec(5, Bool()))

  // debug
  val cancel              = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numEntries, Bool())))

  def wakeup = wakeUpFromWB ++ wakeUpFromIQ
}
