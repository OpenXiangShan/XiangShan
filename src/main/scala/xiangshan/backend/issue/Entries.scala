package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.vector.Utils.NOnes
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.issue.EntryBundles._
import xiangshan.mem.{LqPtr, SqPtr}
import xiangshan.mem.Bundles.MemWaitUpdateReqBundle

class Entries(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  override def desiredName: String = params.getEntryName

  require(params.numEnq <= 2, "number of enq should be no more than 2")

  private val EnqEntryNum         = params.numEnq
  private val OthersEntryNum      = params.numEntries - params.numEnq
  private val SimpEntryNum        = params.numSimp
  private val CompEntryNum        = params.numComp
  val io = IO(new EntriesIO)

  val fakeS1Resp = Option.when(params.needFakeS1Resp)(0.U.asTypeOf(io.og0Resp))
  val allResps = Seq(
    Some(io.og0Resp),
    Some(io.og1Resp),
    io.og2Resp.orElse(io.s0Resp),
    if (io.og2Resp.nonEmpty) io.s0Resp else fakeS1Resp,
    io.s2Resp.orElse(io.snResp)
  ).filter(_.nonEmpty).map(_.get)
  assert(allResps.length == params.issueTimerMaxValue + 1, "allResps.length == params.issueTimerMaxValue + 1")
  val resps = Wire(Vec(allResps.length, chiselTypeOf(io.og0Resp)))
  resps := allResps

  //Module
  val enqEntries          = Seq.fill(EnqEntryNum)(Module(EnqEntry(isComp = true)(p, params)))
  val othersEntriesSimp   = Seq.fill(SimpEntryNum)(Module(OthersEntry(isComp = false)(p, params)))
  val othersEntriesComp   = Seq.fill(CompEntryNum)(Module(OthersEntry(isComp = true)(p, params)))
  val othersEntries       = othersEntriesSimp ++ othersEntriesComp
  val othersTransPolicy   = OptionWrapper(params.isAllComp || params.isAllSimp, Module(new EnqPolicy))
  val simpTransPolicy     = OptionWrapper(params.hasCompAndSimp, Module(new EnqPolicy))
  val compTransPolicy     = OptionWrapper(params.hasCompAndSimp, Module(new EnqPolicy))

  //Wire
  //entries status
  val entries             = Wire(Vec(params.numEntries, ValidIO(new EntryBundle)))
  val robIdxVec           = Wire(Vec(params.numEntries, new RobPtr))
  val validVec            = Wire(Vec(params.numEntries, Bool()))
  val issuedVec           = Wire(Vec(params.numEntries, Bool()))
  val validForTrans       = VecInit(validVec.zip(issuedVec).map(x => x._1 && !x._2))
  val canIssueVec         = Wire(Vec(params.numEntries, Bool()))
  val srcReadyVec         = Wire(Vec(params.numEntries, Bool()))
  val rfWenVec            = Wire(Vec(params.numEntries, Bool()))
  val fuTypeVec           = Wire(Vec(params.numEntries, FuType()))
  val isFirstIssueVec     = Wire(Vec(params.numEntries, Bool()))
  val issueTimerVec       = Wire(Vec(params.numEntries, UInt(params.issueTimerWidth.W)))
  val sqIdxVec            = OptionWrapper(params.needFeedBackSqIdx, Wire(Vec(params.numEntries, new SqPtr())))
  val lqIdxVec            = OptionWrapper(params.needFeedBackLqIdx, Wire(Vec(params.numEntries, new LqPtr())))
  //src status
  val dataSourceVec       = Wire(Vec(params.numEntries, Vec(params.numRegSrc, DataSource())))
  val loadDependencyVec   = Wire(Vec(params.numEntries, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
  val exuSourceVec        = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, ExuSource()))))
  //deq sel
  val deqSelVec           = Wire(Vec(params.numEntries, Bool()))
  val issueRespVec        = Wire(Vec(params.numEntries, new IssueQueueRespBundle))
  val deqPortIdxWriteVec  = Wire(Vec(params.numEntries, UInt(1.W)))
  val deqPortIdxReadVec   = Wire(Vec(params.numEntries, UInt(1.W)))
  //trans sel
  val othersEntryEnqReadyVec = Wire(Vec(OthersEntryNum, Bool()))
  val othersEntryEnqVec      = Wire(Vec(OthersEntryNum, Valid(new EntryBundle)))
  val enqEntryTransVec       = Wire(Vec(EnqEntryNum, Valid(new EntryBundle)))
  val simpEntryTransVec      = OptionWrapper(params.hasCompAndSimp, Wire(Vec(SimpEntryNum, Valid(new EntryBundle))))
  val compEnqVec             = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, Valid(new EntryBundle))))

  val enqCanTrans2Simp       = OptionWrapper(params.hasCompAndSimp, Wire(Bool()))
  val enqCanTrans2Comp       = OptionWrapper(params.hasCompAndSimp, Wire(Bool()))
  val simpCanTrans2Comp      = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, Bool())))
  val simpTransSelVec        = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, Valid(UInt(SimpEntryNum.W)))))
  val compTransSelVec        = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, Valid(UInt(CompEntryNum.W)))))
  val finalSimpTransSelVec   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, UInt(SimpEntryNum.W))))
  val finalCompTransSelVec   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(EnqEntryNum, UInt(CompEntryNum.W))))

  val enqCanTrans2Others     = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Bool()))
  val othersTransSelVec      = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(EnqEntryNum, Valid(UInt(OthersEntryNum.W)))))
  val finalOthersTransSelVec = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(EnqEntryNum, UInt(OthersEntryNum.W))))

  val simpEntryEnqReadyVec   = othersEntryEnqReadyVec.take(SimpEntryNum)
  val compEntryEnqReadyVec   = othersEntryEnqReadyVec.takeRight(CompEntryNum)
  val simpEntryEnqVec        = othersEntryEnqVec.take(SimpEntryNum)
  val compEntryEnqVec        = othersEntryEnqVec.takeRight(CompEntryNum)
  //debug
  val entryInValidVec        = Wire(Vec(params.numEntries, Bool()))
  val entryOutDeqValidVec    = Wire(Vec(params.numEntries, Bool()))
  val entryOutTransValidVec  = Wire(Vec(params.numEntries, Bool()))
  val perfLdCancelVec        = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, Bool()))))
  val perfOg0CancelVec       = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, Bool()))))
  val perfWakeupByWBVec      = Wire(Vec(params.numEntries, Vec(params.numRegSrc, Bool())))
  val perfWakeupByIQVec      = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))))
  //cancel bypass
  val cancelBypassVec        = Wire(Vec(params.numEntries, Bool()))


  //enqEntries
  enqEntries.zipWithIndex.foreach { case (enqEntry, entryIdx) =>
    enqEntry.io.commonIn.enq                  := io.enq(entryIdx)
    enqEntry.io.commonIn.transSel             := (if (params.isAllComp || params.isAllSimp) enqCanTrans2Others.get && othersTransSelVec.get(entryIdx).valid
                                                  else enqCanTrans2Simp.get && simpTransSelVec.get(entryIdx).valid || enqCanTrans2Comp.get && compTransSelVec.get(entryIdx).valid)
    EntriesConnect(enqEntry.io.commonIn, enqEntry.io.commonOut, entryIdx)
    enqEntry.io.enqDelayIn1.wakeUpFromWB := io.wakeUpFromWBDelayed
    enqEntry.io.enqDelayIn1.wakeUpFromIQ := io.wakeUpFromIQDelayed
    enqEntry.io.enqDelayIn1.srcLoadDependency := RegEnable(VecInit(io.enq(entryIdx).bits.payload.srcLoadDependency.take(params.numRegSrc)), io.enq(entryIdx).valid)
    enqEntry.io.enqDelayIn1.og0Cancel         := RegNext(io.og0Cancel)
    enqEntry.io.enqDelayIn1.ldCancel          := RegNext(io.ldCancel)
    // note: these signals with 2 cycle delay should not be enabled by io.enq.valid
    enqEntry.io.enqDelayIn2.wakeUpFromWB      := DelayN(io.wakeUpFromWB, 2)
    enqEntry.io.enqDelayIn2.wakeUpFromIQ      := DelayN(io.wakeUpFromIQ, 2)
    enqEntry.io.enqDelayIn2.srcLoadDependency := DelayN(VecInit(io.enq(entryIdx).bits.payload.srcLoadDependency.take(params.numRegSrc)), 2)
    enqEntry.io.enqDelayIn2.og0Cancel         := DelayN(io.og0Cancel, 2)
    enqEntry.io.enqDelayIn2.ldCancel          := DelayN(io.ldCancel, 2)
    enqEntryTransVec(entryIdx)                := enqEntry.io.commonOut.transEntry
  }
  //othersEntries
  othersEntries.zipWithIndex.foreach { case (othersEntry, entryIdx) =>
    othersEntry.io.commonIn.enq               := othersEntryEnqVec(entryIdx)
    othersEntry.io.commonIn.transSel          := (if (params.hasCompAndSimp && (entryIdx < SimpEntryNum))
                                                    io.simpEntryDeqSelVec.get.zip(simpCanTrans2Comp.get).map(x => x._1(entryIdx) && x._2).reduce(_ | _)
                                                  else false.B)
    EntriesConnect(othersEntry.io.commonIn, othersEntry.io.commonOut, entryIdx + EnqEntryNum)
    othersEntryEnqReadyVec(entryIdx)          := othersEntry.io.commonOut.enqReady
    if (params.hasCompAndSimp && (entryIdx < SimpEntryNum)) {
      simpEntryTransVec.get(entryIdx)         := othersEntry.io.commonOut.transEntry
    }
  }


  deqSelVec.zip(deqPortIdxWriteVec).zipWithIndex.foreach { case ((deqSel, deqPortIdxWrite), i) =>
    val deqVec = io.deqSelOH.zip(io.deqReady).map(x => x._1.valid && x._1.bits(i) && x._2)
    deqPortIdxWrite := OHToUInt(deqVec)
    deqSel := deqVec.reduce(_ | _)
  }


  if (params.isAllComp || params.isAllSimp) {
    //transPolicy
    othersTransPolicy.get.io.canEnq := VecInit(validVec.takeRight(OthersEntryNum).map(!_)).asUInt

    // we only allow all or none of the enq entries transfering to others entries.
    enqCanTrans2Others.get := PopCount(validVec.take(EnqEntryNum)) <= PopCount(validVec.takeRight(OthersEntryNum).map(!_))
    // othersTransSelVec(i) is the target others entry for enq entry [i].
    // note that dispatch does not guarantee the validity of enq entries with low index.
    // that means in some cases enq entry [0] is invalid while enq entry [1] is valid.
    // in this case, enq entry [1] should use result [0] of TransPolicy.
    othersTransSelVec.get(0).valid := othersTransPolicy.get.io.enqSelOHVec(0).valid && validForTrans(0)
    othersTransSelVec.get(0).bits  := othersTransPolicy.get.io.enqSelOHVec(0).bits
    if (params.numEnq == 2) {
      othersTransSelVec.get(1).valid := Mux(!validForTrans(0), othersTransPolicy.get.io.enqSelOHVec(0).valid, othersTransPolicy.get.io.enqSelOHVec(1).valid) && validForTrans(1)
      othersTransSelVec.get(1).bits  := Mux(!validForTrans(0), othersTransPolicy.get.io.enqSelOHVec(0).bits,  othersTransPolicy.get.io.enqSelOHVec(1).bits)
    }

    finalOthersTransSelVec.get.zip(othersTransSelVec.get).zipWithIndex.foreach { case ((finalOH, selOH), enqIdx) =>
      finalOH := Fill(OthersEntryNum, enqCanTrans2Others.get && selOH.valid) & selOH.bits
    }

    //othersEntryEnq
    othersEntryEnqVec.zipWithIndex.foreach { case (othersEntryEnq, othersIdx) =>
      val othersEnqOH = finalOthersTransSelVec.get.map(_(othersIdx))
      if (othersEnqOH.size == 1)
        othersEntryEnq := Mux(othersEnqOH.head, enqEntryTransVec.head, 0.U.asTypeOf(enqEntryTransVec.head))
      else
        othersEntryEnq := Mux1H(othersEnqOH, enqEntryTransVec)
    }
  }
  else {
    //transPolicy
    simpTransPolicy.get.io.canEnq := VecInit(simpEntryEnqReadyVec).asUInt
    compTransPolicy.get.io.canEnq := VecInit(validVec.takeRight(CompEntryNum).map(!_)).asUInt

    // we only allow all or none of the enq entries transfering to comp/simp entries.
    // when all of simp entries are empty and comp entries are enough, transfer to comp entries.
    // otherwise, transfer to simp entries.
    enqCanTrans2Comp.get := PopCount(validVec.take(EnqEntryNum)) <= PopCount(validVec.takeRight(CompEntryNum).map(!_)) && !validVec.drop(EnqEntryNum).take(SimpEntryNum).reduce(_ || _)
    enqCanTrans2Simp.get := !enqCanTrans2Comp.get && PopCount(validVec.take(EnqEntryNum)) <= PopCount(simpEntryEnqReadyVec)
    simpCanTrans2Comp.get.zipWithIndex.foreach { case (canTrans, idx) =>
      canTrans := !enqCanTrans2Comp.get && PopCount(validVec.takeRight(CompEntryNum).map(!_)) >= (idx + 1).U
    }

    // simp/compTransSelVec(i) is the target simp/comp entry for enq entry [i].
    // note that dispatch does not guarantee the validity of enq entries with low index.
    // that means in some cases enq entry [0] is invalid while enq entry [1] is valid.
    // in this case, enq entry [1] should use result [0] of TransPolicy.
    simpTransSelVec.get(0).valid := simpTransPolicy.get.io.enqSelOHVec(0).valid && validForTrans(0)
    simpTransSelVec.get(0).bits  := simpTransPolicy.get.io.enqSelOHVec(0).bits
    compTransSelVec.get(0).valid := compTransPolicy.get.io.enqSelOHVec(0).valid && validForTrans(0)
    compTransSelVec.get(0).bits  := compTransPolicy.get.io.enqSelOHVec(0).bits
    if (params.numEnq == 2) {
      simpTransSelVec.get(1).valid := Mux(!validForTrans(0), simpTransPolicy.get.io.enqSelOHVec(0).valid, simpTransPolicy.get.io.enqSelOHVec(1).valid) && validForTrans(1)
      simpTransSelVec.get(1).bits  := Mux(!validForTrans(0), simpTransPolicy.get.io.enqSelOHVec(0).bits,  simpTransPolicy.get.io.enqSelOHVec(1).bits)
      compTransSelVec.get(1).valid := Mux(!validForTrans(0), compTransPolicy.get.io.enqSelOHVec(0).valid, compTransPolicy.get.io.enqSelOHVec(1).valid) && validForTrans(1)
      compTransSelVec.get(1).bits  := Mux(!validForTrans(0), compTransPolicy.get.io.enqSelOHVec(0).bits,  compTransPolicy.get.io.enqSelOHVec(1).bits)
    }

    finalSimpTransSelVec.get.zip(simpTransSelVec.get).zipWithIndex.foreach { case ((finalOH, selOH), enqIdx) =>
      finalOH := Fill(SimpEntryNum, enqCanTrans2Simp.get && selOH.valid) & selOH.bits
    }
    finalCompTransSelVec.get.zip(compTransSelVec.get).zip(compTransPolicy.get.io.enqSelOHVec).zipWithIndex.foreach {
      case (((finalOH, selOH), origSelOH), enqIdx) =>
        finalOH := Mux(enqCanTrans2Comp.get, Fill(CompEntryNum, selOH.valid) & selOH.bits, Fill(CompEntryNum, origSelOH.valid) & origSelOH.bits)
    }

    //othersEntryEnq
    simpEntryEnqVec.zipWithIndex.foreach { case (simpEntryEnq, simpIdx) =>
      val simpEnqOH = finalSimpTransSelVec.get.map(_(simpIdx))
      // shit Mux1H directly returns in(0) if the seq has only 1 elements
      if (simpEnqOH.size == 1)
        simpEntryEnq := Mux(simpEnqOH.head, enqEntryTransVec.head, 0.U.asTypeOf(enqEntryTransVec.head))
      else
        simpEntryEnq := Mux1H(simpEnqOH, enqEntryTransVec)
    }

    compEnqVec.get.zip(enqEntryTransVec).zip(io.simpEntryDeqSelVec.get).foreach { case ((compEnq, enqEntry), deqSel) =>
      compEnq := Mux(enqCanTrans2Comp.get, enqEntry, Mux1H(deqSel, simpEntryTransVec.get))
    }
    compEntryEnqVec.zipWithIndex.foreach { case (compEntryEnq, compIdx) =>
      val compEnqOH = finalCompTransSelVec.get.map(_(compIdx))
      // shit Mux1H directly returns in(0) if the seq has only 1 elements
      if (compEnqOH.size == 1)
        compEntryEnq := Mux(compEnqOH.head, compEnqVec.get.head, 0.U.asTypeOf(compEnqVec.get.head))
      else
        compEntryEnq := Mux1H(compEnqOH, compEnqVec.get)
    }

    assert(PopCount(simpEntryEnqVec.map(_.valid)) <= params.numEnq.U, "the number of simpEntryEnq is more than numEnq\n")
    assert(PopCount(compEntryEnqVec.map(_.valid)) <= params.numEnq.U, "the number of compEntryEnq is more than numEnq\n")
  }

  if(backendParams.debugEn) {
    dontTouch(othersEntryEnqVec)
  }

    issueRespVec.lazyZip(issueTimerVec.lazyZip(deqPortIdxReadVec)).foreach { case (issueResp, (issueTimer, deqPortIdx)) =>
      val Resp = resps(issueTimer)(deqPortIdx)
      issueResp := Resp
    }

  //deq
  val enqEntryOldest          = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val simpEntryOldest         = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val compEntryOldest         = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val othersEntryOldest       = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val enqEntryOldestDelay     = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val simpEntryOldestDelay    = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val compEntryOldestDelay    = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val othersEntryOldestDelay  = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val enqEntryOldestCancel    = Wire(Vec(params.numDeq, Bool()))
  val simpEntryOldestCancel   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, Bool())))
  val compEntryOldestCancel   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, Bool())))
  val othersEntryOldestCancel = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(params.numDeq, Bool())))

  io.enqEntryOldestSel.zipWithIndex.map { case (sel, deqIdx) =>
    enqEntryOldest(deqIdx) := Mux1H(sel.bits, entries.take(EnqEntryNum))
    enqEntryOldestCancel(deqIdx) := Mux1H(sel.bits, cancelBypassVec.take(EnqEntryNum))
  }
  io.enqEntryOldestSelDelay.zipWithIndex.map { case (sel, deqIdx) =>
    enqEntryOldestDelay(deqIdx) := Mux1H(sel.bits, entries.take(EnqEntryNum))
  }

  if (params.isAllComp || params.isAllSimp) {
    io.othersEntryOldestSel.get.zipWithIndex.map { case (sel, deqIdx) =>
      othersEntryOldest.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum))
      othersEntryOldestCancel.get(deqIdx) := Mux1H(sel.bits, cancelBypassVec.drop(EnqEntryNum))
    }
    io.othersEntryOldestSelDelay.get.zipWithIndex.map { case (sel, deqIdx) =>
      othersEntryOldestDelay.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum))
    }
  }
  else {
    io.simpEntryOldestSel.get.zipWithIndex.map { case (sel, deqIdx) =>
      simpEntryOldest.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum).take(SimpEntryNum))
      simpEntryOldestCancel.get(deqIdx) := Mux1H(sel.bits, cancelBypassVec.drop(EnqEntryNum).take(SimpEntryNum))
    }
    io.compEntryOldestSel.get.zipWithIndex.map { case (sel, deqIdx) =>
      compEntryOldest.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum).takeRight(CompEntryNum))
      compEntryOldestCancel.get(deqIdx) := Mux1H(sel.bits, cancelBypassVec.drop(EnqEntryNum).takeRight(CompEntryNum))
    }
    io.simpEntryOldestSelDelay.get.zipWithIndex.map { case (sel, deqIdx) =>
      simpEntryOldestDelay.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum).take(SimpEntryNum))
    }
    io.compEntryOldestSelDelay.get.zipWithIndex.map { case (sel, deqIdx) =>
      compEntryOldestDelay.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum).takeRight(CompEntryNum))
    }
  }

  if (params.deqFuSame) {
    val subDeqPolicyEntryVec = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
    val subDeqPolicyValidVec = Wire(Vec(params.numDeq, Bool()))
    val subDeqPolicyCancelBypassVec = Wire(Vec(params.numDeq, Bool()))

    subDeqPolicyValidVec(0) := PopCount(io.subDeqRequest.get(0)) >= 1.U
    subDeqPolicyValidVec(1) := PopCount(io.subDeqRequest.get(0)) >= 2.U

    if (params.isAllComp || params.isAllSimp) {
      subDeqPolicyEntryVec(0) := PriorityMux(io.subDeqRequest.get(0), entries)
      subDeqPolicyEntryVec(1) := PriorityMux(Reverse(io.subDeqRequest.get(0)), entries.reverse)
      subDeqPolicyCancelBypassVec(0) := PriorityMux(io.subDeqRequest.get(0), cancelBypassVec)
      subDeqPolicyCancelBypassVec(1) := PriorityMux(Reverse(io.subDeqRequest.get(0)), cancelBypassVec.reverse)

      io.deqEntry(0) := Mux(io.othersEntryOldestSel.get(0).valid, othersEntryOldest.get(0), subDeqPolicyEntryVec(1))
      io.deqEntry(1) := subDeqPolicyEntryVec(0)
      io.cancelDeqVec(0) := Mux(io.othersEntryOldestSel.get(0).valid, othersEntryOldestCancel.get(0), subDeqPolicyCancelBypassVec(1))
      io.cancelDeqVec(1) := subDeqPolicyCancelBypassVec(0)
    }
    else {
      subDeqPolicyEntryVec(0) := PriorityMux(Reverse(io.subDeqRequest.get(0)), entries.reverse)
      subDeqPolicyEntryVec(1) := PriorityMux(io.subDeqRequest.get(0), entries)
      subDeqPolicyCancelBypassVec(0) := PriorityMux(Reverse(io.subDeqRequest.get(0)), cancelBypassVec.reverse)
      subDeqPolicyCancelBypassVec(1) := PriorityMux(io.subDeqRequest.get(0), cancelBypassVec)

      io.deqEntry(0) := Mux(io.compEntryOldestSel.get(0).valid,
                            compEntryOldest.get(0),
                            Mux(io.simpEntryOldestSel.get(0).valid, simpEntryOldest.get(0), subDeqPolicyEntryVec(1)))
      io.deqEntry(1) := subDeqPolicyEntryVec(0)
      io.cancelDeqVec(0) := Mux(io.compEntryOldestSel.get(0).valid,
                                compEntryOldestCancel.get(0),
                                Mux(io.simpEntryOldestSel.get(0).valid, simpEntryOldestCancel.get(0), subDeqPolicyCancelBypassVec(1)))
      io.cancelDeqVec(1) := subDeqPolicyCancelBypassVec(0)
    }

    when (subDeqPolicyValidVec(0)) {
      assert(Mux1H(io.subDeqSelOH.get(0), entries).bits.status.robIdx === subDeqPolicyEntryVec(0).bits.status.robIdx, "subDeqSelOH(0) is not the same\n")
    }
    when (subDeqPolicyValidVec(1)) {
      assert(Mux1H(io.subDeqSelOH.get(1), entries).bits.status.robIdx === subDeqPolicyEntryVec(1).bits.status.robIdx, "subDeqSelOH(1) is not the same\n")
    }
  }
  else {
    if (params.isAllComp || params.isAllSimp) {
      io.othersEntryOldestSel.get.zipWithIndex.foreach { case (sel, i) =>
        io.deqEntry(i)     := Mux(sel.valid, othersEntryOldest.get(i), enqEntryOldest(i))
        io.cancelDeqVec(i) := Mux(sel.valid, othersEntryOldestCancel.get(i), enqEntryOldestCancel(i))
      }
      io.othersEntryOldestSelDelay.get.zipWithIndex.foreach { case (sel, i) =>
        io.deqOg1Payload(i) := Mux(sel.valid, othersEntryOldestDelay.get(i), enqEntryOldestDelay(i)).bits.payload.og1Payload
      }
    }
    else {
      io.compEntryOldestSel.get.zip(io.simpEntryOldestSel.get).zipWithIndex.foreach { case ((compSel, simpSel), i) =>
        val deqEntry = Mux(compSel.valid,
                           compEntryOldest.get(i),
                           Mux(simpSel.valid, simpEntryOldest.get(i), enqEntryOldest(i)))
        val cancelDeqVec = Mux(compSel.valid,
                               compEntryOldestCancel.get(i),
                               Mux(simpSel.valid, simpEntryOldestCancel.get(i), enqEntryOldestCancel(i)))
        io.deqEntry(i)     := deqEntry
        io.cancelDeqVec(i) := cancelDeqVec
        if (params.aluDeqNeedPickJump) {
          val aluDeqSelectJump = io.deqEntry(0).valid && io.deqEntry(0).bits.payload.rfWen.get && FuType.isJump(io.deqEntry(0).bits.payload.fuType)
          io.aluDeqSelectJump.get := aluDeqSelectJump
          if (params.deqFuCfgs(i).contains(AluCfg)) {
            assert(i == 0, "IQ needPickRfWen ALU must in deq 0")
            // alu uop fuType change to alu
            io.deqEntry(i).bits.status.fuType := Mux(aluDeqSelectJump, FuType.alu.U.asTypeOf(deqEntry.bits.status.fuType), deqEntry.bits.status.fuType)
          }
          else if (params.deqFuCfgs(i).contains(JmpCfg)) {
            val deqEntry0 = Mux(io.compEntryOldestSel.get(0).valid,
                                compEntryOldest.get(0),
                                Mux(io.simpEntryOldestSel.get(0).valid, simpEntryOldest.get(0), enqEntryOldest(0)))
            assert(i == 1, "IQ needPickRfWen BJU must in deq 1")
            // jump uop use alu uop before change
            io.deqEntry(i) := Mux(aluDeqSelectJump, deqEntry0, deqEntry)
            dontTouch(io.deqEntry(i))
            io.cancelDeqVec(i) := Mux(aluDeqSelectJump, io.cancelDeqVec(0), cancelDeqVec)
          }
        }
      }
      io.compEntryOldestSelDelay.get.zip(io.simpEntryOldestSelDelay.get).zipWithIndex.foreach { case ((compSel, simpSel), i) =>
        val deqOg1Payload = Mux(compSel.valid,
                           compEntryOldestDelay.get(i),
                           Mux(simpSel.valid, simpEntryOldestDelay.get(i), enqEntryOldestDelay(i))).bits.payload.og1Payload
        io.deqOg1Payload(i) := deqOg1Payload
        if (params.aluDeqNeedPickJump) {
          val aluDeqSelectJump = RegNext(io.deqEntry(0).valid && io.deqEntry(0).bits.payload.rfWen.get && FuType.isJump(io.deqEntry(0).bits.payload.fuType))
          if (params.deqFuCfgs(i).contains(JmpCfg)) {
            val deqOg1Payload0 = Mux(io.compEntryOldestSelDelay.get(0).valid,
                                compEntryOldestDelay.get(0),
                                Mux(io.simpEntryOldestSelDelay.get(0).valid, simpEntryOldestDelay.get(0), enqEntryOldestDelay(0))).bits.payload.og1Payload
            // jump uop use alu uop before change
            io.deqOg1Payload(i) := Mux(aluDeqSelectJump, deqOg1Payload0, deqOg1Payload)
          }
        }
      }
    }
  }

  io.valid                          := validVec.asUInt
  io.issued                         := issuedVec.asUInt
  io.canIssue                       := canIssueVec.asUInt
  io.srcReady                       := srcReadyVec.asUInt
  io.rfWen                          := rfWenVec.asUInt
  io.fuType                         := fuTypeVec
  io.dataSources                    := dataSourceVec
  io.exuSources.foreach(_           := exuSourceVec.get)
  io.loadDependency                 := loadDependencyVec
  io.isFirstIssue.zipWithIndex.foreach{ case (isFirstIssue, deqIdx) =>
    isFirstIssue                    := io.deqSelOH(deqIdx).valid && Mux1H(io.deqSelOH(deqIdx).bits, isFirstIssueVec)
  }
  io.simpEntryEnqSelVec.foreach(_   := finalSimpTransSelVec.get.zip(enqEntryTransVec).map(x => x._1 & Fill(SimpEntryNum, x._2.valid)))
  io.compEntryEnqSelVec.foreach(_   := finalCompTransSelVec.get.zip(compEnqVec.get).map(x => x._1 & Fill(CompEntryNum, x._2.valid)))
  io.othersEntryEnqSelVec.foreach(_ := finalOthersTransSelVec.get.zip(enqEntryTransVec).map(x => x._1 & Fill(OthersEntryNum, x._2.valid)))
  io.robIdx.foreach(_               := robIdxVec)


  def EntriesConnect(in: CommonInBundle, out: CommonOutBundle, entryIdx: Int) = {
    in.flush                    := io.flush
    in.wakeUpFromWB             := io.wakeUpFromWB
    in.wakeUpFromIQ             := io.wakeUpFromIQ
    in.vlFromIntIsZero          := io.vlFromIntIsZero
    in.vlFromIntIsVlmax         := io.vlFromIntIsVlmax
    in.vlFromVfIsZero           := io.vlFromVfIsZero
    in.vlFromVfIsVlmax          := io.vlFromVfIsVlmax
    in.og0Cancel                := io.og0Cancel
    in.og1Cancel                := io.og1Cancel
    in.ldCancel                 := io.ldCancel
    in.deqSel                   := deqSelVec(entryIdx)
    in.deqPortIdxWrite          := deqPortIdxWriteVec(entryIdx)
    in.issueResp                := issueRespVec(entryIdx)
    in.vecMemIn.foreach(_       := io.vecMemIn.get)
    validVec(entryIdx)          := out.valid
    issuedVec(entryIdx)         := out.issued
    canIssueVec(entryIdx)       := out.canIssue
    rfWenVec(entryIdx)          := out.entry.bits.payload.rfWen.getOrElse(false.B)
    srcReadyVec(entryIdx)       := out.srcReady
    fuTypeVec(entryIdx)         := out.fuType
    robIdxVec(entryIdx)         := out.robIdx
    dataSourceVec(entryIdx)     := out.dataSources
    isFirstIssueVec(entryIdx)   := out.isFirstIssue
    entries(entryIdx)           := out.entry
    deqPortIdxReadVec(entryIdx) := out.deqPortIdxRead
    issueTimerVec(entryIdx)     := out.issueTimerRead
    loadDependencyVec(entryIdx) := out.entry.bits.status.mergedLoadDependency
    cancelBypassVec(entryIdx)   := out.cancelBypass
    exuSourceVec.foreach(_(entryIdx) := out.exuSources.get)
    lqIdxVec.foreach(_(entryIdx) := out.entry.bits.payload.og1Payload.lqIdx.get)
    sqIdxVec.foreach(_(entryIdx) := out.entry.bits.payload.og1Payload.sqIdx.get)
    entryInValidVec(entryIdx)       := out.entryInValid
    entryOutDeqValidVec(entryIdx)   := out.entryOutDeqValid
    entryOutTransValidVec(entryIdx) := out.entryOutTransValid
    perfWakeupByWBVec(entryIdx)     := out.perfWakeupByWB
    if (params.hasIQWakeUp) {
      perfLdCancelVec.get(entryIdx)   := out.perfLdCancel.get
      perfOg0CancelVec.get(entryIdx)  := out.perfOg0Cancel.get
      perfWakeupByIQVec.get(entryIdx) := out.perfWakeupByIQ.get
    }
  }

  // entries perf counter
  // enq
  for (i <- 0 until params.numEnq) {
    XSPerfAccumulate(s"enqEntry_${i}_in_cnt", entryInValidVec(i))
    XSPerfAccumulate(s"enqEntry_${i}_out_deq_cnt", entryOutDeqValidVec(i))
    XSPerfAccumulate(s"enqEntry_${i}_out_trans_cnt", entryOutTransValidVec(i))
  }
  // simple
  for (i <- 0 until params.numSimp) {
    XSPerfAccumulate(s"simpEntry_${i}_in_cnt", entryInValidVec(i + params.numEnq))
    XSPerfAccumulate(s"simpEntry_${i}_out_deq_cnt", entryOutDeqValidVec(i + params.numEnq))
    XSPerfAccumulate(s"simpEntry_${i}_out_trans_cnt", entryOutTransValidVec(i + params.numEnq))
  }
  // complex
  for (i <- 0 until params.numComp) {
    XSPerfAccumulate(s"compEntry_${i}_in_cnt", entryInValidVec(i + params.numEnq + params.numSimp))
    XSPerfAccumulate(s"compEntry_${i}_out_deq_cnt", entryOutDeqValidVec(i + params.numEnq + params.numSimp))
    XSPerfAccumulate(s"compEntry_${i}_out_trans_cnt", entryOutTransValidVec(i + params.numEnq + params.numSimp))
  }
  // total
  XSPerfAccumulate(s"enqEntry_all_in_cnt", PopCount(entryInValidVec.take(params.numEnq)))
  XSPerfAccumulate(s"enqEntry_all_out_deq_cnt", PopCount(entryOutDeqValidVec.take(params.numEnq)))
  XSPerfAccumulate(s"enqEntry_all_out_trans_cnt", PopCount(entryOutTransValidVec.take(params.numEnq)))
  for (srcIdx <- 0 until params.numRegSrc) {
    XSPerfAccumulate(s"enqEntry_all_wakeup_wb_src${srcIdx}_cnt", PopCount(perfWakeupByWBVec.take(params.numEnq).map(_(srcIdx))))
    if (params.hasIQWakeUp) {
      XSPerfAccumulate(s"enqEntry_all_ldCancel_src${srcIdx}_cnt", PopCount(perfLdCancelVec.get.take(params.numEnq).map(_(srcIdx))))
      XSPerfAccumulate(s"enqEntry_all_og0Cancel_src${srcIdx}_cnt", PopCount(perfOg0CancelVec.get.take(params.numEnq).map(_(srcIdx))))
      for (iqIdx <- 0 until params.numWakeupFromIQ) {
        XSPerfAccumulate(s"enqEntry_all_wakeup_iq_from_exu${params.wakeUpSourceExuIdx(iqIdx)}_src${srcIdx}_cnt", PopCount(perfWakeupByIQVec.get.take(params.numEnq).map(_(srcIdx)(iqIdx))))
      }
    }
  }

  XSPerfAccumulate(s"othersEntry_all_in_cnt", PopCount(entryInValidVec.drop(params.numEnq)))
  XSPerfAccumulate(s"othersEntry_all_out_deq_cnt", PopCount(entryOutDeqValidVec.drop(params.numEnq)))
  XSPerfAccumulate(s"othersEntry_all_out_trans_cnt", PopCount(entryOutTransValidVec.drop(params.numEnq)))

  for (srcIdx <- 0 until params.numRegSrc) {
    XSPerfAccumulate(s"othersEntry_all_wakeup_wb_src${srcIdx}_cnt", PopCount(perfWakeupByWBVec.drop(params.numEnq).map(_(srcIdx))))
    if (params.hasIQWakeUp) {
      XSPerfAccumulate(s"othersEntry_all_ldCancel_src${srcIdx}_cnt", PopCount(perfLdCancelVec.get.drop(params.numEnq).map(_(srcIdx))))
      XSPerfAccumulate(s"othersEntry_all_og0Cancel_src${srcIdx}_cnt", PopCount(perfOg0CancelVec.get.drop(params.numEnq).map(_(srcIdx))))
      for (iqIdx <- 0 until params.numWakeupFromIQ) {
        XSPerfAccumulate(s"othersEntry_all_wakeup_iq_from_exu${params.wakeUpSourceExuIdx(iqIdx)}_src${srcIdx}_cnt", PopCount(perfWakeupByIQVec.get.drop(params.numEnq).map(_(srcIdx)(iqIdx))))
      }
    }
  }

  for (t <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(t)
    if (params.getFuCfgs.map(_.fuType == t).reduce(_ | _) && params.getFuCfgs.size > 1) {
      for (srcIdx <- 0 until params.numRegSrc) {
        XSPerfAccumulate(s"allEntry_futype_${fuName}_wakeup_wb_src${srcIdx}_cnt", PopCount(perfWakeupByWBVec.zip(fuTypeVec).map{ case(x, fu) => x(srcIdx) && fu(t.id) }))
        if (params.hasIQWakeUp) {
          XSPerfAccumulate(s"allEntry_futype_${fuName}_ldCancel_src${srcIdx}_cnt", PopCount(perfLdCancelVec.get.zip(fuTypeVec).map{ case(x, fu) => x(srcIdx) && fu(t.id) }))
          XSPerfAccumulate(s"allEntry_futype_${fuName}_og0Cancel_src${srcIdx}_cnt", PopCount(perfOg0CancelVec.get.zip(fuTypeVec).map{ case(x, fu) => x(srcIdx) && fu(t.id) }))
          for (iqIdx <- 0 until params.numWakeupFromIQ) {
            XSPerfAccumulate(s"allEntry_futype_${fuName}_wakeup_iq_from_exu${params.wakeUpSourceExuIdx(iqIdx)}_src${srcIdx}_cnt", PopCount(perfWakeupByIQVec.get.zip(fuTypeVec).map{ case(x, fu) => x(srcIdx)(iqIdx) && fu(t.id) }))
          }
        }
      }
    }
  }
}

class EntriesIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val flush               = Flipped(ValidIO(new Redirect))
  //enq
  val enq                 = Vec(params.numEnq, Flipped(ValidIO(new EntryBundle)))
  val og0Resp             = Vec(params.numDeq, Flipped(new IssueQueueRespBundle))
  val og1Resp             = Vec(params.numDeq, Flipped(new IssueQueueRespBundle))
  val og2Resp             = OptionWrapper(params.needOg2Resp, Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val s0Resp              = OptionWrapper(params.needS0Resp, Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val s2Resp              = OptionWrapper(params.needS2Resp, Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  val snResp              = OptionWrapper(params.needSnResp, Vec(params.numDeq, Flipped(new IssueQueueRespBundle)))
  //deq sel
  val deqReady            = Vec(params.numDeq, Input(Bool()))
  val deqSelOH            = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
  val enqEntryOldestSel   = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEnq.W))))
  val simpEntryOldestSel  = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numSimp.W)))))
  val compEntryOldestSel  = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numComp.W)))))
  val othersEntryOldestSel= OptionWrapper(params.isAllComp || params.isAllSimp, Vec(params.numDeq, Flipped(ValidIO(UInt((params.numEntries - params.numEnq).W)))))
  val enqEntryOldestSelDelay    = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEnq.W))))
  val simpEntryOldestSelDelay   = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numSimp.W)))))
  val compEntryOldestSelDelay   = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numComp.W)))))
  val othersEntryOldestSelDelay = OptionWrapper(params.isAllComp || params.isAllSimp, Vec(params.numDeq, Flipped(ValidIO(UInt((params.numEntries - params.numEnq).W)))))
  val subDeqRequest       = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  val subDeqSelOH         = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  // wakeup
  val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = if (params.isVecExeIQ) (Flipped(params.genVfWBWakeUpSinkValidBundle)) else (Flipped(params.genWBWakeUpSinkValidBundle))
  val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val wakeUpFromWBDelayed: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = if (params.isVecExeIQ) (Flipped(params.genVfWBWakeUpSinkValidBundle)) else (Flipped(params.genWBWakeUpSinkValidBundle))
  val wakeUpFromIQDelayed: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val vlFromIntIsZero     = Input(Bool())
  val vlFromIntIsVlmax    = Input(Bool())
  val vlFromVfIsZero      = Input(Bool())
  val vlFromVfIsVlmax     = Input(Bool())
  val og0Cancel           = Input(ExuVec())
  val og1Cancel           = Input(ExuVec())
  val ldCancel            = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
  //entries status
  val valid               = Output(UInt(params.numEntries.W))
  val issued              = Output(UInt(params.numEntries.W))
  val canIssue            = Output(UInt(params.numEntries.W))
  val rfWen               = Output(UInt(params.numEntries.W))
  val srcReady            = Output(UInt(params.numEntries.W))
  val fuType              = Vec(params.numEntries, Output(FuType()))
  val dataSources         = Vec(params.numEntries, Vec(params.numRegSrc, Output(DataSource())))
  val loadDependency      = Vec(params.numEntries, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))
  val exuSources          = OptionWrapper(params.hasIQWakeUp, Vec(params.numEntries, Vec(params.numRegSrc, Output(ExuSource()))))
  //deq status
  val isFirstIssue        = Vec(params.numDeq, Output(Bool()))
  val deqEntry            = Vec(params.numDeq, ValidIO(new EntryBundle))
  val deqOg1Payload       = Vec(params.numDeq, new Og1Payload(params))
  val cancelDeqVec        = Vec(params.numDeq, Output(Bool()))
  val aluDeqSelectJump    = Option.when(params.aluDeqNeedPickJump)(Output(Bool()))

  // vec mem only
  val vecMemIn = OptionWrapper(params.isVecMemIQ, new Bundle {
    val sqDeqPtr          = Input(new SqPtr)
    val lqDeqPtr          = Input(new LqPtr)
  })
  val robIdx = OptionWrapper(params.isVecMemIQ, Output(Vec(params.numEntries, new RobPtr)))

  // trans
  val simpEntryDeqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Input(UInt(params.numSimp.W))))
  val simpEntryEnqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Output(UInt(params.numSimp.W))))
  val compEntryEnqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Output(UInt(params.numComp.W))))
  val othersEntryEnqSelVec = OptionWrapper(params.isAllComp || params.isAllSimp, Vec(params.numEnq, Output(UInt((params.numEntries - params.numEnq).W))))

  def wakeup = wakeUpFromWB ++ wakeUpFromIQ
}
