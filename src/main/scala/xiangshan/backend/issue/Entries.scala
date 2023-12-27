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
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.backend.issue.EntryBundles._

class Entries(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  private val EnqEntryNum         = params.numEnq
  private val OthersEntryNum      = params.numEntries - params.numEnq
  private val SimpEntryNum        = params.numSimp
  private val CompEntryNum        = params.numComp
  val io = IO(new EntriesIO)

  val resps: Vec[Vec[ValidIO[EntryDeqRespBundle]]] = if(params.isLdAddrIQ) VecInit(io.og0Resp, io.og1Resp, io.finalIssueResp.get, io.fromMem.get.fastResp, io.fromMem.get.slowResp)
                                                     else if(params.isMemAddrIQ) VecInit(io.og0Resp, io.og1Resp, io.fromMem.get.fastResp, io.fromMem.get.slowResp)
                                                     else VecInit(io.og0Resp, io.og1Resp, 0.U.asTypeOf(io.og0Resp), 0.U.asTypeOf(io.og0Resp))

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
  val canIssueVec         = Wire(Vec(params.numEntries, Bool()))
  val fuTypeVec           = Wire(Vec(params.numEntries, FuType()))
  val isFirstIssueVec     = Wire(Vec(params.numEntries, Bool()))
  val issueTimerVec       = Wire(Vec(params.numEntries, UInt(2.W)))
  //src status
  val dataSourceVec       = Wire(Vec(params.numEntries, Vec(params.numRegSrc, DataSource())))
  val loadDependencyVec   = Wire(Vec(params.numEntries, Vec(LoadPipelineWidth, UInt(3.W))))
  val srcLoadDependencyVec= Wire(Vec(params.numEntries, Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(3.W)))))
  val srcTimerVec         = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, UInt(3.W)))))
  val srcWakeUpL1ExuOHVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Vec(params.numRegSrc, ExuVec()))))
  //deq sel
  val deqSelVec           = Wire(Vec(params.numEntries, Bool()))
  val issueRespVec        = Wire(Vec(params.numEntries, ValidIO(new EntryDeqRespBundle)))
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
  val cancelVec              = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numEntries, Bool())))
  //cancel bypass
  val cancelBypassVec        = Wire(Vec(params.numEntries, Bool()))


  //enqEntries
  enqEntries.zipWithIndex.foreach { case (enqEntry, entryIdx) =>
    enqEntry.io.commonIn.enq                  := io.enq(entryIdx)
    enqEntry.io.commonIn.transSel             := (if (params.isAllComp || params.isAllSimp) enqCanTrans2Others.get && othersTransSelVec.get(entryIdx).valid
                                                  else enqCanTrans2Simp.get && simpTransSelVec.get(entryIdx).valid || enqCanTrans2Comp.get && compTransSelVec.get(entryIdx).valid)
    EntriesConnect(enqEntry.io.commonIn, enqEntry.io.commonOut, entryIdx)
    enqEntry.io.enqDelayWakeUpFromWB          := RegNext(io.wakeUpFromWB)
    enqEntry.io.enqDelayWakeUpFromIQ          := RegNext(io.wakeUpFromIQ)
    enqEntry.io.enqDelayOg0Cancel             := RegNext(io.og0Cancel)
    enqEntry.io.enqDelayLdCancel              := RegNext(io.ldCancel)
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
    othersTransPolicy.get.io.canEnq := othersEntryEnqReadyVec.asUInt
    enqCanTrans2Others.get := PopCount(validVec.take(EnqEntryNum)) <= PopCount(othersEntryEnqReadyVec)
    othersTransSelVec.get(0).valid := othersTransPolicy.get.io.enqSelOHVec(0).valid && validVec(0)
    othersTransSelVec.get(0).bits  := othersTransPolicy.get.io.enqSelOHVec(0).bits
    if (params.numEnq == 2) {
      othersTransSelVec.get(1).valid := Mux(!validVec(0), othersTransPolicy.get.io.enqSelOHVec(0).valid, othersTransPolicy.get.io.enqSelOHVec(1).valid)
      othersTransSelVec.get(1).bits  := Mux(!validVec(0), othersTransPolicy.get.io.enqSelOHVec(0).bits,  othersTransPolicy.get.io.enqSelOHVec(1).bits)
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

    enqCanTrans2Comp.get := PopCount(validVec.take(EnqEntryNum)) <= PopCount(validVec.takeRight(CompEntryNum).map(!_)) && !validVec.drop(EnqEntryNum).take(SimpEntryNum).reduce(_ || _)
    enqCanTrans2Simp.get := !enqCanTrans2Comp.get && PopCount(validVec.take(EnqEntryNum)) <= PopCount(simpEntryEnqReadyVec)
    simpCanTrans2Comp.get.zipWithIndex.foreach { case (canTrans, idx) =>
      canTrans := !enqCanTrans2Comp.get && PopCount(validVec.takeRight(CompEntryNum).map(!_)) >= (idx + 1).U
    }

    simpTransSelVec.get(0).valid := simpTransPolicy.get.io.enqSelOHVec(0).valid && validVec(0)
    simpTransSelVec.get(0).bits  := simpTransPolicy.get.io.enqSelOHVec(0).bits
    compTransSelVec.get(0).valid := compTransPolicy.get.io.enqSelOHVec(0).valid && validVec(0)
    compTransSelVec.get(0).bits  := compTransPolicy.get.io.enqSelOHVec(0).bits
    if (params.numEnq == 2) {
      simpTransSelVec.get(1).valid := Mux(!validVec(0), simpTransPolicy.get.io.enqSelOHVec(0).valid, simpTransPolicy.get.io.enqSelOHVec(1).valid)
      simpTransSelVec.get(1).bits  := Mux(!validVec(0), simpTransPolicy.get.io.enqSelOHVec(0).bits,  simpTransPolicy.get.io.enqSelOHVec(1).bits)
      compTransSelVec.get(1).valid := Mux(!validVec(0), compTransPolicy.get.io.enqSelOHVec(0).valid, compTransPolicy.get.io.enqSelOHVec(1).valid)
      compTransSelVec.get(1).bits  := Mux(!validVec(0), compTransPolicy.get.io.enqSelOHVec(0).bits,  compTransPolicy.get.io.enqSelOHVec(1).bits)
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

  //issueRespVec
  if(params.isMemAddrIQ){
    issueRespVec.zip(robIdxVec).foreach { case (issueResp, robIdx) =>
      val hitRespsVec = VecInit(resps.flatten.map(x => x.valid && (x.bits.robIdx === robIdx)))
      issueResp.valid := hitRespsVec.reduce(_ | _)
      issueResp.bits := Mux1H(hitRespsVec, resps.flatten.map(_.bits))
    }
  }
  else {
    issueRespVec.zip(issueTimerVec).zip(deqPortIdxReadVec).foreach { case ((issueResp, issueTimer), deqPortIdx) =>
      val Resp = resps(issueTimer)(deqPortIdx)
      issueResp := Resp
    }
  }

  //deq
  val enqEntryOldest          = Wire(Vec(params.numDeq, ValidIO(new EntryBundle)))
  val simpEntryOldest         = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val compEntryOldest         = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val othersEntryOldest       = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(params.numDeq, ValidIO(new EntryBundle))))
  val enqEntryOldestCancel    = Wire(Vec(params.numDeq, Bool()))
  val simpEntryOldestCancel   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, Bool())))
  val compEntryOldestCancel   = OptionWrapper(params.hasCompAndSimp, Wire(Vec(params.numDeq, Bool())))
  val othersEntryOldestCancel = OptionWrapper(params.isAllComp || params.isAllSimp, Wire(Vec(params.numDeq, Bool())))

  io.enqEntryOldestSel.zipWithIndex.map { case (sel, deqIdx) =>
    enqEntryOldest(deqIdx) := Mux1H(sel.bits, entries.take(EnqEntryNum))
    enqEntryOldestCancel(deqIdx) := Mux1H(sel.bits, cancelBypassVec.take(EnqEntryNum))
  }

  if (params.isAllComp || params.isAllSimp) {
    io.othersEntryOldestSel.get.zipWithIndex.map { case (sel, deqIdx) =>
      othersEntryOldest.get(deqIdx) := Mux1H(sel.bits, entries.drop(EnqEntryNum))
      othersEntryOldestCancel.get(deqIdx) := Mux1H(sel.bits, cancelBypassVec.drop(EnqEntryNum))
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
    }
    else {
      io.compEntryOldestSel.get.zip(io.simpEntryOldestSel.get).zipWithIndex.foreach { case ((compSel, simpSel), i) =>
        io.deqEntry(i)     := Mux(compSel.valid, 
                                  compEntryOldest.get(i), 
                                  Mux(simpSel.valid, simpEntryOldest.get(i), enqEntryOldest(i)))
        io.cancelDeqVec(i) := Mux(compSel.valid, 
                                  compEntryOldestCancel.get(i), 
                                  Mux(simpSel.valid, simpEntryOldestCancel.get(i), enqEntryOldestCancel(i)))
      }
    }
  }

  if (params.hasIQWakeUp) {
    cancelBypassVec.zip(srcWakeUpL1ExuOHVec.get).zip(srcTimerVec.get).zip(srcLoadDependencyVec).foreach{ case (((cancelBypass: Bool, l1ExuOH: Vec[Vec[Bool]]), srcTimer: Vec[UInt]), srcLoadDependency: Vec[Vec[UInt]]) =>
      val cancelByOg0 = l1ExuOH.zip(srcTimer).map {
        case(exuOH, srcTimer) =>
          (exuOH.asUInt & io.og0Cancel.asUInt).orR && srcTimer === 1.U
      }.reduce(_ | _)
      val cancelByLd = srcLoadDependency.map(x => LoadShouldCancel(Some(x), io.ldCancel)).reduce(_ | _)
      cancelBypass := cancelByOg0 || cancelByLd
    }
  } else {
    cancelBypassVec.zip(srcLoadDependencyVec).foreach { case (cancelBypass, srcLoadDependency) =>
      val cancelByLd = srcLoadDependency.map(x => LoadShouldCancel(Some(x), io.ldCancel)).reduce(_ | _)
      cancelBypass := cancelByLd
    }
  }

  io.valid                          := validVec.asUInt
  io.canIssue                       := canIssueVec.asUInt
  io.fuType                         := fuTypeVec
  io.dataSources                    := dataSourceVec
  io.srcWakeUpL1ExuOH.foreach(_     := srcWakeUpL1ExuOHVec.get)
  io.srcTimer.foreach(_             := srcTimerVec.get)
  io.loadDependency                 := loadDependencyVec
  io.isFirstIssue.zipWithIndex.foreach{ case (isFirstIssue, deqIdx) =>
    isFirstIssue                    := io.deqSelOH(deqIdx).valid && Mux1H(io.deqSelOH(deqIdx).bits, isFirstIssueVec)
  }
  io.simpEntryEnqSelVec.foreach(_   := finalSimpTransSelVec.get.zip(enqEntryTransVec).map(x => x._1 & Fill(SimpEntryNum, x._2.valid)))
  io.compEntryEnqSelVec.foreach(_   := finalCompTransSelVec.get.zip(compEnqVec.get).map(x => x._1 & Fill(CompEntryNum, x._2.valid)))
  io.othersEntryEnqSelVec.foreach(_ := finalOthersTransSelVec.get.zip(enqEntryTransVec).map(x => x._1 & Fill(OthersEntryNum, x._2.valid)))
  io.rsFeedback                     := 0.U.asTypeOf(io.rsFeedback)  //should be removed
  io.cancel.foreach(_               := cancelVec.get)               //for debug

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
    validVec(entryIdx)          := out.valid
    canIssueVec(entryIdx)       := out.canIssue
    fuTypeVec(entryIdx)         := out.fuType
    robIdxVec(entryIdx)         := out.robIdx
    dataSourceVec(entryIdx)     := out.dataSource
    isFirstIssueVec(entryIdx)   := out.isFirstIssue
    entries(entryIdx)           := out.entry
    deqPortIdxReadVec(entryIdx) := out.deqPortIdxRead
    issueTimerVec(entryIdx)     := out.issueTimerRead
    srcLoadDependencyVec(entryIdx)          := out.srcLoadDependency
    loadDependencyVec(entryIdx)             := out.entry.bits.status.mergedLoadDependency
    if (params.hasIQWakeUp) {
      srcWakeUpL1ExuOHVec.get(entryIdx)       := out.srcWakeUpL1ExuOH.get
      srcTimerVec.get(entryIdx)               := out.srcTimer.get
      cancelVec.get(entryIdx)                 := out.cancel.get
    }
  }
}

class EntriesIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val flush               = Flipped(ValidIO(new Redirect))
  //enq
  val enq                 = Vec(params.numEnq, Flipped(ValidIO(new EntryBundle)))
  val og0Resp             = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
  val og1Resp             = Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle)))
  val finalIssueResp      = OptionWrapper(params.LduCnt > 0, Vec(params.numDeq, Flipped(ValidIO(new EntryDeqRespBundle))))
  //deq sel
  val deqReady            = Vec(params.numDeq, Input(Bool()))
  val deqSelOH            = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
  val enqEntryOldestSel   = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEnq.W))))
  val simpEntryOldestSel  = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numSimp.W)))))
  val compEntryOldestSel  = OptionWrapper(params.hasCompAndSimp, Vec(params.numDeq, Flipped(ValidIO(UInt(params.numComp.W)))))
  val othersEntryOldestSel= OptionWrapper(params.isAllComp || params.isAllSimp, Vec(params.numDeq, Flipped(ValidIO(UInt((params.numEntries - params.numEnq).W)))))
  val subDeqRequest       = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  val subDeqSelOH         = OptionWrapper(params.deqFuSame, Vec(params.numDeq, Input(UInt(params.numEntries.W))))
  // wakeup
  val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel           = Input(ExuVec(backendParams.numExu))
  val og1Cancel           = Input(ExuVec(backendParams.numExu))
  val ldCancel            = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  //entries status
  val valid               = Output(UInt(params.numEntries.W))
  val canIssue            = Output(UInt(params.numEntries.W))
  val fuType              = Vec(params.numEntries, Output(FuType()))
  val dataSources         = Vec(params.numEntries, Vec(params.numRegSrc, Output(DataSource())))
  val loadDependency      = Vec(params.numEntries, Vec(LoadPipelineWidth, UInt(3.W)))
  val srcWakeUpL1ExuOH    = OptionWrapper(params.hasIQWakeUp, Vec(params.numEntries, Vec(params.numRegSrc, Output(ExuVec()))))
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

  val rsFeedback          = Output(Vec(5, Bool()))
  // trans
  val simpEntryDeqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Input(UInt(params.numSimp.W))))
  val simpEntryEnqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Output(UInt(params.numSimp.W))))
  val compEntryEnqSelVec = OptionWrapper(params.hasCompAndSimp, Vec(params.numEnq, Output(UInt(params.numComp.W))))
  val othersEntryEnqSelVec = OptionWrapper(params.isAllComp || params.isAllSimp, Vec(params.numEnq, Output(UInt((params.numEntries - params.numEnq).W))))

  // debug
  val cancel              = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numEntries, Bool())))

  def wakeup = wakeUpFromWB ++ wakeUpFromIQ
}
