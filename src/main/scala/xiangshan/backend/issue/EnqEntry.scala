package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils.{MathUtils, OptionWrapper}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}


class EnqEntryIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  //input
  val enq = Flipped(ValidIO(new EntryBundle))
  val flush = Flipped(ValidIO(new Redirect))
  val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel = Input(ExuVec(backendParams.numExu))
  val og1Cancel = Input(ExuVec(backendParams.numExu))
  val ldCancel = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  val deqSel = Input(Bool())
  val deqPortIdxWrite = Input(UInt(1.W))
  val transSel = Input(Bool())
  val issueResp = Flipped(ValidIO(new EntryDeqRespBundle))
  //output
  val valid = Output(Bool())
  val canIssue = Output(Bool())
  val clear = Output(Bool())
  val fuType = Output(FuType())
  val dataSource = Output(Vec(params.numRegSrc, DataSource()))
  val srcWakeUpL1ExuOH = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, ExuVec())))
  val srcTimer = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, UInt(3.W))))
  val transEntry =  ValidIO(new EntryBundle)
  val isFirstIssue = Output(Bool())
  val entry = ValidIO(new EntryBundle)
  val robIdx = Output(new RobPtr)
  val deqPortIdxRead = Output(UInt(1.W))
  val issueTimerRead = Output(UInt(2.W))
  // mem only
  val fromMem = if(params.isMemAddrIQ) Some(new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }) else None
  // debug
  val cancel = OptionWrapper(params.hasIQWakeUp, Output(Bool()))

  def wakeup = wakeUpFromWB ++ wakeUpFromIQ
}

class EnqEntry(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new EnqEntryIO)

  val validReg = RegInit(false.B)
  val entryReg = Reg(new EntryBundle)

  val validRegNext = Wire(Bool())
  val entryRegNext = Wire(new EntryBundle)
  val entryUpdate = Wire(new EntryBundle)
  val enqReady = Wire(Bool())
  val clear = Wire(Bool())
  val flushed = Wire(Bool())
  val deqSuccess = Wire(Bool())
  val srcWakeUp = Wire(Vec(params.numRegSrc, Bool()))
  val srcCancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))
  val srcLoadCancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))
  val srcWakeUpByIQVec = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val wakeupLoadDependencyByIQVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  val shiftedWakeupLoadDependencyByIQVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  val cancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))

  //Reg
  validReg := validRegNext
  entryReg := entryRegNext

  //Wire
  when(io.enq.valid && enqReady) {
    validRegNext := true.B
  }.elsewhen(clear) {
    validRegNext := false.B
  }.otherwise {
    validRegNext := validReg
  }

  when(io.enq.valid && enqReady) {
    entryRegNext := io.enq.bits
  }.otherwise {
    entryRegNext := entryUpdate
  }

  enqReady := !validReg || clear
  clear := flushed || io.transSel || deqSuccess
  flushed := entryReg.status.robIdx.needFlush(io.flush)
  deqSuccess := io.issueResp.valid && io.issueResp.bits.respType === RSFeedbackType.fuIdle && !srcLoadCancelVec.map(_.reduce(_ || _)).getOrElse(false.B)
  srcWakeUp := io.wakeup.map(bundle => bundle.bits.wakeUp(entryReg.status.psrc zip entryReg.status.srcType, bundle.valid)).transpose.map(x => VecInit(x.toSeq).asUInt.orR).toSeq

  shiftedWakeupLoadDependencyByIQVec
    .zip(wakeupLoadDependencyByIQVec)
    .zip(params.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
          dep := (originalDep << 1).asUInt | 1.U
        else
          dep := originalDep << 1
    }
  }

  if (params.hasIQWakeUp) {
    srcCancelVec.get.zip(srcLoadCancelVec.get).zip(srcWakeUpByIQVec).zipWithIndex.foreach { case (((srcCancel, srcLoadCancel), wakeUpByIQVec), srcIdx) =>
      // level1 cancel: A(s)->C, A(s) are the level1 cancel
      val l1Cancel = (io.og0Cancel.asUInt & entryReg.status.srcWakeUpL1ExuOH.get(srcIdx).asUInt).orR &&
        entryReg.status.srcTimer.get(srcIdx) === 1.U
      val ldTransCancel = Mux(
        wakeUpByIQVec.asUInt.orR,
        Mux1H(wakeUpByIQVec, wakeupLoadDependencyByIQVec.map(dep => LoadShouldCancel(Some(dep), io.ldCancel))),
        false.B
      )
      srcLoadCancel := LoadShouldCancel(entryReg.status.srcLoadDependency.map(_(srcIdx)), io.ldCancel)
      srcCancel := l1Cancel || srcLoadCancel || ldTransCancel
    }
  }

  if (io.wakeUpFromIQ.isEmpty) {
    srcWakeUpByIQVec := 0.U.asTypeOf(srcWakeUpByIQVec)
    wakeupLoadDependencyByIQVec := 0.U.asTypeOf(wakeupLoadDependencyByIQVec)
  } else {
    val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.wakeUpFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
      bundle.bits.wakeUp(entryReg.status.psrc zip entryReg.status.srcType, bundle.valid)
    ).toIndexedSeq.transpose
    srcWakeUpByIQVec := wakeupVec.map(x => VecInit(x))
    wakeupLoadDependencyByIQVec := io.wakeUpFromIQ.map(_.bits.loadDependency).toSeq
  }

  //entryUpdate
  entryUpdate.status.srcState.zip(entryReg.status.srcState).zip(srcWakeUp).zipWithIndex.foreach { case (((stateNext, state), wakeup), srcIdx) =>
    val cancel = srcCancelVec.map(_ (srcIdx)).getOrElse(false.B)
    stateNext := Mux(cancel, false.B, wakeup | state)
    if (params.hasIQWakeUp) {
      cancelVec.get(srcIdx) := cancel
    }
  }
  entryUpdate.status.dataSources.zip(entryReg.status.dataSources).zip(srcWakeUpByIQVec).foreach {
    case ((dataSourceNext: DataSource, dataSource: DataSource), wakeUpByIQOH: Vec[Bool]) =>
      when(wakeUpByIQOH.asUInt.orR) {
        dataSourceNext.value := DataSource.forward
      }.elsewhen(dataSource.value === DataSource.forward) {
        dataSourceNext.value := DataSource.bypass
      }.otherwise {
        dataSourceNext.value := DataSource.reg
      }
  }
  if (params.hasIQWakeUp) {
    entryUpdate.status.srcWakeUpL1ExuOH.get.zip(srcWakeUpByIQVec).zip(srcWakeUp).zipWithIndex.foreach {
      case (((exuOH: Vec[Bool], wakeUpByIQOH: Vec[Bool]), wakeUp: Bool), srcIdx) =>
        when(wakeUpByIQOH.asUInt.orR) {
          exuOH := Mux1H(wakeUpByIQOH, io.wakeUpFromIQ.map(x => MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W)).toSeq).asBools
        }.elsewhen(wakeUp) {
          exuOH := 0.U.asTypeOf(exuOH)
        }.otherwise {
          exuOH := entryReg.status.srcWakeUpL1ExuOH.get(srcIdx)
        }
    }
    entryUpdate.status.srcTimer.get.zip(entryReg.status.srcTimer.get).zip(srcWakeUpByIQVec).zipWithIndex.foreach {
      case (((srcIssuedTimerNext, srcIssuedTimer), wakeUpByIQOH: Vec[Bool]), srcIdx) =>
        srcIssuedTimerNext := MuxCase(0.U, Seq(
          // T0: waked up by IQ, T1: reset timer as 1
          wakeUpByIQOH.asUInt.orR -> 1.U,
          // do not overflow
          srcIssuedTimer.andR -> srcIssuedTimer,
          // T2+: increase if the entry is valid, the src is ready, and the src is woken up by iq
          (validReg && SrcState.isReady(entryReg.status.srcState(srcIdx)) && entryReg.status.srcWakeUpL1ExuOH.get.asUInt.orR) -> (srcIssuedTimer + 1.U)
        ))
    }
    entryUpdate.status.srcLoadDependency.get.zip(entryReg.status.srcLoadDependency.get).zip(srcWakeUpByIQVec).zip(srcWakeUp).foreach {
      case (((loadDependencyNext, loadDependency), wakeUpByIQVec), wakeup) =>
        loadDependencyNext :=
          Mux(wakeup,
            Mux(wakeUpByIQVec.asUInt.orR, Mux1H(wakeUpByIQVec, shiftedWakeupLoadDependencyByIQVec), 0.U.asTypeOf(loadDependency)),
            Mux(validReg && loadDependency.asUInt.orR, VecInit(loadDependency.map(i => i(i.getWidth - 2, 0) << 1)), loadDependency)
          )
    }
  }
  entryUpdate.status.issueTimer := "b11".U //otherwise
  entryUpdate.status.deqPortIdx := 0.U //otherwise
  when(io.deqSel) {
    entryUpdate.status.issueTimer := 1.U
    entryUpdate.status.deqPortIdx := io.deqPortIdxWrite
  }.elsewhen(entryReg.status.issued){
    entryUpdate.status.issueTimer := entryReg.status.issueTimer + 1.U
    entryUpdate.status.deqPortIdx := entryReg.status.deqPortIdx
  }
  entryUpdate.status.psrc := entryReg.status.psrc
  entryUpdate.status.srcType := entryReg.status.srcType
  entryUpdate.status.fuType := entryReg.status.fuType
  entryUpdate.status.robIdx := entryReg.status.robIdx
  entryUpdate.status.issued := entryReg.status.issued // otherwise
  when(!entryReg.status.srcReady){
    entryUpdate.status.issued := false.B
  }.elsewhen(srcLoadCancelVec.map(_.reduce(_ || _)).getOrElse(false.B)) {
    entryUpdate.status.issued := false.B
  }.elsewhen(io.issueResp.valid) {
    when(RSFeedbackType.isStageSuccess(io.issueResp.bits.respType)) {
      entryUpdate.status.issued := true.B
    }.elsewhen(RSFeedbackType.isBlocked(io.issueResp.bits.respType)) {
      entryUpdate.status.issued := false.B
    }
  }
  entryUpdate.status.firstIssue := io.deqSel || entryReg.status.firstIssue
  entryUpdate.status.blocked := false.B //todo
  //remain imm and payload
  entryUpdate.imm := entryReg.imm
  entryUpdate.payload := entryReg.payload
  if(params.needPc) {
    entryUpdate.status.pc.get := entryReg.status.pc.get
  }

  //output
  io.transEntry.valid := validReg && io.transSel && !flushed && !deqSuccess
  io.transEntry.bits := entryUpdate
  io.canIssue := entryReg.status.canIssue && validReg
  io.clear := clear
  io.fuType := entryReg.status.fuType
  io.dataSource := entryReg.status.dataSources
  io.srcWakeUpL1ExuOH.foreach(_ := entryReg.status.srcWakeUpL1ExuOH.get)
  io.srcTimer.foreach(_ := entryReg.status.srcTimer.get)
  io.valid := validReg
  io.isFirstIssue := !entryReg.status.firstIssue
  io.entry.valid := validReg
  io.entry.bits := entryReg
  io.robIdx := entryReg.status.robIdx
  io.issueTimerRead := Mux(io.deqSel, 0.U, entryReg.status.issueTimer)
  io.deqPortIdxRead := Mux(io.deqSel, io.deqPortIdxWrite, entryReg.status.deqPortIdx)
  io.cancel.foreach(_ := cancelVec.get.asUInt.orR)
}

class EnqEntryMem()(implicit p: Parameters, params: IssueBlockParams) extends EnqEntry
  with HasCircularQueuePtrHelper {
  val fromMem = io.fromMem.get

  val memStatus = entryReg.status.mem.get
  println("memStatus" + memStatus)
  val memStatusNext = entryRegNext.status.mem.get
  val memStatusUpdate = entryUpdate.status.mem.get

  // load cannot be issued before older store, unless meet some condition
  val blockedByOlderStore = isAfter(memStatusNext.sqIdx, fromMem.stIssuePtr)

  val deqFailedForStdInvalid = io.issueResp.valid && io.issueResp.bits.respType === RSFeedbackType.dataInvalid

  val staWaitedReleased = Cat(
    fromMem.memWaitUpdateReq.robIdx.map(x => x.valid && x.bits.value === memStatusNext.waitForRobIdx.value)
  ).orR
  val stdWaitedReleased = Cat(
    fromMem.memWaitUpdateReq.sqIdx.map(x => x.valid && x.bits.value === memStatusNext.waitForSqIdx.value)
  ).orR
  val olderStaNotViolate = staWaitedReleased && !memStatusNext.strictWait
  val olderStdReady = stdWaitedReleased && memStatusNext.waitForStd
  val waitStd = !olderStdReady
  val waitSta = !olderStaNotViolate

  when (io.enq.valid && enqReady) {
    memStatusNext.waitForSqIdx := io.enq.bits.status.mem.get.waitForSqIdx
    // update by lfst at dispatch stage
    memStatusNext.waitForRobIdx := io.enq.bits.status.mem.get.waitForRobIdx
    // new load inst don't known if it is blocked by store data ahead of it
    memStatusNext.waitForStd := false.B
    // update by ssit at rename stage
    memStatusNext.strictWait := io.enq.bits.status.mem.get.strictWait
    memStatusNext.sqIdx := io.enq.bits.status.mem.get.sqIdx
  }.otherwise {
    memStatusNext := memStatusUpdate
  }

  when(deqFailedForStdInvalid) {
    memStatusUpdate.waitForSqIdx := io.issueResp.bits.dataInvalidSqIdx
    memStatusUpdate.waitForRobIdx := memStatus.waitForRobIdx
    memStatusUpdate.waitForStd := true.B
    memStatusUpdate.strictWait := memStatus.strictWait
    memStatusUpdate.sqIdx := memStatus.sqIdx
  }.otherwise {
    memStatusUpdate := memStatus
  }

  val shouldBlock = Mux(io.enq.valid && enqReady, io.enq.bits.status.blocked, entryReg.status.blocked)
  val blockNotReleased = waitStd || waitSta
  val respBlock = deqFailedForStdInvalid
  entryRegNext.status.blocked := shouldBlock && blockNotReleased && blockedByOlderStore || respBlock
  entryUpdate.status.blocked := shouldBlock && blockNotReleased && blockedByOlderStore || respBlock

}

object EnqEntry {
  def apply(implicit p: Parameters, iqParams: IssueBlockParams): EnqEntry = {
    iqParams.schdType match {
      case IntScheduler() => new EnqEntry()
      case MemScheduler() =>
        if (iqParams.StdCnt == 0) new EnqEntryMem()
        else new EnqEntry()
      case VfScheduler() => new EnqEntry()
      case _ => null
    }
  }
}