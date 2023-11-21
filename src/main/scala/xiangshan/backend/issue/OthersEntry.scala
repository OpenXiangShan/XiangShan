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


class OthersEntryIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  //input
  val enq = Flipped(ValidIO(new EntryBundle))
  val flush = Flipped(ValidIO(new Redirect))
  val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val og0Cancel = Input(ExuVec(backendParams.numExu))
  val og1Cancel = Input(ExuVec(backendParams.numExu))
  val ldCancel = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  val deqSel = Input(Bool())
  val transSel = Input(Bool())
  val issueResp = Flipped(ValidIO(new EntryDeqRespBundle))
  val deqPortIdxWrite = Input(UInt(1.W))
  //output
  val valid = Output(Bool())
  val canIssue = Output(Bool())
  val clear = Output(Bool())
  val fuType = Output(FuType())
  val dataSource = Output(Vec(params.numRegSrc, DataSource()))
  val srcWakeUpL1ExuOH = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, ExuVec())))
  val srcTimer = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, UInt(3.W))))
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

class OthersEntry(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new OthersEntryIO)

  val validReg = RegInit(false.B)
  val entryReg = Reg(new EntryBundle)

  val validRegNext = Wire(Bool())
  val entryRegNext = Wire(new EntryBundle)
  val flushed = Wire(Bool())
  val clear = Wire(Bool())
  val deqSuccess = Wire(Bool())
  val srcWakeUp = Wire(Vec(params.numRegSrc, Bool()))
  val srcWakeUpByWB = Wire(Vec(params.numRegSrc, Bool()))
  val srcCancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))
  val srcLoadCancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))
  val srcWakeUpByIQVec = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val srcWakeUpByIQWithoutCancel = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val srcWakeUpL1ExuOHOut = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, ExuVec())))
  val srcLoadDependencyOut = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(3.W)))))
  val srcWakeUpButCancel = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val wakeupLoadDependencyByIQVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  val shiftedWakeupLoadDependencyByIQVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  val shiftedWakeupLoadDependencyByIQBypassVec = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))
  val cancelVec = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Bool())))

  //Reg
  validReg := validRegNext
  entryReg := entryRegNext

  //Wire
  flushed := entryReg.status.robIdx.needFlush(io.flush)
  clear := flushed || deqSuccess
  deqSuccess := io.issueResp.valid && io.issueResp.bits.respType === RSFeedbackType.fuIdle && !srcLoadCancelVec.map(_.reduce(_ || _)).getOrElse(false.B)
  srcWakeUpByWB := io.wakeUpFromWB.map(bundle => bundle.bits.wakeUp(entryReg.status.psrc zip entryReg.status.srcType, bundle.valid)).transpose.map(x => VecInit(x.toSeq).asUInt.orR).toSeq
  srcWakeUp := srcWakeUpByWB.zip(srcWakeUpByIQVec).map { case (x, y) => x || y.asUInt.orR }

  shiftedWakeupLoadDependencyByIQVec
    .zip(wakeupLoadDependencyByIQVec)
    .zip(params.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
          dep := (originalDep << 2).asUInt | 2.U
        else
          dep := originalDep << 1
    }
  }
  shiftedWakeupLoadDependencyByIQBypassVec
    .zip(wakeupLoadDependencyByIQVec)
    .zip(params.wakeUpInExuSources.map(_.name)).foreach {
    case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
      case ((dep, originalDep), deqPortIdx) =>
        if (name.contains("LDU") && name.replace("LDU", "").toInt == deqPortIdx)
          dep := (originalDep << 1).asUInt | 1.U
        else
          dep := originalDep
    }
  }

  when(io.enq.valid && io.transSel) {
    validRegNext := true.B
  }.elsewhen(clear) {
    validRegNext := false.B
  }.otherwise {
    validRegNext := validReg
  }

  if (params.hasIQWakeUp) {
    srcCancelVec.get.zip(srcLoadCancelVec.get).zip(srcWakeUpByIQVec).zipWithIndex.foreach { case (((srcCancel, srcLoadCancel), wakeUpByIQVec), srcIdx) =>
      val ldTransCancel = Mux1H(wakeUpByIQVec, wakeupLoadDependencyByIQVec.map(dep => LoadShouldCancel(Some(dep), io.ldCancel)))
      srcLoadCancel := LoadShouldCancel(entryReg.status.srcLoadDependency.map(_(srcIdx)), io.ldCancel)
      srcCancel := srcLoadCancel || ldTransCancel
    }
    cancelVec.get.foreach(_ := false.B)
  }

  if (io.wakeUpFromIQ.isEmpty) {
    srcWakeUpByIQVec := 0.U.asTypeOf(srcWakeUpByIQVec)
    wakeupLoadDependencyByIQVec := 0.U.asTypeOf(wakeupLoadDependencyByIQVec)
  } else {
    val wakeupVec: Seq[Seq[Bool]] = io.wakeUpFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
      bundle.bits.wakeUp(entryReg.status.psrc zip entryReg.status.srcType, bundle.valid)
    ).toSeq.transpose
    val cancelSel = io.wakeUpFromIQ.map(x => x.bits.exuIdx).map(x => io.og0Cancel(x))
    srcWakeUpByIQVec := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    srcWakeUpButCancel := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && cancel }))
    srcWakeUpByIQWithoutCancel := wakeupVec.map(x => VecInit(x))
    wakeupLoadDependencyByIQVec := io.wakeUpFromIQ.map(_.bits.loadDependency).toSeq
  }

  when(io.enq.valid && io.transSel) {
    entryRegNext := io.enq.bits
  }.otherwise {
    //update status
    entryRegNext.status.srcState.zip(entryReg.status.srcState).zip(srcWakeUp).zipWithIndex.foreach { case (((stateNext, state), wakeup), srcIdx) =>
      val cancel = srcCancelVec.map(_ (srcIdx)).getOrElse(false.B)
      stateNext := Mux(cancel, false.B, wakeup | state)
      if (params.hasIQWakeUp) {
        cancelVec.get(srcIdx) := cancel
      }
    }
    entryRegNext.status.dataSources.zip(entryReg.status.dataSources).zip(srcWakeUpByIQVec).foreach {
      case ((dataSourceNext: DataSource, dataSource: DataSource), wakeUpByIQOH: Vec[Bool]) =>
        when(wakeUpByIQOH.asUInt.orR) {
          dataSourceNext.value := DataSource.bypass
        }.otherwise {
          dataSourceNext.value := DataSource.reg
        }
    }
    if (params.hasIQWakeUp) {
      entryRegNext.status.srcWakeUpL1ExuOH.get.zip(srcWakeUpByIQVec).zip(srcWakeUp).zipWithIndex.foreach {
        case (((exuOH: Vec[Bool], wakeUpByIQOH: Vec[Bool]), wakeUp: Bool), srcIdx) =>
          when(wakeUpByIQOH.asUInt.orR) {
            exuOH := Mux1H(wakeUpByIQOH, io.wakeUpFromIQ.map(x => MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W)).toSeq).asBools
          }.elsewhen(wakeUp) {
            exuOH := 0.U.asTypeOf(exuOH)
          }.otherwise {
            exuOH := entryReg.status.srcWakeUpL1ExuOH.get(srcIdx)
          }
      }
      entryRegNext.status.srcTimer.get.zip(entryReg.status.srcTimer.get).zip(srcWakeUpByIQVec).zipWithIndex.foreach {
        case (((srcIssuedTimerNext, srcIssuedTimer), wakeUpByIQOH: Vec[Bool]), srcIdx) =>
          srcIssuedTimerNext := MuxCase(3.U, Seq(
            // T0: waked up by IQ, T1: reset timer as 1
            wakeUpByIQOH.asUInt.orR -> 2.U,
            // do not overflow
            srcIssuedTimer.andR -> srcIssuedTimer,
            // T2+: increase if the entry is valid, the src is ready, and the src is woken up by iq
            (validReg && SrcState.isReady(entryReg.status.srcState(srcIdx)) && entryReg.status.srcWakeUpL1ExuOH.get.asUInt.orR) -> (srcIssuedTimer + 1.U)
          ))
      }
      entryRegNext.status.srcLoadDependency.get.zip(entryReg.status.srcLoadDependency.get).zip(srcWakeUpByIQVec).zip(srcWakeUp).foreach {
        case (((loadDependencyNext, loadDependency), wakeUpByIQVec), wakeup) =>
          loadDependencyNext :=
            Mux(wakeup,
              Mux1H(wakeUpByIQVec, shiftedWakeupLoadDependencyByIQVec),
              Mux(validReg && loadDependency.asUInt.orR, VecInit(loadDependency.map(i => i(i.getWidth - 2, 0) << 1)), loadDependency)
            )
      }
    }
    entryRegNext.status.issueTimer := "b10".U //otherwise
    entryRegNext.status.deqPortIdx := 0.U //otherwise
    when(io.deqSel){
      entryRegNext.status.issueTimer := 0.U
      entryRegNext.status.deqPortIdx := io.deqPortIdxWrite
    }.elsewhen(entryReg.status.issued){
      entryRegNext.status.issueTimer := entryReg.status.issueTimer + 1.U
      entryRegNext.status.deqPortIdx := entryReg.status.deqPortIdx
    }.otherwise {
      entryRegNext.status.issueTimer := "b10".U
      entryRegNext.status.deqPortIdx := 0.U
    }
    entryRegNext.status.psrc := entryReg.status.psrc
    entryRegNext.status.srcType := entryReg.status.srcType
    entryRegNext.status.fuType := entryReg.status.fuType
    entryRegNext.status.robIdx := entryReg.status.robIdx
    when(srcLoadCancelVec.map(_.reduce(_ || _)).getOrElse(false.B) || srcWakeUpButCancel.map(_.fold(false.B)(_ || _)).fold(false.B)(_ || _)) {
      entryRegNext.status.issued := false.B
    }.elsewhen(io.deqSel) {
      entryRegNext.status.issued := true.B
    }.elsewhen(io.issueResp.valid && RSFeedbackType.isBlocked(io.issueResp.bits.respType)) {
      entryRegNext.status.issued := false.B
    }.elsewhen(!entryReg.status.srcReady) {
      entryRegNext.status.issued := false.B
    }.otherwise {
      entryRegNext.status.issued := entryReg.status.issued
    }
    entryRegNext.status.firstIssue := io.deqSel || entryReg.status.firstIssue
    entryRegNext.status.blocked := false.B //todo
    //remain imm and payload
    entryRegNext.imm := entryReg.imm
    entryRegNext.payload := entryReg.payload
    if (params.needPc) {
      entryRegNext.status.pc.get := entryReg.status.pc.get
    }
  }

  //output
  val canIssue = entryReg.status.canIssue && validReg && !srcCancelVec.getOrElse(false.B).asUInt.orR
  val canIssueBypass = validReg && !entryReg.status.issued && !entryReg.status.blocked &&
    VecInit(entryReg.status.srcState.zip(srcWakeUpByIQWithoutCancel).zipWithIndex.map { case ((state, wakeupVec), srcIdx) =>
      val cancel = srcCancelVec.map(_ (srcIdx)).getOrElse(false.B)
      Mux(cancel, false.B, wakeupVec.asUInt.orR | state)
    }).asUInt.andR
  io.dataSource.zip(entryReg.status.dataSources).zip(srcWakeUpByIQVec).zip(srcWakeUp).foreach {
    case (((dataSourceOut: DataSource, dataSource: DataSource), wakeUpByIQOH: Vec[Bool]), wakeUpAll) =>
      when(wakeUpByIQOH.asUInt.orR) {
        dataSourceOut.value := DataSource.forward
      }.elsewhen(wakeUpAll) {
        dataSourceOut.value := DataSource.reg
      }.otherwise {
        dataSourceOut.value := dataSource.value
      }
  }
  if (params.hasIQWakeUp) {
    srcWakeUpL1ExuOHOut.get.zip(srcWakeUpByIQWithoutCancel).zip(srcWakeUp).zipWithIndex.foreach {
      case (((exuOH: Vec[Bool], wakeUpByIQOH: Vec[Bool]), wakeUp: Bool), srcIdx) =>
        when(wakeUpByIQOH.asUInt.orR) {
          exuOH := Mux1H(wakeUpByIQOH, io.wakeUpFromIQ.map(x => MathUtils.IntToOH(x.bits.exuIdx).U(backendParams.numExu.W)).toSeq).asBools
        }.elsewhen(wakeUp) {
          exuOH := 0.U.asTypeOf(exuOH)
        }.otherwise {
          exuOH := entryReg.status.srcWakeUpL1ExuOH.get(srcIdx)
        }
    }
    srcLoadDependencyOut.get.zip(entryReg.status.srcLoadDependency.get).zip(srcWakeUpByIQVec).zip(srcWakeUp).foreach {
      case (((loadDependencyOut, loadDependency), wakeUpByIQVec), wakeup) =>
        loadDependencyOut :=
          Mux(wakeup,
            Mux1H(wakeUpByIQVec, shiftedWakeupLoadDependencyByIQBypassVec),
            loadDependency
          )
    }
    io.srcTimer.get.zip(entryReg.status.srcTimer.get).zip(srcWakeUpByIQWithoutCancel).zip(srcWakeUp).foreach {
      case (((srcTimerOut, srcTimer), wakeUpByIQOH: Vec[Bool]), wakeUpAll) =>
        when(wakeUpByIQOH.asUInt.orR) {
          srcTimerOut := 1.U
        }.otherwise {
          srcTimerOut := srcTimer
        }
    }
    io.srcWakeUpL1ExuOH.get := Mux(canIssueBypass && !canIssue, srcWakeUpL1ExuOHOut.get, entryReg.status.srcWakeUpL1ExuOH.get)
  }
  io.canIssue := (canIssue || canIssueBypass) && !flushed
  io.clear := clear
  io.fuType := entryReg.status.fuType
  io.valid := validReg
  io.isFirstIssue := !entryReg.status.firstIssue
  io.entry.valid := validReg
  io.entry.bits := entryReg
  io.entry.bits.status.srcLoadDependency.foreach(_ := Mux(canIssueBypass && !canIssue, srcLoadDependencyOut.get, entryReg.status.srcLoadDependency.get))
  io.robIdx := entryReg.status.robIdx
  io.issueTimerRead := entryReg.status.issueTimer
  io.deqPortIdxRead := entryReg.status.deqPortIdx
  io.cancel.foreach(_ := cancelVec.get.asUInt.orR)
}

class OthersEntryMem()(implicit p: Parameters, params: IssueBlockParams) extends OthersEntry
  with HasCircularQueuePtrHelper {

  val fromMem = io.fromMem.get

  val memStatus = entryReg.status.mem.get
  val memStatusNext = entryRegNext.status.mem.get
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

  when(io.enq.valid && io.transSel) {
    memStatusNext.waitForSqIdx := io.enq.bits.status.mem.get.waitForSqIdx
    // update by lfst at dispatch stage
    memStatusNext.waitForRobIdx := io.enq.bits.status.mem.get.waitForRobIdx
    // new load inst don't known if it is blocked by store data ahead of it
    memStatusNext.waitForStd := false.B
    // update by ssit at rename stage
    memStatusNext.strictWait := io.enq.bits.status.mem.get.strictWait
    memStatusNext.sqIdx := io.enq.bits.status.mem.get.sqIdx
  }.elsewhen(deqFailedForStdInvalid) {
    // Todo: check if need assign statusNext.block
    memStatusNext.waitForSqIdx := io.issueResp.bits.dataInvalidSqIdx
    memStatusNext.waitForRobIdx := memStatus.waitForRobIdx
    memStatusNext.waitForStd := true.B
    memStatusNext.strictWait := memStatus.strictWait
    memStatusNext.sqIdx := memStatus.sqIdx
  }.otherwise {
    memStatusNext := memStatus
  }

  val shouldBlock = Mux(io.enq.valid && io.transSel, io.enq.bits.status.blocked, entryReg.status.blocked)
  val blockNotReleased = waitStd || waitSta
  val respBlock = deqFailedForStdInvalid
  entryRegNext.status.blocked := shouldBlock && blockNotReleased && blockedByOlderStore || respBlock
}

object OthersEntry {
  def apply(implicit p: Parameters, iqParams: IssueBlockParams): OthersEntry = {
    iqParams.schdType match {
      case IntScheduler() => new OthersEntry()
      case MemScheduler() =>
        if (iqParams.StdCnt == 0) new OthersEntryMem()
        else new OthersEntry()
      case VfScheduler() => new OthersEntry()
      case _ => null
    }
  }
}