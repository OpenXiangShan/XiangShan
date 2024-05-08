package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{HasCircularQueuePtrHelper, GatedValidRegNext}
import utils.{MathUtils, OptionWrapper}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.FuType
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.issue.EntryBundles._
import xiangshan.mem.{MemWaitUpdateReq, SqPtr, LqPtr}


class EnqEntryIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  //input
  val commonIn            = new CommonInBundle
  val enqDelayIn1         = new EnqDelayInBundle
  val enqDelayIn2         = new EnqDelayInBundle

  //output
  val commonOut           = new CommonOutBundle

  def wakeup              = commonIn.wakeUpFromWB ++ commonIn.wakeUpFromIQ
}

class EnqEntry(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new EnqEntryIO)

  val common              = Wire(new CommonWireBundle)
  val entryUpdate         = Wire(new EntryBundle)
  val entryRegNext        = Wire(new EntryBundle)
  val enqDelayValidRegNext= Wire(Bool())
  val hasWakeupIQ         = OptionWrapper(params.hasIQWakeUp, Wire(new CommonIQWakeupBundle))

  val currentStatus               = Wire(new Status())
  val enqDelaySrcState            = Wire(Vec(params.numRegSrc, SrcState()))
  val enqDelayDataSources         = Wire(Vec(params.numRegSrc, DataSource()))
  val enqDelaySrcWakeUpL1ExuOH    = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, ExuOH())))
  val enqDelaySrcLoadDependency   = Wire(Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))

  //Reg
  val validReg = GatedValidRegNext(common.validRegNext, false.B)
  val entryReg = RegEnable(entryRegNext, validReg || common.validRegNext)
  val enqDelayValidReg = GatedValidRegNext(enqDelayValidRegNext, false.B)

  //Wire
  CommonWireConnect(common, hasWakeupIQ, validReg, currentStatus, io.commonIn, true)

  when(io.commonIn.enq.valid) {
    assert(common.enqReady, "Entry is not ready when enq is valid\n")
  }

  when(io.commonIn.enq.valid && common.enqReady) {
    entryRegNext := io.commonIn.enq.bits
  }.otherwise {
    entryRegNext := entryUpdate
  }

  when(io.commonIn.enq.valid && common.enqReady) {
    enqDelayValidRegNext := true.B
  }.otherwise {
    enqDelayValidRegNext := false.B
  }

  if (params.hasIQWakeUp) {
    ShiftLoadDependency(hasWakeupIQ.get)
    CommonIQWakeupConnect(common, hasWakeupIQ.get, validReg, currentStatus, io.commonIn, true)
  }

  // enq delay wakeup
  val enqDelayOut1         = Wire(new EnqDelayOutBundle)
  val enqDelayOut2         = Wire(new EnqDelayOutBundle)
  EnqDelayWakeupConnect(io.enqDelayIn1, enqDelayOut1, entryReg.status, delay = 1)
  EnqDelayWakeupConnect(io.enqDelayIn2, enqDelayOut2, entryReg.status, delay = 2)

  for (i <- 0 until params.numRegSrc) {
    val enqDelay1WakeUpValid = enqDelayOut1.srcWakeUpByIQVec(i).asUInt.orR
    val enqDelay1WakeUpOH    = enqDelayOut1.srcWakeUpByIQVec(i)
    val enqDelay2WakeUpOH    = enqDelayOut2.srcWakeUpByIQVec(i)
    val enqDelay1IsWakeupByMemIQ = enqDelay1WakeUpOH.zip(io.commonIn.wakeUpFromIQ).filter(_._2.bits.params.isMemExeUnit).map(_._1).fold(false.B)(_ || _)
    val enqDelay2IsWakeupByMemIQ = enqDelay2WakeUpOH.zip(io.commonIn.wakeUpFromIQ).filter(_._2.bits.params.isMemExeUnit).map(_._1).fold(false.B)(_ || _)
    val enqDelay2IsWakeupByVfIQ  = enqDelay2WakeUpOH.zip(io.commonIn.wakeUpFromIQ).filter(_._2.bits.params.isVfExeUnit).map(_._1).fold(false.B)(_ || _)

    if (params.inVfSchd && params.readVfRf && params.hasIQWakeUp) {
      enqDelayDataSources(i).value            := MuxCase(entryReg.status.srcStatus(i).dataSources.value, Seq(
                                                    (enqDelayOut1.srcWakeUpByIQ(i).asBool && !enqDelay1IsWakeupByMemIQ)  -> DataSource.bypass,
                                                    (enqDelayOut1.srcWakeUpByIQ(i).asBool && enqDelay1IsWakeupByMemIQ)   -> DataSource.bypass2,
                                                    (enqDelayOut2.srcWakeUpByIQ(i).asBool && !enqDelay2IsWakeupByMemIQ)  -> DataSource.bypass2,
                                                 ))
      enqDelaySrcWakeUpL1ExuOH.get(i)         := Mux(enqDelay1WakeUpValid, 
                                                      Mux1H(enqDelay1WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W))), 
                                                      Mux1H(enqDelay2WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W))))
    }
    else if (params.inMemSchd && params.readVfRf && params.hasIQWakeUp) {
      enqDelayDataSources(i).value            := MuxCase(entryReg.status.srcStatus(i).dataSources.value, Seq(
                                                    enqDelayOut1.srcWakeUpByIQ(i).asBool                                 -> DataSource.bypass,
                                                    (enqDelayOut2.srcWakeUpByIQ(i).asBool && enqDelay2IsWakeupByVfIQ)    -> DataSource.bypass2,
                                                 ))
      enqDelaySrcWakeUpL1ExuOH.get(i)         := Mux(enqDelay1WakeUpValid, 
                                                      Mux1H(enqDelay1WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W))), 
                                                      Mux1H(enqDelay2WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W))))
    }
    else {
      enqDelayDataSources(i).value            := Mux(enqDelayOut1.srcWakeUpByIQ(i).asBool, DataSource.bypass, entryReg.status.srcStatus(i).dataSources.value)
      if (params.hasIQWakeUp) {
        enqDelaySrcWakeUpL1ExuOH.get(i)       := Mux1H(enqDelay1WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W)).toSeq)
      }
    }

    enqDelaySrcState(i)                     := entryReg.status.srcStatus(i).srcState | enqDelayOut1.srcWakeUpByWB(i) | enqDelayOut1.srcWakeUpByIQ(i)
    if (params.hasIQWakeUp) {
      enqDelaySrcLoadDependency(i)          := Mux(enqDelay1WakeUpValid, Mux1H(enqDelay1WakeUpOH, enqDelayOut1.shiftedWakeupLoadDependencyByIQVec), entryReg.status.srcStatus(i).srcLoadDependency)
    } else {
      enqDelaySrcLoadDependency(i)          := entryReg.status.srcStatus(i).srcLoadDependency
    }
  }

  // current status
  currentStatus                             := entryReg.status
  when (enqDelayValidReg) {
    currentStatus.srcStatus.zipWithIndex.foreach { case (srcStatus, srcIdx) =>
      srcStatus.srcState                    := enqDelaySrcState(srcIdx)
      srcStatus.dataSources                 := enqDelayDataSources(srcIdx)
      srcStatus.srcLoadDependency           := enqDelaySrcLoadDependency(srcIdx)
    }
  }

  if (params.hasIQWakeUp) {
    currentStatus.srcStatus.map(_.srcWakeUpL1ExuOH.get).zip(entryReg.status.srcStatus.map(_.srcWakeUpL1ExuOH.get)).zip(enqDelaySrcWakeUpL1ExuOH.get).foreach {
      case ((currExuOH, regExuOH), enqDelayExuOH) =>
        currExuOH := 0.U.asTypeOf(currExuOH)
        params.wakeUpSourceExuIdx.foreach(x => currExuOH(x) := Mux(enqDelayValidReg, enqDelayExuOH(x), regExuOH(x)))
    }
  }

  EntryRegCommonConnect(common, hasWakeupIQ, validReg, entryUpdate, entryReg, currentStatus, io.commonIn, true)

  //output
  CommonOutConnect(io.commonOut, common, hasWakeupIQ, validReg, entryUpdate, entryReg, currentStatus, io.commonIn, true, isComp)
}

class EnqEntryVecMem(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends EnqEntry(isComp)
  with HasCircularQueuePtrHelper {

  require(params.isVecMemIQ, "EnqEntryVecMem can only be instance of VecMem IQ")

  EntryVecMemConnect(io.commonIn, common, validReg, entryReg, entryRegNext, entryUpdate)
}

object EnqEntry {
  def apply(isComp: Boolean)(implicit p: Parameters, iqParams: IssueBlockParams): EnqEntry = {
    iqParams.schdType match {
      case IntScheduler() => new EnqEntry(isComp)
      case MemScheduler() =>
        if (iqParams.isVecMemIQ) new EnqEntryVecMem(isComp)
        else new EnqEntry(isComp)
      case VfScheduler() => new EnqEntry(isComp)
      case _ => null
    }
  }
}