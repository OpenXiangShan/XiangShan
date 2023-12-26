package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils.{MathUtils, OptionWrapper}
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.issue.EntryBundles.{CommonInBundle, CommonOutBundle}


class EnqEntryIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  //input
  val commonIn            = new CommonInBundle
  val enqDelayWakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
  val enqDelayWakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
  val enqDelayOg0Cancel   = Input(ExuVec(backendParams.numExu))
  val enqDelayLdCancel    = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  //output
  val commonOut           = new CommonOutBundle

  def wakeup              = commonIn.wakeUpFromWB ++ commonIn.wakeUpFromIQ
}

class EnqEntry(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new EnqEntryIO)

  val validReg            = RegInit(false.B)
  val enqDelayValidReg    = RegInit(false.B)
  val entryReg            = Reg(new EntryBundle)

  val common              = Wire(new CommonWireBundle)
  val entryUpdate         = Wire(new EntryBundle)
  val entryRegNext        = Wire(new EntryBundle)
  val enqDelayValidRegNext= Wire(Bool())
  val hasWakeupIQ         = OptionWrapper(params.hasIQWakeUp, Wire(new CommonIQWakeupBundle))

  val currentStatus               = Wire(new Status())
  val enqDelaySrcState            = Wire(Vec(params.numRegSrc, SrcState()))
  val enqDelayDataSources         = Wire(Vec(params.numRegSrc, DataSource()))
  val enqDelaySrcWakeUpL1ExuOH    = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, ExuVec())))
  val enqDelaySrcTimer            = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, UInt(3.W))))
  val enqDelaySrcLoadDependency   = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(3.W)))))

  val enqDelaySrcWakeUpByWB: Vec[UInt]                            = Wire(Vec(params.numRegSrc, SrcState()))
  val enqDelaySrcWakeUpByIQ: Vec[UInt]                            = Wire(Vec(params.numRegSrc, SrcState()))
  val enqDelaySrcWakeUpByIQVec: Vec[Vec[Bool]]                    = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val enqDelayShiftedWakeupLoadDependencyByIQVec: Vec[Vec[UInt]]  = Wire(Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W))))

  //Reg
  validReg                        := common.validRegNext
  entryReg                        := entryRegNext
  enqDelayValidReg                := enqDelayValidRegNext

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
  enqDelaySrcWakeUpByWB.zipWithIndex.foreach { case (wakeup, i) =>
    wakeup := io.enqDelayWakeUpFromWB.map(x => x.bits.wakeUp(Seq((entryReg.status.srcStatus(i).psrc, entryReg.status.srcStatus(i).srcType)), x.valid).head
    ).reduce(_ || _)
  }

  if (params.hasIQWakeUp) {
    val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.enqDelayWakeUpFromIQ.map( x =>
      x.bits.wakeUpFromIQ(entryReg.status.srcStatus.map(_.psrc) zip entryReg.status.srcStatus.map(_.srcType))
    ).toIndexedSeq.transpose
    val cancelSel = params.wakeUpSourceExuIdx.zip(io.enqDelayWakeUpFromIQ).map{ case (x, y) => io.enqDelayOg0Cancel(x) && y.bits.is0Lat}
    enqDelaySrcWakeUpByIQVec := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
  } else {
    enqDelaySrcWakeUpByIQVec := 0.U.asTypeOf(enqDelaySrcWakeUpByIQVec)
  }

  if (params.hasIQWakeUp) {
    enqDelaySrcWakeUpByIQ.zipWithIndex.foreach { case (wakeup, i) =>
      val ldTransCancel = Mux1H(enqDelaySrcWakeUpByIQVec(i), io.enqDelayWakeUpFromIQ.map(_.bits.loadDependency).map(dp => LoadShouldCancel(Some(dp), io.enqDelayLdCancel)).toSeq)
      wakeup := enqDelaySrcWakeUpByIQVec(i).asUInt.orR && !ldTransCancel
    }
  } else {
    enqDelaySrcWakeUpByIQ := 0.U.asTypeOf(enqDelaySrcWakeUpByIQ)
  }

  enqDelayShiftedWakeupLoadDependencyByIQVec.zip(io.enqDelayWakeUpFromIQ.map(_.bits.loadDependency))
    .zip(params.wakeUpInExuSources.map(_.name)).foreach { case ((dps, ldps), name) =>
    dps.zip(ldps).zipWithIndex.foreach { case ((dp, ldp), i) =>
      if (name.contains("LDU") && name.replace("LDU", "").toInt == i)
        dp := (ldp << 2).asUInt | 2.U
      else
        dp := ldp << 1
    }
  }

  for (i <- 0 until params.numRegSrc) {
    enqDelaySrcState(i)                     := entryReg.status.srcStatus(i).srcState | enqDelaySrcWakeUpByWB(i) | enqDelaySrcWakeUpByIQ(i)
    enqDelayDataSources(i).value            := Mux(enqDelaySrcWakeUpByIQ(i).asBool, DataSource.bypass, DataSource.reg)
    if (params.hasIQWakeUp) {
      val wakeUpValid = enqDelaySrcWakeUpByIQVec(i).asUInt.orR
      val wakeUpOH = enqDelaySrcWakeUpByIQVec(i)
      enqDelaySrcWakeUpL1ExuOH.get(i)       := Mux1H(wakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W)).toSeq).asBools
      enqDelaySrcTimer.get(i)               := Mux(wakeUpValid, 2.U, 3.U)
      enqDelaySrcLoadDependency.get(i)      := Mux(wakeUpValid, Mux1H(wakeUpOH, enqDelayShiftedWakeupLoadDependencyByIQVec), entryReg.status.srcStatus(i).srcLoadDependency.get)
    }
  }
  currentStatus                             := entryReg.status
  when (enqDelayValidReg) {
    currentStatus.srcStatus.zipWithIndex.foreach { case (srcStatus, srcIdx) =>
      srcStatus.srcState                    := enqDelaySrcState(srcIdx)
      srcStatus.dataSources                 := enqDelayDataSources(srcIdx)
      srcStatus.srcTimer.foreach(_          := enqDelaySrcTimer.get(srcIdx))
      srcStatus.srcLoadDependency.foreach(_ := enqDelaySrcLoadDependency.get(srcIdx))
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

class EnqEntryMem(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends EnqEntry(isComp)
  with HasCircularQueuePtrHelper {
  EntryMemConnect(io.commonIn, common, validReg, entryReg, entryRegNext, entryUpdate, true)
}

object EnqEntry {
  def apply(isComp: Boolean)(implicit p: Parameters, iqParams: IssueBlockParams): EnqEntry = {
    iqParams.schdType match {
      case IntScheduler() => new EnqEntry(isComp)
      case MemScheduler() =>
        if (iqParams.StdCnt == 0) new EnqEntryMem(isComp)
        else new EnqEntry(isComp)
      case VfScheduler() => new EnqEntry(isComp)
      case _ => null
    }
  }
}