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
import xiangshan.mem.{SqPtr, LqPtr}
import xiangshan.mem.Bundles.MemWaitUpdateReqBundle


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
  val enqDelayValidRegNext= Wire(Bool())
  val hasWakeupIQ         = OptionWrapper(params.hasIQWakeUp, Wire(new CommonIQWakeupBundle))

  val currentStatus               = Wire(new Status())
  val enqDelaySrcState            = Wire(Vec(params.numRegSrc, SrcState()))
  val enqDelayDataSources         = Wire(Vec(params.numRegSrc, DataSource()))
  val enqDelayExuSources          = OptionWrapper(params.hasIQWakeUp, Wire(Vec(params.numRegSrc, ExuSource())))
  val enqDelaySrcLoadDependency   = Wire(Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W))))
  val enqDelayUseRegCache         = OptionWrapper(params.needReadRegCache, Wire(Vec(params.numRegSrc, Bool())))
  val enqDelayRegCacheIdx         = OptionWrapper(params.needReadRegCache, Wire(Vec(params.numRegSrc, UInt(RegCacheIdxWidth.W))))

  //Reg
  val validReg = GatedValidRegNext(common.validRegNext, false.B)
  val entryReg = RegNext(Mux(
    io.commonIn.enq.valid && common.enqReady,
    io.commonIn.enq.bits,
    entryUpdate
  ))
  val enqDelayValidReg = GatedValidRegNext(enqDelayValidRegNext, false.B)

  //Wire
  CommonWireConnect(common, hasWakeupIQ, validReg, entryReg.payload.og1Payload, currentStatus, io.commonIn, true)

  when(io.commonIn.enq.valid) {
    assert(common.enqReady, s"${params.getIQName}'s EnqEntry is not ready when enq is valid\n")
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
      enqDelayExuSources.get(i).value         := Mux(enqDelay1WakeUpValid,
                                                      ExuSource().fromExuOH(params, Mux1H(enqDelay1WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W)))),
                                                      ExuSource().fromExuOH(params, Mux1H(enqDelay2WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W)))))
    }
    else {
      enqDelayDataSources(i).value            := Mux(enqDelayOut1.srcWakeUpByIQ(i).asBool, DataSource.bypass, entryReg.status.srcStatus(i).dataSources.value)
      if (params.hasIQWakeUp) {
        enqDelayExuSources.get(i).value       := ExuSource().fromExuOH(params, Mux1H(enqDelay1WakeUpOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(backendParams.numExu.W))))
      }
    }

    enqDelaySrcState(i)                     := (!enqDelayOut1.srcCancelByLoad(i) & entryReg.status.srcStatus(i).srcState) | enqDelayOut1.srcWakeUpByWB(i) | enqDelayOut1.srcWakeUpByIQ(i)
    if (params.hasIQWakeUp) {
      enqDelaySrcLoadDependency(i)          := Mux(enqDelay1WakeUpValid, Mux1H(enqDelay1WakeUpOH, enqDelayOut1.shiftedWakeupLoadDependencyByIQVec), entryReg.status.srcStatus(i).srcLoadDependency)
    } else {
      enqDelaySrcLoadDependency(i)          := entryReg.status.srcStatus(i).srcLoadDependency
    }

    if (params.needReadRegCache) {
      val enqDelay1WakeupSrcExuWriteRC = enqDelay1WakeUpOH.zip(io.enqDelayIn1.wakeUpFromIQ).filter(_._2.bits.params.needWriteRegCache)
      val enqDelay1WakeupRC    = enqDelay1WakeupSrcExuWriteRC.map(_._1).fold(false.B)(_ || _) && SrcType.isXp(entryReg.status.srcStatus(i).srcType)
      val enqDelay1WakeupRCIdx = Mux1H(enqDelay1WakeupSrcExuWriteRC.map(_._1), enqDelay1WakeupSrcExuWriteRC.map(_._2.bits.rcDest.get))
      val enqDelay1ReplaceRC   = enqDelay1WakeupSrcExuWriteRC.map(x => x._2.bits.rfWen && x._2.bits.rcDest.get === entryReg.status.srcStatus(i).regCacheIdx.get).fold(false.B)(_ || _)

      enqDelayUseRegCache.get(i)            := entryReg.status.srcStatus(i).useRegCache.get && !(enqDelayOut1.srcCancelByLoad(i) || enqDelay1ReplaceRC) || enqDelay1WakeupRC
      enqDelayRegCacheIdx.get(i)            := Mux(enqDelay1WakeupRC, enqDelay1WakeupRCIdx, entryReg.status.srcStatus(i).regCacheIdx.get)
    }
  }

  val enqDelay1LoadWakeupHitOH = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  val enqDelay2LoadWakeupHitOH = Wire(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool())))
  enqDelay1LoadWakeupHitOH.zip(enqDelayOut1.srcWakeUpByIQVec).foreach { case (hitOH, wakeupOH) =>
    if (params.hasIQWakeUp) {
      hitOH := VecInit(wakeupOH.zip(io.enqDelayIn1.wakeUpFromIQ).map {
        case (hit, wakeup) => hit && wakeup.bits.params.hasLoadExu.B
      })
    } else {
      hitOH := 0.U.asTypeOf(hitOH)
    }
  }
  enqDelay2LoadWakeupHitOH.zip(enqDelayOut2.srcWakeUpByIQVec).foreach { case (hitOH, wakeupOH) =>
    if (params.hasIQWakeUp) {
      hitOH := VecInit(wakeupOH.zip(io.enqDelayIn2.wakeUpFromIQ).map {
        case (hit, wakeup) => hit && wakeup.bits.params.hasLoadExu.B
      })
    } else {
      hitOH := 0.U.asTypeOf(hitOH)
    }
  }
  // Only src(0) contributes to a scalar load address.  Wakeups of the other
  // issue sources must not train MDP as address-producing load dependencies.
  val enqDelay1LoadBaseWakeupHitOH = if (params.hasIQWakeUp) enqDelay1LoadWakeupHitOH.head else VecInit(Seq(false.B))
  val enqDelay2LoadBaseWakeupHitOH = if (params.hasIQWakeUp) enqDelay2LoadWakeupHitOH.head else VecInit(Seq(false.B))
  val enqDelay1LoadWakeupHit = enqDelay1LoadBaseWakeupHitOH.asUInt.orR
  val enqDelay2LoadWakeupHit = enqDelay2LoadBaseWakeupHitOH.asUInt.orR
  val enqDelayLoadWakeupHit = enqDelay1LoadWakeupHit || enqDelay2LoadWakeupHit
  val enqDelayIsLoadEntry = FuType.isLoadVload(IQFuType.readFuType(entryReg.status.fuType, params.getFuCfgs.map(_.fuType)).asUInt)
  val enqDelayLoadWakeupHitByIQ = Wire(Vec(params.numWakeupFromIQ, Bool()))
  enqDelayLoadWakeupHitByIQ.zipWithIndex.foreach { case (hit, iqIdx) =>
    hit := enqDelayIsLoadEntry && enqDelayValidReg && (
      enqDelay1LoadBaseWakeupHitOH(iqIdx) ||
      enqDelay2LoadBaseWakeupHitOH(iqIdx)
    )
  }
  val enqDelayLoadWakeupPCByIQ = Wire(Vec(params.numWakeupFromIQ, UInt(VAddrBits.W)))
  enqDelayLoadWakeupPCByIQ.zipWithIndex.foreach { case (pc, iqIdx) =>
    val delay1Hit = enqDelay1LoadBaseWakeupHitOH(iqIdx)
    val delay2Hit = enqDelay2LoadBaseWakeupHitOH(iqIdx)
    pc := MuxCase(0.U(VAddrBits.W), Seq(
      delay1Hit -> io.enqDelayIn1.wakeUpFromIQ(iqIdx).bits.pc,
      delay2Hit -> io.enqDelayIn2.wakeUpFromIQ(iqIdx).bits.pc
    ))
  }
  val enqDelay1LoadWakeupPC = if (params.hasIQWakeUp) {
    Mux1H(enqDelay1LoadBaseWakeupHitOH, io.enqDelayIn1.wakeUpFromIQ.map(_.bits.pc))
  } else {
    0.U(VAddrBits.W)
  }
  val enqDelay2LoadWakeupPC = if (params.hasIQWakeUp) {
    Mux1H(enqDelay2LoadBaseWakeupHitOH, io.enqDelayIn2.wakeUpFromIQ.map(_.bits.pc))
  } else {
    0.U(VAddrBits.W)
  }
  val enqDelayLoadWakeupPC = Mux(enqDelay1LoadWakeupHit, enqDelay1LoadWakeupPC, enqDelay2LoadWakeupPC)

  // current status
  currentStatus                             := entryReg.status
  when (enqDelayValidReg) {
    currentStatus.srcStatus.zipWithIndex.foreach { case (srcStatus, srcIdx) =>
      srcStatus.srcState                    := enqDelaySrcState(srcIdx)
      srcStatus.dataSources                 := enqDelayDataSources(srcIdx)
      srcStatus.srcLoadDependency           := enqDelaySrcLoadDependency(srcIdx)
      srcStatus.useRegCache.foreach(_       := enqDelayUseRegCache.get(srcIdx))
      srcStatus.regCacheIdx.foreach(_       := enqDelayRegCacheIdx.get(srcIdx))
    }
    currentStatus.srcStatusVl.foreach { case srcStatusVl =>
      srcStatusVl.srcState := entryReg.status.srcStatusVl.get.srcState | enqDelayOut1.srcVlWakeUpByWB.get
      srcStatusVl.dataSource.value := DataSource.reg // change it when support fast wakeup.
    }
    when (enqDelayIsLoadEntry && enqDelayLoadWakeupHit && !entryReg.status.wakedup) {
      currentStatus.wakedup                  := true.B
      currentStatus.wakedupPC                := enqDelayLoadWakeupPC
    }
  }

  if (params.hasIQWakeUp) {
    currentStatus.srcStatus.map(_.exuSources.get).zip(entryReg.status.srcStatus.map(_.exuSources.get)).zip(enqDelayExuSources.get).foreach {
      case ((currExu, regExu), enqDelayExu) =>
        currExu := Mux(enqDelayValidReg, enqDelayExu, regExu)
    }
  }

  EntryRegCommonConnect(common, hasWakeupIQ, validReg, entryUpdate, entryReg, currentStatus, io.commonIn, true, isComp)

  //output
  CommonOutConnect(io.commonOut, common, hasWakeupIQ, validReg, entryUpdate, entryReg, currentStatus, io.commonIn, true, isComp)
  io.commonOut.perfEnqDelayLoadWakeupByIQ.foreach(_ := enqDelayLoadWakeupHitByIQ)
  io.commonOut.perfEnqDelayLoadWakeupPCByIQ.foreach(_ := enqDelayLoadWakeupPCByIQ)
}

class EnqEntryVecMem(isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) extends EnqEntry(isComp)
  with HasCircularQueuePtrHelper {

  require(params.isVecMemIQ, "EnqEntryVecMem can only be instance of VecMem IQ")

  EntryVecMemConnect(io.commonIn, entryReg, entryUpdate)
}

object EnqEntry {
  def apply(isComp: Boolean)(implicit p: Parameters, iqParams: IssueBlockParams): EnqEntry = {
    iqParams.schdType match {
      case IntScheduler() => new EnqEntry(isComp)
      case FpScheduler()  => new EnqEntry(isComp)
      case VecScheduler() =>
        if (iqParams.isVecMemIQ) new EnqEntryVecMem(isComp)
        else new EnqEntry(isComp)
      case _ => null
    }
  }
}
