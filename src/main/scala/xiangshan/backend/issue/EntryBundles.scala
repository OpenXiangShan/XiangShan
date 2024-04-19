package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.{MathUtils, OptionWrapper}
import utility.HasCircularQueuePtrHelper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{MemWaitUpdateReq, SqPtr, LqPtr}

object EntryBundles extends HasCircularQueuePtrHelper {

  class Status(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    //basic status
    val robIdx                = new RobPtr
    val fuType                = IQFuType()
    //src status
    val srcStatus             = Vec(params.numRegSrc, new SrcStatus)
    //issue status
    val blocked               = Bool()
    val issued                = Bool()
    val firstIssue            = Bool()
    val issueTimer            = UInt(2.W)
    val deqPortIdx            = UInt(1.W)
    //vector mem status
    val vecMem                = OptionWrapper(params.isVecMemIQ, new StatusVecMemPart)

    def srcReady: Bool        = {
      VecInit(srcStatus.map(_.srcState).map(SrcState.isReady)).asUInt.andR
    }

    def canIssue: Bool        = {
      srcReady && !issued && !blocked
    }

    def mergedLoadDependency: Vec[UInt] = {
      srcStatus.map(_.srcLoadDependency).reduce({
        case (l: Vec[UInt], r: Vec[UInt]) => VecInit(l.zip(r).map(x => x._1 | x._2))
      }: (Vec[UInt], Vec[UInt]) => Vec[UInt])
    }
  }

  class SrcStatus(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val psrc                  = UInt(params.rdPregIdxWidth.W)
    val srcType               = SrcType()
    val srcState              = SrcState()
    val dataSources           = DataSource()
    val srcLoadDependency     = Vec(LoadPipelineWidth, UInt(3.W))
    val srcWakeUpL1ExuOH      = OptionWrapper(params.hasIQWakeUp, ExuVec())
  }

  class StatusVecMemPart(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
    val sqIdx                 = new SqPtr
    val lqIdx                 = new LqPtr
  }

  class EntryDeqRespBundle(implicit p: Parameters, params: IssueBlockParams) extends Bundle {
    val robIdx                = new RobPtr
    val resp                  = RespType()
    val fuType                = FuType()
    val uopIdx                = OptionWrapper(params.isVecMemIQ, Output(UopIdx()))
  }

  object RespType {
    def apply() = UInt(2.W)

    def isBlocked(resp: UInt) = {
      resp === block
    }

    def succeed(resp: UInt) = {
      resp === success
    }

    val block = "b00".U
    val uncertain = "b01".U
    val success = "b11".U
  }

  class EntryBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val status                = new Status()
    val imm                   = OptionWrapper(params.needImm, UInt((params.deqImmTypesMaxLen).W))
    val payload               = new DynInst()
  }

  class CommonInBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val flush                 = Flipped(ValidIO(new Redirect))
    val enq                   = Flipped(ValidIO(new EntryBundle))
    //wakeup
    val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
    val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
    //cancel
    val og0Cancel             = Input(ExuOH(backendParams.numExu))
    val og1Cancel             = Input(ExuOH(backendParams.numExu))
    val ldCancel              = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    //deq sel
    val deqSel                = Input(Bool())
    val deqPortIdxWrite       = Input(UInt(1.W))
    val issueResp             = Flipped(ValidIO(new EntryDeqRespBundle))
    //trans sel
    val transSel              = Input(Bool())
    // vector mem only
    val fromLsq = OptionWrapper(params.isVecMemIQ, new Bundle {
      val sqDeqPtr            = Input(new SqPtr)
      val lqDeqPtr            = Input(new LqPtr)
    })
  }

  class CommonOutBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    //status
    val valid                 = Output(Bool())
    val canIssue              = Output(Bool())
    val fuType                = Output(FuType())
    val robIdx                = Output(new RobPtr)
    val uopIdx                = OptionWrapper(params.isVecMemIQ, Output(UopIdx()))
    //src
    val dataSource            = Vec(params.numRegSrc, Output(DataSource()))
    val srcLoadDependency     = Vec(params.numRegSrc, Output(Vec(LoadPipelineWidth, UInt(3.W))))
    val srcWakeUpL1ExuOH      = OptionWrapper(params.hasIQWakeUp, Vec(params.numRegSrc, Output(ExuVec())))
    //deq
    val isFirstIssue          = Output(Bool())
    val entry                 = ValidIO(new EntryBundle)
    val deqPortIdxRead        = Output(UInt(1.W))
    val issueTimerRead        = Output(UInt(2.W))
    //trans
    val enqReady              = Output(Bool())
    val transEntry            = ValidIO(new EntryBundle)
    // debug
    val entryInValid          = Output(Bool())
    val entryOutDeqValid      = Output(Bool())
    val entryOutTransValid    = Output(Bool())
    val perfLdCancel          = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, Bool())))
    val perfOg0Cancel         = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, Bool())))
    val perfWakeupByWB        = Output(Vec(params.numRegSrc, Bool()))
    val perfWakeupByIQ        = OptionWrapper(params.hasIQWakeUp, Output(Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))))
  }

  class CommonWireBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val validRegNext          = Bool()
    val flushed               = Bool()
    val clear                 = Bool()
    val canIssue              = Bool()
    val enqReady              = Bool()
    val deqSuccess            = Bool()
    val srcWakeup             = Vec(params.numRegSrc, Bool())
    val srcWakeupByWB         = Vec(params.numRegSrc, Bool())
    val srcLoadDependencyOut  = Vec(params.numRegSrc, Vec(LoadPipelineWidth, UInt(3.W)))
    val srcCancelVec          = Vec(params.numRegSrc, Bool())
    val srcLoadCancelVec      = Vec(params.numRegSrc, Bool())
  }

  def CommonWireConnect(common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet        = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    common.flushed            := status.robIdx.needFlush(commonIn.flush)
    common.deqSuccess         := commonIn.issueResp.valid && RespType.succeed(commonIn.issueResp.bits.resp) && !common.srcLoadCancelVec.asUInt.orR
    common.srcWakeup          := common.srcWakeupByWB.zip(hasIQWakeupGet.srcWakeupByIQ).map { case (x, y) => x || y.asUInt.orR }
    common.srcWakeupByWB      := commonIn.wakeUpFromWB.map(bundle => bundle.bits.wakeUp(status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType), bundle.valid)).transpose.map(x => VecInit(x.toSeq).asUInt.orR).toSeq
    common.canIssue           := validReg && status.canIssue
    common.enqReady           := !validReg || common.clear
    common.clear              := common.flushed || common.deqSuccess || commonIn.transSel
    common.srcCancelVec.zip(common.srcLoadCancelVec).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.foreach { case (((srcCancel, srcLoadCancel), wakeUpByIQVec), srcIdx) =>
      val ldTransCancel = if(params.hasIQWakeUp) Mux1H(wakeUpByIQVec, hasIQWakeupGet.wakeupLoadDependencyByIQVec.map(dep => LoadShouldCancel(Some(dep), commonIn.ldCancel))) else false.B
      srcLoadCancel := LoadShouldCancel(Some(status.srcStatus(srcIdx).srcLoadDependency), commonIn.ldCancel)
      srcCancel := srcLoadCancel || ldTransCancel
    }
    common.srcLoadDependencyOut.zip(hasIQWakeupGet.srcWakeupByIQ).zip(status.srcStatus.map(_.srcLoadDependency)).foreach {
      case ((loadDependencyOut, wakeUpByIQVec), loadDependency) =>
        if(params.hasIQWakeUp) {
          loadDependencyOut := Mux(wakeUpByIQVec.asUInt.orR, Mux1H(wakeUpByIQVec, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQBypassVec), loadDependency)
        } else {
          loadDependencyOut := loadDependency
        }

    }
    if(isEnq) {
      common.validRegNext     := Mux(commonIn.enq.valid && common.enqReady, true.B, Mux(common.clear, false.B, validReg))
    } else {
      common.validRegNext     := Mux(commonIn.enq.valid, true.B, Mux(common.clear, false.B, validReg))
    }
  }

  class CommonIQWakeupBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val srcWakeupByIQ                             = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQWithoutCancel                = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val srcWakeupByIQButCancel                    = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val regSrcWakeupL1ExuOH                       = Vec(params.numRegSrc, ExuVec())
    val srcWakeupL1ExuOHOut                       = Vec(params.numRegSrc, ExuVec())
    val wakeupLoadDependencyByIQVec               = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val shiftedWakeupLoadDependencyByIQVec        = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val shiftedWakeupLoadDependencyByIQBypassVec  = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
    val cancelVec                                 = Vec(params.numRegSrc, Bool())
    val canIssueBypass                            = Bool()
  }

  def CommonIQWakeupConnect(common: CommonWireBundle, hasIQWakeupGet: CommonIQWakeupBundle, validReg: Bool, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val wakeupVec: Seq[Seq[Bool]] = commonIn.wakeUpFromIQ.map((bundle: ValidIO[IssueQueueIQWakeUpBundle]) =>
      bundle.bits.wakeUpFromIQ(status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType))
    ).toSeq.transpose
    val cancelSel = params.wakeUpSourceExuIdx.zip(commonIn.wakeUpFromIQ).map { case (x, y) => commonIn.og0Cancel(x) && y.bits.is0Lat }

    hasIQWakeupGet.cancelVec                        := common.srcCancelVec
    hasIQWakeupGet.srcWakeupByIQ                    := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    hasIQWakeupGet.srcWakeupByIQButCancel           := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && cancel }))
    hasIQWakeupGet.srcWakeupByIQWithoutCancel       := wakeupVec.map(x => VecInit(x))
    hasIQWakeupGet.wakeupLoadDependencyByIQVec      := commonIn.wakeUpFromIQ.map(_.bits.loadDependency).toSeq
    hasIQWakeupGet.regSrcWakeupL1ExuOH.zip(status.srcStatus.map(_.srcWakeUpL1ExuOH.get)).foreach {
      case (exuOH, regExuOH) =>
        exuOH                                       := 0.U.asTypeOf(exuOH)
        params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := regExuOH(x))
    }
    hasIQWakeupGet.srcWakeupL1ExuOHOut.zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel).zipWithIndex.foreach {
      case ((exuOH: Vec[Bool], wakeUpByIQOH: Vec[Bool]), srcIdx) =>
        if (isEnq) {
          ExuOHGen(exuOH, wakeUpByIQOH, status.srcStatus(srcIdx).srcWakeUpL1ExuOH.get)
        } else {
          ExuOHGen(exuOH, wakeUpByIQOH, hasIQWakeupGet.regSrcWakeupL1ExuOH(srcIdx))
        }
    }
    hasIQWakeupGet.canIssueBypass := validReg && !status.issued && !status.blocked &&
      VecInit(status.srcStatus.take(params.numFastWakeupSrc).map(_.srcState).zip(hasIQWakeupGet.srcWakeupByIQWithoutCancel.take(params.numFastWakeupSrc)).map { case (state, wakeupVec) =>
        wakeupVec.asUInt.orR | state
      }).asUInt.andR &&
      status.srcStatus.drop(params.numFastWakeupSrc).map(_.srcState.orR).fold(true.B)(_ && _)
  }


  def ShiftLoadDependency(hasIQWakeupGet: CommonIQWakeupBundle)(implicit p: Parameters, params: IssueBlockParams) = {
    hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec
      .zip(hasIQWakeupGet.wakeupLoadDependencyByIQVec)
      .zip(params.wakeUpInExuSources.map(_.name)).foreach {
      case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
        case ((dep, originalDep), deqPortIdx) =>
          if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
            dep := 2.U
          else
            dep := originalDep << 1
      }
    }
    hasIQWakeupGet.shiftedWakeupLoadDependencyByIQBypassVec
      .zip(hasIQWakeupGet.wakeupLoadDependencyByIQVec)
      .zip(params.wakeUpInExuSources.map(_.name)).foreach {
      case ((deps, originalDeps), name) => deps.zip(originalDeps).zipWithIndex.foreach {
        case ((dep, originalDep), deqPortIdx) =>
          if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
            dep := 1.U
          else
            dep := originalDep
      }
    }
  }

  def wakeUpByVf(OH: Vec[Bool])(implicit p: Parameters): Bool = {
    val allExuParams = p(XSCoreParamsKey).backendParams.allExuParams
    OH.zip(allExuParams).map{case (oh,e) =>
      if (e.isVfExeUnit) oh else false.B
    }.reduce(_ || _)
  }

  def EntryRegCommonConnect(common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryUpdate: EntryBundle, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    val cancelByLd                                     = common.srcCancelVec.asUInt.orR
    val cancelWhenWakeup                               = VecInit(hasIQWakeupGet.srcWakeupByIQButCancel.map(_.asUInt.orR)).asUInt.orR
    val respIssueFail                                  = commonIn.issueResp.valid && RespType.isBlocked(commonIn.issueResp.bits.resp)
    val srcWakeupExuOH                                 = if(isEnq) status.srcStatus.map(_.srcWakeUpL1ExuOH.getOrElse(0.U.asTypeOf(ExuVec()))) else hasIQWakeupGet.regSrcWakeupL1ExuOH
    entryUpdate.status.robIdx                         := status.robIdx
    entryUpdate.status.fuType                         := IQFuType.readFuType(status.fuType, params.getFuCfgs.map(_.fuType))
    entryUpdate.status.srcStatus.zip(status.srcStatus).zipWithIndex.foreach { case ((srcStatusNext, srcStatus), srcIdx) =>
      val cancel = common.srcCancelVec(srcIdx)
      val wakeupByIQ = hasIQWakeupGet.srcWakeupByIQ(srcIdx).asUInt.orR
      val wakeupByIQOH = hasIQWakeupGet.srcWakeupByIQ(srcIdx)
      val wakeup = common.srcWakeup(srcIdx)
      srcStatusNext.psrc                              := srcStatus.psrc
      srcStatusNext.srcType                           := srcStatus.srcType
      srcStatusNext.srcState                          := Mux(cancel, false.B, wakeup | srcStatus.srcState)
      srcStatusNext.dataSources.value                 := (if (params.inVfSchd && params.readVfRf && params.hasIQWakeUp) {
                                                            // Vf / Mem -> Vf
                                                            val isWakeupByMemIQ = wakeupByIQOH.zip(commonIn.wakeUpFromIQ).filter(_._2.bits.params.isMemExeUnit).map(_._1).fold(false.B)(_ || _)
                                                            MuxCase(srcStatus.dataSources.value, Seq(
                                                              (wakeupByIQ && isWakeupByMemIQ)    -> DataSource.bypass2,
                                                              (wakeupByIQ && !isWakeupByMemIQ)   -> DataSource.bypass,
                                                              srcStatus.dataSources.readBypass   -> DataSource.bypass2,
                                                              srcStatus.dataSources.readBypass2  -> DataSource.reg,
                                                            ))
                                                          }
                                                          else if (params.inMemSchd && params.readVfRf && params.hasIQWakeUp) {
                                                            // Vf / Int -> Mem
                                                            MuxCase(srcStatus.dataSources.value, Seq(
                                                              wakeupByIQ                                                               -> DataSource.bypass,
                                                              (srcStatus.dataSources.readBypass && wakeUpByVf(srcStatus.srcWakeUpL1ExuOH.get)) -> DataSource.bypass2,
                                                              (srcStatus.dataSources.readBypass && !wakeUpByVf(srcStatus.srcWakeUpL1ExuOH.get)) -> DataSource.reg,
                                                              srcStatus.dataSources.readBypass2                                        -> DataSource.reg,
                                                            ))
                                                          }
                                                          else {
                                                            MuxCase(srcStatus.dataSources.value, Seq(
                                                              wakeupByIQ                         -> DataSource.bypass,
                                                              srcStatus.dataSources.readBypass   -> DataSource.reg,
                                                            ))
                                                          })
      if(params.hasIQWakeUp) {
        ExuOHGen(srcStatusNext.srcWakeUpL1ExuOH.get, wakeupByIQOH, srcWakeupExuOH(srcIdx))
        srcStatusNext.srcLoadDependency               :=
          Mux(wakeup,
            Mux1H(wakeupByIQOH, hasIQWakeupGet.shiftedWakeupLoadDependencyByIQVec),
            Mux(validReg && srcStatus.srcLoadDependency.asUInt.orR, VecInit(srcStatus.srcLoadDependency.map(i => i(i.getWidth - 2, 0) << 1)), srcStatus.srcLoadDependency))
      } else {
        srcStatusNext.srcLoadDependency               := Mux(validReg && srcStatus.srcLoadDependency.asUInt.orR, VecInit(srcStatus.srcLoadDependency.map(i => i(i.getWidth - 2, 0) << 1)), srcStatus.srcLoadDependency)
      }
    }
    entryUpdate.status.blocked                        := false.B
    entryUpdate.status.issued                         := MuxCase(status.issued, Seq(
      (cancelByLd || cancelWhenWakeup || respIssueFail) -> false.B,
      commonIn.deqSel                                   -> true.B,
      !status.srcReady                                  -> false.B,
    ))
    entryUpdate.status.firstIssue                     := commonIn.deqSel || status.firstIssue
    entryUpdate.status.issueTimer                     := Mux(commonIn.deqSel, 0.U, Mux(status.issued, status.issueTimer + 1.U, "b11".U))
    entryUpdate.status.deqPortIdx                     := Mux(commonIn.deqSel, commonIn.deqPortIdxWrite, Mux(status.issued, status.deqPortIdx, 0.U))
    entryUpdate.imm.foreach(_                         := entryReg.imm.get)
    entryUpdate.payload                               := entryReg.payload
    if (params.isVecMemIQ) {
      entryUpdate.status.vecMem.get := entryReg.status.vecMem.get
    }
  }

  def CommonOutConnect(commonOut: CommonOutBundle, common: CommonWireBundle, hasIQWakeup: Option[CommonIQWakeupBundle], validReg: Bool, entryUpdate: EntryBundle, entryReg: EntryBundle, status: Status, commonIn: CommonInBundle, isEnq: Boolean, isComp: Boolean)(implicit p: Parameters, params: IssueBlockParams) = {
    val hasIQWakeupGet                                 = hasIQWakeup.getOrElse(0.U.asTypeOf(new CommonIQWakeupBundle))
    val srcWakeupExuOH                                 = if(isEnq) status.srcStatus.map(_.srcWakeUpL1ExuOH.getOrElse(0.U.asTypeOf(ExuVec()))) else hasIQWakeupGet.regSrcWakeupL1ExuOH
    commonOut.valid                                   := validReg
    commonOut.canIssue                                := (if (isComp) (common.canIssue || hasIQWakeupGet.canIssueBypass) && !common.flushed
                                                          else common.canIssue && !common.flushed)
    commonOut.fuType                                  := IQFuType.readFuType(status.fuType, params.getFuCfgs.map(_.fuType)).asUInt
    commonOut.robIdx                                  := status.robIdx
    commonOut.dataSource.zipWithIndex.foreach{ case (dataSourceOut, srcIdx) =>
      val wakeupByIQWithoutCancel = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx).asUInt.orR
      val wakeupByIQWithoutCancelOH = hasIQWakeupGet.srcWakeupByIQWithoutCancel(srcIdx)
      val isWakeupByMemIQ = wakeupByIQWithoutCancelOH.zip(commonIn.wakeUpFromIQ).filter(_._2.bits.params.isMemExeUnit).map(_._1).fold(false.B)(_ || _)
      dataSourceOut.value                             := (if(srcIdx < params.numFastWakeupSrc) Mux(wakeupByIQWithoutCancel,
                                                              if (params.inVfSchd && params.readVfRf && params.hasWakeupFromMem) Mux(isWakeupByMemIQ, DataSource.bypass, DataSource.forward) else DataSource.forward, 
                                                              status.srcStatus(srcIdx).dataSources.value)
                                                         else status.srcStatus(srcIdx).dataSources.value)
    }
    commonOut.isFirstIssue                            := !status.firstIssue
    commonOut.entry.valid                             := validReg
    commonOut.entry.bits                              := entryReg
    if(isEnq) {
      commonOut.entry.bits.status                     := status
    }
    commonOut.issueTimerRead                          := status.issueTimer
    commonOut.deqPortIdxRead                          := status.deqPortIdx
    if(params.hasIQWakeUp) {
      val wakeupSrcLoadDependency                      = hasIQWakeupGet.srcWakeupByIQWithoutCancel.map(x => Mux1H(x, hasIQWakeupGet.wakeupLoadDependencyByIQVec))
      commonOut.srcWakeUpL1ExuOH.get.zipWithIndex.foreach { case (srcWakeupL1ExuOHOut, srcIdx) =>
        srcWakeupL1ExuOHOut                           := (if (isComp && (srcIdx < params.numFastWakeupSrc)) Mux(hasIQWakeupGet.canIssueBypass && !common.canIssue, hasIQWakeupGet.srcWakeupL1ExuOHOut(srcIdx), srcWakeupExuOH(srcIdx))
                                                          else srcWakeupExuOH(srcIdx))
      }
      commonOut.srcLoadDependency.zipWithIndex.foreach { case (srcLoadDependencyOut, srcIdx) =>
        srcLoadDependencyOut                          := (if (isComp && (srcIdx < params.numFastWakeupSrc)) Mux(hasIQWakeupGet.canIssueBypass && !common.canIssue,
                                                                      VecInit(status.srcStatus(srcIdx).srcLoadDependency.zip(wakeupSrcLoadDependency(srcIdx)).map(x => x._1 | x._2)),
                                                                      status.srcStatus(srcIdx).srcLoadDependency)
                                                          else status.srcStatus(srcIdx).srcLoadDependency)
      }
    } else {
      commonOut.srcLoadDependency.zipWithIndex.foreach { case (srcLoadDependencyOut, srcIdx) =>
        srcLoadDependencyOut                          := status.srcStatus(srcIdx).srcLoadDependency
      }
    }
    commonOut.entry.bits.status.srcStatus.map(_.srcLoadDependency).zipWithIndex.foreach { case (srcLoadDependencyOut, srcIdx) =>
      srcLoadDependencyOut                            := (if (isComp && (srcIdx < params.numFastWakeupSrc)) Mux(hasIQWakeupGet.canIssueBypass && !common.canIssue,
                                                                      common.srcLoadDependencyOut(srcIdx),
                                                                      status.srcStatus(srcIdx).srcLoadDependency)
                                                          else status.srcStatus(srcIdx).srcLoadDependency)
    }
    commonOut.enqReady                                := common.enqReady
    commonOut.transEntry.valid                        := validReg && !common.flushed && !common.deqSuccess
    commonOut.transEntry.bits                         := entryUpdate
    // debug
    commonOut.entryInValid                            := commonIn.enq.valid
    commonOut.entryOutDeqValid                        := validReg && (common.flushed || common.deqSuccess)
    commonOut.entryOutTransValid                      := validReg && commonIn.transSel && !(common.flushed || common.deqSuccess)
    commonOut.perfWakeupByWB                          := common.srcWakeupByWB.zip(status.srcStatus).map{ case (w, s) => w && SrcState.isBusy(s.srcState) && validReg }
    if (params.hasIQWakeUp) {
      commonOut.perfLdCancel.get                      := hasIQWakeupGet.cancelVec.map(_ && validReg)
      commonOut.perfOg0Cancel.get                     := hasIQWakeupGet.srcWakeupByIQButCancel.map(_.asUInt.orR && validReg)
      commonOut.perfWakeupByIQ.get                    := hasIQWakeupGet.srcWakeupByIQ.map(x => VecInit(x.map(_ && validReg)))
    }
    // vecMem
    if (params.isVecMemIQ) {
      commonOut.uopIdx.get                            := entryReg.payload.uopIdx
    }
  }

  def EntryVecMemConnect(commonIn: CommonInBundle, common: CommonWireBundle, validReg: Bool, entryReg: EntryBundle, entryRegNext: EntryBundle, entryUpdate: EntryBundle)(implicit p: Parameters, params: IssueBlockParams) = {
    val fromLsq                                        = commonIn.fromLsq.get
    val vecMemStatus                                   = entryReg.status.vecMem.get
    val vecMemStatusUpdate                             = entryUpdate.status.vecMem.get
    vecMemStatusUpdate                                := vecMemStatus

    entryUpdate.status.blocked                        := false.B
  }

  def ExuOHGen(exuOH: Vec[Bool], wakeupByIQOH: Vec[Bool], regSrcExuOH: Vec[Bool])(implicit p: Parameters, params: IssueBlockParams) = {
    val origExuOH = 0.U.asTypeOf(exuOH)
    when(wakeupByIQOH.asUInt.orR) {
      origExuOH := Mux1H(wakeupByIQOH, params.wakeUpSourceExuIdx.map(x => MathUtils.IntToOH(x).U(p(XSCoreParamsKey).backendParams.numExu.W)).toSeq).asBools
    }.otherwise {
      origExuOH := regSrcExuOH
    }
    exuOH := 0.U.asTypeOf(exuOH)
    params.wakeUpSourceExuIdx.foreach(x => exuOH(x) := origExuOH(x))
  }

  object IQFuType {
    def num = FuType.num

    def apply() = Vec(num, Bool())

    def readFuType(fuType: Vec[Bool], fus: Seq[FuType.OHType]): Vec[Bool] = {
      val res = 0.U.asTypeOf(fuType)
      fus.foreach(x => res(x.id) := fuType(x.id))
      res
    }
  }

  class EnqDelayInBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    //wakeup
    val wakeUpFromWB: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = Flipped(params.genWBWakeUpSinkValidBundle)
    val wakeUpFromIQ: MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = Flipped(params.genIQWakeUpSinkValidBundle)
    //cancel
    val og0Cancel             = Input(ExuOH(backendParams.numExu))
    val ldCancel              = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
  }

  class EnqDelayOutBundle(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
    val srcWakeUpByWB: Vec[UInt]                            = Vec(params.numRegSrc, SrcState())
    val srcWakeUpByIQ: Vec[UInt]                            = Vec(params.numRegSrc, SrcState())
    val srcWakeUpByIQVec: Vec[Vec[Bool]]                    = Vec(params.numRegSrc, Vec(params.numWakeupFromIQ, Bool()))
    val shiftedWakeupLoadDependencyByIQVec: Vec[Vec[UInt]]  = Vec(params.numWakeupFromIQ, Vec(LoadPipelineWidth, UInt(3.W)))
  }

  def EnqDelayWakeupConnect(enqDelayIn: EnqDelayInBundle, enqDelayOut: EnqDelayOutBundle, status: Status, delay: Int)(implicit p: Parameters, params: IssueBlockParams) = {
    enqDelayOut.srcWakeUpByWB.zipWithIndex.foreach { case (wakeup, i) =>
      wakeup := enqDelayIn.wakeUpFromWB.map(x => x.bits.wakeUp(Seq((status.srcStatus(i).psrc, status.srcStatus(i).srcType)), x.valid).head
      ).reduce(_ || _)
    }

    if (params.hasIQWakeUp) {
      val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = enqDelayIn.wakeUpFromIQ.map( x =>
        x.bits.wakeUpFromIQ(status.srcStatus.map(_.psrc) zip status.srcStatus.map(_.srcType))
      ).toIndexedSeq.transpose
      val cancelSel = params.wakeUpSourceExuIdx.zip(enqDelayIn.wakeUpFromIQ).map{ case (x, y) => enqDelayIn.og0Cancel(x) && y.bits.is0Lat}
      enqDelayOut.srcWakeUpByIQVec := wakeupVec.map(x => VecInit(x.zip(cancelSel).map { case (wakeup, cancel) => wakeup && !cancel }))
    } else {
      enqDelayOut.srcWakeUpByIQVec := 0.U.asTypeOf(enqDelayOut.srcWakeUpByIQVec)
    }

    if (params.hasIQWakeUp) {
      enqDelayOut.srcWakeUpByIQ.zipWithIndex.foreach { case (wakeup, i) =>
        val ldTransCancel = Mux1H(enqDelayOut.srcWakeUpByIQVec(i), enqDelayIn.wakeUpFromIQ.map(_.bits.loadDependency).map(dp => LoadShouldCancel(Some(dp), enqDelayIn.ldCancel)).toSeq)
        wakeup := enqDelayOut.srcWakeUpByIQVec(i).asUInt.orR && !ldTransCancel
      }
    } else {
      enqDelayOut.srcWakeUpByIQ := 0.U.asTypeOf(enqDelayOut.srcWakeUpByIQ)
    }

    enqDelayOut.shiftedWakeupLoadDependencyByIQVec.zip(enqDelayIn.wakeUpFromIQ.map(_.bits.loadDependency))
      .zip(params.wakeUpInExuSources.map(_.name)).foreach { case ((dps, ldps), name) =>
      dps.zip(ldps).zipWithIndex.foreach { case ((dp, ldp), deqPortIdx) =>
        if (params.backendParam.getLdExuIdx(params.backendParam.allExuParams.find(_.name == name).get) == deqPortIdx)
          dp := 1.U << delay
        else
          dp := ldp << delay
      }
    }
  }
}
