package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.ExuInput
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.vagq._

class VAGQDownstreamAdapterIO(
  loadParams: Seq[ExeUnitParams],
  storeParams: Seq[ExeUnitParams],
  stdParams: Seq[ExeUnitParams]
)(implicit p: Parameters) extends XSBundle {
  val vagqLsuReq  = Flipped(Vec(VAGQConstants.ActiveIssueWidth, Decoupled(new VAGQLsuReq)))
  val vagqLduResp = Vec(VAGQConstants.LduRespWidth, Valid(new VAGQResp))
  val vagqStaResp = Vec(VAGQConstants.StaRespWidth, Valid(new VAGQResp))

  val issueLda = Flipped(MixedVec(loadParams.map(param => DecoupledIO(new ExuInput(param)))))
  val lduReq = MixedVec(loadParams.map(param => DecoupledIO(new ExuInput(param))))
  val lduReqMeta = Output(Vec(loadParams.length, new VAGQMemPipelineMeta))
  val lduResp = Flipped(Vec(VAGQConstants.LduRespWidth, Valid(new VAGQResp)))

  val issueSta = Flipped(MixedVec(storeParams.map(param => DecoupledIO(new ExuInput(param)))))
  val staReq = MixedVec(storeParams.map(param => DecoupledIO(new ExuInput(param))))
  val staReqMeta = Output(Vec(storeParams.length, new VAGQMemPipelineMeta))
  val staResp = Flipped(Vec(VAGQConstants.StaRespWidth, Valid(new VAGQResp)))

  val stdDataBusy = Input(Vec(stdParams.length, Bool()))
  val vagqStdData = Output(Vec(stdParams.length, Valid(new StoreQueueDataWrite)))

  val vagqLsqEmptyReq  = Flipped(Decoupled(new VAGQLsqEmptyReq))
  val vagqLsqEmptyResp = Valid(new VAGQLsqEmptyResp)

  val lsqEmptyReq  = Decoupled(new VAGQLsqEmptyReq)
  val lsqEmptyResp = Flipped(Valid(new VAGQLsqEmptyResp))
}

class VAGQDownstreamAdapter(
  loadParams: Seq[ExeUnitParams],
  storeParams: Seq[ExeUnitParams],
  stdParams: Seq[ExeUnitParams]
)(implicit p: Parameters)
  extends XSModule {
  require(loadParams.length >= VAGQConstants.LduRespWidth)
  require(storeParams.length >= VAGQConstants.StaRespWidth)
  require(storeParams.length >= VAGQConstants.ActiveIssueWidth)
  require(stdParams.length >= VAGQConstants.ActiveIssueWidth)

  val io = IO(new VAGQDownstreamAdapterIO(loadParams, storeParams, stdParams))

  private def vagqLoadFuOpType(alignedType: UInt): UInt = {
    MuxLookup(alignedType(1, 0), LSUOpType.vle8.asUInt)(Seq(
      0.U -> LSUOpType.vle8.asUInt,
      1.U -> LSUOpType.vle16.asUInt,
      2.U -> LSUOpType.vle32.asUInt,
      3.U -> LSUOpType.vle64.asUInt,
    ))
  }

  private def vagqStoreFuOpType(alignedType: UInt): UInt = {
    MuxLookup(alignedType(1, 0), LSUOpType.vsse8.asUInt)(Seq(
      0.U -> LSUOpType.vsse8.asUInt,
      1.U -> LSUOpType.vsse16.asUInt,
      2.U -> LSUOpType.vsse32.asUInt,
      3.U -> LSUOpType.vsse64.asUInt,
    ))
  }

  io.vagqLduResp.zip(io.lduResp).foreach { case (toVagq, fromLdu) =>
    toVagq := fromLdu
  }

  io.vagqStaResp.zip(io.staResp).foreach { case (toVagq, fromSta) =>
    toVagq := fromSta
  }

  val vagqLoadReady = WireInit(VecInit(Seq.fill(VAGQConstants.ActiveIssueWidth)(false.B)))
  val vagqStoreReady = WireInit(VecInit(Seq.fill(VAGQConstants.ActiveIssueWidth)(false.B)))

  val activeLoadValid = VecInit((0 until VAGQConstants.ActiveIssueWidth).map { i =>
    io.vagqLsuReq(i).valid && io.vagqLsuReq(i).bits.isLoad
  })

  for (i <- loadParams.indices) {
    if (i < VAGQConstants.ActiveIssueWidth) {
      val activeReq = io.vagqLsuReq(i)
      val selectActive = activeLoadValid(i) && !io.issueLda(i).valid

      val activeLdin = Wire(chiselTypeOf(io.lduReq(i).bits))
      activeLdin := 0.U.asTypeOf(activeLdin)
      activeLdin.fuType   := FuType.vldu.U
      activeLdin.fuOpType := vagqLoadFuOpType(activeReq.bits.alignedType)
      activeLdin.src(0)   := activeReq.bits.vaddr
      activeLdin.imm      := 0.U
      activeLdin.robIdx   := activeReq.bits.robIdx
      activeLdin.pdest    := activeReq.bits.pdest
      activeLdin.vecWen.foreach(_ := true.B)
      activeLdin.lqIdx.foreach(_ := activeReq.bits.lqIdx)
      activeLdin.sqIdx.foreach(_ := activeReq.bits.sqIdx)

      val activeMeta = Wire(chiselTypeOf(io.lduReqMeta(i)))
      activeMeta := 0.U.asTypeOf(activeMeta)
      activeMeta.valid      := true.B
      activeMeta.entryIdx   := activeReq.bits.entryIdx
      activeMeta.robIdx     := activeReq.bits.robIdx
      activeMeta.isLoad     := true.B
      activeMeta.isStore    := false.B
      activeMeta.byteOffset := activeReq.bits.byteOffset
      activeMeta.mask       := activeReq.bits.mask

      io.lduReq(i).valid := io.issueLda(i).valid || selectActive
      io.lduReq(i).bits  := Mux(selectActive, activeLdin, io.issueLda(i).bits)
      io.issueLda(i).ready := !selectActive && io.lduReq(i).ready
      vagqLoadReady(i) := selectActive && io.lduReq(i).ready
      io.lduReqMeta(i) := Mux(selectActive, activeMeta, 0.U.asTypeOf(io.lduReqMeta(i)))
    } else {
      io.lduReq(i) <> io.issueLda(i)
      io.lduReqMeta(i) := 0.U.asTypeOf(io.lduReqMeta(i))
    }
  }

  io.vagqStdData.foreach { data =>
    data.valid := false.B
    data.bits  := 0.U.asTypeOf(data.bits)
  }

  for (i <- storeParams.indices) {
    if (i < VAGQConstants.ActiveIssueWidth) {
      val activeReq = io.vagqLsuReq(i)
      val activeStoreValid = activeReq.valid && activeReq.bits.isStore
      val selectActive = activeStoreValid && !io.issueSta(i).valid

      val activeStin = Wire(chiselTypeOf(io.staReq(i).bits))
      activeStin := 0.U.asTypeOf(activeStin)
      activeStin.fuType   := FuType.vstu.U
      activeStin.fuOpType := vagqStoreFuOpType(activeReq.bits.alignedType)
      activeStin.src(0)   := activeReq.bits.vaddr
      activeStin.imm      := 0.U
      activeStin.robIdx   := activeReq.bits.robIdx
      activeStin.lqIdx.foreach(_ := activeReq.bits.lqIdx)
      activeStin.sqIdx.foreach(_ := activeReq.bits.sqIdx)

      val activeMeta = Wire(chiselTypeOf(io.staReqMeta(i)))
      activeMeta := 0.U.asTypeOf(activeMeta)
      activeMeta.valid      := true.B
      activeMeta.entryIdx   := activeReq.bits.entryIdx
      activeMeta.robIdx     := activeReq.bits.robIdx
      activeMeta.isLoad     := false.B
      activeMeta.isStore    := activeReq.bits.isStore
      activeMeta.byteOffset := activeReq.bits.byteOffset
      activeMeta.mask       := activeReq.bits.mask

      val activeData = Wire(chiselTypeOf(io.vagqStdData(i).bits))
      activeData := 0.U.asTypeOf(activeData)
      activeData.fuType   := FuType.vstu.U
      activeData.fuOpType := vagqStoreFuOpType(activeReq.bits.alignedType)
      activeData.data     := activeReq.bits.data
      activeData.sqIdx    := activeReq.bits.sqIdx

      io.staReq(i).valid := Mux(selectActive, !io.stdDataBusy(i), io.issueSta(i).valid)
      io.staReq(i).bits  := Mux(selectActive, activeStin, io.issueSta(i).bits)
      io.issueSta(i).ready := !selectActive && io.staReq(i).ready
      vagqStoreReady(i) := selectActive && !io.stdDataBusy(i) && io.staReq(i).ready
      io.staReqMeta(i) := Mux(selectActive, activeMeta, 0.U.asTypeOf(io.staReqMeta(i)))

      io.vagqStdData(i).valid := vagqStoreReady(i)
      io.vagqStdData(i).bits  := activeData
    } else {
      io.staReq(i) <> io.issueSta(i)
      io.staReqMeta(i) := 0.U.asTypeOf(io.staReqMeta(i))
    }
  }

  for (i <- 0 until VAGQConstants.ActiveIssueWidth) {
    io.vagqLsuReq(i).ready := vagqLoadReady(i) || vagqStoreReady(i)
  }

  io.lsqEmptyReq.valid := io.vagqLsqEmptyReq.valid
  io.lsqEmptyReq.bits  := io.vagqLsqEmptyReq.bits
  io.vagqLsqEmptyReq.ready := io.lsqEmptyReq.ready
  io.vagqLsqEmptyResp := io.lsqEmptyResp
}
