package xiangshan.backend.vector.vagq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr

class MergeCtrl(numEntries: Int)(implicit p: Parameters) extends VAGQModule {
  val io = IO(new MergeCtrlIO(numEntries))

  private val respVec = io.lduResp.toSeq ++ io.staResp.toSeq ++ Seq(io.lsqEmptyResp)
  private val respExceptionHit = Wire(Vec(numEntries, Bool()))
  for (i <- 0 until numEntries) {
    respExceptionHit(i) := respVec.map { resp =>
      resp.valid &&
        resp.bits.exception &&
        resp.bits.entryIdx === i.U(vagqEntryIdxWidth.W) &&
        respMatchesEntry(resp.bits, io.entry, numEntries)
    }.reduce(_ || _)
  }

  for (lane <- 0 until VAGQConstants.MergeRespWidth) {
    val resp = respVec(lane)
    val respAccepted = resp.valid && respMatchesEntry(resp.bits, io.entry, numEntries)
    val respMask = resp.bits.mask

    io.reqUpdate(lane).valid                := respAccepted
    io.reqUpdate(lane).bits                 := 0.U.asTypeOf(io.reqUpdate(lane).bits)
    io.reqUpdate(lane).bits.entryIdx        := resp.bits.entryIdx
    io.reqUpdate(lane).bits.setReqAck       := Mux(resp.bits.isNACK && !resp.bits.exception, 0.U, respMask)
    io.reqUpdate(lane).bits.clearReqSent    := Mux(resp.bits.isNACK && !resp.bits.exception, respMask, 0.U)
    io.reqUpdate(lane).bits.exception       := resp.bits.exception
    io.reqUpdate(lane).bits.exceptionNumber := resp.bits.exceptionNumber
    io.reqUpdate(lane).bits.faultElemIdx    := resp.bits.byteOffset
  }

  private val mergeCandidates = VecInit(io.entry.map { x =>
    entryAlive(x.entry, io.redirect) && x.entry.state === VAGQEntryState.merge
  })
  private val wbCandidates = VecInit(io.entry.map { x =>
    entryAlive(x.entry, io.redirect) && x.entry.state === VAGQEntryState.wb
  })
  private val excpCandidates = VecInit(io.entry.map { x =>
    val olderReqMask = prefixMask(x.entry.faultElemIdx)
    val olderReqInFlight = (x.entry.reqSent & ~x.entry.reqAck & olderReqMask).orR
    entryAlive(x.entry, io.redirect) && x.entry.state === VAGQEntryState.excp && !olderReqInFlight
  })
  private val splitDoneCandidates = VecInit(io.entry.zipWithIndex.map { case (x, i) =>
    entryAlive(x.entry, io.redirect) && x.entry.state === VAGQEntryState.split && x.entry.reqAck.andR && !respExceptionHit(i)
  })

  private val hasMerge     = mergeCandidates.asUInt.orR
  private val hasWb        = wbCandidates.asUInt.orR
  private val hasExcp      = excpCandidates.asUInt.orR
  private val hasSplitDone = splitDoneCandidates.asUInt.orR

  private val mergeSel     = PriorityEncoder(mergeCandidates)
  private val wbSel        = PriorityEncoder(wbCandidates)
  private val excpSel      = PriorityEncoder(excpCandidates)
  private val splitDoneSel = PriorityEncoder(splitDoneCandidates)
  private val mergeEntry     = io.entry(mergeSel)
  private val wbEntry        = io.entry(wbSel)
  private val excpEntry      = io.entry(excpSel)
  private val splitDoneEntry = io.entry(splitDoneSel)

  private val splitDoneNonActiveMask = ~splitDoneEntry.entry.elemActiveMask
  private val skipMerge = !splitDoneNonActiveMask.orR
  private val splitDoneStateNext = Mux(
    splitDoneEntry.entry.isStore | skipMerge,
    VAGQEntryState.wb,
    VAGQEntryState.merge
  )

  io.stateUpdate.valid := false.B
  io.stateUpdate.bits  := 0.U.asTypeOf(io.stateUpdate.bits)

  private val mergeReadValid    = RegInit(false.B)
  private val mergeReadEntryIdx = RegInit(0.U(vagqEntryIdxWidth.W))
  private val mergeReadRobIdx   = RegInit(0.U.asTypeOf(new RobPtr))
  private val mergeReadEntry    = mergeEntryAt(io.entry, mergeReadEntryIdx, numEntries)
  private val mergeReadAlive    = mergeReadValid &&
                                  mergeReadEntry.entry.state === VAGQEntryState.merge &&
                                  mergeReadEntry.entry.robIdx === mergeReadRobIdx &&
                                  entryAlive(mergeReadEntry.entry, io.redirect)

  private val mergeRespValid    = RegInit(false.B)
  private val mergeRespEntryIdx = RegInit(0.U(vagqEntryIdxWidth.W))
  private val mergeRespRobIdx   = RegInit(0.U.asTypeOf(new RobPtr))
  private val mergeRespData     = Reg(UInt(VLEN.W))
  private val mergeRespEntry    = mergeEntryAt(io.entry, mergeRespEntryIdx, numEntries)
  private val mergeRespAlive    = mergeRespValid &&
                                  mergeRespEntry.entry.state === VAGQEntryState.merge &&
                                  mergeRespEntry.entry.robIdx === mergeRespRobIdx &&
                                  entryAlive(mergeRespEntry.entry, io.redirect)

  io.vrfReadReq.valid         := hasMerge && !mergeReadValid && !mergeRespValid
  io.vrfReadReq.bits.entryIdx := mergeEntry.entryIdx
  io.vrfReadReq.bits.robIdx   := mergeEntry.entry.robIdx
  io.vrfReadReq.bits.psrc     := mergeEntry.entry.psrc2

  private val vrfReadRespEntry = mergeEntryAt(io.entry, io.vrfReadResp.bits.entryIdx, numEntries)
  private val vrfReadRespAlive = mergeReadValid &&
                            io.vrfReadResp.valid &&
                            io.vrfReadResp.bits.entryIdx === mergeReadEntryIdx &&
                            io.vrfReadResp.bits.robIdx === mergeReadRobIdx &&
                            vrfReadRespEntry.entry.valid &&
                            vrfReadRespEntry.entry.state === VAGQEntryState.merge &&
                            vrfReadRespEntry.entry.robIdx === io.vrfReadResp.bits.robIdx &&
                            entryAlive(vrfReadRespEntry.entry, io.redirect)

  private val nonActiveMask = ~mergeRespEntry.entry.elemActiveMask
  private val agnosticMask  = mergeRespEntry.entry.elemAgnosticMask
  private val mergeWriteData = mergeBytes(mergeRespData, Fill(VLEN, true.B), agnosticMask & nonActiveMask)

  io.vrfWriteReq.valid := mergeRespValid && mergeRespAlive && !hasSplitDone
  io.vrfWriteReq.bits  := 0.U.asTypeOf(io.vrfWriteReq.bits)
  io.vrfWriteReq.bits.entryIdx := mergeRespEntryIdx
  io.vrfWriteReq.bits.pdest    := mergeRespEntry.entry.pdest
  io.vrfWriteReq.bits.data     := mergeWriteData
  io.vrfWriteReq.bits.mask     := nonActiveMask

  private val vrfWriteValid = io.vrfWriteReq.valid

  io.robWriteback.valid                := (hasWb || hasExcp) && !hasSplitDone && !vrfWriteValid
  io.robWriteback.bits                 := 0.U.asTypeOf(io.robWriteback.bits)
  io.robWriteback.bits.meta            := Mux(hasExcp, excpEntry.entry.meta, wbEntry.entry.meta)
  io.robWriteback.bits.entryIdx        := Mux(hasExcp, excpEntry.entryIdx, wbEntry.entryIdx)
  io.robWriteback.bits.robIdx          := Mux(hasExcp, excpEntry.entry.robIdx, wbEntry.entry.robIdx)
  io.robWriteback.bits.exception       := hasExcp
  io.robWriteback.bits.exceptionNumber := Mux(hasExcp, excpEntry.entry.exceptionNumber, 0.U)
  io.robWriteback.bits.faultElemIdx    := Mux(hasExcp, excpEntry.entry.faultElemIdx, 0.U)
  io.robWriteback.bits.faultVstart     := Mux(hasExcp, faultVstart(excpEntry.entry), 0.U)
  io.robWriteback.bits.uopType         := Mux(hasExcp, excpEntry.entry.uopType, wbEntry.entry.uopType)
  io.robWriteback.bits.uopIdx          := Mux(hasExcp, excpEntry.entry.uopIdx, wbEntry.entry.uopIdx)
  io.robWriteback.bits.deew            := Mux(hasExcp, excpEntry.entry.deew, wbEntry.entry.deew)
  io.robWriteback.bits.nf              := Mux(hasExcp, excpEntry.entry.nf, wbEntry.entry.nf)

  when(mergeReadValid) {
    when(!mergeReadAlive || vrfReadRespAlive) {
      mergeReadValid := false.B
    }
  }.elsewhen(io.vrfReadReq.valid) {
    mergeReadValid    := true.B
    mergeReadEntryIdx := io.vrfReadReq.bits.entryIdx
    mergeReadRobIdx   := io.vrfReadReq.bits.robIdx
  }

  when(mergeRespValid) {
    when(!mergeRespValid || vrfWriteValid) {
      mergeRespValid := false.B
    }
  }.elsewhen(vrfReadRespAlive) {
    mergeRespValid    := true.B
    mergeRespEntryIdx := io.vrfReadResp.bits.entryIdx
    mergeRespRobIdx   := io.vrfReadResp.bits.robIdx
    mergeRespData     := io.vrfReadResp.bits.data
  }

  when(hasSplitDone) {
    io.stateUpdate.valid := true.B
    io.stateUpdate.bits.entryIdx  := splitDoneEntry.entryIdx
    io.stateUpdate.bits.stateNext := splitDoneStateNext
  }.elsewhen(vrfWriteValid) {
    io.stateUpdate.valid := true.B
    io.stateUpdate.bits.entryIdx  := io.vrfWriteReq.bits.entryIdx
    io.stateUpdate.bits.stateNext := VAGQEntryState.wb
  }.elsewhen(io.robWriteback.fire) {
    io.stateUpdate.valid := true.B
    io.stateUpdate.bits.entryIdx   := io.robWriteback.bits.entryIdx
    io.stateUpdate.bits.clearValid := true.B
  }
}

class MergeCtrlIO(numEntries: Int)(implicit p: Parameters) extends VAGQBundle {
  val entry        = Input(Vec(numEntries, new CtrlInput))
  val lduResp      = Flipped(Vec(VAGQConstants.LduRespWidth, Valid(new VAGQResp)))
  val staResp      = Flipped(Vec(VAGQConstants.StaRespWidth, Valid(new VAGQResp)))
  val lsqEmptyResp = Flipped(Valid(new VAGQResp))
  val reqUpdate    = Vec(VAGQConstants.MergeRespWidth, Valid(new VAGQReqBitmapUpdate))
  val stateUpdate  = Valid(new VAGQEntryStateUpdate)
  val vrfReadReq   = Valid(new VAGQVRFReadReq)
  val vrfReadResp  = Flipped(Valid(new VAGQVRFReadResp))
  val vrfWriteReq  = ValidIO(new VAGQVRFWriteReq)
  val robWriteback = Decoupled(new VAGQWritebackReq)
  val redirect     = Flipped(Valid(new Redirect))
}
