package xiangshan.backend.vector.vagq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr

import VAGQConstants._

class SplitCtrl(numEntries: Int)(implicit p: Parameters) extends VAGQModule {
  val io = IO(new SplitCtrlIO(numEntries))

  private val activePending  = Wire(Vec(numEntries, UInt(vagqFlowBytes.W)))
  private val emptyPending   = Wire(Vec(numEntries, UInt(vagqFlowBytes.W)))
  private val orderedBlocked = Wire(Vec(numEntries, Bool()))
  private val canSplit       = Wire(Vec(numEntries, Bool()))

  for (i <- 0 until numEntries) {
    val entry = io.in(i).entry
    canSplit(i) := entry.valid && entry.state === VAGQEntryState.split && !entry.robIdx.needFlush(io.redirect)
    orderedBlocked(i) := entry.isOrdered && ((entry.reqSent & ~entry.reqAck & entry.elemActiveMask).orR)
    activePending(i) := Mux(
      canSplit(i) && !orderedBlocked(i),
      ~entry.reqSent & entry.elemActiveMask,
      0.U(vagqFlowBytes.W)
    )
    emptyPending(i) := Mux(
      canSplit(i),
      ~entry.reqSent & ~entry.reqAck & ~entry.elemActiveMask,
      0.U(vagqFlowBytes.W)
    )
  }

  private val activeEntryHasReq = VecInit(activePending.map(_.orR))
  private val emptyEntryHasReq  = VecInit(emptyPending.map(_.orR))

  private val hasActiveReq = activeEntryHasReq.asUInt.orR
  private val hasEmptyReq  = emptyEntryHasReq.asUInt.orR
  private val activeSel    = OHToUInt(oldestEntryOH((0 until numEntries).map(i => activeEntryHasReq(i)), io.in, numEntries))
  private val emptySel     = OHToUInt(oldestEntryOH((0 until numEntries).map(i => emptyEntryHasReq(i)), io.in, numEntries))
  private val activeInput  = io.in(activeSel)
  private val emptyInput   = io.in(emptySel)
  private val activeMask   = activePending(activeSel)
  private val emptyMask    = emptyPending(emptySel)

  private val activeLowOffset = lowBit(activeMask)

  private val activeAddrGen = Seq.fill(VAGQConstants.ActiveIssueWidth)(Module(new AddrGen))
  activeAddrGen(0).in.uopType    := activeInput.entry.uopType
  activeAddrGen(0).in.baseAddr   := activeInput.entry.baseAddr
  activeAddrGen(0).in.op2Data    := activeInput.entry.op2Data
  activeAddrGen(0).in.uopIdx     := activeInput.entry.uopIdx
  activeAddrGen(0).in.byteOffset := activeLowOffset
  activeAddrGen(0).in.deew       := activeInput.entry.deew
  activeAddrGen(0).in.ieew       := activeInput.entry.ieew

  private val activeLowIssueMask = activeAddrGen(0).out.elemMask & activeMask
  private val activeRemaining    = activeMask & ~activeLowIssueMask
  private val activeHighOffset   = highBit(activeRemaining)
  private val activeHasTwoReq    = activeRemaining.orR
  private val activeCanIssueSecond = activeHasTwoReq && !activeInput.entry.isOrdered
  activeAddrGen(1).in.uopType    := activeInput.entry.uopType
  activeAddrGen(1).in.baseAddr   := activeInput.entry.baseAddr
  activeAddrGen(1).in.op2Data    := activeInput.entry.op2Data
  activeAddrGen(1).in.uopIdx     := activeInput.entry.uopIdx
  activeAddrGen(1).in.byteOffset := activeHighOffset
  activeAddrGen(1).in.deew       := activeInput.entry.deew
  activeAddrGen(1).in.ieew       := activeInput.entry.ieew

  private val activeOffsets = Seq(activeLowOffset, activeHighOffset)

  private val activeIssueMasks = Wire(Vec(VAGQConstants.ActiveIssueWidth, UInt(vagqFlowBytes.W)))
  activeIssueMasks(0) := activeLowIssueMask
  activeIssueMasks(1) := Mux(
    activeCanIssueSecond,
    activeAddrGen(1).out.elemMask & activeRemaining,
    0.U(vagqFlowBytes.W)
  )

  private val activeLaneValids = Seq(
    hasActiveReq,
    hasActiveReq && activeCanIssueSecond
  )

  for (lane <- 0 until VAGQConstants.ActiveIssueWidth) {
    val activeElemIdx = activeAddrGen(lane).out.elemIdx
    val activeAlignedType = Cat(0.U((VAGQConstants.AlignedTypeWidth - VAGQConstants.EewWidth).W), activeInput.entry.deew)

    io.lsuReq(lane).valid := activeLaneValids(lane)
    io.lsuReq(lane).bits  := 0.U.asTypeOf(io.lsuReq(lane).bits)
    io.lsuReq(lane).bits.entryIdx    := activeInput.entryIdx
    io.lsuReq(lane).bits.robIdx      := activeInput.entry.robIdx
    io.lsuReq(lane).bits.isLoad      := activeInput.entry.isLoad
    io.lsuReq(lane).bits.isStore     := activeInput.entry.isStore
    io.lsuReq(lane).bits.byteOffset  := activeOffsets(lane)
    io.lsuReq(lane).bits.elemIdx     := activeElemIdx
    io.lsuReq(lane).bits.mask        := activeIssueMasks(lane)
    io.lsuReq(lane).bits.alignedType := activeAlignedType
    io.lsuReq(lane).bits.lqIdx       := activeInput.entry.meta.lqIdx + activeElemIdx
    io.lsuReq(lane).bits.sqIdx       := activeInput.entry.meta.sqIdx + activeElemIdx
    io.lsuReq(lane).bits.vaddr := activeAddrGen(lane).out.vaddr
    io.lsuReq(lane).bits.data  := 0.U(VLEN.W)
    io.lsuReq(lane).bits.pdest := activeInput.entry.pdest
    io.lsuReq(lane).bits.nf    := activeInput.entry.nf
  }

  private val emptyEntryMask = byteMaskToEntryMask(emptyMask, emptyInput.entry.deew)
  io.lsqEmptyReq.valid := hasEmptyReq
  io.lsqEmptyReq.bits := 0.U.asTypeOf(io.lsqEmptyReq.bits)
  io.lsqEmptyReq.bits.entryIdx := emptyInput.entryIdx
  io.lsqEmptyReq.bits.robIdx := emptyInput.entry.robIdx
  io.lsqEmptyReq.bits.isLoad := emptyInput.entry.isLoad
  io.lsqEmptyReq.bits.isStore := emptyInput.entry.isStore
  io.lsqEmptyReq.bits.lqIdx := emptyInput.entry.meta.lqIdx
  io.lsqEmptyReq.bits.sqIdx := emptyInput.entry.meta.sqIdx
  io.lsqEmptyReq.bits.emptyMask := emptyMask
  io.lsqEmptyReq.bits.entryMask := emptyEntryMask

  for (lane <- 0 until VAGQConstants.SplitUpdateWidth) {
    io.update(lane).valid := false.B
    io.update(lane).bits  := 0.U.asTypeOf(io.update(lane).bits)
  }

  private val activeFireMask = (0 until VAGQConstants.ActiveIssueWidth).map { lane =>
    Mux(io.lsuReq(lane).fire, activeIssueMasks(lane), 0.U(vagqFlowBytes.W))
  }.reduce(_ | _)
  io.update(0).valid           := io.lsuReq.map(_.fire).reduce(_ || _)
  io.update(0).bits.entryIdx   := activeInput.entryIdx
  io.update(0).bits.setReqSent := activeFireMask

  io.update(1).valid           := io.lsqEmptyReq.fire
  io.update(1).bits.entryIdx   := emptyInput.entryIdx
  io.update(1).bits.setReqSent := emptyMask
}

class SplitCtrlIO(numEntries: Int)(implicit p: Parameters) extends VAGQBundle {
  val in          = Input(Vec(numEntries, new CtrlInput))
  val redirect    = Flipped(Valid(new Redirect))
  val lsuReq      = Vec(VAGQConstants.ActiveIssueWidth, Decoupled(new VAGQLsuReq))
  val lsqEmptyReq = Decoupled(new VAGQLsqEmptyReq)
  val update      = Vec(VAGQConstants.SplitUpdateWidth, Valid(new VAGQReqBitmapUpdate))
}

class VAGQReqBitmapUpdate(implicit p: Parameters) extends VAGQBundle {
  val entryIdx        = UInt(vagqEntryIdxWidth.W)
  val setReqSent      = UInt(vagqFlowBytes.W)
  val clearReqSent    = UInt(vagqFlowBytes.W)
  val setReqAck       = UInt(vagqFlowBytes.W)
  val exception       = Bool()
  val exceptionNumber = UInt(ExceptionNumberWidth.W)
  val faultElemIdx    = UInt(vagqFlowByteWidth.W)
}
