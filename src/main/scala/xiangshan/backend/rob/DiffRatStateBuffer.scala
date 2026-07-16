/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

case class DiffRatStateParams()(implicit p: Parameters) {
  private val coreParams = p(XSCoreParamsKey)

  val intEntries: Int = 32
  val fpEntries: Int = 32
  val vecEntries: Int = 31
  val v0Entries: Int = 1
  val vlEntries: Int = 1
  val robEntries: Int = coreParams.RobSize
  val renameWidth: Int = coreParams.RenameWidth
  val commitWidth: Int = coreParams.CommitWidth

  require(Seq(intEntries, fpEntries, vecEntries, v0Entries, vlEntries).forall(_ > 0))
  require(coreParams.IntLogicRegs >= intEntries)
  require(coreParams.FpLogicRegs >= fpEntries)
  require(coreParams.VecLogicRegs >= vecEntries + v0Entries)
  require(coreParams.V0LogicRegs >= v0Entries)
  require(coreParams.VlLogicRegs >= vlEntries)
  require(robEntries > 0)
  require(renameWidth > 0)
  require(commitWidth > 0)

  val totalEntries: Int = intEntries + fpEntries + vecEntries + v0Entries + vlEntries
  val storageEntries: Int = robEntries
  val bankBits: Int = log2Ceil(renameWidth max 2)
}

class DiffRatState(val params: DiffRatStateParams)(implicit p: Parameters) extends XSBundle {
  val intRat = Vec(params.intEntries, UInt(PhyRegIdxWidth.W))
  val fpRat = Vec(params.fpEntries, UInt(PhyRegIdxWidth.W))
  val vecRat = Vec(params.vecEntries, UInt(PhyRegIdxWidth.W))
  val v0Rat = Vec(params.v0Entries, UInt(PhyRegIdxWidth.W))
  val vlRat = Vec(params.vlEntries, UInt(PhyRegIdxWidth.W))
}

class DiffRatRenameUpdate(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val lastUop = Bool()
  val ldest = UInt(LogicRegsWidth.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val vecWen = Bool()
  val v0Wen = Bool()
  val vlWen = Bool()
}

object DiffRatState {
  def init(params: DiffRatStateParams)(implicit p: Parameters): DiffRatState = {
    val state = Wire(new DiffRatState(params))
    val pregWidth = state.v0Rat.head.getWidth
    state.intRat := VecInit.fill(params.intEntries)(0.U(pregWidth.W))
    state.fpRat := VecInit.tabulate(params.fpEntries)(_.U(pregWidth.W))
    state.vecRat := VecInit.tabulate(params.vecEntries)(i => (i + params.v0Entries).U(pregWidth.W))
    state.v0Rat := VecInit.tabulate(params.v0Entries)(_.U(pregWidth.W))
    state.vlRat := VecInit.tabulate(params.vlEntries)(_.U(pregWidth.W))
    state
  }
}

class DiffRatStateBuffer(implicit p: Parameters) extends XSModule {
  private val params = DiffRatStateParams()
  private val stateWidth = params.totalEntries * PhyRegIdxWidth

  val io = IO(new Bundle {
    val diffRatBase = Input(new DiffRatState(params))
    val renameUpdates = Input(Vec(params.renameWidth, Valid(new DiffRatRenameUpdate)))
    val commitRobIdx = Input(Valid(new RobPtr))
    val commitRobIdxVec = Input(Vec(params.commitWidth, Valid(new RobPtr)))
    val diffRat = Output(new DiffRatState(params))
  })

  private def slotOf(ptr: RobPtr): UInt = ptr.value

  val laneRat = Wire(Vec(params.renameWidth + 1, new DiffRatState(params)))
  laneRat.head := io.diffRatBase

  for (lane <- 0 until params.renameWidth) {
    val req = io.renameUpdates(lane)
    val prev = laneRat(lane)
    val next = laneRat(lane + 1)

    for (reg <- 0 until params.intEntries) {
      val hit = req.valid && req.bits.rfWen && req.bits.ldest === reg.U
      next.intRat(reg) := Mux(hit, req.bits.pdest, prev.intRat(reg))
    }
    for (reg <- 0 until params.fpEntries) {
      val hit = req.valid && req.bits.fpWen && req.bits.ldest === reg.U
      next.fpRat(reg) := Mux(hit, req.bits.pdest, prev.fpRat(reg))
    }
    for (reg <- 0 until params.vecEntries) {
      val hit = req.valid && req.bits.vecWen && req.bits.ldest === (reg + params.v0Entries).U
      next.vecRat(reg) := Mux(hit, req.bits.pdest, prev.vecRat(reg))
    }
    for (reg <- 0 until params.v0Entries) {
      val hit = req.valid && req.bits.v0Wen && req.bits.ldest === reg.U
      next.v0Rat(reg) := Mux(hit, req.bits.pdest, prev.v0Rat(reg))
    }
    for (reg <- 0 until params.vlEntries) {
      val hit = req.valid && req.bits.vlWen && req.bits.ldest === reg.U
      next.vlRat(reg) := Mux(hit, req.bits.pdest, prev.vlRat(reg))
    }
  }

  val stateBanks = Seq.tabulate(params.renameWidth) { bank =>
    SyncReadMem(params.storageEntries, UInt(stateWidth.W), SyncReadMem.ReadFirst)
      .suggestName(s"diff_rat_state_bank_$bank")
  }
  val stateWriteValid = VecInit(io.renameUpdates.map(req => req.valid && req.bits.lastUop))
  val stateWriteSlots = io.renameUpdates.map(req => slotOf(req.bits.robIdx))
  val stateWriteData = laneRat.tail.map(_.asUInt)
  val stateBankTags = SyncReadMem(params.storageEntries, UInt(params.bankBits.W), SyncReadMem.ReadFirst)
    .suggestName("diff_rat_state_bank_tags")
  val statePtrTags = SyncReadMem(params.storageEntries, new RobPtr, SyncReadMem.ReadFirst)
    .suggestName("diff_rat_state_ptr_tags")
  val stateSlotValid = RegInit(VecInit.fill(params.storageEntries)(false.B))

  // ROB compression means last-uop state slots are not necessarily consecutive.
  // Give each rename lane a dedicated bank and retain the selected lane per ROB slot.
  for {
    older <- 0 until params.renameWidth
    younger <- older + 1 until params.renameWidth
  } {
    assert(
      !(stateWriteValid(older) && stateWriteValid(younger) &&
        stateWriteSlots(older) === stateWriteSlots(younger)),
      "two rename lanes write the same diff RAT slot"
    )
  }
  for (lane <- 0 until params.renameWidth) {
    when(stateWriteValid(lane)) {
      stateBanks(lane).write(stateWriteSlots(lane), stateWriteData(lane))
      stateBankTags.write(stateWriteSlots(lane), lane.U)
      statePtrTags.write(stateWriteSlots(lane), io.renameUpdates(lane).bits.robIdx)
      stateSlotValid(stateWriteSlots(lane)) := true.B
    }
  }

  val readSlot = slotOf(io.commitRobIdx.bits)
  val readRequestValid = io.commitRobIdx.valid && stateSlotValid(readSlot)
  assert(!io.commitRobIdx.valid || stateSlotValid(readSlot), "diff RAT commit reads an invalid state")

  for (commit <- io.commitRobIdxVec) {
    val commitSlot = slotOf(commit.bits)
    val commitWriteCollision = VecInit.tabulate(params.renameWidth) { lane =>
      stateWriteValid(lane) && stateWriteSlots(lane) === commitSlot
    }.asUInt.orR
    when(commit.valid) {
      assert(stateSlotValid(commitSlot), "diff RAT commit clears an invalid state")
      for (lane <- 0 until params.renameWidth) {
        assert(
          !(stateWriteValid(lane) && stateWriteSlots(lane) === commitSlot &&
            io.renameUpdates(lane).bits.robIdx === commit.bits),
          "the same diff RAT pointer is committed and written in the same cycle"
        )
      }
      when(!commitWriteCollision) {
        stateSlotValid(commitSlot) := false.B
      }
    }
  }
  for {
    older <- 0 until params.commitWidth
    younger <- older + 1 until params.commitWidth
  } {
    assert(
      !(io.commitRobIdxVec(older).valid && io.commitRobIdxVec(younger).valid &&
        slotOf(io.commitRobIdxVec(older).bits) === slotOf(io.commitRobIdxVec(younger).bits)),
      "two commit lanes clear the same diff RAT slot"
    )
  }

  val bankReadData = VecInit(stateBanks.map(_.read(readSlot, readRequestValid)))
  val readValid = RegNext(readRequestValid, false.B)
  val readBankOH = UIntToOH(stateBankTags.read(readSlot, readRequestValid), params.renameWidth)
  val readPtrTag = statePtrTags.read(readSlot, readRequestValid)
  val readRobIdx = RegEnable(io.commitRobIdx.bits, readRequestValid)
  val readState = Mux1H(readBankOH, bankReadData).asTypeOf(new DiffRatState(params))
  val committedRat = RegInit(DiffRatState.init(params))

  when(readValid) {
    assert(readPtrTag === readRobIdx, "diff RAT state tag does not match commit ROB pointer")
    committedRat := readState
  }
  io.diffRat := Mux(readValid, readState, committedRat)
}
