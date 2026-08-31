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
  val vecEntries: Int = 32
  val vlEntries: Int = 1
  val robEntries: Int = coreParams.RobSize
  val renameWidth: Int = coreParams.RenameWidth
  val commitWidth: Int = coreParams.CommitWidth

  require(Seq(intEntries, fpEntries, vecEntries, vlEntries).forall(_ > 0))
  require(coreParams.IntLogicRegs >= intEntries)
  require(coreParams.FpLogicRegs >= fpEntries)
  require(coreParams.VecLogicRegs >= vecEntries)
  require(coreParams.VlLogicRegs >= vlEntries)
  require(robEntries > 0)
  require(renameWidth > 0)
  require(commitWidth > 0)

  val totalEntries: Int = intEntries + fpEntries + vecEntries + vlEntries
  // Keep the two circular ROB pointer generations distinct in snapshot storage.
  val storageEntries: Int = robEntries * 2
  val bankBits: Int = log2Ceil(renameWidth max 2)
  val slotBits: Int = log2Ceil(storageEntries)
}

class DiffRatState(val params: DiffRatStateParams)(implicit p: Parameters) extends XSBundle {
  val intRat = Vec(params.intEntries, UInt(PhyRegIdxWidth.W))
  val fpRat = Vec(params.fpEntries, UInt(PhyRegIdxWidth.W))
  val vecRat = Vec(params.vecEntries, UInt(PhyRegIdxWidth.W))
  val vlRat = Vec(params.vlEntries, UInt(PhyRegIdxWidth.W))
}

class DiffRatRenameUpdate(implicit p: Parameters) extends XSBundle {
  val ldest = UInt(LogicRegsWidth.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val pdestVl = UInt(VlPhyRegIdxWidth.W)
  val rfWen = Bool()
  val fpWen = Bool()
  val vecWen = Bool()
  val vlWen = Bool()
}

object DiffRatState {
  def init(params: DiffRatStateParams)(implicit p: Parameters): DiffRatState = {
    val state = Wire(new DiffRatState(params))
    val pregWidth = state.intRat.head.getWidth
    state.intRat := VecInit.fill(params.intEntries)(0.U(pregWidth.W))
    state.fpRat := VecInit.tabulate(params.fpEntries)(_.U(pregWidth.W))
    state.vecRat := VecInit.tabulate(params.vecEntries)(_.U(pregWidth.W))
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
    val snapshotEnds = Input(Vec(params.renameWidth, Valid(new RobPtr)))
    val commitRobIdx = Input(Valid(new RobPtr))
    val commitRobIdxVec = Input(Vec(params.commitWidth, Valid(new RobPtr)))
    val diffRat = Output(new DiffRatState(params))
  })

  private def slotOf(ptr: RobPtr): UInt = {
    val generationOffset = Mux(ptr.flag, params.robEntries.U(params.slotBits.W), 0.U(params.slotBits.W))
    (Cat(0.U(1.W), ptr.value) +& generationOffset)(params.slotBits - 1, 0)
  }

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
      val hit = req.valid && req.bits.vecWen && req.bits.ldest === reg.U
      next.vecRat(reg) := Mux(hit, req.bits.pdest, prev.vecRat(reg))
    }
    for (reg <- 0 until params.vlEntries) {
      // VL has a single logical register indexed 0, so it does not use the generic ldest.
      val hit = req.valid && req.bits.vlWen
      next.vlRat(reg) := Mux(hit, req.bits.pdestVl, prev.vlRat(reg))
    }
  }

  val stateBanks = Seq.tabulate(params.renameWidth) { bank =>
    Mem(params.storageEntries, UInt(stateWidth.W))
      .suggestName(s"diff_rat_state_bank_$bank")
  }
  val stateWriteValid = VecInit(io.snapshotEnds.map(_.valid))
  val stateWriteSlots = io.snapshotEnds.map(req => slotOf(req.bits))
  val stateWriteData = laneRat.tail.map(_.asUInt)
  val stateBankTags = Mem(params.storageEntries, UInt(params.bankBits.W))
  val stateSlotValid = RegInit(VecInit.fill(params.storageEntries)(false.B))

  // ROB compression means snapshot slots are not necessarily consecutive.
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
      stateSlotValid(stateWriteSlots(lane)) := true.B
    }
  }

  val readSlot = slotOf(io.commitRobIdx.bits)
  val readRequestValid = io.commitRobIdx.valid && stateSlotValid(readSlot)
  val readWriteConflict = VecInit.tabulate(params.renameWidth) { lane =>
    stateWriteValid(lane) && stateWriteSlots(lane) === readSlot
  }.asUInt.orR
  assert(!io.commitRobIdx.valid || stateSlotValid(readSlot), "diff RAT commit reads an invalid state")
  assert(!io.commitRobIdx.valid || !readWriteConflict, "diff RAT state is read and written in the same cycle")

  for (commit <- io.commitRobIdxVec) {
    val commitSlot = slotOf(commit.bits)
    val commitWriteConflict = VecInit.tabulate(params.renameWidth) { lane =>
      stateWriteValid(lane) && stateWriteSlots(lane) === commitSlot
    }.asUInt.orR
    when(commit.valid) {
      assert(stateSlotValid(commitSlot), "diff RAT commit clears an invalid state")
      assert(!commitWriteConflict, "diff RAT state is committed and written in the same cycle")
      stateSlotValid(commitSlot) := false.B
    }
  }
  for {
    older <- 0 until params.commitWidth
    younger <- older + 1 until params.commitWidth
  } {
    assert(
      !(io.commitRobIdxVec(older).valid && io.commitRobIdxVec(younger).valid &&
        io.commitRobIdxVec(older).bits === io.commitRobIdxVec(younger).bits),
      "two commit lanes clear the same diff RAT slot"
    )
  }

  val bankReadData = RegEnable(VecInit(stateBanks.map(_.read(readSlot))), readRequestValid)
  val readValid = RegNext(readRequestValid, false.B)
  val readBankOH = RegEnable(UIntToOH(stateBankTags(readSlot), params.renameWidth), readRequestValid)
  val readState = Mux1H(readBankOH, bankReadData).asTypeOf(new DiffRatState(params))
  val committedRat = RegInit(DiffRatState.init(params))

  when(readValid) {
    committedRat := readState
  }
  io.diffRat := Mux(readValid, readState, committedRat)
}
