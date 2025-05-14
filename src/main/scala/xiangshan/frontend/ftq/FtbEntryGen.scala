// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.FTBEntry
import xiangshan.frontend.HasBPUParameter
import xiangshan.frontend.PrunedAddr

class FtbEntryGen(implicit p: Parameters) extends FtqModule with HasBPUParameter {
  val io = IO(new Bundle {
    val start_addr     = Input(PrunedAddr(VAddrBits))
    val old_entry      = Input(new FTBEntry)
    val pd             = Input(new FtqPdEntry)
    val cfiIndex       = Flipped(Valid(UInt(log2Ceil(PredictWidth).W)))
    val target         = Input(PrunedAddr(VAddrBits))
    val hit            = Input(Bool())
    val mispredict_vec = Input(Vec(PredictWidth, Bool()))

    val new_entry         = Output(new FTBEntry)
    val new_br_insert_pos = Output(Vec(numBr, Bool()))
    val taken_mask        = Output(Vec(numBr, Bool()))
    val jmp_taken         = Output(Bool())
    val mispred_mask      = Output(Vec(numBr + 1, Bool()))

    // for perf counters
    val is_init_entry           = Output(Bool())
    val is_old_entry            = Output(Bool())
    val is_new_br               = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_strong_bias_modified = Output(Bool())
    val is_br_full              = Output(Bool())
  })

  // no mispredictions detected at predecode
  val hit = io.hit
  val pd  = io.pd

  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))

  val cfi_is_br       = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp   = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp && pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp && pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp && pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi    = entry_has_jmp && pd.jmpOffset === (PredictWidth - 1).U && !pd.rvcMask.last
  // val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last

  val cfi_is_jal  = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  def carryPos = log2Ceil(PredictWidth) + instOffsetBits
  private def getLower(pc: PrunedAddr) = pc(carryPos - 1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := true.B
  // tag is left for ftb to assign

  // case br
  val init_br_slot = init_entry.getSlotForBr(0)
  when(cfi_is_br) {
    init_br_slot.valid  := true.B
    init_br_slot.offset := io.cfiIndex.bits
    init_br_slot.setLowerStatByTarget(io.start_addr, io.target, numBr == 1)
    init_entry.strong_bias(0) := true.B // set to strong bias on init
  }

  // case jmp
  when(entry_has_jmp) {
    init_entry.tailSlot.offset := pd.jmpOffset
    init_entry.tailSlot.valid  := new_jmp_is_jal || new_jmp_is_jalr
    init_entry.tailSlot.setLowerStatByTarget(io.start_addr, Mux(cfi_is_jalr, io.target, pd.jalTarget), isShare = false)
    init_entry.strong_bias(numBr - 1) := new_jmp_is_jalr // set strong bias for the jalr on init
  }

  val jmpPft = getLower(io.start_addr) +& pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, getLower(io.start_addr))
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos - instOffsetBits), true.B)

  require(
    isPow2(PredictWidth),
    "If PredictWidth does not satisfy the power of 2," +
      "pftAddr := getLower(io.start_addr) and carry := true.B  not working!!"
  )

  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  // that means fall thru points to the middle of an inst
  init_entry.last_may_be_rvi_call := pd.jmpOffset === (PredictWidth - 1).U && !pd.rvcMask(pd.jmpOffset)

  // if hit, check whether a new cfi(only br is possible) is detected
  val oe              = io.old_entry
  val br_recorded_vec = oe.getBrRecordedVec(io.cfiIndex.bits)
  val br_recorded     = br_recorded_vec.asUInt.orR
  val is_new_br       = cfi_is_br && !br_recorded
  val new_br_offset   = io.cfiIndex.bits
  // vec(i) means new br will be inserted BEFORE old br(i)
  val allBrSlotsVec = oe.allSlotsForBr
  val new_br_insert_onehot = VecInit((0 until numBr).map {
    i =>
      i match {
        case 0 =>
          !allBrSlotsVec(0).valid || new_br_offset < allBrSlotsVec(0).offset
        case idx =>
          allBrSlotsVec(idx - 1).valid && new_br_offset > allBrSlotsVec(idx - 1).offset &&
          (!allBrSlotsVec(idx).valid || new_br_offset < allBrSlotsVec(idx).offset)
      }
  })

  val old_entry_modified = WireInit(io.old_entry)
  for (i <- 0 until numBr) {
    val slot = old_entry_modified.allSlotsForBr(i)
    when(new_br_insert_onehot(i)) {
      slot.valid  := true.B
      slot.offset := new_br_offset
      slot.setLowerStatByTarget(io.start_addr, io.target, i == numBr - 1)
      old_entry_modified.strong_bias(i) := true.B
    }.elsewhen(new_br_offset > oe.allSlotsForBr(i).offset) {
      old_entry_modified.strong_bias(i) := false.B
      // all other fields remain unchanged
    }.otherwise {
      // case i == 0, remain unchanged
      if (i != 0) {
        val noNeedToMoveFromFormerSlot = (i == numBr - 1).B && !oe.brSlots.last.valid
        when(!noNeedToMoveFromFormerSlot) {
          slot.fromAnotherSlot(oe.allSlotsForBr(i - 1))
          old_entry_modified.strong_bias(i) := oe.strong_bias(i)
        }
      }
    }
  }

  // two circumstances:
  // 1. oe: | br | j  |, new br should be in front of j, thus addr of j should be new pft
  // 2. oe: | br | br |, new br could be anywhere between, thus new pft is the addr of either
  //        the previous last br or the new br
  val may_have_to_replace = oe.noEmptySlotForNewBr
  val pft_need_to_change  = is_new_br && may_have_to_replace
  // it should either be the given last br or the new br
  when(pft_need_to_change) {
    val new_pft_offset =
      Mux(!new_br_insert_onehot.asUInt.orR, new_br_offset, oe.allSlotsForBr.last.offset)

    // set jmp to invalid
    old_entry_modified.pftAddr              := getLower(io.start_addr) + new_pft_offset
    old_entry_modified.carry                := (getLower(io.start_addr) +& new_pft_offset).head(1).asBool
    old_entry_modified.last_may_be_rvi_call := false.B
    old_entry_modified.isCall               := false.B
    old_entry_modified.isRet                := false.B
    old_entry_modified.isJalr               := false.B
  }

  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target      = oe.tailSlot.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.tailSlot.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when(jalr_target_modified) {
    old_entry_jmp_target_modified.setByJmpTarget(io.start_addr, io.target)
    old_entry_jmp_target_modified.strong_bias := 0.U.asTypeOf(Vec(numBr, Bool()))
  }

  val old_entry_strong_bias    = WireInit(oe)
  val strong_bias_modified_vec = Wire(Vec(numBr, Bool())) // whether modified or not
  for (i <- 0 until numBr) {
    when(br_recorded_vec(0)) {
      old_entry_strong_bias.strong_bias(0) :=
        oe.strong_bias(0) && io.cfiIndex.valid && oe.brValids(0) && io.cfiIndex.bits === oe.brOffset(0)
    }.elsewhen(br_recorded_vec(numBr - 1)) {
      old_entry_strong_bias.strong_bias(0) := false.B
      old_entry_strong_bias.strong_bias(numBr - 1) :=
        oe.strong_bias(numBr - 1) && io.cfiIndex.valid && oe.brValids(numBr - 1) && io.cfiIndex.bits === oe.brOffset(
          numBr - 1
        )
    }
    strong_bias_modified_vec(i) := oe.strong_bias(i) && oe.brValids(i) && !old_entry_strong_bias.strong_bias(i)
  }
  val strong_bias_modified = strong_bias_modified_vec.reduce(_ || _)

  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified, Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_strong_bias))

  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.new_br_insert_pos := new_br_insert_onehot
  io.taken_mask := VecInit((io.new_entry.brOffset zip io.new_entry.brValids).map {
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v
  })
  io.jmp_taken := io.new_entry.jmpValid && io.new_entry.tailSlot.offset === io.cfiIndex.bits
  for (i <- 0 until numBr) {
    io.mispred_mask(i) := io.new_entry.brValids(i) && io.mispredict_vec(io.new_entry.brOffset(i))
  }
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredict_vec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry           := !hit
  io.is_old_entry            := hit && !is_new_br && !jalr_target_modified && !strong_bias_modified
  io.is_new_br               := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_target_modified
  io.is_strong_bias_modified := hit && strong_bias_modified
  io.is_br_full              := hit && is_new_br && may_have_to_replace
}
