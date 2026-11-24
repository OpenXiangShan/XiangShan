/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._

class BitMapFreeList(size: Int, allocWidth: Int)(implicit p: Parameters) extends XSModule {
  require(allocWidth > 0 && size >= allocWidth)
  private val indexWidth = log2Up(size)
  private val countWidth = log2Ceil(allocWidth + 1)

  val io = IO(new Bundle {
    val freeBitmap = Input(UInt(size.W))
    val allocateCount = Input(UInt(countWidth.W))
    val allocateSlot = Output(Vec(allocWidth, UInt(indexWidth.W)))
    val canAllocate = Output(Vec(allocWidth, Bool()))
  })

  val candidates = RegInit(VecInit(Seq.tabulate(allocWidth)(index => index.U(indexWidth.W))))
  val candidateValid = RegInit(((BigInt(1) << allocWidth) - 1).U(allocWidth.W))
  val candidateMask = (0 until allocWidth).map { rank =>
    Mux(candidateValid(rank), UIntToOH(candidates(rank), size), 0.U(size.W))
  }.reduce(_ | _)
  val refillBitmap = io.freeBitmap & ~candidateMask

  val prefixCounts = Seq.fill(log2Ceil(size) + 1)(Wire(Vec(size, UInt(countWidth.W))))
  for (entry <- 0 until size) {
    prefixCounts.head(entry) := refillBitmap(entry)
  }
  for (stage <- 0 until log2Ceil(size)) {
    val stride = 1 << stage
    for (entry <- 0 until size) {
      if (entry < stride) {
        prefixCounts(stage + 1)(entry) := prefixCounts(stage)(entry)
      } else {
        val sum = prefixCounts(stage)(entry) +& prefixCounts(stage)(entry - stride)
        prefixCounts(stage + 1)(entry) := Mux(sum > allocWidth.U, allocWidth.U, sum)
      }
    }
  }
  val refillSlots = VecInit((0 until allocWidth).map { rank =>
    val selected = VecInit((0 until size).map { entry =>
      val precedingCount = if (entry == 0) 0.U else prefixCounts.last(entry - 1)
      refillBitmap(entry) && precedingCount === rank.U
    })
    OHToUInt(selected)
  })

  val keptValid = candidateValid >> io.allocateCount
  val refillValid = UIntToMask(prefixCounts.last.last, allocWidth)
  // Decode the contiguous valid mask so refill indices are constants at elaboration time.
  val keepCountIs = (0 to allocWidth).map { kept =>
    keptValid === ((BigInt(1) << kept) - 1).U(allocWidth.W)
  }
  val nextValid = Wire(Vec(allocWidth, Bool()))
  for (rank <- 0 until allocWidth) {
    val choices = (0 to allocWidth).map { kept =>
      val slot = if (rank < kept) candidates(io.allocateCount + rank.U) else refillSlots(rank - kept)
      val valid = if (rank < kept) true.B else refillValid(rank - kept)
      (keepCountIs(kept), slot, valid)
    }
    candidates(rank) := Mux1H(choices.map { case (select, slot, _) => select -> slot })
    nextValid(rank) := Mux1H(choices.map { case (select, _, valid) => select -> valid })
    io.allocateSlot(rank) := candidates(rank)
    io.canAllocate(rank) := candidateValid(rank)
  }
  candidateValid := nextValid.asUInt

  XSError(((candidateValid >> 1) & ~candidateValid).orR, "Freelist candidate valid mask must be contiguous from bit 0")
  XSError(io.allocateCount > PopCount(candidateValid), "Freelist allocation exceeds prepared candidates")
  XSError((candidateMask & ~io.freeBitmap).orR, "Freelist candidate refers to an allocated entry")
  XSError(PopCount(candidateMask) =/= PopCount(candidateValid), "Freelist candidates are duplicated or out of range")
}
