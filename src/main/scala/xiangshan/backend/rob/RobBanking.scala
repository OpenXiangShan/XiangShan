/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rob

import chisel3._
import chisel3.util._

/**
 * A combinational view of the ROB's interleaved entry registers. The banks
 * are logical groups of registers, not physical SRAM banks. The caller owns
 * all storage and pipeline registers.
 *
 * The low bits of an entry index select the bank. The high bits select the row
 * within that bank. For example, 160 entries span eight banks as follows:
 *
 * {{{
 * Bank 0: 0, 8, 16, 24, ...
 * Bank 1: 1, 9, 17, 25, ...
 * Bank 2: 2, 10, 18, 26, ...
 * ...
 * Bank 7: 7, 15, 23, 31, ...
 * }}}
 *
 * A window of up to `accessWidth` consecutive entries uses each bank at most
 * once. [[bankWriteSelect]] uses this property to share one payload selector
 * among the entries in each bank.
 *
 * All banks share a one-hot row selector. Use [[firstRowSelect]] to
 * initialize it, [[rowSelect]] to derive it from a ROB entry index, and
 * [[nextRowSelect]] to advance to the next row.
 *
 * A retirement window can straddle a row boundary. [[readRows]] uses the
 * shared selector to read the current and next rows across all banks.
 * The next row wraps to row zero at the end of the ROB.
 *
 * TODO: Select a row independently for each bank to avoid reading two full rows
 * and improve timing.
 *
 * @param Size Total number of ROB entries.
 * @param accessWidth Maximum number of consecutive entries accessed at once.
 */
private[rob] final class RobBanking(Size: Int, accessWidth: Int) {
  require(accessWidth > 0 && accessWidth <= Size)

  // Round the access width up to a power of two so the bank and row can be
  // decoded directly from the index bits.
  private val bankBits = log2Ceil(accessWidth)
  val bankNum = 1 << bankBits
  val rowCount = Size / bankNum

  require(Size % bankNum == 0, "ROB size must be divisible by the rounded banking width")

  def bankIndex(index: UInt): UInt = if (bankBits == 0) 0.U else index(bankBits - 1, 0)
  def rowIndex(index: UInt): UInt = index >> bankBits

  // Initial one-hot selector for row zero.
  def firstRowSelect: UInt = 1.U(rowCount.W)
  // Decode the row bits of a ROB entry index into a one-hot selector.
  def rowSelect(index: UInt): UInt = UIntToOH(rowIndex(index), rowCount)
  // Advance the selector by one row, wrapping from the last row to row zero.
  def nextRowSelect(select: UInt): UInt = Mux(select.head(1) === 1.U, 1.U, select << 1)

  /**
   * Selects one enqueue payload per bank from the allocation pointers.
   * Enabled allocation indices must span at most `accessWidth` consecutive
   * ROB entries, so each bank has at most one enabled lane.
   *
   * @param valid Whether each allocation lane enqueues an entry.
   * @param indices ROB entry index allocated to each lane.
   * @param data Enqueue payload for each allocation lane.
   * @return One reference per ROB entry to the selected payload for its bank.
   */
  def bankWriteSelect[T <: Data](valid: Seq[Bool], indices: Seq[UInt], data: Seq[T]): Seq[T] = {
    require(valid.nonEmpty && valid.length <= accessWidth)
    require(valid.length == indices.length && valid.length == data.length)
    val selected = (0 until bankNum).map { bank =>
      val hits = VecInit(valid.zip(indices).map { case (enabled, index) =>
        enabled && bankIndex(index) === bank.U
      })
      Mux1H(hits, data)
    }

    // Each entry references its bank's result without adding a hardware mux.
    (0 until Size).map(entry => selected(entry % bankNum))
  }

  // Match rowIndices: the current row followed by the next row, each in bank order.
  def readRows[T <: Data](entries: Seq[T], select: UInt, keepDebug: Boolean): Vec[T] = {
    require(entries.length == Size && select.getWidth == rowCount)
    val banks = VecInit((0 until bankNum).map { bank =>
      VecInit((0 until rowCount).map(row => entries(row * bankNum + bank)))
    })
    if (keepDebug) {
      dontTouch(banks)
    }
    val current = banks.map(bank => Mux1H(select, bank))
    val next = banks.map(bank => Mux1H(select, bank.drop(1) :+ bank(0)))
    VecInit(current ++ next)
  }

  // Labels follow the supplied pointer, independently of the registered read address.
  def rowIndices(index: UInt): Vec[UInt] = {
    val current = rowIndex(index)
    val next = Mux(current === (rowCount - 1).U, 0.U, current + 1.U)
    VecInit(Seq(current, next).flatMap { row =>
      (0 until bankNum).map { bank =>
        if (bankBits == 0) row else Cat(row, bank.U(bankBits.W))
      }
    })
  }

  // Load one updated row into the shared dequeue registers, keeping each entry paired
  // with its commit readiness in the order returned by readRows.
  def updateDeqEntries(
    deqEntries: Vec[RobCommitEntryBundle],
    entries: Vec[RobEntryBundle],
    commitW: Vec[Bool],
    selectNextRow: Bool
  ): Unit = {
    require(deqEntries.length == bankNum)
    require(entries.length == 2 * bankNum && commitW.length == entries.length)
    for (bank <- 0 until bankNum) {
      val nextRowIndex = bank + bankNum
      deqEntries(bank).connectCommitEntry(entries(bank), commitW(bank))
      when(selectNextRow) {
        deqEntries(bank).connectCommitEntry(entries(nextRowIndex), commitW(nextRowIndex))
      }
    }
  }
}
