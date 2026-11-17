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

// A combinational banking view; the caller owns all storage and pipeline registers.
private[rob] final class RobBanking(entryCount: Int, accessWidth: Int) {
  require(accessWidth > 0 && accessWidth <= entryCount)
  private val bankBits = log2Ceil(accessWidth)
  private val bankCount = 1 << bankBits
  require(entryCount % bankCount == 0, "ROB size must be divisible by the rounded banking width")
  private val rowCount = entryCount / bankCount

  private def bankIndex(index: UInt): UInt = if (bankBits == 0) 0.U else index(bankBits - 1, 0)
  private def rowIndex(index: UInt): UInt = index >> bankBits

  def firstRowSelect: UInt = 1.U(rowCount.W)

  def rowSelect(index: UInt): UInt = UIntToOH(rowIndex(index), rowCount)

  def nextRowSelect(select: UInt): UInt = Mux(select.head(1) === 1.U, 1.U, select << 1)

  // Allocation addresses must be consecutive and span at most accessWidth entries.
  // Return aliases for every entry so callers need no knowledge of bank numbering.
  def selectEnqueue[T <: Data](valid: Seq[Bool], indices: Seq[UInt], data: Seq[T]): Seq[T] = {
    require(valid.nonEmpty && valid.length <= accessWidth)
    require(valid.length == indices.length && valid.length == data.length)
    val selected = (0 until bankCount).map { bank =>
      val hits = VecInit(valid.zip(indices).map { case (enabled, index) =>
        enabled && bankIndex(index) === bank.U
      })
      Mux1H(hits, data)
    }
    (0 until entryCount).map(index => selected(index % bankCount))
  }

  // Both rows retain bank order. The second row wraps from the last row to row zero.
  def readRows[T <: Data](entries: Seq[T], select: UInt, keepDebug: Boolean): (Vec[T], Vec[T]) = {
    require(entries.length == entryCount && select.getWidth == rowCount)
    val banks = VecInit((0 until bankCount).map { bank =>
      VecInit((0 until rowCount).map(row => entries(row * bankCount + bank)))
    })
    if (keepDebug) {
      dontTouch(banks)
    }
    val current = VecInit(banks.map(bank => Mux1H(select, bank)))
    val next = VecInit(banks.map(bank => Mux1H(select, bank.drop(1) :+ bank(0))))
    (current, next)
  }

  // Labels follow the supplied pointer, independently of the registered read address.
  def rowIndices(index: UInt): Vec[UInt] = {
    val current = rowIndex(index)
    val next = Mux(current === (rowCount - 1).U, 0.U, current + 1.U)
    VecInit(Seq(current, next).flatMap { row =>
      (0 until bankCount).map { bank =>
        if (bankBits == 0) row else Cat(row, bank.U(bankBits.W))
      }
    })
  }
}
