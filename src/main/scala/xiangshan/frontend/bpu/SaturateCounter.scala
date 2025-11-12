// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu

import chisel3._

class SaturateCounter(width: Int) extends Bundle {
  val value: UInt = UInt(width.W)

  def isPositive: Bool = value(width - 1) // value >= (1 << (width - 1)).U

  def isSaturatePositive: Bool = value.andR // value === ((1 << width) - 1).U

  def isNegative: Bool = !value(width - 1) // value < (1 << (width - 1)).U

  def isSaturateNegative: Bool = !value.orR // value === 0.U

  def getUpdate(positive: Bool): UInt = Mux(
    isSaturatePositive && positive || isSaturateNegative && !positive,
    value,
    Mux(positive, value + 1.U, value - 1.U)
  )

  def getNeutral: UInt = (1 << (width - 1)).U

  def getIncrease: UInt = Mux(isSaturatePositive, value, value + 1.U)

  def getDecrease: UInt = Mux(isSaturateNegative, value, value - 1.U)

  def update(positive: Bool): Unit =
    value := getUpdate(positive)

  def increase(): Unit =
    value := getIncrease

  def decrease(): Unit =
    value := getDecrease

  def resetZero(): Unit =
    value := 0.U

  def resetNegative(): Unit =
    value := 0.U

  def resetNeutral(): Unit =
    value := (1 << (width - 1)).U

  def resetPositive(): Unit =
    value := ((1 << width) - 1).U

  // for TAGE taken ctr
  def isWeak: Bool = {
    require(width >= 2)
    val weakNotTaken = !value(width - 1) && value(width - 2, 0).andR
    val weakTaken    = value(width - 1) && !value(width - 2, 0).orR
    weakNotTaken || weakTaken // for 3 bit ctr: 011 || 100
  }
}
