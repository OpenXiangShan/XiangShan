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

// TODO: maybe move to utility?
class SignedSaturateCounter(width: Int) extends Bundle {
  val value: SInt = SInt(width.W)

  def isPositive: Bool = value >= 0.S

  def isSaturatePositive: Bool = value === ((1 << (width - 1)) - 1).S

  def isNegative: Bool = value < 0.S

  def isSaturateNegative: Bool = value === (-(1 << (width - 1))).S // scalastyle:ignore

  def getUpdate(positive: Bool): SInt = Mux(
    isSaturatePositive && positive || isSaturateNegative && !positive,
    value,
    Mux(positive, value + 1.S, value - 1.S)
  )

  def getIncrease: SInt = Mux(isSaturatePositive, value, value + 1.S)

  def getDecrease: SInt = Mux(isSaturateNegative, value, value - 1.S)

  def update(positive: Bool): Unit =
    value := getUpdate(positive)

  def increase(): Unit =
    value := getIncrease

  def decrease(): Unit =
    value := getDecrease

  def resetZero(): Unit =
    value := 0.S

  def resetNegative(): Unit =
    value := -(1 << (width - 1)).S // scalastyle:ignore

  def resetNeutral(): Unit =
    value := 0.S

  def resetPositive(): Unit =
    value := ((1 << (width - 1)) - 1).S
}
