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

package xiangshan.frontend.bpu.ras

import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters

case class RasParameters(
    StackSize:         Int = 16, // Size of the RAS stack
    SpecSize:          Int = 32, // Size of the RAS speculative queue
    StackCounterWidth: Int = 3   // Width of the RAS counter (log2 of number of same calls merged in single stack entry)
) {
  require(isPow2(SpecSize), "SpecSize must be a power of 2")
}

trait HasRasParameters extends HasBpuParameters {
  def rasParameters: RasParameters = bpuParameters.rasParameters

  def StackSize:         Int = rasParameters.StackSize
  def SpecQueueSize:     Int = rasParameters.SpecSize
  def StackCounterWidth: Int = rasParameters.StackCounterWidth
  def StackCounterMax:   Int = (1 << StackCounterWidth) - 1

  // ras cannot be fast-trained, this is required by abstract class BasePredictor
  def EnableFastTrain: Boolean = false
}
