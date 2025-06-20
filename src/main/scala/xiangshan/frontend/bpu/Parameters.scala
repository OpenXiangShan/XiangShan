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
import chisel3.util._
import xiangshan.HasXSParameter

// TODO: expose this to Parameters.scala / XSCore.scala
trait HasBpuParameters extends HasXSParameter {
  // general
  def FetchBlockMaxSize:    Int = 64 // bytes
  def FetchBlockAlign:      Int = 32 // bytes
  def FetchBlockAlignWidth: Int = log2Ceil(FetchBlockAlign)

  def PositionWidth: Int = log2Ceil(FetchBlockMaxSize) - 1 // 2B(rvc inst) aligned

  // sanity check
  require(isPow2(FetchBlockMaxSize))
  require(isPow2(FetchBlockAlign))
}
