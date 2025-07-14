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

import chisel3._
import xiangshan.frontend.bpu.HasBpuParameters

trait Helpers extends HasBpuParameters {
  def ptrInc(ptr: UInt): UInt = ptr + 1.U
  def ptrDec(ptr: UInt): UInt = ptr - 1.U

  def specPtrInc(ptr: RasPtr): RasPtr = ptr + 1.U
  def specPtrDec(ptr: RasPtr): RasPtr = ptr - 1.U
}
