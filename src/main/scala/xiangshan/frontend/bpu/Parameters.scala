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
import xiangshan.frontend.bpu.abtb.AheadBtbParameters
import xiangshan.frontend.bpu.ubtb.MicroBtbParameters

case class BpuParameters(
    // general
    FetchBlockSize:      Int = 32, // bytes // FIXME: 64B, waiting for ftq/icache support
    FetchBlockAlignSize: Int = 32, // bytes
    // sub predictors
    ubtbParameters: MicroBtbParameters = MicroBtbParameters(),
    aBtbParameters: AheadBtbParameters = AheadBtbParameters()
) {
  // sanity check
  require(isPow2(FetchBlockSize))
  require(isPow2(FetchBlockAlignSize))
}

trait HasBpuParameters extends HasXSParameter {
  def bpuParameters: BpuParameters = coreParams.bpuParameters

  // general
  def FetchBlockSize:       Int = bpuParameters.FetchBlockSize
  def FetchBlockAlignSize:  Int = bpuParameters.FetchBlockAlignSize
  def FetchBlockAlignWidth: Int = log2Ceil(FetchBlockAlignSize)
  def FetchBlockInstNum:    Int = FetchBlockSize / instBytes

  def CfiPositionWidth: Int = log2Ceil(FetchBlockInstNum) // 2/4B(inst) aligned
}
