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
import xiangshan.frontend.HasFrontendParameters
import xiangshan.frontend.bpu.abtb.AheadBtbParameters
import xiangshan.frontend.bpu.mbtb.MainBtbParameters
import xiangshan.frontend.bpu.ubtb.MicroBtbParameters

// For users: these are default Bpu parameters set by dev, do not change them here,
// use top-level Parameters.scala instead.
case class BpuParameters(
    // general
    FetchBlockAlignSize: Option[Int] = None, // bytes, if None, use half-align (FetchBLockSize / 2) by default
    // sub predictors
    ubtbParameters: MicroBtbParameters = MicroBtbParameters(),
    abtbParameters: AheadBtbParameters = AheadBtbParameters(),
    mbtbParameters: MainBtbParameters = MainBtbParameters()
) {}

trait HasBpuParameters extends HasFrontendParameters {
  def bpuParameters: BpuParameters = frontendParameters.bpuParameters

  // general
  def FetchBlockSizeWidth:  Int = log2Ceil(FetchBlockSize)
  def FetchBlockAlignSize:  Int = bpuParameters.FetchBlockAlignSize.getOrElse(FetchBlockSize / 2)
  def FetchBlockAlignWidth: Int = log2Ceil(FetchBlockAlignSize)

  // sanity check
  // should do this check in `case class BpuParameters` constructor, but we don't have access to `FetchBlockSize` there
  require(isPow2(FetchBlockAlignSize))
}
