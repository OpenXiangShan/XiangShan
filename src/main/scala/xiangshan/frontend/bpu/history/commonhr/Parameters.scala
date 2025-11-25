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

package xiangshan.frontend.bpu.history.commonhr

import xiangshan.frontend.bpu.HasBpuParameters

case class CommonHRParameters(
    StallQueueSize:  Int = 2,
    BWHistoryLength: Int = 8
) {}

trait HasCommonHRParameters extends HasBpuParameters {
  def commonHRParameters: CommonHRParameters = bpuParameters.commonHRParameters

  def StallQueueSize:  Int = commonHRParameters.StallQueueSize
  def BWHistoryLength: Int = commonHRParameters.BWHistoryLength
}
