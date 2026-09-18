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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import xiangshan.frontend.HasFrontendParameters

case class FtqParameters(
    FtqSize:           Int = 64,
    ResolveQueueSize:  Int = 16,
    BpTrainStallLimit: Int = 8,
    CommitQueueSize:   Int = 64,
    // submodules
    pqParameters: FtqPrefetchQueueParameters = FtqPrefetchQueueParameters()
) {
  // sanity check
  require(isPow2(FtqSize))
}

trait HasFtqParameters extends HasFrontendParameters {
  def ftqParameters: FtqParameters = frontendParameters.ftqParameters

  def ResolveQueueSize:  Int = ftqParameters.ResolveQueueSize
  def BpTrainStallLimit: Int = ftqParameters.BpTrainStallLimit
  def CommitQueueSize:   Int = ftqParameters.CommitQueueSize
}

case class FtqPrefetchQueueParameters(
    Size: Int = 8,
    // when distanceBetween(pfPtr, fetchPtr), prefer 1-fdip over 2- or 1-queued
    PreferFdipDistance: Int = 2
) {}

trait HasFtqPrefetchQueueParameters extends HasFtqParameters {
  def pqParameters: FtqPrefetchQueueParameters = ftqParameters.pqParameters

  def Size: Int = pqParameters.Size

  def PreferFdipDistance: Int = pqParameters.PreferFdipDistance

  def EnqueueSwNum: Int = backendParams.LduCnt // every load unit may send software prefetch.i request to pq
  def EnqueueNum:   Int = EnqueueSwNum         // TODO: more prefetch source
}
