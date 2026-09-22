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
    CommitStackSize:   Int = 16, // Size of the RAS stack
    SpecQueueSize:     Int = 64 // Size of the RAS speculative queue
) {
  require(isPow2(SpecQueueSize), "SpecSize must be a power of 2")
}

trait HasRasParameters extends HasBpuParameters {
  def rasParameters: RasParameters = bpuParameters.rasParameters

  def CommitStackSize: Int = rasParameters.CommitStackSize
  def SpecQueueSize:   Int = rasParameters.SpecQueueSize
  require(isPow2(SpecQueueSize), "SpecSize must be a power of 2")
  require(isPow2(CommitStackSize), "CommitStackSize must be a power of 2")

  // Address width used to index the committed stack.
  def CommitStackAddrWidth: Int = log2Up(CommitStackSize)

  // Width of the committed-stack occupancy counter (0..CommitStackSize).
  def CommitDepthWidth: Int = log2Up(CommitStackSize + 1)

  // Width of the stack pointers. Large enough that `ssp - nsp` (net in-flight) can be
  // sign-interpreted across the whole operating range without modular aliasing, so emptiness
  // detection needs no extra disambiguation bit. The only aliased endpoint (a fully saturated
  // +SpecQueueSize push window) always has its top inside the spec queue and is masked there.
  def StackPtrWidth: Int = log2Up(CommitStackSize + SpecQueueSize)

  // A single FTQ entry drives at most one RAS spec op (BPU S3), so the number of
  // outstanding speculative pushes is bounded by FtqSize. SpecQueueSize must be at
  // least FtqSize, otherwise the circular spec queue can wrap and overwrite entries
  // that are still live.
  require(SpecQueueSize >= FtqSize, s"SpecQueueSize ($SpecQueueSize) must be >= FtqSize ($FtqSize)")
}
