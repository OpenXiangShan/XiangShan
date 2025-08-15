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

package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.bpu.BpuParameters
import xiangshan.frontend.ftq.FtqParameters
import xiangshan.frontend.icache.ICacheParameters

case class FrontendParameters(
    FetchBlockSize: Int = 32, // bytes // FIXME: 64B, waiting for ftq/icache support

    bpuParameters:    BpuParameters = BpuParameters(),
    ftqParameters:    FtqParameters = FtqParameters(),
    icacheParameters: ICacheParameters = ICacheParameters()
) {
  // according to style guide, this should be in `trait HasBpuParameters` and named `PhrHistoryLength`,
  // but, we need to use this value in `class PhrPtr` definition, so we cannot put it in a trait.
  def getPhrHistoryLength: Int = {
    def nextMultipleOf(number: Int, factor: Int): Int = (number + factor - 1) / factor * factor

    // TODO: ittage table history length
//    def MaxTableHistoryLength: Int = (
//      bpuParameters.tageParameters.TableInfos.map(_.HistoryLength) ++
//        bpuParameters.ittageParameters.TableInfos.map(_.HistoryLength)
//    ).max
    def MaxTableHistoryLength: Int = bpuParameters.tageParameters.TableInfos.map(_.HistoryLength).max

    def Shamt:   Int = bpuParameters.phrParameters.Shamt
    def FtqSize: Int = ftqParameters.FtqSize

    // when ftq is full, Phr can overflow, so we need some extra bits to save Phr overflow bits for error-recovery
    // magic, don't touch
    def FtqFullFix:   Int = 4
    def HistoryAlign: Int = bpuParameters.phrParameters.HistoryAlign

    nextMultipleOf(MaxTableHistoryLength + Shamt * FtqSize + FtqFullFix, HistoryAlign)
  }

  // sanity check
  require(isPow2(FetchBlockSize))
}

trait HasFrontendParameters extends HasXSParameter {
  def frontendParameters: FrontendParameters = coreParams.frontendParameters

  def FetchBlockSize:    Int = frontendParameters.FetchBlockSize
  def FetchBlockInstNum: Int = FetchBlockSize / instBytes
  def CfiPositionWidth:  Int = log2Ceil(FetchBlockInstNum) // 2/4B(inst) aligned
}
