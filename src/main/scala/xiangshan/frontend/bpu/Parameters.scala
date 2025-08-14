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

import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.bpu.abtb.AheadBtbParameters
import xiangshan.frontend.bpu.mbtb.MainBtbParameters
import xiangshan.frontend.bpu.phr.PhrParameters
import xiangshan.frontend.bpu.ras.RasParameters
import xiangshan.frontend.bpu.sc.ScParameters
import xiangshan.frontend.bpu.tage.TageParameters
import xiangshan.frontend.bpu.ubtb.MicroBtbParameters

// For users: these are default Bpu parameters set by dev, do not change them here,
// use top-level Parameters.scala instead.
case class BpuParameters(
    // general
    FetchBlockSize:      Int = 32,           // bytes // FIXME: 64B, waiting for ftq/icache support
    FetchBlockAlignSize: Option[Int] = None, // bytes, if None, use half-align (FetchBLockSize / 2) by default
    phrParameters:       PhrParameters = PhrParameters(),
    // sub predictors
    ubtbParameters: MicroBtbParameters = MicroBtbParameters(),
    abtbParameters: AheadBtbParameters = AheadBtbParameters(),
    mbtbParameters: MainBtbParameters = MainBtbParameters(),
    tageParameters: TageParameters = TageParameters(),
    scParameters:   ScParameters = ScParameters(),
    rasParameters:  RasParameters = RasParameters()
) {
  // according to style guide, this should be in `trait HasBpuParameters` and named `PhrHistoryLength`,
  // but, we need to use this value in `class PhrPtr` definition, so we cannot put it in a trait.
  def getPhrHistoryLength(ftqSize: Int): Int = {
    def nextMultipleOf(number: Int, factor: Int): Int = (number + factor - 1) / factor * factor

    // TODO: ittage table history length
    //    def MaxTableHistoryLength: Int = (
    //      bpuParameters.tageParameters.TableInfos.map(_.HistoryLength) ++
    //        bpuParameters.ittageParameters.TableInfos.map(_.HistoryLength)
    //    ).max
    def MaxTableHistoryLength: Int = tageParameters.TableInfos.map(_.HistoryLength).max

    def Shamt: Int = phrParameters.Shamt

    // when ftq is full, Phr can overflow, so we need some extra bits to save Phr overflow bits for error-recovery
    // magic, don't touch
    def FtqFullFix:   Int = 4
    def HistoryAlign: Int = phrParameters.HistoryAlign

    nextMultipleOf(MaxTableHistoryLength + Shamt * ftqSize + FtqFullFix, HistoryAlign)
  }

  // sanity check
  require(isPow2(FetchBlockSize))
  require(isPow2(FetchBlockAlignSize.getOrElse(FetchBlockSize / 2)))
}

trait HasBpuParameters extends HasXSParameter {
  def bpuParameters: BpuParameters = coreParams.bpuParameters

  // general
  def FetchBlockSize:         Int = bpuParameters.FetchBlockSize
  def FetchBlockSizeWidth:    Int = log2Ceil(FetchBlockSize)
  def FetchBlockAlignSize:    Int = bpuParameters.FetchBlockAlignSize.getOrElse(FetchBlockSize / 2)
  def FetchBlockAlignWidth:   Int = log2Ceil(FetchBlockAlignSize)
  def FetchBlockInstNum:      Int = FetchBlockSize / instBytes
  def FetchBlockAlignInstNum: Int = FetchBlockAlignSize / instBytes

  def CfiPositionWidth: Int = log2Ceil(FetchBlockInstNum) // 2/4B(inst) aligned

  def PhrHistoryLength: Int = bpuParameters.getPhrHistoryLength(FtqSize)

  // phr history
  // TODO: add ittage info
//    def AllFoldedHistoryInfo: Set[FoldedHistoryInfo] =
//      (bpuParameters.tageParameters.TableInfos ++
//        bpuParameters.ittageParameters.TableInfos).map {
//        _.getFoldedHistoryInfoSet(bpuParameters.tageParameters.TagWidth)
//      }.reduce(_ ++ _)
  def AllFoldedHistoryInfo: Set[FoldedHistoryInfo] =
    bpuParameters.tageParameters.TableInfos.map {
      _.getFoldedHistoryInfoSet(bpuParameters.tageParameters.TagWidth)
    }.reduce(_ ++ _)

  // sanity check
  // should do this check in `case class BpuParameters` constructor, but we don't have access to `FetchBlockSize` there
  require(isPow2(FetchBlockAlignSize))
}
