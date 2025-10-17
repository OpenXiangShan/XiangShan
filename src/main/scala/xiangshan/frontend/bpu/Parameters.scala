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
import xiangshan.frontend.HasFrontendParameters
import xiangshan.frontend.bpu.abtb.AheadBtbParameters
import xiangshan.frontend.bpu.history.phr.PhrParameters
import xiangshan.frontend.bpu.ittage.IttageParameters
import xiangshan.frontend.bpu.mbtb.MainBtbParameters
import xiangshan.frontend.bpu.ras.RasParameters
import xiangshan.frontend.bpu.sc.ScParameters
import xiangshan.frontend.bpu.tage.TageParameters
import xiangshan.frontend.bpu.ubtb.MicroBtbParameters

// For users: these are default Bpu parameters set by dev, do not change them here,
// use top-level Parameters.scala instead.
case class BpuParameters(
    // general
    FetchBlockAlignSize: Option[Int] = None, // bytes, if None, use half-align (FetchBLockSize / 2) by default
    // debug
    EnableBpTrace: Boolean = false,
    // history
    phrParameters: PhrParameters = PhrParameters(),
    // sub predictors
    ubtbParameters:   MicroBtbParameters = MicroBtbParameters(),
    abtbParameters:   AheadBtbParameters = AheadBtbParameters(),
    mbtbParameters:   MainBtbParameters = MainBtbParameters(),
    tageParameters:   TageParameters = TageParameters(),
    scParameters:     ScParameters = ScParameters(),
    ittageParameters: IttageParameters = IttageParameters(),
    rasParameters:    RasParameters = RasParameters()
) {
  def NumBtbResultEntries: Int = mbtbParameters.NumWay * mbtbParameters.NumAlignBanks
}

trait HasBpuParameters extends HasFrontendParameters {
  def bpuParameters: BpuParameters = frontendParameters.bpuParameters

  def EnableBpTrace: Boolean = bpuParameters.EnableBpTrace

  // general
  def FetchBlockSizeWidth:    Int = log2Ceil(FetchBlockSize)
  def FetchBlockAlignSize:    Int = bpuParameters.FetchBlockAlignSize.getOrElse(FetchBlockSize / 2)
  def FetchBlockAlignWidth:   Int = log2Ceil(FetchBlockAlignSize)
  def FetchBlockAlignInstNum: Int = FetchBlockAlignSize / instBytes

  def PhrHistoryLength: Int = frontendParameters.getPhrHistoryLength

  def GhrHistoryLength: Int = frontendParameters.getGhrHistoryLength

  def NumBtbResultEntries: Int = bpuParameters.NumBtbResultEntries

  // phr history
  def AllFoldedHistoryInfo: Set[FoldedHistoryInfo] =
    bpuParameters.tageParameters.TableInfos.map {
      _.getFoldedHistoryInfoSet(bpuParameters.tageParameters.NumBanks, bpuParameters.tageParameters.TagWidth)
    }.reduce(_ ++ _) ++
      bpuParameters.ittageParameters.TableInfos.map {
        _.getFoldedHistoryInfoSet(bpuParameters.ittageParameters.TagWidth)
      }.reduce(_ ++ _) ++
      bpuParameters.scParameters.PathTableInfos.map {
        _.getFoldedHistoryInfoSet(NumBtbResultEntries, bpuParameters.scParameters.TagWidth)
      }.reduce(_ ++ _)

  // sanity check
  // should do this check in `case class BpuParameters` constructor, but we don't have access to `FetchBlockSize` there
  require(isPow2(FetchBlockAlignSize))
}
