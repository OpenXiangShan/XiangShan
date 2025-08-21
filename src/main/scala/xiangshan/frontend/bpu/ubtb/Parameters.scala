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

package xiangshan.frontend.bpu.ubtb

import xiangshan.frontend.bpu.HasBpuParameters

// default MicroBtb parameters, do not change here, use top-level xiangshan/Parameters.scala
case class MicroBtbParameters(
    NumEntries:     Int = 32,
    TagWidth:       Int = 22,
    TargetWidth:    Int = 22, // 2B aligned
    UsefulCntWidth: Int = 2,
    TakenCntWidth:  Int = 2,
    Replacer:       String = "plru",
    // use s3 prediction to train ubtb
    EnableFastTrain: Boolean = false,
    // enable carry and borrow fix for target, so jumps around 2^(TargetWidth+1) boundary will not cause misprediction
    // mainBtb should handle this case, so performance affect should be slight, and, bad for timing
    EnableTargetFix: Boolean = false
) {}

trait HasMicroBtbParameters extends HasBpuParameters {
  def ubtbParameters: MicroBtbParameters = bpuParameters.ubtbParameters

  def NumEntries:     Int    = ubtbParameters.NumEntries
  def TagWidth:       Int    = ubtbParameters.TagWidth
  def TargetWidth:    Int    = ubtbParameters.TargetWidth
  def UsefulCntWidth: Int    = ubtbParameters.UsefulCntWidth
  def TakenCntWidth:  Int    = ubtbParameters.TakenCntWidth
  def Replacer:       String = ubtbParameters.Replacer

  def EnableFastTrain: Boolean = ubtbParameters.EnableFastTrain
  def EnableTargetFix: Boolean = ubtbParameters.EnableTargetFix
}
