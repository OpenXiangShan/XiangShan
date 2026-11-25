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

import chisel3._
import chisel3.util.Valid
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.Pc
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.Prediction

class CommonHREntry(implicit p: Parameters) extends CommonHRBundle {
  val valid: Bool = Bool()
  val ghr:   UInt = UInt(GhrHistoryLength.W)
  val bw:    UInt = UInt(BWHistoryLength.W)

  val predStartPc: Option[Pc] = Some(Pc()) // for debug
}

class Update(implicit p: Parameters) extends CommonHRBundle {
  val isOverride: Bool       = Bool()
  val startPc:    Pc         = Pc()
  val prediction: Prediction = new Prediction()
}

class S1Update(implicit p: Parameters) extends CommonHRBundle {
  val startPc:               Pc          = Pc()
  val prediction:            Prediction  = new Prediction
  val condHitMask:           Vec[Bool]   = Vec(NumAheadBtbPredictionEntries, Bool())
  val positions:             Vec[UInt]   = Vec(NumAheadBtbPredictionEntries, UInt(CfiPositionWidth.W))
  val ubtbNumNotTakenBefore: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
  val abtbBlockNtCount:      Valid[UInt] = Valid(UInt(log2Ceil(NumBtbResultEntries + 1).W))
}

class S2Update(implicit p: Parameters) extends Update {
  val condHitMask: Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val positions:   Vec[UInt] = Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W))
  val targets:     Vec[Pc]   = Vec(NumBtbResultEntries, Pc())
}

class S3Update(implicit p: Parameters) extends Update {
  val firstTakenBranchOH: Vec[Bool] = Vec(NumBtbResultEntries, Bool())
}

class CommonHRResolveMeta(implicit p: Parameters) extends CommonHRBundle {
  val valid: Bool = Bool() // commonHR valid bit
  val ghr:   UInt = UInt(GhrHistoryLength.W)
  val bw:    UInt = UInt(BWHistoryLength.W)

  val imli: UInt = UInt(ImliHistoryLength.W)
}
class CommonHRMeta(implicit p: Parameters) extends CommonHRBundle {
  val ghr:       UInt                 = UInt(GhrHistoryLength.W)
  val bw:        UInt                 = UInt(BWHistoryLength.W)
  val imli:      UInt                 = UInt(ImliHistoryLength.W)
  val hitMask:   Vec[Bool]            = Vec(NumBtbResultEntries, Bool())
  val attribute: Vec[BranchAttribute] = Vec(NumBtbResultEntries, new BranchAttribute)
  val position:  Vec[UInt]            = Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W))
}

class CommonHRRedirect(implicit p: Parameters) extends CommonHRBundle {
  val valid:     Bool            = Bool()
  val cfiPc:     Pc              = Pc()
  val taken:     Bool            = Bool()
  val attribute: BranchAttribute = new BranchAttribute
  val target:    Pc              = Pc()
  val meta:      CommonHRMeta    = new CommonHRMeta
}
