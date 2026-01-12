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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.bpu.SaturateCounterFactory

object TakenCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.utageParameters.TakenCtrWidth
}

object UsefulCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.utageParameters.UsefulWidth
}

class MicroTagePrediction(implicit p: Parameters) extends MicroTageBundle {
  // val ubtbHit:   Bool = Bool()
  // val ubtbTaken: Bool = Bool()
  val takenVec: Vec[Bool] = Vec(NumAheadBtbPredictionEntries, Bool())
  val hitVec:   Vec[Bool] = Vec(NumAheadBtbPredictionEntries, Bool())
}

class MicroTageMeta(implicit p: Parameters) extends MicroTageBundle {
  val abtbResult: Vec[AbtbResult] = Vec(NumAheadBtbPredictionEntries, new AbtbResult)
}

class MicroTageDebug(implicit p: Parameters) extends MicroTageBundle {
  val debug_idx:      UInt = UInt(log2Ceil(MaxNumSets).W)
  val debug_tag:      UInt = UInt(MaxTagLen.W)
  val debug_tableId:  UInt = UInt(log2Ceil(NumTables).W)
  val debug_useful:   UInt = UInt(UsefulWidth.W)
  val debug_takenCtr: UInt = UInt(TakenCtrWidth.W)
}

class AbtbResult(implicit p: Parameters) extends MicroTageBundle {
  val valid:            Bool = Bool()
  val baseTaken:        Bool = Bool()
  val baseIsStrongBias: Bool = Bool()
  val hit:              Bool = Bool()
  val predTaken:        Bool = Bool()
  val tableId:          UInt = UInt(log2Ceil(NumTables).W)
  val wayId:            UInt = UInt(log2Ceil(NumWays).W)
  val cfiPosition:      UInt = UInt(CfiPositionWidth.W)
}

class MicroTageTablePred(implicit p: Parameters) extends MicroTageBundle {
  val taken:       Bool = Bool()
  val cfiPosition: UInt = UInt(CfiPositionWidth.W)
}

class MicroTageTrainResult(implicit p: Parameters) extends MicroTageBundle {
  val valid:            Bool = Bool()
  val hit:              Bool = Bool()
  val baseTaken:        Bool = Bool()
  val actualTaken:      Bool = Bool()
  val predTaken:        Bool = Bool()
  val baseIsStrongBias: Bool = Bool()
  val cfiPosition:      UInt = UInt(CfiPositionWidth.W)
  val tableId:          UInt = UInt(log2Ceil(NumTables).W)
  val wayId:            UInt = UInt(log2Ceil(NumWays).W)
}

class TraceBranch(implicit p: Parameters) extends MicroTageBundle {
  val valid:            Bool = Bool()
  val misPred:          Bool = Bool()
  val hit:              Bool = Bool()
  val baseTaken:        Bool = Bool()
  val actualTaken:      Bool = Bool()
  val predTaken:        Bool = Bool()
  val baseIsStrongBias: Bool = Bool()
  val cfiPosition:      UInt = UInt(CfiPositionWidth.W)
  val tableId:          UInt = UInt(log2Ceil(NumTables).W)
  val wayId:            UInt = UInt(log2Ceil(NumWays).W)
}

class MicroTageTrace(implicit p: Parameters) extends MicroTageBundle {
  val startVAddr:     UInt             = UInt(VAddrBits.W)
  val hasHitMisPred:  Bool             = Bool()
  val missHitMisPred: Bool             = Bool()
  val branches:       Vec[TraceBranch] = Vec(NumAheadBtbPredictionEntries, new TraceBranch)
  val setIdx:         Vec[UInt]        = Vec(NumTables, UInt(log2Ceil(MaxNumSets).W))
}
