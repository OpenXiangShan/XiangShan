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
import xiangshan.frontend.bpu.SaturateCounter

class MicroTagePrediction(implicit p: Parameters) extends MicroTageBundle {
  val taken:       Bool      = Bool()
  val cfiPosition: UInt      = UInt(CfiPositionWidth.W)
  val hitAbtbVec:  Vec[Bool] = Vec(NumAbtbResultEntries, Bool())
}

class MicroTageMeta(implicit p: Parameters) extends MicroTageBundle {
  val histTableHitMap:         Vec[Bool] = Vec(NumTables, Bool())
  val histTableTakenMap:       Vec[Bool] = Vec(NumTables, Bool())
  val histTableUsefulVec:      Vec[UInt] = Vec(NumTables, UInt(UsefulWidth.W))
  val histTableCfiPositionVec: Vec[UInt] = Vec(NumTables, UInt(CfiPositionWidth.W))
  val baseTaken:               Bool      = Bool()
  val baseCfiPosition:         UInt      = UInt(CfiPositionWidth.W)

  val finalTaken:       Bool = Bool()
  val finalCfiPosition: UInt = UInt(CfiPositionWidth.W)
  val finalIsBr:        Bool = Bool()
  val hasOverride:      Bool = Bool()
  // only for test and debug
  val debug_startVAddr:   Option[UInt] = Option.when(EnableTraceAndDebug)(UInt(VAddrBits.W))
  val debug_useMicroTage: Option[Bool] = Option.when(EnableTraceAndDebug)(Bool())
  val debug_predIdx0:     Option[UInt] = Option.when(EnableTraceAndDebug)(UInt(DebugPredIdxWidth.W))
  val debug_predTag0:     Option[UInt] = Option.when(EnableTraceAndDebug)(UInt(DebugPredTagWidth.W))
}

class MicroTageTrace(implicit p: Parameters) extends MicroTageBundle {
  val startVAddr:  UInt = UInt(VAddrBits.W)
  val branchPc:    UInt = UInt(VAddrBits.W)
  val cfiPosition: UInt = UInt(CfiPositionWidth.W)
  val hit:         Bool = Bool()
  val misPred:     Bool = Bool()
  val actualTaken: Bool = Bool()
  val tableId:     UInt = UInt(log2Ceil(NumTables).W)
  val setIdx:      UInt = UInt(log2Ceil(MaxNumSets).W)
  val multiHit:    Bool = Bool()
  val oldUseful:   UInt = UInt(UsefulWidth.W)
  val oldTakenCtr: UInt = UInt(TakenCtrWidth.W)
  val needUpdate:  Bool = Bool()
  val needAlloc:   Bool = Bool()
  val allocFailed: Bool = Bool()
}

class MicroTageDebug(implicit p: Parameters) extends MicroTageBundle {
  val debug_idx:      UInt = UInt(log2Ceil(MaxNumSets).W)
  val debug_tag:      UInt = UInt(MaxTagLen.W)
  val debug_tableId:  UInt = UInt(log2Ceil(NumTables).W)
  val debug_useful:   UInt = UInt(UsefulWidth.W)
  val debug_takenCtr: UInt = UInt(TakenCtrWidth.W)
}
