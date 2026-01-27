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

package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu._

class PrefetchBtbEntry(implicit p: Parameters) extends PrefetchBtbBundle {
  val valid:    Bool                 = Bool()
  val victim:   Bool                 = Bool()
  val sramData: PrefetchBtbSramEntry = new PrefetchBtbSramEntry
}
class PrefetchBtbSramEntry(implicit p: Parameters) extends PrefetchBtbBundle {
  val tag: UInt = UInt(TagWidth.W)
  val target = UInt(TargetWidth.W)
  val attribute: BranchAttribute = new BranchAttribute
  val position:  UInt            = UInt(CfiPositionWidth.W)
}
class PrefetchBtbMetaEntry(implicit p: Parameters) extends PrefetchBtbBundle {
  val rawHit:    Bool            = Bool()
  val position:  UInt            = UInt(CfiPositionWidth.W)
  val attribute: BranchAttribute = new BranchAttribute

  def hit(branch: BranchInfo): Bool = rawHit && position === branch.cfiPosition
}

class PrefetchBtbMeta(implicit p: Parameters) extends PrefetchBtbBundle {
  val entries: Vec[PrefetchBtbMetaEntry] = Vec(NumWay, new PrefetchBtbMetaEntry)
}
//Prefetch pipe write at most 4 entry
class PrefetchWriteReq(implicit p: Parameters) extends PrefetchBtbBundle {
  val setIdx:  UInt                           = UInt(SetIdxLen.W)
  val bankIdx: UInt                           = UInt(BankIdxLen.W)
  val entries: Vec[ValidIO[PrefetchBtbEntry]] = Vec(NumWay, Valid(new PrefetchBtbEntry()))
}
//Victim pipe only write one entry
class VictimWriteReq(implicit p: Parameters) extends PrefetchBtbBundle {
  val setIdx:  UInt             = UInt(SetIdxLen.W)
  val bankIdx: UInt             = UInt(BankIdxLen.W)
  val entries: PrefetchBtbEntry = new PrefetchBtbEntry()
}
class PredReadReq(implicit p: Parameters) extends PrefetchBtbBundle {
  val setIdx:  UInt = UInt(SetIdxLen.W)
  val bankIdx: UInt = UInt(BankIdxLen.W)
}
class PredReadResp(implicit p: Parameters) extends PrefetchBtbBundle {
  val entries: Vec[PrefetchBtbEntry] = Vec(NumWay, new PrefetchBtbEntry())
}
class TrainWriteReq(implicit p: Parameters) extends PrefetchBtbBundle {
  val setIdx:      UInt      = UInt(SetIdxLen.W)
  val needInvalid: Vec[Bool] = Vec(NumWay, Bool())
}

class BankReadReq(implicit p: Parameters) extends PrefetchBtbBundle {
  val setIdx: UInt = UInt(SetIdxLen.W)
}

class BankReadResp(implicit p: Parameters) extends PrefetchBtbBundle {
  val entries: Vec[PrefetchBtbEntry] = Vec(NumWay, new PrefetchBtbEntry)
}

class BankWriteReq(implicit p: Parameters) extends WriteReqBundle with HasPrefetchBtbParameters {
  val setIdx:  UInt                  = UInt(SetIdxLen.W)
  val wayMask: UInt                  = UInt(NumWay.W)
  val entry:   Vec[PrefetchBtbEntry] = Vec(NumWay, new PrefetchBtbEntry())
}
class WbufWriteReq(implicit p: Parameters) extends WriteReqBundle with HasPrefetchBtbParameters {
  val setIdx:  UInt             = UInt(SetIdxLen.W)
  val wayMask: UInt             = UInt(NumWay.W)
  val entry:   PrefetchBtbEntry = new PrefetchBtbEntry()
}
