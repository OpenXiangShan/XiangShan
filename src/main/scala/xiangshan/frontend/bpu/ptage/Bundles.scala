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

package xiangshan.frontend.bpu.ptage

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.WriteReqBundle

object PtageBlock {

  /** Whether the entry itself supplies this block's target, and so knows where a following block would start.
    *
    * An indirect exit, return included, takes its target from somewhere else, so the next pc kept here is not where
    * control actually goes and cannot be used as the start of a second block.
    */
  def hasStaticTarget(taken: Bool, attribute: BranchAttribute): Bool = taken && !attribute.isIndirect
}

object PtageCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ptageParameters.CounterWidth
}

/** One block of a prediction group: where it leaves, and where it goes next. */
class PtageBlock(implicit p: Parameters) extends PtageBundle {
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val attribute:   BranchAttribute = new BranchAttribute
  val counter:     SaturateCounter = PtageCounter()
  // low bits of this block's next pc, read out and used directly as an index with no adder in the way
  val nextPcLow: UInt = UInt(NextPcLowWidth.W)

  def taken: Bool = counter.isPositive
}

/** One pTAGE entry holds a whole prediction group.
  *
  * Keyed by the previous group's start pc together with the folded path history, an entry answers "what comes after
  * that", which is what lets a single read produce both blocks. The second block's start is not stored, because it is
  * by construction the first block's next pc; training is what keeps that true, and any entry reached through a tag
  * alias is caught by the high-level predictor rather than by a check here.
  */
class PtageEntry(implicit p: Parameters) extends PtageBundle {
  val valid:  Bool = Bool()
  val tag:    UInt = UInt(TagWidth.W)
  val useful: Bool = Bool()

  val p1:      PtageBlock = new PtageBlock
  val p2:      PtageBlock = new PtageBlock
  val p2Valid: Bool       = Bool()
}

class BankReadReq(implicit p: Parameters) extends PtageBundle {
  val setIdx: UInt = UInt(SetIdxWidth.W)
}

class BankReadResp(implicit p: Parameters) extends PtageBundle {
  val entry: PtageEntry = new PtageEntry
}

class BankWriteReq(implicit p: Parameters) extends WriteReqBundle with HasPtageParameters {
  val setIdx: UInt       = UInt(SetIdxWidth.W)
  val entry:  PtageEntry = new PtageEntry

  override def tag: Option[UInt] = Some(entry.tag)
}
