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
import utils.EnumUInt
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.WriteReqBundle

/** What pTAGE knows about how a block leaves.
  *
  * Only Conditional and Direct name a target that the entry itself can supply, so only those can be followed by a
  * second block in the same group. Deferred covers indirect, call and return exits as well as targets too far away to
  * fit in the stored low bits: pTAGE still predicts such a block, but its successor is left to the fallback predictor
  * and the high-level predictor.
  */
object PtageAttribute extends EnumUInt(4) {
  def FallThrough: UInt = 0.U(width.W) // no cfi in the block, it runs to the end
  def Conditional: UInt = 1.U(width.W)
  def Direct:      UInt = 2.U(width.W) // unconditional direct jump
  def Deferred:    UInt = 3.U(width.W)

  // a block whose stored next pc is the real one, and so can be followed by a second block
  def hasStaticTarget(attribute: UInt): Bool =
    attribute === Conditional || attribute === Direct
}

object PtageCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ptageParameters.CounterWidth
}

/** One block of a prediction group: where it leaves, and where it goes next. */
class PtageBlock(implicit p: Parameters) extends PtageBundle {
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val attribute:   UInt            = PtageAttribute()
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

  // The group's effect on the path history, worked out at training time. Keeping it in the entry is what holds the
  // single-cycle prediction loop together: the history advance needs the new bits in the same cycle the entry is read,
  // and deriving them from the blocks' contents there would not make timing.
  val phrToken: UInt = UInt(MaxUpdateNum.W)
  val ghrShamt: UInt = UInt(GhrShamtWidth.W)
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
