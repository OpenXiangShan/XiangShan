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
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
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
  // and deriving them from the blocks' contents there would not make timing. It is a full path hash per block rather
  // than just the shifted-in bits, because that is what Phr applies and what FastPhr therefore has to mirror.
  val phrToken: UInt = UInt(TokenWidth.W)
  val ghrShamt: UInt = UInt(GhrShamtWidth.W)
}

/** One decoded block of a pTAGE prediction group, with its target already reconstructed. */
class PtageBlockPrediction(implicit p: Parameters) extends PtageBundle {
  val taken:       Bool       = Bool()
  val cfiPosition: UInt       = UInt(CfiPositionWidth.W)
  val attribute:   UInt       = PtageAttribute()
  val target:      PrunedAddr = PrunedAddr(VAddrBits)
}

/** pTAGE's answer for one prediction cycle: one block, two on a 2-taken hit, or none at all. */
class PtagePrediction(implicit p: Parameters) extends PtageBundle {
  val blocks: Vec[Valid[PtageBlockPrediction]] = Vec(MaxPredictionNum, Valid(new PtageBlockPrediction))

  def hit:       Bool = blocks.head.valid
  def numBlocks: UInt = PopCount(blocks.map(_.valid))
}

/** What training needs to know about the lookup that produced a prediction.
  *
  * Carried down the predictor pipeline rather than recomputed, because pTAGE indexes on the *previous* group's start
  * pc and history: re-deriving an index at training time would have to reproduce a context that has since moved on,
  * and any drift would train a different entry than the one that predicted.
  */
class PtageMeta(implicit p: Parameters) extends PtageBundle {
  val setIdx:  Vec[UInt] = Vec(NumTables, UInt(SetIdxWidth.W))
  val tag:     Vec[UInt] = Vec(NumTables, UInt(TagWidth.W))
  val bankIdx: UInt      = UInt(BankIdxWidth.W)
  val hitVec:  Vec[Bool] = Vec(NumTables, Bool())
  // the hit entries' useful bits, used to pick an allocation victim; may be stale, which costs allocation quality
  // but never correctness
  val usefulVec: Vec[Bool]   = Vec(NumTables, Bool())
  val provider:  Valid[UInt] = Valid(UInt(log2Ceil(NumTables).W))
  val alt:       Valid[UInt] = Valid(UInt(log2Ceil(NumTables).W))
  // the provider entry as it was read, so training need not read the table again to know what it is correcting
  val p1Counter:     SaturateCounter = PtageCounter()
  val p2Counter:     SaturateCounter = PtageCounter()
  val p1CfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val p1Attribute:   UInt            = PtageAttribute()
  val p2Valid:       Bool            = Bool()
  // The first group after a redirect was indexed with the history as it stood before the correction landed, so it
  // belongs to no entry and must not be trained. This is the warm-up that indexing a group ahead costs.
  val noAnchor: Bool = Bool()
}

/** A verified group, held back one training event so the group that follows it can become its second block.
  *
  * This is what lets a pair be learned without ever having been predicted as one, which matters because otherwise
  * second blocks could only ever be learned where they already existed.
  */
class PtagePendingGroup(implicit p: Parameters) extends PtageBundle {
  val meta:        PtageMeta  = new PtageMeta
  val cfiPosition: UInt       = UInt(CfiPositionWidth.W)
  val attribute:   UInt       = PtageAttribute()
  val nextPcLow:   UInt       = UInt(NextPcLowWidth.W)
  val nextPc:      PrunedAddr = PrunedAddr(VAddrBits)
  val taken:       Bool       = Bool()
  // this block's contribution to the path history, kept so the entry can carry a whole group's contribution
  val pathHash: UInt = UInt(PathHashWidth.W)
}

/** A pending write to one table's bank, registered so the decision and the write land in different cycles. */
class PtageTrainWrite(implicit p: Parameters) extends PtageBundle {
  val table:  UInt       = UInt(log2Ceil(NumTables).W)
  val bank:   UInt       = UInt(BankIdxWidth.W)
  val setIdx: UInt       = UInt(SetIdxWidth.W)
  val entry:  PtageEntry = new PtageEntry
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
