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
import utils.AddrField
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.TargetFixHelper
import xiangshan.frontend.bpu.history.phr.{Helpers => PhrPathHelpers}

trait Helpers extends HasPtageParameters with TargetFixHelper with PhrPathHelpers {
  val addrFields = AddrField(
    Seq(
      ("instOffset", instOffsetBits),
      ("bankIdx", BankIdxWidth),
      ("setIdx", SetIdxWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      // the top of the own-pc bits the tag folds, see getTagPc
      ("tag", instOffsetBits + BankIdxWidth + SetIdxWidth, TagWidth),
      ("nextPcLow", instOffsetBits, NextPcLowWidth)
    )
  )

  /** The bank, from the lowest pc bits mixed with two bits from inside the set field.
    *
    * The lowest bits alone are badly skewed: a group that starts on a fetch block boundary, or at a 4-byte
    * instruction, has them mostly zero, so most reads land in one bank, and a bank that serves most reads cannot drain
    * the training writes queued behind them. The mixed-in bits are also part of the set index, so a bank and a set
    * still name exactly one value of the pc bits they cover, and no capacity is lost.
    */
  private def bankMixLo: Int = instOffsetBits + BankIdxWidth + SetIdxWidth / 2
  require(SetIdxWidth / 2 + BankIdxWidth <= SetIdxWidth, "the bank's mixed-in bits must lie inside the set field")
  def getBankIndex(pc: PrunedAddr): UInt =
    addrFields.extract("bankIdx", pc) ^ pc(bankMixLo + BankIdxWidth - 1, bankMixLo)

  // The pc half of a set index. The folded history supplies the other half, and the two are XORed rather than
  // concatenated so that a table's whole index depends on its own history span.
  def getSetIndexPc(pc: PrunedAddr): UInt = addrFields.extract("setIdx", pc)

  /** The pc half of a tag, from the group's own start pc folded to the tag width.
    *
    * The index is keyed on the group before this one, so no bit of this group's own pc reaches it. Every bit below
    * the tag field therefore has to reach the tag instead: otherwise two groups that follow the same key a few
    * instructions apart -- a branch's target and its fall-through -- share one entry, and each reads the other's exit.
    */
  def getTagPc(pc: PrunedAddr): UInt = {
    val bits = pc(addrFields.getEnd("tag"), instOffsetBits)
    (0 until bits.getWidth by TagWidth).map(i => bits(scala.math.min(i + TagWidth, bits.getWidth) - 1, i)).reduce(_ ^ _)
  }

  def getEntryNextPc(nextPc: PrunedAddr): UInt = addrFields.extract("nextPcLow", nextPc)

  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, addrFields.getEnd("nextPcLow") + 1)
}
