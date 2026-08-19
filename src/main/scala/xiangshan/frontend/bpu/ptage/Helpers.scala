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

trait Helpers extends HasPtageParameters with TargetFixHelper {
  val addrFields = AddrField(
    Seq(
      ("instOffset", instOffsetBits),
      ("bankIdx", BankIdxWidth),
      ("setIdx", SetIdxWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      // the tag takes pc bits above the set index, so that it adds information the index does not already carry
      ("tag", instOffsetBits + BankIdxWidth + SetIdxWidth, TagWidth),
      ("nextPcLow", instOffsetBits, NextPcLowWidth)
    )
  )

  def getBankIndex(pc: PrunedAddr): UInt = addrFields.extract("bankIdx", pc)

  // The pc half of a set index. The folded history supplies the other half, and the two are XORed rather than
  // concatenated so that a table's whole index depends on its own history span.
  def getSetIndexPc(pc: PrunedAddr): UInt = addrFields.extract("setIdx", pc)

  def getTagPc(pc: PrunedAddr): UInt = addrFields.extract("tag", pc)

  def getEntryNextPc(nextPc: PrunedAddr): UInt = addrFields.extract("nextPcLow", nextPc)

  def getTargetUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, addrFields.getEnd("nextPcLow") + 1)
}
