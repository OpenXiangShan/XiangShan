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
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages.isC
import xiangshan.frontend.bpu.HalfAlignHelper

trait Helpers extends HasCommonHRParameters with HalfAlignHelper {
  def getNumShift(numLess: UInt, numHit: UInt, taken: Bool, isCond: Bool): UInt = {
    val numShift = MuxCase(
      Mux(numHit === 0.U, numHit, numHit - 1.U),
      Seq(
        (taken && isCond)  -> numLess,
        (taken && !isCond) -> Mux(numLess === 0.U, numLess, numLess - 1.U)
      )
    )
    numShift
  }
  def getNewGhr(oldGhr: UInt, numLess: UInt, numHit: UInt, taken: Bool, isCond: Bool)(histLen: Int): UInt = {
    val numShift = getNumShift(numLess, numHit, taken, isCond)
    Cat(oldGhr << numShift, taken && isCond)(histLen - 1, 0)
  }

  def getNewBW(oldBW: UInt, numLess: UInt, numHit: UInt, taken: Bool, isCond: Bool, isBW: Bool)(histLen: Int): UInt = {
    val numShift = getNumShift(numLess, numHit, taken, isCond)
    Cat(oldBW << numShift, taken && isCond && isBW)(histLen - 1, 0)
  }
}
