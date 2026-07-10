// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend

import chisel3._
import chisel3.util._
import ftq.FtqPrefetchReq
import icache.MetaInfo
import icache.PrefetchReqBundle
import utils.EnumUInt

// As 2-prefetch / 2-fetch support is spread across Ftq/Ifu/ICache,
// we put these classes / objects in package frontend instead of package icache.
// Logically, this file is part of Bundles.scala, we split it out for easier management.

class TwoPrefetchCase extends Bundle {
  val value: UInt = TwoPrefetchCase.Value()

  def valid: Bool = !isConflict

  // select 2 vaddr to read ICacheMetaArray
  def selectMetaVAddr(reqVec: Vec[PrefetchReqBundle]): Vec[PrunedAddr] =
    MuxCase(
      // unable to do 2-prefetch, or isSameLine or isOverlap1, both use req1's start and nextLine
      VecInit(reqVec(0).startVAddr, reqVec(0).nextLineVAddr),
      Seq(
        isOverlap2   -> VecInit(reqVec(1).startVAddr, reqVec(1).nextLineVAddr),
        isInterleave -> VecInit(reqVec(0).startVAddr, reqVec(1).startVAddr)
      )
    )

  // select isCrossLine flag to read ICacheMetaArray
  def selectIsCrossLine(reqVec: Vec[PrefetchReqBundle]): Bool =
    MuxCase(
      // unable to do 2-prefetch, use req1.isCrossLine
      reqVec(0).isCrossLine,
      Seq(
        // if 2 fb are in the same line, read 2 if one of them crosses cacheline
        isSameLine -> (reqVec(0).isCrossLine || reqVec(1).isCrossLine),
        // otherwise, we must read 2 cacheline
        (isOverlap1 || isOverlap2 || isInterleave) -> true.B
      )
    )

  // NOTE: refer to object TwoPrefetchCase.Value for explanation
  def isConflict:   Bool = !value.orR
  def isSameLine:   Bool = value(0)
  def isOverlap1:   Bool = value(1)
  def isOverlap2:   Bool = value(2)
  def isInterleave: Bool = value(3)

  // after read 2 (at most) metaInfo from ICacheMetaArray, broadcast to 2 fetch blocks
  def generateReqMetaInfo(readInfoVec: Vec[MetaInfo]): Vec[Vec[MetaInfo]] =
    VecInit(
      VecInit(
        // if isOverlap2, fb1's first line is fb2's second line and is from port 2, otherwise from port 1
        Mux(isOverlap2, readInfoVec(1), readInfoVec(0)),
        // if isOverlap2 or isInterleave, fb1 does not have a second line, otherwise from port 1
        Mux(isOverlap2 || isInterleave, 0.U.asTypeOf(readInfoVec(0)), readInfoVec(1))
      ),
      VecInit(
        // if isSameLine or isOverlap2, fb2's first line is from port 1, otherwise from port 2
        Mux(isSameLine || isOverlap2, readInfoVec(0), readInfoVec(1)),
        // if isOverlap1 or isInterleave, fb2 does not have a second line, otherwise from port 2
        Mux(isOverlap1 || isInterleave, 0.U.asTypeOf(readInfoVec(0)), readInfoVec(1))
      )
    )

  def getValidSeq: Seq[(String, Bool)] = TwoPrefetchCase.Value.getValidSeq(value, exclude = Set("Conflict"))
}

object TwoPrefetchCase {
  def Conflict: TwoPrefetchCase = apply(Value.Conflict)

  def apply(that: UInt, canAssert: Bool = true.B): TwoPrefetchCase = {
    when(canAssert) {
      Value.assertLegal(that)
    }
    val twoPrefetchCase = Wire(new TwoPrefetchCase)
    twoPrefetchCase.value := that
    twoPrefetchCase
  }

  def SameLine: TwoPrefetchCase = apply(Value.SameLine)

  def Overlap1: TwoPrefetchCase = apply(Value.Overlap1)

  def Overlap2: TwoPrefetchCase = apply(Value.Overlap2)

  def Interleave: TwoPrefetchCase = apply(Value.Interleave)

  def apply(reqVec: Vec[FtqPrefetchReq], canAssert: Bool): TwoPrefetchCase =
    TwoPrefetchCase(
      reqVec(0).vSetIdx(0) === reqVec(1).vSetIdx(0),                                                    // sameLine
      reqVec(0).isCrossLine && !reqVec(1).isCrossLine && reqVec(0).vSetIdx(1) === reqVec(1).vSetIdx(0), // overlap1
      !reqVec(0).isCrossLine && reqVec(1).isCrossLine && reqVec(1).vSetIdx(1) === reqVec(0).vSetIdx(0), // overlap2
      !reqVec(0).isCrossLine && !reqVec(1).isCrossLine && reqVec(0).vSetIdx(0)(0) =/= reqVec(1).vSetIdx(0)(0), // inter
      canAssert
    )

  def apply(sameLine: Bool, overlap1: Bool, overlap2: Bool, interleave: Bool, canAssert: Bool): TwoPrefetchCase =
    apply(VecInit(sameLine, overlap1, overlap2, interleave).asUInt, canAssert)

  private object Value extends EnumUInt(5, useOneHot = true, allowZeroForOneHot = true) {
    // Conflict: cannot do 2-prefetch due to SRAM read port conflict
    def Conflict: UInt = 0.U(width.W)

    /* SameLine: 2 fetch block in the same cacheline(s)
     * |    cacheline0    |    cacheline1    |
     *      |  fb1             |
     *          | fb2 |
     */
    def SameLine: UInt = 1.U(width.W)

    /* Overlap1: fb2 is in fb1's next line
     * |    cacheline0    |    cacheline1    |
     *      |  fb1             |
     *                       | fb2 |
     *                       | fb2                | // bad: fb2 cannot cross cacheline
     */
    def Overlap1: UInt = 2.U(width.W)

    /* Overlap2: reverse of Overlap1, i.e. fb1 is in fb2's next line
     */
    def Overlap2: UInt = 4.U(width.W)

    /* Interleave: 2 fetch block in interleaved cachelines
     *  |    cacheline(2n)    | ... |    cacheline(2n+1)    |
     *        | fb1 |
     *                                   | fb2 |
     *                                   | fb2                 | // bad: both fb1 and fb2 cannot cross cacheline
     */
    def Interleave: UInt = 8.U(width.W)
  }
}
