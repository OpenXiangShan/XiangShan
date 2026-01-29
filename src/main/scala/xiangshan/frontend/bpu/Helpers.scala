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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import scala.math.min
import utility.ParallelXOR
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

trait HalfAlignHelper extends HasBpuParameters {
  def getAlignedPcUpper(pc: PrunedAddr): UInt =
    pc(pc.length - 1, FetchBlockAlignWidth)

  def getAlignedPc(pc: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getAlignedPcUpper(pc),
      0.U(FetchBlockAlignWidth.W)
    ))

  def getNextAlignedPc(pc: PrunedAddr): PrunedAddr = {
    val nextAlignedPcUpperBits = getAlignedPcUpper(pc) + 1.U
    PrunedAddrInit(Cat(nextAlignedPcUpperBits, 0.U(FetchBlockAlignWidth.W)))
  }

  def getCfiPcFromPosition(startPc: PrunedAddr, position: UInt): PrunedAddr = {
    val cfiPcUpperBits = startPc(startPc.length - 1, FetchBlockAlignWidth) + position(CfiPositionWidth - 1)
    PrunedAddrInit(Cat(cfiPcUpperBits, position(CfiPositionWidth - 2, 0), 0.U(instOffsetBits.W)))
  }

  def getCfiPcFromOffset(startPc: PrunedAddr, ftqOffset: UInt): PrunedAddr =
    startPc + (ftqOffset << instOffsetBits.U).asUInt

  def getRealCfiPcFromOffset(startPc: PrunedAddr, ftqOffset: UInt, isRVC: Bool): UInt = {
    val cfiPc = getCfiPcFromOffset(startPc, ftqOffset).toUInt
    Mux(isRVC, cfiPc, cfiPc - 2.U)
  }

  def getAlignedInstOffset(pc: PrunedAddr): UInt =
    // given an instruction pc, return the offset of the instruction in the fetch block
    // example:
    // if Rvc enabled, aligned to 2B, and if fetch block is aligned to 32B, then:
    // - pc 10 is the inst at offset(5) in fb(0)
    // - pc 68 is the inst at offset(2) in fb(2)
    // if Rvc disabled, aligned to 4B
    pc(FetchBlockAlignWidth - 1, instOffsetBits)

  def getAlignedPosition(startPc: PrunedAddr, ftqOffset: UInt): (UInt, Bool) = {
    /* ftqOffset is relative to fetch block startPc (0 <= ftqOffset < 64B), aligned to inst
     * we want position to be relative to 32B-aligned startPc, also aligned to inst
     * assume Rvc enabled (if not, all the 'bit' below should minus 1)
     * so, we use the lower 4 bits (inst aligned) of startPc, adding 5 bits ftqOffset, to get 6 bits fullPosition
     * if bit 5 (carry) is set, the position is illegal (out of 2 * 32B aligned fetch block)
     * then we take the lower 5 bits as position
     * addr = 0     ...   2     ...   8     ...                   ...   40    ...                   ...   64    ...
     *                    |           |                                 |                                 |
     *                    |           ftqOffset = 3, position = 4       ftqOffset = 19, position = 20     illegal
     *                    startPc(4, 1) = 1
     * addr = 32    ...   62    ...   64    ...                   ...   94    ...                   ...   96    ...
     *                    |           |                                 |                                 |
     *                    |           ftqOffset = 1, position = 16      ftqOffset = 16, position = 31     illegal
     *                    startPc(4, 1) = 15
     * and, for predictors using 4-bit position, we can just take the lower 4 bits of the returned position
     */
    val fullPosition = ftqOffset +& getAlignedInstOffset(startPc)
    val position     = fullPosition(CfiPositionWidth - 1, 0)
    val carry        = fullPosition(CfiPositionWidth)
    (position, carry)
  }

  def getFtqOffset(startPc: PrunedAddr, position: UInt): UInt = {
    // given a 5-bit position, calculate the ftqOffset
    require(
      position.getWidth == CfiPositionWidth,
      s"position width should be $CfiPositionWidth, but got ${position.getWidth}"
    )
    position - getAlignedInstOffset(startPc)
  }
}

trait CrossPageHelper extends HasBpuParameters {
  // FIXME: is there a top-level getVPN function?
  def getVpn(addr: PrunedAddr): UInt =
    // get virtual page number (VPN) from a virtual address
    addr(addr.length - 1, PageOffsetWidth)

  def isCrossPage(startPc: PrunedAddr, target: PrunedAddr): Bool =
    // a simplified version of isCrossPage, to tell if a single fetch block can cross page
    // we only need to check the LSB of VPN, as +FetchBlockSize can only cross 1 boundary
    startPc(PageOffsetWidth) =/= target(PageOffsetWidth)

  def isCrossPageFull(addr1: PrunedAddr, addr2: PrunedAddr): Bool =
    // check full VPN
    getVpn(addr1) =/= getVpn(addr2)

  def getPageAlignedAddr(addr: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getVpn(addr),
      0.U(PageOffsetWidth.W)
    ))
}

trait TargetFixHelper extends HasBpuParameters {
  // abstract getTargetUpper function, to be implemented by sub-predictors.
  // basically, it should be `vAddr(VAddrBits - 1, TargetLowerBitsWidth + instOffsetBits)`,
  // where TargetLowerBitsWidth differs for different predictors.
  def getTargetUpper(pc: PrunedAddr): UInt

  def getTargetCarry(startPc: PrunedAddr, fullTarget: PrunedAddr): TargetCarry = {
    val startPcUpper = getTargetUpper(startPc)
    val targetUpper  = getTargetUpper(fullTarget)
    MuxCase(
      TargetCarry.Fit,
      Seq(
        (targetUpper > startPcUpper) -> TargetCarry.Overflow,
        (targetUpper < startPcUpper) -> TargetCarry.Underflow
      )
    )
  }

  def getTargetCarrySlow(startPc: PrunedAddr, fullTarget: PrunedAddr): TargetCarry = {
    val startPcUpper = getTargetUpper(startPc)
    val targetUpper  = getTargetUpper(fullTarget)
    MuxCase(
      TargetCarry.Invalid,
      Seq(
        (targetUpper + 1.U === startPcUpper) -> TargetCarry.Underflow,
        (targetUpper - 1.U === startPcUpper) -> TargetCarry.Overflow,
        (targetUpper === startPcUpper)       -> TargetCarry.Fit
      )
    )
  }

  def fixTargetUpper(
      startPcUpper: UInt,
      targetCarry:  TargetCarry,
      // pre-computed startPcUpper+-1 for timing, default is not needed
      startPcUpperPlusOne:  Option[UInt] = None,
      startPcUpperMinusOne: Option[UInt] = None
  ): UInt =
    MuxCase(
      startPcUpper,
      Seq(
        targetCarry.isOverflow  -> startPcUpperPlusOne.getOrElse(startPcUpper + 1.U),
        targetCarry.isUnderflow -> startPcUpperMinusOne.getOrElse(startPcUpper - 1.U)
      )
    )

  // ubtb and abtb can select whether to use the targetCarry or not, so Option[TargetCarry]
  def getFullTarget(
      startPc:     PrunedAddr,
      target:      UInt,
      targetCarry: Option[TargetCarry],
      // pre-computed startPcUpper+-1 for timing, default is not needed
      startPcUpperPlusOne:  Option[UInt] = None,
      startPcUpperMinusOne: Option[UInt] = None
  ): PrunedAddr = {
    val startPcUpper = getTargetUpper(startPc)
    PrunedAddrInit(Cat(
      if (targetCarry.isDefined) // `if (EnableTargetFix)` in ubtb and abtb
        fixTargetUpper(startPcUpper, targetCarry.get, startPcUpperPlusOne, startPcUpperMinusOne)
      else
        startPcUpper,       // (VAddrBits - 1, TargetWidth + instOffsetBits)
      target,               // (TargetWidth + instOffsetBits - 1, instOffsetBits)
      0.U(instOffsetBits.W) // (instOffsetBits - 1, 0)
    ))
  }
}

trait PhrHelper extends HasBpuParameters {
  def computeFoldedHist(hist: UInt, compLen: Int)(histLen: Int): UInt =
    if (histLen > 0) {
      val nChunks     = (histLen + compLen - 1) / compLen
      val hist_chunks = (0 until nChunks) map { i => hist(min((i + 1) * compLen, histLen) - 1, i * compLen) }
      ParallelXOR(hist_chunks)
    } else 0.U
}
