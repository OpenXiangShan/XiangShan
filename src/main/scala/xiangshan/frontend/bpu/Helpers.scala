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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

trait HalfAlignHelper extends HasBpuParameters {
  def getAlignedAddrUpper(addr: PrunedAddr): UInt =
    addr(addr.length - 1, FetchBlockAlignWidth)

  def getAlignedAddr(addr: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getAlignedAddrUpper(addr),
      0.U(FetchBlockAlignWidth.W)
    ))

  def getAlignedInstOffset(addr: PrunedAddr): UInt =
    // given an instruction address, return the offset of the instruction in the fetch block
    // example:
    // if Rvc enabled, aligned to 2B, and if fetch block is aligned to 32B, then:
    // - addr 10 is the inst at offset(5) in fb(0)
    // - addr 68 is the inst at offset(2) in fb(2)
    // if Rvc disabled, aligned to 4B
    addr(FetchBlockAlignWidth - 1, instOffsetBits)

  def getAlignedPosition(startVAddr: PrunedAddr, ftqOffset: UInt): (UInt, Bool) = {
    /* ftqOffset is relative to fetch block startVAddr (0 <= ftqOffset < 64B), aligned to inst
     * we want position to be relative to 32B-aligned startVAddr, also aligned to inst
     * assume Rvc enabled (if not, all the 'bit' below should minus 1)
     * so, we use the lower 4 bits (inst aligned) of startVAddr, adding 5 bits ftqOffset, to get 6 bits fullPosition
     * if bit 5 (carry) is set, the position is illegal (out of 2 * 32B aligned fetch block)
     * then we take the lower 5 bits as position
     * addr = 0     ...   2     ...   8     ...                   ...   40    ...                   ...   64    ...
     *                    |           |                                 |                                 |
     *                    |           ftqOffset = 3, position = 4       ftqOffset = 19, position = 20     illegal
     *                    startVAddr(4, 1) = 1
     * addr = 32    ...   62    ...   64    ...                   ...   94    ...                   ...   96    ...
     *                    |           |                                 |                                 |
     *                    |           ftqOffset = 1, position = 16      ftqOffset = 16, position = 31     illegal
     *                    startVAddr(4, 1) = 15
     * and, for predictors using 4-bit position, we can just take the lower 4 bits of the returned position
     */
    val fullPosition = ftqOffset +& getAlignedInstOffset(startVAddr)
    val position     = fullPosition(CfiPositionWidth - 1, 0)
    val carry        = fullPosition(CfiPositionWidth)
    (position, carry)
  }

  def getFtqOffset(startVAddr: PrunedAddr, position: UInt): UInt = {
    // given a 5-bit position, calculate the ftqOffset
    // TODO: select from two 4-bit position? (startVAddr -> mbtb & startVAddr+32 -> mbtb)
    require(
      position.getWidth == CfiPositionWidth,
      s"position width should be $CfiPositionWidth, but got ${position.getWidth}"
    )
    position - getAlignedInstOffset(startVAddr)
  }
}

trait CrossPageHelper extends HasBpuParameters {
  // FIXME: is there a top-level getVPN function?
  def getVpn(addr: PrunedAddr): UInt =
    // get virtual page number (VPN) from a virtual address
    addr(addr.length - 1, PageOffsetWidth)

  def isCrossPage(startVAddr: PrunedAddr, target: PrunedAddr): Bool =
    // a simplified version of isCrossPage, to tell if a single fetch block can cross page
    // we only need to check the LSB of VPN, as +FetchBlockSize can only cross 1 boundary
    startVAddr(PageOffsetWidth) =/= target(PageOffsetWidth)

  def isCrossPageFull(addr1: PrunedAddr, addr2: PrunedAddr): Bool =
    // check full VPN
    getVpn(addr1) =/= getVpn(addr2)

  def getPageAlignedAddr(addr: PrunedAddr): PrunedAddr =
    PrunedAddrInit(Cat(
      getVpn(addr),
      0.U(PageOffsetWidth.W)
    ))
}

trait BtbHelper extends HasBpuParameters {
  def getFirstTakenEntryWayIdxOH(positions: IndexedSeq[UInt], takenMask: IndexedSeq[Bool]): IndexedSeq[Bool] = {
    require(positions.length == takenMask.length)
    val n = positions.length
    val compareMatrix = (0 until n).map(i =>
      (0 until i).map(j =>
        positions(j) > positions(i)
      )
    )
    (0 until n).map { i =>
      (0 until n).map { j =>
        if (j < i) !takenMask(j) || compareMatrix(i)(j)
        else if (j == i) takenMask(i)
        else !takenMask(j) || !compareMatrix(j)(i)
      }.reduce(_ && _)
    }
  }
}
