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

// The design of this file is based on the implementation of PseudoLRU in rock-chip at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/src/main/scala/util/Replacement.scala
// See LICENSE.Berkeley for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.Berkeley
// See LICENSE.SiFive for license details at:
//          https://github.com/chipsalliance/rocket-chip/blob/master/LICENSE.SiFive

package xiangshan.frontend.bpu.replacer

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.UIntToAugmentedUInt

class PlruStateGen(NumWays: Int, AccessSize: Int = 1) extends ReplacerStateGen(NumWays, AccessSize) {
  // Pseudo-LRU tree algorithm: https://en.wikipedia.org/wiki/Pseudo-LRU#Tree-PLRU
  //
  //
  // - bits storage example for 4-way PLRU binary tree:
  //                  bit[2]: ways 3+2 older than ways 1+0
  //                  /                                  \
  //     bit[1]: way 3 older than way 2    bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 3-way PLRU binary tree:
  //                  bit[1]: way 2 older than ways 1+0
  //                                                  \
  //                                       bit[0]: way 1 older than way 0
  //
  //
  // - bits storage example for 8-way PLRU binary tree:
  //                      bit[6]: ways 7-4 older than ways 3-0
  //                      /                                  \
  //            bit[5]: ways 7+6 > 5+4                bit[2]: ways 3+2 > 1+0
  //            /                    \                /                    \
  //     bit[4]: way 7>6    bit[3]: way 5>4    bit[1]: way 3>2    bit[0]: way 1>0
  def StateWidth: Int = NumWays - 1

  /**
    * Computes the next PLRU (Pseudo-Least Recently Used) state based on current state and access patterns.
    * @param state       Current PLRU state as a bit vector (n-1 bits for n ways)
    * @param touchWays  Sequence of potential cache way accesses, each with:
    *                    - valid: Whether this access should affect PLRU state
    *                    - bits:  Index of the way being accessed (log2(n) bits)
    * @return            Updated PLRU state after processing all accesses
    * @note The function processes accesses sequentially - earlier entries in touchWays
    *       have priority when multiple valid accesses occur simultaneously.
    */
  def getNextState(state: UInt, touchWays: Seq[Valid[UInt]]): UInt =
    touchWays.foldLeft(state)((prev, touchWay) => Mux(touchWay.valid, getNextState(prev, touchWay.bits), prev))

  /** @param state stateIn bits for this sub-tree
    * @param touchWay touched way encoded value bits for this sub-tree
    * @param treeNways number of ways in this sub-tree
    */
  def getNextState(state: UInt, touchWay: UInt, treeNways: Int): UInt = {
    require(state.getWidth == (treeNways - 1), s"wrong state bits width ${state.getWidth} for $treeNways ways")
    require(
      touchWay.getWidth == (log2Ceil(treeNways) max 1),
      s"wrong encoded way width ${touchWay.getWidth} for $treeNways ways"
    )

    if (treeNways > 2) {
      // we are at a branching node in the tree, so recurse
      val rightNways: Int = 1 << (log2Ceil(treeNways) - 1) // number of ways in the right sub-tree
      val leftNways:  Int = treeNways - rightNways         // number of ways in the left sub-tree
      val setLeftOlder      = !touchWay(log2Ceil(treeNways) - 1)
      val leftSubtreeState  = state.extract(treeNways - 3, rightNways - 1)
      val rightSubtreeState = state(rightNways - 2, 0)

      if (leftNways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(
          setLeftOlder,
          Mux(
            setLeftOlder,
            leftSubtreeState, // if setting left sub-tree as older, do NOT recurse into left sub-tree
            getNextState(leftSubtreeState, touchWay.extract(log2Ceil(leftNways) - 1, 0), leftNways)
          ), // recurse left if newer
          Mux(
            setLeftOlder,
            getNextState(
              rightSubtreeState,
              touchWay(log2Ceil(rightNways) - 1, 0),
              rightNways
            ), // recurse right if newer
            rightSubtreeState
          )
        ) // if setting right sub-tree as older, do NOT recurse into right sub-tree
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(
          setLeftOlder,
          Mux(
            setLeftOlder,
            getNextState(
              rightSubtreeState,
              touchWay(log2Ceil(rightNways) - 1, 0),
              rightNways
            ), // recurse right if newer
            rightSubtreeState
          )
        ) // if setting right sub-tree as older, do NOT recurse into right sub-tree
      }
    } else if (treeNways == 2) {
      // we are at a leaf node at the end of the tree,
      // so set the single state bit opposite of the lsb of the touched way encoded value
      !touchWay(0)
    } else { // treeNways <= 1
      // we are at an empty node in an empty tree for 1 way, so return single zero bit for Chisel (no zero-width wires)
      0.U(1.W)
    }
  }

  def getNextState(state: UInt, touchWay: UInt): UInt = {
    val touchWaySized = if (touchWay.getWidth < log2Ceil(NumWays)) touchWay.padTo(log2Ceil(NumWays))
    else touchWay.extract(log2Ceil(NumWays) - 1, 0)
    getNextState(state, touchWaySized, NumWays)
  }

  /** @param state stateOut bits for this sub-tree
    * @param treeNways number of ways in this sub-tree
    */
  def getVictim(state: UInt, treeNways: Int): UInt = {
    require(state.getWidth == (treeNways - 1), s"wrong state bits width ${state.getWidth} for $treeNways ways")

    // this algorithm recursively descends the binary tree, filling in the way-to-replace encoded value from msb to lsb
    if (treeNways > 2) {
      // we are at a branching node in the tree, so recurse
      val rightNways: Int = 1 << (log2Ceil(treeNways) - 1) // number of ways in the right sub-tree
      val leftNways:  Int = treeNways - rightNways         // number of ways in the left sub-tree
      val leftSubtreeOlder  = state(treeNways - 2)
      val leftSubtreeState  = state.extract(treeNways - 3, rightNways - 1)
      val rightSubtreeState = state(rightNways - 2, 0)

      if (leftNways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(
          leftSubtreeOlder, // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(
            leftSubtreeOlder,                       // if left sub-tree is older, recurse left, else recurse right
            getVictim(leftSubtreeState, leftNways), // recurse left
            getVictim(rightSubtreeState, rightNways)
          )
        ) // recurse right
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(
          leftSubtreeOlder, // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(
            leftSubtreeOlder, // if left sub-tree is older, return and do not recurse right
            0.U(1.W),
            getVictim(rightSubtreeState, rightNways)
          )
        ) // recurse right
      }
    } else if (treeNways == 2) {
      // we are at a leaf node at the end of the tree,
      // so just return the single state bit as lsb of the way-to-replace encoded value
      state(0)
    } else { // treeNways <= 1
      // we are at an empty node in an unbalanced tree for non-power-of-2 ways,
      // so return single zero bit as lsb of the way-to-replace encoded value
      0.U(1.W)
    }
  }

  def getVictim(state: UInt): UInt = getVictim(state, NumWays)
}
