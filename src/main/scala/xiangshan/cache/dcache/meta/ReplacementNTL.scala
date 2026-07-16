/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

// Tree-PLRU helper for NTL: standard touch marks MRU; NTL touch marks the hit way as the replacement victim (oldest).
class NtlPseudoLRU(n_ways: Int) extends PseudoLRU(n_ways) {
  // Victim touch: update PLRU state so touch_way becomes the next replace candidate.
  // Same tree walk as PseudoLRU.get_next_state, but with inverted root/leaf polarity.
  def get_next_state_ntl(state: UInt, touch_way: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways-1),                   s"wrong state bits width ${state.getWidth} for $tree_nways ways")
    require(touch_way.getWidth == (log2Ceil(tree_nways) max 1), s"wrong encoded way width ${touch_way.getWidth} for $tree_nways ways")

    if (tree_nways > 2) {
      // we are at a branching node in the tree, so recurse
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val left_nways:  Int = tree_nways - right_nways         // number of ways in the left sub-tree
      val set_left_older      = touch_way(log2Ceil(tree_nways)-1)  // NTL set the touch way as oldest
      val left_subtree_state  = state.extract(tree_nways-3, right_nways-1)
      val right_subtree_state = state(right_nways-2, 0)

      if (left_nways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(set_left_older,
            Mux(set_left_older,
                get_next_state_ntl(left_subtree_state, touch_way.extract(log2Ceil(left_nways)-1,0), left_nways),
                left_subtree_state),
            Mux(set_left_older,
                right_subtree_state,
                get_next_state_ntl(right_subtree_state, touch_way(log2Ceil(right_nways)-1,0), right_nways)))
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(set_left_older,
            Mux(set_left_older,
                right_subtree_state,
                get_next_state_ntl(right_subtree_state, touch_way(log2Ceil(right_nways)-1,0), right_nways)))
      }
    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree
      touch_way(0)
    } else {  // tree_nways <= 1
      // we are at an empty node in an empty tree for 1 way, so return single zero bit for Chisel (no zero-width wires)
      0.U(1.W)
    }
  }

  def get_next_state_ntl(state: UInt, touch_way: UInt): UInt = {
    val touch_way_sized = if (touch_way.getWidth < log2Ceil(n_ways)) touch_way.padTo  (log2Ceil(n_ways))
                                                                else touch_way.extract(log2Ceil(n_ways)-1,0)
    get_next_state_ntl(state, touch_way_sized, n_ways)
  }

  // Select MRU touch or NTL victim touch for a single access.
  def get_next_state_final(state: UInt, touch_way: UInt, ntl: Bool): UInt = {
    Mux(ntl, get_next_state_ntl(state, touch_way), get_next_state(state, touch_way))
  }

  // Fold multiple port touches in order; each port picks MRU or victim via ntl(i).
  def get_next_state_final(state: UInt, touch_ways: Seq[Valid[UInt]], ntl: Seq[Bool]): UInt = {
    (touch_ways zip ntl).foldLeft(state) { case (prev, (touch_way, ntl)) =>
      Mux(touch_way.valid,
          get_next_state_final(prev, touch_way.bits, ntl),
          prev)
    }
  }
}

class NtlSetAssocLRU(n_sets: Int, n_ways: Int) {
  val logic = new NtlPseudoLRU(n_ways)
  val state_vec =
    if (logic.nBits == 0) Reg(Vec(n_sets, UInt(logic.nBits.W))) // Work around elaboration error on following line
    else RegInit(VecInit(Seq.fill(n_sets)(0.U(logic.nBits.W))))

  def access(set: UInt, touch_way: UInt, ntl: Bool = false.B) = {
    state_vec(set) := logic.get_next_state_final(state_vec(set), touch_way, ntl)
  }

  def access(sets: Seq[UInt], touch_ways: Seq[Valid[UInt]], ntl: Seq[Bool]) = {
    require(sets.size == touch_ways.size, "internal consistency check: should be same number of simultaneous updates for sets and touch_ways")
    require(sets.size == ntl.size, "internal consistency check: should be same number of simultaneous updates for sets and ntls")
    for (set <- 0 until n_sets) {
      val set_touch_ways = (sets zip touch_ways).map { case (touch_set, touch_way) =>
        Pipe(touch_way.valid && (touch_set === set.U), touch_way.bits, 0)}
      when (set_touch_ways.map(_.valid).orR) {
        state_vec(set) := logic.get_next_state_final(state_vec(set), set_touch_ways, ntl)
      }
    }
  }

  def way(set: UInt) = logic.get_replace_way(state_vec(set))

}