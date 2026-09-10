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

package xiangshan.frontend.bpu.block2

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.BpuBundle
import xiangshan.frontend.bpu.Prediction

/** What the duplicated btb and tage make of the block starting at a group's second block pc. */
class Block2Prediction(implicit p: Parameters) extends BpuBundle {
  // the btb held at least one branch inside this block. Its absence is the one case this predictor cannot speak to:
  // it means the block was never trained here, not that it runs to the end.
  val hasEntry: Bool = Bool()
  // the block leaves early, at exit; otherwise it runs to the end of the fetch block
  val taken: Bool = Bool()
  // where the block leaves and where it goes, valid when taken
  val exit: Prediction = new Prediction

  val debug_tageDecided: Bool              = Bool()
  val debug_startPc:     Option[GuardedPc] = Option.when(!env.FPGAPlatform)(GuardedPc())

  /** Whether this predictor confirms the second block a pTAGE entry proposed.
    *
    * Confirmation is agreement on everything that decides a fetch block: that it leaves at all, where it leaves, what
    * kind of branch that is, and where control goes. Anything less would let a block into the fetch stream on the
    * strength of a partial match.
    */
  def confirms(proposed: Prediction): Bool =
    hasEntry && taken && proposed.taken &&
      exit.cfiPosition === proposed.cfiPosition &&
      !(exit.attribute =/= proposed.attribute) &&
      exit.target === proposed.target
}
