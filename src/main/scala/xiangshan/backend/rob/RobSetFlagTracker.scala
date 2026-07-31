/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

/** A set-only flag update tagged with the instruction that generated it. */
class RobSetFlagUpdateReq(val flagWidth: Int)(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val setMask = UInt(flagWidth.W)
}

/** Tracks the oldest in-flight setter for each bit of a sticky ROB flag. */
class RobSetFlagTracker(
  val width: Int,
  val numUpdatePorts: Int,
  val numCommitPorts: Int
)(implicit p: Parameters) extends XSModule {
  require(width > 0, "RobSetFlagTracker requires at least one flag bit")
  require(numUpdatePorts > 0, "RobSetFlagTracker requires at least one update port")
  require(numCommitPorts > 0, "RobSetFlagTracker requires at least one commit port")

  val io = IO(new Bundle {
    val update = Input(Vec(numUpdatePorts, Valid(new RobSetFlagUpdateReq(width))))
    val commit = Input(Vec(numCommitPorts, Valid(new RobPtr)))
    val redirect = Input(Valid(new Redirect))
    val commitSetMask = Output(UInt(width.W))
  })

  private def selectOldest(candidates: Seq[(Bool, RobPtr)]): (Bool, RobPtr) = {
    ParallelOperation(candidates, (left: (Bool, RobPtr), right: (Bool, RobPtr)) => {
      val selectLeft = left._1 && (!right._1 || left._2.isBeforeSlot(right._2))
      (left._1 || right._1, Mux(selectLeft, left._2, right._2))
    })
  }

  private val pendingValid = RegInit(VecInit(Seq.fill(width)(false.B)))
  private val pendingRobIdx = RegInit(VecInit(Seq.fill(width)(RobPtr(false.B, 0.U))))

  private val updateNeedsFlush = io.update.map(_.bits.robIdx.needFlush(io.redirect))
  private val updateCommits = io.update.map { update =>
    io.commit.map(commit => commit.valid && commit.bits.isSameEntry(update.bits.robIdx)).reduce(_ || _)
  }

  io.commitSetMask := VecInit((0 until width).map { bit =>
    val pendingNeedsFlush = pendingRobIdx(bit).needFlush(io.redirect)
    val pendingCommits = pendingValid(bit) && !io.redirect.valid && io.commit.map { commit =>
      commit.valid && commit.bits.isSameEntry(pendingRobIdx(bit))
    }.reduce(_ || _) && !pendingNeedsFlush

    val updateCandidates = io.update.indices.map { index =>
      val update = io.update(index)
      val valid = update.valid && update.bits.setMask(bit) && !updateNeedsFlush(index) &&
        !(!io.redirect.valid && updateCommits(index))
      (valid, update.bits.robIdx)
    }
    val pendingCandidate = (
      pendingValid(bit) && !pendingCommits && !pendingNeedsFlush,
      pendingRobIdx(bit)
    )
    val next = selectOldest(pendingCandidate +: updateCandidates)

    pendingValid(bit) := next._1
    when(next._1) {
      pendingRobIdx(bit) := next._2
    }

    pendingCommits
  }).asUInt
}
