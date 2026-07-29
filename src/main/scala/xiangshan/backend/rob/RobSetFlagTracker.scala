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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._

/** A set-only flag update tagged with the instruction that generated it. */
class RobSetFlagUpdateReq(val flagWidth: Int)(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val setMask = UInt(flagWidth.W)
}

/**
  * Tracks the oldest in-flight setter for each bit of a sticky ROB flag.
  */
class RobSetFlagTracker(
  val width: Int,
  val numUpdatePorts: Int,
  val numCommitPorts: Int
)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
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
    require(candidates.nonEmpty)
    ParallelOperation(candidates, (left: (Bool, RobPtr), right: (Bool, RobPtr)) => {
      val leftIsOldest = left._1 && (!right._1 || isBefore(left._2, right._2))
      val selected = Mux(leftIsOldest, left._2, right._2)
      (left._1 || right._1, selected)
    })
  }

  private val pendingValid = RegInit(VecInit(Seq.fill(width)(false.B)))
  private val pendingRobIdx = RegInit(VecInit(Seq.fill(width)(0.U.asTypeOf(new RobPtr))))

  private val updateNeedsFlush = io.update.map(_.bits.robIdx.needFlush(io.redirect))
  private val updateCommits = io.update.map { update =>
    io.commit.map(commit => commit.valid && commit.bits === update.bits.robIdx).reduce(_ || _)
  }

  val commitSetBits = (0 until width).map { bit =>
    val pendingNeedsFlush = pendingRobIdx(bit).needFlush(io.redirect)
    val commitMatchesPending = io.commit.map(commit =>
      commit.valid && commit.bits === pendingRobIdx(bit)
    ).reduce(_ || _)
    // ROB blocks architectural commits on redirects.  Keep the same priority
    // here so malformed standalone inputs cannot emit a stale CSR update.
    val commitAllowed = !io.redirect.valid
    val pendingCommits = pendingValid(bit) && commitAllowed && commitMatchesPending && !pendingNeedsFlush

    val updateCandidates = io.update.indices.map { index =>
      val update = io.update(index)
      val updateValid = update.valid && update.bits.setMask(bit) && !updateNeedsFlush(index) &&
        !(commitAllowed && updateCommits(index))
      (updateValid, update.bits.robIdx)
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
  }

  io.commitSetMask := VecInit(commitSetBits).asUInt
}
