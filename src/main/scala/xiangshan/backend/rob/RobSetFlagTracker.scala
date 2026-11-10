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
import xiangshan.backend.rob.RobSetFlagTracker._

/** Tracks the oldest in-flight setter for each bit of a sticky ROB flag. */
class RobSetFlagTracker(
  val width: Int,
  val numUpdatePorts: Int,
  val numCommitPorts: Int,
  candidateIndices: Seq[Seq[Int]] = Seq.empty,
  orderedUpdatePorts: Boolean = false
)(implicit p: Parameters) extends XSModule {
  require(width > 0, "RobSetFlagTracker requires at least one flag bit")
  require(numUpdatePorts > 0, "RobSetFlagTracker requires at least one update port")
  require(numCommitPorts > 0, "RobSetFlagTracker requires at least one commit port")

  private val candidatesByFlag = if (candidateIndices.isEmpty) {
    Seq.fill(width)((0 until numUpdatePorts).toSeq)
  } else {
    require(
      candidateIndices.length == width,
      s"RobSetFlagTracker candidateIndices must have $width flag entries, got ${candidateIndices.length}"
    )
    candidateIndices.foreach { candidates =>
      require(
        candidates.distinct.length == candidates.length,
        "RobSetFlagTracker candidateIndices must not contain duplicate ports"
      )
      require(
        candidates.forall(index => index >= 0 && index < numUpdatePorts),
        s"RobSetFlagTracker candidateIndices must be in [0, $numUpdatePorts)"
      )
    }
    candidateIndices
  }

  val in = IO(new In(width, numUpdatePorts, numCommitPorts))
  val out = IO(new Out(width, numUpdatePorts, numCommitPorts))

  private def selectOldest(candidates: Seq[(Bool, RobPtr)]): (Bool, RobPtr) = {
    ParallelOperation(candidates, (left: (Bool, RobPtr), right: (Bool, RobPtr)) => {
      val selectLeft = left._1 && (!right._1 || left._2.isBeforeSlot(right._2))
      (left._1 || right._1, Mux(selectLeft, left._2, right._2))
    })
  }

  // Enqueue lanes are already ordered by ROB age. Keep this reduction balanced,
  // but select the left (older) valid lane without another RobPtr comparison.
  private def selectFirstValid(candidates: Seq[(Bool, RobPtr)]): (Bool, RobPtr) = {
    ParallelOperation(candidates, (left: (Bool, RobPtr), right: (Bool, RobPtr)) => {
      (left._1 || right._1, Mux(left._1, left._2, right._2))
    })
  }

  private val pendingValid = RegInit(VecInit(Seq.fill(width)(false.B)))
  private val pendingRobIdx = RegInit(VecInit(Seq.fill(width)(RobPtr(false.B, 0.U))))

  private val updateNeedsFlush = in.update.map(_.bits.robIdx.needFlush(in.redirect))
  private val updateCommits = in.update.map { update =>
    in.commit.map(commit => commit.valid && commit.bits.isSameEntry(update.bits.robIdx)).reduce(_ || _)
  }
  private val commonUpdateValid = in.update.zipWithIndex.map { case (update, index) =>
    update.valid && !updateNeedsFlush(index) &&
      !(!in.redirect.valid && updateCommits(index))
  }

  if (orderedUpdatePorts) {
    for {
      older <- 0 until numUpdatePorts
      younger <- older + 1 until numUpdatePorts
    } {
      when(in.update(older).valid && in.update(younger).valid) {
        assert(
          in.update(older).bits.robIdx.isNotAfterSlot(in.update(younger).bits.robIdx),
          s"ordered RobSetFlagTracker update ports $older and $younger are out of ROB order"
        )
      }
    }
  }

  private val taken = Wire(Vec(width, Bool()))
  private val takeEnabled = in.takeSlot.valid && !reset.asBool
  out.takenSetMask := taken.asUInt

  out.commitSetMask := VecInit((0 until width).map { bit =>
    val pendingNeedsFlush = pendingRobIdx(bit).needFlush(in.redirect)
    val pendingCommits = pendingValid(bit) && !reset.asBool && !in.redirect.valid && in.commit.map { commit =>
      commit.valid && commit.bits.isSameEntry(pendingRobIdx(bit))
    }.reduce(_ || _) && !pendingNeedsFlush
    val takeBit = takeEnabled && in.takeSlot.bits.mask(bit)
    val pendingTaken = takeBit && pendingValid(bit) && !pendingNeedsFlush &&
      pendingRobIdx(bit).isSameSlot(in.takeSlot.bits.robIdx)
    // Include accepted setters in this cycle, even if the same entry also
    // commits. A consumed update must not be reinserted into pending state.
    val updateTaken = in.update.zipWithIndex.map { case (update, index) =>
      candidatesByFlag(bit).contains(index).B && takeBit && update.valid && !updateNeedsFlush(index) && update.bits.setMask(bit) &&
        update.bits.robIdx.isSameSlot(in.takeSlot.bits.robIdx)
    }
    taken(bit) := pendingTaken || updateTaken.reduce(_ || _)

    val updateCandidates = candidatesByFlag(bit).map { index =>
      val update = in.update(index)
      val valid = commonUpdateValid(index) && update.bits.setMask(bit) && !updateTaken(index)
      (valid, update.bits.robIdx)
    }
    val pendingCandidate = (
      pendingValid(bit) && !pendingCommits && !pendingNeedsFlush && !pendingTaken,
      pendingRobIdx(bit)
    )
    val updateWinner = if (orderedUpdatePorts) {
      selectFirstValid(updateCandidates)
    } else {
      selectOldest(updateCandidates)
    }
    val next = if (orderedUpdatePorts) {
      selectOldest(Seq(pendingCandidate, updateWinner))
    } else {
      selectOldest(pendingCandidate +: updateCandidates)
    }

    pendingValid(bit) := next._1
    when(next._1) {
      pendingRobIdx(bit) := next._2
    }

    // A sticky bit uses only one delivery path when take and commit coincide.
    pendingCommits && !taken(bit)
  }).asUInt
}

object RobSetFlagTracker {
  class In(width: Int, numUpdatePorts: Int, numCommitPorts: Int)(implicit p: Parameters) extends XSBundle {
    // Speculative flag setters.
    val update = Input(Vec(numUpdatePorts, Valid(new RobSetFlagUpdateReq(width))))

    // Retired setter identities.
    val commit = Input(Vec(numCommitPorts, Valid(new RobPtr)))

    // Surviving setter consumed with a trap.
    val takeSlot = Input(Valid(new RobSetFlagTakeReq(width)))

    // Recovery boundary.
    val redirect = Input(Valid(new Redirect))
  }

  class Out(width: Int, numUpdatePorts: Int, numCommitPorts: Int)(implicit p: Parameters) extends XSBundle {
    // Architectural flag updates.
    val commitSetMask = Output(UInt(width.W))
    val takenSetMask = Output(UInt(width.W))
  }

  /** A set-only flag update tagged with the inst robIdx that generated it. */
  class RobSetFlagUpdateReq(val flagWidth: Int)(implicit p: Parameters) extends XSBundle {
    val robIdx = new RobPtr
    val setMask = UInt(flagWidth.W)
  }

  /** Consume selected sticky flags from one surviving architectural slot. */
  class RobSetFlagTakeReq(val flagWidth: Int)(implicit p: Parameters) extends XSBundle {
    val robIdx = new RobPtr
    val mask = UInt(flagWidth.W)
  }
}
