/***************************************************************************************
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2026 Beijing Institute of Open Source Chip
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._

/** Fixed port priority for same-cycle leadership and ordinary allocation. */
private[cache] object MissQueueEnqLogic {
  // Only earlier ports can lead; MainPipe (port 0) never follows a LoadPipe.
  def mergeLeaders(valid: Seq[Bool], canMergeInto: Seq[Seq[Bool]]): Seq[UInt] = {
    require(valid.nonEmpty)
    val n = valid.length
    require(canMergeInto.length == n && canMergeInto.forall(_.length == n))
    valid.indices.map { i =>
      if (i == 0) 0.U(log2Up(n).W) else {
        val earlier = VecInit((0 until i).map(j => valid(j) && canMergeInto(i)(j))).asUInt
        Mux(earlier.orR, PriorityEncoder(earlier), i.U(log2Up(n).W))
      }
    }
  }

  def allocationRanks(needsAllocate: Seq[Bool]): Seq[UInt] = {
    require(needsAllocate.nonEmpty)
    needsAllocate.indices.map { i =>
      val rank = Wire(UInt(log2Up(needsAllocate.length).W))
      rank := (if (i == 0) 0.U else PopCount(needsAllocate.take(i)))
      rank
    }
  }
}
