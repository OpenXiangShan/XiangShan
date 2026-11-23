/***************************************************************************************
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2026 Beijing Institute of Open Source Chip
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._

/** Early enqueue decisions: deliberately independent of late cancel/WBQ and downstream ready. */
private[cache] object MissQueueEnqLogic {
  // The lowest demand port leads; prefetches follow demand when they can merge.
  // Only mainpipe supplies hardware prefetches; software prefetches use the demand class.
  def mergeLeaders(
    valid: Seq[Bool],
    isPrefetch: Seq[Bool],
    canMergeInto: Seq[Seq[Bool]]
  ): Seq[UInt] = {
    require(valid.nonEmpty && valid.length == isPrefetch.length)
    val n = valid.length
    require(canMergeInto.length == n && canMergeInto.forall(_.length == n))
    valid.indices.map { i =>
      val members = valid.indices.map(j => valid(j) && (canMergeInto(i)(j) || (i == j).B))
      val demandMembers = members.zip(isPrefetch).map { case (member, prefetch) => member && !prefetch }
      val demandMask = VecInit(demandMembers).asUInt
      Mux(valid(i) && demandMask.orR, PriorityEncoder(demandMask), i.U(log2Up(n).W))
    }
  }

  def allocationRanks(needsAllocate: Seq[Bool], isPrefetch: Seq[Bool]): Seq[UInt] = {
    require(needsAllocate.nonEmpty && needsAllocate.length == isPrefetch.length)
    needsAllocate.indices.map { i =>
      PopCount(needsAllocate.indices.filter(_ != i).map { j =>
        val higherPriority = (!isPrefetch(j) && isPrefetch(i)) ||
          (isPrefetch(j) === isPrefetch(i) && (j < i).B)
        needsAllocate(j) && higherPriority
      })
    }
  }
}
