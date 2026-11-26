/***************************************************************************************
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2026 Beijing Institute of Open Source Chip
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._

/** Demand-first leadership and ordinary allocation, with fixed priority among demand ports. */
private[cache] object MissQueueEnqLogic {
  // The caller permits port 0 to follow loads only for a MainPipe hardware prefetch.
  def mergeLeaders(valid: Seq[Bool], canMergeInto: Seq[Seq[Bool]]): Seq[UInt] = {
    require(valid.nonEmpty)
    val n = valid.length
    require(canMergeInto.length == n && canMergeInto.forall(_.length == n))
    valid.indices.map { i =>
      val candidates = if (i == 0) valid.indices else 0 until i
      val matches = VecInit(candidates.map(j => valid(j) && canMergeInto(i)(j))).asUInt
      Mux(matches.orR, PriorityEncoder(matches), i.U(log2Up(n).W))
    }
  }

  def allocationRanks(needsAllocate: Seq[Bool], mainpipeIsPrefetch: Bool): Seq[UInt] = {
    require(needsAllocate.nonEmpty)
    needsAllocate.indices.map { i =>
      val rank = Wire(UInt(log2Up(needsAllocate.length).W))
      rank := (if (i == 0) Mux(mainpipeIsPrefetch, PopCount(needsAllocate.tail), 0.U)
               else PopCount(needsAllocate.take(i)))
      rank
    }
  }
}
