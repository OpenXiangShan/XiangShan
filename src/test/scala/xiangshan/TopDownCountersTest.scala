// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan

import org.scalatest.flatspec.AnyFlatSpec

class TopDownCountersTest extends AnyFlatSpec {
  "TopDownCounters" should "keep NoStall at id 0 and NumStallReasons as width sentinel" in {
    assert(TopDownCounters.NoStall.id == 0)
    assert(TopDownCounters.NumStallReasons.id == TopDownCounters.perfCounters.size)
    assert(!TopDownCounters.perfCounters.exists(_.toString == "NumStallReasons"))
  }

  it should "assign ids by declaration order" in {
    TopDownCounters.perfCounters.zipWithIndex.foreach { case (ctr, idx) =>
      assert(ctr.id == idx, s"${ctr.toString} id ${ctr.id} != $idx")
    }
  }

  it should "attach L1/L2 to every perf counter" in {
    assert(TopDownCounters.ICacheMissBubble.l1 == "Frontend")
    assert(TopDownCounters.ICacheMissBubble.l2 == "FetchLatency")
    assert(TopDownCounters.ICacheMissBubble.displayName == "Frontend_FetchLatency_ICacheMissBubble")
    assert(TopDownCounters.perfCounters.forall(ctr => ctr.displayName.endsWith("_" + ctr.toString)))
  }
}
