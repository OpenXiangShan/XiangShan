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

package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLAdapterNode

// FakeL3 is only used to merge clients of [L2, DMA#0, DMA#1, Debug]
// This file follows the implementation of Utility.TLClientsMerger
class FakeL3()(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(
    clientFn = s => {

      println("TLClientsMerger: Merging clients:")
      for (c <- s.masters) {
        println(c)
      }

      val sourceIds = s.masters.map(_.sourceId)
      val minId = sourceIds.map(_.start).min
      val maxId = sourceIds.map(_.end).max
      val merged = s.v1copy(
        clients = Seq(s.masters.find(_.name == "L2").get.v1copy(
          sourceId = IdRange(minId, maxId),
          visibility = Seq(AddressSet(0x0, ~0x0))
        ))
      )
      println("Merged params:")
      println(merged.masters)

      merged
    }
  )

  lazy val module = new LazyModuleImp(this){
    require(node.in.size == 1)
    // TODO: this is duplicated??
    for((in, out) <- node.in.map(_._1).zip(node.out.map(_._1))){
      out <> in
    }
    // TODO: we should handle b channel like in TLClientsMerger, but direct all Probes to L2
    // However, no probes would come from downstream of L3, so we just skip it for now
  }
}

object FakeL3 {
  def apply()(implicit p: Parameters) = {
    val fakeL3 = LazyModule(new FakeL3)
    fakeL3.node
  }
}