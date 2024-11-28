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
import org.chipsalliance.cde.config._
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, MaxHartIdBits}
import freechips.rocketchip.tilelink._
import coupledL2.{L2ParamKey, EnableCHI}
import coupledL2.tl2tl.TL2TLCoupledL2
import coupledL2.tl2chi.{TL2CHICoupledL2, PortIO, CHIIssue}
import huancun.BankBitsKey
import system.HasSoCParameter
import top.BusPerfMonitor
import utility._


class FakeL3()(implicit p: Parameters) extends TLClientsMerger(debug = true) {
  override val node = TLAdapterNode(
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


}

object FakeL3 {
  def apply()(implicit p: Parameters) = {
    val fakeL3 = LazyModule(new FakeL3)
    fakeL3.node
  }
}