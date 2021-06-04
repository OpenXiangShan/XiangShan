/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package cache.TLCTest

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLClientNode, TLManagerNode, TLSlaveParameters, TLSlavePortParameters}
import xiangshan.cache.{DCacheBundle, HasDCacheParameters}

class TLULMMIO(implicit p: Parameters) extends DCacheBundle {
  val AChannel = Output(new TLCFakeABundle())
  val AFire = Output(new Bool())
  val DChannel = Output(new TLCFakeDBundle())
  val DFire = Output(new Bool())
  val isOn = Input(new Bool())
}


class TLCSnoopMMIONode()(implicit p: Parameters) extends LazyModule
  with HasDCacheParameters {

  val l2params = p(TLCCacheTestKey)

  val node = TLAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new TLULMMIO)

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a.bits := in.a.bits
      in.d.bits := out.d.bits
      in.a.ready := out.a.ready && io.isOn
      out.a.valid := in.a.valid && io.isOn
      in.d.valid := out.d.valid && io.isOn
      out.d.ready := in.d.ready && io.isOn
    }
    val (bus, edge) = node.in.head
    
    io.AChannel.opcode := bus.a.bits.opcode
    io.AChannel.param := bus.a.bits.param
    io.AChannel.size := bus.a.bits.size
    io.AChannel.source := bus.a.bits.source
    io.AChannel.address := bus.a.bits.address
    io.AChannel.mask := bus.a.bits.mask
    io.AChannel.data := bus.a.bits.data
    io.AFire := bus.a.fire()

    io.DChannel.opcode := bus.d.bits.opcode
    io.DChannel.param := bus.d.bits.param
    io.DChannel.size := bus.d.bits.size
    io.DChannel.source := bus.d.bits.source
    io.DChannel.sink := bus.d.bits.sink
    io.DChannel.denied := bus.d.bits.denied
    io.DChannel.data := bus.d.bits.data
    io.DFire := bus.d.fire()
  }

}
