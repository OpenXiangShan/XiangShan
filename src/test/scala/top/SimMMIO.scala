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

package top

import chisel3._
import chipsalliance.rocketchip.config
import device._
import freechips.rocketchip.amba.axi4.{AXI4EdgeParameters, AXI4MasterNode, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import difftest._
import system.SoCParamsKey

class SimMMIO(edge: AXI4EdgeParameters)(implicit p: config.Parameters) extends LazyModule {

  val node = AXI4MasterNode(List(edge.master))

  val flash = LazyModule(new AXI4Flash(Seq(AddressSet(0x10000000L, 0xfffffff))))
  val uart = LazyModule(new AXI4UART(Seq(AddressSet(0x40600000L, 0xf))))
  val socParams = p(SoCParamsKey)
  val uart1 = if (socParams.NohypeDevOffset == 0) {
    LazyModule(new AXI4UART(Seq(AddressSet(0x40600000L + 0x3000000, 0xf))))
  }
  else {
    LazyModule(new AXI4UART(Seq(AddressSet(0x40600000L + socParams.NohypeDevOffset, 0xf))))
  }

  // val vga = LazyModule(new AXI4VGA(
  //   sim = false,
  //   fbAddress = Seq(AddressSet(0x50000000L, 0x3fffffL)),
  //   ctrlAddress = Seq(AddressSet(0x40001000L, 0x7L))
  // ))
  val sd = LazyModule(new AXI4DummySD(Seq(AddressSet(0x40002000L, 0xfff))))
  val intrGen = LazyModule(new AXI4IntrGenerator(Seq(AddressSet(0x40070000L, 0x0000ffffL))))

  val axiBus = AXI4Xbar()

  uart.node := axiBus
  uart1.node := axiBus
  // vga.node :*= axiBus
  flash.node := axiBus
  sd.node := axiBus
  intrGen.node := axiBus

  axiBus := node

  val io_axi4 = InModuleBody {
    node.makeIOs()
  }

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle() {
      val uart = new UARTIO
      val uart1 = new UARTIO
      val interrupt = new IntrGenIO
    })
    io.uart <> uart.module.io.extra.get
    io.uart1 <> uart1.module.io.extra.get
    io.interrupt <> intrGen.module.io.extra.get
  }

}
