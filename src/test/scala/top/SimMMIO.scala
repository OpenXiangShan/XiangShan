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
import org.chipsalliance.cde.config.Parameters
import device._
import freechips.rocketchip.amba.axi4.{AXI4EdgeParameters, AXI4MasterNode, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import difftest._
import utility.AXI4Error
import system.{HasPeripheralRanges, HasSoCParameter}

class SimMMIO(edge: AXI4EdgeParameters)(implicit p: Parameters) extends LazyModule
  with HasSoCParameter
  with HasPeripheralRanges
{

  val node = AXI4MasterNode(List(edge.master))

  // val uartRange = AddressSet(0x40600000, 0x3f) // ?
  val flashRange = AddressSet(0x10000000L, 0xfffffff)
  val sdRange = AddressSet(0x40002000L, 0xfff)
  val intrGenRange = AddressSet(0x40070000L, 0x0000ffffL)

  val illegalRange = (onChipPeripheralRanges.values ++ Seq(
    soc.UARTLiteRange,
    soc.UART16550Range,
    flashRange,
    sdRange,
    intrGenRange
  )).foldLeft(Seq(AddressSet(0x0, 0x7fffffffL)))((acc, x) => acc.flatMap(_.subtract(x)))

  val flash = LazyModule(new AXI4Flash(Seq(AddressSet(0x10000000L, 0xfffffff))))
  val uartLite = LazyModule(new AXI4UART(Seq(soc.UARTLiteRange)))
  private val uart16550Params = UART16550Params(address = soc.UART16550Range.base)
  val uart16550 = LazyModule(new AXI4UART16550(uart16550Params))
  // val vga = LazyModule(new AXI4VGA(
  //   sim = false,
  //   fbAddress = Seq(AddressSet(0x50000000L, 0x3fffffL)),
  //   ctrlAddress = Seq(AddressSet(0x40001000L, 0x7L))
  // ))
  val sd = LazyModule(new AXI4DummySD(Seq(AddressSet(0x40002000L, 0xfff))))
  val intrGen = LazyModule(new AXI4IntrGenerator(Seq(AddressSet(0x40070000L, 0x0000ffffL))))
  val error = LazyModule(new AXI4Error(illegalRange))

  private val uartIntSink = IntSinkNode(IntSinkPortSimple())
  uartIntSink := uart16550.intXing()

  val axiBus = AXI4Xbar()

  uartLite.node := axiBus
  uart16550.controlXing() := axiBus
  // vga.node :*= axiBus
  flash.node := axiBus
  sd.node := axiBus
  intrGen.node := axiBus
  error.node := axiBus

  axiBus := node

  val io_axi4 = InModuleBody {
    node.makeIOs()
  }

  class SimMMIOImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle() {
      val uart = new UARTIO
      val interrupt = new IntrGenIO
    })

    // FIXME: char will be lost if both devices send or receive char in the same cycle
    io.uart.in.valid := uartLite.module.io.extra.get.in.valid | uart16550.module.io.uart.in.valid
    uartLite.module.io.extra.get.in.ch := io.uart.in.ch
    uart16550.module.io.uart.in.ch := io.uart.in.ch

    io.uart.out.valid := uartLite.module.io.extra.get.out.valid | uart16550.module.io.uart.out.valid
    io.uart.out.ch := Mux(uartLite.module.io.extra.get.out.valid,
      uartLite.module.io.extra.get.out.ch,
      uart16550.module.io.uart.out.ch
    )

    val uart16550Int = uartIntSink.makeIOs()
    private val uart16550IntNum = 0xa;
    io.interrupt.intrVec := intrGen.module.io.extra.get.intrVec | uart16550Int.elts.head.head << (uart16550IntNum - 1)

    // TODO: rx not supported yet
    uart16550.module.io.tx.ready := true.B
    uart16550.module.io.rx.valid := false.B
    uart16550.module.io.rx.bits := 0.U
  }

  lazy val module = new SimMMIOImp(this)
}
