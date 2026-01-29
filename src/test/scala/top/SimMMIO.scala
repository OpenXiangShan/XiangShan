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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import difftest._
import utility.AXI4Error
import system.{HasPeripheralRanges, HasSoCParameter}
import iopmp._

class SimMMIO(edge: AXI4EdgeParameters)(implicit p: Parameters) extends LazyModule
  with HasSoCParameter
  with HasPeripheralRanges
{

  val node = AXI4MasterNode(List(edge.master))

  val device = new MemoryDevice
  val memRange = AddressSet(0x00000000L, 0xffffffffffffL).subtract(AddressSet(0x0L, 0x7fffffffL))
  val memNode = AXI4SlaveNode(Seq(
    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = memRange,
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, L3BlockSize),
          supportsWrite = TransferSizes(1, L3BlockSize),
          interleavedId = Some(0),
          resources = device.reg("mem")
        )
      ),
      beatBytes = L3OuterBusWidth / 8,
      requestKeys = if (debugOpts.FPGAPlatform) Seq() else Seq(ReqSourceKey),
    )
  ))

  // val uartRange = AddressSet(0x40600000, 0x3f) // ?
  val flashRange = AddressSet(0x10000000L, 0xfffffff)
  val sdRange = AddressSet(0x40002000L, 0xfff)
  val dmacRange = AddressSet(0x40003000L, 0xfff)
  val intrGenRange = AddressSet(0x40070000L, 0x0000ffffL)
  val apbRange = AddressSet(0x40100000L, 0xffff)

  val illegalRange = (onChipPeripheralRanges.values ++ Seq(
    soc.UARTLiteRange,
    flashRange,
    sdRange,
    dmacRange,
    intrGenRange,
    apbRange
  )).foldLeft(Seq(AddressSet(0x0, 0x7fffffffL)))((acc, x) => acc.flatMap(_.subtract(x)))

  val flash = LazyModule(new AXI4Flash(Seq(AddressSet(0x10000000L, 0xfffffff))))
  val uart = LazyModule(new AXI4UART(Seq(soc.UARTLiteRange)))
  // val vga = LazyModule(new AXI4VGA(
  //   sim = false,
  //   fbAddress = Seq(AddressSet(0x50000000L, 0x3fffffL)),
  //   ctrlAddress = Seq(AddressSet(0x40001000L, 0x7L))
  // ))
  val sd = LazyModule(new AXI4DummySD(Seq(AddressSet(0x40002000L, 0xfff))))
  val dmac = LazyModule(new AXI4DMAC(Seq(AddressSet(0x40003000L, 0xfff))))
  val intrGen = LazyModule(new AXI4IntrGenerator(Seq(AddressSet(0x40070000L, 0x0000ffffL))))
  val error = LazyModule(new AXI4Error(illegalRange))
  val error_tl = LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x1000000000000L, 0xffffffffffffL)), maxAtomic = 1, maxTransfer = 8),beatBytes = 8))

  val axiBus = AXI4Xbar()
  val tlBus = TLXbar()

  uart.node := axiBus
  // vga.node :*= axiBus
  flash.node := axiBus
  sd.node := axiBus
  dmac.node := axiBus
  intrGen.node := axiBus
  error.node := axiBus

  axiBus := node

  error_tl.node := tlBus
  tlBus :=
    TLFIFOFixer() :=
    AXI4ToTL(wcorrupt = false) :=
    AXI4UserYanker() :=
    AXI4IdIndexer(1) :=
    axiBus

  val apb_node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = Seq(apbRange),
      regionType    = RegionType.UNCACHED)),
    beatBytes     = 4)))

  val iopmp = LazyModule(new IopmpLazy(numBridge = 1)) // only one bridge test pass

  apb_node :=
    TLToAPB() :=
    TLFragmenter(4,8) :=
    TLWidthWidget(8) :=
    tlBus

  iopmp.slaveNodes(0) := dmac.masterNode
  memNode := iopmp.masterNodes(0)

  val io_axi4 = InModuleBody {
    node.makeIOs()
  }
  val io_mem = InModuleBody {
    memNode.makeIOs()
  }

  class SimMMIOImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle() {
      val uart = new UARTIO
      val interrupt = new IntrGenIO
    })
    io.uart <> uart.module.io.extra.get
    io.interrupt <> intrGen.module.io.extra.get
  }

  lazy val module = new SimMMIOImp(this){
    
    val iopmp_apb = apb_node.in.head._1

    iopmp.module.apb_s.paddr    <> iopmp_apb.paddr
    iopmp.module.apb_s.psel     <> iopmp_apb.psel
    iopmp.module.apb_s.penable  <> iopmp_apb.penable
    iopmp.module.apb_s.pwrite   <> iopmp_apb.pwrite
    iopmp.module.apb_s.pwdata   <> iopmp_apb.pwdata
    iopmp.module.apb_s.pready   <> iopmp_apb.pready
    iopmp.module.apb_s.prdata   <> iopmp_apb.prdata
  }
}
