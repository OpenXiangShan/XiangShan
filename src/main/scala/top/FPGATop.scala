/***************************************************************************************
 * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import device.TLPMAIO
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TopIOAdapter(_top: XSTop)(implicit p: Parameters) extends RawModule {
  // This io is the same as NANHU.
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val sram_config = Input(UInt(16.W))
    val extIntrs = Input(UInt(64.W))
    val pll0_lock = Input(Bool())
    val pll0_ctrl = Output(Vec(6, UInt(32.W)))
    val systemjtag = new Bundle {
      val jtag = Flipped(new JTAGIO(hasTRSTn = false))
      val reset = Input(AsyncReset())
      val mfr_id = Input(UInt(11.W))
      val part_number = Input(UInt(16.W))
      val version = Input(UInt(4.W))
    }
    val debug_reset = Output(Bool())
    val cacheable_check = new TLPMAIO()
  })

  // This is the IO of southlake.
  val top = IO(Flipped(_top.module.io.cloneType))

  io := DontCare

  top := DontCare
  top.clock := io.clock
  withClockAndReset(io.clock, io.reset) {
    top.clock_div2 := Module(new Pow2ClockDivider(1)).io.clock_out
  }
  top.reset := io.reset
  top.extIntrs := io.extIntrs
  top.systemjtag <> io.systemjtag
  io.debug_reset := top.debug_reset
  // soc.io.rtc_clock is a div100 of soc.io.clock
  withClockAndReset(io.clock, io.reset) {
    val rtcClockDiv = 100
    val rtcTickCycle = rtcClockDiv / 2
    val rtcCounter = RegInit(0.U(log2Ceil(rtcTickCycle + 1).W))
    rtcCounter := Mux(rtcCounter === (rtcTickCycle - 1).U, 0.U, rtcCounter + 1.U)
    val rtcClock = RegInit(false.B)
    when(rtcCounter === 0.U) {
      rtcClock := ~rtcClock
    }
    top.rtc_clock := rtcClock
  }
  // FPGA supports booting directly from memory.
  top.riscv_rst_vec.foreach(_ := 0x2000000000L.U)

}

class TopMemoryAdapter(_top: XSTop)(implicit p: Parameters) extends Module {
  class TopMemoryBusAdapter()(implicit p: Parameters) extends LazyModule {
    val slave = AXI4SlaveNode(_top.misc.memAXI4SlaveNode.portParams)
    val master = AXI4MasterNode(List(_top.misc.memAXI4SlaveNode.in.head._2.master))

    val errorDev = LazyModule(new TLError(
      params = DevNullParams(
        address = Seq(AddressSet(0x0L, 0x7fffffffL)),
        maxAtomic = 8,
        maxTransfer = 128
      ),
      beatBytes = 32
    ))
    val tlBus = TLXbar()
    tlBus :=
      TLFIFOFixer() :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      master
    errorDev.node := tlBus
    class MemFastClockDomain()(implicit p: Parameters) extends LazyModule {
      val node = AXI4IdentityNode()
      val rationalNode = TLRationalIdentityNode()

      node := AXI4UserYanker(Some(1)) := TLToAXI4() := TLRationalCrossingSink(SlowToFast) := rationalNode

      lazy val module = new LazyModuleImp(this) {
        override def desiredName: String = "MemFastClockDomain"
      }
    }
    val fastClockDomain = LazyModule(new MemFastClockDomain)
    fastClockDomain.rationalNode := TLRationalCrossingSource() := tlBus
    slave := fastClockDomain.node

    val io_slave = InModuleBody {
      slave.makeIOs()
    }
    val io_master = InModuleBody {
      master.makeIOs()
    }
    lazy val module = new LazyModuleImp(this) {
      val clock_fast = IO(Input(Clock()))
      fastClockDomain.module.clock := clock_fast
    }
  }

  val clock_fast = IO(Input(Clock()))
  val l_adapter = LazyModule(new TopMemoryBusAdapter)
  val adapter = Module(l_adapter.module)
  adapter.clock_fast := clock_fast

  val memory = IO(l_adapter.io_slave.cloneType)
  val top = IO(Flipped(_top.module.memory.cloneType))

  memory <> l_adapter.io_slave
  l_adapter.io_master.elts.foreach(_ := DontCare)
  l_adapter.io_master <> top

  def reMapAddress(addr: UInt): UInt = {
    // Memory: 0x20_0000_0000 --> 0x8000_0000
    addr - (0x2000000000L - 0x80000000L).U
  }

  memory.elts.zip(l_adapter.io_slave.elts).foreach{ case (m, a) =>
    m.ar.bits.addr := reMapAddress(a.ar.bits.addr)
    m.aw.bits.addr := reMapAddress(a.aw.bits.addr)
  }
}

class TopPeripheralAdapter(_top: XSTop)(implicit p: Parameters) extends Module {

  class TopPeripheralBusAdapter()(implicit p: Parameters) extends LazyModule {
    val slave = AXI4SlaveNode(_top.misc.peripheralNode.portParams.map(_.copy(beatBytes = 8)))
    val master = AXI4MasterNode(List(_top.misc.peripheralNode.in.head._2.master))

    val errorDev = LazyModule(new TLError(
      params = DevNullParams(
        address = Seq(AddressSet(0x0L, 0x7fffffffL)),
        maxAtomic = 8,
        maxTransfer = 128
      ),
      beatBytes = 8
    ))
    val tlBus = TLXbar()
    tlBus :=
      TLFIFOFixer() :=
      TLWidthWidget(32) :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      master
    errorDev.node := tlBus
    class PeriFastClockDomain()(implicit p: Parameters) extends LazyModule {
      val node = AXI4IdentityNode()
      val rationalNode = TLRationalIdentityNode()

      node := AXI4UserYanker(Some(1)) := TLToAXI4() := TLRationalCrossingSink(SlowToFast) := rationalNode

      lazy val module = new LazyModuleImp(this) {
        override def desiredName: String = "PeriFastClockDomain"
      }
    }
    val fastClockDomain = LazyModule(new PeriFastClockDomain)
    fastClockDomain.rationalNode := TLRationalCrossingSource() := tlBus
    slave := fastClockDomain.node

    val io_slave = InModuleBody {
      slave.makeIOs()
    }
    val io_master = InModuleBody {
      master.makeIOs()
    }
    lazy val module = new LazyModuleImp(this) {
      val clock_fast = IO(Input(Clock()))
      fastClockDomain.module.clock := clock_fast
    }
  }

  val clock_fast = IO(Input(Clock()))
  val l_adapter = LazyModule(new TopPeripheralBusAdapter)
  val adapter = Module(l_adapter.module)
  adapter.clock_fast := clock_fast

  val peripheral = IO(l_adapter.io_slave.cloneType)
  val top = IO(Flipped(_top.module.peripheral.cloneType))

  peripheral <> l_adapter.io_slave
  l_adapter.io_master.elts.foreach(_ := DontCare)
  l_adapter.io_master <> top

  def reMapAddress(addr: UInt): UInt = {
    // Peripheral:
    // (1) UART: 0x1f_0005_0000 --> 0x4060_0000
    // (2) QSPI: 0x1f_fff8_0000 --> 0x1000_0000
    Mux(addr(31), addr - (0x1ffff80000L - 0x10000000L).U, addr - (0x1f00050000L - 0x40600000L).U)
  }
  peripheral.elts.zip(l_adapter.io_slave.elts).foreach{ case (p, a) =>
    p.ar.bits.addr := reMapAddress(a.ar.bits.addr)
    p.aw.bits.addr := reMapAddress(a.aw.bits.addr)
  }
}

class FPGATop()(implicit p: Parameters) extends RawModule {
  val lazy_module_top = LazyModule(new XSTop)
  val top = Module(lazy_module_top.module)

  val io_adapter = Module(new TopIOAdapter(lazy_module_top))
  io_adapter.top <> top.io
  val io = IO(io_adapter.io.cloneType)
  io <> io_adapter.io

  val memory_adapter = withClockAndReset(io_adapter.top.clock_div2, io.reset) {
    Module(new TopMemoryAdapter(lazy_module_top))
  }
  memory_adapter.top <> top.memory
  memory_adapter.clock_fast := io.clock
  val memory = IO(memory_adapter.memory.cloneType)
  memory <> memory_adapter.memory

  val peripheral_adapter = withClockAndReset(io_adapter.top.clock_div2, io.reset) {
    Module(new TopPeripheralAdapter(lazy_module_top))
  }
  peripheral_adapter.top <> top.peripheral
  peripheral_adapter.clock_fast := io.clock
  val peripheral = IO(peripheral_adapter.peripheral.cloneType)
  peripheral <> peripheral_adapter.peripheral

  val dma = IO(top.dma.cloneType)
  dma.elts.foreach(dontTouch(_))
  dma := DontCare
  top.dma := DontCare

  // Extra bits are DontCare
  top.xsx_fscan := DontCare
  top.xsl2_ultiscan := DontCare
  top.hd2prf_in := DontCare
  top.hsuspsr_in := DontCare
  top.uhdusplr_in := DontCare
  top.hduspsr_in := DontCare
  top.mem := DontCare
  top.l1l2_mbist_sram_jtag := DontCare
  top.bisr_mem_chain_select := DontCare
  if (top.l3_sram_mbist.isDefined) {
    top.L3_BISR.get := DontCare
    top.l3_sram_mbist.get.foreach(_ := DontCare)
  }
}


object FPGATop extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts) = ArgParser.parse(args)
    XiangShanStage.execute(firrtlOpts, Seq(
      ChiselGeneratorAnnotation(() => {
        DisableMonitors(p => new FPGATop()(p))(config)
      })
    ))
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
