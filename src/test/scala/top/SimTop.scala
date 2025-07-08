/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import device.{AXI4MemorySlave, SimJTAG}
import difftest._
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import freechips.rocketchip.util.HeterogeneousBag
import utility.{ChiselDB, Constantin, FileRegisters, GTimer, XSLog}
import xiangshan.DebugOptionsKey
import system.SoCParamsKey

class SimTop(implicit p: Parameters) extends Module {
  val debugOpts = p(DebugOptionsKey)

  val l_soc = LazyModule(new XSTop())
  val soc = Module(l_soc.module)
  // Don't allow the top-level signals to be optimized out,
  // so that we can re-use this SimTop for any generated Verilog RTL.
  dontTouch(soc.io)

  if (!l_soc.module.dma.isEmpty) {
    l_soc.module.dma.get <> WireDefault(0.U.asTypeOf(l_soc.module.dma.get))
  }

  val l_simMMIO = LazyModule(new SimMMIO(l_soc.misc.peripheralNode.in.head._2)(p.alter((site, here, up) => {
    case SoCParamsKey => up(SoCParamsKey).copy(UARTLiteForDTS = false)
  })))
  val simMMIO = Module(l_simMMIO.module)
  l_simMMIO.io_axi4.elements.head._2 <> soc.peripheral.viewAs[AXI4Bundle]

  val l_simAXIMem = AXI4MemorySlave(
    l_soc.misc.memAXI4SlaveNode,
    8190L * 1024 * 1024 * 1024,
    useBlackBox = true,
    dynamicLatency = debugOpts.UseDRAMSim
  )
  val simAXIMem = Module(l_simAXIMem.module)
  l_simAXIMem.io_axi4.elements.head._2 :<>= soc.memory.viewAs[AXI4Bundle].waiveAll

  soc.io.clock := clock
  soc.io.reset := (reset.asBool || soc.io.debug_reset).asAsyncReset
  soc.io.extIntrs := simMMIO.io.interrupt.intrVec
  soc.io.sram_config := 0.U
  soc.io.pll0_lock := true.B
  soc.io.cacheable_check := DontCare
  soc.io.riscv_rst_vec.foreach(_ := 0x10000000L.U)
  l_soc.nmi.foreach(_.foreach(intr => { intr := false.B; dontTouch(intr) }))
  soc.io.traceCoreInterface.foreach(_.fromEncoder.enable := false.B)
  soc.io.traceCoreInterface.foreach(_.fromEncoder.stall  := false.B)

  // soc.io.rtc_clock is a div100 of soc.io.clock
  val rtcClockDiv = 100
  val rtcTickCycle = rtcClockDiv / 2
  val rtcCounter = RegInit(0.U(log2Ceil(rtcTickCycle + 1).W))
  rtcCounter := Mux(rtcCounter === (rtcTickCycle - 1).U, 0.U, rtcCounter + 1.U)
  val rtcClock = RegInit(false.B)
  when (rtcCounter === 0.U) {
    rtcClock := ~rtcClock
  }
  soc.io.rtc_clock := rtcClock.asClock

  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay = 3)(p))
  jtag.connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset.asAsyncReset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)

  val difftest = DifftestModule.finish("XiangShan")

  simMMIO.io.uart <> difftest.uart

  val hasPerf = !debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug
  val hasLog = !debugOpts.FPGAPlatform && debugOpts.EnableDebug
  val hasPerfLog = hasPerf || hasLog
  val timer = if (hasPerfLog) GTimer() else WireDefault(0.U(64.W))
  val logEnable = if (hasPerfLog) WireDefault(difftest.logCtrl.enable(timer)) else WireDefault(false.B)
  val clean = if (hasPerf) WireDefault(difftest.perfCtrl.clean) else WireDefault(false.B)
  val dump = if (hasPerf) WireDefault(difftest.perfCtrl.dump) else WireDefault(false.B)

  XSLog.collect(timer, logEnable, clean, dump)
}

object SimTop extends App {
  // Keep this the same as TopMain except that SimTop is used here instead of XSTop
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  Generator.execute(
    firrtlOpts,
    DisableMonitors(p => new SimTop()(p))(config),
    firtoolOpts
  )

  // tools: write cpp files
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
