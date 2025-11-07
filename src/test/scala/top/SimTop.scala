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

import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3._
import device.{AXI4RAMWrapper, SimJTAG}
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule, LazyModuleImp}
import utils.GTimer
import utility.{ChiselDB, FileRegisters, XSLog}
import xiangshan.{DebugOptions, DebugOptionsKey}
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.debug._
import difftest._

class XiangShanSim(implicit p: Parameters) extends Module with HasDiffTestInterfaces {
  val debugOpts = p(DebugOptionsKey)
  val useDRAMSim = debugOpts.UseDRAMSim

  val l_soc = LazyModule(new XSTop())
  val soc = Module(l_soc.module)

  l_soc.module.dma := DontCare

  val l_simMMIO = LazyModule(new SimMMIO(l_soc.misc.peripheralNode.in.head._2))
  val simMMIO = Module(l_simMMIO.module)
  l_simMMIO.io_axi4 <> soc.peripheral

  if(!useDRAMSim){
    val l_simAXIMem = LazyModule(new AXI4RAMWrapper(
      l_soc.misc.memAXI4SlaveNode, 16L * 1024 * 1024 * 1024, useBlackBox = true
    ))
    val simAXIMem = Module(l_simAXIMem.module)
    l_simAXIMem.io_axi4 <> soc.memory
  }

  soc.io.clock := clock.asBool
  soc.io.reset := (reset.asBool || soc.io.debug_reset).asAsyncReset
  soc.io.extIntrs := simMMIO.io.interrupt.intrVec
  soc.io.sram_config := 0.U
  soc.io.pll0_lock := true.B
  soc.io.cacheable_check := DontCare

  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay=3)(p))
  jtag.connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset.asAsyncReset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)

  val io = IO(new Bundle {
    val uart = new UARTIO
    val memAXI = Option.when(useDRAMSim)(chiselTypeOf(soc.memory))
  })

  io.uart <> simMMIO.io.uart
  if (useDRAMSim) {
    io.memAXI.get <> soc.memory
  }

  // Check and dispaly all source and sink connections
  ExcitingUtils.fixConnections()
  ExcitingUtils.checkAndDisplay()

  override def cpuName: Option[String] = Some("XiangShan")
  override def connectTopIOs(difftest: DifftestTopIO): Seq[chisel3.Data] = {
    difftest.uart <> io.uart

    val hasPerf = !debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug
    val hasLog = !debugOpts.FPGAPlatform && debugOpts.EnableDebug
    val hasPerfLog = hasPerf || hasLog
    val timer = if (hasPerfLog) GTimer() else WireDefault(0.U(64.W))
    val logEnable = if (hasPerfLog) WireDefault(difftest.logCtrl.enable(timer)) else WireDefault(false.B)
    val clean = if (hasPerf) WireDefault(difftest.perfCtrl.clean) else WireDefault(false.B)
    val dump = if (hasPerf) WireDefault(difftest.perfCtrl.dump) else WireDefault(false.B)
    XSLog.collect(timer, logEnable, clean, dump)

    Seq.empty
  }
}

object XiangShanSim extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val enableChiselDB = true
  ChiselDB.init(enableChiselDB)

  // Keep this the same as TopMain except that XiangShanSim is used here instead of XSTop
  Generator.execute(
    firrtlOpts,
    DisableMonitors(p => DifftestModule.top(new XiangShanSim()(p)))(config),
    firtoolOpts
  )

  ChiselDB.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
