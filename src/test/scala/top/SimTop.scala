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
import utils.{FileRegisters, GTimer, HardenXSPerfAccumulate}
import xiangshan.{DebugOptions, DebugOptionsKey}
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.debug._
import difftest._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import utils.HardenXSPerfAccumulate.generateCppParser
import xiangshan.DebugOptionsKey

class SimTop(implicit p: Parameters) extends Module {
  val debugOpts = p(DebugOptionsKey)
  val useDRAMSim = debugOpts.UseDRAMSim

  val l_soc = LazyModule(new XSTop())
  val soc = Module(l_soc.module)
  // Don't allow the top-level signals to be optimized out,
  // so that we can re-use this SimTop for any generated Verilog RTL.
  dontTouch(soc.io)

  l_soc.module.dma <> 0.U.asTypeOf(l_soc.module.dma)

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

  val io = IO(new Bundle(){
    val logCtrl = new LogCtrlIO
    val perfInfo = new PerfInfoIO
    val uart = new UARTIO
    val memAXI = if(useDRAMSim) soc.memory.cloneType else null
    val dse_rst = Input(Bool())
    val instrCnt = Output(UInt(64.W))
    val reset_vector = Input(UInt(36.W))
    val dse_reset_valid = Output(Bool())
    val dse_reset_vec = Output(UInt(36.W))
    val dse_max_epoch = Output(UInt(64.W))
    val dse_epoch = Output(UInt(64.W))
    val dse_max_instr = Output(UInt(64.W))
  })

  simMMIO.io.uart <> io.uart
  soc.io.dse_rst := io.dse_rst
  // ExcitingUtils.addSink(io.instrCnt, "DSE_INSTRCNT")
  io.instrCnt := soc.io.instrCnt
  soc.io.reset_vector := io.reset_vector
  io.dse_reset_valid := soc.io.dse_reset_valid
  io.dse_reset_vec := soc.io.dse_reset_vec
  io.dse_max_epoch := soc.io.dse_max_epoch
  io.dse_epoch := soc.io.dse_epoch
  io.dse_max_instr := soc.io.dse_max_instr

  if(useDRAMSim){
    io.memAXI <> soc.memory
  }

  // if (!debugOpts.FPGAPlatform && (debugOpts.EnableDebug || debugOpts.EnablePerfDebug)) {
  //   val timer = GTimer()
  //   val logEnable = (timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end)
  //   ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
  //   ExcitingUtils.addSource(timer, "logTimestamp")
  // }

  // if (!debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug) {
  //   val clean = io.perfInfo.clean
  //   val dump = io.perfInfo.dump
  //   ExcitingUtils.addSource(clean, "XSPERF_CLEAN")
  //   ExcitingUtils.addSource(dump, "XSPERF_DUMP")
  // }

  // // Check and dispaly all source and sink connections
  // ExcitingUtils.fixConnections()
  // ExcitingUtils.checkAndDisplay()

  val hasPerf = !debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug
  val hasLog = !debugOpts.FPGAPlatform && debugOpts.EnableDebug
  val hasPerfLog = hasPerf || hasLog
  val timer = if (hasPerfLog) GTimer() else WireDefault(0.U(64.W))
  val logEnable = if (hasPerfLog) WireDefault((timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end)) else WireDefault(false.B)
  val clean = if (hasPerf) WireDefault(io.perfInfo.clean) else WireDefault(false.B)
  val dump = if (hasPerf) WireDefault(io.perfInfo.dump) else WireDefault(false.B)

  lazy val io_perf = HardenXSPerfAccumulate.reclaim()

  dontTouch(timer)
  dontTouch(logEnable)
  dontTouch(clean)
  dontTouch(dump)
  dontTouch(io_perf)
}

object SimTop extends App {
  // Keep this the same as TopMain except that SimTop is used here instead of XSTop
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform

  Generator.execute(
    firrtlOpts,
    DisableMonitors(p => new SimTop()(p))(config),
    firtoolOpts
  )

  // tools: write cpp files
  FileRegisters.write(fileDir = "./build")
}
