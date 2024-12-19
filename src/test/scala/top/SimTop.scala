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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import device.{AXI4MemorySlave, SimJTAG}
import difftest._
import difftest.common.{DifftestMemReadIO, DifftestMemWriteIO, HasTopMemoryMasterPort}
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import utility.{ChiselDB, Constantin, FileRegisters, GTimer, MaskExpand}
import xiangshan.DebugOptionsKey

class XiangShan(implicit p: Parameters) extends Module with HasTopMemoryMasterPort {
  val l_soc = LazyModule(new XSTop())
  val top = Module(l_soc.module)

  val io = IO(chiselTypeOf(top.io))
  val dma = IO(chiselTypeOf(top.dma))
  val peripheral = IO(chiselTypeOf(top.peripheral))
  val memory = IO(chiselTypeOf(top.memory))

  io <> top.io
  top.io.clock := clock.asBool
  top.io.reset := reset.asAsyncReset
  dma <> top.dma
  peripheral <> top.peripheral
  memory <> top.memory

  override def getTopMemoryMasterRead: DifftestMemReadIO = {
    val mem = chisel3.util.experimental.BoringUtils.tapAndRead(memory.elts.head)
    val req_valid = RegInit(false.B)
    val req_bits = Reg(chiselTypeOf(mem.ar.bits))
    val req_beat_count = Reg(UInt(8.W))
    assert(!mem.r.fire || (req_valid && mem.r.bits.id === req_bits.id), "data fire but req mismatch")
    when (mem.ar.fire) {
      req_valid := true.B
      req_bits := mem.ar.bits
      // wrap here for burst
      val wrap_mask = ~(mem.ar.bits.len << mem.ar.bits.size).asTypeOf(UInt(mem.ar.bits.addr.getWidth.W))
      req_bits.addr := mem.ar.bits.addr & wrap_mask.asUInt
      req_beat_count := (mem.ar.bits.addr >> mem.ar.bits.size).asUInt & mem.ar.bits.len
      assert(!req_valid || mem.r.fire && mem.r.bits.last, "multiple inflight not supported")
    }.elsewhen(mem.r.fire) {
      val should_wrap = req_bits.burst === 2.U && req_beat_count === req_bits.len
      req_beat_count := Mux(should_wrap, 0.U, req_beat_count + 1.U)
      when (mem.r.bits.last) {
        req_valid := false.B
      }
    }
    val read = Wire(Output(new DifftestMemReadIO(mem.r.bits.data.getWidth / 64)))
    read.valid := mem.r.fire
    read.index := (req_bits.addr >> log2Ceil(mem.r.bits.data.getWidth / 8 - 1)) + req_beat_count
    read.data := mem.r.bits.data.asTypeOf(read.data)
    read
  }

  override def getTopMemoryMasterWrite: DifftestMemWriteIO = {
    val mem = chisel3.util.experimental.BoringUtils.tapAndRead(memory.elts.head)
    val req_bits_reg = Reg(chiselTypeOf(mem.aw.bits))
    val req_bits = Mux(mem.aw.fire, mem.aw.bits, req_bits_reg)
    when (mem.aw.fire) {
      req_bits_reg := mem.aw.bits
      when (mem.w.fire) {
        req_bits_reg.addr := mem.aw.bits.addr + (mem.w.bits.data.getWidth / 8).U
      }
    }.elsewhen(mem.w.fire) {
      req_bits_reg.addr := req_bits.addr + (mem.w.bits.data.getWidth / 8).U
    }
    val write = Wire(Output(new DifftestMemWriteIO(mem.w.bits.data.getWidth / 64)))
    write.valid := mem.w.fire
    write.index := req_bits.addr >> log2Ceil(mem.w.bits.data.getWidth / 8 - 1)
    write.data := mem.w.bits.data.asTypeOf(write.data)
    write.mask := MaskExpand(mem.w.bits.strb).asTypeOf(write.mask)
    write
  }
}

class SimTop(implicit p: Parameters) extends Module {
  val debugOpts = p(DebugOptionsKey)

  val soc = DifftestModule.designTop(Module(new XiangShan))
  // Don't allow the top-level signals to be optimized out,
  // so that we can re-use this SimTop for any generated Verilog RTL.
  dontTouch(soc.io)

  soc.dma <> 0.U.asTypeOf(soc.dma)

  val l_simMMIO = LazyModule(new SimMMIO(soc.l_soc.misc.peripheralNode.in.head._2))
  val simMMIO = Module(l_simMMIO.module)
  l_simMMIO.io_axi4 <> soc.peripheral

  val l_simAXIMem = AXI4MemorySlave(
    soc.l_soc.misc.memAXI4SlaveNode,
    2L * 1024 * 1024 * 1024,
    useBlackBox = true,
    dynamicLatency = debugOpts.UseDRAMSim
  )
  val simAXIMem = Module(l_simAXIMem.module)
  l_simAXIMem.io_axi4.getWrappedValue :<>= soc.memory.waiveAll

  soc.io.clock := clock.asBool
  soc.io.reset := reset.asAsyncReset
  soc.io.extIntrs := simMMIO.io.interrupt.intrVec
  soc.io.sram_config := 0.U
  soc.io.pll0_lock := true.B
  soc.io.cacheable_check := DontCare
  soc.io.riscv_rst_vec.foreach(_ := 0x10000000L.U)

  // soc.io.rtc_clock is a div100 of soc.io.clock
  val rtcClockDiv = 100
  val rtcTickCycle = rtcClockDiv / 2
  val rtcCounter = RegInit(0.U(log2Ceil(rtcTickCycle + 1).W))
  rtcCounter := Mux(rtcCounter === (rtcTickCycle - 1).U, 0.U, rtcCounter + 1.U)
  val rtcClock = RegInit(false.B)
  when (rtcCounter === 0.U) {
    rtcClock := ~rtcClock
  }
  soc.io.rtc_clock := rtcClock

  val success = Wire(Bool())
  val jtag = Module(new SimJTAG(tickDelay=3)(p)).connect(soc.io.systemjtag.jtag, clock, reset.asBool, !reset.asBool, success)
  soc.io.systemjtag.reset := reset.asAsyncReset
  soc.io.systemjtag.mfr_id := 0.U(11.W)
  soc.io.systemjtag.part_number := 0.U(16.W)
  soc.io.systemjtag.version := 0.U(4.W)

  DifftestModule.atSimTop { difftest =>
    simMMIO.io.uart <> difftest.uart

    val hasPerf = !debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug
    val hasLog = !debugOpts.FPGAPlatform && debugOpts.EnableDebug
    val hasPerfLog = hasPerf || hasLog
    val timer = if (hasPerfLog) GTimer() else WireDefault(0.U(64.W))
    val logEnable = if (hasPerfLog) WireDefault(difftest.logCtrl.enable(timer)) else WireDefault(false.B)
    val clean = if (hasPerf) WireDefault(difftest.perfCtrl.clean) else WireDefault(false.B)
    val dump = if (hasPerf) WireDefault(difftest.perfCtrl.dump) else WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)
  }
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
