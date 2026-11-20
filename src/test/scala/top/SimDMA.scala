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
import chisel3.util._
import device.{AXI4SlaveModule, AXI4SlaveModuleImp}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.AddressSet
import org.chipsalliance.cde.config.Parameters

/** Hardware baseline only; --enable-dma-sim explicitly enables the simulation fixture. */
class DMASystemTestConfig(n: Int = 1) extends KunminghuV2WithoutLLCConfig(n)

object DMASystemTest {
  val base = 0x40080000L
  val bytes = 0x1000L
  val beats = 16
  val events = 12
}

class DMACommand extends Bundle {
  val write = Bool()
  val addr = UInt(64.W)
  val id = UInt(16.W)
  val size = UInt(3.W)
  val len = UInt(8.W)
  val repeats = UInt(16.W)
  val stride = UInt(64.W)
  val mask = UInt(32.W)
  val data = UInt(256.W)
  val stalls = Bool()
}

class DMAFixtureStatus extends Bundle {
  val busy = Bool()
  val done = Bool()
  val error = UInt(16.W)
  val completed = UInt(32.W)
  val readData = Vec(DMASystemTest.beats, UInt(256.W))
}

class DMAFixtureHost extends Bundle {
  val start = Output(Valid(new DMACommand))
  val clear = Output(Bool())
  val watchBase = Output(UInt(64.W))
  val watchMask = Output(UInt(64.W))
  val status = Input(new DMAFixtureStatus)
  val counters = Input(Vec(DMASystemTest.events, UInt(64.W)))
}

/** Software command/result registers, independent of the tested AXI datapath. */
class SimDMAControl(implicit p: Parameters) extends AXI4SlaveModule(
  Seq(AddressSet(DMASystemTest.base, DMASystemTest.bytes - 1)),
  executable = false, _extra = new DMAFixtureHost
) {
  override lazy val module = new AXI4SlaveModuleImp(this) {
    val host = io.extra.get
    val addr = RegInit(0.U(64.W))
    val mask = RegInit("hffffffff".U(32.W))
    val config = RegInit(5.U(64.W))
    val id = RegInit(0.U(16.W))
    val repeats = RegInit(1.U(16.W))
    val stride = RegInit(64.U(64.W))
    val data = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
    val watchBase = RegInit(0.U(64.W))
    val watchMask = RegInit(0.U(64.W))
    val offset = waddr(11, 0)
    val incoming = in.w.bits.data
    host.start.valid := in.w.fire && offset === 8.U
    host.start.bits.write := incoming(0)
    host.start.bits.addr := addr
    host.start.bits.mask := mask
    host.start.bits.size := config(2, 0)
    host.start.bits.len := config(15, 8)
    host.start.bits.stalls := config(16)
    host.start.bits.id := id
    host.start.bits.repeats := repeats
    host.start.bits.stride := stride
    host.start.bits.data := data.asUInt
    host.clear := in.w.fire && offset === "h88".U
    host.watchBase := watchBase
    host.watchMask := watchMask
    when(in.w.fire) {
      assert(in.w.bits.strb.andR, "DMA control registers require aligned 64-bit writes")
      switch(offset) {
        is("h10".U) { addr := incoming }
        is("h18".U) { mask := incoming }
        is("h20".U) { config := incoming }
        is("h28".U) { id := incoming }
        is("h38".U) { repeats := incoming }
        is("h40".U) { stride := incoming }
        is("h80".U) { watchBase := incoming }
        is("h90".U) { watchMask := incoming }
      }
      for(i <- 0 until 4) {
        when(offset === (0x100 + 8*i).U) { data(i) := incoming }
      }
    }
    val readMap = Seq(
      0x00 -> "h444d415445535431".U(64.W), // DMATEST1
      0x10 -> addr, 0x18 -> mask, 0x20 -> config, 0x28 -> id,
      0x30 -> Cat(0.U(40.W), host.status.error, 0.U(6.W), host.status.done, host.status.busy),
      0x38 -> repeats, 0x40 -> stride, 0x48 -> host.status.completed,
      0x80 -> watchBase, 0x90 -> watchMask
    ) ++ (0 until 4).map(i => (0x100 + i*8) -> data(i)) ++
      (0 until DMASystemTest.beats*4).map(i =>
        (0x200 + i*8) -> host.status.readData(i/4)(i%4*64+63, i%4*64)) ++
      (0 until DMASystemTest.events).map(i => (0x400 + i*8) -> host.counters(i))
    in.r.bits.data := MuxLookup(raddr(11, 0), 0.U(64.W))(readMap.map { case (a,d) => a.U -> d })
  }
}

/** A transaction driver, not a DMA controller model. Every access enters soc.dma. */
class SimDMAAXIMaster(params: AXI4BundleParameters) extends Module {
  require(params.dataBits == 256)
  val io = IO(new Bundle {
    val axi = new AXI4Bundle(params)
    val start = Flipped(Valid(new DMACommand))
    val status = Output(new DMAFixtureStatus)
  })
  val idle :: issue :: response :: Nil = Enum(3)
  val state = RegInit(idle)
  val command = Reg(new DMACommand)
  val address = Reg(UInt(64.W))
  val awDone = RegInit(false.B)
  val wDone = RegInit(false.B)
  val beat = RegInit(0.U(8.W))
  val completed = RegInit(0.U(32.W))
  val done = RegInit(false.B)
  val error = RegInit(0.U(16.W))
  val readData = Reg(Vec(DMASystemTest.beats, UInt(256.W)))
  val tick = RegInit(0.U(8.W))
  tick := tick + 1.U
  io.status.busy := state =/= idle
  io.status.done := done
  io.status.error := error
  io.status.completed := completed
  io.status.readData := readData

  io.axi.aw.valid := state === issue && command.write && !awDone
  io.axi.w.valid := state === issue && command.write && !wDone
  io.axi.ar.valid := state === issue && !command.write
  io.axi.aw.bits := 0.U.asTypeOf(io.axi.aw.bits)
  io.axi.ar.bits := 0.U.asTypeOf(io.axi.ar.bits)
  for(a <- Seq(io.axi.aw.bits, io.axi.ar.bits)) {
    a.addr := address
    a.id := command.id
    a.size := command.size
    a.len := command.len
    a.burst := AXI4Parameters.BURST_INCR
  }
  io.axi.w.bits := 0.U.asTypeOf(io.axi.w.bits)
  io.axi.w.bits.data := command.data
  io.axi.w.bits.strb := command.mask
  io.axi.w.bits.last := beat === command.len
  val acceptResponse = !command.stalls || tick(2, 0) >= 3.U
  io.axi.r.ready := state === response && !command.write && acceptResponse
  io.axi.b.ready := state === response && command.write && acceptResponse

  when(io.start.valid) {
    assert(state === idle, "Software launched a DMA command while busy")
    assert(io.start.bits.repeats =/= 0.U)
    assert(io.start.bits.len < DMASystemTest.beats.U)
    command := io.start.bits
    address := io.start.bits.addr
    awDone := false.B
    wDone := false.B
    beat := 0.U
    completed := 0.U
    error := 0.U
    done := false.B
    state := issue
  }
  when(io.axi.aw.fire) { awDone := true.B }
  when(io.axi.w.fire) {
    beat := beat + 1.U
    when(io.axi.w.bits.last) { wDone := true.B }
  }
  when(state === issue && command.write && (awDone || io.axi.aw.fire) &&
    (wDone || io.axi.w.fire && io.axi.w.bits.last)) {
    state := response
    beat := 0.U
  }
  when(io.axi.ar.fire) { state := response; beat := 0.U }

  val transferDone = io.axi.b.fire || io.axi.r.fire && io.axi.r.bits.last
  when(io.axi.r.fire) {
    readData(beat(log2Ceil(DMASystemTest.beats)-1, 0)) := io.axi.r.bits.data
    beat := beat + 1.U
    when(io.axi.r.bits.id =/= command.id || io.axi.r.bits.last =/= (beat === command.len)) {
      error := error | "h100".U
    }
    when(io.axi.r.bits.resp =/= AXI4Parameters.RESP_OKAY) {
      error := error | io.axi.r.bits.resp
    }
  }
  when(io.axi.b.fire) {
    when(io.axi.b.bits.id =/= command.id) { error := error | "h200".U }
    when(io.axi.b.bits.resp =/= AXI4Parameters.RESP_OKAY) {
      error := error | (io.axi.b.bits.resp << 4)
    }
  }
  when(transferDone) {
    completed := completed + 1.U
    when(completed + 1.U === command.repeats) {
      state := idle
      done := true.B
    }.otherwise {
      address := address + command.stride
      awDone := false.B
      wDone := false.B
      beat := 0.U
      state := issue
    }
  }
  // AXI responses must remain stable while the testbench applies backpressure.
  for(channel <- Seq(io.axi.r, io.axi.b)) {
    val stalled = RegNext(channel.valid && !channel.ready, false.B)
    val payload = RegEnable(channel.bits.asUInt, channel.valid && !channel.ready)
    when(stalled) {
      assert(channel.valid && channel.bits.asUInt === payload, "DMA AXI response changed under backpressure")
    }
  }
}
