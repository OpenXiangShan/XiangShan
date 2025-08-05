/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package device

import chisel3._
import chisel3.util._
import chisel3.util.ShiftRegister
import chisel3.util.ValidIO
import freechips.rocketchip.devices.debug.RWNotify
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import scala.collection.immutable.Seq

object SYSCNTConsts {

  def timeOffset  = 0xbff8 // 0xbff8 base addr is 0x8000
  def timefreq    = 0xc000 // 0xC000:0x8000+0x4000
  def timefreqReq = timefreq + 0x0008
  def timeswReq   = timefreq + 0x0010
  def size        = 0x10000
  def timeWidth   = 64
}

case class SYSCNTParams(baseAddress: BigInt = 0x02000000, intStages: Int = 0) {
  def address = AddressSet(baseAddress, SYSCNTConsts.size - 1)
}

case object SYSCNTKey extends Field[Option[SYSCNTParams]](None)

case class SYSCNTAttachParams(
    slaveWhere: TLBusWrapperLocation = CBUS
)

case object SYSCNTAttachKey extends Field(SYSCNTAttachParams())

class SYSCNT(params: SYSCNTParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  import SYSCNTConsts._

  // clint0 => at most 4095 devices
  val device = new SimpleDevice("clint", Seq("riscv,clint0")) {
    override val alwaysExtended = true
  }

  val node: TLRegisterNode = TLRegisterNode(
    address = Seq(params.address),
    device = device,
    beatBytes = beatBytes
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    Annotated.params(this, params)

    val rtc_clock = IO(Input(Clock()))
    val rtc_reset = IO(Input(AsyncReset()))
    val bus_clock = IO(Input(Clock()))
    val bus_reset = IO(Input(AsyncReset()))
    val io = IO(new Bundle {
      val update_en    = Input(Bool())
      val update_value = Input(UInt(timeWidth.W))
      val stop_en      = Input(Bool())
      val time         = Output(ValidIO(UInt(timeWidth.W)))
    })
    dontTouch(io)
    // increasing time define working on rtc_clock
    val time    = withClockAndReset(rtc_clock, rtc_reset)(RegInit(0.U(timeWidth.W)))
    val time_sw = withClockAndReset(bus_clock, bus_reset)(RegInit(0.U(timeWidth.W))) // software config time register
    val time_sw_req = withClockAndReset(bus_clock, bus_reset)(RegInit(false.B))
    // register define working on bus clock
    // freqidx bit[1:0] is incr width 0: 1GHz,1:500MHz,2:250MHz,3:125MHz,4:62.5MHz,..
    val freqidx     = withClockAndReset(bus_clock, bus_reset)(RegInit(0.U(3.W)))
    val freqidx_req = withClockAndReset(bus_clock, bus_reset)(RegInit(false.B))
    // async process about soc signal
    val update_sync     = withClockAndReset(rtc_clock, rtc_reset)(AsyncResetSynchronizerShiftReg(io.update_en, 3, 0))
    val stop_sync       = withClockAndReset(rtc_clock, rtc_reset)(AsyncResetSynchronizerShiftReg(io.stop_en, 3, 0))
    val freqidx_req_rtc = withClockAndReset(rtc_clock, rtc_reset)(AsyncResetSynchronizerShiftReg(freqidx_req, 3, 0))
    val time_sw_req_rtc = withClockAndReset(rtc_clock, rtc_reset)(AsyncResetSynchronizerShiftReg(time_sw_req, 3, 0))
    // generate freqidx_req_rtc's rising edge
    val freqidx_req_rtc_1f = withClockAndReset(rtc_clock, rtc_reset)(RegNext(freqidx_req_rtc, init = false.B))
    val inccfg_vld         = freqidx_req_rtc & (!freqidx_req_rtc_1f)
    val time_req_rtc_1f    = withClockAndReset(rtc_clock, rtc_reset)(RegNext(time_sw_req_rtc, init = false.B))
    val time_req_rtc_ris   = time_sw_req_rtc & (!time_req_rtc_1f)
    // async process from rtc clock to bus clock
    val time_sw_update_bus =
      withClockAndReset(bus_clock, bus_reset)(AsyncResetSynchronizerShiftReg(time_req_rtc_ris, 3, 0))
    when(time_sw_update_bus)(time_sw_req := false.B)
    // inc freq will be active after freqidx request is set from 0 to 1.
    val incwidth = WireInit(0.U(3.W))
    incwidth := withClockAndReset(rtc_clock, rtc_reset)(RegEnable(freqidx, inccfg_vld))
    val inc_up_dis = withClockAndReset(rtc_clock, rtc_reset)(RegInit(false.B))
    // generate the low bit: time_low= time[incwidth-1:0]
    val time_low = Mux(incwidth === 0.U, 1.U, time & ((1.U << incwidth) - 1.U))

    val inczero      = incwidth === 0.U
    val timelow_zero = time_low === 0.U

    val inc_update = inc_up_dis & (inczero | timelow_zero)
    // count step will not update before count arrive at 2^n,n is increg
    when(inccfg_vld) {
      inc_up_dis := true.B
    }.elsewhen(inc_update) {
      inc_up_dis := false.B
    }

    // update the increasing step only when counter arrivals integer times of step config.
    val incr_width_value = withClockAndReset(rtc_clock, rtc_reset)(RegInit(0.U(3.W)))
    when(inc_update) {
      incr_width_value := incwidth
    }
    // async process about inc_update
    val inc_update_bus = withClockAndReset(bus_clock, bus_reset)(AsyncResetSynchronizerShiftReg(inc_update, 3, 0))
    when(inc_update_bus) {
      freqidx_req := false.B
    } // freqidx update cfg is cleared auto by hardware.
    val time_en      = withClockAndReset(rtc_clock, rtc_reset)(RegInit(false.B))
    val incwidth_mux = Mux(inc_update, incwidth, incr_width_value)

    when(stop_sync) {
      time_en := false.B
    }.otherwise {
      time_en := true.B
    }

    when(time_req_rtc_ris) {
      time := time_sw
    }.elsewhen(stop_sync) {
      time := time
    }.elsewhen(update_sync) {
      time := io.update_value
    }.otherwise {
      time := time + (1.U << incwidth_mux)
    }
    io.time.bits  := time
    io.time.valid := time_en
    // time working on rtc clock is to be synced with bus clock
    val timeasync    = withClockAndReset(bus_clock, bus_reset)(Module(new TimeAsync()))
    val time_rpt_bus = timeasync.io.o_time.bits

    timeasync.io.i_time := io.time
    /* 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * bffc mtime hi
     */
    // no use, only statement for syntax.
    val time_rpt_rd    = WireInit(false.B)
    val time_sw_vld    = WireInit(false.B)
    val freqidx_rd     = WireInit(false.B)
    val freqidx_wr     = WireInit(false.B)
    val freqidx_req_rd = WireInit(false.B)
    val freqidx_req_wr = WireInit(false.B)
    val mtime_req_rd   = WireInit(false.B)
    val mtime_req_wr   = WireInit(false.B)
    //
    node.regmap(
      timeOffset -> RegFieldGroup(
        "mtime",
        Some("Timer Register"),
        Seq(RWNotify(
          64,
          time_rpt_bus,
          time_sw,
          time_rpt_rd,
          time_sw_vld,
          Some(RegFieldDesc("timeswReq", "", reset = Some(0), volatile = true))
        ))
      ),
      timefreq -> RegFieldGroup(
        "freqidx",
        Some("mtime frequency Register"),
        Seq(RWNotify(
          3,
          freqidx,
          freqidx,
          freqidx_rd,
          freqidx_wr,
          Some(RegFieldDesc("freqidx", "", reset = Some(0), volatile = true))
        ))
      ),
      timefreqReq -> RegFieldGroup(
        "freqidxReq",
        Some("mtime frequency update Request Register"),
        Seq(RWNotify(
          1,
          freqidx_req,
          freqidx_req,
          freqidx_req_rd,
          freqidx_req_wr,
          Some(RegFieldDesc("freqidxReq", "", reset = Some(0), volatile = true))
        ))
      ),
      timeswReq -> RegFieldGroup(
        "mtimeReq",
        Some("mtime software update Request Register"),
        Seq(RWNotify(
          1,
          time_sw_req,
          time_sw_req,
          mtime_req_rd,
          mtime_req_wr,
          Some(RegFieldDesc("mtime_req", "", reset = Some(0), volatile = true))
        ))
      )
    )
  }
}
