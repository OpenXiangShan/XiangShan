// See LICENSE.bosc for license details.

package device

import chisel3._
import chisel3.util.ShiftRegister
import chisel3.util.ValidIO
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.devices.debug.RWNotify
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import chisel3.util._

import scala.collection.immutable.Seq

object SYSCNTConsts {

  def timeOffset   = 0xbff8//0xbff8 base addr is 0x8000
  def timefreq     = 0xC000//0xC000:0x8000+0x4000
  def timefreqReq  = timefreq + 0x0008
  def timeswReq  = timefreq + 0x0010
  def size         = 0x10000
  def timeWidth    = 64
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
      val update_en = Input(Bool())
      val update_value = Input(UInt(timeWidth.W))
      val stop_en = Input(Bool())
      val time = Output(ValidIO(UInt(timeWidth.W)))
    })
    dontTouch(io)
    // increasing time define working on rtc_clock
    val time = withClockAndReset(rtc_clock, rtc_reset){RegInit(0.U(timeWidth.W))}
    val time_sw = withClockAndReset(bus_clock, bus_reset){RegInit(0.U(timeWidth.W))} //software config time register
    val time_sw_req_byte = withClockAndReset(bus_clock, bus_reset){RegInit(0.U(8.W))}
    val time_sw_req = time_sw_req_byte(0)
    val time_sw_vld = withClockAndReset(bus_clock, bus_reset){RegInit(false.B)}
    //register define working on bus clock
    //incfreq bit[1:0] is incr width 0: 1GHz,1:500MHz,2:250MHz,3:125MHz,4:62.5MHz,..
    val incfreq = withClockAndReset(bus_clock, bus_reset){RegInit(0.U(8.W))}
    val incfreq_update_byte = withClockAndReset(bus_clock, bus_reset){RegInit(0.U(8.W))}
    val incfreq_update = incfreq_update_byte(0)
    //async process about soc signal
    val update_sync = withClockAndReset(rtc_clock,rtc_reset){AsyncResetSynchronizerShiftReg(io.update_en, 3, 0)}
    val stop_sync = withClockAndReset(rtc_clock,rtc_reset){AsyncResetSynchronizerShiftReg(io.stop_en, 3, 0)}
    val incfreq_update_rtc = withClockAndReset(rtc_clock,rtc_reset){AsyncResetSynchronizerShiftReg(incfreq_update, 3, 0)}
    val time_sw_req_rtc = withClockAndReset(rtc_clock, rtc_reset){AsyncResetSynchronizerShiftReg(time_sw_req, 3, 0)}
    // generate incfreq_update_rtc's rising edge
    val incfreq_update_rtc_1f = withClockAndReset(rtc_clock,rtc_reset){RegNext(incfreq_update_rtc, init=false.B)}
    val inccfg_vld = incfreq_update_rtc & (!incfreq_update_rtc_1f)
    val time_req_rtc_1f = withClockAndReset(rtc_clock,rtc_reset){RegNext(time_sw_req_rtc, init=false.B)}
    val time_req_rtc_ris = time_sw_req_rtc & (!time_req_rtc_1f)
    //async process from rtc clock to bus clock
    val time_sw_update_bus = withClockAndReset(bus_clock, bus_reset){AsyncResetSynchronizerShiftReg(time_req_rtc_ris, 3, 0)}
    when(time_sw_update_bus)(time_sw_req_byte := false.B)
    // inc freq will be active after incfreq request is set from 0 to 1.
    val incwidth = WireInit(0.U(3.W))
    incwidth := withClockAndReset(rtc_clock,rtc_reset){RegEnable((incfreq(2,0)), inccfg_vld)}
    val inc_up_dis = withClockAndReset(rtc_clock,rtc_reset){RegInit(false.B)}
    //generate the low bit: time_low= time[incwidth-1:0]
    val time_low = WireInit(1.U(7.W))
    switch(incwidth) { //bus clock
      is(1.U) {
        time_low := time(0)
      }
      is(2.U) {
        time_low := time(1, 0)
      }
      is(3.U) {
        time_low := time(2, 0)
      }
      is(4.U) {
        time_low := time(3, 0)
      }
      is(5.U) {
        time_low := time(4, 0)
      }
      is(6.U) {
        time_low := time(5, 0)
      }
      is(7.U) {
        time_low := time(6, 0)
      }
      is(0.U) {
        time_low := 1.U
      }
    }
    val inczero = WireInit(false.B) //rtc clock
    when(incwidth === 0.U) {
      inczero := true.B
    }.otherwise {
      inczero := false.B
    }
    val timelow_zero = WireInit(false.B)
    when(time_low === 0.U) {
      timelow_zero := true.B
    }.otherwise {
      timelow_zero := false.B
    }
    val inc_update = inc_up_dis & (inczero | timelow_zero)
    // count step will not update before count arrive at 2^n,n is increg
    when(inccfg_vld) {
      inc_up_dis := true.B
    }.elsewhen(inc_update) {
      inc_up_dis := false.B
    }

    //update the increasing step only when counter arrivals integer times of step config.
    val incr_width_value = withClockAndReset(rtc_clock,rtc_reset){RegInit(0.U(3.W))}
    when(inc_update) {
      incr_width_value := incwidth
    }
    // async process about inc_update
    val inc_update_bus = withClockAndReset(bus_clock,bus_reset){AsyncResetSynchronizerShiftReg(inc_update, 3, 0)}
    when(inc_update_bus){incfreq_update_byte := false.B} //incfreq update cfg is cleared auto by hardware.
    val time_en = withClockAndReset(rtc_clock,rtc_reset){RegInit(false.B)}
    val incwidth_mux = Mux(inc_update,incwidth,incr_width_value)

    when(stop_sync) {
      time_en := false.B
    }.otherwise(time_en := true.B)

    when(time_req_rtc_ris){
      time := time_sw
    }.elsewhen(stop_sync){
      time := time
    }.elsewhen(update_sync){
      time := io.update_value
    }.otherwise {
      time := time + (1.U << incwidth_mux)
    }
    io.time.bits := time
    io.time.valid := time_en
    //time working on rtc clock is to be synced with bus clock
    val timeasync = withClockAndReset(bus_clock, bus_reset)(Module(new TimeAsync()))
    val time_rpt_bus = timeasync.io.o_time.bits
    val time_rpt_rd = RegInit(false.B)

    timeasync.io.i_time := io.time
    /* 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * bffc mtime hi
     */
    node.regmap(
      timefreq -> RegFieldGroup(
        "timefreq",
        Some("mtime frequency Register"),
        RegField.bytes(incfreq, Some(RegFieldDesc("timefreq", "", reset = Some(0), volatile = true)))
      ),
      timefreqReq -> RegFieldGroup(
        "timefreqReq",
        Some("mtime frequency update Request Register"),
        RegField.bytes(incfreq_update_byte, Some(RegFieldDesc("timefreqReq", "", reset = Some(0), volatile = true)))
      ),
      timeswReq -> RegFieldGroup(
        "timeswReq",
        Some("mtime software update Request Register"),
        RegField.bytes(time_sw_req_byte, Some(RegFieldDesc("timeswReq", "", reset = Some(0), volatile = true)))
      ),
      timeOffset -> RegFieldGroup(
        "mtime",
        Some("Timer Register"),
        Seq(RWNotify(64,time_rpt_bus,time_sw,time_rpt_rd,time_sw_vld,Some(RegFieldDesc("timeswReq", "", reset = Some(0), volatile = true)))))
    )
  }
}
