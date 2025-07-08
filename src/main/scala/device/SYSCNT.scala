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
import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import chisel3.util._

object SYSCNTConsts {

  def timeOffset   = 0xbff8//0xbff8 base addr is 0x8000
  def incOffset    = 0xC000//0xC000:0x8000+0x4000
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

    val io = IO(new Bundle {
      val update_en = Input(Bool())
      val update_value = Input(UInt(timeWidth.W))
      val stop_en = Input(Bool())
      val time = Output(UInt(timeWidth.W))
      val time_freq = Output(UInt(3.W)) // 0: 1GHz,1:500MHz,2:250MHz,3:125MHz,4:62.5MHz,..
    })

    val time = RegInit(0.U(timeWidth.W))
    val increg = RegInit(0.U(8.W))
    val incwidth = increg(2,0) //bit[1:0] is incr width
    val inccutdly = RegNext(incwidth)
    val inccfg_vld = inccutdly =/= incwidth // flag is high firstly when incr update.
    val inc_up_dis = RegInit(false.B)
    val stopen = io.stop_en
    io.time_freq := incwidth
    //generate the low bit: time_low= time[incwidth-1:0]
    val time_low = WireInit(1.U(7.W))
    switch(incwidth) {
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
    val inczero = WireInit(false.B)
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

    val incr_width_value = RegInit(0.U(2.W))
    when(inc_update) {
      incr_width_value := incwidth
    }
    // count step will not update before count arrive at 2^n,n is increg
    when(inccfg_vld) {
      inc_up_dis := true.B
    }.elsewhen(inc_update) {
      inc_up_dis := false.B
    }
    val time_sw = RegInit(0.U(timeWidth.W))
    when(stopen) {
      time := time
    }.elsewhen(io.update_en) {
      time := io.update_value
      time_sw := io.update_value
    }.otherwise{
      time := time + 1.U
      time_sw := time << incr_width_value
    }
    io.time := time_sw
    /* 0000 msip hart 0
     * 0004 msip hart 1
     * 4000 mtimecmp hart 0 lo
     * 4004 mtimecmp hart 0 hi
     * 4008 mtimecmp hart 1 lo
     * bffc mtime hi
     */

    node.regmap(
      incOffset -> RegFieldGroup(
        "incwidth",
        Some("mtime incwidth Register"),
        RegField.bytes(increg, Some(RegFieldDesc("incwidth", "", reset = Some(0), volatile = true)))
      ),
      timeOffset -> RegFieldGroup(
        "mtime",
        Some("Timer Register"),
        RegField.bytes(time_sw, Some(RegFieldDesc("mtime", "", reset = Some(0), volatile = true)))
      )
    )
  }
}
