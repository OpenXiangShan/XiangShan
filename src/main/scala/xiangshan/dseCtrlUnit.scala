package xiangshan

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, LazyModule, LazyModuleImp, LazyRawModuleImp, SimpleDevice}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortParameters, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import freechips.rocketchip.util.{SimpleRegIO, UIntToOH1}

case class dseParams(baseAddress: BigInt = 0x39010000L)
{
  def address = AddressSet(baseAddress, 0xff)
  def beatBytes = 8
}

class dseCtrlUnit(params: dseParams)(implicit p: Parameters) extends LazyModule {
  val ctrlnode = TLRegisterNode(
    address = Seq(params.address),
    device = new SimpleDevice("dseCtrl", Nil),
    beatBytes = params.beatBytes
  )

  lazy val module = new dseCtrlUnitImp(this)
}

class dseCtrlUnitImp(wrapper: dseCtrlUnit)(implicit p: Parameters) extends LazyRawModuleImp(wrapper) with HasXSParameter {

  val io = IO(new Bundle{
    val clk = Input(Clock())
    val rst = Input(Reset())
//    val robSize = Output(UInt(log2Up(RobSize + 1).W))
  })

  childClock := io.clk
  childReset := io.rst
  val ctrlnode = wrapper.ctrlnode
  withClockAndReset(childClock, childReset) {
    val pingpong = RegInit(0.U(1.W))
    val ctrlSel = RegInit(0.U(1.W))
    val robSize0 = RegInit(RobSize.U)
    val robSize1 = RegInit(RobSize.U)
    val robSize = Wire(UInt(log2Up(RobSize + 1).W))

    ctrlnode.regmap(
      0x00 -> Seq(
        RegField(1, pingpong, RegFieldDesc("pingpong", "pingpong signal")),
        RegField(1, ctrlSel, RegFieldDesc("ctrlSel", "control signal to select the robSize"))
      ),
      0x04 -> Seq(
        RegField(log2Up(RobSize + 1), robSize0, RegFieldDesc("robSize0", "robSize0")),
        RegField(log2Up(RobSize + 1), robSize1, RegFieldDesc("robSize1", "robSize1"))
      ),
    )


    // Mux logic
    robSize := Mux(ctrlSel.asBool, robSize0, robSize1)


    // Bore to ROB
     ExcitingUtils.addSource(robSize, "DSE_ROBSIZE")
  }

}
