package xiangshan

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, LazyModule, LazyModuleImp, LazyRawModuleImp, SimpleDevice}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortParameters, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import freechips.rocketchip.util.{SimpleRegIO, UIntToOH1}


case class dseParams(baseAddress: BigInt = 0x70000000)
{
  def address = AddressSet(baseAddress, 0xffff)
  def beatBytes = 8
}

class dseCtrlUnit(params: dseParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val ctrlnode = TLRegisterNode(
    address = Seq(params.address),
    device = new SimpleDevice("dseCtrl", Nil),
    beatBytes = params.beatBytes
  )

  lazy val module = new dseCtrlUnitImp(this)
}

class dseCtrlUnitImp(wrapper: dseCtrlUnit) extends LazyRawModuleImp(wrapper) with HasXSParameter {
  val dse_clk = IO(Input(Clock()))
  val dse_rst = IO(Input(Bool()))

  childClock := dse_clk
  childReset := dse_rst
  val ctrlnode = wrapper.ctrlnode
  withClockAndReset(childClock, childReset) {
    val pingpong = RegInit(0.U(1.W))
    val ctrlSel = RegInit(0.U(1.W))
    val robSize0 = RegInit(0.U(log2Up(RobSize + 1).W))
    val robSize1 = RegInit(0.U(log2Up(RobSize + 1).W))
    val robSize = Wire(0.U(log2Up(RobSize + 1).W))

    ctrlnode.regmap(
      0x00 -> Seq(
        RegField(1, pingpong),
        RegField(1, ctrlSel)),
      0x04 -> Seq(RegField(robSize0.litValue.toInt)),
      0x08 -> Seq(RegField(robSize1.litValue.toInt))
    )


    // Mux logic
    robSize := Mux(ctrlSel.asBool, robSize0, robSize1)


    // Bore to ROB
    ExcitingUtils.addSource(robSize, "DSE_ROBSIZE")
  }

}
