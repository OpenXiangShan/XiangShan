package xiangshan

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, LazyModule, LazyModuleImp, LazyRawModuleImp, SimpleDevice}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortParameters, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import freechips.rocketchip.util.{SimpleRegIO, UIntToOH1}
import javax.swing.SwingWorker

case class DSEParams(baseAddress: BigInt = 0x39002000L)
{
  def address = AddressSet(baseAddress, 0x0fff)
  def beatBytes = 8
}

class DSECtrlUnit(params: DSEParams)(implicit p: Parameters) extends LazyModule {

  val ctrlnode = TLRegisterNode(
    address = Seq(params.address),
    device = new SimpleDevice("dseCtrl", Nil),
    beatBytes = params.beatBytes
  )

  lazy val module = new DSECtrlUnitImp(this)
}

class DSECtrlUnitImp(wrapper: DSECtrlUnit)(implicit p: Parameters) extends LazyRawModuleImp(wrapper) with HasXSParameter {

  val io = IO(new Bundle{
    val clk = Input(Clock())
    val rst = Input(Reset())
//    val robSize = Output(UInt(log2Up(RobSize + 1).W))
  })

  childClock := io.clk
  childReset := io.rst
  val ctrlnode = wrapper.ctrlnode
  withClockAndReset(childClock, childReset) {
    val pingpong = RegInit(0.U(64.W))
    val ctrlSel = RegInit(0.U(64.W))
    val robSize0 = RegInit(RobSize.U(64.W))
    val robSize1 = RegInit(RobSize.U(64.W))
    val robSize = Wire(UInt(64.W))

    val ctrl_config_regs = (
      Seq(pingpong) ++
      Seq(ctrlSel)
    ).map(reg => RegField(64, reg, RegWriteFn(reg)))

    val param_config_regs = (
      Seq(robSize0) ++
      Seq(robSize1)
    ).map(reg => RegField(64, reg, RegWriteFn(reg)))

    ctrlnode.regmap(
      0x000 -> RegFieldGroup(
        "ctrl_config", Some("Pingpong and control select"),
        ctrl_config_regs),
      0x100 -> RegFieldGroup(
        "param_config", Some("ROB size"),
        param_config_regs),
    )

    // Mux logic
    robSize := Mux(ctrlSel.orR, robSize1, robSize0)


    // Bore to ROB
     ExcitingUtils.addSource(robSize, "DSE_ROBSIZE")
  }

}
