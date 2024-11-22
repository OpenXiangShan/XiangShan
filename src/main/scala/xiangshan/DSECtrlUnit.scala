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

case class DSEParams(baseAddress: BigInt = 0x39020000L)
{
  def address = AddressSet(baseAddress, 0x0ffff)
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
    val core_reset = Output(Bool())
    val reset_vector = Output(UInt(PAddrBits.W))
    val instrCnt = Input(UInt(64.W))
  })

  childClock := io.clk
  childReset := io.rst
  val ctrlnode = wrapper.ctrlnode
  withClockAndReset(childClock, childReset) {
    val pingpong = RegInit(0.U(8.W))
    val ctrlSel = RegInit(0.U(8.W))
    val max_instr_cnt = RegInit(0x1000000.U(64.W))
    val epoch = RegInit(0.U(64.W))

    val robSize0 = RegInit(RobSize.U(64.W))
    val robSize1 = RegInit(RobSize.U(64.W))
    val robSize = Wire(UInt(64.W))

    val lqSize0 = RegInit(LoadQueueSize.U(64.W))
    val lqSize1 = RegInit(LoadQueueSize.U(64.W))
    val lqSize = Wire(UInt(64.W))

    val sqSize0 = RegInit(StoreQueueSize.U(64.W))
    val sqSize1 = RegInit(StoreQueueSize.U(64.W))
    val sqSize = Wire(UInt(64.W))

    val commit_valid = WireInit(false.B)


    ctrlnode.regmap(
      0x000 -> Seq(RegField(8, pingpong)),
      0x004 -> Seq(RegField(8, ctrlSel)),
      0x008 -> Seq(RegField(64, max_instr_cnt)),
      0x010 -> Seq(RegField(64, epoch)),
      0x100 -> Seq(RegField(64, robSize0)),
      0x108 -> Seq(RegField(64, robSize1)),
      0x110 -> Seq(RegField(64, lqSize0)),
      0x118 -> Seq(RegField(64, lqSize1)),
      0x120 -> Seq(RegField(64, sqSize0)),
      0x128 -> Seq(RegField(64, sqSize1))
    )

    // Mux logic
    robSize := Mux(ctrlSel.orR, robSize1, robSize0)
    lqSize := Mux(ctrlSel.orR, lqSize1, lqSize0)
    sqSize := Mux(ctrlSel.orR, sqSize1, sqSize0)

    // Bore to ROB
    ExcitingUtils.addSource(robSize, "DSE_ROBSIZE")
    ExcitingUtils.addSource(lqSize, "DSE_LQSIZE")
    ExcitingUtils.addSource(sqSize, "DSE_SQSIZE")

    ExcitingUtils.addSink(commit_valid, "DSE_COMMITVALID")


    // core reset generation
    val coreResetReg = RegInit(false.B)
    val resetVectorReg = RegInit(0.U(PAddrBits.W))

    // driver -> workload reset
    val ctrlSelDelayed = RegNext(ctrlSel)
    val ctrlSelChanged = (ctrlSelDelayed =/= ctrlSel)
    val lastCycleCommit = RegNext(commit_valid)
    val ctrlSelChangedStall = RegInit(false.B)
    val ctrlSelReset = ctrlSelChangedStall && lastCycleCommit
    when (ctrlSelChanged) {
      ctrlSelChangedStall := true.B
    }


    // workload -> driver reset
    val reach_instr_limit = (io.instrCnt >= max_instr_cnt)


    when (ctrlSelReset) {
      coreResetReg := true.B
      resetVectorReg := 0x80000000L.U
      ctrlSelChangedStall := false.B
    }

    when (reach_instr_limit) {
      coreResetReg := true.B
      resetVectorReg := 0x10000000L.U
    }

    // core reset counter
    val (core_rst_counter, core_rst_end) = Counter(coreResetReg, 10)
    when (core_rst_end) {
      coreResetReg := false.B
    }

    when (RegNext(RegNext(core_rst_end))) {
      resetVectorReg := 0.U
    }

    io.core_reset := coreResetReg
    io.reset_vector := resetVectorReg

  }

}
