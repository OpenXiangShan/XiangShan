package xiangshan

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, BundleBridgeSource, LazyModule, LazyModuleImp, LazyRawModuleImp, SimpleDevice}
import freechips.rocketchip.interrupts.{IntSourceNode, IntSourcePortParameters, IntSourcePortSimple}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import freechips.rocketchip.util.{SimpleRegIO, UIntToOH1}
import xiangshan.backend.regfile.Regfile
import system._

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

class DSECtrlUnitImp(wrapper: DSECtrlUnit)(implicit p: Parameters) extends LazyRawModuleImp(wrapper) with HasXSParameter with HasSoCParameter {

  val io = IO(new Bundle{
    val clk = Input(Clock())
    val rst = Input(Reset())
//    val robSize = Output(UInt(log2Up(RobSize + 1).W))
    val core_reset = Output(Bool())
    val reset_vector = Output(UInt(PAddrBits.W))
    val instrCnt = Input(UInt(64.W))
    val max_epoch = Output(UInt(64.W))
    val epoch = Output(UInt(64.W))
  })

  childClock := io.clk
  childReset := io.rst
  val ctrlnode = wrapper.ctrlnode
  withClockAndReset(childClock, childReset) {
    val pingpong = RegInit(0.U(8.W))
    val ctrlSel = RegInit(0.U(8.W))
    val max_instr_cnt = RegInit(0x1000000.U(64.W))
    val epoch = RegInit(0.U(64.W))
    val max_epoch = RegInit(0.U(64.W))

    val robSize0 = RegInit(RobSize.U(64.W))
    val robSize1 = RegInit(RobSize.U(64.W))
    val robSize = Wire(UInt(64.W))

    val lqSize0 = RegInit(LoadQueueSize.U(64.W))
    val lqSize1 = RegInit(LoadQueueSize.U(64.W))
    val lqSize = Wire(UInt(64.W))

    val sqSize0 = RegInit(StoreQueueSize.U(64.W))
    val sqSize1 = RegInit(StoreQueueSize.U(64.W))
    val sqSize = Wire(UInt(64.W))

    val ftqSize0 = RegInit(FtqSize.U(64.W))
    val ftqSize1 = RegInit(FtqSize.U(64.W))
    val ftqSize = Wire(UInt(64.W))

    val ibufSize0 = RegInit(IBufSize.U(64.W))
    val ibufSize1 = RegInit(IBufSize.U(64.W))
    val ibufSize = Wire(UInt(64.W))

    val intDqSize0 = RegInit(dpParams.IntDqSize.U(64.W))
    val intDqSize1 = RegInit(dpParams.IntDqSize.U(64.W))
    val intDqSize = Wire(UInt(64.W))

    val fpDqSize0 = RegInit(dpParams.FpDqSize.U(64.W))
    val fpDqSize1 = RegInit(dpParams.FpDqSize.U(64.W))
    val fpDqSize = Wire(UInt(64.W))

    val lsDqSize0 = RegInit(dpParams.LsDqSize.U(64.W))
    val lsDqSize1 = RegInit(dpParams.LsDqSize.U(64.W))
    val lsDqSize = Wire(UInt(64.W))

    val l2MSHRs0 = RegInit(L2MSHRs.U(64.W))
    val l2MSHRs1 = RegInit(L2MSHRs.U(64.W))
    val l2MSHRs = Wire(UInt(64.W))

    val l3MSHRs0 = RegInit(L3MSHRs.U(64.W))
    val l3MSHRs1 = RegInit(L3MSHRs.U(64.W))
    val l3MSHRs = Wire(UInt(64.W))

    val l2Sets0 = RegInit(p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
    val l2Sets1 = RegInit(p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
    val l2Sets = Wire(UInt(64.W))

    val l3Sets0 = RegInit(p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
    val l3Sets1 = RegInit(p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
    val l3Sets = Wire(UInt(64.W))

    val commit_valid = WireInit(false.B)

    io.max_epoch := max_epoch
    io.epoch := epoch

    ctrlnode.regmap(
      0x000 -> Seq(RegField(8, pingpong)),
      0x004 -> Seq(RegField(8, ctrlSel)),
      0x008 -> Seq(RegField(64, max_instr_cnt)),
      0x010 -> Seq(RegField(64, epoch)),
      0x018 -> Seq(RegField(64, max_epoch)),
      0x100 -> Seq(RegField(64, robSize0)),
      0x108 -> Seq(RegField(64, robSize1)),
      0x110 -> Seq(RegField(64, lqSize0)),
      0x118 -> Seq(RegField(64, lqSize1)),
      0x120 -> Seq(RegField(64, sqSize0)),
      0x128 -> Seq(RegField(64, sqSize1)),
      0x130 -> Seq(RegField(64, ftqSize0)),
      0x138 -> Seq(RegField(64, ftqSize1)),
      0x140 -> Seq(RegField(64, ibufSize0)),
      0x148 -> Seq(RegField(64, ibufSize1)),
      0x150 -> Seq(RegField(64, intDqSize0)),
      0x158 -> Seq(RegField(64, intDqSize1)),
      0x160 -> Seq(RegField(64, fpDqSize0)),
      0x168 -> Seq(RegField(64, fpDqSize1)),
      0x170 -> Seq(RegField(64, lsDqSize0)),
      0x178 -> Seq(RegField(64, lsDqSize1)),
      0x180 -> Seq(RegField(64, l2MSHRs0)),
      0x188 -> Seq(RegField(64, l2MSHRs1)),
      0x190 -> Seq(RegField(64, l3MSHRs0)),
      0x198 -> Seq(RegField(64, l3MSHRs1)),
      0x1A0 -> Seq(RegField(64, l2Sets0)),
      0x1A8 -> Seq(RegField(64, l2Sets1)),
      0x1B0 -> Seq(RegField(64, l3Sets0)),
      0x1B8 -> Seq(RegField(64, l3Sets1))
    )

    // Mux logic
    robSize := Mux(ctrlSel.orR, robSize1, robSize0)
    lqSize := Mux(ctrlSel.orR, lqSize1, lqSize0)
    sqSize := Mux(ctrlSel.orR, sqSize1, sqSize0)
    ftqSize := Mux(ctrlSel.orR, ftqSize1, ftqSize0)
    ibufSize := Mux(ctrlSel.orR, ibufSize1, ibufSize0)
    intDqSize := Mux(ctrlSel.orR, intDqSize1, intDqSize0)
    fpDqSize := Mux(ctrlSel.orR, fpDqSize1, fpDqSize0)
    lsDqSize := Mux(ctrlSel.orR, lsDqSize1, lsDqSize0)
    l2MSHRs := Mux(ctrlSel.orR, l2MSHRs1, l2MSHRs0)
    l3MSHRs := Mux(ctrlSel.orR, l3MSHRs1, l3MSHRs0)
    l2Sets := Mux(ctrlSel.orR, l2Sets1, l2Sets0)
    l3Sets := Mux(ctrlSel.orR, l3Sets1, l3Sets0)

    // Bore to/from modules
    ExcitingUtils.addSource(robSize, "DSE_ROBSIZE")
    ExcitingUtils.addSource(lqSize, "DSE_LQSIZE")
    ExcitingUtils.addSource(sqSize, "DSE_SQSIZE")
    ExcitingUtils.addSource(ftqSize, "DSE_FTQSIZE")
    ExcitingUtils.addSource(ibufSize, "DSE_IBUFSIZE")
    ExcitingUtils.addSource(intDqSize, "DSE_INTDQSIZE")
    ExcitingUtils.addSource(fpDqSize, "DSE_FPDQSIZE")
    ExcitingUtils.addSource(lsDqSize, "DSE_LSDQSIZE")
    ExcitingUtils.addSource(l2MSHRs, "DSE_L2MSHRS")
    ExcitingUtils.addSource(l3MSHRs, "DSE_L3MSHRS")
    ExcitingUtils.addSource(l2Sets, "DSE_L2SETS")
    ExcitingUtils.addSource(l3Sets, "DSE_L3SETS")

    ExcitingUtils.addSink(commit_valid, "DSE_COMMITVALID")


    // assertion
    assert(robSize <= RobSize.U, "DSE parameter must not exceed ROBSZIE")
    assert(lqSize <= LoadQueueSize.U, "DSE parameter must not exceed LoadQueueSize")
    assert(sqSize <= StoreQueueSize.U, "DSE parameter must not exceed StoreQueueSize")
    assert(ftqSize <= FtqSize.U, "DSE parameter must not exceed FtqSize")
    assert(ibufSize <= IBufSize.U, "DSE parameter must not exceed IBufSize")
    assert(intDqSize <= dpParams.IntDqSize.U, "DSE parameter must not exceed IntDqSize")
    assert(fpDqSize <= dpParams.FpDqSize.U, "DSE parameter must not exceed FpDqSize")
    assert(lsDqSize <= dpParams.LsDqSize.U, "DSE parameter must not exceed LsDqSize")
    assert(l2MSHRs <= L2MSHRs.U, "DSE parameter must not exceed L2MSHRs")
    assert(l3MSHRs <= L3MSHRs.U, "DSE parameter must not exceed L3MSHRs")
    assert(l2Sets <= p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U, "DSE parameter must not exceed L2Sets")
    assert(l3Sets <= p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U, "DSE parameter must not exceed L3Sets")


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
