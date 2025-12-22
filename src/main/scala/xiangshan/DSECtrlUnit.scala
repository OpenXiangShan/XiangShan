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
import utility._
import chisel3.util.experimental.BoringUtils

case class DSEParams(baseAddress: BigInt = 0x39002000L)
{
  def address = AddressSet(baseAddress, 0xfff)
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
    val max_instr_cnt = Output(UInt(64.W))
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

//    val lqSize0 = RegInit(LoadQueueSize.U(64.W))
//    val lqSize1 = RegInit(LoadQueueSize.U(64.W))
//    val lqSize = Wire(UInt(64.W))
//
//    val sqSize0 = RegInit(StoreQueueSize.U(64.W))
//    val sqSize1 = RegInit(StoreQueueSize.U(64.W))
//    val sqSize = Wire(UInt(64.W))
//
//    val ftqSize0 = RegInit(FtqSize.U(64.W))
//    val ftqSize1 = RegInit(FtqSize.U(64.W))
//    val ftqSize = Wire(UInt(64.W))
//
//    val ibufSize0 = RegInit(IBufSize.U(64.W))
//    val ibufSize1 = RegInit(IBufSize.U(64.W))
//    val ibufSize = Wire(UInt(64.W))
//
//    val intDqSize0 = RegInit(dpParams.IntDqSize.U(64.W))
//    val intDqSize1 = RegInit(dpParams.IntDqSize.U(64.W))
//    val intDqSize = Wire(UInt(64.W))
//
//    val fpDqSize0 = RegInit(dpParams.FpDqSize.U(64.W))
//    val fpDqSize1 = RegInit(dpParams.FpDqSize.U(64.W))
//    val fpDqSize = Wire(UInt(64.W))
//
//    val lsDqSize0 = RegInit(dpParams.LsDqSize.U(64.W))
//    val lsDqSize1 = RegInit(dpParams.LsDqSize.U(64.W))
//    val lsDqSize = Wire(UInt(64.W))
//
//    val l2MSHRs0 = RegInit(L2MSHRs.U(64.W))
//    val l2MSHRs1 = RegInit(L2MSHRs.U(64.W))
//    val l2MSHRs = Wire(UInt(64.W))
//
//    val l3MSHRs0 = RegInit(L3MSHRs.U(64.W))
//    val l3MSHRs1 = RegInit(L3MSHRs.U(64.W))
//    val l3MSHRs = Wire(UInt(64.W))
//
//    val l2Sets0 = RegInit(p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
//    val l2Sets1 = RegInit(p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
//    val l2Sets = Wire(UInt(64.W))
//
//    val l3Sets0 = RegInit(p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
//    val l3Sets1 = RegInit(p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U(64.W))
//    val l3Sets = Wire(UInt(64.W))
//
//    val intPhyRegs0 = RegInit(NRPhyRegs.U(64.W))
//    val intPhyRegs1 = RegInit(NRPhyRegs.U(64.W))
//    val intPhyRegs = Wire(UInt(64.W))
//
//    val fpPhyRegs0 = RegInit((NRPhyRegs - 32).U(64.W))
//    val fpPhyRegs1 = RegInit((NRPhyRegs - 32).U(64.W))
//    val fpPhyRegs = Wire(UInt(64.W))
//
//    val rasSize0 = RegInit(RasSize.U(64.W))
//    val rasSize1 = RegInit(RasSize.U(64.W))
//    val rasSize = Wire(UInt(64.W))
//
//    val dcacheWays0 = RegInit(p(XSCoreParamsKey).dcacheParametersOpt.map(_.nWays).getOrElse(0).U(64.W))
//    val dcacheWays1 = RegInit(p(XSCoreParamsKey).dcacheParametersOpt.map(_.nWays).getOrElse(0).U(64.W))
//    val dcacheWays = Wire(UInt(64.W))
//
//    val dcacheMSHRs0 = RegInit(p(XSCoreParamsKey).dcacheParametersOpt.map(_.nMissEntries).getOrElse(0).U(64.W))
//    val dcacheMSHRs1 = RegInit(p(XSCoreParamsKey).dcacheParametersOpt.map(_.nMissEntries).getOrElse(0).U(64.W))
//    val dcacheMSHRs = Wire(UInt(64.W))

    val commit_valid = WireInit(false.B)

    io.max_epoch := max_epoch
    io.epoch := epoch
    io.max_instr_cnt := max_instr_cnt

    ctrlnode.regmap(
      0x000 -> Seq(RegField(8, pingpong)),
      0x004 -> Seq(RegField(8, ctrlSel)),
      0x008 -> Seq(RegField(64, max_instr_cnt)),
      0x010 -> Seq(RegField(64, epoch)),
      0x018 -> Seq(RegField(64, max_epoch)),
      0x100 -> Seq(RegField(64, robSize0)),
      0x108 -> Seq(RegField(64, robSize1)),
//      0x110 -> Seq(RegField(64, lqSize0)),
//      0x118 -> Seq(RegField(64, lqSize1)),
//      0x120 -> Seq(RegField(64, sqSize0)),
//      0x128 -> Seq(RegField(64, sqSize1)),
//      0x130 -> Seq(RegField(64, ftqSize0)),
//      0x138 -> Seq(RegField(64, ftqSize1)),
//      0x140 -> Seq(RegField(64, ibufSize0)),
//      0x148 -> Seq(RegField(64, ibufSize1)),
//      0x150 -> Seq(RegField(64, intDqSize0)),
//      0x158 -> Seq(RegField(64, intDqSize1)),
//      0x160 -> Seq(RegField(64, fpDqSize0)),
//      0x168 -> Seq(RegField(64, fpDqSize1)),
//      0x170 -> Seq(RegField(64, lsDqSize0)),
//      0x178 -> Seq(RegField(64, lsDqSize1)),
//      0x180 -> Seq(RegField(64, l2MSHRs0)),
//      0x188 -> Seq(RegField(64, l2MSHRs1)),
//      0x190 -> Seq(RegField(64, l3MSHRs0)),
//      0x198 -> Seq(RegField(64, l3MSHRs1)),
//      0x1A0 -> Seq(RegField(64, l2Sets0)),
//      0x1A8 -> Seq(RegField(64, l2Sets1)),
//      0x1B0 -> Seq(RegField(64, l3Sets0)),
//      0x1B8 -> Seq(RegField(64, l3Sets1)),
//      0x1C0 -> Seq(RegField(64, intPhyRegs0)),
//      0x1C8 -> Seq(RegField(64, intPhyRegs1)),
//      0x1D0 -> Seq(RegField(64, fpPhyRegs0)),
//      0x1D8 -> Seq(RegField(64, fpPhyRegs1)),
//      0x1E0 -> Seq(RegField(64, rasSize0)),
//      0x1E8 -> Seq(RegField(64, rasSize1)),
//      0x1F0 -> Seq(RegField(64, dcacheWays0)),
//      0x1F8 -> Seq(RegField(64, dcacheWays1)),
//      0x200 -> Seq(RegField(64, dcacheMSHRs0)),
//      0x208 -> Seq(RegField(64, dcacheMSHRs1))
    )

    // Mux logic
    robSize := Mux(ctrlSel.orR, robSize1, robSize0)
//    lqSize := Mux(ctrlSel.orR, lqSize1, lqSize0)
//    sqSize := Mux(ctrlSel.orR, sqSize1, sqSize0)
//    ftqSize := Mux(ctrlSel.orR, ftqSize1, ftqSize0)
//    ibufSize := Mux(ctrlSel.orR, ibufSize1, ibufSize0)
//    intDqSize := Mux(ctrlSel.orR, intDqSize1, intDqSize0)
//    fpDqSize := Mux(ctrlSel.orR, fpDqSize1, fpDqSize0)
//    lsDqSize := Mux(ctrlSel.orR, lsDqSize1, lsDqSize0)
//    l2MSHRs := Mux(ctrlSel.orR, l2MSHRs1, l2MSHRs0)
//    l3MSHRs := Mux(ctrlSel.orR, l3MSHRs1, l3MSHRs0)
//    l2Sets := Mux(ctrlSel.orR, l2Sets1, l2Sets0)
//    l3Sets := Mux(ctrlSel.orR, l3Sets1, l3Sets0)
//    intPhyRegs := Mux(ctrlSel.orR, intPhyRegs1, intPhyRegs0)
//    fpPhyRegs := Mux(ctrlSel.orR, fpPhyRegs1, fpPhyRegs0)
//    rasSize := Mux(ctrlSel.orR, rasSize1, rasSize0)
//    dcacheWays := Mux(ctrlSel.orR, dcacheWays1, dcacheWays0)
//    dcacheMSHRs := Mux(ctrlSel.orR, dcacheMSHRs1, dcacheMSHRs0)

    // Bore to/from modules
    BoringUtils.addSource(robSize, "DSE_ROBSIZE")
//    ExcitingUtils.addSource(lqSize, "DSE_LQSIZE")
//    ExcitingUtils.addSource(sqSize, "DSE_SQSIZE")
//    ExcitingUtils.addSource(ftqSize, "DSE_FTQSIZE")
//    ExcitingUtils.addSource(ibufSize, "DSE_IBUFSIZE")
//    ExcitingUtils.addSource(intDqSize, "DSE_INTDQSIZE")
//    ExcitingUtils.addSource(fpDqSize, "DSE_FPDQSIZE")
//    ExcitingUtils.addSource(lsDqSize, "DSE_LSDQSIZE")
//    ExcitingUtils.addSource(l2MSHRs, "DSE_L2MSHRS")
//    ExcitingUtils.addSource(l3MSHRs, "DSE_L3MSHRS")
//    ExcitingUtils.addSource(l2Sets, "DSE_L2SETS")
//    ExcitingUtils.addSource(l3Sets, "DSE_L3SETS")
//    ExcitingUtils.addSource(intPhyRegs, "DSE_INTFLSIZE")
//    ExcitingUtils.addSource(fpPhyRegs, "DSE_FPFLSIZE")
//    ExcitingUtils.addSource(rasSize, "DSE_RASSIZE")
//    ExcitingUtils.addSource(dcacheWays, "DSE_DCACHEWAYS")
//    ExcitingUtils.addSource(dcacheMSHRs, "DSE_DCACHEMSHRS")

    BoringUtils.addSink(commit_valid, "DSE_COMMITVALID")


    // assertion
    assert(robSize <= RobSize.U, "DSE parameter must not exceed ROBSZIE")
//    assert(lqSize <= LoadQueueSize.U, "DSE parameter must not exceed LoadQueueSize")
//    assert(sqSize <= StoreQueueSize.U, "DSE parameter must not exceed StoreQueueSize")
//    assert(ftqSize <= FtqSize.U, "DSE parameter must not exceed FtqSize")
//    assert(ibufSize <= IBufSize.U, "DSE parameter must not exceed IBufSize")
//    assert(intDqSize <= dpParams.IntDqSize.U, "DSE parameter must not exceed IntDqSize")
//    assert(fpDqSize <= dpParams.FpDqSize.U, "DSE parameter must not exceed FpDqSize")
//    assert(lsDqSize <= dpParams.LsDqSize.U, "DSE parameter must not exceed LsDqSize")
//    assert(l2MSHRs <= L2MSHRs.U, "DSE parameter must not exceed L2MSHRs")
//    assert(l3MSHRs <= L3MSHRs.U, "DSE parameter must not exceed L3MSHRs")
//    assert(l2Sets <= p(XSCoreParamsKey).L2CacheParamsOpt.map(_.sets).getOrElse(0).U, "DSE parameter must not exceed L2Sets")
//    assert(l3Sets <= p(SoCParamsKey).L3CacheParamsOpt.map(_.sets).getOrElse(0).U, "DSE parameter must not exceed L3Sets")
//    assert(intPhyRegs <= NRPhyRegs.U, "DSE parameter must not exceed NRPhyRegs")
//    assert(fpPhyRegs <= (NRPhyRegs - 32).U, "DSE parameter must not exceed fpPhyRegs")
//    assert(rasSize <= RasSize.U, "DSE parameter must not exceed RasSize")
//    assert(dcacheWays <= p(XSCoreParamsKey).dcacheParametersOpt.map(_.nWays).getOrElse(0).U, "DSE parameter must not exceed dcacheWays")
//    assert(dcacheMSHRs <= p(XSCoreParamsKey).dcacheParametersOpt.map(_.nMissEntries).getOrElse(0).U, "DSE parameter must not exceed dcacheMSHRs")


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

    val reset_avoid = RegInit(false.B)
    val (reset_avoid_counter, reset_avoid_end) = Counter(coreResetReg, 10)
    when (reset_avoid_end) {
      reset_avoid := false.B
    }

    // workload -> driver reset
    val reach_instr_limit = (io.instrCnt >= max_instr_cnt) && (resetVectorReg === 0x80000000L.U) && !reset_avoid

    when (ctrlSelReset) {
      coreResetReg := true.B
      resetVectorReg := 0x80000000L.U
      ctrlSelChangedStall := false.B
      reset_avoid := true.B
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


    io.core_reset := coreResetReg
    io.reset_vector := resetVectorReg

  }

}
