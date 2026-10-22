package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.frontend.bpu._
import xiangshan.frontend.bpu.mbtb._

// Populate the real Main BTB and VBTB only through prediction/training ports.
class VictimBtbPromotionHarness(implicit p: Parameters) extends XSModule with Helpers {
  val io = IO(new Bundle {
    val pc = Input(UInt(VAddrBits.W))
    val position = Input(UInt(CfiPositionWidth.W))
    val kind = Input(UInt(2.W))
    val predict = Input(Bool())
    val prepareTrain = Input(Bool())
    val train = Input(Bool())
    val ready = Output(Bool())
    val tagStride = Output(UInt(VAddrBits.W))
    val predictions = Output(Vec(NumWay + 1, Valid(UInt(CfiPositionWidth.W))))
    val writesMain = Output(Bool())
  })
  val bank = Module(new MainBtbAlignBank(0))
  bank.io.enable := true.B
  bank.io.stageCtrl.s0_fire := io.predict
  bank.io.stageCtrl.s1_fire := RegNext(io.predict, false.B)
  bank.io.stageCtrl.s2_fire := RegNext(bank.io.stageCtrl.s1_fire, false.B)
  bank.io.stageCtrl.s3_fire := RegNext(bank.io.stageCtrl.s2_fire, false.B)
  bank.io.stageCtrl.t0_fire := io.prepareTrain
  bank.io.read.req.startPc := Cat(0.U(1.W), io.pc)
  bank.io.read.req.posHigherBits := 0.U
  bank.io.read.req.crossPage := false.B
  bank.io.s3_vbtbTaken := false.B
  bank.io.t0_startPc := io.pc
  bank.io.write.req.valid := io.train
  bank.io.write.req.bits := 0.U.asTypeOf(bank.io.write.req.bits)
  bank.io.write.req.bits.needWrite := true.B
  bank.io.write.req.bits.startPc := io.pc
  bank.io.write.req.bits.meta := VecInit(bank.io.read.resp.metas.take(NumWay))
  bank.io.write.req.bits.posHigherBits := 0.U
  val branch = Wire(new BranchInfo)
  branch := 0.U.asTypeOf(branch)
  branch.cfiPosition := io.position
  branch.target := "h80001000".U(VAddrBits.W)
  branch.taken := true.B
  branch.mispredict := true.B
  branch.attribute.branchType := Mux(io.kind === 1.U,
    BranchAttribute.BranchType.Conditional,
    Mux(io.kind === 3.U, BranchAttribute.BranchType.Indirect, BranchAttribute.BranchType.Direct))
  branch.attribute.rasAction := Mux(io.kind === 3.U, BranchAttribute.RasAction.Pop, BranchAttribute.RasAction.None)
  bank.io.write.req.bits.mispredictInfo.valid := true.B
  bank.io.write.req.bits.mispredictInfo.bits := branch
  bank.io.write.req.bits.branches(0).valid := true.B
  bank.io.write.req.bits.branches(0).bits := branch
  io.ready := bank.io.sramResetDone
  io.tagStride := (BigInt(1) << (FetchBlockAlignWidth + AlignBankIdxLen + InternalBankIdxLen + SetIdxLen)).U
  for ((out, pred) <- io.predictions.zip(bank.io.read.resp.predictions)) {
    out.valid := pred.valid
    out.bits := pred.bits.cfiPosition
  }
  io.writesMain := bank.io.trace.needWrite
}

object VictimBtbPromotionTest extends App with ChiselSim {
  val base = new DefaultConfig
  val core = base(XSTileKey).head
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => core.copy(frontendParameters = core.frontendParameters.copy(
      bpuParameters = core.frontendParameters.bpuParameters.copy(
        mbtbParameters = core.frontendParameters.bpuParameters.mbtbParameters.copy(NumEntries = 128))))
    case DebugOptionsKey => base(DebugOptionsKey).copy(EnableDifftest = false,
      AlwaysBasicDiff = false, EnablePerfDebug = false)
    case DFTOptionsKey => base(DFTOptionsKey).copy(EnableMbist = false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, enableXMR = false)
  }
  ChiselDB.init(false)
  Constantin.init(false)
  simulate(new VictimBtbPromotionHarness) { dut =>
    dut.io.pc.poke("h80000000".U)
    dut.io.position.poke(0.U)
    dut.io.kind.poke(2.U)
    dut.io.predict.poke(false.B)
    dut.io.prepareTrain.poke(false.B)
    dut.io.train.poke(false.B)
    var remaining = 64
    while (!dut.io.ready.peek().litToBoolean && remaining > 0) {
      dut.clock.step()
      remaining -= 1
    }
    dut.io.ready.expect(true.B)
    val pc = BigInt("80000000", 16)
    val stride = dut.io.tagStride.peek().litValue
    def predict(address: BigInt): Set[BigInt] = {
      dut.io.pc.poke(address.U)
      dut.io.predict.poke(true.B)
      dut.clock.step()
      dut.io.predict.poke(false.B)
      dut.clock.step(2)
      dut.io.predictions.filter(_.valid.peek().litToBoolean).map(_.bits.peek().litValue).toSet
    }
    def train(address: BigInt, position: Int, kind: Int, requireMain: Boolean = true): Unit = {
      predict(address)
      dut.io.position.poke(position.U)
      dut.io.kind.poke(kind.U)
      dut.io.prepareTrain.poke(true.B)
      dut.clock.step()
      dut.io.prepareTrain.poke(false.B)
      dut.io.train.poke(true.B)
      if (requireMain) assert(dut.io.writesMain.peek().litToBoolean,
        s"VBTB_HIDDEN_BRANCH_NOT_PROMOTED pc=$address position=$position")
      dut.clock.step()
      dut.io.train.poke(false.B)
      dut.clock.step(12)
    }
    train(pc, 2, 1)
    train(pc, 6, 3)
    assert(predict(pc) == Set(BigInt(2), BigInt(6)), "initial Main BTB population failed")
    for (i <- 1 to 4) train(pc + stride * i, 2, 2)
    val hidden = predict(pc)
    assert(hidden == Set(BigInt(2)), s"test requires two evicted branches with only the earlier exposed: $hidden")
    // The resolved return is present in VBTB but absent from its single prediction.
    // Training must make it visible to the next prediction.
    train(pc, 6, 3)
    assert(predict(pc).contains(BigInt(6)), "retraining did not recover the hidden return")
    println("VBTB_HIDDEN_BRANCH_PROMOTION_PASS")
  }
}
