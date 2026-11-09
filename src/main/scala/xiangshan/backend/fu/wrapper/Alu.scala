package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.Cat
import xiangshan.backend.fu.{AluDataModule, PipedFuncUnit}
import xiangshan.backend.fu.FuConfig
import utility.{HostOpSpec, SignExt, SimulationModels, ZeroExt}
import xiangshan.backend.fu.IntAluComputeCarrier

class Alu(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val behavior = SimulationModels.enabled("int-alu")
  private val grouped = SimulationModels.computeGrouped("int-alu")
  private val behaviorCarrier = if (behavior) Some(Module(new IntAluComputeCarrier(cfg.aluNeedPc))) else None
  private val aluModule = if (!behavior) Some(Module(new AluDataModule(cfg.aluNeedPc))) else None

  private val opSpecs = Seq(
    HostOpSpec("add-sub", Seq(64, 64), 64), HostOpSpec("compare", Seq(64, 64), 64),
    HostOpSpec("shift", Seq(64, 64), 64, shiftMask = 63),
    HostOpSpec("rotate", Seq(64, 64), 64, shiftMask = 63),
    HostOpSpec("word", Seq(64, 64), 64, shiftMask = 31),
    HostOpSpec("bitmanip", Seq(64, 64), 64), HostOpSpec("misc", Seq(64, 64), 64),
    HostOpSpec("zicond", Seq(64, 64), 64), HostOpSpec("pc-relative", Seq(64, 64), 64)
  )

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  private val pc = (if (cfg.aluNeedPc) {
    Mux(io.instrAddrTransType.get.shouldBeSext,
      SignExt(in.data.pc.get, cfg.destDataBits),
      ZeroExt(in.data.pc.get, cfg.destDataBits))
  }
  else 0.U
  )

  behaviorCarrier.foreach { carrier =>
    carrier.io.in := (if (cfg.aluNeedPc) Cat(pc, in.ctrl.fuOpType, in.data.src(1), in.data.src(0))
      else Cat(in.ctrl.fuOpType, in.data.src(1), in.data.src(0)))
  }
  aluModule.foreach { module =>
    module.io.src.zip(in.data.src).foreach { case (sink, source) => sink := source }
    module.io.func := in.ctrl.fuOpType
    module.io.pc := pc
  }
  val result = behaviorCarrier.map(_.io.out).orElse(aluModule.map(_.io.result)).get
  out.res.data := result

  if (behavior) {
    SimulationModels.functionalPipeline(
      "int-alu", behaviorCarrier.get, 0, "", Seq((64, 64)),
      if (grouped) behaviorCarrier else None, "none", opSpecs)
  }
}
