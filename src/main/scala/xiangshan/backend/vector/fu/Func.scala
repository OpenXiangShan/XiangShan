package xiangshan.backend.vector.fu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.decode.opcode.Latency
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.VecRegionModule
import xiangshan.mem.{SqPtr, StoreQueueDataWrite}
import yunsuan.vector.Common.Fflags


class Func(val cfg: VecFuConfig)(implicit p: Parameters) extends XSModule {
  implicit val _cfg: VecFuConfig = cfg

  val in = IO(Input(new Func.In))
  val out = IO(Output(new Func.Out))

  // Ex0 stage register is in Exu, ex0Next is used to generate decoded signal for this fu.
  val ex0Next = in.ex0Next
  val ex = in.ex

  val exValids: Seq[Bool] = ex.map(_.valid)
  val pipeRegValids = ex0Next.valid +: exValids

  out.ex.foreach { out =>
    out.valid := false.B
  }

  for (((inStage: ValidIO[Func.InUop], outStage: ValidIO[Func.OutUop]), i) <- (in.ex zip out.ex).zipWithIndex) {
    outStage.valid := inStage.valid && inStage.bits.ctrl.latency === i.U
    outStage.bits.ctrl.robIdx := inStage.bits.ctrl.robIdx
    outStage.bits.ctrl.pdest := inStage.bits.ctrl.pdest
    outStage.bits.ctrl.pdestV0.foreach(_ := inStage.bits.ctrl.pdestV0.get)
    outStage.bits.ctrl.pdestVl.foreach(_ := inStage.bits.ctrl.pdestVl.get)
    outStage.bits.ctrl.rfWen.foreach(_ := inStage.bits.ctrl.rfWen.get)
    outStage.bits.ctrl.fpWen.foreach(_ := inStage.bits.ctrl.fpWen.get)
    outStage.bits.ctrl.vecWen.foreach(_ := inStage.bits.ctrl.vecWen.get)
    outStage.bits.ctrl.v0Wen.foreach(_ := inStage.bits.ctrl.v0Wen.get)
    outStage.bits.ctrl.vlWen.foreach(_ := inStage.bits.ctrl.vlWen.get)
    outStage.bits.ctrl.exceptionVec.foreach(_ := 0.U.asTypeOf(outStage.bits.ctrl.exceptionVec.get))
    outStage.bits.ctrl.flushPipe.foreach(_ := inStage.bits.ctrl.flushPipe.get)
    outStage.bits.ctrl.replay.foreach(_ := false.B)
    outStage.bits.ctrl.isRVC.foreach(_ := false.B)
    outStage.bits.ctrl.fflagsWen.foreach(_ := inStage.bits.ctrl.fflagsWen.get)
    outStage.bits.debug.foreach(_ := inStage.bits.debug.get)
  }

  // Downstream Vec Exu exposes only one aggregated output per cycle.
  // Mixed-latency uops must therefore be scheduled so this Func never
  // has more than one output stage valid at the same time.
  assert(
    PopCount(out.ex.map(_.valid)) <= 1.U,
    s"${cfg.name} produced multiple outputs in one cycle"
  )

  val fuOpType = in.ex.head.bits.ctrl.opcode
}

object Func {
  class In(implicit val cfg: VecFuConfig, p: Parameters) extends XSBundle {
    val flush = Flipped(ValidIO(new Redirect))
    val ex0Next = ValidIO(new InUop)
    val ex = Vec(cfg.latency + 1, ValidIO(new InUop))
    val frm = Option.when(cfg.needSrcFrm)(Frm())
    val vxrm = Option.when(cfg.needSrcVxrm)(Vxrm())
  }

  class Out(implicit val cfg: VecFuConfig, p: Parameters) extends XSBundle {
    val ex = Vec(cfg.latency + 1, ValidIO(new OutUop))
  }

  class InUop(implicit val cfg: VecFuConfig, p: Parameters) extends XSBundle {
    val ctrl = new InCtrl(cfg)
    val data = new InData(cfg)
    val debug = Option.when(backendParams.debugEn)(new VecRegionModule.DebugBundle)
  }

  class OutUop(implicit val cfg: VecFuConfig, p: Parameters) extends XSBundle {
    val ctrl = new OutCtrl(cfg)
    val data = new OutData(cfg)
    val debug = Option.when(backendParams.debugEn)(new VecRegionModule.DebugBundle)
  }

  class InCtrl(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val opcode    = FuOpType()
    val latency   = Latency()
    val robIdx    = new RobPtr
    val uopIdx    = UopIdx()
    val sqIdx     = Option.when(cfg.needSqIdx)(new SqPtr)
    val pdest     = UInt(PhyRegIdxWidth.W)
    val pdestV0   = Option.when(cfg.writeV0Rf)(UInt(V0PhyRegIdxWidth.W))
    val pdestVl   = Option.when(cfg.writeVlRf)(UInt(VlPhyRegIdxWidth.W))
    val rfWen     = Option.when(cfg.needIntWen)(Bool())
    val fpWen     = Option.when(cfg.needFpWen)(Bool())
    val vecWen    = Option.when(cfg.needVecWen)(Bool())
    val v0Wen     = Option.when(cfg.needV0Wen)(Bool())
    val vlWen     = Option.when(cfg.needVlWen)(Bool())
    val flushPipe = Option.when(cfg.flushPipe)(Bool())
    val fflagsWen = Option.when(cfg.writeFflags)(Bool())
    val vtype     = Option.when(cfg.readVType)(VType())
    val oldVType  = Option.when(cfg.writeVType)(VType())
    val vm        = Option.when(cfg.readVType)(Bool())
  }

  class InData(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val src       = MixedVec(cfg.genSrcDataVec)
    val v0        = Option.when(cfg.readV0)(V0())
    val vl        = Option.when(cfg.readVl)(Vl())
    val imm       = UInt(cfg.destDataBits.W)
    val pc        = Option.when(cfg.needPc)(UInt(VAddrData().dataWidth.W))
  }

  class OutCtrl(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val robIdx        = new RobPtr
    val pdest         = UInt(PhyRegIdxWidth.W)
    val pdestV0       = Option.when(cfg.writeV0Rf)(UInt(V0PhyRegIdxWidth.W))
    val pdestVl       = Option.when(cfg.writeVlRf)(UInt(VlPhyRegIdxWidth.W))
    val rfWen         = Option.when(cfg.needIntWen)(Bool())
    val fpWen         = Option.when(cfg.needFpWen)(Bool())
    val vecWen        = Option.when(cfg.needVecWen)(Bool())
    val v0Wen         = Option.when(cfg.needV0Wen)(Bool())
    val vlWen         = Option.when(cfg.needVlWen)(Bool())
    val exceptionVec  = Option.when(cfg.exceptionOut.nonEmpty)(ExceptionVec())
    val flushPipe     = Option.when(cfg.flushPipe)(Bool())
    val replay        = Option.when(cfg.replayInst)(Bool())
    val isRVC         = Option.when(cfg.hasIsRVC)(Bool())
    val fflagsWen     = Option.when(cfg.writeFflags)(Bool())
  }

  class OutData(cfg: VecFuConfig)(implicit p: Parameters) extends XSBundle {
    val int  = Option.when(cfg.writeIntRf)(UInt(IntData().dataWidth.W))
    val fp   = Option.when(cfg.writeFpRf)(UInt(FpData().dataWidth.W))
    val fflags   = Option.when(cfg.writeFflags && !cfg.isVecArith)(UInt(5.W))
    val redirect = Option.when(cfg.hasRedirect)(ValidIO(new Redirect))
    val vec  = Option.when(cfg.writeVecRf)(new VecSpecialData(cfg, VLEN))
    val vstd = Option.when(cfg.isVStd)(new StoreQueueDataWrite)
  }

  class VecSpecialData(cfg: VecFuConfig, vlen: Int) extends Bundle {
    val vlenb = vlen / 8

    val normal   = UInt(vlen.W)
    val narrow   = UInt((vlen / 2).W)

    val maskE8   = UInt(vlenb.W)
    val maskE16  = UInt((vlenb / 2).W)
    val maskE32  = UInt((vlenb / 4).W)
    val maskE64  = UInt((vlenb / 8).W)

    val vxsatE8: Option[Vec[UInt]] = Option.when(cfg.writeVxsat)(Vec(vlenb, Vxsat()))
    val narrowVxsatE8: Option[Vec[UInt]] = Option.when(cfg.writeVxsat)(Vec(vlenb / 2, Vxsat()))
    val fflagsE8: Option[Vec[UInt]] = Option.when(cfg.writeFflags)(Vec(vlenb, Fflags()))
    val narrowFflagsE8: Option[Vec[UInt]] = Option.when(cfg.writeFflags)(Vec(vlenb, Fflags()))

    // Todo: floatpoint data before normalizing
  }

  class PipeReg[T <: Data](gen: => T, num: Int) extends Bundle {
    val ex = Vec(num, gen)

    def ex0: T = ex(0)
    def ex1: T = ex(1)
    def ex2: T = ex(2)
    def ex3: T = ex(3)
  }

  def makePipeRegImpl[T <: Data](signal: T, validVec: Iterable[Bool]): Seq[T] = {
    val seq: T = RegEnable(signal, validVec.head)

    if (validVec.size == 1) {
      Seq(seq)
    } else {
      seq +: makePipeRegImpl(seq, validVec.drop(1))
    }
  }

  def makePipeReg[T <: Data](signal: T, validVec: Iterable[Bool]): PipeReg[T] = {
    val pipe = Wire(new PipeReg(chiselTypeOf(signal), validVec.size))
    val regs = makePipeRegImpl(signal, validVec)

    for (i <- pipe.ex.indices) {
      pipe.ex(i) := regs(i)
    }

    pipe
  }
}
