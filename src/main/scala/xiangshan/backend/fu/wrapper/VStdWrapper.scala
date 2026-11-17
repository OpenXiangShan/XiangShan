package xiangshan.backend.fu.wrapper

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.vector.fu.{Func, VecFuConfig}

class VStdWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) {
  require(cfg.name == "vstd")

  out.ex.zip(in.ex).foreach { case (outStage, inStage) =>
    outStage.bits.data.vstd.foreach { vstd =>
      vstd.fuType := cfg.fuType.U
      vstd.fuOpType := inStage.bits.ctrl.opcode
      vstd.data := inStage.bits.data.src(0)
      vstd.sqIdx := inStage.bits.ctrl.sqIdx.get
      vstd.vecDebug.foreach(_.start := 0.U)
      vstd.vecDebug.foreach(_.offset := 0.U)
    }
  }
}
