package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{AluDataModule, PipedFuncUnit}
import xiangshan.backend.fu.FuConfig

class Alu(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val aluModule = Module(new AluDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  aluModule.io.src.zip(in.data.src).foreach { case (sink, source) =>
    sink := source
  }
  aluModule.io.func := in.ctrl.fuOpType
  out.res.data := aluModule.io.result
}
