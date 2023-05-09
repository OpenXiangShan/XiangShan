package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.fu.{AluDataModule, FuncUnit}
import xiangshan.backend.fu.FuConfig

class Alu(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val aluModule = Module(new AluDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  aluModule.io.src := in.data.src
  aluModule.io.func := in.ctrl.fuOpType
  out.res.data := aluModule.io.result
  connectNonPipedCtrlSingal
}
