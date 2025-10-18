package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{AluDataModule, PipedFuncUnit}
import xiangshan.backend.fu.FuConfig
import utility.{SignExt, ZeroExt}

class Alu(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val aluModule = Module(new AluDataModule(cfg.aluNeedPc))

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  private val in = io.in.bits
  private val out = io.out.bits
  aluModule.io.src.zip(in.data.src).foreach { case (sink, source) =>
    sink := source
  }
  aluModule.io.func := in.ctrl.fuOpType
  aluModule.io.pc := (if (cfg.aluNeedPc) {
    Mux(io.instrAddrTransType.get.shouldBeSext,
      SignExt(in.data.pc.get, cfg.destDataBits),
      ZeroExt(in.data.pc.get, cfg.destDataBits))
  }
  else 0.U
  )
  out.res.data := aluModule.io.result
}
