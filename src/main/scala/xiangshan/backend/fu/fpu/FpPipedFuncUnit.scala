package xiangshan.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import xiangshan.backend.fu.fpu.Bundles._

trait FpFuncUnitAlias { this: FuncUnit =>
  protected val inCtrl  = io.in.bits.ctrl
  protected val inData  = io.in.bits.data
  protected val fp_fmt  = inCtrl.fuOpType(2, 1)

  protected val frm     = io.frm.getOrElse(0.U.asTypeOf(Frm()))
  protected val instRm  = inCtrl.frm.getOrElse(0.U.asTypeOf(Frm()))
  protected val rm      = Mux(instRm =/= "b111".U, instRm, frm)

  protected val fuOpType  = inCtrl.fuOpType
}

class FpPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with FpFuncUnitAlias
{
  protected val outCtrl     = ctrlVec(cfg.latency.orginLatencyVal.get)
  protected val outData     = dataVec(cfg.latency.orginLatencyVal.get)

  override def latency: Int = cfg.latency.latencyVal.get

}
