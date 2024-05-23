package xiangshan.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}

trait FpFuncUnitAlias { this: FuncUnit =>
  protected val inCtrl  = io.in.bits.ctrl
  protected val inData  = io.in.bits.data
  protected val fpCtrl  = inCtrl.vpu.get
  protected val fp_fmt  = fpCtrl.vsew  // TODO: use fpu

  protected val frm     = io.frm.getOrElse(0.U(3.W))
  protected val instRm  = inCtrl.fpu.getOrElse(0.U.asTypeOf(new FPUCtrlSignals)).rm
  protected val rm      = Mux(instRm =/= "b111".U, instRm, frm)

  protected val fuOpType  = inCtrl.fuOpType
}

class FpPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with FpFuncUnitAlias
{
  protected val outCtrl     = ctrlVec.last
  protected val outData     = dataVec.last

  override def latency: Int = cfg.latency.latencyVal.get

}
