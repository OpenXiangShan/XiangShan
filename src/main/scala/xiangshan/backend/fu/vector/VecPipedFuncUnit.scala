package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import yunsuan.VialuFixType

trait VecFuncUnitAlias { this: FuncUnit =>
  protected val inCtrl  = io.in.bits.ctrl
  protected val inData  = io.in.bits.data
  protected val vecCtrl = inCtrl.vpu.get

  protected val vill    = vecCtrl.vill
  protected val vma     = vecCtrl.vma
  protected val vta     = vecCtrl.vta
  protected val vsew    = vecCtrl.vsew
  protected val vlmul   = vecCtrl.vlmul
  protected val vm      = vecCtrl.vm
  protected val vstart  = vecCtrl.vstart

  protected val frm     = vecCtrl.frm
  protected val vxrm    = vecCtrl.vxrm
  protected val vuopIdx = vecCtrl.vuopIdx
  protected val nf      = vecCtrl.frm

  protected val fuOpType  = inCtrl.fuOpType
  protected val isNarrow  = vecCtrl.isNarrow
  protected val isExt     = vecCtrl.isExt
  protected val isMove    = vecCtrl.isMove
  protected val isReverse = vecCtrl.isReverse

  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to codinpjfg.
  protected val srcMask    = if(!cfg.maskWakeUp) inCtrl.vpu.get.vmask else inData.getSrcMask
  protected val srcVConfig = if(!cfg.vconfigWakeUp) inCtrl.vpu.get.vconfig else inData.getSrcVConfig.asTypeOf(new VConfig)

  // swap vs1 and vs2, used by vrsub, etc
  val needReverse = VialuFixType.needReverse(inCtrl.fuOpType)
  // vadc.vv, vsbc.vv need this
  val needClearMask = VialuFixType.needClearMask(inCtrl.fuOpType)

}

class VecPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with VecFuncUnitAlias
{

  override def latency: Int = cfg.latency.latencyVal.get

}
