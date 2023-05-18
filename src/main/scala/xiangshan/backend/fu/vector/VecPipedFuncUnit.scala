package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.fu.vector.utils.ScalaDupToVector
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

  private val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt

  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to coding.
  protected val srcMask    = if(!cfg.maskWakeUp) inCtrl.vpu.get.vmask else Mux(vm, allMaskTrue, inData.getSrcMask)
  protected val srcVConfig = if(!cfg.vconfigWakeUp) inCtrl.vpu.get.vconfig else inData.getSrcVConfig.asTypeOf(new VConfig)

  // swap vs1 and vs2, used by vrsub, etc
  protected val needReverse = VialuFixType.needReverse(inCtrl.fuOpType)
  // vadc.vv, vsbc.vv need this
  protected val needClearMask = VialuFixType.needClearMask(inCtrl.fuOpType)
}

class VecPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with VecFuncUnitAlias
{
  private val extedVs1 = Wire(UInt(VLEN.W))

  // modules
  private val scalaDupToVector = Module(new ScalaDupToVector(VLEN))

  scalaDupToVector.io.in.scalaData := inData.src(0)
  scalaDupToVector.io.in.vsew := vsew
  extedVs1 := scalaDupToVector.io.out.vecData

  private val src0 = Mux(vecCtrl.needScalaSrc, extedVs1, inData.src(0)) // vs1, rs1, fs1, imm
  private val src1 = WireInit(inData.src(1)) // vs2 only

  protected val vs2 = Mux(needReverse, src0, src1)
  protected val vs1 = Mux(needReverse, src1, src0)
  protected val old_vd = inData.src(2)

  override def latency: Int = cfg.latency.latencyVal.get

}
