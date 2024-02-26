package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.FuConfig.VialuCfg
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

  protected val frm     = if(cfg.needSrcFrm) io.frm.get else 0.U
  protected val vxrm    = if(cfg.needSrcVxrm) io.vxrm.get else 0.U
  protected val vuopIdx = vecCtrl.vuopIdx
  protected val nf      = 0.U  // No need to handle nf in vector arith unit

  protected val fuOpType  = inCtrl.fuOpType
  protected val isNarrow  = vecCtrl.isNarrow
  protected val isExt     = vecCtrl.isExt
  protected val isMove    = vecCtrl.isMove
  // swap vs1 and vs2, used by vrsub, etc
  protected val isReverse = vecCtrl.isReverse

  protected val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
  protected val allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt

  // vadc.vv, vsbc.vv need this
  protected val needClearMask: Bool = if(cfg == VialuCfg) VialuFixType.needClearMask(inCtrl.fuOpType) else false.B

  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to coding.
  protected val srcMask: UInt = if(!cfg.maskWakeUp) inCtrl.vpu.get.vmask else {
    MuxCase(inData.getSrcMask, Seq(
      needClearMask -> allMaskFalse,
      vm -> allMaskTrue
    ))
  }
  protected val srcVConfig: VConfig = if(!cfg.vconfigWakeUp) inCtrl.vpu.get.vconfig else inData.getSrcVConfig.asTypeOf(new VConfig)
  protected val vl = srcVConfig.vl
}

class VecPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with VecFuncUnitAlias
{
  private val src0 = inData.src(0)
  private val src1 = WireInit(inData.src(1)) // vs2 only
  if(cfg == FuConfig.VfaluCfg){
    val vs2Fold = Wire(UInt(VLEN.W))
    vs2Fold := Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> inData.src(1)(VLEN/1-1, VLEN/2),
        vecCtrl.fpu.isFoldTo1_4 -> inData.src(1)(VLEN/2-1, VLEN/4),
        vecCtrl.fpu.isFoldTo1_8 -> inData.src(1)(VLEN/4-1, VLEN/8),
      )
    )
    src1 := Mux(vecCtrl.fpu.isFoldTo1_2 || vecCtrl.fpu.isFoldTo1_4 || vecCtrl.fpu.isFoldTo1_8, vs2Fold, inData.src(1))
  }
  protected val vs2 = Mux(isReverse, src0, src1)
  protected val vs1 = Mux(isReverse, src1, src0)
  protected val oldVd = inData.src(2)

  protected val outCtrl     = ctrlVec.last
  protected val outData     = dataVec.last

  protected val outVecCtrl  = outCtrl.vpu.get
  protected val outVm       = outVecCtrl.vm

  // vadc.vv, vsbc.vv need this
  protected val outNeedClearMask: Bool = if(cfg == VialuCfg) VialuFixType.needClearMask(outCtrl.fuOpType) else false.B
  protected val outVConfig  = if(!cfg.vconfigWakeUp) outCtrl.vpu.get.vconfig else outData.getSrcVConfig.asTypeOf(new VConfig)
  protected val outVl       = outVConfig.vl
  protected val outVstart   = outVecCtrl.vstart
  protected val outOldVd    = outData.src(2)
  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to coding.
  protected val outSrcMask: UInt = if (!cfg.maskWakeUp) outCtrl.vpu.get.vmask else {
    MuxCase(
      outData.getSrcMask, Seq(
        outNeedClearMask -> allMaskFalse,
        outVm -> allMaskTrue
      )
    )
  }

  override def latency: Int = cfg.latency.latencyVal.get

}
