package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.fu.FuConfig.VialuCfg
import xiangshan.backend.fu.{FuConfig, FuncUnit, HasPipelineReg}
import yunsuan.vector.Common._

trait VecFuncUnitAlias { this: FuncUnit =>
  protected val inCtrl  = io.in.bits.ctrl
  protected val inData  = io.in.bits.data
  protected val vtype   = inCtrl.vtype.get

  protected val vma     = vtype.vma
  protected val vta     = vtype.vta
  protected val vsew    = vtype.vsew
  protected val vlmul   = vtype.vlmul
  protected val vm      = inCtrl.vm.get

  protected val frm     = io.frm.getOrElse(0.U.asTypeOf(Frm()))
  protected val vxrm    = io.vxrm.getOrElse(0.U(2.W))
  protected val rm      = frm
  protected val vuopIdx = inCtrl.uopIdx.get
  protected val nf      = 0.U  // No need to handle nf in vector arith unit

  protected val fuOpType  = inCtrl.fuOpType
  protected val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
  protected val allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt

  // vadc.vv, vsbc.vv need this
  protected val needClearMask: Bool = if(cfg == VialuCfg) VIAluOpcodes.isPredicateAlwaysTrue(inCtrl.fuOpType) else false.B

  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to coding.
  protected val srcMask: UInt =
    MuxCase(inData.v0.get, Seq(
      needClearMask -> allMaskFalse,
      vm -> allMaskTrue
    ))
  protected val vl = inData.vl.get
}

class VecPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg
  with VecFuncUnitAlias
{
  private val src0 = inData.src(0)
  private val src1 = WireInit(inData.src(1)) // vs2 only
  protected val vs2 = src1
  protected val vs1 = src0
  protected val oldVd = inData.src(2)

  protected val outCtrl     = ctrlVec(cfg.latency.orginLatencyVal.get)
  protected val outData     = dataVec(cfg.latency.orginLatencyVal.get)

  protected val outVType    = outCtrl.vtype.get
  protected val outVm       = outCtrl.vm.get
  protected val outUopIdx   = outCtrl.uopIdx.get

  // vadc.vv, vsbc.vv need this
  protected val outNeedClearMask: Bool = if(cfg == VialuCfg) VIAluOpcodes.isPredicateAlwaysTrue(outCtrl.fuOpType) else false.B
  protected val outVl       = outData.vl.get
  protected val outOldVd    = outData.src(2)
  protected val outVlmul    = outVType.vlmul
  protected val outLastUop  = outCtrl.lastUop.get
  // There is no difference between control-dependency or data-dependency for function unit,
  // but spliting these in ctrl or data bundles is easy to coding.
  protected val outSrcMask: UInt = {
    MuxCase(
      outData.v0.get, Seq(
        outNeedClearMask -> allMaskFalse,
        outVm -> allMaskTrue
      )
    )
  }

  override def latency: Int = cfg.latency.latencyVal.get

}
