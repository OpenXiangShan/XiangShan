package xiangshan.backend.fu.vector

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.DataHoldBypass
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.fu.vector.utils.ScalaDupToVector
import xiangshan.backend.fu.{FuConfig, FuncUnit}
import xiangshan.ExceptionNO.illegalInstr
import yunsuan.VialuFixType

class VecNonPipedFuncUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
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

  protected val vs2 = Mux(isReverse, src0, src1)
  protected val vs1 = Mux(isReverse, src1, src0)
  protected val oldVd = inData.src(2)

  protected val outCtrl     = DataHoldBypass(io.in.bits.ctrl, io.in.fire)
  protected val outData     = DataHoldBypass(io.in.bits.data, io.in.fire)

  protected val outVecCtrl  = outCtrl.vpu.get
  protected val outVm       = outVecCtrl.vm

  // vadc.vv, vsbc.vv need this
  protected val outNeedClearMask: Bool = VialuFixType.needClearMask(outCtrl.fuOpType)

  protected val outVConfig  = if(!cfg.vconfigWakeUp) outCtrl.vpu.get.vconfig else outData.getSrcVConfig.asTypeOf(new VConfig)
  protected val outVl       = outVConfig.vl
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

  // vstart illegal
  if (cfg.exceptionOut.nonEmpty) {
    val outVstart = outCtrl.vpu.get.vstart
    val vstartIllegal = outVstart =/= 0.U
    io.out.bits.ctrl.exceptionVec.get := 0.U.asTypeOf(io.out.bits.ctrl.exceptionVec.get)
    io.out.bits.ctrl.exceptionVec.get(illegalInstr) := vstartIllegal
  }

  connectNonPipedCtrlSingal
}
