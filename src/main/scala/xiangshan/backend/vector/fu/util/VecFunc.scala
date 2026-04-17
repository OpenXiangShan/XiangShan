package xiangshan.backend.vector.fu.util

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.MuxCase
import xiangshan._
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.backend.datapath.DataConfig.V0Data
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.vector.fu.util.VecFuConfig.VialuCfg


trait VecFuncAlias { this: Func =>
  protected def ex0ctrl = in.ex.head.bits.ctrl
  protected def ex0data = in.ex.head.bits.data
  protected def ex0vs1 = ex0data.src(0)
  protected def ex0vs2 = ex0data.src(1)
  protected def ex0oldVd = ex0data.src(2)
  protected def ex0vl = ex0data.vl.get

  protected def vxrm = in.vxrm.get

  protected def ex0vma = ex0ctrl.vtype.get.vma
  protected def ex0vta = ex0ctrl.vtype.get.vta
  protected def ex0vm = ex0ctrl.vm.get
  
  protected def ex0uopIdx = ex0ctrl.uopIdx

  protected def vstart = 0.U

  protected def allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
  protected def allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt

  protected def vs1Ex: Seq[UInt] = in.ex.map(_.bits.data.src(0))
  protected def vs2Ex: Seq[UInt] = in.ex.map(_.bits.data.src(1))
  protected def vs3Ex: Seq[UInt] = in.ex.map(_.bits.data.src(2))
  protected def oldVdEx: Seq[UInt] = in.ex.map(_.bits.data.src(2))
}

class VecFixLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) with VecFuncAlias {
  protected val needClearMask: Bool = false.B
  protected val srcMask: UInt =
    MuxCase(ex0data.v0.get, Seq(
      needClearMask -> allMaskFalse,
      ex0vm -> allMaskTrue
    ))
}


class VecNoFixLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) 
  with VecFuncAlias {

}
