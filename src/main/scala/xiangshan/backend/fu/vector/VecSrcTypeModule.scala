package xiangshan.backend.fu.vector

import chisel3._
import xiangshan.backend.fu.vector.Bundles.VSew
import yunsuan.OpType
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}

class VecSrcTypeModuleIO extends Bundle {
  val in = Input(new Bundle {
    val fuOpType  : UInt = OpType()
    val vsew      : UInt = VSew()
    val isReverse : Bool = Bool() // vrsub, vrdiv
    val isExt     : Bool = Bool()
    val isDstMask : Bool = Bool() // vvm, vvvm, mmm
    val isMove    : Bool = Bool() // vmv.s.x, vmv.v.v, vmv.v.x, vmv.v.i
  })
  val out = Output(new Bundle {
    val vs1Type : UInt = Vs1IntType()
    val vs2Type : UInt = Vs2IntType()
    val vdType  : UInt = VdType()
    val illegal : Bool = Bool()
  })
}

abstract class VecSrcTypeModule extends Module {
  val io = IO(new VecSrcTypeModuleIO)

  protected val fuOpType = io.in.fuOpType
  protected val vsew = io.in.vsew
  protected val isExt = io.in.isExt
  protected val isDstMask = io.in.isDstMask
}
