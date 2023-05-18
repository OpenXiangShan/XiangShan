package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.XSCoreParamsKey
import xiangshan.backend.fu.vector.Bundles.{VConfig, VSew}
import xiangshan.backend.fu.vector.{VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import xiangshan.backend.fu.{FuConfig, FuType}
import yunsuan.{OpType, VialuFixType}
import yunsuan.vector.alu.VIntFixpAlu
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.encoding.Opcode.VialuOpcode

class VIAluSrcTypeIO extends Bundle {
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

class VIAluSrcTypeModule extends Module {
  val io: VIAluSrcTypeIO = IO(new VIAluSrcTypeIO)

  private val vsew = io.in.vsew
  private val isExt = io.in.isExt
  private val isDstMask = io.in.isDstMask

  private val opcode = VialuFixType.getOpcode(io.in.fuOpType)
  private val isSign = VialuFixType.isSigned(io.in.fuOpType)
  private val format = VialuFixType.getFormat(io.in.fuOpType)

  private val vsewX2 = vsew + 1.U
  private val vsewF2 = vsew - 1.U
  private val vsewF4 = vsew - 2.U
  private val vsewF8 = vsew - 3.U

  private val isAddSub = opcode === VialuOpcode.vadd || opcode === VialuOpcode.vsub
  private val isShiftRight = Seq(VialuOpcode.vsrl, VialuOpcode.vsra, VialuOpcode.vssrl, VialuOpcode.vssra).map(fmt => fmt === format).reduce(_ || _)
  private val isVext = opcode === VialuOpcode.vext

  private val isWiden = isAddSub && Seq(VialuFixType.FMT.VVW, VialuFixType.FMT.WVW).map(fmt => fmt === format).reduce(_ || _)
  private val isNarrow = isShiftRight && format === VialuFixType.FMT.WVV
  private val isVextF2 = isVext && format === VialuFixType.FMT.VF2
  private val isVextF4 = isVext && format === VialuFixType.FMT.VF4
  private val isVextF8 = isVext && format === VialuFixType.FMT.VF8

  // check illegal
  private val widenIllegal  = isWiden   && vsewX2 === VSew.e8
  private val narrowIllegal = isNarrow  && vsewF2 === VSew.e64
  private val vextIllegal   = (isVextF2 && (vsewF2 === VSew.e64)) ||
    (isVextF4 && (vsewF4 === VSew.e64)) ||
    (isVextF8 && (vsewF8 === VSew.e64))
  // Todo: use it
  private val illegal = widenIllegal || narrowIllegal || vextIllegal

  private class Vs2Vs1VdSew extends Bundle {
    val vs2 = VSew()
    val vs1 = VSew()
    val vd = VSew()
  }

  private val addSubSews = Mux1H(Seq(
    (format === VialuFixType.FMT.VVV) -> Cat(vsew  ,  vsew, vsew  ),
    (format === VialuFixType.FMT.VVW) -> Cat(vsew  ,  vsew, vsewX2),
    (format === VialuFixType.FMT.WVW) -> Cat(vsewX2,  vsew, vsewX2),
    (format === VialuFixType.FMT.WVV) -> Cat(vsewX2,  vsew, vsew  ),
  )).asTypeOf(new Vs2Vs1VdSew)

  private val vextSews = Mux1H(Seq(
    (format === VialuFixType.FMT.VF2) -> Cat(vsewF2, vsewF2, vsew),
    (format === VialuFixType.FMT.VF4) -> Cat(vsewF4, vsewF4, vsew),
    (format === VialuFixType.FMT.VF8) -> Cat(vsewF8, vsewF8, vsew),
  )).asTypeOf(new Vs2Vs1VdSew)

  // Todo
  private val maskSews = Mux1H(Seq(
    (format === VialuFixType.FMT.VVMV) -> Cat(vsew, vsew, vsew),
    (format === VialuFixType.FMT.VVM ) -> Cat(vsew, vsew, vsew),
    (format === VialuFixType.FMT.MMM ) -> Cat(vsew, vsew, vsew),
  )).asTypeOf(new Vs2Vs1VdSew)

  private val vs2Type = Mux1H(Seq(
    isExt     -> Cat(0.U(1.W), isSign, vextSews.vs2),
    (!isExt && !isDstMask) -> Cat(0.U(1.W), isSign, addSubSews.vs2),
  ))
  private val vs1Type = Mux1H(Seq(
    isExt     -> Cat(0.U(1.W), isSign, vextSews.vs1),
    (!isExt && !isDstMask) -> Cat(0.U(1.W), isSign, addSubSews.vs1),
  ))
  private val vdType = Mux1H(Seq(
    isExt     -> Cat(0.U(1.W), isSign, vextSews.vd),
    (!isExt && !isDstMask) -> Cat(0.U(1.W), isSign, addSubSews.vd),
  ))

  io.out.vs2Type := vs2Type
  io.out.vs1Type := vs1Type
  io.out.vdType := vdType
  io.out.illegal := illegal
}

class VIAluFix(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VialuFixType.dummy, "VialuF OpType not supported")

  // modules

  private val typeModule = Module(new VIAluSrcTypeModule)
  private val vIntFixpAlu = Module(new VIntFixpAlu)

  val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(srcMask)
  val maskIdx = Mux(isNarrow, (vuopIdx >> 1.U).asUInt, vuopIdx)
  val maskUsed = maskDataVec(maskIdx)

  val vconfig = srcVConfig
  val vl = vconfig.vl


  /**
    * [[typeModule]]'s io connection
    */
  typeModule.io.in.fuOpType := fuOpType
  typeModule.io.in.vsew := vsew
  typeModule.io.in.isReverse := isReverse
  typeModule.io.in.isExt := isExt
  typeModule.io.in.isDstMask := vecCtrl.isDstMask
  typeModule.io.in.isMove := isMove

  /**
    * [[vIntFixpAlu]]'s io connection
    */
  vIntFixpAlu.io match {
    case subIO =>
      subIO.in.opcode       := VialuFixType.getOpcode(inCtrl.fuOpType).asTypeOf(subIO.in.opcode)
      subIO.in.info.vm      := vm
      subIO.in.info.ma      := vma
      subIO.in.info.ta      := vta
      subIO.in.info.vlmul   := vlmul
      subIO.in.info.vl      := srcVConfig.vl
      subIO.in.info.vstart  := vstart
      subIO.in.info.uopIdx  := vuopIdx
      subIO.in.info.vxrm    := vxrm
      subIO.in.srcType(0)   := typeModule.io.out.vs2Type
      subIO.in.srcType(1)   := typeModule.io.out.vs1Type
      subIO.in.vdType       := typeModule.io.out.vdType
      subIO.in.vs2          := vs2
      subIO.in.vs1          := vs1
      subIO.in.old_vd       := old_vd
      subIO.in.mask16b      := maskUsed // Todo: make mask16b more flexiable
      subIO.ctrl.narrow     := isNarrow
      subIO.ctrl.vstart_gte_vl := vstart >= vl
  }

  io.out.bits.res.data := vIntFixpAlu.io.out.vd
  io.out.bits.res.vxsat.foreach(_ := vIntFixpAlu.io.out.vxsat)
}
