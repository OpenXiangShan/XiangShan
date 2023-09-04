package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3.{VecInit, _}
import chisel3.util._
import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}
import utility.DelayN
import utils.XSError
import xiangshan.XSCoreParamsKey
import xiangshan.backend.fu.vector.Bundles.{VConfig, VSew, ma}
import xiangshan.backend.fu.vector.{Mgu, Mgtu, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.{FuConfig, FuType}
import yunsuan.{OpType, VialuFixType}
import yunsuan.vector.alu.{VIntFixpAlu64b, VIntFixpDecode, VIntFixpTable}
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.encoding.Opcode.VialuOpcode
import yunsuan.vector.SewOH

class VIAluSrcTypeIO extends Bundle {
  val in = Input(new Bundle {
    val fuOpType: UInt = OpType()
    val vsew: UInt = VSew()
    val isReverse: Bool = Bool() // vrsub, vrdiv
    val isExt: Bool = Bool()
    val isDstMask: Bool = Bool() // vvm, vvvm, mmm
    val isMove: Bool = Bool() // vmv.s.x, vmv.v.v, vmv.v.x, vmv.v.i
  })
  val out = Output(new Bundle {
    val vs1Type: UInt = Vs1IntType()
    val vs2Type: UInt = Vs2IntType()
    val vdType: UInt = VdType()
    val illegal: Bool = Bool()
    val isVextF2: Bool = Bool()
    val isVextF4: Bool = Bool()
    val isVextF8: Bool = Bool()
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
  private val widenIllegal = isWiden && vsewX2 === VSew.e8
  private val narrowIllegal = isNarrow && vsewF2 === VSew.e64
  private val vextIllegal = (isVextF2 && (vsewF2 === VSew.e64)) ||
    (isVextF4 && (vsewF4 === VSew.e64)) ||
    (isVextF8 && (vsewF8 === VSew.e64))
  // Todo: use it
  private val illegal = widenIllegal || narrowIllegal || vextIllegal

  private val intType = Cat(0.U(1.W), isSign)

  private class Vs2Vs1VdSew extends Bundle {
    val vs2 = VSew()
    val vs1 = VSew()
    val vd = VSew()
  }

  private class Vs2Vs1VdType extends Bundle {
    val vs2 = Vs2IntType()
    val vs1 = Vs1IntType()
    val vd = VdType()
  }

  private val addSubSews = Mux1H(Seq(
    (format === VialuFixType.FMT.VVV) -> Cat(vsew, vsew, vsew),
    (format === VialuFixType.FMT.VVW) -> Cat(vsew, vsew, vsewX2),
    (format === VialuFixType.FMT.WVW) -> Cat(vsewX2, vsew, vsewX2),
    (format === VialuFixType.FMT.WVV) -> Cat(vsewX2, vsew, vsew),
  )).asTypeOf(new Vs2Vs1VdSew)

  private val vextSews = Mux1H(Seq(
    (format === VialuFixType.FMT.VF2) -> Cat(vsewF2, vsewF2, vsew),
    (format === VialuFixType.FMT.VF4) -> Cat(vsewF4, vsewF4, vsew),
    (format === VialuFixType.FMT.VF8) -> Cat(vsewF8, vsewF8, vsew),
  )).asTypeOf(new Vs2Vs1VdSew)

  private val maskTypes = Mux1H(Seq(
    (format === VialuFixType.FMT.VVM) -> Cat(Cat(intType, vsew), Cat(intType, vsew), VdType.mask),
    (format === VialuFixType.FMT.VVMM) -> Cat(Cat(intType, vsew), Cat(intType, vsew), VdType.mask),
    (format === VialuFixType.FMT.MMM) -> Cat(Vs2IntType.mask, Vs1IntType.mask, VdType.mask),
  )).asTypeOf(new Vs2Vs1VdType)

  private val vs2Type = Mux1H(Seq(
    isDstMask -> maskTypes.vs2,
    isExt -> Cat(intType, vextSews.vs2),
    (!isExt && !isDstMask) -> Cat(intType, addSubSews.vs2),
  ))
  private val vs1Type = Mux1H(Seq(
    isDstMask -> maskTypes.vs1,
    isExt -> Cat(intType, vextSews.vs1),
    (!isExt && !isDstMask) -> Cat(intType, addSubSews.vs1),
  ))
  private val vdType = Mux1H(Seq(
    isDstMask -> maskTypes.vd,
    isExt -> Cat(intType, vextSews.vd),
    (!isExt && !isDstMask) -> Cat(intType, addSubSews.vd),
  ))

  io.out.vs2Type := vs2Type
  io.out.vs1Type := vs1Type
  io.out.vdType := vdType
  io.out.illegal := illegal
  io.out.isVextF2 := isVextF2
  io.out.isVextF4 := isVextF4
  io.out.isVextF8 := isVextF8
}

class VIAluFix(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VialuFixType.dummy, "VialuF OpType not supported")

  // config params
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // modules
  private val typeMod = Module(new VIAluSrcTypeModule)
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vIntFixpAlus = Seq.fill(numVecModule)(Module(new VIntFixpAlu64b))
  private val mgu = Module(new Mgu(dataWidth))
  private val mgtu = Module(new Mgtu(dataWidth))

  /**
   * [[typeMod]]'s in connection
   */
  typeMod.io.in.fuOpType := fuOpType
  typeMod.io.in.vsew := vsew
  typeMod.io.in.isReverse := isReverse
  typeMod.io.in.isExt := isExt
  typeMod.io.in.isDstMask := vecCtrl.isDstMask
  typeMod.io.in.isMove := isMove

  private val vs2GroupedVec32b: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs2GroupedVec16b: Vec[UInt] = VecInit(vs2Split.io.outVec16b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs2GroupedVec8b: Vec[UInt] = VecInit(vs2Split.io.outVec8b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)

  /**
   * In connection of [[vs2Split]], [[vs1Split]] and [[oldVdSplit]]
   */
  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1
  oldVdSplit.io.inVecData := oldVd

  /**
   * [[vIntFixpAlus]]'s in connection
   */
  private val opcode = VialuFixType.getOpcode(inCtrl.fuOpType).asTypeOf(vIntFixpAlus.head.io.opcode)
  private val vs1Type = typeMod.io.out.vs1Type
  private val vs2Type = typeMod.io.out.vs2Type
  private val vdType = typeMod.io.out.vdType
  private val isVextF2 = typeMod.io.out.isVextF2
  private val isVextF4 = typeMod.io.out.isVextF4
  private val isVextF8 = typeMod.io.out.isVextF8

  private val truthTable = TruthTable(VIntFixpTable.table, VIntFixpTable.default)
  private val decoderOut = decoder(QMCMinimizer, Cat(opcode.op), truthTable)
  private val vIntFixpDecode = decoderOut.asTypeOf(new VIntFixpDecode)
  private val isFixp = Mux(vIntFixpDecode.misc, opcode.isScalingShift, opcode.isSatAdd || opcode.isAvgAdd)
  private val widen = opcode.isAddSub && vs1Type(1, 0) =/= vdType(1, 0)
  private val widen_vs2 = widen && vs2Type(1, 0) =/= vdType(1, 0)
  private val eewVs1 = SewOH(vs1Type(1, 0))
  private val eewVd = SewOH(vdType(1, 0))

  // Extension instructions
  private val vf2 = isVextF2
  private val vf4 = isVextF4
  private val vf8 = isVextF8

  private val vs1VecUsed: Vec[UInt] = Mux(widen || isNarrow, vs1GroupedVec, vs1Split.io.outVec64b)
  private val vs2VecUsed = Wire(Vec(numVecModule, UInt(64.W)))
  when(vf2 || widen_vs2) {
    vs2VecUsed := vs2GroupedVec32b
  }.elsewhen(vf4) {
    vs2VecUsed := vs2GroupedVec16b
  }.elsewhen(vf8) {
    vs2VecUsed := vs2GroupedVec8b
  }.otherwise {
    vs2VecUsed := vs2Split.io.outVec64b
  }

  // mask
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(srcMask, vsew)
  private val maskIdx = Mux(isNarrow, (vuopIdx >> 1.U).asUInt, vuopIdx)
  private val eewVd_is_1b = vdType === VdType.mask
  private val maskUsed = splitMask(maskDataVec(maskIdx), Mux(eewVd_is_1b, eewVs1, eewVd))

  private val oldVdUsed = splitMask(VecDataToMaskDataVec(oldVd, vs1Type(1, 0))(vuopIdx), eewVs1)

  vIntFixpAlus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.opcode := opcode

      mod.io.info.vm := vm
      mod.io.info.ma := vma
      mod.io.info.ta := vta
      mod.io.info.vlmul := vlmul
      mod.io.info.vl := vl
      mod.io.info.vstart := vstart
      mod.io.info.uopIdx := vuopIdx
      mod.io.info.vxrm := vxrm

      mod.io.srcType(0) := vs2Type
      mod.io.srcType(1) := vs1Type
      mod.io.vdType := vdType
      mod.io.narrow := isNarrow
      mod.io.isSub := vIntFixpDecode.sub
      mod.io.isMisc := vIntFixpDecode.misc
      mod.io.isFixp := isFixp
      mod.io.widen := widen
      mod.io.widen_vs2 := widen_vs2
      mod.io.vs1 := vs1VecUsed(i)
      mod.io.vs2 := vs2VecUsed(i)
      mod.io.vmask := maskUsed(i)
      mod.io.oldVd := oldVdUsed(i)
  }

  /**
   * [[mgu]]'s in connection
   */
  private val outEewVs1 = DelayN(eewVs1, latency)

  private val outVd = Cat(vIntFixpAlus.reverse.map(_.io.vd))
  private val outCmp = Mux1H(outEewVs1.oneHot, Seq(8, 4, 2, 1).map(
    k => Cat(vIntFixpAlus.reverse.map(_.io.cmpOut(k - 1, 0)))))
  private val outNarrow = Cat(vIntFixpAlus.reverse.map(_.io.narrowVd))

  /* insts whose mask is not used to generate 'agnosticEn' and 'keepEn' in mgu:
   * vadc, vmadc...
   * vmerge
   */
  private val needNoMask = VialuFixType.needNoMask(outCtrl.fuOpType)
  private val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)

  private val outFormat = VialuFixType.getFormat(outCtrl.fuOpType)
  private val outWiden = (outFormat === VialuFixType.FMT.VVW | outFormat === VialuFixType.FMT.WVW) & !outVecCtrl.isExt & !outVecCtrl.isDstMask
  private val narrow = outVecCtrl.isNarrow
  private val dstMask = outVecCtrl.isDstMask

  private val outEew = Mux(outWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)

  mgu.io.in.vd := MuxCase(outVd, Seq(
    narrow -> outNarrow,
    dstMask -> outCmp,
  ))
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := narrow
  mgu.io.in.info.dstMask := dstMask

  /**
   * [[mgtu]]'s in connection, for vmask instructions
   */
  mgtu.io.in.vd := outVd
  mgtu.io.in.vl := outVl

  io.out.bits.res.data := Mux(outVecCtrl.isOpMask, mgtu.io.out.vd, mgu.io.out.vd)
  io.out.bits.res.vxsat.get := (Cat(vIntFixpAlus.map(_.io.vxsat)) & mgu.io.out.asUInt).orR

  // util function
  def splitMask(maskIn: UInt, sew: SewOH): Vec[UInt] = {
    val maskWidth = maskIn.getWidth
    val result = Wire(Vec(maskWidth / 8, UInt(8.W)))
    for ((resultData, i) <- result.zipWithIndex) {
      resultData := Mux1H(Seq(
        sew.is8 -> maskIn(i * 8 + 7, i * 8),
        sew.is16 -> Cat(0.U((8 - 4).W), maskIn(i * 4 + 3, i * 4)),
        sew.is32 -> Cat(0.U((8 - 2).W), maskIn(i * 2 + 1, i * 2)),
        sew.is64 -> Cat(0.U((8 - 1).W), maskIn(i)),
      ))
    }
    result
  }

}