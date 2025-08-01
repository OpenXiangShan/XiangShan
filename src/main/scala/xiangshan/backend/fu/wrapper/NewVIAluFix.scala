package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{Mgtu, Mgu, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils.{SplitMask, VecDataToMaskDataVec}
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import yunsuan.encoding.Opcode.VialuOpcode
import yunsuan.{OpType, VialuFixType}
import yunsuan.vector.SewOH
import yunsuan.vector.VectorALU.VIAluFixPoint

import math.pow

class NewVIAluFix(cfg: FuConfig)(implicit p:Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VialuFixType.dummy, "VialuF OpType not supported")

  // params
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val valid = io.in.valid

  // modules
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vIAluFixPoints = Seq.fill(numVecModule)(Module(new VIAluFixPoint(XLEN)))
  private val mgu = Module(new Mgu(dataWidth))
  private val mgtu = Module(new Mgtu(dataWidth))

  private val opcode = VialuFixType.getOpcode(inCtrl.fuOpType).asTypeOf(vIAluFixPoints.head.io.in.opcode)
  private val widenVs2 = VialuFixType.fmtIsVVW(inCtrl.fuOpType) && VialuFixType.isAddSub(inCtrl.fuOpType)
  private val widen = (VialuFixType.fmtIsWVW(inCtrl.fuOpType) || VialuFixType.fmtIsVVW(inCtrl.fuOpType)) &
    VialuFixType.isAddSub(inCtrl.fuOpType)
  private val isSigned = VialuFixType.isSigned(fuOpType)
  private val isVf2 = VialuFixType.fmtIsVF2(inCtrl.fuOpType) & isExt
  private val isVf4 = VialuFixType.fmtIsVF4(inCtrl.fuOpType) & isExt
  private val isVf8 = VialuFixType.fmtIsVF8(inCtrl.fuOpType) & isExt
  private val isMisc = VialuFixType.isMisc(inCtrl.fuOpType)
  private val isAddCarry = VialuFixType.isAddCarry(inCtrl.fuOpType)


  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1

  private val vs2Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs1Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))

  // mask
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(srcMask, vsew)
  private val maskVec: Vec[UInt] = SplitMask(maskDataVec(vuopIdx), vsew)

  dontTouch(maskDataVec)
  dontTouch(maskVec)

  for (i <- 0 until 2) {
    vs2Vec(i) := Mux(widenVs2 || isExt,
      Mux(vuopIdx(0), vs2Split.io.outVec32b(i + 2), vs2Split.io.outVec32b(i)), vs2Split.io.outVec64b(i))
    vs1Vec(i) := Mux(widen || isNarrow,
      Mux(vuopIdx(0), vs1Split.io.outVec32b(i + 2), vs1Split.io.outVec32b(i)), vs1Split.io.outVec64b(i))
  }

  vIAluFixPoints.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.in.valid := valid
      mod.io.in.opcode := opcode
      mod.io.in.info.vsew := vsew
      mod.io.in.info.vm := vm
      mod.io.in.vs2 := vs2Vec(i)
      mod.io.in.vs1 := vs1Vec(i)
      mod.io.in.widenVs2 := widenVs2
      mod.io.in.widen := widen
      mod.io.in.isSigned := isSigned
      mod.io.in.isExt.valid := isExt
      mod.io.in.isExt.bits.isVf2 := isVf2
      mod.io.in.isExt.bits.isVf4 := isVf4
      mod.io.in.isExt.bits.isVf8 := isVf8
      mod.io.in.isMisc := isMisc
      mod.io.in.mask := maskVec(i)
      mod.io.in.isAddCarry := isAddCarry
      mod.io.in.isNarrow := isNarrow
      mod.io.in.vxrm := vxrm
  }


  private val maxUopIdx = VLEN / 8
  private val maskVd = Wire(Vec(16, Bool()))
  private val outVsewOH = SewOH(outVecCtrl.vsew).oneHot

  private val vd = Cat(vIAluFixPoints.map(_.io.out.vd).reverse)
  private val addCarryCmpMask = Mux1H(outVsewOH, Seq(8, 4, 2, 1).map(i =>
    Cat(vIAluFixPoints.map(_.io.out.addCarryCmpMask(i - 1, 0)).reverse)
  ))

  private val addCarryCmpMaskVec = addCarryCmpMask.asTypeOf(maskVd)

  private val elementStartIdx = Mux1H(outVsewOH, Seq(4, 3, 2, 1).map(i =>
    outVecCtrl.vuopIdx(2, 0) << i.U
  )).asUInt

  for (i <- 0 until maxUopIdx) {
    when (elementStartIdx +& i.U >= outVl) {
      maskVd(i) := 1.U
    }.otherwise {
      maskVd(i) := addCarryCmpMaskVec(i)
    }
  }

  private val outDstMask = outVecCtrl.isDstMask
  private val outOpMask = outVecCtrl.isOpMask

  private val needNoMask = VialuFixType.needNoMask(outCtrl.fuOpType)
  private val maskToMgu = Mux(needNoMask, allMaskTrue, srcMask)

  private val outVdWiden = (VialuFixType.fmtIsVVW(outCtrl.fuOpType) || VialuFixType.fmtIsWVW(outCtrl.fuOpType)) &
    !outVecCtrl.isExt & !outDstMask

  private val outEew = Mux(outVdWiden, outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  private val vstartGeVl = outVecCtrl.vstart >= outVl

  private val outNarrow = outVecCtrl.isNarrow
  private val outVuopIdx = outVecCtrl.vuopIdx
  private val narrowVd = Cat(vIAluFixPoints.map(_.io.out.narrowVd).reverse)
  private val outNarrowVd = Mux(outNarrow & outVuopIdx(0).asBool,
    Cat(narrowVd, outOldVd(dataWidth / 2 - 1, 0)), Cat(outOldVd(dataWidth - 1, dataWidth / 2), narrowVd))

  private val vxsat = Mux(outNarrow,
    Cat(vIAluFixPoints.map(_.io.out.vxsat(3, 0)).reverse), Cat(vIAluFixPoints.map(_.io.out.vxsat).reverse))
  private val outVxsat = Mux(outNarrow & outVuopIdx(0).asBool, Cat(vxsat(7, 0), 0.U(8.W)), vxsat)

  mgu.io.in.vd := MuxCase(vd, Seq(
    outDstMask -> maskVd.asUInt,
    outNarrow -> outNarrowVd,
  ))
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := validVec.last
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVuopIdx
  mgu.io.in.info.narrow := outNarrow
  mgu.io.in.info.dstMask := outDstMask
  mgu.io.in.isIndexedVls := false.B


  mgtu.io.in.vd := Mux(outDstMask & outOpMask, vd, mgu.io.out.vd)
  mgtu.io.in.vl := outVl

  io.out.bits.res.data := Mux(vstartGeVl, outOldVd, Mux(outVecCtrl.isDstMask, mgtu.io.out.vd, mgu.io.out.vd))
  io.out.bits.res.vxsat.get := Mux(vstartGeVl, false.B, (outVxsat & mgu.io.out.active).orR)

  dontTouch(vxsat)
  dontTouch(outVxsat)
  dontTouch(addCarryCmpMask)
  dontTouch(addCarryCmpMaskVec)
  dontTouch(elementStartIdx)
  dontTouch(maskVd)

}
