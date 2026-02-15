package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{DstMgu, Mgtu, Mgu, NewMgu, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils._
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import yunsuan.VialuFixType
import yunsuan.vector.Common.SewOH
import yunsuan.vector.VectorALU.VIAlu
import yunsuan.vector.VectorALU.{Adder, Misc}

import math.pow

class VIAluFix(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
//  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VialuFixType.dummy, "VialuF OpType not supported")

  // params
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val valid = io.in.valid

  // modules
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vIAluFixPoints = Seq.fill(numVecModule)(Module(new VIAlu))
  private val mgu = Module(new NewMgu(dataWidth))
  private val mgtu = Module(new Mgtu(dataWidth))

  private val opcode = fuOpType
  private val isSigned = VIAluOpcodes.isSigned(fuOpType)
  private val isMisc = VIAluOpcodes.isMisc(fuOpType)
  private val widenVs2 = inCtrl.vialuCtrl.get.widenVs2
  private val widen = inCtrl.vialuCtrl.get.widen
  private val isVf2 = inCtrl.vialuCtrl.get.isVf2
  private val isVf4 = inCtrl.vialuCtrl.get.isVf4
  private val isVf8 = inCtrl.vialuCtrl.get.isVf8
  private val isAddCarry = inCtrl.vialuCtrl.get.isAddCarry

  // need reg
  private val sel8  = sew8
  private val sel16 = sew16
  private val sel32 = sew32
  private val sel64 = sew64

  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1

  private val vs2Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs1Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs2WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  private val vs1WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))

  // mask
  private val maskVec = Wire(Vec(2, UInt(8.W)))
  maskVec := maskVecGen.asTypeOf(maskVec)

  for (i <- 0 until 2) {
    vs2Vec(i) := vs2Split.io.outVec64b(i)
    vs1Vec(i) := vs1Split.io.outVec64b(i)
    vs2WidenVec(i) := Mux(vuopIdx(0), vs2Split.io.outVec32b(i + 2), vs2Split.io.outVec32b(i))
    vs1WidenVec(i) := Mux(vuopIdx(0), vs1Split.io.outVec32b(i + 2), vs1Split.io.outVec32b(i))
  }

  vIAluFixPoints.zipWithIndex.foreach {
    case (mod, i) =>
      mod.in.vxrm := vxrm
      mod.in.ex0.valid := valid
      mod.in.ex0.bits.ctrl.opcode := opcode
      mod.in.ex0.bits.ctrl.adder match { case adderCtrl =>
        adderCtrl.sel8       := sel8
        adderCtrl.sel16      := sel16
        adderCtrl.sel32      := sel32
        adderCtrl.sel64      := sel64
        adderCtrl.widenVs2   := widenVs2
        adderCtrl.widen      := widen
        adderCtrl.isSigned   := isSigned
        adderCtrl.isAddCarry := isAddCarry
        adderCtrl.isSub      := false.B
        adderCtrl.isCmpEq    := false.B
        adderCtrl.isCmpNe    := false.B
        adderCtrl.isCmpLt    := false.B
        adderCtrl.isCmpLe    := false.B
        adderCtrl.isCmpGt    := false.B
        adderCtrl.isVmsbc    := false.B
        adderCtrl.isAvg      := false.B
        adderCtrl.vm         := vm
      }
      mod.in.ex0.bits.ctrl.misc match { case miscCtrl =>
        miscCtrl.sel8             := sel8
        miscCtrl.sel16            := sel16
        miscCtrl.sel32            := sel32
        miscCtrl.sel64            := sel64
        miscCtrl.isVf2            := isVf2
        miscCtrl.isVf4            := isVf4
        miscCtrl.isVf8            := isVf8
        miscCtrl.widenVs2         := widenVs2
        miscCtrl.widen            := widen
        miscCtrl.isNarrow         := isNarrow
        miscCtrl.isSigned         := isSigned
        miscCtrl.isLeftShiftLogic := false.B
        miscCtrl.isCtz            := false.B
      }
      mod.in.ex0.bits.data.vs2 := vs2Vec(i)
      mod.in.ex0.bits.data.vs1 := vs1Vec(i)
      mod.in.ex0.bits.data.vs2Widen := vs2WidenVec(i)
      mod.in.ex0.bits.data.vs1Widen := vs1WidenVec(i)
      mod.in.ex0.bits.data.mask := maskVec(i)
      mod.in.ex1.valid := valid
      mod.in.ex1.bits.ctrl.opcode := opcode
      mod.in.ex1.bits.ctrl.fixPoint match { case fpCtrl =>
        fpCtrl.sel8       := sel8
        fpCtrl.sel16      := sel16
        fpCtrl.sel32      := sel32
        fpCtrl.sel64      := sel64
        fpCtrl.isSigned   := isSigned
        fpCtrl.isSat      := false.B
        fpCtrl.isMaxMin   := false.B
        fpCtrl.isMax      := false.B
        fpCtrl.isNClip    := false.B
      }
      mod.in.ex1.bits.data.vs2 := vs2Vec(i)
      mod.in.ex1.bits.data.vs1 := vs1Vec(i)
  }

  private val maskToMgu = Mux(isAddCarry, allMaskTrue, srcMask)
  private val vdWiden = VIAluOpcodes.isWiden(fuOpType)
  private val eew = Mux(vdWiden, vsew + 1.U, vsew)
  private val vdIdx = Mux(isNarrow, vuopIdx(2, 1), vuopIdx)

  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := vta
  mgu.io.in.info.ma := vma
  mgu.io.in.info.vstart := 0.U
  mgu.io.in.info.vl := vl
  mgu.io.in.info.eew := eew
  mgu.io.in.info.vsew := vsew
  mgu.io.in.info.vdIdx := vdIdx
  mgu.io.in.isIndexedVls := false.B

  private val maxUopIdx = VLEN / 8
  private val numBytes = maxUopIdx

  private val activeEn = Wire(UInt(numBytes.W))
  private val agnosticEn = Wire(UInt(numBytes.W))
  activeEn := mgu.io.out.activeEn
  agnosticEn := mgu.io.out.agnosticEn

  private val byte1s: UInt = (~0.U(8.W)).asUInt
  private val agnosticVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val oldVdVecByte = oldVd.asTypeOf(agnosticVecByte)
  for (i <- 0 until numBytes) {
    agnosticVecByte(i) := Mux(agnosticEn(i), byte1s, oldVdVecByte(i))
  }

  private val activeEnS1 = RegEnable(activeEn, valid)
  private val agnosticVecByteS1 = RegEnable(agnosticVecByte, valid)

  private val vlIsZero = !vl.orR
  private val vlIsZeroS1 = RegEnable(vlIsZero, valid)

  private val vd = Cat(vIAluFixPoints.map(_.out.ex1.vd).reverse)
  private val outNarrow = outVecCtrl.isNarrow
  private val outVuopIdx0 = outVecCtrl.vuopIdx(0).asBool
  private val narrowVd = Cat(vIAluFixPoints.map(_.out.ex1.narrowVd).reverse)
  private val outNarrowVd = Mux(outVuopIdx0,
    Cat(narrowVd, outOldVd(dataWidth / 2 - 1, 0)),
    Cat(outOldVd(dataWidth - 1, dataWidth / 2), narrowVd))

  private val outVd = Mux(outNarrow, outNarrowVd, vd)

  private val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVecByte = outVd.asTypeOf(resVecByte)

  for (i <- 0 until numBytes) {
    resVecByte(i) := Mux(activeEnS1(i), vdVecByte(i), agnosticVecByteS1(i))
  }

  private val vsewOH = SewOH(vsew).oneHot
  private val outVsewOH = RegEnable(vsewOH, valid)

  // FIXME: addCarryCmpMask not exposed by VIAlu — stubbed to 0
  private val addCarryCmpMask = 0.U(dataWidth.W)

  private val dstMgu = Module(new DstMgu(dataWidth))
  dstMgu.io.in.valid := valid
  dstMgu.io.in.oldVd := oldVd
  dstMgu.io.in.mask := maskToMgu
  dstMgu.io.in.ma := vma
  dstMgu.io.in.eew := vsew
  dstMgu.io.in.vdIdx := vuopIdx(2, 0)
  dstMgu.io.in.toS1.vd := addCarryCmpMask
  dstMgu.io.in.toS1.oldVdS1 := outOldVd
  dstMgu.io.in.toS1.eewS1 := outVecCtrl.vsew
  dstMgu.io.in.toS1.vdIdxS1 := outVecCtrl.vuopIdx(2, 0)

  private val outDstMask = outVecCtrl.isDstMask
  private val outOpMask = outVecCtrl.isOpMask

  private val outVxsat = Mux1H(Seq(
    (outNarrow & outVuopIdx0) -> Cat(Cat(vIAluFixPoints.map(_.out.ex1.vxsat(3, 0)).reverse), 0.U(8.W)),
    (outNarrow & !outVuopIdx0) -> Cat(vIAluFixPoints.map(_.out.ex1.vxsat(3, 0)).reverse),
    !outNarrow -> Cat(vIAluFixPoints.map(_.out.ex1.vxsat).reverse),
  ))

  mgtu.io.in.vd := Mux(outOpMask, vd, dstMgu.io.out.vd)
  mgtu.io.in.vl := outVl

  io.out.bits.res.data := Mux(vlIsZeroS1, outOldVd, Mux(outDstMask, mgtu.io.out.vd, resVecByte.asUInt))
  io.out.bits.res.vxsat.get := Mux(vlIsZeroS1, false.B, (outVxsat & activeEnS1).orR)
}
