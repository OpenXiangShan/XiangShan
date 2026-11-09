package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import org.chipsalliance.cde.config.Parameters
import utility.{HostOpSpec, SimulationModels, XSError}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.{DstMgu, Mgtu, Mgu, NewMgu, VecPipedFuncUnit}
import xiangshan.backend.fu.vector.Utils._
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import yunsuan.VialuFixType
import yunsuan.vector.SewOH
import yunsuan.vector.VectorALU.{VIAluFixComputeCarrier, VIAluFixPoint, VIAluFixPointInput, VIAluFixPointOutput}
import yunsuan.encoding.Opcode.VIAluOpcode

import math.pow

class VIAluFix(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VialuFixType.dummy, "VialuF OpType not supported")

  // params
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val valid = io.in.valid

  // modules
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val laneModel = SimulationModels.enabled("vec-int-alu-lane")
  private val wholeModel = SimulationModels.enabled("vec-int-alu-whole")
  require(!(laneModel && wholeModel), "vector ALU lane and whole models are mutually exclusive")
  require(!wholeModel, "vec-int-alu-whole requires a complete whole-vector carrier and is not enabled yet")
  private val behaviorModel = laneModel || wholeModel
  private val behaviorModelName = if (laneModel) "vec-int-alu-lane" else "vec-int-alu-whole"
  private val computeGrouped = SimulationModels.computeGrouped(behaviorModelName)
  private val pointInputs = Seq.fill(numVecModule)(Wire(new VIAluFixPointInput(XLEN)))
  private val pointOuts = Seq.fill(numVecModule)(Wire(new VIAluFixPointOutput(XLEN)))
  private val pointModules = if (!behaviorModel) Seq.fill(numVecModule)(Module(new VIAluFixPoint(XLEN))) else Seq.empty
  private val pointCarriers = if (behaviorModel) Seq.fill(numVecModule)(Module(new VIAluFixComputeCarrier(behaviorModelName))) else Seq.empty
  pointInputs.zip(pointOuts).zipWithIndex.foreach { case ((input, output), i) =>
    if (behaviorModel) {
      pointCarriers(i).io.in := input.asUInt
      // VIAluFixPoint's original S1 register is outside the pure carrier.
      // Recreate that one-cycle boundary here while keeping the grouped node
      // stateless.
      output := RegEnable(pointCarriers(i).io.out.asTypeOf(new VIAluFixPointOutput(XLEN)), valid)
    } else {
      pointModules(i).io.in := input
      output := pointModules(i).io.out
    }
  }
  if (behaviorModel) {
    val ops = Seq(
      HostOpSpec("lane-add-sub", Seq(64, 64), 64, laneWidth = 8),
      HostOpSpec("lane-compare", Seq(64, 64), 64, laneWidth = 8),
      HostOpSpec("lane-shift-logic", Seq(64, 64), 64, laneWidth = 8),
      HostOpSpec("lane-saturating-round", Seq(64), 64, laneWidth = 8, implementation = "fallback"))
    pointCarriers.foreach { carrier =>
      SimulationModels.functionalPipeline(behaviorModelName, carrier, 0, "", Seq((8, 8), (16, 16), (32, 32), (64, 64)),
        if (computeGrouped) Some(carrier) else None,
        if (laneModel) "lane-core" else "whole-unit", ops)
    }
  }
  private val mgu = Module(new NewMgu(dataWidth))
  private val mgtu = Module(new Mgtu(dataWidth))

  private val opcode = VialuFixType.getOpcode(fuOpType).asTypeOf(new VIAluOpcode)
  private val isSigned = VialuFixType.isSigned(fuOpType)
  private val isMisc = VialuFixType.isMisc(fuOpType)
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

  pointInputs.zipWithIndex.foreach {
    case (point, i) =>
      point.ctrl.valid := valid
      point.opcode := opcode
      point.info.sel8 := sel8
      point.info.sel16 := sel16
      point.info.sel32 := sel32
      point.info.sel64 := sel64
      point.info.vm := vm
      point.ctrl.widen := widen
      point.ctrl.isSigned := isSigned
      point.ctrl.adderCtrl.widenVs2 := widenVs2
      point.ctrl.adderCtrl.isAddCarry := isAddCarry
      point.ctrl.miscCtrl.isExt.valid := isExt
      point.ctrl.miscCtrl.isExt.bits.isVf2 := isVf2
      point.ctrl.miscCtrl.isExt.bits.isVf4 := isVf4
      point.ctrl.miscCtrl.isExt.bits.isVf8 := isVf8
      point.ctrl.miscCtrl.isMisc := isMisc
      point.ctrl.miscCtrl.isNarrow := isNarrow
      point.data.vs2 := vs2Vec(i)
      point.data.vs1 := vs1Vec(i)
      point.data.vs2Widen := vs2WidenVec(i)
      point.data.vs1Widen := vs1WidenVec(i)
      point.data.mask := maskVec(i)
      point.data.vxrm := vxrm
  }

  private val maskToMgu = Mux(isAddCarry, allMaskTrue, srcMask)
  private val vdWiden = (VialuFixType.fmtIsVVW(fuOpType) || VialuFixType.fmtIsWVW(fuOpType)) & !isExt & !isDstMask
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

  private val vd = Cat(pointOuts.map(_.vd).reverse)
  private val outNarrow = outVecCtrl.isNarrow
  private val outVuopIdx0 = outVecCtrl.vuopIdx(0).asBool
  private val narrowVd = Cat(pointOuts.map(_.narrowVd).reverse)
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

  private val addCarryCmpMask = Mux1H(outVsewOH, Seq(8, 4, 2, 1).map(i =>
    Cat(pointOuts.map(_.addCarryCmpMask(i - 1, 0)).reverse)
  ))

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
    (outNarrow & outVuopIdx0) -> Cat(Cat(pointOuts.map(_.vxsat(3, 0)).reverse), 0.U(8.W)),
    (outNarrow & !outVuopIdx0) -> Cat(pointOuts.map(_.vxsat(3, 0)).reverse),
    !outNarrow -> Cat(pointOuts.map(_.vxsat).reverse),
  ))

  mgtu.io.in.vd := Mux(outOpMask, vd, dstMgu.io.out.vd)
  mgtu.io.in.vl := outVl

  io.out.bits.res.data := Mux(vlIsZeroS1, outOldVd, Mux(outDstMask, mgtu.io.out.vd, resVecByte.asUInt))
  io.out.bits.res.vxsat.get := Mux(vlIsZeroS1, false.B, (outVxsat & activeEnS1).orR)
}
