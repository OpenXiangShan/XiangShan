package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import yunsuan.VfpuType
import yunsuan.VfmaType
import yunsuan.vector.VectorFloatFMA

class VFMA(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfalu OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode  = fuOpType(3,0)
  private val resWiden  = fuOpType(4)

  // modules
  private val vfmas = Seq.fill(numVecModule)(Module(new VectorFloatFMA))
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val oldVdSplit  = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val mgu = Module(new Mgu(dataWidth))

  /**
    * In connection of [[vs2Split]], [[vs1Split]] and [[oldVdSplit]]
    */
  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1
  oldVdSplit.io.inVecData := oldVd

  /**
    * [[vfmas]]'s in connection
    */
  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule, UInt(20.W)))
  val fp_aIsFpCanonicalNAN = Wire(Vec(numVecModule, Bool()))
  val fp_bIsFpCanonicalNAN = Wire(Vec(numVecModule, Bool()))
  val fp_cIsFpCanonicalNAN = Wire(Vec(numVecModule, Bool()))
  vfmas.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fp_a         := vs2Split.io.outVec64b(i)
      mod.io.fp_b         := vs1Split.io.outVec64b(i)
      mod.io.fp_c         := oldVdSplit.io.outVec64b(i)
      mod.io.widen_a      := Cat(vs2Split.io.outVec32b(i+numVecModule), vs2Split.io.outVec32b(i))
      mod.io.widen_b      := Cat(vs1Split.io.outVec32b(i+numVecModule), vs1Split.io.outVec32b(i))
      mod.io.frs1         := 0.U     // already vf -> vv
      mod.io.is_frs1      := false.B // already vf -> vv
      mod.io.uop_idx      := vuopIdx(0)
      mod.io.is_vec       := true.B // Todo
      mod.io.round_mode   := frm
      mod.io.fp_format    := Mux(resWiden, vsew + 1.U, vsew)
      mod.io.res_widening := resWiden
      mod.io.op_code      := opcode
      resultData(i) := mod.io.fp_result
      fflagsData(i) := mod.io.fflags
      fp_aIsFpCanonicalNAN(i) := vecCtrl.fpu.isFpToVecInst & (
        ((vsew === VSew.e32) & (!vs2Split.io.outVec64b(i).head(32).andR)) |
          ((vsew === VSew.e16) & (!vs2Split.io.outVec64b(i).head(48).andR))
        )
      fp_bIsFpCanonicalNAN(i) := vecCtrl.fpu.isFpToVecInst & (
        ((vsew === VSew.e32) & (!vs1Split.io.outVec64b(i).head(32).andR)) |
          ((vsew === VSew.e16) & (!vs1Split.io.outVec64b(i).head(48).andR))
        )
      fp_cIsFpCanonicalNAN(i) := !(opcode === VfmaType.vfmul) & vecCtrl.fpu.isFpToVecInst & (
        ((vsew === VSew.e32) & (!oldVdSplit.io.outVec64b(i).head(32).andR)) |
          ((vsew === VSew.e16) & (!oldVdSplit.io.outVec64b(i).head(48).andR))
        )
      mod.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN(i)
      mod.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN(i)
      mod.io.fp_cIsFpCanonicalNAN := fp_cIsFpCanonicalNAN(i)
  }

  val outEew = Mux(RegNext(resWiden), outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  val outVuopidx = outVecCtrl.vuopIdx(2, 0)
  val vlMax = ((VLEN / 8).U >> outEew).asUInt
  val lmulAbs = Mux(outVecCtrl.vlmul(2), (~outVecCtrl.vlmul(1, 0)).asUInt + 1.U, outVecCtrl.vlmul(1, 0))
  val outVlFix = Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
  val vlMaxAllUop = Wire(outVl.cloneType)
  vlMaxAllUop := Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax << lmulAbs).asUInt
  val vlMaxThisUop = Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax).asUInt
  val vlSetThisUop = Mux(outVlFix > outVuopidx * vlMaxThisUop, outVlFix - outVuopidx * vlMaxThisUop, 0.U)
  val vlThisUop = Wire(UInt(3.W))
  vlThisUop := Mux(vlSetThisUop < vlMaxThisUop, vlSetThisUop, vlMaxThisUop)
  val vlMaskRShift = Wire(UInt((4 * numVecModule).W))
  vlMaskRShift := Fill(4 * numVecModule, 1.U(1.W)) >> ((4 * numVecModule).U - vlThisUop)

  private val needNoMask = outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
  val allFFlagsEn = Wire(Vec(4 * numVecModule, Bool()))
  val outSrcMaskRShift = Wire(UInt((4 * numVecModule).W))
  outSrcMaskRShift := (maskToMgu >> (outVecCtrl.vuopIdx(2, 0) * vlMax))(4 * numVecModule - 1, 0)
  val f16FFlagsEn = outSrcMaskRShift
  val f32FFlagsEn = Wire(Vec(numVecModule, UInt(4.W)))
  for (i <- 0 until numVecModule) {
    f32FFlagsEn(i) := Cat(Fill(2, 1.U), outSrcMaskRShift(2 * i + 1, 2 * i))
  }
  val f64FFlagsEn = Wire(Vec(numVecModule, UInt(4.W)))
  for (i <- 0 until numVecModule) {
    f64FFlagsEn(i) := Cat(Fill(3, 1.U), outSrcMaskRShift(i))
  }
  val fflagsEn = Mux1H(
    Seq(
      (outEew === 1.U) -> f16FFlagsEn.asUInt,
      (outEew === 2.U) -> f32FFlagsEn.asUInt,
      (outEew === 3.U) -> f64FFlagsEn.asUInt
    )
  )
  allFFlagsEn := (fflagsEn & vlMaskRShift).asTypeOf(allFFlagsEn)

  val allFFlags = fflagsData.asTypeOf(Vec(4 * numVecModule, UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map {
    case (en, fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags

  val resultDataUInt = resultData.asUInt
  mgu.io.in.vd := resultDataUInt
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := Mux(outVecCtrl.fpu.isFpToVecInst, 1.U, outVl)
  mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  io.out.bits.res.data := mgu.io.out.vd

}
