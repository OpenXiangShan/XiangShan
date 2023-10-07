package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecNonPipedFuncUnit}
import xiangshan.backend.rob.RobPtr
import yunsuan.VfpuType
import yunsuan.vector.VectorFloatDivider

class VFDivSqrt(cfg: FuConfig)(implicit p: Parameters) extends VecNonPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfdiv OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode  = fuOpType(0)

  // modules
  private val vfdivs = Seq.fill(numVecModule)(Module(new VectorFloatDivider))
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
    * [[vfdivs]]'s in connection
    */
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule, UInt(20.W)))
  val fp_aIsFpCanonicalNAN = Wire(Vec(numVecModule, Bool()))
  val fp_bIsFpCanonicalNAN = Wire(Vec(numVecModule, Bool()))

  val thisRobIdx = Wire(new RobPtr)
  when(io.in.ready){
    thisRobIdx := io.in.bits.ctrl.robIdx
  }.otherwise{
    thisRobIdx := outCtrl.robIdx
  }
  vfdivs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.start_valid_i  := io.in.valid
      mod.io.finish_ready_i := io.out.ready & io.out.valid
      mod.io.flush_i        := thisRobIdx.needFlush(io.flush)
      mod.io.fp_format_i    := vsew
      mod.io.opa_i          := vs2Split.io.outVec64b(i)
      mod.io.opb_i          := vs1Split.io.outVec64b(i)
      mod.io.frs2_i         := 0.U     // already vf -> vv
      mod.io.frs1_i         := 0.U     // already vf -> vv
      mod.io.is_frs2_i      := false.B // already vf -> vv
      mod.io.is_frs1_i      := false.B // already vf -> vv
      mod.io.is_sqrt_i      := opcode
      mod.io.rm_i           := rmValue
      mod.io.is_vec_i       := true.B // Todo
      resultData(i) := mod.io.fpdiv_res_o
      fflagsData(i) := mod.io.fflags_o
      fp_aIsFpCanonicalNAN(i) := vecCtrl.fpu.isFpToVecInst & (
        ((vsew === VSew.e32) & (!vs2Split.io.outVec64b(i).head(32).andR)) |
          ((vsew === VSew.e16) & (!vs2Split.io.outVec64b(i).head(48).andR))
        )
      fp_bIsFpCanonicalNAN(i) := vecCtrl.fpu.isFpToVecInst & (
        ((vsew === VSew.e32) & (!vs1Split.io.outVec64b(i).head(32).andR)) |
          ((vsew === VSew.e16) & (!vs1Split.io.outVec64b(i).head(48).andR))
        )
      mod.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN(i)
      mod.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN(i)
  }

  io.in.ready  := vfdivs.map(_.io.start_ready_o).reduce(_&_)
  io.out.valid := vfdivs.map(_.io.finish_valid_o).reduce(_&_)
  val outEew = outVecCtrl.vsew
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
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
  mgu.io.in.info.eew := outVecCtrl.vsew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  io.out.bits.res.data := mgu.io.out.vd

}
