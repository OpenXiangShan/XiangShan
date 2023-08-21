package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.{VSew, ma}
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecInfo, VecPipedFuncUnit}
import yunsuan.{VfaluType, VfpuType}
import yunsuan.vector.VectorFloatAdder

class VFAlu(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "Vfalu OpType not supported")

  // params alias
  private val dataWidth = cfg.dataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  // io alias
  private val opcode  = fuOpType(4,0)
  private val resWiden  = fuOpType(5)
  private val opbWiden  = fuOpType(6)

  // modules
  private val vfalus = Seq.fill(numVecModule)(Module(new VectorFloatAdder))
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
    * [[vfalus]]'s in connection
    */
  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val resultData = Wire(Vec(numVecModule,UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule,UInt(20.W)))
  private val srcMaskRShift = Wire(UInt((4 * numVecModule).W))

  def genMaskForMerge(inmask:UInt, sew:UInt, i:Int): UInt = {
    val f64MaskNum = dataWidth / 64
    val f32MaskNum = dataWidth / 32
    val f16MaskNum = dataWidth / 16
    val f64Mask = inmask(f64MaskNum-1,0)
    val f32Mask = inmask(f32MaskNum-1,0)
    val f16Mask = inmask(f16MaskNum-1,0)
    val f64MaskI = Cat(0.U(3.W),f64Mask(i))
    val f32MaskI = Cat(0.U(2.W),f32Mask(2*i+1,2*i))
    val f16MaskI = f16Mask(4*i+3,4*i)
    val outMask = Mux1H(
      Seq(
        (sew === 3.U) -> f64MaskI,
        (sew === 2.U) -> f32MaskI,
        (sew === 1.U) -> f16MaskI,
      )
    )
    outMask
  }
  val isScalarMove = (fuOpType === VfaluType.vfmv_f_s) || (fuOpType === VfaluType.vfmv_s_f)
  srcMaskRShift := (srcMask >> (vecCtrl.vuopIdx * (16.U >> vecCtrl.vsew)))(4 * numVecModule - 1, 0)
  val fp_aIsFpCanonicalNAN = Wire(Vec(numVecModule,Bool()))
  val fp_bIsFpCanonicalNAN = Wire(Vec(numVecModule,Bool()))
  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fp_a         := Mux(opbWiden, vs1Split.io.outVec64b(i), vs2Split.io.outVec64b(i))  // very dirty TODO
      mod.io.fp_b         := Mux(opbWiden, vs2Split.io.outVec64b(i), vs1Split.io.outVec64b(i))  // very dirty TODO
      mod.io.widen_a      := Cat(vs2Split.io.outVec32b(i+numVecModule), vs2Split.io.outVec32b(i))
      mod.io.widen_b      := Cat(vs1Split.io.outVec32b(i+numVecModule), vs1Split.io.outVec32b(i))
      mod.io.frs1         := 0.U     // already vf -> vv
      mod.io.is_frs1      := false.B // already vf -> vv
      mod.io.mask         := Mux(isScalarMove, !vuopIdx.orR, genMaskForMerge(inmask = srcMaskRShift, sew = vsew, i = i))
      mod.io.uop_idx      := vuopIdx(0)
      mod.io.is_vec       := true.B // Todo
      mod.io.round_mode   := frm
      mod.io.fp_format    := Mux(resWiden, vsew + 1.U, vsew)
      mod.io.opb_widening := opbWiden
      mod.io.res_widening := resWiden
      mod.io.op_code      := opcode
      resultData(i)       := mod.io.fp_result
      fflagsData(i)       := mod.io.fflags
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
  val resultDataUInt = resultData.asUInt
  val cmpResultWidth = dataWidth / 16
  val cmpResult = Wire(Vec(cmpResultWidth, Bool()))
  for (i <- 0 until cmpResultWidth) {
    if(i == 0) {
      cmpResult(i) := resultDataUInt(0)
    }
    else if(i < dataWidth / 64) {
      cmpResult(i) := Mux1H(
        Seq(
          (outVecCtrl.vsew === 1.U) -> resultDataUInt(i*16),
          (outVecCtrl.vsew === 2.U) -> resultDataUInt(i*32),
          (outVecCtrl.vsew === 3.U) -> resultDataUInt(i*64)
        )
      )
    }
    else if(i < dataWidth / 32) {
      cmpResult(i) := Mux1H(
        Seq(
          (outVecCtrl.vsew === 1.U) -> resultDataUInt(i * 16),
          (outVecCtrl.vsew === 2.U) -> resultDataUInt(i * 32),
          (outVecCtrl.vsew === 3.U) -> false.B
        )
      )
    }
    else if(i <  dataWidth / 16) {
      cmpResult(i) := Mux(outVecCtrl.vsew === 1.U, resultDataUInt(i*16), false.B)
    }
  }

  val outEew = Mux(RegNext(resWiden), outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  val outVuopidx = outVecCtrl.vuopIdx(2, 0)
  val vlMax = ((VLEN/8).U >> outEew).asUInt
  val lmulAbs = Mux(outVecCtrl.vlmul(2), (~outVecCtrl.vlmul(1,0)).asUInt + 1.U, outVecCtrl.vlmul(1,0))
  //  vfmv_f_s need vl=1
  val outVlFix = Mux(
    outVecCtrl.fpu.isFpToVecInst || (fuOpType === VfaluType.vfmv_f_s),
    1.U,
    Mux(fuOpType === VfaluType.vfmv_s_f, outVl.orR, outVl)
  )
  val vlMaxAllUop = Wire(outVl.cloneType)
  vlMaxAllUop := Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax << lmulAbs).asUInt
  val vlMaxThisUop = Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax).asUInt
  val vlSetThisUop = Mux(outVlFix > outVuopidx*vlMaxThisUop, outVlFix - outVuopidx*vlMaxThisUop, 0.U)
  val vlThisUop = Wire(UInt(3.W))
  vlThisUop := Mux(vlSetThisUop < vlMaxThisUop, vlSetThisUop, vlMaxThisUop)
  val vlMaskRShift = Wire(UInt((4 * numVecModule).W))
  vlMaskRShift := Fill(4 * numVecModule, 1.U(1.W)) >> ((4 * numVecModule).U - vlThisUop)

  private val needNoMask = (outCtrl.fuOpType === VfaluType.vfmerge) || (outCtrl.fuOpType === VfaluType.vfmv_s_f) || outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
  val allFFlagsEn = Wire(Vec(4*numVecModule,Bool()))
  val outSrcMaskRShift = Wire(UInt((4*numVecModule).W))
  outSrcMaskRShift := (maskToMgu >> (outVecCtrl.vuopIdx(2,0) * vlMax))(4*numVecModule-1,0)
  val f16FFlagsEn = outSrcMaskRShift
  val f32FFlagsEn = Wire(Vec(numVecModule,UInt(4.W)))
  for (i <- 0 until numVecModule){
    f32FFlagsEn(i) := Cat(Fill(2, 1.U),outSrcMaskRShift(2*i+1,2*i))
  }
  val f64FFlagsEn = Wire(Vec(numVecModule, UInt(4.W)))
  for (i <- 0 until numVecModule) {
    f64FFlagsEn(i) := Cat(Fill(3, 1.U), outSrcMaskRShift(i))
  }
  val fflagsEn= Mux1H(
    Seq(
      (outEew === 1.U) -> f16FFlagsEn.asUInt,
      (outEew === 2.U) -> f32FFlagsEn.asUInt,
      (outEew === 3.U) -> f64FFlagsEn.asUInt
    )
  )
  allFFlagsEn := (fflagsEn & vlMaskRShift).asTypeOf(allFFlagsEn)

  val allFFlags = fflagsData.asTypeOf(Vec(4*numVecModule,UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map{
    case(en,fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags


  val cmpResultOldVd = Wire(UInt(cmpResultWidth.W))
  cmpResultOldVd := (outOldVd >> (outVecCtrl.vuopIdx * (16.U >> outVecCtrl.vsew)))(4*numVecModule-1,0)
  val cmpResultForMgu = Wire(Vec(cmpResultWidth, Bool()))
  for (i <- 0 until cmpResultWidth) {
    cmpResultForMgu(i) := Mux(outSrcMaskRShift(i), cmpResult(i), Mux(outVecCtrl.vma, true.B, cmpResultOldVd(i)))
  }

  mgu.io.in.vd := Mux(outVecCtrl.isDstMask, Cat(0.U((dataWidth / 16 * 15).W), cmpResultForMgu.asUInt), resultDataUInt)
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := Mux(outCtrl.fuOpType === VfaluType.vfmv_f_s, true.B , outVecCtrl.vta)
  mgu.io.in.info.ma := Mux(outCtrl.fuOpType === VfaluType.vfmv_s_f, true.B , outVecCtrl.vma)
  mgu.io.in.info.vl := outVlFix
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  val resultFpMask = Wire(UInt(VLEN.W))
  val isFclass = outVecCtrl.fpu.isFpToVecInst && (outCtrl.fuOpType === VfaluType.vfclass)
  val fpCmpFuOpType = Seq(VfaluType.vfeq, VfaluType.vflt, VfaluType.vfle)
  val isCmp = outVecCtrl.fpu.isFpToVecInst && (fpCmpFuOpType.map(_ === outCtrl.fuOpType).reduce(_|_))
  resultFpMask := Mux(isFclass || isCmp, Fill(16, 1.U(1.W)), Fill(VLEN, 1.U(1.W)))
  io.out.bits.res.data := mgu.io.out.vd & resultFpMask

}

class VFMgu(vlen:Int)(implicit p: Parameters) extends Module{
  val io = IO(new VFMguIO(vlen))

  val vd = io.in.vd
  val oldvd = io.in.oldVd
  val mask = io.in.mask
  val vsew = io.in.info.eew
  val num16bits = vlen / 16

}

class VFMguIO(vlen: Int)(implicit p: Parameters) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(vlen.W))
    val oldVd = Input(UInt(vlen.W))
    val mask = Input(UInt(vlen.W))
    val info = Input(new VecInfo)
  }
  val out = new Bundle {
    val vd = Output(UInt(vlen.W))
  }
}