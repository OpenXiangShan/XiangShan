package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew, ma}
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Mgtu, VecInfo, VecPipedFuncUnit}
import xiangshan.ExceptionNO
import yunsuan.{VfaluType, VfpuType}
import yunsuan.vector.VectorFloatAdder
import xiangshan.backend.fu.vector.Bundles.VConfig

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
  private val mgtu = Module(new Mgtu(dataWidth))

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
  private val srcMaskRShiftForReduction = Wire(UInt((8 * numVecModule).W))
  // for reduction
  val isFirstGroupUop = vuopIdx === 0.U ||
    (vuopIdx === 1.U && (vlmul === VLmul.m4 || vlmul === VLmul.m8)) ||
    ((vuopIdx === 2.U || vuopIdx === 3.U) && vlmul === VLmul.m8)
  val maskRshiftWidthForReduction = Wire(UInt(6.W))
  maskRshiftWidthForReduction := Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
    vuopIdx,
    Mux1H(Seq(
      (vsew === VSew.e16) -> (vuopIdx(1, 0) << 4),
      (vsew === VSew.e32) -> (vuopIdx(1, 0) << 3),
      (vsew === VSew.e64) -> (vuopIdx(1, 0) << 2),
    ))
  )
  val vlMaskForReduction = (~(Fill(VLEN, 1.U) << vl)).asUInt
  srcMaskRShiftForReduction := ((srcMask & vlMaskForReduction) >> maskRshiftWidthForReduction)(8 * numVecModule - 1, 0)

  def genMaskForReduction(inmask: UInt, sew: UInt, i: Int): UInt = {
    val f64MaskNum = dataWidth / 64 * 2
    val f32MaskNum = dataWidth / 32 * 2
    val f16MaskNum = dataWidth / 16 * 2
    val f64Mask = inmask(f64MaskNum - 1, 0)
    val f32Mask = inmask(f32MaskNum - 1, 0)
    val f16Mask = inmask(f16MaskNum - 1, 0)
    // vs2 reordered, so mask use high bits
    val f64FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(3.W), f64Mask(0), 0.U(3.W), f64Mask(1)),
      )
    )
    val f64FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(3.W), f64Mask(1), 0.U(3.W), f64Mask(0))
      )
    )
    val f32FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(2.W), f32Mask(1), f32Mask(0), 0.U(2.W), f32Mask(3), f32Mask(2)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(3.W), f32Mask(0), 0.U(3.W), f32Mask(1)),
      )
    )
    val f32FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(0.U(2.W), f32Mask(3), f32Mask(2), 0.U(2.W), f32Mask(1), f32Mask(0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(3.W), f32Mask(1), 0.U(3.W), f32Mask(0)),
      )
    )
    val f16FirstFoldMaskUnorder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(f16Mask(7,4), f16Mask(3,0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(2.W), f16Mask(1), f16Mask(0), 0.U(2.W), f16Mask(3), f16Mask(2)),
        vecCtrl.fpu.isFoldTo1_8 -> Cat(0.U(3.W), f16Mask(0), 0.U(3.W), f16Mask(1)),
      )
    )
    val f16FirstFoldMaskOrder = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> Cat(f16Mask(7,4), f16Mask(3,0)),
        vecCtrl.fpu.isFoldTo1_4 -> Cat(0.U(2.W), f16Mask(3), f16Mask(2), 0.U(2.W), f16Mask(1), f16Mask(0)),
        vecCtrl.fpu.isFoldTo1_8 -> Cat(0.U(3.W), f16Mask(1), 0.U(3.W), f16Mask(0)),
      )
    )
    val f64FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b00010001".U,
      )
    )
    val f32FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b00110011".U,
        vecCtrl.fpu.isFoldTo1_4 -> "b00010001".U,
      )
    )
    val f16FoldMask = Mux1H(
      Seq(
        vecCtrl.fpu.isFoldTo1_2 -> "b11111111".U,
        vecCtrl.fpu.isFoldTo1_4 -> "b00110011".U,
        vecCtrl.fpu.isFoldTo1_8 -> "b00010001".U,
      )
    )
    // low 4 bits for vs2(fp_a), high 4 bits for vs1(fp_b),
    val isFold = vecCtrl.fpu.isFoldTo1_2 || vecCtrl.fpu.isFoldTo1_4 || vecCtrl.fpu.isFoldTo1_8
    val f64FirstNotFoldMask = Cat(0.U(3.W), f64Mask(i + 2), 0.U(3.W), f64Mask(i))
    val f32FirstNotFoldMask = Cat(0.U(2.W), f32Mask(i * 2 + 5, i * 2 + 4), 0.U(2.W), Cat(f32Mask(i * 2 + 1, i * 2)))
    val f16FirstNotFoldMask = Cat(f16Mask(i * 4 + 11, i * 4 + 8), f16Mask(i * 4 + 3, i * 4))
    val f64MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f64FirstFoldMaskOrder, f64FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f64FirstFoldMaskUnorder, f64FirstNotFoldMask),
        Mux(isFold, f64FoldMask, Fill(8, 1.U))))
    val f32MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f32FirstFoldMaskOrder, f32FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f32FirstFoldMaskUnorder, f32FirstNotFoldMask),
        Mux(isFold, f32FoldMask, Fill(8, 1.U))))
    val f16MaskI = Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum,
      Mux(isFold, f16FirstFoldMaskOrder, f16FirstNotFoldMask),
      Mux(isFirstGroupUop,
        Mux(isFold, f16FirstFoldMaskUnorder, f16FirstNotFoldMask),
        Mux(isFold, f16FoldMask, Fill(8, 1.U))))
    val outMask = Mux1H(
      Seq(
        (sew === 3.U) -> f64MaskI,
        (sew === 2.U) -> f32MaskI,
        (sew === 1.U) -> f16MaskI,
      )
    )
    Mux(fuOpType === VfaluType.vfredosum || fuOpType === VfaluType.vfwredosum, outMask(0),outMask)
  }
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
  val srcMaskRShift = Wire(UInt((4 * numVecModule).W))
  val maskRshiftWidth = Wire(UInt(6.W))
  maskRshiftWidth := Mux1H(
    Seq(
      (vsew === VSew.e16) -> (vuopIdx(2,0) << 3),
      (vsew === VSew.e32) -> (vuopIdx(2,0) << 2),
      (vsew === VSew.e64) -> (vuopIdx(2,0) << 1),
    )
  )
  srcMaskRShift := (srcMask >> maskRshiftWidth)(4 * numVecModule - 1, 0)
  val fp_aIsFpCanonicalNAN = Wire(Vec(numVecModule,Bool()))
  val fp_bIsFpCanonicalNAN = Wire(Vec(numVecModule,Bool()))
  val inIsFold = Wire(UInt(3.W))
  inIsFold := Cat(vecCtrl.fpu.isFoldTo1_8, vecCtrl.fpu.isFoldTo1_4, vecCtrl.fpu.isFoldTo1_2)
  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire             := io.in.valid
      mod.io.fp_a             := vs2Split.io.outVec64b(i)
      mod.io.fp_b             := vs1Split.io.outVec64b(i)
      mod.io.widen_a          := Cat(vs2Split.io.outVec32b(i+numVecModule), vs2Split.io.outVec32b(i))
      mod.io.widen_b          := Cat(vs1Split.io.outVec32b(i+numVecModule), vs1Split.io.outVec32b(i))
      mod.io.frs1             := 0.U     // already vf -> vv
      mod.io.is_frs1          := false.B // already vf -> vv
      mod.io.mask             := Mux(isScalarMove, !vuopIdx.orR, genMaskForMerge(inmask = srcMaskRShift, sew = vsew, i = i))
      mod.io.maskForReduction := genMaskForReduction(inmask = srcMaskRShiftForReduction, sew = vsew, i = i)
      mod.io.uop_idx          := vuopIdx(0)
      mod.io.is_vec           := true.B // Todo
      mod.io.round_mode       := rm
      mod.io.fp_format        := Mux(resWiden, vsew + 1.U, vsew)
      mod.io.opb_widening     := opbWiden
      mod.io.res_widening     := resWiden
      mod.io.op_code          := opcode
      mod.io.is_vfwredosum    := fuOpType === VfaluType.vfwredosum
      mod.io.is_fold          := inIsFold
      mod.io.vs2_fold         := vs2      // for better timing
      resultData(i)           := mod.io.fp_result
      fflagsData(i)           := mod.io.fflags
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
  val outCtrl_s0 = ctrlVec.head
  val outVecCtrl_s0 = ctrlVec.head.vpu.get
  val outEew_s0 = Mux(resWiden, outVecCtrl_s0.vsew + 1.U, outVecCtrl_s0.vsew)
  val outEew = Mux(RegEnable(resWiden, io.in.fire), outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  val outVuopidx = outVecCtrl.vuopIdx(2, 0)
  val vlMax_s0 = ((VLEN/8).U >> outEew_s0).asUInt
  val vlMax = ((VLEN/8).U >> outEew).asUInt
  val lmulAbs = Mux(outVecCtrl.vlmul(2), (~outVecCtrl.vlmul(1,0)).asUInt + 1.U, outVecCtrl.vlmul(1,0))
  //  vfmv_f_s need vl=1, reduction last uop need vl=1, other uop need vl=vlmax
  val numOfUopVFRED = {
    // addTime include add frs1
    val addTime = MuxLookup(outVecCtrl_s0.vlmul, 1.U(4.W))(Array(
      VLmul.m2 -> 2.U,
      VLmul.m4 -> 4.U,
      VLmul.m8 -> 8.U,
    ))
    val foldLastVlmul = MuxLookup(outVecCtrl_s0.vsew, "b000".U)(Array(
      VSew.e16 -> VLmul.mf8,
      VSew.e32 -> VLmul.mf4,
      VSew.e64 -> VLmul.mf2,
    ))
    // lmul < 1, foldTime = vlmul - foldFastVlmul
    // lmul >= 1, foldTime = 0.U - foldFastVlmul
    val foldTime = Mux(outVecCtrl_s0.vlmul(2), outVecCtrl_s0.vlmul, 0.U) - foldLastVlmul
    addTime + foldTime
  }
  val reductionVl = Mux((outVecCtrl_s0.vuopIdx ===  numOfUopVFRED - 1.U) || (outCtrl_s0.fuOpType === VfaluType.vfredosum || outCtrl_s0.fuOpType === VfaluType.vfwredosum), 1.U, vlMax_s0)
  val outIsResuction = outCtrl.fuOpType === VfaluType.vfredusum ||
    outCtrl.fuOpType === VfaluType.vfredmax ||
    outCtrl.fuOpType === VfaluType.vfredmin ||
    outCtrl.fuOpType === VfaluType.vfredosum ||
    outCtrl.fuOpType === VfaluType.vfwredosum
  val outIsResuction_s0 = outCtrl_s0.fuOpType === VfaluType.vfredusum ||
    outCtrl_s0.fuOpType === VfaluType.vfredmax ||
    outCtrl_s0.fuOpType === VfaluType.vfredmin ||
    outCtrl_s0.fuOpType === VfaluType.vfredosum ||
    outCtrl_s0.fuOpType === VfaluType.vfwredosum
  val outVConfig_s0  = if(!cfg.vconfigWakeUp) outVecCtrl_s0.vconfig else dataVec.head.getSrcVConfig.asTypeOf(new VConfig)
  val outVl_s0       = outVConfig_s0.vl
  val outVlFix_s0 = Mux(
    outVecCtrl_s0.fpu.isFpToVecInst || (outCtrl_s0.fuOpType === VfaluType.vfmv_f_s),
    1.U,
    Mux(
      outCtrl_s0.fuOpType === VfaluType.vfmv_s_f,
      outVl_s0.orR,
      Mux(outIsResuction_s0, reductionVl, outVl_s0)
    )
  )
  val outVlFix = RegEnable(outVlFix_s0,io.in.fire)

  val vlMaxAllUop = Wire(outVl.cloneType)
  vlMaxAllUop := Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax << lmulAbs).asUInt
  val vlMaxThisUop = Mux(outVecCtrl.vlmul(2), vlMax >> lmulAbs, vlMax).asUInt
  val vlSetThisUop = Mux(outVlFix > outVuopidx*vlMaxThisUop, outVlFix - outVuopidx*vlMaxThisUop, 0.U)
  val vlThisUop = Wire(UInt(3.W))
  vlThisUop := Mux(vlSetThisUop < vlMaxThisUop, vlSetThisUop, vlMaxThisUop)
  val vlMaskRShift = Wire(UInt((4 * numVecModule).W))
  vlMaskRShift := Fill(4 * numVecModule, 1.U(1.W)) >> ((4 * numVecModule).U - vlThisUop)

  private val needNoMask = outCtrl.fuOpType === VfaluType.vfmerge ||
    outCtrl.fuOpType === VfaluType.vfmv_s_f ||
    outIsResuction ||
    outVecCtrl.fpu.isFpToVecInst
  val maskToMgu = Mux(needNoMask, allMaskTrue, outSrcMask)
  val allFFlagsEn = Wire(Vec(4*numVecModule,Bool()))
  val outSrcMaskRShift = Wire(UInt((4*numVecModule).W))
  outSrcMaskRShift := (maskToMgu >> (outVecCtrl.vuopIdx(2,0) * vlMax))(4*numVecModule-1,0)
  val f16FFlagsEn = outSrcMaskRShift
  val f32FFlagsEn = Wire(Vec(numVecModule,UInt(4.W)))
  val f64FFlagsEn = Wire(Vec(numVecModule, UInt(4.W)))
  val f16VlMaskEn = vlMaskRShift
  val f32VlMaskEn = Wire(Vec(numVecModule, UInt(4.W)))
  val f64VlMaskEn = Wire(Vec(numVecModule, UInt(4.W)))
  for (i <- 0 until numVecModule){
    f32FFlagsEn(i) := Cat(Fill(2, 0.U), outSrcMaskRShift(2*i+1,2*i))
    f64FFlagsEn(i) := Cat(Fill(3, 0.U), outSrcMaskRShift(i))
    f32VlMaskEn(i) := Cat(Fill(2, 0.U), vlMaskRShift(2 * i + 1, 2 * i))
    f64VlMaskEn(i) := Cat(Fill(3, 0.U), vlMaskRShift(i))
  }
  val fflagsEn= Mux1H(
    Seq(
      (outEew === 1.U) -> f16FFlagsEn.asUInt,
      (outEew === 2.U) -> f32FFlagsEn.asUInt,
      (outEew === 3.U) -> f64FFlagsEn.asUInt
    )
  )
  val vlMaskEn = Mux1H(
    Seq(
      (outEew === 1.U) -> f16VlMaskEn.asUInt,
      (outEew === 2.U) -> f32VlMaskEn.asUInt,
      (outEew === 3.U) -> f64VlMaskEn.asUInt
    )
  )
  allFFlagsEn := Mux(outIsResuction, Fill(4*numVecModule, 1.U), (fflagsEn & vlMaskEn)).asTypeOf(allFFlagsEn)

  val allFFlags = fflagsData.asTypeOf(Vec(4*numVecModule,UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map{
    case(en,fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags


  val cmpResultOldVd = Wire(UInt(cmpResultWidth.W))
  val cmpResultOldVdRshiftWidth = Wire(UInt(6.W))
  cmpResultOldVdRshiftWidth := Mux1H(
    Seq(
      (outVecCtrl.vsew === VSew.e16) -> (outVecCtrl.vuopIdx(2, 0) << 3),
      (outVecCtrl.vsew === VSew.e32) -> (outVecCtrl.vuopIdx(2, 0) << 2),
      (outVecCtrl.vsew === VSew.e64) -> (outVecCtrl.vuopIdx(2, 0) << 1),
    )
  )
  cmpResultOldVd := (outOldVd >> cmpResultOldVdRshiftWidth)(4*numVecModule-1,0)
  val cmpResultForMgu = Wire(Vec(cmpResultWidth, Bool()))
  private val maxVdIdx = 8
  private val elementsInOneUop = Mux1H(
    Seq(
      (outEew === 1.U) -> (cmpResultWidth).U(4.W),
      (outEew === 2.U) -> (cmpResultWidth / 2).U(4.W),
      (outEew === 3.U) -> (cmpResultWidth / 4).U(4.W),
    )
  )
  private val vdIdx = outVecCtrl.vuopIdx(2, 0)
  private val elementsComputed = Mux1H(Seq.tabulate(maxVdIdx)(i => (vdIdx === i.U) -> (elementsInOneUop * i.U)))
  for (i <- 0 until cmpResultWidth) {
    val cmpResultWithVmask = Mux(outSrcMaskRShift(i), cmpResult(i), Mux(outVecCtrl.vma, true.B, cmpResultOldVd(i)))
    cmpResultForMgu(i) := Mux(elementsComputed +& i.U >= outVl, true.B, cmpResultWithVmask)
  }
  val outIsFold = outVecCtrl.fpu.isFoldTo1_2 || outVecCtrl.fpu.isFoldTo1_4 || outVecCtrl.fpu.isFoldTo1_8
  val outOldVdForREDO = Mux1H(Seq(
    (outVecCtrl.vsew === VSew.e16) -> (outOldVd >> 16),
    (outVecCtrl.vsew === VSew.e32) -> (outOldVd >> 32),
    (outVecCtrl.vsew === VSew.e64) -> (outOldVd >> 64),
  ))
  val outOldVdForWREDO = Mux(
    !outIsFold,
    Mux(outVecCtrl.vsew === VSew.e16, Cat(outOldVd(VLEN-1-16,16), 0.U(32.W)), Cat(outOldVd(VLEN-1-32,32), 0.U(64.W))),
    Mux(outVecCtrl.vsew === VSew.e16,
      // Divide vuopIdx by 8 and the remainder is 1
      Mux(outVecCtrl.vuopIdx(2,0) === 1.U, outOldVd, outOldVd >> 16),
      // Divide vuopIdx by 4 and the remainder is 1
      Mux(outVecCtrl.vuopIdx(1,0) === 1.U, outOldVd, outOldVd >> 32)
    ),
  )
  val outOldVdForRED = Mux(outCtrl.fuOpType === VfaluType.vfredosum, outOldVdForREDO, outOldVdForWREDO)
  val numOfUopVFREDOSUM = {
    val uvlMax = MuxLookup(outVecCtrl.vsew, 0.U)(Array(
      VSew.e16 -> 8.U,
      VSew.e32 -> 4.U,
      VSew.e64 -> 2.U,
    ))
    val vlMax = Mux(outVecCtrl.vlmul(2), uvlMax >> (-outVecCtrl.vlmul)(1, 0), uvlMax << outVecCtrl.vlmul(1, 0)).asUInt
    vlMax
  }
  val isOutOldVdForREDO = (outCtrl.fuOpType === VfaluType.vfredosum && outIsFold) || outCtrl.fuOpType === VfaluType.vfwredosum
  val taIsFalseForVFREDO = ((outCtrl.fuOpType === VfaluType.vfredosum) || (outCtrl.fuOpType === VfaluType.vfwredosum)) && (outVecCtrl.vuopIdx =/= numOfUopVFREDOSUM - 1.U)
  // outVecCtrl.fpu.isFpToVecInst means the instruction is float instruction, not vector float instruction
  val notUseVl = outVecCtrl.fpu.isFpToVecInst || (outCtrl.fuOpType === VfaluType.vfmv_f_s)
  val notModifyVd = !notUseVl && (outVl === 0.U)
  mgu.io.in.vd := Mux(outVecCtrl.isDstMask, Cat(0.U((dataWidth / 16 * 15).W), cmpResultForMgu.asUInt), resultDataUInt)
  mgu.io.in.oldVd := Mux(isOutOldVdForREDO, outOldVdForRED, outOldVd)
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := Mux(outCtrl.fuOpType === VfaluType.vfmv_f_s, true.B , Mux(taIsFalseForVFREDO, false.B, outVecCtrl.vta))
  mgu.io.in.info.ma := Mux(outCtrl.fuOpType === VfaluType.vfmv_s_f, true.B , outVecCtrl.vma)
  mgu.io.in.info.vl := outVlFix
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := Mux(notModifyVd, false.B, io.in.valid)
  mgu.io.in.info.vstart := Mux(outVecCtrl.fpu.isFpToVecInst, 0.U, outVecCtrl.vstart)
  mgu.io.in.info.eew :=  RegEnable(outEew_s0,io.in.fire)
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := RegEnable(Mux(outIsResuction_s0, 0.U, outVecCtrl_s0.vuopIdx), io.in.fire)
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  mgu.io.in.isIndexedVls := false.B
  mgtu.io.in.vd := Mux(outVecCtrl.isDstMask, mgu.io.out.vd, resultDataUInt)
  mgtu.io.in.vl := outVl
  val resultFpMask = Wire(UInt(VLEN.W))
  val isFclass = outVecCtrl.fpu.isFpToVecInst && (outCtrl.fuOpType === VfaluType.vfclass)
  val fpCmpFuOpType = Seq(VfaluType.vfeq, VfaluType.vflt, VfaluType.vfle)
  val isCmp = outVecCtrl.fpu.isFpToVecInst && (fpCmpFuOpType.map(_ === outCtrl.fuOpType).reduce(_|_))
  resultFpMask := Mux(isFclass || isCmp, Fill(16, 1.U(1.W)), Fill(VLEN, 1.U(1.W)))
  // when dest is mask, the result need to be masked by mgtu
  io.out.bits.res.data := Mux(notModifyVd, outOldVd, Mux(outVecCtrl.isDstMask, mgtu.io.out.vd, mgu.io.out.vd) & resultFpMask)
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal

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