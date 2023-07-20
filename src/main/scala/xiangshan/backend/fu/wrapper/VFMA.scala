package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import yunsuan.VfpuType
import yunsuan.encoding.Opcode.VimacOpcode
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.{OpType, VimacType}
import yunsuan.vector.VectorFloatFMA
import yunsuan.vector.mac.VIMac64b

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
  }

  val resultDataUInt = resultData.asUInt
  private val outEew = Mux(RegNext(resWiden), outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  mgu.io.in.vd := resultDataUInt
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outEew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  io.out.bits.res.data := mgu.io.out.vd

  val allFFlagsEn = Wire(Vec(4 * numVecModule, Bool()))
  val outSrcMaskRShift = Wire(UInt((4 * numVecModule).W))
  outSrcMaskRShift := (outSrcMask >> (outVecCtrl.vuopIdx * (16.U >> outVecCtrl.vsew)))(4 * numVecModule - 1, 0)
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
      (outVecCtrl.vsew === 1.U) -> f16FFlagsEn.asUInt,
      (outVecCtrl.vsew === 2.U) -> f32FFlagsEn.asUInt,
      (outVecCtrl.vsew === 3.U) -> f64FFlagsEn.asUInt
    )
  )
  allFFlagsEn := fflagsEn.asTypeOf(allFFlagsEn)

  val allFFlags = fflagsData.asTypeOf(Vec(4 * numVecModule, UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map {
    case (en, fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags
}
