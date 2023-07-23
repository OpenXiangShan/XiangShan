package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecNonPipedFuncUnit, VecPipedFuncUnit, VecSrcTypeModule}
import yunsuan.VfpuType
import yunsuan.encoding.Opcode.VimacOpcode
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.{OpType, VimacType}
import yunsuan.vector.VectorFloatDivider
import yunsuan.vector.mac.VIMac64b

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

  vfdivs.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.start_valid_i  := io.in.valid
      mod.io.finish_ready_i := io.out.ready
      mod.io.flush_i        := 0.U
      mod.io.fp_format_i    := vsew
      mod.io.opa_i          := vs2Split.io.outVec64b(i)
      mod.io.opb_i          := vs1Split.io.outVec64b(i)
      mod.io.frs2_i         := 0.U     // already vf -> vv
      mod.io.frs1_i         := 0.U     // already vf -> vv
      mod.io.is_frs2_i      := false.B // already vf -> vv
      mod.io.is_frs1_i      := false.B // already vf -> vv
      mod.io.is_sqrt_i      := opcode
      mod.io.rm_i           := frm
      mod.io.is_vec_i       := true.B // Todo
      resultData(i) := mod.io.fpdiv_res_o
      fflagsData(i) := mod.io.fflags_o

      io.in.ready  := mod.io.start_ready_o
      io.out.valid := mod.io.finish_valid_o
  }

  val resultDataUInt = resultData.asUInt
  mgu.io.in.vd := resultDataUInt
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outVecCtrl.vsew
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
