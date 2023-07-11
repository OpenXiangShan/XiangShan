package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecInfo, VecPipedFuncUnit, VecSrcTypeModule}
import yunsuan.{VfaluType, VfpuType}
import xiangshan.backend.fu.vector.Bundles.{VSew, Vl}
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

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fp_a         := Mux(opbWiden, vs1Split.io.outVec64b(i), vs2Split.io.outVec64b(i))  // very dirty TODO
      mod.io.fp_b         := Mux(opbWiden, vs2Split.io.outVec64b(i), vs1Split.io.outVec64b(i))  // very dirty TODO
      mod.io.widen_a      := Cat(vs2Split.io.outVec32b(i+numVecModule), vs2Split.io.outVec32b(i))
      mod.io.widen_b      := Cat(vs1Split.io.outVec32b(i+numVecModule), vs1Split.io.outVec32b(i))
      mod.io.frs1         := 0.U     // already vf -> vv
      mod.io.is_frs1      := false.B // already vf -> vv
      mod.io.mask         := 0.U // Todo
      mod.io.uop_idx      := vuopIdx(0)
      mod.io.is_vec       := true.B // Todo
      mod.io.round_mode   := frm
      mod.io.fp_format    := Mux(resWiden, vsew + 1.U, vsew)
      mod.io.opb_widening := opbWiden
      mod.io.res_widening := resWiden
      mod.io.op_code      := opcode
      resultData(i)       := mod.io.fp_result
      fflagsData(i)       := mod.io.fflags
  }

  val allFFlagsEn = Wire(Vec(4*numVecModule,Bool()))
  val srcMaskRShift = Wire(UInt((4*numVecModule).W))
  srcMaskRShift := (outSrcMask >> (vuopIdx * (16.U >> vsew)))(4*numVecModule-1,0)
  val f16FFlagsEn = srcMaskRShift
  val f32FFlagsEn = Wire(Vec(numVecModule,UInt(4.W)))
  for (i <- 0 until numVecModule){
    f32FFlagsEn(i) := Cat(Fill(2, 1.U),srcMaskRShift(2*i+1,2*i))
  }
  val f64FFlagsEn = Wire(Vec(numVecModule, UInt(4.W)))
  for (i <- 0 until numVecModule) {
    f64FFlagsEn(i) := Cat(Fill(3, 1.U), srcMaskRShift(i))
  }
  val fflagsEn= Mux1H(
    Seq(
      (vsew === 1.U) -> f16FFlagsEn.asUInt,
      (vsew === 2.U) -> f32FFlagsEn.asUInt,
      (vsew === 3.U) -> f64FFlagsEn.asUInt
    )
  )
  allFFlagsEn := Mux(outVecCtrl.isDstMask, 0.U((4*numVecModule).W), fflagsEn).asTypeOf(allFFlagsEn)

  val allFFlags = fflagsData.asTypeOf(Vec(4*numVecModule,UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map{
    case(en,fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags

//  private val needNoMask = VfaluType.needNoMask(outCtrl.fuOpType)

  private val outEew = Mux(RegNext(resWiden), outVecCtrl.vsew + 1.U, outVecCtrl.vsew)
  mgu.io.in.vd := resultData.asUInt
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

//  io.out.bits.res.data := resultData.asUInt
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