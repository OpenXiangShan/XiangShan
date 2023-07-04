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
import yunsuan.vector.VectorFloatAdder
import yunsuan.vector.mac.VIMac64b

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
//  private val mgu = Module(new Mgu(dataWidth))

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

  vfalus.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fp_a         := vs2Split.io.outVec64b(i)
      mod.io.fp_b         := vs1Split.io.outVec64b(i)
      mod.io.widen_a      := Cat(vs2Split.io.outVec32b(i+numVecModule), vs2Split.io.outVec32b(i))
      mod.io.widen_b      := Cat(vs1Split.io.outVec32b(i+numVecModule), vs1Split.io.outVec32b(i))
      mod.io.frs1         := 0.U     // already vf -> vv
      mod.io.is_frs1      := false.B // already vf -> vv
      mod.io.mask         := 0.U // Todo
      mod.io.uop_idx      := vuopIdx(0)
      mod.io.is_vec       := true.B // Todo
      mod.io.round_mode   := frm
      mod.io.fp_format    := vsew
      mod.io.opb_widening := opbWiden
      mod.io.res_widening := resWiden
      mod.io.op_code      := opcode
  }
  io.out.bits.res.data := vfalus.map(_.io.fp_result).reduceRight(Cat(_,_))
  val allFFlagsEn = Wire(Vec(4*numVecModule,Bool()))
  allFFlagsEn.foreach(en => en := true.B) // Todo
  val allFFlags = vfalus.map(_.io.fflags).reduceRight(Cat(_,_)).asTypeOf(Vec(4*numVecModule,UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map{
    case(en,fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags//vfalus.map(_.io.fflags).reduceRight(_ | _).asTypeOf(Vec(4,UInt(5.W))).reduce(_ | _)
}
