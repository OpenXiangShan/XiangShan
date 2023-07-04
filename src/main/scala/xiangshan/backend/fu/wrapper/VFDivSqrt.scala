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
import yunsuan.vector.VectorFloatDivider
import yunsuan.vector.mac.VIMac64b

class VFDivSqrt(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) { //here extends VecPipedFuncUnit has error
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
//  private val mgu = Module(new Mgu(dataWidth))

  /**
    * In connection of [[vs2Split]], [[vs1Split]] and [[oldVdSplit]]
    */
  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1
  oldVdSplit.io.inVecData := oldVd

  /**
    * [[vfdivs]]'s in connection
    */
  // Vec(vs2(31,0), vs2(63,32), vs2(95,64), vs2(127,96)) ==>
  // Vec(
  //   Cat(vs2(95,64),  vs2(31,0)),
  //   Cat(vs2(127,96), vs2(63,32)),
  // )
  private val vs2GroupedVec: Vec[UInt] = VecInit(vs2Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)
  private val vs1GroupedVec: Vec[UInt] = VecInit(vs1Split.io.outVec32b.zipWithIndex.groupBy(_._2 % 2).map(x => x._1 -> x._2.map(_._1)).values.map(x => Cat(x.reverse)).toSeq)

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

      io.in.ready  := mod.io.start_ready_o
      io.out.valid := mod.io.finish_valid_o
  }

  io.out.bits.res.data := vfdivs.map(_.io.fpdiv_res_o).reduceRight(Cat(_,_))
  val allFFlagsEn = Wire(Vec(4*numVecModule,Bool()))
  allFFlagsEn.foreach(en => en := true.B) // Todo
  val allFFlags = vfdivs.map(_.io.fflags_o).reduceRight(Cat(_,_)).asTypeOf(Vec(4*numVecModule,UInt(5.W)))
  val outFFlags = allFFlagsEn.zip(allFFlags).map{
    case(en,fflags) => Mux(en, fflags, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags
}
