package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.PriorityMuxDefault
import xiangshan._
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}
import xiangshan.backend.fu.VsetModule
import xiangshan.backend.fu.vector.Bundles.{VType, VsetVType}


class VTypeGen(implicit p: Parameters) extends XSModule {
  import VTypeGen._

  val in = IO(Input(new In))
  val out = IO(Output(new Out))

  private val vtypeSpec = RegInit(VType.initVtype())

  private val vtypeArch = RegInit(VType.initVtype())

  private val vtypeSpecNext = WireInit(vtypeSpec)

  private val vtypeArchNext = WireInit(vtypeArch)
  /** valid signals of instructions */
  private val instValidVec = in.insts.map(_.valid)
  /** instructions code */
  private val instFieldVec = in.insts.map(_.bits.asTypeOf(new XSInstBitFields))

  private val isVsetiVec = VecInit(instFieldVec.map(fields =>
    (fields.OPCODE === "b1010111".U) && (fields.WIDTH === "b111".U) && (
      fields.ALL(31) === "b0".U ||
        fields.ALL(31, 30) === "b11".U
      )
  ).zip(instValidVec).map { case (isVset, valid) => valid && isVset})

  private val isVsetvliVec = VecInit(instFieldVec.map(fields =>
    (fields.OPCODE === "b1010111".U) &&
      (fields.WIDTH === "b111".U) &&
      (fields.ALL(31) === "b0".U)
    )
  ).zip(instValidVec).map { case (isVsetvli, valid) => valid && isVsetvli}

  private val instVTypeVec = isVsetvliVec.zip(instFieldVec).map { case (isVsetvli, field) =>
    Mux(
      isVsetvli,
      field.ZIMM_VSETVLI.asTypeOf(new InstVType),
      field.ZIMM_VSETIVLI.asTypeOf(new InstVType),
    )
  }.map(instVType => VsetVType.fromInstVType(instVType))

  // generate vtype depending on instructions
  private val vsetModuleVec = Seq.fill(DecodeWidth)(Module(new VsetModule()))
  for(i <- 0 until DecodeWidth) {
    vsetModuleVec(i).io.in.avl := 0.U
    vsetModuleVec(i).io.in.vtype := instVTypeVec(i)
    vsetModuleVec(i).io.in.func := VSETOpType.uvsetvcfg_xi
  }

  private val vtypeNewVec = vsetModuleVec.map(_.io.out.vconfig.vtype)

  private val specvtype = vtypeSpec +: out.vtype
  out.specvtype := specvtype.take(out.specvtype.length)
  // select vtype for each instruction in a priority manner
  for(i <- 0 until DecodeWidth) {
    out.vtype(i) := PriorityMuxDefault((isVsetiVec zip vtypeNewVec).take(i + 1).reverse, vtypeSpec)
  }

  // assign the last vtype to vtypeSpec
  private val vtypeNew = out.vtype(DecodeWidth - 1)

  vtypeSpec := vtypeSpecNext
  vtypeArch := vtypeArchNext

  when(in.commitVType.hasVsetvl) {
    vtypeArchNext := in.vsetvlVType
  } .elsewhen(in.commitVType.vtype.valid) {
    vtypeArchNext := in.commitVType.vtype.bits
  }

  /**
   * Set the source of vtypeSpec from the following sources:
   * 1. committed vsetvl instruction, which flushes the pipeline.
   * 2. walk-vtype, which is used to update vtype when walking.
   * 3. walking to architectural vtype
   * 4. new vset instruction
   */
  when(in.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    vtypeSpecNext := in.vsetvlVType
  }.elsewhen(in.walkVType.valid) {
    vtypeSpecNext := in.walkVType.bits
  }.elsewhen(in.walkToArchVType) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(in.canUpdateVType) {
    vtypeSpecNext := vtypeNew
  }
}

object VTypeGen {
  class In()(implicit p: Parameters) extends XSBundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val walkToArchVType = Input(Bool())
    val walkVType   = Flipped(Valid(new VType))
    val canUpdateVType = Input(Bool())
    val vsetvlVType = Input(new VType)
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
  }

  class Out()(implicit p: Parameters) extends XSBundle {
    val vtype = Output(Vec(DecodeWidth, new VType))
    /**
     *  Speculated vtype, for snapshot, different from vtype when the instruction is vset.
     *  However, there's no need for every instruction to take a specvtype. Should be modified in the future.
     */
    val specvtype = Output(Vec(DecodeWidth, new VType))
  }
}
