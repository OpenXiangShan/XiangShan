package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VType, VsetVType}
import xiangshan.backend.decode.isa.bitfield.{InstVType, Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.fu.VsetModule

/**
 * IO of VTypeGen
 */
class VTypeGenIO(implicit p: Parameters) extends XSBundle {
  val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
  val walkToArchVType = Input(Bool())
  val walkVType   = Flipped(Valid(new VType))
  val canUpdateVType = Input(Bool())
  val vsetvlVType = Input(new VType)
  val commitVType = new Bundle {
    val vtype = Flipped(Valid(new VType))
    val hasVsetvl = Input(Bool())
  }
  val vtype = Output(Vec(DecodeWidth, new VType))
  /**
   *  Speculated vtype, for snapshot, different from vtype when the instruction is vset.
   *  However, there's no need for every instruction to take a specvtype. Should be modified in the future.
   */
  val specvtype = Output(Vec(DecodeWidth, new VType))
}

class VTypeGen(implicit p: Parameters) extends XSModule {
  val io = IO(new VTypeGenIO())

  private val vtypeSpec = RegInit(VType.initVtype())

  private val vtypeArch = RegInit(VType.initVtype())

  private val vtypeSpecNext = WireInit(vtypeSpec)

  private val vtypeArchNext = WireInit(vtypeArch)
  /** valid signals of instructions */
  private val instValidVec = io.insts.map(_.valid)
  /** instructions code */
  private val instFieldVec = io.insts.map(_.bits.asTypeOf(new XSInstBitFields))
  // valid vseti
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
    Mux(isVsetvli, field.ZIMM_VSETVLI.asTypeOf(new InstVType), field.ZIMM_VSETIVLI.asTypeOf(new InstVType))
  }.map(instVType => VsetVType.fromInstVType(instVType))

  // generate vtype depending on instructions
  private val vsetModuleVec = Seq.fill(DecodeWidth)(Module(new VsetModule()))
  for(i <- 0 until DecodeWidth) {
    vsetModuleVec(i).io.in.avl := 0.U
    vsetModuleVec(i).io.in.vtype := instVTypeVec(i)
    vsetModuleVec(i).io.in.func := VSETOpType.uvsetvcfg_xi
  }

  private val vtypeNewVec = vsetModuleVec.map(_.io.out.vconfig.vtype)

  io.specvtype(0) := vtypeSpec
  for(i <- 1 until DecodeWidth) {
    io.specvtype(i) := io.vtype(i - 1)
  }
  // select vtype for each instruction in a priority manner
  for(i <- 0 until DecodeWidth) {
    io.vtype(i) := PriorityMuxDefault(isVsetiVec.take(i+1).reverse.zip(vtypeNewVec.take(i+1).reverse), vtypeSpec)
  }

  // assign the last vtype to vtypeSpec
  private val vtypeNew = io.vtype(DecodeWidth - 1)

  vtypeSpec := vtypeSpecNext
  vtypeArch := vtypeArchNext

  when(io.commitVType.hasVsetvl) {
    vtypeArchNext := io.vsetvlVType
  } .elsewhen(io.commitVType.vtype.valid) {
    vtypeArchNext := io.commitVType.vtype.bits
  }

  /**
   * Set the source of vtypeSpec from the following sources:
   * 1. committed vsetvl instruction, which flushes the pipeline.
   * 2. walk-vtype, which is used to update vtype when walking.
   * 3. walking to architectural vtype
   * 4. new vset instruction
   */
  when(io.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    vtypeSpecNext := io.vsetvlVType
  }.elsewhen(io.walkVType.valid) {
    vtypeSpecNext := io.walkVType.bits
  }.elsewhen(io.walkToArchVType) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(io.canUpdateVType) {
    vtypeSpecNext := vtypeNew
  }
}
