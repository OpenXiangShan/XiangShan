package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.decode.isa.bitfield.{InstVType, Riscv32BitInst, XSInstBitFields}
import xiangshan.backend.fu.VsetModule

class VTypeGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val insts = Flipped(Vec(DecodeWidth, ValidIO(UInt(32.W))))
    val redirect = Input(Bool())
    val walkVType   = Flipped(Valid(new VType))
    val canUpdateVType = Input(Bool())
    val vtype = Output(new VType)
    val vsetvlVType = Input(new VType)
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
    val lastSpecVType = (Valid(new VType))
    val specVtype = Output(new VType)
  })
  private val instValidVec = io.insts.map(_.valid)
  private val instFieldVec = io.insts.map(_.bits.asTypeOf(new XSInstBitFields))
  // Only check vsetvli and vsetivli here.
  // vsetvl will flush pipe, need not to generate new vtype in decode stage.
  private val isVsetVec = VecInit(instFieldVec.map(fields =>
    (fields.OPCODE === "b1010111".U) && (fields.WIDTH === "b111".U) && (
      fields.ALL(31) === "b0".U ||
      fields.ALL(31, 30) === "b11".U
    )
  ).zip(instValidVec).map { case (isVset, valid) => valid && isVset})

  private val firstVsetOH: Vec[Bool] = VecInit(PriorityEncoderOH(isVsetVec))
  private val firstVsetInstField: XSInstBitFields = PriorityMux(firstVsetOH, instFieldVec)

  private val vtypeArch = RegInit(0.U.asTypeOf(new VType))
  private val vtypeSpec = RegInit(0.U.asTypeOf(new VType))
  private val lastSpecVType = RegInit(0.U.asTypeOf(new ValidIO(VType())))

  private val vtypeArchNext = WireInit(vtypeArch)
  private val vtypeSpecNext = WireInit(vtypeSpec)
  private val lastSpecVTypeNext = WireInit(lastSpecVType)

  vtypeArch := vtypeArchNext
  vtypeSpec := vtypeSpecNext
  lastSpecVType := lastSpecVTypeNext

  private val instVType: InstVType = firstVsetInstField.ZIMM_VTYPE.asTypeOf(new InstVType)
  private val vtypei: VType = VType.fromInstVType(instVType)

  private val vsetModule = Module(new VsetModule)
  vsetModule.io.in.avl := 0.U
  vsetModule.io.in.vtype := vtypei
  vsetModule.io.in.func := VSETOpType.uvsetvcfg_xi

  private val vtypeNew = vsetModule.io.out.vconfig.vtype

  when(io.commitVType.hasVsetvl) {
    vtypeArchNext := io.vsetvlVType
  }.elsewhen(io.commitVType.vtype.valid) {
    vtypeArchNext := io.commitVType.vtype.bits
  }

  private val inHasVset = isVsetVec.asUInt.orR

  when(io.commitVType.hasVsetvl) {
    // when vsetvl instruction commit, also update vtypeSpec, because vsetvl flush pipe
    lastSpecVTypeNext.valid := true.B
    lastSpecVTypeNext.bits := vtypeSpec
    vtypeSpecNext := io.vsetvlVType
  }.elsewhen(io.walkVType.valid) {
    lastSpecVTypeNext.valid := false.B
    vtypeSpecNext := io.walkVType.bits
  }.elsewhen(io.redirect && io.commitVType.vtype.valid) {
    // when redirect and commit both coming, we should use commit vtype
    lastSpecVTypeNext.valid := false.B
    vtypeSpecNext := io.commitVType.vtype.bits
  }.elsewhen(io.redirect && !io.commitVType.vtype.valid) {
    lastSpecVTypeNext.valid := false.B
    vtypeSpecNext := vtypeArch
  }.elsewhen(inHasVset && io.canUpdateVType) {
    lastSpecVTypeNext.valid := true.B
    lastSpecVTypeNext.bits := vtypeSpec
    vtypeSpecNext := vtypeNew
  }.otherwise {
    lastSpecVTypeNext.valid := false.B
  }

  io.vtype := vtypeSpec
  io.specVtype := vtypeSpec
  io.lastSpecVType := lastSpecVType

  // just make verilog more readable
  dontTouch(isVsetVec)
}