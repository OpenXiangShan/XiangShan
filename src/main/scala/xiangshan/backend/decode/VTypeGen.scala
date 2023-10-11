package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.decode.isa.bitfield.InstVType
import xiangshan.backend.fu.VsetModule

class VTypeGen(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle(){
    val firstInstr = Flipped(Valid(new Bundle() {
      val instr = UInt(32.W)
      val isVset = Bool()
    }))
    val isRedirect = Input(Bool())
    val commitVType = Flipped(Valid(new VType))
    val walkVType   = Flipped(Valid(new VType))

    val vtype = Output(new VType)
  })
  private val VTYPE_IMM_MSB = 27
  private val VTYPE_IMM_LSB = 20

  private val vtypeArch = RegInit(0.U.asTypeOf(new VType))
  private val vtypeSpec = RegInit(0.U.asTypeOf(new VType))

  private val vtypeArchNext = WireInit(vtypeArch)
  private val vtypeSpecNext = WireInit(vtypeSpec)

  vtypeArch := vtypeArchNext
  vtypeSpec := vtypeSpecNext

  private val instVType: InstVType = io.firstInstr.bits.instr(VTYPE_IMM_MSB, VTYPE_IMM_LSB).asTypeOf(new InstVType)
  private val vtype: VType = VType.fromInstVType(instVType)

  private val vsetModule = Module(new VsetModule)
  vsetModule.io.in.avl := 0.U
  vsetModule.io.in.vtype := vtype
  vsetModule.io.in.func := VSETOpType.uvsetvcfg_xi

  when(io.commitVType.valid) {
    vtypeArchNext := io.commitVType.bits
  }

  when(io.isRedirect) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(io.walkVType.valid) {
    vtypeSpecNext := io.walkVType.bits
  }.elsewhen(io.firstInstr.valid && io.firstInstr.bits.isVset) {
    vtypeSpecNext := vsetModule.io.out.vconfig.vtype
  }

  io.vtype := vtypeSpecNext
}