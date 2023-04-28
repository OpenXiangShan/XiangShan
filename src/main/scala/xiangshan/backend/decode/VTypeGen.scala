package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._

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

  when(io.commitVType.valid) {
    vtypeArchNext := io.commitVType.bits
  }

  when(io.walkVType.valid) {
    vtypeSpecNext := io.walkVType.bits
  }.elsewhen(RegNext(io.isRedirect)) {
    vtypeSpecNext := vtypeArch
  }.elsewhen(io.firstInstr.valid && io.firstInstr.bits.isVset) {
    vtypeSpecNext := io.firstInstr.bits.instr(VTYPE_IMM_MSB, VTYPE_IMM_LSB).asTypeOf(new VType)
  }

  io.vtype := vtypeSpec
}