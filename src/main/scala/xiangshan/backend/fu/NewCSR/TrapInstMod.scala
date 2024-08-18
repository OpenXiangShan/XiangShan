package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan._
import xiangshan.backend.Bundles.TrapInstInfo
import xiangshan.backend.decode.Imm_Z
import xiangshan.frontend.FtqPtr
import xiangshan.backend.decode.isa.bitfield.OPCODE5Bit

class FtqInfo(implicit p: Parameters) extends XSBundle {
  val ftqPtr = new FtqPtr()
  val ftqOffset = UInt(log2Up(PredictWidth).W)
}

class TrapInstMod(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val fromDecode = Input(new Bundle {
      val trapInstInfo = ValidIO(new TrapInstInfo)
    })

    val fromRob = Input(new Bundle {
      val flush = ValidIO(new FtqInfo)
    })

    val faultCsrUop = Input(ValidIO(new Bundle {
      val fuOpType = FuOpType()
      val imm      = UInt(Imm_Z().len.W)
    }))

    val readClear = Input(Bool())
    val currentTrapInst = Output(ValidIO(UInt(32.W)))
  })

  // alias
  val flush = io.fromRob.flush
  val newTrapInstInfo = io.fromDecode.trapInstInfo

  val valid = RegInit(false.B)
  val trapInstInfo = Reg(new TrapInstInfo)

  val csrAddr = Imm_Z().getCSRAddr(io.faultCsrUop.bits.imm)
  val rs1 = Imm_Z().getRS1(io.faultCsrUop.bits.imm)
  val rd = Imm_Z().getRD(io.faultCsrUop.bits.imm)
  val func3 = CSROpType.getFunc3(io.faultCsrUop.bits.fuOpType)

  val csrInst = Cat(csrAddr, rs1, func3, rd, OPCODE5Bit.SYSTEM, "b11".U)
  require(csrInst.getWidth == 32)

  val newCSRInstValid = io.faultCsrUop.valid
  val newCSRInst = WireInit(0.U.asTypeOf(new TrapInstInfo))
  newCSRInst.instr := csrInst

  when (flush.valid && valid && trapInstInfo.needFlush(flush.bits.ftqPtr, flush.bits.ftqOffset)) {
    valid := false.B
  }.elsewhen(io.readClear) {
    valid := false.B
  }.elsewhen(newTrapInstInfo.valid && !valid) {
    valid := true.B
    trapInstInfo := newTrapInstInfo.bits
    trapInstInfo.instr := Mux(
      newTrapInstInfo.bits.instr(1, 0) === "b11".U,
      newTrapInstInfo.bits.instr,
      newTrapInstInfo.bits.instr(15, 0)
    )
  }.elsewhen(newCSRInstValid && !valid) {
    valid := true.B
    trapInstInfo := newCSRInst
  }

  io.currentTrapInst.valid := valid
  io.currentTrapInst.bits := trapInstInfo.instr

  XSError(newTrapInstInfo.valid && valid, "Already a trap instruction in TrapInstMod")
}
