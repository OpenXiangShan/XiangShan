package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import xiangshan._
import xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
import xiangshan.backend.rob.RobPtr

class TrapTvalMod(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val fromCtrlBlock = Input(new Bundle {
      val flush = ValidIO(new Redirect)
      val robDeqPtr = Input(new RobPtr)
    })

    val targetPc = Input(ValidIO(new TargetPCBundle))
    val clear = Input(Bool())
    val tval = Output(UInt(XLEN.W))
  })

  private val valid = RegInit(false.B)
  private val tval = Reg(UInt(XLEN.W))
  private val robIdx = Reg(new RobPtr)

  private val updateFromFlush = io.fromCtrlBlock.flush.valid && io.fromCtrlBlock.flush.bits.cfiUpdate.hasBackendFault
  private val clearFromFlush = io.fromCtrlBlock.flush.valid && !io.fromCtrlBlock.flush.bits.cfiUpdate.hasBackendFault

  when(io.targetPc.valid && io.targetPc.bits.raiseFault) {
    valid := true.B
    tval := io.targetPc.bits.pc
    robIdx := io.fromCtrlBlock.robDeqPtr
  }.elsewhen(valid) {
    when(updateFromFlush && isBefore(io.fromCtrlBlock.flush.bits.robIdx, robIdx)) {
      valid := true.B
      tval := io.fromCtrlBlock.flush.bits.fullTarget
      robIdx := io.fromCtrlBlock.flush.bits.robIdx
    }.elsewhen(clearFromFlush && isBefore(io.fromCtrlBlock.flush.bits.robIdx, robIdx) || io.clear) {
      valid := false.B
    }
  }.otherwise {
    when(updateFromFlush) {
      valid := true.B
      tval := io.fromCtrlBlock.flush.bits.fullTarget
      robIdx := io.fromCtrlBlock.flush.bits.robIdx
    }
  }

  io.tval := tval

  when(io.clear) { assert(valid) }
}
