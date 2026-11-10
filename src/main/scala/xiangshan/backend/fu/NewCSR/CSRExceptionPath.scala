package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.ValidIO
import utility.DelayN
import xiangshan.backend.Bundles.ExceptionInfo

/** The production exception transport has no ready or cancellation handshake.
  * Keep the PC correction, execution-unit delay, and CSR payload mapping shared
  * with module integration tests so new metadata cannot silently take a bypass.
  */
object CSRExceptionPath {
  def withPc(exception: ValidIO[ExceptionInfo], pc: UInt): ValidIO[ExceptionInfo] = {
    val corrected = WireDefault(exception)
    corrected.bits.pc := pc
    corrected
  }

  def delay(exception: ValidIO[ExceptionInfo]): ValidIO[ExceptionInfo] = DelayN(exception, 2)

  def connect(csr: NewCSR, exception: ValidIO[ExceptionInfo]): Unit = {
    val trap = csr.io.fromRob.trap
    trap.valid := exception.valid
    trap.bits.pc := exception.bits.pc
    trap.bits.instr := exception.bits.instr
    trap.bits.pcGPA := exception.bits.gpaddr
    // The CSR vector is wider than the backend's sparse exception vector.
    trap.bits.trapVec := exception.bits.exceptionVec.asUInt
    trap.bits.isFetchBkpt := exception.bits.isPcBkpt
    trap.bits.singleStep := exception.bits.singleStep
    trap.bits.crossPageIPFFix := exception.bits.crossPageIPFFix
    trap.bits.isInterrupt := exception.bits.isInterrupt
    trap.bits.trigger := exception.bits.trigger
    trap.bits.isHls := exception.bits.isHls
    trap.bits.isFetchMalAddr := exception.bits.isFetchMalAddr
    trap.bits.isForVSnonLeafPTE := exception.bits.isForVSnonLeafPTE
    trap.bits.satpFlushFirstFetchFault := exception.bits.satpFlushFirstFetchFault
    trap.bits.isFormer := exception.bits.isFormer
    trap.bits.formerFsDirty := exception.bits.formerFsDirty
  }
}
