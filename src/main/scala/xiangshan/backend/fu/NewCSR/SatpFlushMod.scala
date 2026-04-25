package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import system.HasSoCParameter
import utility.HasCircularQueuePtrHelper
import xiangshan.backend.rob.RobPtr

class SatpFlushMod(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val in = IO(Input(new SatpFlushMod.In))
  val out = IO(Output(new SatpFlushMod.Out))

  private val valid = RegInit(false.B)
  private val epc = Reg(UInt(VAddrBits.W))
  private val robIdx = Reg(new RobPtr)

  private val flush = in.fromCtrlBlock.flush.valid

  when(in.targetPC.valid) {
    valid := true.B
    epc := in.targetPC.bits
    robIdx := in.fromCtrlBlock.robDeqPtr
  }.elsewhen(valid) {
    when(flush && isBefore(in.fromCtrlBlock.flush.bits.robIdx, robIdx) || in.clear) {
      valid := false.B
    }
  }

  out.satpFlushPc.valid := valid
  out.satpFlushPc.bits := epc

}

object SatpFlushMod {
  class In(implicit p: Parameters) extends XSBundle with HasXSParameter {
    val targetPC = ValidIO(UInt(VAddrBits.W))
    val firstFetchFault = Bool()
    val clear = Bool()
    val fromCtrlBlock = new FromCtrlBlockInfo
  }

  class Out(implicit p: Parameters) extends XSBundle with HasXSParameter {
    val satpFlushPc = ValidIO(UInt(VAddrBits.W))
  }

  class FromCtrlBlockInfo(implicit p: Parameters) extends XSBundle with HasXSParameter {
    val flush = ValidIO(new Redirect)
    val robDeqPtr = new RobPtr
  }

}
