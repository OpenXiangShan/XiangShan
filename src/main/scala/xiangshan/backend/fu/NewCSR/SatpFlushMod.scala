package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import system.HasSoCParameter
import utility.HasCircularQueuePtrHelper
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState

class SatpFlushMod(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val in = IO(Input(new SatpFlushMod.In))
  val out = IO(Output(new SatpFlushMod.Out))

  private val oldPrivState = Reg(new PrivState)
  private val oldSatpMode  = Reg(UInt(SatpMode.getWidth.W))
  private val oldVsatpMode = Reg(UInt(SatpMode.getWidth.W))

  private val privState = in.privState

  private val satpWen   = in.satp.valid
  private val vsatpWen  = in.vsatp.valid
  private val satpMode  = in.satp.bits
  private val vsatpMode = in.vsatp.bits

  private val wen = satpWen || vsatpWen

  when(wen) {
    oldPrivState := privState
    oldSatpMode  := satpMode
    oldVsatpMode := vsatpMode
  }

  out.oldPrivState := oldPrivState
  out.oldSatpMode  := oldSatpMode
  out.oldVsatpMode := oldVsatpMode
}

object SatpFlushMod {
  class In(implicit p: Parameters) extends XSBundle with HasXSParameter {
    val satp  = ValidIO(UInt(SatpMode.getWidth.W))
    val vsatp = ValidIO(UInt(SatpMode.getWidth.W))
    val privState  = new PrivState
  }

  class Out(implicit p: Parameters) extends XSBundle with HasXSParameter {
    val oldPrivState = new PrivState
    val oldSatpMode = UInt(SatpMode.getWidth.W)
    val oldVsatpMode = UInt(SatpMode.getWidth.W)
  }
}
