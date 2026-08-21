package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState


class SatpFlushMod extends Module {
  val in  = IO(Input(new SatpFlushMod.In))
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
    oldSatpMode  := satpMode
    oldVsatpMode := vsatpMode
    oldPrivState := privState
  }

  out.oldSatpMode  := oldSatpMode
  out.oldVsatpMode := oldVsatpMode
  out.oldPrivState := oldPrivState
}

object SatpFlushMod {
  class In extends Bundle {
    val satp      = Valid(UInt(SatpMode.getWidth.W))
    val vsatp     = Valid(UInt(SatpMode.getWidth.W))
    val privState = new PrivState
  }

  class Out extends Bundle {
    val oldSatpMode  = UInt(SatpMode.getWidth.W)
    val oldVsatpMode = UInt(SatpMode.getWidth.W)
    val oldPrivState = new PrivState
  }
}
