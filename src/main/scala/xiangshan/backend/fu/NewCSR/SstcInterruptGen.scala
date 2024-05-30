package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._

class SstcInterruptGen extends Module {
  val i = IO(Input(new Bundle {
    val stime      = ValidIO(UInt(64.W))
    val vstime     = ValidIO(UInt(64.W))
    val stimecmp   = UInt(64.W)
    val vstimecmp  = UInt(64.W)
    val menvcfgSTCE = Bool()
    val henvcfgSTCE = Bool()
  }))
  val o = IO(Output(new Bundle {
    val STIP = Bool()
    val VSTIP = Bool()
  }))

  // Guard TIP by envcfg.STCE to avoid wrong assertion of time interrupt
  o.STIP  := RegEnable(i.stime.bits  >= i.stimecmp,  false.B, i.stime.valid  && i.menvcfgSTCE)
  o.VSTIP := RegEnable(i.vstime.bits >= i.vstimecmp, false.B, i.vstime.valid && i.henvcfgSTCE)
}
