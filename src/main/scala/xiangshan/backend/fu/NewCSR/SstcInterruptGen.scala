package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._

class SstcInterruptGen extends Module {
  val i = IO(Input(new Bundle {
    val time       = ValidIO(UInt(64.W))
    val htimedelta = UInt(64.W)
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
  o.STIP  := RegEnable(i.time.bits                >= i.stimecmp,  false.B, i.time.valid && i.menvcfgSTCE)
  o.VSTIP := RegEnable(i.time.bits + i.htimedelta >= i.vstimecmp, false.B, i.time.valid && i.henvcfgSTCE)
}
