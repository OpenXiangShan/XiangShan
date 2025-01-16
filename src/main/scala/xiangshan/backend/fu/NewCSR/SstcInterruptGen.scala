package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._

class SstcInterruptGen extends Module {
  val i = IO(Input(new Bundle {
    val stime      = ValidIO(UInt(64.W))
    val vstime     = ValidIO(UInt(64.W))
    val stimecmp   = new Bundle {
      val wen = Bool()
      val rdata = UInt(64.W)
    }
    val vstimecmp  = new Bundle {
      val wen = Bool()
      val rdata = UInt(64.W)
    }
    val htimedeltaWen = Bool()
    val menvcfg = new Bundle {
      val wen = Bool()
      val STCE = Bool()
    }
    val henvcfg = new Bundle {
      val wen = Bool()
      val STCE = Bool()
    }
  }))
  val o = IO(Output(new Bundle {
    val STIP = Bool()
    val VSTIP = Bool()
  }))

  // Guard TIP by envcfg.STCE to avoid wrong assertion of time interrupt
  o.STIP  := RegEnable(i.stime.bits >= i.stimecmp.rdata && i.menvcfg.STCE, false.B, i.stime.valid || i.stimecmp.wen || i.menvcfg.wen)
  o.VSTIP := RegEnable(i.vstime.bits >= i.vstimecmp.rdata && i.henvcfg.STCE, false.B, i.vstime.valid || i.vstimecmp.wen || i.htimedeltaWen || i.menvcfg.wen || i.henvcfg.wen)
}
