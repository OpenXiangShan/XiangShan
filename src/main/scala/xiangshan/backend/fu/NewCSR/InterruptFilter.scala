package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState, XtvecBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.XtvecMode
import xiangshan.backend.fu.util.CSRConst


class InterruptFilter extends Module {
  val io = IO(new InterruptFilterIO)
}

class InterruptFilterIO extends Bundle {
  val in = Input(new Bundle {
    val mstatusMIE  = Bool()
    val sstatusSIE  = Bool()
    val vsstatusSIE = Bool()
    val mip = new MipBundle
    val mie = new MieBundle
    val privState = new PrivState
  })

  val out = Output(new Bundle {
    val interruptVec = ValidIO(UInt(64.W))
  })
}
