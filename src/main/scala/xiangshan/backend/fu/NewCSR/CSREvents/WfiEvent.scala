package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR._


class WfiEventOutput extends Bundle {
  val wfi_event = ValidIO(Bool())
}

class WfiEventInput extends Bundle {
  val mie = Input(new MieBundle)
  val mip = Input(new MipBundle)
}

class WfiEventModule extends Module with CSREventBase {
  val in = IO(new WfiEventInput)
  val out = IO(new WfiEventOutput)

  out := DontCare

  out.wfi_event.valid := valid
  out.wfi_event.bits := (in.mie.asUInt(11, 0) & in.mip.asUInt).orR
}
