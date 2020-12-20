package xiangshan.backend.fu.fpu

import chisel3._
import xiangshan.NeedImpl

class FMA extends FPUSubModule with NeedImpl {

  io.in.ready := true.B

}
