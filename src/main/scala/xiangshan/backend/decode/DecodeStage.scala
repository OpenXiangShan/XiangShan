package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class DecodeStage extends XSModule {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))

    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits
    io.out(i).valid      := io.in(i).valid
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
    io.in(i).ready := io.out(i).ready
  }
}
