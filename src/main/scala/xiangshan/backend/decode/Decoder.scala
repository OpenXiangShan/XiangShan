package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._


class Decoder extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val in = Input(new CtrlFlow)
    val out = Output(new CfCtrl)
  })
}

