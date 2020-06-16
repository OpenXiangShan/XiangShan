package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import xiangshan.utils._

class Ibuffer extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  })
}