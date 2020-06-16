package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._



class Brq extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val roqRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(DecodeWidth, Flipped(DecoupledIO(new MicroOp)))
    val brTags = Output(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Output(Vec(DecodeWidth, UInt(BrqSize.W)))
    val redirect = Output(Valid(new Redirect))
  })


}
