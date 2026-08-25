package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}

// Zicfilp
class SpecZicfilpIO(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val redirect = Flipped(ValidIO(Bool()))
  val valid = Input(Vec(DecodeWidth, Bool()))
  val fire = Input(Vec(DecodeWidth, Bool()))
  val isJalr = Input(Vec(DecodeWidth, Bool()))
  val isLPAD = Input(Vec(DecodeWidth, Bool()))
  val exception = Output(Vec(DecodeWidth, Bool()))
  val lpadValid = Output(Vec(DecodeWidth, Bool()))
}

class SpecZicfilp(implicit p: Parameters) extends XSModule {
  val io = IO(new SpecZicfilpIO)

  val speculativeELP = RegInit(false.B)
  val outputELP = Wire(Vec(DecodeWidth + 1, Bool()))
  val nextELP = Wire(Vec(DecodeWidth + 1, Bool()))
  outputELP.head := speculativeELP
  nextELP.head := speculativeELP

  for (i <- 0 until DecodeWidth) {
    val checkLandingPad = io.valid(i) && outputELP(i)
    io.exception(i) := io.enable && checkLandingPad && !io.isLPAD(i)
    io.lpadValid(i) := io.enable && checkLandingPad && io.isLPAD(i)
    outputELP(i + 1) := Mux(io.valid(i), io.isJalr(i), outputELP(i))
    nextELP(i + 1) := Mux(io.fire(i), io.isJalr(i), nextELP(i))
  }

  when(io.redirect.valid) {
    speculativeELP := io.redirect.bits
  }.elsewhen(!io.enable) {
    speculativeELP := false.B
  }.otherwise {
    speculativeELP := nextELP.last
  }
}
