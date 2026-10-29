package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}

// Zicfilp
class SpecZicfilpIO(mopWidth: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val redirect = Flipped(ValidIO(Bool()))
  val valid = Input(Vec(mopWidth, Bool()))
  val fire = Input(Vec(mopWidth, Bool()))
  val isJalr = Input(Vec(mopWidth, Bool()))
  val isLPAD = Input(Vec(mopWidth, Bool()))
  val elp = Output(Vec(mopWidth, Bool()))
  val exception = Output(Vec(mopWidth, Bool()))
  val lpadValid = Output(Vec(mopWidth, Bool()))
}

class SpecZicfilp(mopWidth: Int)(implicit p: Parameters) extends XSModule {
  require(mopWidth > 0)
  val io = IO(new SpecZicfilpIO(mopWidth))

  val speculativeELP = RegInit(false.B)
  val outputELP = Wire(Vec(mopWidth + 1, Bool()))
  val nextELP = Wire(Vec(mopWidth + 1, Bool()))
  outputELP.head := speculativeELP
  nextELP.head := speculativeELP

  for (i <- 0 until mopWidth) {
    val checkLandingPad = io.valid(i) && outputELP(i)
    io.elp(i) := outputELP(i)
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
