package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}

class SpecZicfilpIO(mopWidth: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val redirect = Flipped(ValidIO(Bool()))
  // One slot per input instruction, before uop splitting and buffering.
  val valid = Input(Vec(mopWidth, Bool()))
  val fire = Input(Vec(mopWidth, Bool()))
  val isJalr = Input(Vec(mopWidth, Bool()))
  val isLPAD = Input(Vec(mopWidth, Bool()))
  // ELP at the entrance to each instruction, including older valid instructions this cycle.
  val elp = Output(Vec(mopWidth, Bool()))
  val exception = Output(Vec(mopWidth, Bool()))
  val lpadValid = Output(Vec(mopWidth, Bool()))
}

/** Tracks the instruction boundary accepted by DecodeChannels, including buffered uop tails. */
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
    // Preview all valid instructions for channel selection, but advance persistent state only
    // over accepted instructions. A split JALR updates ELP once, even if its tail is buffered.
    outputELP(i + 1) := Mux(io.valid(i), io.isJalr(i), outputELP(i))
    nextELP(i + 1) := Mux(io.fire(i), io.isJalr(i), nextELP(i))
  }

  // Buffered uops retain their original LPAD/check metadata; stalls do not consume ELP.
  // Architectural ELP at commit remains the responsibility of ArchZicfilp.
  when(io.redirect.valid) {
    speculativeELP := io.redirect.bits
  }.elsewhen(!io.enable) {
    speculativeELP := false.B
  }.otherwise {
    speculativeELP := nextELP.last
  }
}
