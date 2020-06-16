package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._




class Dispatch1 extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    // from rename
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new MicroOp)))

    // enq Roq
    val toRoq =  Vec(DecodeWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(DecodeWidth, UInt(RoqIdxWidth.W)))

    // enq Brq
    val toBrq = Vec(DecodeWidth, DecoupledIO(new MicroOp))
    // get brMask/brTag
    val brTags = Input(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Input(Vec(DecodeWidth, UInt(BrqSize.W)))

    // to Dp2
    val out = new Dp1ToDp2IO
  })
}
