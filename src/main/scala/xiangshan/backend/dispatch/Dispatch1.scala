package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._




class Dispatch1 extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    // from rename
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))

    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))

    // to Dp2
    val out = new Dp1ToDp2IO
  })
}
