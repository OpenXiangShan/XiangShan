package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._

class DecodeStage extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    // enq Brq
    val toBrq = Vec(DecodeWidth, DecoupledIO(new MicroOp))
    // get brMask/brTag
    val brTags = Input(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Input(Vec(DecodeWidth, UInt(BrqSize.W)))

    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))

    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  })
}