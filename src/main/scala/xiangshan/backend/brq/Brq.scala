package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._



class Brq extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    // interrupt/exception happen, flush Brq
    val roqRedirect = Input(Valid(new Redirect))
    // receive branch/jump calculated target
    val exuRedirect = Vec(exuConfig.BruCnt + exuConfig.AluCnt, Flipped(ValidIO(new Redirect)))
    // from decode, branch insts enq
    val enqReqs = Vec(DecodeWidth, Flipped(DecoupledIO(new MicroOp)))
    // to decode
    val brTags = Output(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Output(Vec(DecodeWidth, UInt(BrqSize.W)))

    // misprediction, flush pipeline
    val redirect = Output(Valid(new Redirect))
  })
}
