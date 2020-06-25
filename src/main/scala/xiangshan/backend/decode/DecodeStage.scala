package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._

class DecodeStage extends XSModule {
  val io = IO(new Bundle() {
    // enq Brq
    val toBrq = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    // get brMask/brTag
    val brTags = Input(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Input(Vec(DecodeWidth, UInt(BrqSize.W)))

    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))

    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  })
  val decoders = Seq.fill(DecodeWidth)(Module(new Decoder))
  val decoderToBrq = Wire(Vec(DecodeWidth, new CfCtrl)) // without brTag and brMask
  val decoderToDecBuffer = Wire(Vec(DecodeWidth, new CfCtrl)) // with brTag and brMask

  // Handshake ---------------------
  // 1. if current instruction is valid, then:
  //    First, assert toBrq(i).valid if (in.valid and out.ready and isBr) and present toBrq(i).bits
  //    Second, check toBrq(i).ready and connect it to io.out(i).valid
  // 2. To Decode Buffer:
  //    First, assert in(i).ready if out(i).ready
  //    Second, assert out(i).valid iff in(i).valid and instruction is valid (not implemented) and toBrq(i).ready

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.in <> io.in(i).bits
    decoderToBrq(i) := decoders(i).io.out // CfCtrl without bfTag and brMask
    // send CfCtrl without brTags and brMasks to brq
    io.toBrq(i).valid := io.in(i).valid && io.out(i).ready && decoders(i).io.out.cf.isBr
    XSDebug(io.toBrq(i).valid && io.toBrq(i).ready, p"Branch instr detected. Sending it to BRQ.")
    XSDebug(io.toBrq(i).valid && !io.toBrq(i).ready, p"Branch instr detected. BRQ full...waiting")
    XSDebug(io.in(i).valid && !io.out(i).ready, p"DecBuf full...waiting")
    decoderToBrq(i).brMask := DontCare
    decoderToBrq(i).brTag := DontCare
    io.toBrq(i).bits := decoderToBrq(i)
    // if brq returns ready, then assert valid and send CfCtrl with bfTag and brMask to DecBuffer
    io.out(i).valid := io.toBrq(i).ready && io.in(i).valid
    XSDebug(io.out(i).valid && decoders(i).io.out.cf.isBr && io.out(i).ready, p"Sending branch instr to DecBuf")
    XSDebug(io.out(i).valid && !decoders(i).io.out.cf.isBr && io.out(i).ready, p"Sending non-branch instr to DecBuf")
    decoderToDecBuffer(i) := decoders(i).io.out
    decoderToDecBuffer(i).brTag := io.brTags(i)
    decoderToDecBuffer(i).brMask := io.brMasks(i)
    io.out(i).bits := decoderToDecBuffer(i)

    // If an instruction i is received by DecBuffer,
    // then assert in(i).ready, waiting for new instructions
    // Only when all out(i).ready signals are true can we decode next instruction group (?)
    io.in(i).ready := io.out(i).ready
  }
}