package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.brq.BrqPtr
import utils._

class DecodeStage extends XSModule {
  val io = IO(new Bundle() {
    // enq Brq
    val toBrq = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
    // get brMask/brTag
    val brTags = Input(Vec(DecodeWidth, new BrqPtr))

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
    decoderToBrq(i).brTag := DontCare
    io.toBrq(i).bits := decoderToBrq(i)
    // if brq returns ready, then assert valid and send CfCtrl with bfTag and brMask to DecBuffer
    io.out(i).valid := io.toBrq(i).ready && io.in(i).valid
    decoderToDecBuffer(i) := decoders(i).io.out
    decoderToDecBuffer(i).brTag := io.brTags(i)
    io.out(i).bits := decoderToDecBuffer(i)
    XSDebug(
      io.out(i).valid && io.out(i).bits.ctrl.noSpecExec,
      p"noSpecExec inst: pc=${Hexadecimal(io.out(i).bits.cf.pc)}\n"
    )

    // If an instruction i is received by DecBuffer,
    // then assert in(i).ready, waiting for new instructions
    // Only when all out(i).ready signals are true can we decode next instruction group (?)
    io.in(i).ready := io.out(i).ready
  }
}