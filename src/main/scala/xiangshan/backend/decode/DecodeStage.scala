package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.brq.BrqEnqIO
import utils._
import xiangshan.backend.decode.Instructions.{AUIPC, MRET, SRET}

class DecodeStage extends XSModule {
  val io = IO(new Bundle() {
    // enq Brq
    val enqBrq = Flipped(new BrqEnqIO)

    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))

    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  // Handshake ---------------------
  // 1. if current instruction is valid, then:
  //    First, assert toBrq(i).valid if (in.valid and out.ready and isBr) and present toBrq(i).bits
  //    Second, check toBrq(i).ready and connect it to io.out(i).valid
  // 2. To Decode Buffer:
  //    First, assert in(i).ready if out(i).ready
  //    Second, assert out(i).valid iff in(i).valid and instruction is valid (not implemented) and toBrq(i).ready

  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits

    val isMret = io.in(i).bits.instr === MRET
    val isSret = io.in(i).bits.instr === SRET
    val isAuiPc = io.in(i).bits.instr === AUIPC
    val thisBrqValid = !io.in(i).bits.brUpdate.pd.notCFI || isMret || isSret || isAuiPc
    io.enqBrq.needAlloc(i) := thisBrqValid
    io.enqBrq.req(i).valid := io.in(i).valid && thisBrqValid && io.out(i).ready
    io.enqBrq.req(i).bits  := io.in(i).bits
    io.enqBrq.req(i).bits.instr := decoders(i).io.deq.cf_ctrl.cf.instr

    io.out(i).valid      := io.in(i).valid && io.enqBrq.req(i).ready
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
    io.out(i).bits.brTag := io.enqBrq.resp(i)

    io.in(i).ready := io.out(i).ready && io.enqBrq.req(i).ready

    XSDebug(io.in(i).valid || io.out(i).valid || io.enqBrq.req(i).valid,
      "i:%d In(%d %d) Out(%d %d) ToBrq(%d %d) pc:%x instr:%x\n",
      i.U, io.in(i).valid, io.in(i).ready, io.out(i).valid, io.out(i).ready,
      io.enqBrq.req(i).valid, io.enqBrq.req(i).ready, io.in(i).bits.pc, io.in(i).bits.instr)
  }
}
