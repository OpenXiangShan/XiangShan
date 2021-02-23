package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class DecodeStage extends XSModule {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new CtrlFlow)))
    // from memblock
    val waitTableUpdate = Vec(StorePipelineWidth, Input(new WaitTableUpdateReq))
    // to DecBuffer
    val out = Vec(DecodeWidth, DecoupledIO(new CfCtrl))
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  val waittable = Module(new WaitTable)
  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrl_flow <> io.in(i).bits
    
    // read waittable, update loadWaitBit
    waittable.io.raddr(i) := io.in(i).bits.pc(VAddrBits-1, 1)
    decoders(i).io.enq.ctrl_flow.loadWaitBit := waittable.io.rdata(i)

    io.out(i).valid      := io.in(i).valid
    io.out(i).bits       := decoders(i).io.deq.cf_ctrl
    io.in(i).ready := io.out(i).ready
  }

  for (i <- 0 until StorePipelineWidth) {
    waittable.io.update(i) <> RegNext(io.waitTableUpdate(i))
  }

  val loadWaitBitSet = PopCount(VecInit((0 until DecodeWidth).map(i => waittable.io.rdata(i) && io.out(i).fire())))
  XSPerf("loadWaitBitSet", loadWaitBitSet, acc = true) // rollback redirect generated
}
