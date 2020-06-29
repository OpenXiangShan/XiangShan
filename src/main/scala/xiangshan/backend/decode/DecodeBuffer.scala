package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._

class DecodeBuffer extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in  = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    val out = Vec(RenameWidth, DecoupledIO(new CfCtrl))
  })

  val q = Module(new MIMOQueue[CfCtrl](
    gen = new CfCtrl,
    entries = DecBufSize,
    inCnt = io.in.size,
    outCnt = io.out.size,
    mem = true,
    perf = false
  ))

  q.io.flush := io.redirect.valid
  q.io.enq <> io.in
  for((out, deq) <- io.out.zip(q.io.deq)){
    out.bits := deq.bits
    out.valid := deq.valid && !io.redirect.valid
    deq.ready := out.ready
  }
}
