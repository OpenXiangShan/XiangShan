package bus.simplebus

import chisel3._
import chisel3.util._

import utils._

class SimpleBusCrossbar(m: Int, addressSpace: List[(Long, Long)]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(m, new SimpleBusUC))
    val out = Vec(addressSpace.length, new SimpleBusUC)
  })

  val debug = false

  require(m == 1, "now we only support 1 input channel")
  val inSel = io.in(0)

  // select the output channel according to the address
  val addr = inSel.req.bits.addr
  val outSelVec = VecInit(addressSpace.map(
    range => (addr >= range._1.U && addr < (range._1 + range._2).U)))
  val outSelIdx = PriorityEncoder(outSelVec)
  val outSel = io.out(outSelIdx)

  assert(!inSel.req.valid || outSelVec.asUInt.orR, "address decode error, bad addr = 0x%x\n", addr)
  assert(!(inSel.req.valid && outSelVec.asUInt.andR), "address decode error, bad addr = 0x%x\n", addr)

  val s_idle :: s_req :: s_resp :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // bind out.req channel
  (io.out zip outSelVec).map { case (o, v) => {
    o.req.bits := inSel.req.bits
    o.req.valid := v && ((inSel.req.valid && (state === s_idle)) || (state === s_req))
    o.resp.ready := v
  }}

  val bypass_s_resp = Mux(outSel.resp.fire(), s_idle, s_resp)
  val bypass_s_req = Mux(outSel.req.fire(), bypass_s_resp, s_req)
  switch (state) {
    is (s_idle) {
      when (inSel.req.valid) { state := bypass_s_req }
    }
    is (s_req) {
      when (outSel.req.fire()) { state := bypass_s_resp }
    }
    is (s_resp) {
      when (outSel.resp.fire()) { state := s_idle }
    }
  }

  inSel.resp.valid := outSel.resp.fire()
  inSel.resp.bits <> outSel.resp.bits
  outSel.resp.ready := inSel.resp.ready

  // ack in.req when the response is received
  inSel.req.ready := outSel.resp.fire()

  if (debug) {
    when (state === s_idle && inSel.req.valid) {
      printf(p"${GTimer()}: xbar: in.req: ${inSel.req.bits}\n")
    }

    when (outSel.req.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx = ${outSelIdx}, outSel.req: ${outSel.req.bits}\n")
    }
    when (outSel.resp.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx= ${outSelIdx}, outSel.resp: ${outSel.resp.bits}\n")
    }

    when (inSel.resp.fire()) {
      printf(p"${GTimer()}: xbar: in.resp: ${inSel.resp.bits}\n")
    }
  }
}
