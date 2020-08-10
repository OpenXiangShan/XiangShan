package bus.tilelink

import chisel3._
import chisel3.util._
import utils.{Debug, GTimer}

// Only support A and D channel, very naive...

class NaiveTL1toN
(
  addressSpace: List[(Long, Long)],
  para: TLParameters
) extends Module{
  val io = IO(new Bundle() {
    val in = Flipped(TLCached(para))
    val out = Vec(addressSpace.length, TLCached(para))
  })

  io.in <> DontCare
  io.out <> DontCare

  val s_idle :: s_resp :: s_error :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // select the output channel according to the address
  val addr = io.in.a.bits.address
  val outSelVec = VecInit(addressSpace.map(
    range => addr >= range._1.U && addr < (range._1 + range._2).U
  ))
  val outSelIdx = PriorityEncoder(outSelVec)
  val outSel = io.out(outSelIdx)
  val outSelIdxResp = RegEnable(outSelIdx, outSel.a.fire() && (state === s_idle))
  val outSelResp = io.out(outSelIdxResp)
  val reqInvalidAddr = io.in.a.valid && !outSelVec.asUInt.orR

  when(
    !(!io.in.a.valid || outSelVec.asUInt.orR) || (io.in.a.valid && outSelVec.asUInt.andR)
  ){
    printf("[ERROR] bad addr %x, time %d\n", addr, GTimer())
  }
  // assert(!io.in.req.valid || outSelVec.asUInt.orR, "address decode error, bad addr = 0x%x\n", addr)
  assert(
    !(io.in.a.valid && outSelVec.asUInt.andR),
    "address decode error, bad addr = 0x%x\n", addr
  )

  // bind out.req channel
  (io.out zip outSelVec).foreach { case (o, v) =>
    o.a.bits := io.in.a.bits
    o.a.valid := v && (io.in.a.valid && (state === s_idle))
    o.d.ready := v
  }

  switch (state) {
    is (s_idle) {
      when (outSel.a.fire()) { state := s_resp }
      when (reqInvalidAddr) { state := s_error }
    }
    is (s_resp) { when (outSelResp.d.fire()) { state := s_idle } }
    is (s_error) { when(io.in.d.fire()){ state := s_idle } }
  }

  io.in.d.valid := outSelResp.d.fire() || state === s_error
  io.in.d.bits <> outSelResp.d.bits
  // io.in.resp.bits.exc.get := state === s_error
  outSelResp.d.ready := io.in.d.ready
  io.in.a.ready := outSel.a.ready || reqInvalidAddr

  Debug() {
    when (state === s_idle && io.in.a.valid) {
      printf(p"${GTimer()}: req: ")
      io.in.a.bits.dump()
    }

    when (outSel.a.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx = $outSelIdx, outSel.req: ")
      outSel.a.bits.dump()
    }
    when (outSel.d.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx= $outSelIdx, outSel.resp: ")
      outSel.d.bits.dump()
    }

    when (io.in.d.fire()) {
      printf(p"${GTimer()}: xbar: in.resp: ")
      io.in.d.bits.dump()
    }
  }
}
