package system

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus._

class CoherenceInterconnect extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(2, new SimpleBusC))
    val out = new SimpleBusUH
  })

  val inflight = RegInit(false.B)
  val inflightSrc = Reg(UInt(1.W)) // 0 - icache, 1 - dcache

  val lockWriteFun = ((x: SimpleBusUHReqBundle) => x.isWrite())
  val inputArb = Module(new LockingArbiter(chiselTypeOf(io.in(0).mem.req.bits), 2, 8, Some(lockWriteFun)))
  (inputArb.io.in zip io.in.map(_.mem.req)).map{ case (arb, in) => arb <> in }

  io.out.req.valid := inputArb.io.out.valid && !inflight
  io.out.req.bits := inputArb.io.out.bits
  inputArb.io.out.ready := io.out.req.ready && !inflight

  io.in.map(_.mem.resp.bits := io.out.resp.bits)
  io.in.map(_.mem.resp.valid := false.B)
  (io.in(inflightSrc).mem.resp, io.out.resp) match { case (l, r) => {
    l.valid := r.valid
    r.ready := l.ready
  }}

  io.in.map(_.coh).map { case coh => {
    coh.req.bits := DontCare
    coh.req.valid := false.B
    coh.resp.ready := true.B
  }}

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) {
      when (inputArb.io.out.fire()) {
        inflightSrc := inputArb.io.chosen
        when (!inputArb.io.out.bits.isWrite()) {
          inflight := true.B
          state := s_memReadResp
        } .elsewhen (inputArb.io.out.bits.wlast) {
          inflight := true.B
          state := s_memWriteResp
        }
      }
    }
    is (s_memReadResp) {
      when (io.out.resp.fire() && io.out.resp.bits.rlast) {
        inflight := false.B
        state := s_idle
      }
    }
    is (s_memWriteResp) {
      when (io.out.resp.fire()) {
        inflight := false.B
        state := s_idle
      }
    }
  }
}
