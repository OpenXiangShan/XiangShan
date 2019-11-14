package system

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus._
import noop.HasNOOPParameter

trait HasCoherenceParameter extends HasNOOPParameter {
  val supportCoh = HasDcache
}

class CoherenceManager extends Module with HasCoherenceParameter {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = new Bundle {
      val mem = new SimpleBusUC
      val coh = new SimpleBusUC
    }
  })

  // state transition:
  // write: s_idle -> s_memWriteResp -> s_idle
  // read: s_idle -> s_probeResp -> (hit) s_probeForward -> s_idle
  //                             +> (miss) s_memReadReq -> s_memReadResp -> s_idle

  val s_idle :: s_probeResp :: s_probeForward :: s_memReadReq :: s_memReadResp :: s_memWriteResp :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val inflight = state =/= s_idle

  val thisReq = io.in.req
  assert(!(thisReq.valid && !thisReq.bits.isRead() && !thisReq.bits.isWrite()))

  // when read, we should first probe dcache
  val reqLatch = RegEnable(thisReq.bits, !inflight && thisReq.bits.isRead())
  io.out.coh match { case c => {
    c.req.bits := thisReq.bits
    c.req.bits.cmd := SimpleBusCmd.probe
    c.resp.ready := true.B
  }}

  io.out.mem.req.bits := thisReq.bits
  // bind correct valid and ready signals
  io.out.mem.req.valid := false.B
  thisReq.ready := false.B
  io.out.coh.req.valid := false.B
  when (if (supportCoh) thisReq.bits.isWrite() else true.B) {
    io.out.mem.req.valid := thisReq.valid && !inflight
    thisReq.ready := io.out.mem.req.ready && !inflight
  } .elsewhen (thisReq.bits.isRead()) {
    io.out.coh.req.valid := thisReq.valid && !inflight
    thisReq.ready := io.out.coh.req.ready && !inflight
  }

  io.in.resp <> io.out.mem.resp

  switch (state) {
    is (s_idle) {
      when (thisReq.fire()) {
        when (thisReq.bits.isRead()) { state := Mux(supportCoh.B, s_probeResp, s_memReadResp) }
        .elsewhen (thisReq.bits.isWriteLast()) { state := s_memWriteResp }
      }
    }
    is (s_probeResp) {
      when (io.out.coh.resp.fire()) {
        state := Mux(io.out.coh.resp.bits.isProbeHit(), s_probeForward, s_memReadReq)
      }
    }
    is (s_probeForward) {
      val thisResp = io.in.resp
      thisResp <> io.out.coh.resp
      when (thisResp.fire() && thisResp.bits.isReadLast()) { state := s_idle }
    }
    is (s_memReadReq) {
      io.out.mem.req.bits := reqLatch
      io.out.mem.req.valid := true.B
      when (io.out.mem.req.fire()) { state := s_memReadResp }
    }
    is (s_memReadResp) { when (io.out.mem.resp.fire() && io.out.mem.resp.bits.isReadLast()) { state := s_idle } }
    is (s_memWriteResp) { when (io.out.mem.resp.fire()) { state := s_idle } }
  }
}
