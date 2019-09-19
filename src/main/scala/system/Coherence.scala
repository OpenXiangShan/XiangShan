package system

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus._

trait HasCoherenceConst {
  val supportCoh = false
}

class CoherenceInterconnect extends Module with HasCoherenceConst {
  val io = IO(new Bundle {
    val in = Flipped(Vec(2, new SimpleBusC))
    val out = new SimpleBusUC
  })

  def anotherMaster(thisMaster: UInt) = Mux(thisMaster === 1.U, 0.U, 1.U)
  def isDcache() = inputArb.io.chosen === 1.U

  // state transition:
  // write: s_idle -> s_memWriteResp -> s_idle
  // read from Dcache: s_idle -> s_memResp -> s_idle
  // read from Icache: s_idle -> s_probeResp -> (hit) s_probeForward -> s_idle
  //                                         +> (miss) s_memReadReq -> s_memReadResp -> s_idle

  val s_idle :: s_probeResp :: s_probeForward :: s_memReadReq :: s_memReadResp :: s_memWriteResp :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val inflight = RegInit(false.B)
  val inflightSrc = Reg(UInt(1.W)) // 0 - icache, 1 - dcache

  val lockWriteFun = ((x: SimpleBusReqBundle) => x.isWrite())
  val inputArb = Module(new LockingArbiter(chiselTypeOf(io.in(0).mem.req.bits), 2, 4, Some(lockWriteFun)))
  (inputArb.io.in zip io.in.map(_.mem.req)).map{ case (arb, in) => arb <> in }

  val thisReq = inputArb.io.out
  assert(!(thisReq.valid && !thisReq.bits.isRead() && !thisReq.bits.isWrite()))

  // when read, we should first probe another master
  val reqLatch = RegEnable(thisReq.bits, !inflight && thisReq.bits.isRead())
  io.in.map(_.coh).map { case c => {
    c.req.bits := thisReq.bits
    c.req.bits.cmd := SimpleBusCmd.probe
    c.resp.ready := true.B
  }}

  io.out.req.bits := thisReq.bits
  // bind correct valid and ready signals
  io.out.req.valid := false.B
  thisReq.ready := false.B
  io.in.map(_.coh.req.valid).map { _ := false.B }
  when (if (supportCoh) (thisReq.bits.isWrite() || isDcache()) else true.B) {
    io.out.req.valid := thisReq.valid && !inflight
    thisReq.ready := io.out.req.ready && !inflight
  } .elsewhen (thisReq.bits.isRead()) {
    io.in(anotherMaster(inputArb.io.chosen)).coh.req.valid := thisReq.valid && !inflight
    thisReq.ready := io.in(anotherMaster(inputArb.io.chosen)).coh.req.ready && !inflight
  }

  io.in.map(_.mem.resp.bits := io.out.resp.bits)
  io.in.map(_.mem.resp.valid := false.B)
  (io.in(inflightSrc).mem.resp, io.out.resp) match { case (l, r) => {
    l.valid := r.valid
    r.ready := l.ready
  }}

  switch (state) {
    is (s_idle) {
      when (thisReq.fire()) {
        inflightSrc := inputArb.io.chosen
        when (thisReq.bits.isRead()) {
          inflight := true.B
          state := Mux(if (supportCoh) isDcache() else true.B, s_memReadResp, s_probeResp)
        } .elsewhen (thisReq.bits.isWriteLast()) {
          inflight := true.B
          state := s_memWriteResp
        }
      }
    }
    is (s_probeResp) {
      when (io.in(anotherMaster(inflightSrc)).coh.resp.fire()) {
        state := Mux(io.in(anotherMaster(inflightSrc)).coh.resp.bits.isProbeHit(), s_probeForward, s_memReadReq)
      }
    }
    is (s_probeForward) {
      val thisResp = io.in(inflightSrc).mem.resp
      val anotherCohResp = io.in(anotherMaster(inflightSrc)).coh.resp
      thisResp.bits := anotherCohResp.bits
      thisResp.valid := anotherCohResp.valid
      anotherCohResp.ready := thisResp.ready
      when (thisResp.fire() && thisResp.bits.isReadLast()) {
        inflight := false.B
        state := s_idle
      }
    }
    is (s_memReadReq) {
      io.out.req.bits := reqLatch
      io.out.req.valid := true.B
      when (io.out.req.fire()) { state := s_memReadResp }
    }
    is (s_memReadResp) {
      when (io.out.resp.fire() && io.out.resp.bits.isReadLast()) {
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
