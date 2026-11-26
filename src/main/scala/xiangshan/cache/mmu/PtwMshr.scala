/***************************************************************************************
* Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
*
* XiangShan is licensed under Mulan PSL v2
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE
***************************************************************************************/

package xiangshan.cache.mmu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

class PtwMshrFeedback(implicit p: Parameters) extends PtwBundle {
  val mshrID = UInt(log2Up(MissQueueSize).W)
  val retry = Bool()
  val isLLptw = Bool()
  val done = Bool()
}

class PtwMshrResume(implicit p: Parameters) extends PtwBundle {
  val mshrID = UInt(log2Up(MissQueueSize).W)
  val isLLptw = Bool()
}

class PtwMshrIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(Decoupled(new L2TlbInnerBundle))
  val lookup = Decoupled(new L2TlbInnerBundle)
  val lookupMshrID = Output(UInt(log2Up(MissQueueSize).W))
  val lookupFirst = Output(Bool())
  val ptwReady = Input(Bool())
  val llptwReady = Input(Bool())
  val mptReady = Input(Bool())
  val mptBlocked = Input(Vec(PtwWidth, Bool()))
  val feedback = Flipped(Valid(new PtwMshrFeedback))
  val resume = Flipped(Vec(2, Valid(new PtwMshrResume)))
  val complete = Flipped(Vec(2 + PtwWidth, Valid(UInt(log2Up(MissQueueSize).W))))
  val retire = Flipped(Vec(PtwWidth, Valid(new PtwRespS2)))
  val needed = Output(Vec(MissQueueSize, Bool()))
  val prefetchBusy = Output(Bool())
  val occupancy = Output(UInt(log2Up(MissQueueSize + 1).W))
  val full = Output(Bool())
}

class PtwMshr(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwMshrIO)
  private val flush = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed ||
    io.csr.hgatp.changed || io.csr.priv.virtChanged ||
    (if (HasMptCheck) io.csr.mmpt.changed else false.B)

  val requests = Reg(Vec(MissQueueSize, new L2TlbInnerBundle))
  val live = RegInit(VecInit.fill(MissQueueSize)(false.B))
  val owed = RegInit(VecInit.fill(MissQueueSize)(false.B))
  val pending = RegInit(VecInit.fill(MissQueueSize)(false.B))
  val busy = RegInit(VecInit.fill(MissQueueSize)(false.B))
  val first = Reg(Vec(MissQueueSize, Bool()))
  val retryToLlptw = Reg(Vec(MissQueueSize, Bool()))

  val free = live.map(!_)
  val allocate = ParallelPriorityEncoder(free)
  val sourceBusy = VecInit((0 until PtwWidth).map(source =>
    ParallelOR(requests.indices.map(index => live(index) && busy(index) && requests(index).source === source.U))))
  for (index <- requests.indices) {
    io.needed(index) := live(index) && (owed(index) || from_pre(requests(index).source))
  }
  val prefetchLive = requests.indices.map(index => live(index) && from_pre(requests(index).source))
  io.prefetchBusy := ParallelOR(prefetchLive)
  io.occupancy := PopCount(live)
  io.full := !ParallelOR(free)
  io.req.ready := !io.full && !flush

  val candidates = requests.indices.map { index =>
    val request = requests(index)
    val mptOnly = request.mptOnly.getOrElse(false.B)
    val source = request.source
    val sourceBlocked = source < PtwWidth.U && io.mptBlocked(source)
    val ready = Mux(mptOnly, io.mptReady && !sourceBusy(source),
      first(index) || Mux(retryToLlptw(index), io.llptwReady, io.ptwReady))
    live(index) && pending(index) && !sourceBlocked && ready
  }
  val selected = ParallelPriorityEncoder(candidates)
  io.lookup.valid := ParallelOR(candidates) && !flush
  io.lookup.bits := ParallelPriorityMux(candidates, requests.toSeq)
  io.lookupMshrID := selected
  io.lookupFirst := ParallelPriorityMux(candidates, first.toSeq)

  when (io.req.fire) {
    assert(!from_pre(io.req.bits.source) || !io.prefetchBusy, "MSHR allocates a second autonomous prefetch")
    requests(allocate) := io.req.bits
    live(allocate) := true.B
    owed(allocate) := !from_pre(io.req.bits.source)
    pending(allocate) := true.B
    busy(allocate) := false.B
    first(allocate) := true.B
    retryToLlptw(allocate) := false.B
  }
  when (io.lookup.fire) {
    pending(selected) := false.B
    busy(selected) := true.B
    first(selected) := false.B
  }

  when (io.feedback.valid && !flush) {
    val feedback = io.feedback.bits
    assert(live(feedback.mshrID) && busy(feedback.mshrID), "Cache feedback has no executing MSHR request")
    assert(!(feedback.retry && feedback.done), "Cache feedback both retries and completes a request")
    when (feedback.retry || feedback.done) { busy(feedback.mshrID) := false.B }
    when (feedback.retry) {
      pending(feedback.mshrID) := io.needed(feedback.mshrID)
      retryToLlptw(feedback.mshrID) := feedback.isLLptw
    }
  }
  for (resume <- io.resume) {
    when (resume.valid && !flush) {
      val mshrID = resume.bits.mshrID
      assert(live(mshrID) && busy(mshrID), "Walker handoff has no executing MSHR request")
      busy(mshrID) := false.B
      pending(mshrID) := io.needed(mshrID)
      retryToLlptw(mshrID) := resume.bits.isLLptw
    }
  }
  for (complete <- io.complete) {
    when (complete.valid && !flush) {
      assert(live(complete.bits) && busy(complete.bits), "Walker completion has no executing MSHR request")
      busy(complete.bits) := false.B
    }
  }

  // Clear exactly the responsibilities consumed by the actual I/D response handshake
  for (index <- requests.indices) {
    val incoming = io.req.fire && allocate === index.U
    val request = Mux(incoming, io.req.bits, requests(index))
    val consumed = io.retire.zipWithIndex.map { case (retire, source) =>
      val result = retire.bits
      retire.valid && request.source === source.U && request.s2xlate === result.s2xlate &&
        result.hit(request.vpn, io.csr.satp.asid, io.csr.vsatp.asid, io.csr.hgatp.vmid, allType = true)
    }
    when ((incoming || (live(index) && owed(index))) && ParallelOR(consumed)) {
      owed(index) := false.B
      pending(index) := false.B
    }
    // A retired request keeps its identity until its old Cache/walker operation drains
    when (live(index) && !owed(index) && !pending(index) && !busy(index)) {
      live(index) := false.B
    }
    assert(!busy(index) || live(index), "An execution outlives its MSHR identity")
    assert(!(pending(index) && busy(index)), "An executing request is also queued")
  }
  assert(PopCount(prefetchLive) <= 1.U, "MSHR holds more than one autonomous prefetch")

  when (flush) {
    live.foreach(_ := false.B)
    owed.foreach(_ := false.B)
    pending.foreach(_ := false.B)
    busy.foreach(_ := false.B)
  }
}
