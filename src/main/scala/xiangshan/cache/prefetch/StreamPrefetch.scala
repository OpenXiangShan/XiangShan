package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class StreamPrefetchParameters(
  _type: String,
  streamCnt: Int,
  streamSize: Int,
  ageWidth: Int,
  blockBytes: Int
) {
  def streamWidth = log2Up(streamCnt)
  def idxWidth = log2Up(streamSize)
  def totalWidth = streamWidth + idxWidth
}

class StreamPrefetchReq(p: StreamPrefetchParameters) extends PrefetchReq {
  val id = UInt(p.totalWidth.W)

  def stream = id(p.totalWidth - 1, p.totalWidth - p.streamWidth)
  def idx = id(p.idxWidth - 1, 0)
}

class StreamPrefetchResp(p: StreamPrefetchParameters) extends PrefetchResp {
  val id = UInt(p.totalWidth.W)
  
  def stream = id(p.totalWidth - 1, p.totalWidth - p.streamWidth)
  def idx = id(p.idxWidth - 1, 0)
}

class StreamBufferUpdate(p: StreamPrefetchParameters) extends PrefetchBundle {
  val hitIdx = UInt(log2Up(p.streamSize).W)
}

class StreamBufferAlloc(p: StreamPrefetchParameters) extends StreamPrefetchReq(p)


class StreamBuffer(p: StreamPrefetchParameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val addrs = Vec(p.streamSize, ValidIO(UInt(PAddrBits.W)))
    val update = Flipped(ValidIO(new StreamBufferUpdate(p)))
    val alloc = Flipped(ValidIO(new StreamBufferAlloc(p)))
    // prefetch req
    val req = DecoupledIO(new StreamPrefetchReq(p))
    val resp = Flipped(DecoupledIO(new StreamPrefetchResp(p)))
  })

  def streamSize = p.streamSize
  def streamCnt = p.streamCnt
  def blockBytes = p.blockBytes

  val baseReq = RegInit(0.U.asTypeOf(Valid(new StreamPrefetchReq(p))))
  val nextReq = RegInit(0.U.asTypeOf(new StreamPrefetchReq(p)))
  val buf = RegInit(VecInit(Seq.fill(streamSize)(0.U.asTypeOf(new StreamPrefetchReq(p)))))
  val valid = RegInit(VecInit(Seq.fill(streamSize)(false.B)))
  val head = RegInit(0.U(log2Up(streamSize).W))
  val tail = RegInit(0.U(log2Up(streamCnt).W))
  val full = head === tail && valid(head)
  val empty = head === tail && !valid(head)

  val s_idle :: s_req :: s_resp :: Nil = Enum(4)
  val state = RegInit(VecInit(Seq.fill(streamSize)(s_idle)))

  val isPrefetching = VecInit(state.map(_ =/= s_idle))
  val deqLater = RegInit(VecInit(Seq.fill(streamSize)(false.B)))

  // dequeue
  val hitIdx = io.update.bits.hitIdx
  when (io.update.valid && !empty && valid(hitIdx)) {
    val headBeforehitIdx = head <= hitIdx && (hitIdx < tail || tail <= head)
    val hitIdxBeforeHead = hitIdx < tail && tail <= head
    when (headBeforehitIdx) {
      (0 until streamSize).foreach(i => deqLater(i) := Mux(i.U >= head && i.U <= hitIdx, true.B, deqLater(i)))
    }

    when (hitIdxBeforeHead) {
      (0 until streamSize).foreach(i => deqLater(i) := Mux(i.U >= head || i.U <= hitIdx, true.B, deqLater(i)))
    }
  }

  val deqValid = Wire(Vec(streamSize, Bool()))
  deqValid := DontCare
  deqValid(head) := deqLater(head) && !isPrefetching(head)
  for (i <- 1 until streamSize) {
    val idx = head + i.U
    deqValid(idx) := deqLater(idx) && !isPrefetching(idx) && deqValid(head + (i-1).U)
  }

  (0 until streamSize).foreach(i => valid(i) := valid(i) && !deqValid(i))
  (0 until streamSize).foreach(i => deqLater(i) := deqLater(i) && !deqValid(i))
  val nextHead = head + PopCount(deqValid)
  when (deqValid.asUInt.orR) {
    head := nextHead
    baseReq.valid := true.B
    baseReq.bits := buf(nextHead - 1.U)
  }

  // enqueue
  when (!full && baseReq.valid) {
    state(tail) := s_req
    tail := tail + 1.U
    buf(tail) := nextReq
    nextReq.addr := nextReq.addr + blockBytes.U
  }

  val reqs = Wire(Vec(streamSize, Decoupled(new StreamPrefetchReq(p))))
  val resps = Wire(Vec(streamSize, Decoupled(new StreamPrefetchResp(p))))
  (0 until streamSize).foreach{ i =>
    when (state(i) === s_req) {
      when (reqs(i).fire()) {
        state(i) := s_resp
      }
    }

    when (state(i) === s_resp) {
      when (resps(i).fire()) {
        state(i) := s_idle
        valid(i) := true.B
      }
    }

    reqs(i).valid := state(i) === s_req
    reqs(i).bits := buf(i)
    resps(i).ready := state(i) === s_resp
  }

  // send req sequentially
  val prefetchPrior = Wire(Vec(streamSize, UInt(log2Up(streamSize).W)))
  val reqArb = Module(new Arbiter(new StreamPrefetchReq(p), streamSize))
  for (i <- 0 until streamSize) {
    prefetchPrior := head + i.U
    reqs(prefetchPrior(i)) <> reqArb.io.in(i)
    resps(i).bits := io.resp.bits
    resps(i).valid := io.resp.valid && io.resp.bits.idx === i.U
  }
  reqArb.io.out <> io.req
  io.resp.ready := VecInit(resps.zipWithIndex.map{ case (r, i) =>
    r.ready && i.U === io.resp.bits.idx})
  
  // realloc this stream buffer for a newly-found stream
  val reallocReq = RegInit(0.U.asTypeOf(new StreamBufferAlloc(p)))
  val needRealloc = RegInit(false.B)
  when (io.alloc.valid) {
    needRealloc := true.B
    reallocReq := io.alloc.bits
  }.elsewhen (needRealloc && !isPrefetching.asUInt.orR) {
    baseReq.valid := true.B
    baseReq.bits := reallocReq
    nextReq.bits.write := reallocReq.write
    nextReq.bits.addr := reallocReq.addr + blockBytes.U
    head := 0.U
    tail := 0.U
    needRealloc := false.B
    valid.foreach(_ := false.B)
  }
}

class StreamPrefetch(p: StreamPrefetchParameters) extends PrefetchModule {
  val io = IO(new PrefetchIO)
  
  // TODO: implement this
  io.req.valid := false.B
  io.req.bits := DontCare
  io.resp.ready := true.B

  val streamBufs = Seq.fill(p.streamCnt) { Module(new StreamBuffer(p)) }
  
}
