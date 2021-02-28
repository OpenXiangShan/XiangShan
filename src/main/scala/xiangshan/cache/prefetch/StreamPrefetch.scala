package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class StreamPrefetchParameters(
  streamCnt: Int,
  streamSize: Int,
  ageWidth: Int,
  blockBytes: Int,
  reallocStreamOnMissInstantly: Boolean,
  cacheName: String // distinguish between different prefetchers
) {
  def streamWidth = log2Up(streamCnt)
  def idxWidth = log2Up(streamSize)
  def totalWidth = streamWidth + idxWidth
}

class StreamPrefetchReq(p: StreamPrefetchParameters) extends PrefetchReq {
  val id = UInt(p.totalWidth.W)

  def stream = id(p.totalWidth - 1, p.totalWidth - p.streamWidth)
  def idx = id(p.idxWidth - 1, 0)

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} w=${write} id=0x${Hexadecimal(id)} stream=${Binary(stream)} idxInAStream=${Binary(idx)}"
  }
  override def cloneType: this.type = (new StreamPrefetchReq(p)).asInstanceOf[this.type]
}

class StreamPrefetchResp(p: StreamPrefetchParameters) extends PrefetchResp {
  val id = UInt(p.totalWidth.W)
  
  def stream = id(p.totalWidth - 1, p.totalWidth - p.streamWidth)
  def idx = id(p.idxWidth - 1, 0)

  override def toPrintable: Printable = {
    p"id=0x${Hexadecimal(id)} stream=${Binary(stream)} idxInAStream=${Binary(idx)}"
  }
  override def cloneType: this.type = (new StreamPrefetchResp(p)).asInstanceOf[this.type]
}

class StreamPrefetchFinish(p: StreamPrefetchParameters) extends PrefetchFinish {
  val id = UInt(p.totalWidth.W)

  def stream = id(p.totalWidth - 1, p.totalWidth - p.streamWidth)
  def idx = id(p.idxWidth - 1, 0)

  override def toPrintable: Printable = {
    p"id=0x${Hexadecimal(id)} stream=${Binary(stream)} idxInAStream=${Binary(idx)}"
  }
  override def cloneType: this.type = (new StreamPrefetchFinish(p)).asInstanceOf[this.type]
}

class StreamPrefetchIO(p: StreamPrefetchParameters) extends PrefetchBundle {
  val train = Flipped(ValidIO(new PrefetchTrain))
  val req = DecoupledIO(new StreamPrefetchReq(p))
  val resp = Flipped(DecoupledIO(new StreamPrefetchResp(p)))
  val finish = DecoupledIO(new StreamPrefetchFinish(p))

  override def toPrintable: Printable = {
    p"train: v=${train.valid} ${train.bits} " +
      p"req: v=${req.valid} r=${req.ready} ${req.bits} " +
      p"resp: v=${resp.valid} r=${resp.ready} ${resp.bits}" +
      p"finish: v=${finish.valid} r=${finish.ready} ${finish.bits}"
  }
  override def cloneType: this.type = (new StreamPrefetchIO(p)).asInstanceOf[this.type]
}

class StreamBufferUpdate(p: StreamPrefetchParameters) extends PrefetchBundle {
  val hitIdx = UInt(log2Up(p.streamSize).W)

  override def toPrintable: Printable = { p"hitIdx=${hitIdx}" }
  override def cloneType: this.type = (new StreamBufferUpdate(p)).asInstanceOf[this.type]
}

class StreamBufferAlloc(p: StreamPrefetchParameters) extends StreamPrefetchReq(p) {
  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} w=${write} id=0x${Hexadecimal(id)} stream=${Binary(stream)} idxInAStream=${Binary(idx)}"
  }
  override def cloneType: this.type = (new StreamBufferAlloc(p)).asInstanceOf[this.type]
}


class StreamBuffer(p: StreamPrefetchParameters) extends PrefetchModule with HasTlbConst {
  val io = IO(new Bundle {
    val streamBufId = Input(UInt(log2Up(streamCnt).W))
    val addrs = Vec(p.streamSize, ValidIO(UInt(PAddrBits.W)))
    val update = Flipped(ValidIO(new StreamBufferUpdate(p)))
    val alloc = Flipped(ValidIO(new StreamBufferAlloc(p)))
    // prefetch req
    val req = DecoupledIO(new StreamPrefetchReq(p))
    val resp = Flipped(DecoupledIO(new StreamPrefetchResp(p)))
    val finish = DecoupledIO(new StreamPrefetchFinish(p))
  })

  def streamSize = p.streamSize
  def streamCnt = p.streamCnt
  def blockBytes = p.blockBytes
  // def getBlockAddr(addr: UInt) = addr & ~((blockBytes - 1).U(addr.getWidth.W))
  def getBlockAddr(addr: UInt) = Cat(addr(PAddrBits - 1, log2Up(p.blockBytes)), 0.U(log2Up(p.blockBytes).W))
  def getPageNum(addr: UInt) = addr(PAddrBits - 1, offLen)

  val baseReq = RegInit(0.U.asTypeOf(Valid(new PrefetchReq)))
  val nextReq = RegInit(0.U.asTypeOf(new PrefetchReq))
  val buf = RegInit(VecInit(Seq.fill(streamSize)(0.U.asTypeOf(new PrefetchReq))))
  val valid = RegInit(VecInit(Seq.fill(streamSize)(false.B)))
  val head = RegInit(0.U(log2Up(streamSize).W))
  val tail = RegInit(0.U(log2Up(streamSize).W))

  val s_idle :: s_req :: s_resp :: s_finish :: Nil = Enum(4)
  val state = RegInit(VecInit(Seq.fill(streamSize)(s_idle)))

  val isPrefetching = VecInit(state.map(_ =/= s_idle))
  val full = head === tail && (valid(head) || isPrefetching(head))
  val empty = head === tail && !(valid(head) || isPrefetching(head))
  val deqLater = RegInit(VecInit(Seq.fill(streamSize)(false.B)))

  val reallocReq = RegInit(0.U.asTypeOf(new StreamBufferAlloc(p)))
  val needRealloc = RegInit(false.B)

  // dequeue
  val hitIdx = io.update.bits.hitIdx
  when (io.update.valid && !empty && (isPrefetching(hitIdx) || valid(hitIdx))) {
    val headBeforehitIdx = head <= hitIdx && (hitIdx < tail || tail <= head)
    val hitIdxBeforeHead = hitIdx < tail && tail <= head
    when (headBeforehitIdx) {
      (0 until streamSize).foreach(i => deqLater(i) := Mux(i.U >= head && i.U <= hitIdx, true.B, deqLater(i)))
    }

    when (hitIdxBeforeHead) {
      (0 until streamSize).foreach(i => deqLater(i) := Mux(i.U >= head || i.U <= hitIdx, true.B, deqLater(i)))
    }
    
    XSDebug(io.update.valid && !empty && (isPrefetching(hitIdx) || valid(hitIdx)), p"hitIdx=${hitIdx} headBeforehitIdx=${headBeforehitIdx} hitIdxBeforeHead=${hitIdxBeforeHead}\n")
  }

  val deqValid = WireInit(VecInit(Seq.fill(streamSize)(false.B)))
  deqValid(head) := deqLater(head) && !isPrefetching(head)
  var deq = deqLater(head) && !isPrefetching(head)
  for (i <- 1 until streamSize) {
    val idx = head + i.U
    deq = deq && deqLater(idx) && !isPrefetching(idx)
    deqValid(idx) := deq
  }

  // (0 until streamSize).foreach(i => valid(i) := valid(i) && !deqValid(i))
  // (0 until streamSize).foreach(i => deqLater(i) := deqLater(i) && !deqValid(i))
  for (i <- 0 until streamSize) {
    when (deqValid(i)) {
      valid(i) := false.B
      deqLater(i) := false.B
    }
  }

  val nextHead = head + PopCount(deqValid)
  when (deqValid.asUInt.orR) {
    head := nextHead
    baseReq.valid := true.B
    baseReq.bits := buf(nextHead - 1.U)
  }

  // enqueue
  val nextAddrCrossPage = getPageNum(baseReq.bits.addr) =/= getPageNum(nextReq.addr)
  when (!full && baseReq.valid && !needRealloc) {
    when (!nextAddrCrossPage) {
      state(tail) := s_req
      tail := tail + 1.U
      buf(tail) := nextReq
      nextReq.addr := nextReq.addr + blockBytes.U
      XSDebug(p"enqueue 0x${nextReq.addr}\n")
    }.otherwise {
      XSDebug(p"addr 0x${nextReq.addr} could not enqueue for crossing pages\n")
    }
  }

  val reqs = Wire(Vec(streamSize, Decoupled(new StreamPrefetchReq(p))))
  val resps = Wire(Vec(streamSize, Decoupled(new StreamPrefetchResp(p))))
  val finishs = Wire(Vec(streamSize, DecoupledIO(new StreamPrefetchFinish(p))))
  (0 until streamSize).foreach{ i =>
    when (state(i) === s_req) {
      when (reqs(i).fire()) {
        state(i) := s_resp
      }
    }

    when (state(i) === s_resp) {
      when (resps(i).fire()) {
        state(i) := s_finish
      }
    }

    when (state(i) === s_finish) {
      when (finishs(i).fire()) {
        state(i) := s_idle
        valid(i) := true.B
      }
    }

    reqs(i).valid := state(i) === s_req
    reqs(i).bits.addr := buf(i).addr
    reqs(i).bits.write := buf(i).write
    reqs(i).bits.id := Cat(0.U(p.streamWidth.W), i.U(p.idxWidth.W))
    resps(i).ready := state(i) === s_resp
    finishs(i).valid := state(i) === s_finish
    finishs(i).bits.id := Cat(0.U(p.streamWidth.W), i.U(p.idxWidth.W))
  }

  // send req sequentially
  val prefetchPrior = Wire(Vec(streamSize, UInt(log2Up(streamSize).W)))
  val reqArb = Module(new Arbiter(new StreamPrefetchReq(p), streamSize))
  val finishArb = Module(new Arbiter(new StreamPrefetchFinish(p), streamSize))
  for (i <- 0 until streamSize) {
    prefetchPrior(i) := head + i.U
    reqArb.io.in(i) <> reqs(prefetchPrior(i))
    reqs(i).ready := DontCare
    finishArb.io.in(i) <> finishs(prefetchPrior(i))
    finishs(i).ready := DontCare
    resps(i).bits := io.resp.bits
    resps(i).valid := io.resp.valid && io.resp.bits.idx === i.U
  }
  for (i <- 0 until streamSize) {
    reqs(prefetchPrior(i)).ready := reqArb.io.in(i).ready
    finishs(prefetchPrior(i)).ready := finishArb.io.in(i).ready
  }
  io.req <> reqArb.io.out
  io.finish <> finishArb.io.out
  io.resp.ready := VecInit(resps.zipWithIndex.map{ case (r, i) => r.ready}).asUInt.orR
  
  // realloc this stream buffer for a newly-found stream
  when (io.alloc.valid) {
    needRealloc := true.B
    reallocReq := io.alloc.bits
    reallocReq.addr := getBlockAddr(io.alloc.bits.addr)
  }.elsewhen (needRealloc && !isPrefetching.asUInt.orR) {
    baseReq.valid := true.B
    baseReq.bits := reallocReq
    nextReq.write := reallocReq.write
    nextReq.addr := reallocReq.addr + blockBytes.U
    head := 0.U
    tail := 0.U
    needRealloc := false.B
    state.foreach(_ := s_idle)
    valid.foreach(_ := false.B)
    deqLater.foreach(_ := false.B)
  }

  for (i <- 0 until streamSize) {
    io.addrs(i).valid := baseReq.valid && (valid(i) || state(i) =/= s_idle)
    io.addrs(i).bits := getBlockAddr(buf(i).addr)
  }

  // debug info
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} io.req:    v=${io.req.valid} r=${io.req.ready} ${io.req.bits}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} io.resp:   v=${io.resp.valid} r=${io.resp.ready} ${io.resp.bits}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} io.finish: v=${io.finish.valid} r=${io.finish.ready} ${io.finish.bits}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} io.update: v=${io.update.valid} ${io.update.bits}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} io.alloc:  v=${io.alloc.valid} ${io.alloc.bits}\n")
  for (i <- 0 until streamSize) {
    XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} [${i.U}] io.addrs: ${io.addrs(i).valid} 0x${Hexadecimal(io.addrs(i).bits)} " +
      p"buf: ${buf(i)} valid: ${valid(i)} state: ${state(i)} isPfting: ${isPrefetching(i)} " +
      p"deqLater: ${deqLater(i)} deqValid: ${deqValid(i)}\n")
  }
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} head: ${head} tail: ${tail} full: ${full} empty: ${empty} nextHead: ${nextHead} blockBytes: ${blockBytes.U}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} baseReq: v=${baseReq.valid} ${baseReq.bits} nextReq: ${nextReq} crossPage: ${nextAddrCrossPage}\n")
  XSDebug(needRealloc, s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} needRealloc: ${needRealloc} reallocReq: ${reallocReq}\n")
  XSDebug(s"${p.cacheName} " + p"StreamBuf ${io.streamBufId} prefetchPrior: ")
  (0 until streamSize).foreach(i => XSDebug(false, true.B, p"${prefetchPrior(i)} "))
  XSDebug(false, true.B, "\n")
}

class CompareBundle(width: Int) extends PrefetchBundle {
  val bits = UInt(width.W)
  val idx = UInt()

  override def cloneType: this.type = (new CompareBundle(width)).asInstanceOf[this.type]
}

object ParallelMin {
  def apply[T <: Data](xs: Seq[CompareBundle]): CompareBundle = {
    ParallelOperation(xs, (a: CompareBundle, b: CompareBundle) => Mux(a.bits < b.bits, a, b))
  }
}

class StreamPrefetch(p: StreamPrefetchParameters) extends PrefetchModule {
  val io = IO(new StreamPrefetchIO(p))

  require(p.blockBytes > 0)
  
  // TODO: implement this
  def streamCnt = p.streamCnt
  def streamSize = p.streamSize
  def ageWidth = p.ageWidth
  // def getBlockAddr(addr: UInt) = addr & ~((p.blockBytes - 1).U(addr.getWidth.W))
  def getBlockAddr(addr: UInt) = Cat(addr(PAddrBits - 1, log2Up(p.blockBytes)), 0.U(log2Up(p.blockBytes).W))
  val streamBufs = Seq.fill(streamCnt) { Module(new StreamBuffer(p)) }
  val addrValids = Wire(Vec(streamCnt, Vec(streamSize, Bool())))
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      addrValids(i)(j) := streamBufs(i).io.addrs(j).valid
    }
  }
  val bufValids = WireInit(VecInit(addrValids.map(_.asUInt.orR)))
  val ages = Seq.fill(streamCnt)(RegInit(0.U(ageWidth.W)))
  val maxAge = -1.S(ageWidth.W).asUInt

  // assign default value
  for (i <- 0 until streamCnt) {
    streamBufs(i).io.streamBufId := i.U
    streamBufs(i).io.update.valid := false.B
    streamBufs(i).io.update.bits := DontCare
    streamBufs(i).io.alloc.valid := false.B
    streamBufs(i).io.alloc.bits := DontCare
  }

  // 1. streamBufs hit while l1i miss
  val hit = WireInit(false.B)
  val hitVec = WireInit(VecInit(Seq.fill(streamCnt * streamSize)(false.B)))
  for (i <- 0 until streamCnt) {
    for (j <- 0 until streamSize) {
      when (io.train.valid && addrValids(i)(j) && getBlockAddr(io.train.bits.addr) === streamBufs(i).io.addrs(j).bits) {
        // hit := true.B
        hitVec(i*streamSize+j) := true.B
        streamBufs(i).io.update.valid := true.B
        streamBufs(i).io.update.bits.hitIdx := j.U
        ages(i) := maxAge
      }
    }
  }
  hit := ParallelOR(hitVec)

  // 2. streamBufs miss
  val allocIdx = Wire(UInt(log2Up(streamCnt).W))
  val ageCmp = Seq.fill(streamCnt)(Wire(new CompareBundle(ageWidth)))
  (0 until streamCnt).foreach(i => ageCmp(i).bits := ages(i))
  (0 until streamCnt).foreach(i => ageCmp(i).idx := i.U)
  when ((~bufValids.asUInt).orR) {
    allocIdx := PriorityMux(~bufValids.asUInt, VecInit(List.tabulate(streamCnt)(_.U)))
  }.otherwise {
    allocIdx := ParallelMin(ageCmp).idx
  }
  when (!hit && io.train.valid) {
    (0 until streamCnt).foreach(i => ages(i) := Mux(ages(i) =/= 0.U, ages(i) - 1.U, 0.U))

    // realloc an invalid or the eldest stream buffer with new one
    for (i <- 0 until streamCnt) {
      streamBufs(i).io.alloc.valid := allocIdx === i.U
      streamBufs(i).io.alloc.bits := DontCare
      streamBufs(i).io.alloc.bits.addr := io.train.bits.addr
      streamBufs(i).io.alloc.bits.write := io.train.bits.write
      when (allocIdx === i.U) { ages(i) := maxAge }
    }
  }

  // 3. send reqs from streamBufs
  val reqArb = Module(new Arbiter(new StreamPrefetchReq(p), streamCnt))
  val finishArb = Module(new Arbiter(new StreamPrefetchFinish(p), streamCnt))
  for (i <- 0 until streamCnt) {
    reqArb.io.in(i).valid := streamBufs(i).io.req.valid
    reqArb.io.in(i).bits := streamBufs(i).io.req.bits
    reqArb.io.in(i).bits.id := Cat(i.U(p.streamWidth.W), streamBufs(i).io.req.bits.id(p.idxWidth - 1, 0))
    streamBufs(i).io.req.ready := reqArb.io.in(i).ready

    streamBufs(i).io.resp.valid := io.resp.valid && i.U === io.resp.bits.stream
    streamBufs(i).io.resp.bits := io.resp.bits

    finishArb.io.in(i).valid := streamBufs(i).io.finish.valid
    finishArb.io.in(i).bits.id := Cat(i.U(p.streamWidth.W), streamBufs(i).io.finish.bits.id(p.idxWidth - 1, 0))
    streamBufs(i).io.finish.ready := finishArb.io.in(i).ready
  }
  io.req <> reqArb.io.out
  io.finish <> finishArb.io.out
  io.resp.ready := VecInit(streamBufs.zipWithIndex.map { case (buf, i) =>  buf.io.resp.ready}).asUInt.orR
  
  // debug info
  XSDebug(s"${p.cacheName} " + p"io: ${io}\n")
  XSDebug(s"${p.cacheName} " + p"bufValids: ${Binary(bufValids.asUInt)} hit: ${hit} ages: ")
  (0 until streamCnt).foreach(i => XSDebug(false, true.B, p"${Hexadecimal(ages(i))} "))
  XSDebug(false, true.B, "\n")
}
