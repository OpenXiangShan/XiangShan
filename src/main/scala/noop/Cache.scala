package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

sealed trait HasCacheConst {
  val TotalSize = 16 // Kbytes
  val LineSize = 32 // byte
  val LineBeats = LineSize / 4
  val Sets = TotalSize * 1024 / LineSize
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = 32 - OffsetBits - IndexBits
  val dataBits = 32

  val debug = false

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt(2.W)
  }

  def CacheArrayReqBus() = new ArrayReqBus(set = Sets)
  def CacheMetaArrayWriteBus() = new ArrayWriteBus(new MetaBundle, set = Sets)
  def CacheDataArrayWriteBus() = new ArrayWriteBus(new DataBundle, set = Sets, way = LineBeats)

  def maskExpand(m: UInt): UInt = Cat(m.toBools.map(Fill(8, _)).reverse)
  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed class MetaBundle extends Bundle with HasCacheConst {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())
}

sealed class MetaPipelineBundle extends Bundle with HasCacheConst {
  val tag = Output(UInt(TagBits.W))
  val hit = Output(Bool())
  val dirty = Output(Bool())
}

sealed class DataBundle extends Bundle with HasCacheConst {
  val data = Output(UInt(32.W))
}

sealed class Stage1IO(userBits: Int = 0) extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits = dataBits, userBits = userBits)

  override def cloneType = new Stage1IO(userBits).asInstanceOf[this.type]
}

// meta read
sealed class CacheStage1(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(dataBits, userBits)))
    val out = Decoupled(new Stage1IO(userBits))
    val metaReadReq = CacheArrayReqBus()
    val dataReadReq = CacheArrayReqBus()

    val s2Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s3Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s2s3Miss = Input(Bool())
    val metaReadOk = Input(Bool())
  })

  if (ro) when (io.in.fire()) { assert(!io.in.bits.wen) }

  // read meta array and data array
  List(io.metaReadReq, io.dataReadReq).map { case x => {
    x.valid := io.in.valid && io.out.ready
    x.idx := io.in.bits.addr.asTypeOf(addrBundle).index
  }}

  io.out.bits.req := io.in.bits

  val (addr, s2addr, s3addr) = (io.in.bits.addr, io.s2Req.bits.addr, io.s3Req.bits.addr)
  // set conflict will evict the dirty line, so we should wait
  // the victim line to be up-to-date, else we may writeback staled data
  val s2WriteSetConflict = io.s2Req.valid && isSetConflict(s2addr, addr) && io.s2Req.bits.wen
  val s3WriteSetConflict = io.s3Req.valid && isSetConflict(s3addr, addr) && io.s3Req.bits.wen
  val stall = s2WriteSetConflict || s3WriteSetConflict
  io.out.valid := io.in.valid && !stall && !io.s2s3Miss && io.metaReadOk
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadOk
}

sealed class Stage2IO(userBits: Int = 0) extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits, userBits)
  val meta = new MetaPipelineBundle

  override def cloneType = new Stage2IO(userBits).asInstanceOf[this.type]
}

// check
sealed class CacheStage2(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1IO(userBits)))
    val out = Decoupled(new Stage2IO(userBits))
    val metaReadResp = Flipped(new MetaBundle)
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.metaReadResp
  val dirty = if (ro) false.B else meta.dirty

  io.out.bits.meta.hit := meta.valid && (meta.tag === addr.tag) && io.in.valid
  io.out.bits.meta.tag := meta.tag
  io.out.bits.meta.dirty := dirty && io.in.valid
  io.out.bits.req <> io.in.bits.req

  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

// writeback
sealed class CacheStage3(ro: Boolean, name: String, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2IO(userBits)))
    val out = Decoupled(new SimpleBusRespBundle(dataBits = dataBits, userBits = userBits))
    val addr = Output(UInt(32.W))
    val flush = Input(Bool())
    val dataReadResp = Flipped(Vec(LineBeats, new DataBundle))
    val dataWriteReq = CacheDataArrayWriteBus()
    val metaWriteReq = CacheMetaArrayWriteBus()
    val mem = new AXI4
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val dataBlock = io.dataReadResp
  val meta = io.in.bits.meta
  val hit = io.in.valid && meta.hit
  val miss = io.in.valid && !meta.hit

  val dataBlockIdx = Wire(UInt(WordIndexBits.W))
  val dataRead = dataBlock(dataBlockIdx).data

  val wen = if (ro) false.B else req.wen
  val wmaskExpand = maskExpand(req.wmask)
  val wordMask = Mux(wen, wmaskExpand, 0.U(32.W))

  val dataHitWriteReq = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val metaHitWriteReq = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  if (!ro) {
    val update = hit && wen
    val dataMerge = (dataRead & ~wordMask) | (req.wdata & wordMask)
    dataHitWriteReq.req.valid := update
    dataHitWriteReq.req.idx := addr.index
    dataHitWriteReq.entry.data := dataMerge
    dataHitWriteReq.wordIndex := addr.wordIndex

    metaHitWriteReq.req.valid := update && !meta.dirty
    metaHitWriteReq.req.idx := addr.index
    metaHitWriteReq.entry.valid := true.B
    metaHitWriteReq.entry.tag := meta.tag
    metaHitWriteReq.entry.dirty := true.B
  }

  // if miss, access memory
  io.mem := DontCare
  List(io.mem.ar.bits, io.mem.aw.bits).map { a =>
    a.size := "b10".U
    a.id    := 0.U
    a.len   := (LineBeats - 1).U
    a.burst := AXI4Parameters.BURST_INCR
    a.lock  := false.B
    a.cache := 0.U
    a.qos   := 0.U
    a.user  := 0.U
  }

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val needFlush = Reg(Bool())
  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  io.mem.ar.valid := (state === s_memReadReq)

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(io.mem.aw.fire(), wSend)
  val wAck = BoolStopWatch(io.mem.w.fire() && io.mem.w.bits.last, wSend)
  wSend := (io.mem.aw.fire() && io.mem.w.fire() && io.mem.w.bits.last) || (awAck && wAck)

  io.mem.aw.valid := (state === s_memWriteReq) && !awAck
  io.mem. w.valid := (state === s_memWriteReq) && !wAck

  io.mem.ar.bits.addr := req.addr & ~(LineSize - 1).U(32.W)
  // dirty block addr
  io.mem.aw.bits.addr := Cat(meta.tag, addr.index, 0.U(OffsetBits.W))

  io.mem.r.ready := true.B
  io.mem.b.ready := true.B

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
  io.mem.w.bits.data := dataRead
  io.mem.w.bits.strb := 0xf.U
  io.mem.w.bits.last := (writeBeatCnt.value === (LineBeats - 1).U)

  dataBlockIdx := Mux(state === s_memWriteReq, writeBeatCnt.value, addr.wordIndex)

  val metaRefillWriteReq = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  val dataRefillWriteReq = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val inRdataRegDemand = Reg(UInt(32.W))

  switch (state) {
    is (s_idle) {
      // actually this can use s2 to test
      when (miss && !io.flush) { state := Mux(if (ro) false.B else meta.dirty, s_memWriteReq, s_memReadReq) }
    }
    is (s_memReadReq) { when (io.mem.ar.fire()) { state := s_memReadResp } }

    is (s_memReadResp) {
      when (io.mem.r.fire()) {
        val rdata = io.mem.r.bits.data
        when (readBeatCnt.value === addr.wordIndex) { inRdataRegDemand := rdata }

        val inRdata = if (!ro) {
          val rdataMergeWrite = (rdata & ~wordMask) | (req.wdata & wordMask)
          val rdataMergeWriteSel = (readBeatCnt.value === addr.wordIndex)
          Mux(rdataMergeWriteSel, rdataMergeWrite, rdata)
        } else rdata

        dataRefillWriteReq.entry.data := inRdata
        dataRefillWriteReq.wordIndex := readBeatCnt.value

        readBeatCnt.inc()
        when (io.mem.r.bits.last) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.w.fire()) { writeBeatCnt.inc() }
      when (wSend) { state := Mux(io.mem.b.fire(), s_memReadReq, s_memWriteResp) }
    }

    is (s_memWriteResp) { when (io.mem.b.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush) { state := s_idle } }
  }


  dataRefillWriteReq.req.valid := (state === s_memReadResp) && io.mem.r.fire()
  dataRefillWriteReq.req.idx := addr.index

  io.dataWriteReq := Mux(dataHitWriteReq.req.valid, dataHitWriteReq, dataRefillWriteReq)

  metaRefillWriteReq.req.valid := (state === s_memReadResp) && io.mem.r.fire() && io.mem.r.bits.last
  metaRefillWriteReq.req.idx := addr.index
  metaRefillWriteReq.entry.valid := true.B
  metaRefillWriteReq.entry.tag := addr.tag
  if (!ro) metaRefillWriteReq.entry.dirty := wen

  io.metaWriteReq := Mux(metaHitWriteReq.req.valid, metaHitWriteReq, metaRefillWriteReq)

  io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
  if (userBits > 0) {
    (io.out.bits.user zip io.in.bits.req.user).map{ case (x, y) => x := y }
  }
  io.out.valid := io.in.valid && Mux(hit, true.B, state === s_wait_resp)
  io.addr := req.addr
  io.in.ready := io.out.ready && (state === s_idle) && !miss

  if (name == "dcache" && debug) {
    printf("%d: cache stage3: in.ready = %d, in.valid = %d, state = %d, addr = %x\n",
      GTimer(), io.in.ready, io.in.valid, state, req.addr)
  }
}

class Cache(ro: Boolean, name: String, dataBits: Int = 32, userBits: Int = 0) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits, userBits))
    val addr = Output(UInt(32.W))
    val flush = Input(UInt(2.W))
    val mem = new AXI4
  })

  val s1 = Module(new CacheStage1(ro, name, userBits))
  val s2 = Module(new CacheStage2(ro, name, userBits))
  val s3 = Module(new CacheStage3(ro, name, userBits))
  val metaArray = Module(new ArrayTemplate(new MetaBundle, set = Sets, shouldReset = true, singlePort = true))
  val dataArray = Module(new ArrayTemplate(new DataBundle, set = Sets, way = LineBeats, shouldReset = true))

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire(), io.flush(1))
  io.in.resp <> s3.io.out

  s3.io.flush := io.flush(1)
  io.addr := s3.io.addr
  io.mem <> s3.io.mem

  // stalling
  s1.io.s2Req.valid := s2.io.in.valid
  s1.io.s2Req.bits := s2.io.in.bits.req
  s1.io.s3Req.valid := s3.io.in.valid
  s1.io.s3Req.bits := s3.io.in.bits.req
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.meta.hit

  metaArray.io.r.req <> s1.io.metaReadReq
  metaArray.io.w <> s3.io.metaWriteReq
  dataArray.io.r.req <> s1.io.dataReadReq
  dataArray.io.w <> s3.io.dataWriteReq
  s2.io.metaReadResp <> metaArray.io.r.entry
  s3.io.dataReadResp <> RegEnable(dataArray.io.r.entry, s2.io.out.fire())
  s1.io.metaReadOk := metaArray.io.r.isRrespOk()

  BoringUtils.addSource(s3.io.in.valid && s3.io.in.bits.meta.hit, "perfCntCondM" + name + "Hit")

  if (name == "dcache" && debug) {
    io.in.dump(name + ".in")
    printf("%d: s1:(%d,%d), s2:(%d,%d), s2:(%d,%d)\n",
      GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
    when (s1.io.in.valid) { printf(p"[S1]: ${s1.io.in.bits}\n") }
    when (s2.io.in.valid) { printf(p"[S2]: ${s2.io.in.bits.req}\n") }
    when (s3.io.in.valid) { printf(p"[S3]: ${s3.io.in.bits.req}\n") }
  }
}
