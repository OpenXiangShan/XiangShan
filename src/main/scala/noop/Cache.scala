package noop

import chisel3._
import chisel3.util._

import bus.simplebus._
import bus.axi4._
import utils._

sealed trait HasCacheConst {
  val TotalSize = 16 // Kbytes
  val LineSize = 64 // byte
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

  def wordShift(data: UInt, wordIndex: UInt, step: Int) = (data << (wordIndex * step.U))
  def maskExpand(m: UInt): UInt = Cat(m.toBools.map(Fill(8, _)).reverse)
  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).tag =/= a2.asTypeOf(addrBundle).tag) &&
    (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed class MetaBundle extends Bundle with HasCacheConst {
  val tag = UInt(TagBits.W)
  val valid = Bool()
  val dirty = Bool()
}

sealed class MetaPipelineBundle extends Bundle with HasCacheConst {
  val tag = Output(UInt(TagBits.W))
  val hit = Output(Bool())
  val dirty = Output(Bool())
}

sealed class MetaReadReqIO extends Bundle with HasCacheConst {
  val idx = Output(UInt(IndexBits.W))
}

sealed class MetaReadRespIO extends Bundle with HasCacheConst {
  val meta = Output(new MetaBundle)
}

sealed class MetaReadBus extends Bundle {
  val req = new MetaReadReqIO
  val resp = Flipped(new MetaReadRespIO)
}

sealed class MetaWriteBus extends Bundle with HasCacheConst {
  val req = Valid(new Bundle {
    val idx = Output(UInt(IndexBits.W))
    val meta = Input(new MetaBundle)
  })
}

sealed class MetaArray extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val read = Flipped(new MetaReadBus)
    val write = Flipped(new MetaWriteBus)
    val finishReset = Output(Bool())
  })

  val array = Mem(Sets, UInt((new MetaBundle).getWidth.W))

  // should reset meta.valid
  val resetState = RegInit(true.B)
  val (resetIdx, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) { resetState := false.B }

  when (io.write.req.valid || resetState) {
    array.write(Mux(resetState, resetIdx, io.write.req.bits.idx),
                Mux(resetState, 0.U, io.write.req.bits.meta.asUInt))
  }

  io.read.resp.meta := array.read(io.read.req.idx).asTypeOf(new MetaBundle)
  io.finishReset := !resetState
}

sealed class DataReadReqIO extends MetaReadReqIO
sealed class DataReadRespIO extends Bundle with HasCacheConst {
  val data = Output(Vec(LineBeats, UInt(32.W)))
}

sealed class DataReadBus extends Bundle {
  val req = Valid(new DataReadReqIO)
  val resp = Flipped(new DataReadRespIO)
}

// we only support 32-bit write to save resource
sealed class DataWriteBus extends Bundle with HasCacheConst {
  val req = Valid(new Bundle {
    val idx = Output(UInt(IndexBits.W))
    val data = Output(UInt(32.W))
    val wordIndex = Output(UInt(WordIndexBits.W))
  })
}

sealed class DataArray extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val read = Flipped(new DataReadBus)
    val write = Flipped(new DataWriteBus)
    val s2OutFire = Input(Bool())
  })

  val array = Mem(Sets, chiselTypeOf(io.read.resp.data))

  val req = io.write.req.bits
  val dataBlock = wordShift(req.data, req.wordIndex, 32).asTypeOf(Vec(LineBeats, UInt(32.W)))
  when (io.write.req.valid) {
    array.write(req.idx, dataBlock, (1.U << req.wordIndex).toBools)
  }

  io.read.resp.data := RegEnable(RegEnable(array.read(io.read.req.bits.idx), io.read.req.valid), io.s2OutFire)
}


sealed class Stage1IO extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits)
  val meta = new MetaReadRespIO
}

// meta read
sealed class CacheStage1(ro: Boolean, name: String) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(dataBits)))
    val out = Decoupled(new Stage1IO)
    val metaRead = new MetaReadBus
    val dataReadReq = Valid(new DataReadReqIO)

    val s2Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s3Req = Flipped(Valid(new SimpleBusReqBundle(dataBits)))
    val s2s3Miss = Input(Bool())
    val metaFinishReset = Input(Bool())
  })

  if (ro) when (io.in.fire()) { assert(!io.in.bits.wen) }
  val idx = io.in.bits.addr.asTypeOf(addrBundle).index

  // read meta array
  io.metaRead.req.idx := idx
  io.out.bits.meta := io.metaRead.resp
  if (ro) io.out.bits.meta.meta.dirty := false.B

  // read data array
  io.dataReadReq.valid := io.out.fire()
  io.dataReadReq.bits.idx := idx

  io.out.bits.req := io.in.bits

  val (addr, s2addr, s3addr) = (io.in.bits.addr, io.s2Req.bits.addr, io.s3Req.bits.addr)
  val s2WriteSameWord = io.s2Req.valid && isSameWord(s2addr, addr) && io.s2Req.bits.wen
  val s3WriteSameWord = io.s3Req.valid && isSameWord(s3addr, addr) && io.s3Req.bits.wen
  // set conflict will evict the dirty line, so we should wait
  // the victim line to be up-to-date, else we may writeback staled data
  val s2WriteSetConflict = io.s2Req.valid && isSetConflict(s2addr, addr) && io.s2Req.bits.wen
  val s3WriteSetConflict = io.s3Req.valid && isSetConflict(s3addr, addr) && io.s3Req.bits.wen
  val stall = s2WriteSameWord || s3WriteSameWord || s2WriteSetConflict || s3WriteSetConflict
  io.out.valid := io.in.valid && !stall && !io.s2s3Miss && io.metaFinishReset
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaFinishReset
}

sealed class Stage2IO extends Bundle with HasCacheConst {
  val req = new SimpleBusReqBundle(dataBits)
  val meta = new MetaPipelineBundle
}

// check
sealed class CacheStage2(ro: Boolean, name: String) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1IO))
    val out = Decoupled(new Stage2IO)
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.in.bits.meta.meta
  val dirty = if (ro) false.B else meta.dirty

  io.out.bits.meta.hit := meta.valid && (meta.tag === addr.tag) && io.in.valid
  io.out.bits.meta.tag := meta.tag
  io.out.bits.meta.dirty := dirty && io.in.valid
  io.out.bits.req <> io.in.bits.req

  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

// writeback
sealed class CacheStage3(ro: Boolean, name: String) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2IO))
    val out = Decoupled(new SimpleBusRespBundle(dataBits))
    val addr = Output(UInt(32.W))
    val flush = Input(Bool())
    val dataReadResp = Flipped(new DataReadRespIO)
    val dataWrite = new DataWriteBus
    val metaWrite = new MetaWriteBus
    val mem = new AXI4
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val dataBlock = io.dataReadResp.data
  val meta = io.in.bits.meta
  val hit = io.in.valid && meta.hit
  val miss = io.in.valid && !meta.hit

  val dataBlockIdx = Wire(UInt(WordIndexBits.W))
  val dataRead = dataBlock(dataBlockIdx)

  val wen = if (ro) false.B else req.wen
  val wmaskExpand = maskExpand(req.wmask)
  val wordMask = Mux(wen, wmaskExpand, 0.U(32.W))

  val dataHitWrite = WireInit(0.U.asTypeOf(new DataWriteBus))
  val metaHitWrite = WireInit(0.U.asTypeOf(new MetaWriteBus))
  if (!ro) {
    val update = hit && wen
    val dataMerge = (dataRead & ~wordMask) | (req.wdata & wordMask)
    dataHitWrite.req.valid := update
    dataHitWrite.req.bits.idx := addr.index
    dataHitWrite.req.bits.data := dataMerge
    dataHitWrite.req.bits.wordIndex := addr.wordIndex

    metaHitWrite.req.valid := update && !meta.dirty
    metaHitWrite.req.bits.idx := addr.index
    metaHitWrite.req.bits.meta.valid := true.B
    metaHitWrite.req.bits.meta.tag := meta.tag
    metaHitWrite.req.bits.meta.dirty := true.B
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

  val metaRefillWrite = WireInit(0.U.asTypeOf(new MetaWriteBus))
  val dataRefillWrite = WireInit(0.U.asTypeOf(new DataWriteBus))
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

        dataRefillWrite.req.bits.data := inRdata
        dataRefillWrite.req.bits.wordIndex := readBeatCnt.value

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


  dataRefillWrite.req.valid := (state === s_memReadResp) && io.mem.r.fire()
  dataRefillWrite.req.bits.idx := addr.index

  io.dataWrite := Mux(dataHitWrite.req.valid, dataHitWrite, dataRefillWrite)

  metaRefillWrite.req.valid := (state === s_memReadResp) && io.mem.r.fire() && io.mem.r.bits.last
  metaRefillWrite.req.bits.idx := addr.index
  metaRefillWrite.req.bits.meta.valid := true.B
  metaRefillWrite.req.bits.meta.tag := addr.tag
  if (!ro) metaRefillWrite.req.bits.meta.dirty := wen

  io.metaWrite := Mux(metaHitWrite.req.valid, metaHitWrite, metaRefillWrite)

  io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
  io.out.valid := io.in.valid && Mux(hit, true.B, state === s_wait_resp)
  io.addr := req.addr
  io.in.ready := io.out.ready && (state === s_idle) && !miss

  if (name == "dcache" && debug) {
    printf("%d: cache stage3: in.ready = %d, in.valid = %d, state = %d, addr = %x\n",
      GTimer(), io.in.ready, io.in.valid, state, req.addr)
  }
}

class Cache(ro: Boolean, name: String, dataBits: Int = 32) extends Module with HasCacheConst {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits))
    val addr = Output(UInt(32.W))
    val flush = Input(Bool())
    val mem = new AXI4
    val hit = Output(Bool())
  })

  val s1 = Module(new CacheStage1(ro, name))
  val s2 = Module(new CacheStage2(ro, name))
  val s3 = Module(new CacheStage3(ro, name))
  val metaArray = Module(new MetaArray)
  val dataArray = Module(new DataArray)

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush)
  PipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire(), io.flush)
  io.in.resp <> s3.io.out

  s3.io.flush := io.flush
  io.addr := s3.io.addr
  io.mem <> s3.io.mem

  // stalling
  s1.io.s2Req.valid := s2.io.in.valid
  s1.io.s2Req.bits := s2.io.in.bits.req
  s1.io.s3Req.valid := s3.io.in.valid
  s1.io.s3Req.bits := s3.io.in.bits.req
  s1.io.s2s3Miss := (s2.io.in.valid && !s2.io.out.bits.meta.hit) || (s3.io.in.valid && !s3.io.in.bits.meta.hit)

  metaArray.io.read <> s1.io.metaRead
  metaArray.io.write <> s3.io.metaWrite
  dataArray.io.read.req <> s1.io.dataReadReq
  dataArray.io.write <> s3.io.dataWrite
  dataArray.io.s2OutFire := s2.io.out.fire()
  s3.io.dataReadResp <> dataArray.io.read.resp
  s1.io.metaFinishReset := metaArray.io.finishReset

  // perfcnt
  io.hit := s3.io.in.valid && s3.io.in.bits.meta.hit

  if (name == "dcache" && debug) {
    io.in.dump(name + ".in")
    printf("%d: s1:(%d,%d), s2:(%d,%d), s2:(%d,%d)\n",
      GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
    when (s1.io.in.valid) { printf(p"[S1]: ${s1.io.in.bits}\n") }
    when (s2.io.in.valid) { printf(p"[S2]: ${s2.io.in.bits.req}\n") }
    when (s3.io.in.valid) { printf(p"[S3]: ${s3.io.in.bits.req}\n") }
  }
}
