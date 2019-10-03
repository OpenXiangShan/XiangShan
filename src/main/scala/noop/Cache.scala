package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

sealed trait HasCacheConst {
  val AddrBits: Int
  val XLEN: Int

  val TotalSize = 32 // Kbytes
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA WIDTH 64
  val Ways = 1
  val Sets = TotalSize * 1024 / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats)
  val TagBits = AddrBits - OffsetBits - IndexBits

  val debug = false

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt((if (XLEN == 64) 3 else 2).W)
  }

  def CacheMetaArrayReadBus() = new SRAMReadBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets, way = Ways * LineBeats)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets, way = Ways * LineBeats)

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class CacheBundle extends Bundle with HasNOOPParameter with HasCacheConst
sealed abstract class CacheModule extends Module with HasNOOPParameter with HasCacheConst

sealed class MetaBundle extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())
}

sealed class MetaPipelineBundle extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val hit = Output(Bool())
  val dirty = Output(Bool())
}

sealed class DataBundle extends CacheBundle {
  val data = Output(UInt(DataBits.W))
}

sealed class Stage1IO(userBits: Int = 0) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits)

  override def cloneType = new Stage1IO(userBits).asInstanceOf[this.type]
}

// meta read
sealed class CacheStage1(ro: Boolean, name: String, userBits: Int = 0) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new Stage1IO(userBits))
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()

    val s2Req = Flipped(Valid(new SimpleBusReqBundle))
    val s3Req = Flipped(Valid(new SimpleBusReqBundle))
    val s2s3Miss = Input(Bool())
  })

  if (ro) when (io.in.fire()) { assert(!io.in.bits.isWrite()) }

  // read meta array and data array
  List(io.metaReadBus, io.dataReadBus).map { case x => {
    x.req.valid := io.in.valid && io.out.ready
    x.req.bits.idx := io.in.bits.addr.asTypeOf(addrBundle).index
  }}

  io.out.bits.req := io.in.bits

  val (addr, s2addr, s3addr) = (io.in.bits.addr, io.s2Req.bits.addr, io.s3Req.bits.addr)
  // set conflict will evict the dirty line, so we should wait
  // the victim line to be up-to-date, else we may writeback staled data
  val s2WriteSetConflict = io.s2Req.valid && isSetConflict(s2addr, addr) && io.s2Req.bits.isWrite()
  val s3WriteSetConflict = io.s3Req.valid && isSetConflict(s3addr, addr) && io.s3Req.bits.isWrite()
  val stall = s2WriteSetConflict || s3WriteSetConflict
  io.out.valid := io.in.valid && !stall && !io.s2s3Miss && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready
}

sealed class Stage2IO(userBits: Int = 0) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits)
  val meta = new MetaPipelineBundle

  override def cloneType = new Stage2IO(userBits).asInstanceOf[this.type]
}

// check
sealed class CacheStage2(ro: Boolean, name: String, userBits: Int = 0) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1IO(userBits)))
    val out = Decoupled(new Stage2IO(userBits))
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.metaReadResp(0)
  val dirty = if (ro) false.B else meta.dirty

  io.out.bits.meta.hit := meta.valid && (meta.tag === addr.tag) && io.in.valid
  io.out.bits.meta.tag := meta.tag
  io.out.bits.meta.dirty := dirty && io.in.valid
  io.out.bits.req <> io.in.bits.req
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()
}

// writeback
sealed class CacheStage3(ro: Boolean, name: String, userBits: Int = 0) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2IO(userBits)))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits))
    val isFinish = Output(Bool())
    val addr = Output(UInt(AddrBits.W))
    val flush = Input(Bool())
    val dataBlock = Flipped(Vec(Ways * LineBeats, new DataBundle))
    val dataWriteBus = CacheDataArrayWriteBus()
    val metaWriteBus = CacheMetaArrayWriteBus()
    val mem = new SimpleBusUC
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val meta = io.in.bits.meta
  val hit = io.in.valid && meta.hit
  val miss = io.in.valid && !meta.hit

  val dataBlockIdx = Wire(UInt(WordIndexBits.W))
  val dataRead = io.dataBlock(dataBlockIdx).data
  val wordMask = Mux(req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))

  val dataHitWriteBus = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val metaHitWriteBus = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  val hitWrite = hit && req.isWrite()
  val dataMerge = MaskData(dataRead, req.wdata, wordMask)
  dataHitWriteBus.req.valid := hitWrite
  dataHitWriteBus.req.bits.idx := addr.index
  dataHitWriteBus.req.bits.data.data := dataMerge
  dataHitWriteBus.req.bits.wordIndex := addr.wordIndex

  metaHitWriteBus.req.valid := hitWrite && !meta.dirty
  metaHitWriteBus.req.bits.idx := addr.index
  metaHitWriteBus.req.bits.data.valid := true.B
  metaHitWriteBus.req.bits.data.tag := meta.tag
  if (!ro) metaHitWriteBus.req.bits.data.dirty := true.B

  // if miss, access memory
  io.mem := DontCare
  List(io.mem.req.bits).map { a =>
    a.size := (if (XLEN == 64) "b11".U else "b10".U)
    a.user  := 0.U
  }

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_wait_resp :: Nil = Enum(6)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)
  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
  dataBlockIdx := Mux(state === s_memWriteReq, writeBeatCnt.value, addr.wordIndex)

  io.mem.req.bits.wdata := dataRead
  io.mem.req.bits.wmask := Fill(DataBytes, 1.U)
  io.mem.req.bits.cmd := Mux(state === s_memReadReq, SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))

  // critical word first
  val raddr = (if (XLEN == 64) Cat(req.addr(AddrBits-1,3), 0.U(3.W))
                          else Cat(req.addr(AddrBits-1,2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  io.mem.req.bits.addr := Mux(state === s_memReadReq, raddr, waddr)

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || (state === s_memWriteReq)

  val metaRefillWriteBus = WireInit(0.U.asTypeOf(CacheMetaArrayWriteBus()))
  val dataRefillWriteBus = WireInit(0.U.asTypeOf(CacheDataArrayWriteBus()))
  val afterFirstRead = RegInit(false.B)
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)
  val inRdataRegDemand = RegEnable(io.mem.resp.bits.rdata, readingFirst)

  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B

      // actually this can use s2 to test
      when (miss && !io.flush) { 
        state := Mux(if (ro) false.B else meta.dirty, s_memWriteReq, s_memReadReq) 
        }
    }
    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
      readBeatCnt.value := addr.wordIndex
    }}

    is (s_memReadResp) {
      when (io.mem.resp.fire()) {
        val rdata = io.mem.resp.bits.rdata
        afterFirstRead := true.B

        val inRdata = if (!ro) {
          val rdataMergeWrite = MaskData(rdata, req.wdata, wordMask)
          Mux(readingFirst, rdataMergeWrite, rdata)
        } else rdata

        dataRefillWriteBus.req.bits.data.data := inRdata
        dataRefillWriteBus.req.bits.wordIndex := readBeatCnt.value

        readBeatCnt.inc()
        when (io.mem.resp.bits.isReadLast()) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.req.fire()) { writeBeatCnt.inc() }
      when (io.mem.req.bits.isWriteLast()) { state := s_memWriteResp }
    }

    is (s_memWriteResp) { when (io.mem.resp.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire) { state := s_idle } }
  }


  dataRefillWriteBus.req.valid := (state === s_memReadResp) && io.mem.resp.fire()
  dataRefillWriteBus.req.bits.idx := addr.index

  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))
  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  metaRefillWriteBus.req.valid := (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast()
  metaRefillWriteBus.req.bits.idx := addr.index
  metaRefillWriteBus.req.bits.data.valid := true.B
  metaRefillWriteBus.req.bits.data.tag := addr.tag
  if (!ro) metaRefillWriteBus.req.bits.data.dirty := req.isWrite()

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
  io.out.bits.cmd := DontCare
  io.out.bits.user := io.in.bits.req.user
  io.out.valid := io.in.valid && Mux(hit, true.B, Mux(req.isWrite(), state === s_wait_resp, afterFirstRead && !alreadyOutFire))
  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.
  io.isFinish := Mux(hit || req.isWrite(), io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))

  io.addr := req.addr
  io.in.ready := io.out.ready && (state === s_idle) && !miss

  assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
  assert(!(dataHitWriteBus.req.valid && dataRefillWriteBus.req.valid))
  Debug(debug) {
    printf("%d: [" + name + " stage3]: in.ready = %d, in.valid = %d, state = %d, addr = %x\n",
      GTimer(), io.in.ready, io.in.valid, state, req.addr)
  }
}

// probe
sealed class CacheProbeStage(ro: Boolean, name: String) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle))
    val out = Decoupled(new SimpleBusRespBundle)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
    //val metaWriteBus = CacheMetaArrayWriteBus()
  })

  val s_idle :: s_arrayRead :: s_arrayReadWait :: s_check :: s_release :: Nil = Enum(5)
  val state = RegInit(s_idle)

  io.in.ready := (state === s_idle)
  val req = RegEnable(io.in.bits, io.in.fire())

  // read meta array and data array
  List(io.metaReadBus, io.dataReadBus).map { case x => {
    x.req.valid := (state === s_arrayRead)
    x.req.bits.idx := req.addr.asTypeOf(addrBundle).index
  }}

  // Latching meta and data
  val meta = RegEnable(io.metaReadBus.resp.data(0), state === s_arrayReadWait)
  val data = RegEnable(io.dataReadBus.resp.data, state === s_arrayReadWait)

  // check
  val addr = req.addr.asTypeOf(addrBundle)
  val hit = meta.valid && (meta.tag === addr.tag)

  // release
  val beatCnt = Counter(LineBeats)
  val idxCnt = Counter(LineBeats)
  val last = WireInit(false.B)

  switch (state) {
    is (s_idle) { when (io.in.fire()) { state := s_arrayRead } }
    is (s_arrayRead) {
      when (io.metaReadBus.req.ready && io.dataReadBus.req.ready) { state := s_arrayReadWait }
      assert(req.isProbe())
    }
    is (s_arrayReadWait) { state := s_check }
    is (s_check) {
      when (io.out.fire()) {
        state := Mux(hit, s_release, s_idle)
        idxCnt.value := addr.wordIndex
      }
    }
    is (s_release) {
      when (io.out.fire()) {
        idxCnt.inc()
        when (beatCnt.inc()) {
          state := s_idle
          last := true.B
        }
      }
    }
  }

  io.out.valid := (state === s_check) || (state === s_release)
  io.out.bits.rdata := data(idxCnt.value).data
  io.out.bits.user := 0.U
  io.out.bits.cmd := Mux(state === s_release, Mux(last, SimpleBusCmd.readLast, 0.U),
    Mux(hit, SimpleBusCmd.probeHit, SimpleBusCmd.probeMiss))

  // FIXME: should invalidate the meta array
}

class Cache(ro: Boolean, name: String, userBits: Int = 0) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits))
    val addr = Output(UInt(AddrBits.W))
    val flush = Input(UInt(2.W))
    val out = new SimpleBusC
  })

  // cpu pipeline
  val s1 = Module(new CacheStage1(ro, name, userBits))
  val s2 = Module(new CacheStage2(ro, name, userBits))
  val s3 = Module(new CacheStage3(ro, name, userBits))
  val metaArray = Module(new SRAMTemplate(new MetaBundle, set = Sets, way = Ways, shouldReset = true, singlePort = true))
  val dataArray = Module(new SRAMTemplate(new DataBundle, set = Sets, way = Ways * LineBeats, shouldReset = true, singlePort = true))

  if (name == "icache") {
    // flush icache when executing fence.i
    val flushICache = WireInit(false.B)
    BoringUtils.addSink(flushICache, "MOUFlushICache")
    metaArray.reset := reset.asBool || flushICache
  }

  s1.io.in <> io.in.req
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1))
  io.in.resp <> s3.io.out
  // printf("io.flush(0): %x io.flush(1): %x\n", io.flush(0), io.flush(1))
  s3.io.flush := io.flush(1)
  io.addr := s3.io.addr
  io.out.mem <> s3.io.mem

  // stalling
  s1.io.s2Req.valid := s2.io.in.valid
  s1.io.s2Req.bits := s2.io.in.bits.req
  s1.io.s3Req.valid := s3.io.in.valid
  s1.io.s3Req.bits := s3.io.in.bits.req
  s1.io.s2s3Miss := s3.io.in.valid && !s3.io.in.bits.meta.hit

  // coherence state machine
  val coh = Module(new CacheProbeStage(ro, name))
  coh.io.in <> io.out.coh.req
  io.out.coh.resp <> coh.io.out

  // Since SRAMTemplate are synchronous bus, read data will be returned
  // one cycle later read request is accepted. Therefore we do not need
  // to use ID bit to distribute the read data to the correct master.
  val metaReadArb = Module(new Arbiter(chiselTypeOf(metaArray.io.r.req.bits), 2))
  metaReadArb.io.in(0) <> coh.io.metaReadBus.req
  metaReadArb.io.in(1) <> s1.io.metaReadBus.req
  metaArray.io.r.req <> metaReadArb.io.out
  coh.io.metaReadBus.resp := metaArray.io.r.resp
  s1.io.metaReadBus.resp := metaArray.io.r.resp
  metaArray.io.w <> s3.io.metaWriteBus

  val dataReadArb = Module(new Arbiter(chiselTypeOf(dataArray.io.r.req.bits), 2))
  dataReadArb.io.in(0) <> coh.io.dataReadBus.req
  dataReadArb.io.in(1) <> s1.io.dataReadBus.req
  dataArray.io.r.req <> dataReadArb.io.out
  coh.io.dataReadBus.resp := dataArray.io.r.resp
  s1.io.dataReadBus.resp := dataArray.io.r.resp
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := metaArray.io.r.resp.data
  s3.io.dataBlock := RegEnable(dataArray.io.r.resp.data, s2.io.out.fire())

  BoringUtils.addSource(s3.io.in.valid && s3.io.in.bits.meta.hit, "perfCntCondM" + name + "Hit")

  Debug(debug) {
    io.in.dump(name + ".in")
    printf("%d: s1:(%d,%d), s2:(%d,%d), s3:(%d,%d)\n",
      GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
    when (s1.io.in.valid) { printf(p"[${name}.S1]: ${s1.io.in.bits}\n") }
    when (s2.io.in.valid) { printf(p"[${name}.S2]: ${s2.io.in.bits.req}\n") }
    when (s3.io.in.valid) { printf(p"[${name}.S3]: ${s3.io.in.bits.req}\n") }
    s3.io.mem.dump(name + ".mem")
  }
}
