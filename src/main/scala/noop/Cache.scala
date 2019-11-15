package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.simplebus._
import bus.axi4._
import utils._

case class CacheConfig (
  ro: Boolean = false,
  name: String = "cache",
  userBits: Int = 0,
	cacheLevel: Int = 1,

  totalSize: Int = 32, // Kbytes
  ways: Int = 4
)

sealed trait HasCacheConst {
  implicit val cacheConfig: CacheConfig

  val AddrBits: Int
  val XLEN: Int

  val cacheName = cacheConfig.name
  val userBits = cacheConfig.userBits

  val ro = cacheConfig.ro
  val hasCoh = !ro
  val hasCohInt = (if (hasCoh) 1 else 0)
  val hasPrefetch = cacheName == "l2cache"
	
	val cacheLevel = cacheConfig.cacheLevel
  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = XLEN // byte
  val LineBeats = LineSize / 8 //DATA WIDTH 64
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
  def CacheDataArrayReadBus() = new SRAMReadBus(new DataBundle, set = Sets * LineBeats, way = Ways)
  def CacheMetaArrayWriteBus() = new SRAMWriteBus(new MetaBundle, set = Sets, way = Ways)
  def CacheDataArrayWriteBus() = new SRAMWriteBus(new DataBundle, set = Sets * LineBeats, way = Ways)

  def getMetaIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(addr.asTypeOf(addrBundle).index, addr.asTypeOf(addrBundle).wordIndex)

  def isSameWord(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) = (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class CacheBundle(implicit cacheConfig: CacheConfig) extends Bundle with HasNOOPParameter with HasCacheConst
sealed abstract class CacheModule(implicit cacheConfig: CacheConfig) extends Module with HasNOOPParameter with HasCacheConst

sealed class MetaBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
  val dirty = Output(Bool())

  def apply(tag: UInt, valid: Bool, dirty: Bool) = {
    this.tag := tag
    this.valid := valid
    this.dirty := dirty
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val data = Output(UInt(DataBits.W))

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

sealed class Stage1IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits)
}

// meta read
sealed class CacheStage1(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new SimpleBusReqBundle(userBits = userBits)))
    val out = Decoupled(new Stage1IO)
    val metaReadBus = CacheMetaArrayReadBus()
    val dataReadBus = CacheDataArrayReadBus()
  })

  if (ro) when (io.in.fire()) { assert(!io.in.bits.isWrite()) }
  Debug(){
    when(io.in.fire()){
      printf("[L1$] " +name+" cache stage1, addr in: %x, user: %x\n", io.in.bits.addr, io.in.bits.user.getOrElse(0.U))
    }
  }

  // read meta array and data array
  val readBusValid = io.in.valid && io.out.ready
  io.metaReadBus.apply(valid = readBusValid, setIdx = getMetaIdx(io.in.bits.addr))
  io.dataReadBus.apply(valid = readBusValid, setIdx = getDataIdx(io.in.bits.addr))

  io.out.bits.req := io.in.bits
  io.out.valid := io.in.valid && io.metaReadBus.req.ready && io.dataReadBus.req.ready
  io.in.ready := (!io.in.valid || io.out.fire()) && io.metaReadBus.req.ready && io.dataReadBus.req.ready

	Debug(debug) {
    printf("%d: [" + cacheName + " stage1]: in.ready = %d, in.valid = %d, out.valid = %d, out.ready = %d, addr = %x, cmd = %x, dataReadBus.req.valid = %d\n",
      GTimer(), io.in.ready, io.in.valid, io.out.valid, io.out.ready, io.in.bits.addr, io.in.bits.cmd, io.dataReadBus.req.valid)
  }
}

sealed class Stage2IO(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val req = new SimpleBusReqBundle(userBits = userBits)
  val metas = Vec(Ways, new MetaBundle)
  val datas = Vec(Ways, new DataBundle)
  val hit = Output(Bool())
  val waymask = Output(UInt(Ways.W))
  val mmio = Output(Bool())
  val isForwardData = Output(Bool())
  val forwardData = Output(CacheDataArrayWriteBus().req.bits)
}

// check
sealed class CacheStage2(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1IO))
    val out = Decoupled(new Stage2IO)
    val metaReadResp = Flipped(Vec(Ways, new MetaBundle))
    val dataReadResp = Flipped(Vec(Ways, new DataBundle))
    val metaWriteBus = Input(CacheMetaArrayWriteBus())
    val dataWriteBus = Input(CacheDataArrayWriteBus())
  })

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)

  val isForwardMeta = io.in.valid && io.metaWriteBus.req.valid && io.metaWriteBus.req.bits.setIdx === getMetaIdx(req.addr)
  val isForwardMetaReg = RegInit(false.B)
  when (isForwardMeta) { isForwardMetaReg := true.B }
  when (io.in.fire() || !io.in.valid) { isForwardMetaReg := false.B }
  val forwardMetaReg = RegEnable(io.metaWriteBus.req.bits, isForwardMeta)

  val metaWay = Wire(Vec(Ways, chiselTypeOf(forwardMetaReg.data)))
  forwardMetaReg.waymask.getOrElse("b1".U).asBools.zipWithIndex.map { case (w, i) =>
    metaWay(i) := Mux(isForwardMetaReg && w, forwardMetaReg.data, io.metaReadResp(i))
  }

  val hitVec = VecInit(metaWay.map(m => m.valid && (m.tag === addr.tag) && io.in.valid)).asUInt
  val victimWaymask = if (Ways > 1) (1.U << LFSR64()(log2Up(Ways)-1,0)) else "b1".U
  val waymask = Mux(io.out.bits.hit, hitVec, victimWaymask)
  assert(!(io.in.valid && PopCount(waymask) > 1.U))

  io.out.bits.metas := metaWay
  io.out.bits.hit := io.in.valid && hitVec.orR
  io.out.bits.waymask := waymask
  io.out.bits.datas := io.dataReadResp
  io.out.bits.mmio := AddressSpace.isMMIO(req.addr)

  val isForwardData = io.in.valid && (io.dataWriteBus.req match { case r =>
    r.valid && r.bits.setIdx === getDataIdx(req.addr)
  })
  val isForwardDataReg = RegInit(false.B)
  when (isForwardData) { isForwardDataReg := true.B }
  when (io.in.fire() || !io.in.valid) { isForwardDataReg := false.B }
  val forwardDataReg = RegEnable(io.dataWriteBus.req.bits, isForwardData)
  io.out.bits.isForwardData := isForwardDataReg
  io.out.bits.forwardData := forwardDataReg

  io.out.bits.req <> req
  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire()

	Debug(debug) {
    printf("%d: [" + cacheName + " stage2]: in.ready = %d, in.valid = %d, out.valid = %d, out.ready = %d, addr = %x, waymask = %d\n",
      GTimer(), io.in.ready, io.in.valid, io.out.valid, io.out.ready, req.addr, waymask)
  }
}

// writeback
sealed class CacheStage3(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2IO))
    val out = Decoupled(new SimpleBusRespBundle(userBits = userBits))
    val isFinish = Output(Bool())
    val flush = Input(Bool())
    val dataReadBus = CacheDataArrayReadBus()
    val dataWriteBus = CacheDataArrayWriteBus()
    val metaWriteBus = CacheMetaArrayWriteBus()

    val mem = new SimpleBusUC
    val mmio = new SimpleBusUC
    val cohResp = Decoupled(new SimpleBusRespBundle)

    // use to distinguish prefetch request and normal request
		val dataReadRespToL1 = Output(Bool())
  })

  val metaWriteArb = Module(new Arbiter(CacheMetaArrayWriteBus().req.bits, 2))
  val dataWriteArb = Module(new Arbiter(CacheDataArrayWriteBus().req.bits, 2))

  val req = io.in.bits.req
  val addr = req.addr.asTypeOf(addrBundle)
  val mmio = io.in.valid && io.in.bits.mmio
  val hit = io.in.valid && io.in.bits.hit
  val miss = io.in.valid && !io.in.bits.hit
  val probe = io.in.valid && hasCoh.B && req.isProbe()
	val hitReadBurst = hit && req.isReadBurst()
  val meta = Mux1H(io.in.bits.waymask, io.in.bits.metas)
  assert(!(mmio && hit), "MMIO request should not hit in cache")

  val useForwardData = io.in.bits.isForwardData && io.in.bits.waymask === io.in.bits.forwardData.waymask.getOrElse("b1".U)
  val dataReadArray = Mux1H(io.in.bits.waymask, io.in.bits.datas).data
  val dataRead = Mux(useForwardData, io.in.bits.forwardData.data.data, dataReadArray)
  val wordMask = Mux(!ro.B && req.isWrite(), MaskExpand(req.wmask), 0.U(DataBits.W))

	val writeL2BeatCnt = Counter(LineBeats)
	when(io.out.fire() && (req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast())) {
		writeL2BeatCnt.inc()
	}

  val hitWrite = hit && req.isWrite()
  val dataHitWriteBus = Wire(CacheDataArrayWriteBus()).apply(
    data = Wire(new DataBundle).apply(MaskData(dataRead, req.wdata, wordMask)),
    valid = hitWrite, setIdx = Cat(addr.index, Mux(req.cmd === SimpleBusCmd.writeBurst || req.isWriteLast(), writeL2BeatCnt.value, addr.wordIndex)), waymask = io.in.bits.waymask)

  val metaHitWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = hitWrite && !meta.dirty, setIdx = getMetaIdx(req.addr), waymask = io.in.bits.waymask,
    data = Wire(new MetaBundle).apply(tag = meta.tag, valid = true.B, dirty = (!ro).B)
  )

  val s_idle :: s_memReadReq :: s_memReadResp :: s_memWriteReq :: s_memWriteResp :: s_mmioReq :: s_mmioResp :: s_wait_resp :: s_release :: Nil = Enum(9)
  val state = RegInit(s_idle)
  val needFlush = RegInit(false.B)
  when (io.flush && (state =/= s_idle)) { needFlush := true.B }
  when (io.out.fire() && needFlush) { needFlush := false.B }

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
	
	val s2_idle :: s2_dataReadWait :: s2_dataOK :: Nil = Enum(3)
  val state2 = RegInit(s2_idle)

  io.dataReadBus.apply(valid = (state === s_memWriteReq || state === s_release) && (state2 === s2_idle),
    setIdx = Cat(addr.index, Mux(state === s_release, readBeatCnt.value, writeBeatCnt.value)))
  val dataWay = RegEnable(io.dataReadBus.resp.data, state2 === s2_dataReadWait)
  val dataHitWay = Mux1H(io.in.bits.waymask, dataWay).data

  switch (state2) {
    is (s2_idle) { when (io.dataReadBus.req.fire()) { state2 := s2_dataReadWait } }
    is (s2_dataReadWait) { state2 := s2_dataOK }
    is (s2_dataOK) { when (io.mem.req.fire() || io.cohResp.fire() || hitReadBurst && io.out.ready) { state2 := s2_idle } }
  }

  // critical word first read
  val raddr = (if (XLEN == 64) Cat(req.addr(AddrBits-1,3), 0.U(3.W))
                          else Cat(req.addr(AddrBits-1,2), 0.U(2.W)))
  // dirty block addr
  val waddr = Cat(meta.tag, addr.index, 0.U(OffsetBits.W))
  val cmd = Mux(state === s_memReadReq, SimpleBusCmd.readBurst,
    Mux((writeBeatCnt.value === (LineBeats - 1).U), SimpleBusCmd.writeLast, SimpleBusCmd.writeBurst))
  io.mem.req.bits.apply(addr = Mux(state === s_memReadReq, raddr, waddr),
    cmd = cmd, size = (if (XLEN == 64) "b11".U else "b10".U),
    wdata = dataHitWay, wmask = Fill(DataBytes, 1.U))

  io.mem.resp.ready := true.B
  io.mem.req.valid := (state === s_memReadReq) || ((state === s_memWriteReq) && (state2 === s2_dataOK))

  // mmio
  io.mmio.req.bits := req
  io.mmio.resp.ready := true.B
  io.mmio.req.valid := (state === s_mmioReq)

  val afterFirstRead = RegInit(false.B)
  val alreadyOutFire = RegEnable(true.B, init = false.B, io.out.fire())
  val readingFirst = !afterFirstRead && io.mem.resp.fire() && (state === s_memReadResp)
  val inRdataRegDemand = RegEnable(Mux(mmio, io.mmio.resp.bits.rdata, io.mem.resp.bits.rdata),
                                   Mux(mmio, state === s_mmioResp, readingFirst))

  // probe
  io.cohResp.valid := ((state === s_idle) && probe) ||
                      ((state === s_release) && (state2 === s2_dataOK))
  io.cohResp.bits.rdata := dataHitWay
  val releaseLast = Counter(state === s_release && io.cohResp.fire(), LineBeats)._2
	io.cohResp.bits.cmd := Mux(state === s_release, Mux(releaseLast, SimpleBusCmd.readLast, 0.U),
    Mux(hit, SimpleBusCmd.probeHit, SimpleBusCmd.probeMiss))
	
	val respToL1Fire = hitReadBurst && io.out.ready && state2 === s2_dataOK
	val respToL1Last = Counter((state === s_idle || state === s_release && state2 === s2_dataOK) && hitReadBurst && io.out.ready, LineBeats)._2
	
  switch (state) {
    is (s_idle) {
      afterFirstRead := false.B
      alreadyOutFire := false.B

      when (probe) {
        when (io.cohResp.fire()) {
          state := Mux(hit, s_release, s_idle)
          readBeatCnt.value := addr.wordIndex
        }
      }.elsewhen (hitReadBurst && io.out.ready) {
				state := s_release
				readBeatCnt.value := Mux(addr.wordIndex === (LineBeats - 1).U, 0.U, (addr.wordIndex + 1.U))
			}.elsewhen ((miss || mmio) && !io.flush) {
        state := Mux(mmio, s_mmioReq, Mux(!ro.B && meta.dirty, s_memWriteReq, s_memReadReq))
      }
    }

    is (s_mmioReq) { when (io.mmio.req.fire()) { state := s_mmioResp } }
    is (s_mmioResp) { when (io.mmio.resp.fire()) { state := s_wait_resp } }

    is (s_release) {
      when (io.cohResp.fire() || respToL1Fire) { readBeatCnt.inc() }
      when (probe && io.cohResp.fire() && releaseLast || respToL1Fire && respToL1Last) { state := s_idle }
		}

    is (s_memReadReq) { when (io.mem.req.fire()) {
      state := s_memReadResp
      readBeatCnt.value := addr.wordIndex
    }}

    is (s_memReadResp) {
      when (io.mem.resp.fire()) {
        afterFirstRead := true.B
        readBeatCnt.inc()
				when (req.cmd === SimpleBusCmd.writeBurst) { writeL2BeatCnt.value := 0.U }
        when (io.mem.resp.bits.isReadLast()) { state := s_wait_resp }
      }
    }

    is (s_memWriteReq) {
      when (io.mem.req.fire()) { writeBeatCnt.inc() }
      when (io.mem.req.bits.isWriteLast() && io.mem.req.fire()) { state := s_memWriteResp }
    }

    is (s_memWriteResp) { when (io.mem.resp.fire()) { state := s_memReadReq } }
    is (s_wait_resp) { when (io.out.fire() || needFlush || alreadyOutFire) { state := s_idle } }
  }

  val dataRefill = MaskData(io.mem.resp.bits.rdata, req.wdata, Mux(readingFirst, wordMask, 0.U(DataBits.W)))
  val dataRefillWriteBus = Wire(CacheDataArrayWriteBus).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire(), setIdx = Cat(addr.index, readBeatCnt.value),
    data = Wire(new DataBundle).apply(dataRefill), waymask = io.in.bits.waymask)

  dataWriteArb.io.in(0) <> dataHitWriteBus.req
  dataWriteArb.io.in(1) <> dataRefillWriteBus.req
  io.dataWriteBus.req <> dataWriteArb.io.out

  val metaRefillWriteBus = Wire(CacheMetaArrayWriteBus()).apply(
    valid = (state === s_memReadResp) && io.mem.resp.fire() && io.mem.resp.bits.isReadLast(),
    data = Wire(new MetaBundle).apply(valid = true.B, tag = addr.tag, dirty = !ro.B && req.isWrite()),
    setIdx = getMetaIdx(req.addr), waymask = io.in.bits.waymask
  )

  metaWriteArb.io.in(0) <> metaHitWriteBus.req
  metaWriteArb.io.in(1) <> metaRefillWriteBus.req
  io.metaWriteBus.req <> metaWriteArb.io.out

  if (cacheLevel == 2) {
		when ((state === s_memReadResp) && io.mem.resp.fire() && req.isReadBurst()) {
			// readBurst request miss
			io.out.bits.rdata := dataRefill
			io.out.bits.cmd := Mux(io.mem.resp.bits.isReadLast(), SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
		}.elsewhen (req.isWriteLast() || req.cmd === SimpleBusCmd.writeBurst) {
			// writeBurst/writeLast request, no matter hit or miss
			io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
			io.out.bits.cmd := DontCare
		}.elsewhen (hitReadBurst && state === s_release) {
			// readBurst request hit
			io.out.bits.rdata := dataHitWay
			io.out.bits.cmd := Mux(respToL1Last, SimpleBusCmd.readLast, SimpleBusCmd.readBurst)
		}.otherwise {
			io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
			io.out.bits.cmd := req.cmd
		}

		when (req.isBurst()) {
			io.out.valid := io.in.valid && (Mux(req.isWrite() && (hit || !hit && state === s_wait_resp), true.B, (state === s_memReadResp && io.mem.resp.fire() && req.cmd === SimpleBusCmd.readBurst)) || (respToL1Fire && respToL1Last && state === s_release))
		}.otherwise {
			io.out.valid := io.in.valid && Mux(probe, false.B, Mux(hit, true.B, Mux(req.isWrite() || mmio, state === s_wait_resp, afterFirstRead && !alreadyOutFire)))
		}

	} else {
		io.out.bits.rdata := Mux(hit, dataRead, inRdataRegDemand)
    io.out.bits.cmd := DontCare
    io.out.valid := io.in.valid && Mux(probe, false.B, Mux(hit, true.B, Mux(req.isWrite() || mmio, state === s_wait_resp, afterFirstRead && !alreadyOutFire)))
	}
  io.out.bits.user.zip(req.user).map { case (o,i) => o := i }

  // With critical-word first, the pipeline registers between
  // s2 and s3 can not be overwritten before a missing request
  // is totally handled. We use io.isFinish to indicate when the
  // request really ends.
  io.isFinish := Mux(probe, io.cohResp.fire() && Mux(miss, state === s_idle, (state === s_release) && releaseLast),
    Mux(hit || req.isWrite(), io.out.fire(), (state === s_wait_resp) && (io.out.fire() || alreadyOutFire))
  )

  io.in.ready := io.out.ready && (state === s_idle) && !miss && !probe
	io.dataReadRespToL1 := hitReadBurst && (state === s_idle && io.out.ready || state === s_release && state2 === s2_dataOK)

	assert(!(metaHitWriteBus.req.valid && metaRefillWriteBus.req.valid))
  assert(!(dataHitWriteBus.req.valid && dataRefillWriteBus.req.valid))
  assert(!(!ro.B && io.flush), "only allow to flush icache")
  Debug(debug) {
    printf("%d: [" + cacheName + " stage3]: in.ready = %d, in.valid = %d, out.valid = %d, out.ready = %d, state = %d, addr = %x, mem.req.valid = %d, mem.req.ready = %d\n\n",
      GTimer(), io.in.ready, io.in.valid, io.out.valid, io.out.ready, state, req.addr, io.mem.req.valid, io.mem.req.ready)
  }
}

class Cache(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC(userBits = userBits))
    val flush = Input(UInt(2.W))
    val out = new SimpleBusC
    val mmio = new SimpleBusUC
  })

  // cpu pipeline
  val s1 = Module(new CacheStage1)
  val s2 = Module(new CacheStage2)
  val s3 = Module(new CacheStage3)
  val metaArray = Module(new SRAMTemplateWithArbiter(nRead = 1, new MetaBundle, set = Sets, way = Ways, shouldReset = true))
  val dataArray = Module(new SRAMTemplateWithArbiter(nRead = 2, new DataBundle, set = Sets * LineBeats, way = Ways))

  if (cacheName == "icache") {
    // flush icache when executing fence.i
    val flushICache = WireInit(false.B)
    BoringUtils.addSink(flushICache, "MOUFlushICache")
    metaArray.reset := reset.asBool || flushICache
  }

  val arb = Module(new Arbiter(new SimpleBusReqBundle(userBits = userBits), hasCohInt + 1))
  arb.io.in(hasCohInt + 0) <> io.in.req

  s1.io.in <> arb.io.out
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire(), io.flush(0))
  PipelineConnect(s2.io.out, s3.io.in, s3.io.isFinish, io.flush(1) || s2.io.out.bits.mmio && s2.io.out.bits.req.isPrefetch())
  io.in.resp <> s3.io.out
  s3.io.flush := io.flush(1)
  io.out.mem <> s3.io.mem
  io.mmio <> s3.io.mmio

  io.in.resp.valid := Mux(s3.io.out.bits.isPrefetch(), false.B, s3.io.out.valid || s3.io.dataReadRespToL1)

  if (hasCoh) {
    val cohReq = io.out.coh.req.bits
    // coh does not have user signal, any better code?
    val coh = Wire(new SimpleBusReqBundle(userBits = userBits))
    coh.apply(addr = cohReq.addr, cmd = cohReq.cmd, size = cohReq.cmd, wdata = cohReq.wdata, wmask = cohReq.wmask)
    arb.io.in(0).bits := coh
    arb.io.in(0).valid := io.out.coh.req.valid
    io.out.coh.req.ready := arb.io.in(0).ready
    io.out.coh.resp <> s3.io.cohResp
  } else {
    io.out.coh.req.ready := true.B
    io.out.coh.resp := DontCare
    io.out.coh.resp.valid := false.B
    s3.io.cohResp.ready := true.B
  }

  metaArray.io.r(0) <> s1.io.metaReadBus
  dataArray.io.r(0) <> s1.io.dataReadBus
  dataArray.io.r(1) <> s3.io.dataReadBus

  metaArray.io.w <> s3.io.metaWriteBus
  dataArray.io.w <> s3.io.dataWriteBus

  s2.io.metaReadResp := s1.io.metaReadBus.resp.data
  s2.io.dataReadResp := s1.io.dataReadBus.resp.data
  s2.io.dataWriteBus := s3.io.dataWriteBus
  s2.io.metaWriteBus := s3.io.metaWriteBus

  BoringUtils.addSource(s3.io.in.valid && s3.io.in.bits.hit, "perfCntCondM" + cacheName + "Hit")

 	Debug(debug) {
    io.in.dump(cacheName + ".in")
    printf("%d: s1:(%d,%d), s2:(%d,%d), s3:(%d,%d)\n",
      GTimer(), s1.io.in.valid, s1.io.in.ready, s2.io.in.valid, s2.io.in.ready, s3.io.in.valid, s3.io.in.ready)
    when (s1.io.in.valid) { printf(p"[${cacheName}.S1]: ${s1.io.in.bits}\n") }
    when (s2.io.in.valid) { printf(p"[${cacheName}.S2]: ${s2.io.in.bits.req}\n") }
    when (s3.io.in.valid) { printf(p"[${cacheName}.S3]: ${s3.io.in.bits.req}\n") }
    s3.io.mem.dump(cacheName + ".mem")
  }
}

object Cache {
  def apply(in: SimpleBusUC, mmio: SimpleBusUC, flush: UInt, enable: Boolean = true)(implicit cacheConfig: CacheConfig) = {
    if (enable) {
      val cache = Module(new Cache)
      cache.io.flush := flush
      cache.io.in <> in
      mmio <> cache.io.mmio
      cache.io.out
    } else {
      val addrspace = List(AddressSpace.dram) ++ AddressSpace.mmio
      val xbar = Module(new SimpleBusCrossbar1toN(addrspace))
      val busC = WireInit(0.U.asTypeOf(new SimpleBusC))
      busC.mem <> xbar.io.out(0)
      xbar.io.in <> in
      mmio <> xbar.io.out(1)
      busC
    }
  }
}
