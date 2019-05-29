package noop

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus
import bus.axi4._
import utils._

class Cache(ro: Boolean, name: String, dataBits: Int = 32,
  hasMMIO: Boolean = false, MMIOAddressSpace: List[(Long, Long)] = Nil) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits))
    val out = new AXI4
    val hit = Output(Bool())
    val mmio = new SimpleBus(dataBits)
  })

  val debug = false

  val TotalSize = 16 // Kbytes
  val LineSize = 64 // byte
  val LineBeats = LineSize / 4
  val Sets = TotalSize * 1024 / LineSize
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val TagBits = 32 - OffsetBits - IndexBits

  val addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val wordIndex = UInt((OffsetBits - 2).W)
    val byteOffset = UInt(2.W)
  }

  val metaBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val valid = Bool()
    val dirty = if (ro) None else Some(Bool())
  }
  val metaArray = Mem(Sets, UInt(metaBundle.getWidth.W))
  val dataArray = Mem(Sets, UInt((LineSize * 8).W))

  // should reset meta.valid
  val resetState = RegInit(true.B)
  val (resetIdx, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) {
    resetState := false.B
  }

  val s_idle :: s_metaRead :: s_outReadReq :: s_outReadResp :: s_outWriteReq :: s_outWriteResp :: s_metaWrite :: s_mmioReq :: s_mmioResp :: Nil = Enum(9)
  val state = RegInit(s_idle)

  // read metadata
  io.in.req.ready := (state === s_idle) && !resetState
  val metaReadEnable = io.in.req.fire() && (state === s_idle)
  val idx = io.in.req.bits.addr.asTypeOf(addrBundle).index

  val metaRead0 = metaArray.read(idx).asTypeOf(metaBundle)
  val metaRead = RegEnable(metaRead0, metaReadEnable)
  val dataRead = RegEnable(dataArray.read(idx), metaReadEnable)
  // reading SeqMem has 1 cycle latency, there tag should be compared in the next cycle
  // and the address should be latched
  val reqReg = RegEnable(io.in.req.bits, metaReadEnable)
  if (ro) when (metaReadEnable) { assert(!io.in.req.bits.wen) }
  val addrReg = reqReg.addr.asTypeOf(addrBundle)
  val mmio = if (!hasMMIO) false.B else MMIOAddressSpace.map(
    range => (io.in.req.bits.addr >= range._1.U && io.in.req.bits.addr < range._2.U)).reduce(_ || _)
  val hit = RegNext(metaRead0.valid && (io.in.req.bits.addr.asTypeOf(addrBundle).tag === metaRead0.tag) && !mmio)
  val dirty = metaRead.dirty.getOrElse(false.B)

    if (name == "dcache" && debug) {
      when (RegNext(metaReadEnable)) {
        printf("%d:@@@@@@@@@@@@@@@@@@@@@ dataArray[%d] -> 0x%x\n", GTimer(), addrReg.index, dataRead)
      }
    }

  // if miss, access memory
  val dirtyBlockAddr = Cat(metaRead.tag, addrReg.index, 0.U(OffsetBits.W))
  io.out := DontCare
  List(io.out.ar.bits, io.out.aw.bits).map { a =>
    a.size := "b10".U
    a.id    := 0.U
    a.len   := (LineBeats - 1).U
    a.burst := AXI4Parameters.BURST_INCR
    a.lock  := false.B
    a.cache := 0.U
    a.qos   := 0.U
    a.user  := 0.U
  }

  io.out.ar.valid := (state === s_outReadReq)

  val wSend = Wire(Bool())
  val awAck = BoolStopWatch(io.out.aw.fire(), wSend)
  val wAck = BoolStopWatch(io.out.w.fire() && io.out.w.bits.last, wSend)
  wSend := (io.out.aw.fire() && io.out.w.fire() && io.out.w.bits.last) || (awAck && wAck)

  io.out.aw.valid := (state === s_outWriteReq) && !awAck
  io.out. w.valid := (state === s_outWriteReq) && !wAck

  io.out.ar.bits.addr := reqReg.addr & ~(LineSize - 1).U(32.W)
  io.out.aw.bits.addr := dirtyBlockAddr

  io.out.r.ready := (state === s_outReadResp)
  io.out.b.ready := (state === s_outWriteResp)

  // refill
  val metaWriteEnable = !metaReadEnable && ((state === s_metaWrite) ||
    ((state === s_metaRead) && hit && reqReg.wen) )
  val metaWrite = Wire(metaBundle)
  val inRdataReg = Reg(Vec(LineBeats, UInt(32.W)))
  val retData = Mux(state === s_metaWrite, inRdataReg.asUInt, dataRead)
  // when burst is supported, should calculate the word index
  def wordShift(data: UInt, wordIndex: UInt, step: Int) = data << (wordIndex * step.U)
  val lineWmask = Cat(0.U((4 * (LineBeats - 1)).W), Mux(reqReg.wen, reqReg.wmask, 0.U(4.W)))
  val fullMask = wordShift(Cat(lineWmask.toBools.map(Mux(_, 0xff.U(8.W), 0x0.U(8.W))).reverse), addrReg.wordIndex, 32)(LineSize * 8 - 1, 0)
  val dataWrite = (retData & ~fullMask) | (wordShift(reqReg.wdata, addrReg.wordIndex, 32) & fullMask)

  metaWrite.tag := addrReg.tag
  metaWrite.valid := Mux(resetState, false.B, true.B)
  if (!ro) metaWrite.dirty.map(_ := reqReg.wen)
  when (metaWriteEnable || resetState) {
    metaArray.write(Mux(resetState, resetIdx, addrReg.index), metaWrite.asUInt)
    dataArray.write(addrReg.index, dataWrite)
    if (name == "dcache" && debug) {
      when (!resetState) {
        printf("%d: @@@@@@@@@@@@@@@@@@@@@ dataArray[%d] <- 0x%x\n", GTimer(), addrReg.index, dataWrite)
      }
    }
  }

  // mmio
  io.mmio.req.bits := reqReg
  io.mmio.req.valid := (state === s_mmioReq)
  io.mmio.resp.ready := (state === s_mmioResp)

  // return data
  io.in.resp.bits.rdata := Mux(io.mmio.resp.fire(), io.mmio.resp.bits.rdata, (if (dataBits == 512) retData else retData.asTypeOf(Vec(LineBeats, UInt(32.W)))(addrReg.wordIndex)))
  io.in.resp.valid := (hit && (state === s_metaRead)) || (state === s_metaWrite) || io.mmio.resp.fire()

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
  io.out.w.bits.data := dataRead.asTypeOf(Vec(LineBeats, UInt(32.W)))(writeBeatCnt.value)
  io.out.w.bits.strb := 0xf.U
  io.out.w.bits.last := (writeBeatCnt.value === (LineBeats - 1).U)

  switch (state) {
    is (s_idle) {
      when (io.in.req.fire()) { state := Mux(mmio, s_mmioReq, s_metaRead) }
    }

    is (s_metaRead) {
      state := Mux(hit, s_idle, Mux(metaRead.valid && dirty, s_outWriteReq, s_outReadReq))
    }

    is (s_outReadReq) {
      when (io.out.ar.fire()) { state := s_outReadResp }
    }

    is (s_outReadResp) {
      when (io.out.r.fire()) {
        inRdataReg(readBeatCnt.value) := io.out.r.bits.data
        readBeatCnt.inc()
        when (io.out.r.bits.last) { state := s_metaWrite }
      }
    }

    is (s_outWriteReq) {
      when (io.out.w.fire()) { writeBeatCnt.inc() }
      when (wSend) { state := s_outWriteResp }
    }

    is (s_outWriteResp) {
      when (io.out.b.fire()) { state := s_outReadReq }
    }

    is (s_metaWrite) { state := s_idle }

    is (s_mmioReq) { when (io.mmio.req.fire()) { state := s_mmioResp } }
    is (s_mmioResp) { when (io.mmio.resp.fire()) { state := s_idle } }
  }

  // perfcnt
  io.hit := hit && (state === s_metaRead)

  if (debug) {
    io.in.dump(name + ".in")
  }
}
