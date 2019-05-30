package noop

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus
import bus.axi4._
import utils._

class Cache(ro: Boolean, name: String, dataBits: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus(dataBits))
    val out = new AXI4
    val hit = Output(Bool())
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
  val dataArray = Mem(Sets, Vec(LineBeats, UInt(32.W)))

  // should reset meta.valid
  val resetState = RegInit(true.B)
  val (resetIdx, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) {
    resetState := false.B
  }

  val s_idle :: s_metaRead :: s_outReadReq :: s_outReadResp :: s_outWriteReq :: s_outWriteResp :: s_metaWrite :: Nil = Enum(7)
  val state = RegInit(s_idle)

  // read metadata
  io.in.req.ready := (state === s_idle) && !resetState
  val metaReadEnable = io.in.req.fire() && (state === s_idle)
  val idx = io.in.req.bits.addr.asTypeOf(addrBundle).index

  val metaRead0 = metaArray.read(idx).asTypeOf(metaBundle)
  val metaRead = RegEnable(metaRead0, metaReadEnable)

  // reading SeqMem has 1 cycle latency, there tag should be compared in the next cycle
  // and the address should be latched
  val reqReg = RegEnable(io.in.req.bits, metaReadEnable)
  if (ro) when (metaReadEnable) { assert(!io.in.req.bits.wen) }
  val addrReg = reqReg.addr.asTypeOf(addrBundle)
  val hit = RegNext(metaRead0.valid && (io.in.req.bits.addr.asTypeOf(addrBundle).tag === metaRead0.tag))
  val dirty = metaRead.dirty.getOrElse(false.B)

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
  val reqWen = if (ro) false.B else reqReg.wen
  val metaWriteEnable = !metaReadEnable && ((state === s_metaWrite) || ((state === s_metaRead) && hit && reqWen))
  val metaWrite = Wire(metaBundle)
  val inRdataReg = Reg(Vec(LineBeats, UInt(32.W)))
  val inRdataRegDemand = Reg(UInt(32.W))

  val dataReadBlock = RegEnable(dataArray.read(idx), metaReadEnable)
  val dataRead = if (dataBits == 512) Cat(dataReadBlock.reverse) else dataReadBlock(addrReg.wordIndex)
  val retData = Mux(state === s_metaWrite, (if (dataBits == 512) Cat(inRdataReg.reverse) else inRdataRegDemand), dataRead)

  def maskExpand(m: UInt): UInt = Cat(m.toBools.map(Fill(8, _)).reverse)
  val wmaskExpand = maskExpand(reqReg.wmask)

  if (!ro) {
    // when burst is supported, should calculate the word index
    def wordShift(data: UInt, wordIndex: UInt, step: Int) = (data << (wordIndex * step.U))

    val fullMask = wordShift(maskExpand(Mux(reqWen, reqReg.wmask, 0.U(4.W))), addrReg.wordIndex, 32)
    val dataWriteHitEnable = ~metaReadEnable && ((state === s_metaRead) && hit) && reqWen
    val dataWriteHitBlock = (Cat(dataReadBlock.reverse) & ~fullMask) | (wordShift(reqReg.wdata, addrReg.wordIndex, 32) & fullMask)
    when (dataWriteHitEnable) {
      dataArray.write(addrReg.index, dataWriteHitBlock.asTypeOf(Vec(LineBeats, UInt(32.W))))
    }
  }

  metaWrite.tag := addrReg.tag
  metaWrite.valid := Mux(resetState, false.B, true.B)
  if (!ro) metaWrite.dirty.map(_ := reqWen)
  when (metaWriteEnable || resetState) {
    metaArray.write(Mux(resetState, resetIdx, addrReg.index), metaWrite.asUInt)
  }

  // return data
  io.in.resp.bits.rdata := retData
  io.in.resp.valid := (hit && (state === s_metaRead)) || (state === s_metaWrite)

  val readBeatCnt = Counter(LineBeats)
  val writeBeatCnt = Counter(LineBeats)
  io.out.w.bits.data := dataReadBlock(writeBeatCnt.value)
  io.out.w.bits.strb := 0xf.U
  io.out.w.bits.last := (writeBeatCnt.value === (LineBeats - 1).U)

  switch (state) {
    is (s_idle) { when (io.in.req.fire()) { state := s_metaRead } }
    is (s_metaRead) { state := Mux(hit, s_idle, Mux(metaRead.valid && dirty, s_outWriteReq, s_outReadReq)) }
    is (s_outReadReq) { when (io.out.ar.fire()) { state := s_outReadResp } }

    is (s_outReadResp) {
      when (io.out.r.fire()) {
        val rdata = io.out.r.bits.data
        if (dataBits != 512) { when (readBeatCnt.value === addrReg.wordIndex) { inRdataRegDemand := rdata } }

        val inRdata = if (!ro) {
          val rdataMergeWrite = (rdata & ~wmaskExpand) | (reqReg.wdata & wmaskExpand)
          val rdataMergeWriteSel = (readBeatCnt.value === addrReg.wordIndex) && reqWen
          Mux(rdataMergeWriteSel, rdataMergeWrite, rdata)
        } else rdata

        inRdataReg(readBeatCnt.value) := inRdata

        readBeatCnt.inc()
        when (io.out.r.bits.last) {
          val dataRefill = Cat(inRdata, Cat(inRdataReg.reverse)((LineBeats - 1) * 32 - 1, 0))
          dataArray.write(addrReg.index, dataRefill.asTypeOf(Vec(LineBeats, UInt(32.W))))
          state := s_metaWrite
        }
      }
    }

    is (s_outWriteReq) {
      when (io.out.w.fire()) { writeBeatCnt.inc() }
      when (wSend) { state := s_outWriteResp }
    }

    is (s_outWriteResp) { when (io.out.b.fire()) { state := s_outReadReq } }
    is (s_metaWrite) { state := s_idle }
  }

  // perfcnt
  io.hit := hit && (state === s_metaRead)

  if (debug) {
    io.in.dump(name + ".in")
  }
}
