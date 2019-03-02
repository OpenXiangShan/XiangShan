package noop

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus
import bus.axi4._
import utils._

class Cache(ro: Boolean, name: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus)
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
  val dataArray = Mem(Sets, UInt((LineSize * 8).W))

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
  val metaRead = RegEnable(metaArray.read(idx), metaReadEnable).asTypeOf(metaBundle)
  val dataRead = RegEnable(dataArray.read(idx), metaReadEnable)
  // reading SeqMem has 1 cycle latency, there tag should be compared in the next cycle
  // and the address should be latched
  val reqReg = RegEnable(io.in.req.bits, metaReadEnable)
  if (ro) when (metaReadEnable) { assert(!io.in.req.bits.wen) }
  val addrReg = reqReg.addr.asTypeOf(addrBundle)
  val hit = metaRead.valid && (addrReg.tag === metaRead.tag)
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
  io.out.aw.valid := (state === s_outWriteReq)
  io.out.w.valid := (state === s_outWriteReq)
  io.out.ar.bits.addr := reqReg.addr & ~(LineSize - 1).U(32.W)
  io.out.aw.bits.addr := dirtyBlockAddr
  io.out.w.bits.data := dataRead
  io.out.w.bits.strb := 0xf.U

  io.out.r.ready := (state === s_outReadResp)
  io.out.b.ready := (state === s_outWriteResp)

  // refill
  val metaWriteEnable = !metaReadEnable && ((state === s_metaWrite) ||
    ((state === s_metaRead) && hit && reqReg.wen) )
  val metaWrite = Wire(metaBundle)
  val inRdataReg = Reg(Vec(LineBeats, UInt(32.W)))
  val retData = Mux(hit && (state === s_metaRead), dataRead, inRdataReg.asUInt)
  // FIXME: when burst is supported, should calculate the word index
  val fullMask = Cat(reqReg.wmask.toBools.map(Mux(_, 0xff.U(8.W), 0x0.U(8.W))).reverse)
  val dataWrite = Mux(reqReg.wen, (retData & ~fullMask) | (reqReg.wdata & fullMask), retData)

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

  // return data
  io.in.resp.bits.rdata := retData.asTypeOf(Vec(LineBeats, UInt(32.W)))(addrReg.wordIndex)
  io.in.resp.valid := (hit && (state === s_metaRead)) || (state === s_metaWrite)

  val readBeatCnt = Counter(LineBeats)
  switch (state) {
    is (s_idle) {
      when (io.in.req.fire()) { state := s_metaRead }
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
      when (io.out.aw.fire()) { state := s_outWriteResp }
    }

    is (s_outWriteResp) {
      when (io.out.b.fire()) { state := s_outReadReq }
    }

    is (s_metaWrite) { state := s_idle }
  }

  // perfcnt
  io.hit := hit && (state === s_metaRead)

  if (debug) {
    io.in.dump(name + ".in")
  }
}
