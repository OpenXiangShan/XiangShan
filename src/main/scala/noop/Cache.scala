package noop

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus
import utils._

class Cache(ro: Boolean, name: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus)
    val out = new SimpleBus
  })

  val debug = false

  val TotalSize = 16 // Kbytes
  val LineSize = 4 //64 // byte
  val Sets = TotalSize * 1024 / LineSize
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val TagBits = 32 - OffsetBits - IndexBits

  val addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    //val wordIndex = UInt((OffsetBits - 2).W)
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

  val s_idle :: s_metaRead :: s_outReadReq :: s_outReadResp :: s_outWriteReq :: s_outWriteResp :: Nil = Enum(6)
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
  io.out.req.valid := (state === s_outReadReq) || (state === s_outWriteReq)
  io.out.req.bits.addr := Mux(state === s_outWriteReq, dirtyBlockAddr, reqReg.addr)
  io.out.req.bits.size := "b10".U //"b110".U
  io.out.req.bits.wen := state === s_outWriteReq
  io.out.req.bits.wdata := dataRead
  io.out.req.bits.wmask := 0xf.U
  io.out.resp.ready := (state === s_outReadResp) || (state === s_outWriteResp)

  // refill
  val metaWriteEnable = !metaReadEnable && (
    ((state === s_outReadResp) && io.out.resp.fire()) ||
    ((state === s_metaRead) && hit && reqReg.wen) )
  val metaWrite = Wire(metaBundle)
  val inRdata = Mux(hit && (state === s_metaRead), dataRead, io.out.resp.bits.rdata)
  // FIXME: when burst is supported, should calculate the word index
  val fullMask = Cat(reqReg.wmask.toBools.map(Mux(_, 0xff.U(8.W), 0x0.U(8.W))).reverse)
  val dataWrite = Mux(reqReg.wen, (inRdata & ~fullMask) | (reqReg.wdata & fullMask), inRdata)

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
  io.in.resp.bits.rdata := inRdata//.asTypeOf(Vec(LineSize / 4, UInt(32.W)))(addrReg.wordIndex)
  io.in.resp.valid := (hit && (state === s_metaRead)) || ((state === s_outReadResp && io.out.resp.fire()))

  switch (state) {
    is (s_idle) {
      when (io.in.req.fire()) { state := s_metaRead }
    }

    is (s_metaRead) {
      state := Mux(hit, s_idle, Mux(metaRead.valid && dirty, s_outWriteReq, s_outReadReq))
    }

    is (s_outReadReq) {
      when (io.out.req.fire()) { state := s_outReadResp }
    }

    is (s_outReadResp) {
      when (io.out.resp.fire()) { state := s_idle }
    }

    is (s_outWriteReq) {
      when (io.out.req.fire()) { state := s_outWriteResp }
    }

    is (s_outWriteResp) {
      when (io.out.resp.fire()) { state := s_outReadReq }
    }
  }

  if (debug) {
    io.in.dump(name + ".in")
    io.out.dump(name + ".out")
  }
}
