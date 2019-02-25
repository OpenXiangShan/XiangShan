package noop

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus
import utils._

class ICache extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBus)
    val out = new SimpleBus
  })

  val TotalSize = 16 // Kbytes
  val LineSize = 4 //64 // byte
  val Sets = TotalSize * 1024 / LineSize
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val TagBits = 32 - OffsetBits - IndexBits

  val addrBundle = io.in.req.bits.addr.asTypeOf(new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    //val wordIndex = UInt((OffsetBits - 2).W)
    val byteOffset = UInt(2.W)
  })

  val metaBundle = new Bundle {
      val tag = UInt(TagBits.W)
      val valid = Bool()
    }
  val metaArray = Mem(Sets, UInt(metaBundle.getWidth.W))
  val dataArray = Mem(Sets, UInt((LineSize * 8).W))

  // should reset meta.valid
  val resetState = RegInit(true.B)
  val (resetIdx, resetFinish) = Counter(resetState, Sets)
  when (resetFinish) {
    resetState := false.B
  }

  val s_idle :: s_metaRead :: s_memReadReq :: s_memReadResp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // read metadata
  io.in.req.ready := (state === s_idle) && !resetState
  val metaReadEnable = io.in.req.fire() && (state === s_idle)
  val metaRead = RegEnable(metaArray.read(addrBundle.index), metaReadEnable).asTypeOf(metaBundle)
  val dataRead = RegEnable(dataArray.read(addrBundle.index), metaReadEnable)
  // reading SeqMem has 1 cycle latency, there tag should be compared in the next cycle
  // and the address should be latched
  val addrReg = RegEnable(addrBundle, metaReadEnable)
  val hit = metaRead.valid && (addrReg.tag === metaRead.tag)

  // if miss, access memory
  io.out := DontCare
  io.out.req.valid := state === s_memReadReq
  io.out.req.bits.addr := addrReg.asUInt
  io.out.req.bits.size := "b10".U //"b110".U
  io.out.req.bits.wen := false.B
  io.out.resp.ready := state === s_memReadResp

  // refill
  val metaWriteEnable = (state === s_memReadResp) && io.out.resp.fire() && !metaReadEnable
  val metaWrite = Wire(metaBundle)
  metaWrite.tag := addrReg.tag
  metaWrite.valid := Mux(resetState, false.B, true.B)
  when (metaWriteEnable || resetState) {
    metaArray.write(Mux(resetState, resetIdx, addrReg.index), metaWrite.asUInt)
    dataArray.write(addrReg.index, io.out.resp.bits.rdata)
  }

  // return data
  val retData = Mux(hit && (state === s_metaRead), dataRead, io.out.resp.bits.rdata)
  io.in.resp.bits.rdata := retData//.asTypeOf(Vec(LineSize / 4, UInt(32.W)))(addrReg.wordIndex)
  io.in.resp.valid := (hit && (state === s_metaRead)) || ((state === s_memReadResp && io.out.resp.fire()))

  switch (state) {
    is (s_idle) {
      when (io.in.req.fire()) { state := s_metaRead }
    }

    is (s_metaRead) {
      state := Mux(hit, s_idle, s_memReadReq)
    }

    is (s_memReadReq) {
      when (io.out.req.fire()) { state := s_memReadResp }
    }

    is (s_memReadResp) {
      when (io.out.resp.fire()) { state := s_idle }
    }
  }
}
