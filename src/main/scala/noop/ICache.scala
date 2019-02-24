package noop

import chisel3._
import chisel3.util._

import memory.MemIO

class ICache extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new MemIO
  })

  val TotalSize = 16 // Kbytes
  val LineSize = 4 //64 // byte
  val Sets = TotalSize * 1024 / LineSize
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val TagBits = 32 - OffsetBits - IndexBits

  val addrBundle = io.in.a.bits.addr.asTypeOf(new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    //val wordIndex = UInt((OffsetBits - 2).W)
    val byteOffset = UInt(2.W)
  })

  val tagArray = Mem(Sets, UInt(TagBits.W))
  val valids = RegInit(0.U(Sets.W))
  val dataArray = Mem(Sets, UInt((LineSize * 8).W))

  val s_idle :: s_metaRead :: s_memReadReq :: s_memReadResp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // read metadata
  io.in.a.ready := (state === s_idle)
  val metaReadEnable = io.in.a.fire() && (state === s_idle)
  val addrReg = RegEnable(addrBundle, metaReadEnable)
  val tagRead = RegEnable(tagArray.read(addrBundle.index), metaReadEnable)
  val dataRead = RegEnable(dataArray.read(addrBundle.index), metaReadEnable)
  // reading SeqMem has 1 cycle latency, there tag should be compared in the next cycle
  // and the address should be latched
  val validRead = valids(addrReg.index)
  val hit = validRead && (addrReg.tag === tagRead)

  // if miss, access memory
  io.out := DontCare
  io.out.a.valid := state === s_memReadReq
  io.out.a.bits.addr := addrReg.asUInt
  io.out.a.bits.size := "b10".U //"b110".U
  io.out.r.ready := state === s_memReadResp
  io.out.w.valid := false.B

  // refill
  val metaWriteEnable = (state === s_memReadResp) && io.out.r.fire() && !metaReadEnable
  when (metaWriteEnable) {
    tagArray.write(addrReg.index, addrReg.tag)
    valids := valids | (1.U << addrReg.index)
    dataArray.write(addrReg.index, io.out.r.bits.data)
  }

  // return data
  val retData = Mux(hit && (state === s_metaRead), dataRead, io.out.r.bits.data)
  io.in.r.bits.data := retData//.asTypeOf(Vec(LineSize / 4, UInt(32.W)))(addrReg.wordIndex)
  io.in.r.valid := (hit && (state === s_metaRead)) || ((state === s_memReadResp && io.out.r.fire()))

  switch (state) {
    is (s_idle) {
      when (io.in.a.fire()) { state := s_metaRead }
    }

    is (s_metaRead) {
      state := Mux(hit, s_idle, s_memReadReq)
    }

    is (s_memReadReq) {
      when (io.out.a.fire()) { state := s_memReadResp }
    }

    is (s_memReadResp) {
      when (io.out.r.fire()) { state := s_idle }
    }
  }
}
