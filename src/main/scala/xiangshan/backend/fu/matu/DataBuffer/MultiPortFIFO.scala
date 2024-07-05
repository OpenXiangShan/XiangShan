package matu.DataBuffer

import chisel3._
import chisel3.util._

class MultiWritePortFIFO(width: Int, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Input(Bool())
    val deq = Input(Bool())
    val enqData = Input(Vec(depth, SInt(width.W)))
    val deqData = Output(SInt(width.W))
    val full = Output(Bool())
    val empty = Output(Bool())
  })

  val mem = RegInit(VecInit(Seq.fill(depth)(0.S(width.W))))
  val addr_width = log2Ceil(depth)
  val readPtr = RegInit(0.U((addr_width + 1).W)) // extra bit to indicate full or empty
  val writePtr = RegInit(0.U((addr_width + 1).W))

  val isFull = WireInit(false.B)
  val isEmpty = WireInit(false.B)
  val deqData = WireInit(0.S(width.W))

  isEmpty := readPtr === writePtr
  isFull := readPtr === Cat(~writePtr(addr_width), writePtr(addr_width - 1, 0))

  when(io.enq && !isFull) {
    mem <> io.enqData
    writePtr := writePtr + depth.U
  }

  when(io.deq && !isEmpty) {
    readPtr := readPtr + 1.U
    deqData := mem(readPtr)
  }

  io.deqData := deqData
  io.full := isFull
  io.empty := isEmpty

}

class MultiReadPortFIFO(width: Int, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Input(Bool())
    val deq = Input(Bool())
    val enqData = Input(SInt(width.W))
    val deqData = Output(Vec(depth, SInt(width.W)))
    val full = Output(Bool())
    val empty = Output(Bool())
  })

  val mem = RegInit(VecInit(Seq.fill(depth)(0.S(width.W))))
  val addr_width = log2Ceil(depth)
  val readPtr = RegInit(0.U((addr_width + 1).W)) // extra bit to indicate full or empty
  val writePtr = RegInit(0.U((addr_width + 1).W))

  val isFull = WireInit(false.B)
  val isEmpty = WireInit(false.B)
  val deqData = WireInit(VecInit(Seq.fill(depth)(0.S(width.W))))

  isEmpty := readPtr === writePtr
  isFull := readPtr === Cat(~writePtr(addr_width), writePtr(addr_width - 1, 0))

  when(io.enq && !isFull) {
    mem(writePtr) := io.enqData
    writePtr := writePtr + 1.U
  }

  when(io.deq && isFull) {
    readPtr := readPtr + depth.U
    for (i <- 0 until depth) {
      deqData(i) := mem(i)
    }
  }

  io.deqData := deqData
  io.full := isFull
  io.empty := isEmpty


}

