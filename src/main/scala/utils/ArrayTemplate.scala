package utils

import chisel3._
import chisel3.util._

class ArrayReqBus(set: Int) extends Bundle {
  val valid = Output(Bool())
  val idx = Output(UInt(log2Up(set).W))
}

class ArrayReadBus[T <: Data](gen: T, set: Int, way: Int = 1) extends Bundle {
  val req = new ArrayReqBus(set)
  val entry = if (way > 1) Input(Vec(way, gen)) else Input(gen)

  override def cloneType = new ArrayReadBus(gen, set, way).asInstanceOf[this.type]
}

class ArrayWriteBus[T <: Data](gen: T, set: Int, way: Int = 1) extends Bundle {
  val req = new ArrayReqBus(set)
  // only write one word at a time
  val entry = Output(gen)
  val wordIndex = Output(UInt(log2Up(way).W))

  override def cloneType = new ArrayWriteBus(gen, set, way).asInstanceOf[this.type]
}

class ArrayTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new ArrayReadBus(gen, set, way))
    val w = Flipped(new ArrayWriteBus(gen, set, way))
    val finishReset = Output(Bool())
  })

  val wordType = UInt(gen.getWidth.W)
  val wayType = Vec(way, wordType)
  val array = SyncReadMem(set, wayType)
  val (resetState, resetIdx) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetIdx, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetIdx := _resetIdx
  }

  val idx = Mux(resetState, resetIdx, io.w.req.idx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.entry.asUInt)
  val wordIndex = if (way > 1) io.w.wordIndex else 0.U
  val wdata = WordShift(wdataword, wordIndex, gen.getWidth).asTypeOf(wayType)
  val wmask = if (way > 1) (1.U << wordIndex).asBools else Seq(true.B)

  when (io.w.req.valid || resetState) { array.write(idx, wdata, wmask) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.idx, io.r.req.valid)
              else array.read(io.r.req.idx, io.r.req.valid)).map(_.asTypeOf(gen))
  io.r.entry := (if (way > 1) VecInit(rdata) else rdata(0))
  io.finishReset := !resetState
}
