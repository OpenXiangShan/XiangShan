package utils

import chisel3._
import chisel3.util._

class SRAMBundleA(val set: Int) extends Bundle {
  val idx = Output(UInt(log2Up(set).W))
}

class SRAMBundleAW[T <: Data](private val gen: T,
  set: Int, val way: Int = 1, val subarray: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val subarrayMask = if (subarray > 1) Some(Output(UInt(subarray.W))) else None
  val wayMask = if (way > 1) Some(Output(Vec(way, Bool()))) else None
}

class SRAMBundleR[T <: Data](private val gen: T,
  val way: Int = 1, val subarray: Int = 1) extends Bundle {
  val data = Output(Vec(way, Vec(subarray, gen)))
}

class SRAMReadBus[T <: Data](private val gen: T,
  val set: Int, val way: Int = 1, val subarray: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way, subarray))
}

class SRAMWriteBus[T <: Data](private val gen: T,
  val set: Int, val way: Int = 1, val subarray: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way, subarray))
}

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1, subarray: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way, subarray))
    val w = Flipped(new SRAMWriteBus(gen, set, way, subarray))
  })

  val wordType = UInt(gen.getWidth.W)
  val arrays = Seq.tabulate(subarray) { i => SyncReadMem(set, Vec(way, wordType)) }
  val (resetState, resetIdx) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetIdx, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetIdx := _resetIdx
  }

  val idx = Mux(resetState, resetIdx, io.w.req.bits.idx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val subarrayMask = Mux(resetState, Fill(subarray, "b1".U), io.w.req.bits.subarrayMask.getOrElse("b1".U))
  val wayMask = io.w.req.bits.wayMask.getOrElse("b1".U.asBools)
  val wdata = VecInit(Seq.fill(way)(wdataword))

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val rdatas = for ((array, i) <- arrays.zipWithIndex) yield {
    when (wen & subarrayMask(i)) { array.write(idx, wdata, wayMask) }
    (if (holdRead) ReadAndHold(array, io.r.req.bits.idx, realRen)
      else array.read(io.r.req.bits.idx, realRen)).map(_.asTypeOf(gen))
  }

  io.r.resp.data := VecInit(rdatas.transpose.map(VecInit(_)))
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B
}
