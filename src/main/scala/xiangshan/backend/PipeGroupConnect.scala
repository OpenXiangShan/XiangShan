package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan.TopDownCounters._

class StoreBubbleReason(reasonW: Int) extends Module {
  val io = IO(new Bundle{
    val bubbleValid = Input(Bool())
    val bubbleReason = Input(UInt(reasonW.W))
    val redirect = Input(Bool())
    val reasonFire = Input(Bool())
    val outReasonValid = Output(Bool())
    val outReason = Output(UInt(reasonW.W))
  })
  val NoStallReason = NoStall.id.U
  val bubbleValidReg = RegInit(false.B)
  val bubbleReasonReg = RegInit(NoStallReason)
  val bubblevalidNext = WireDefault(bubbleValidReg)
  val bubbleReasonNext = WireDefault(bubbleReasonReg)

  val bubbleConsumed = bubbleValidReg && io.reasonFire
  val canUpdate = (!bubbleValidReg) || bubbleConsumed

  when (io.redirect) {
    bubblevalidNext := false.B
    bubbleReasonNext := NoStallReason
  }.elsewhen(canUpdate && io.bubbleValid) {
    bubblevalidNext := true.B
    bubbleReasonNext := io.bubbleReason
  }.elsewhen(bubbleConsumed) {
    bubblevalidNext := false.B
    bubbleReasonNext := NoStallReason
  }.otherwise {
    bubblevalidNext := bubbleValidReg
    bubbleReasonNext := bubbleReasonReg
  }

  bubbleValidReg := bubblevalidNext
  bubbleReasonReg := bubbleReasonNext

  dontTouch(bubbleValidReg)
  dontTouch(bubbleReasonReg)

  io.outReason := Mux(io.bubbleValid && !bubbleValidReg, io.bubbleReason, bubbleReasonReg)
  io.outReasonValid := bubbleValidReg || io.bubbleValid
}

class PipelineStallReason(reasonW: Int) extends Module {
  val io = IO(new Bundle{
    val rightFire = Input(Bool())
    val rightHasFire = Input(Bool())
    val prePipeStall = Input(Bool())
    val prePipeStallReason = Input(UInt(reasonW.W))
    val prePipeBubble = Input(Bool())
    val prePipeBubbleReason = Input(UInt(reasonW.W))
    val redirect = Input(Bool())
    val redirectReason = Input(UInt(reasonW.W))
    val currentPipeStall = Input(Bool())
    val currentPipeStallReason = Input(UInt(reasonW.W))
    val currentPipeBubble = Input(Bool())
    val currentPipeBubbleReason = Input(UInt(reasonW.W))
    val outReason = Output(UInt(reasonW.W))
  })

  val NoStallReason = NoStall.id.U

  val redirectReg = RegNext(io.redirect)
  val redirectReason = RegNext(io.redirectReason)

  val prePipeStallReg = RegNext(io.prePipeStall)
  val prePipeStallReasonReg = RegNext(io.prePipeStallReason)

  val currentPipeStallReg = RegNext(io.currentPipeStall)
  val currentPipeStallReasonReg = RegNext(io.currentPipeStallReason)

  // todo current pipe bubble
  // todo seperate bubble stall here(with class)

  val currentPipeBubbleReg = RegNext(io.currentPipeBubble)
  val currentPipeBubbleReasonReg = RegNext(io.currentPipeBubbleReason)


  val reasonNext = Wire(UInt(reasonW.W))

  when (io.redirect || redirectReg) {
    reasonNext := Mux(io.redirect, io.redirectReason, redirectReg)
  }.otherwise {
    when (io.rightFire) {
      reasonNext := NoStallReason
    }.elsewhen(prePipeStallReg && !io.rightHasFire) {
      reasonNext := prePipeStallReasonReg
    }.elsewhen (currentPipeStallReg && !io.rightHasFire) {
      reasonNext := currentPipeStallReasonReg
    }.elsewhen (io.prePipeBubble) {
      reasonNext := io.prePipeBubbleReason
    }.elsewhen (currentPipeBubbleReg){
      reasonNext := currentPipeBubbleReasonReg
    }otherwise {
      reasonNext := NoStallReason
    }
  }
  io.outReason := reasonNext
}

class PipeGroupConnect[T <: Data](n: Int, gen: => T) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(gen)))
    val out = Vec(n, DecoupledIO(gen))
    val flush = Input(Bool())
    val outAllFire = Input(Bool())
  })

  // Input Alias
  // Use private[this] to limit the wrong usage for not IO hardware in object with the same name.
  private[this] val flush = io.flush
  private[this] val inValidSeq  = io.in.map(_.valid)
  private[this] val inDataSeq   = io.in.map(_.bits)
  private[this] val outReadySeq = io.out.map(_.ready)

  // Regs
  private[this] val validVec = RegInit(VecInit.fill(n)(false.B))
  private[this] val dataVec  = Reg(Vec(n, gen))

  // Logic
  private[this] val valids    = Cat(validVec.reverse)
  private[this] val inValids  = Cat(inValidSeq.reverse)
  private[this] val outReadys = Cat(outReadySeq.reverse)

  // Todo: canAccVec for each elem
  // Todo: no outReadys version for better timing and lower performance
  private[this] val canAcc = io.outAllFire || !valids.orR

  (validVec zip inValids.asBools zip outReadys.asBools).foreach { case ((valid, inValid), outReady) =>
    valid := MuxCase(
      default = valid /*keep*/,
      Seq(
        flush               -> false.B,
        (inValid && canAcc) -> true.B,
        outReady            -> false.B
      )
    )
  }

  (dataVec zip inValids.asBools zip inDataSeq).foreach { case ((data, inValid), inData) =>
    when (inValid && canAcc) {
      data := inData
    }
  }

  // Output connections
  for (i <- 0 until n) {
    io.in(i).ready  := canAcc
    io.out(i).valid := validVec(i)
    io.out(i).bits  := dataVec(i)
  }
}

object PipeGroupConnect {
  def apply[T <: Data](
    // Left can be not Vec, but right must be Vec
    left: Seq[DecoupledIO[T]],
    right: Vec[DecoupledIO[T]],
    flush: Bool,
    rightAllFire: Bool,
    suggestName: String = null,
  ): Unit =  {
    require(left.size == right.size, "The sizes of left and right Vec Bundle should be equal in PipeGroupConnect")
    require(left.size > 0, "The size of Vec Bundle in PipeGroupConnect should be more than 0")
    val mod = Module(new PipeGroupConnect(left.size, chiselTypeOf(left.head.bits)))
    mod.io.flush := flush
    mod.io.in.zipWithIndex.foreach { case (in, i) =>
      in.valid := left(i).valid
      in.bits := left(i).bits
      left(i).ready := in.ready
    }
    mod.io.outAllFire := rightAllFire
    right <> mod.io.out

    if (suggestName != null)
      mod.suggestName(suggestName)
  }
}
