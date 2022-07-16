/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import chisel3._
import chisel3.util._

class PipelineConnectPipe[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val rightOutFire = Input(Bool())
    val isFlush = Input(Bool())
  })

  PipelineConnect.connect(io.in, io.out, io.rightOutFire, io.isFlush, false.B)
}

class PipelineConnectBuffer[T <: Data, FlushT <: Data](gen: T, flushGen: FlushT, flushFunc: (T, FlushT) => Bool)
  extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(gen.cloneType))
    val out = DecoupledIO(gen.cloneType)
    val flush = Input(flushGen.cloneType)
  })

  val valid = RegInit(VecInit.fill(2)(false.B))
  val data = Reg(Vec(2, gen.cloneType))
  val older = RegInit(false.B)

  // out
  io.out.valid := valid.asUInt.orR
  io.out.bits := data(older)
  when (io.out.fire) {
    valid(older) := false.B
    older := !older
  }

  // in
  io.in.ready := !valid.asUInt.andR
  val updateVec = WireInit(VecInit.fill(2)(false.B))
  when (io.in.valid && !flushFunc(io.in.bits, io.flush)) {
    // how to choose: this_empty && (this_older || other_older && other_not_empty)
    when (!valid(0) && (!older || older && valid(1))) {
      valid(0) := true.B
      data(0) := io.in.bits
      updateVec(0) := true.B
    }.elsewhen (!valid(1) && (older || !older && valid(0))) {
      valid(1) := true.B
      data(1) := io.in.bits
      updateVec(1) := true.B
    }
  }

  // flush
  val flushVec = data.zip(valid).map{ case (d, v) => flushFunc(d, io.flush) && v }
  flushVec.zip(valid).foreach{ case (f, v) =>
    when (f) {
      v := false.B
    }
  }
}

class PipelineConnectBufferWithExtraData[T <: Data, FlushT <: Data, ExtraT <: Data](
  gen: T, flushGen: FlushT, flushFunc: (T, FlushT) => Bool, extraGen: ExtraT, extraLatency: Int
) extends PipelineConnectBuffer(gen, flushGen, flushFunc) {
  require(extraLatency > 0, "why not use PipelineConnectBuffer?")
  require(extraLatency == 1, "only 1 is supported now")

  val extra = IO(new Bundle {
    val in = Input(extraGen.cloneType)
    val out = Output(extraGen.cloneType)
  })

  val extraData = Reg(Vec(2, extraGen.cloneType))
  for (i <- 0 until 2) {
    when (RegNext(updateVec(i) && !flushVec(i))) {
      extraData(i) := extra.in
    }
  }

  // after out.fire, we assert(!older === RegNext(older))
  extra.out := extraData(!older)
}

object PipelineConnect {
  def connect[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool,
    block: Bool
  ): Unit = {
    val valid = RegInit(false.B)
    val leftFire = left.valid && right.ready && !block
    when (rightOutFire) { valid := false.B }
    when (leftFire) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready && !block
    right.bits := RegEnable(left.bits, leftFire)
    right.valid := valid
  }

  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool,
    block: Bool = false.B,
    moduleName: Option[String] = None
  ): Unit = {
    if (moduleName.isDefined) {
      val pipeline = Module(new PipelineConnectPipe(left.bits))
      pipeline.suggestName(moduleName.get)
      pipeline.io.in <> left
      pipeline.io.rightOutFire := rightOutFire
      pipeline.io.isFlush := isFlush
      pipeline.io.out <> right
      pipeline.io.out.ready := right.ready && !block
    }
    else {
      // do not use module here to please DCE
      connect(left, right, rightOutFire, isFlush, block)
    }
  }

  def apply[T <: Data, FlushT <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    flushFunc: (T, FlushT) => Bool,
    flush: FlushT,
    moduleName: Option[String]
  ): Unit = {
    val pipe_buffer = Module(new PipelineConnectBuffer(left.bits, flush, flushFunc))
    if(moduleName.nonEmpty) pipe_buffer.suggestName(moduleName.get)
    pipe_buffer.io.in <> left
    pipe_buffer.io.out <> right
    pipe_buffer.io.flush := flush
  }

  def apply[T <: Data, FlushT <: Data, ExtraT <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    flushFunc: (T, FlushT) => Bool,
    flush: FlushT,
    extraGen: ExtraT,
    extraLatency: Int
  ): PipelineConnectBufferWithExtraData[T, FlushT, ExtraT] = {
    val pipe_buffer = Module(new PipelineConnectBufferWithExtraData(left.bits, flush, flushFunc, extraGen, extraLatency))
    pipe_buffer.io.in <> left
    pipe_buffer.io.out <> right
    pipe_buffer.io.flush := flush
    pipe_buffer
  }
}

object PipelineNext {
  def apply[T <: Data](
    left: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ): DecoupledIO[T] = {
    val right = Wire(Decoupled(left.bits.cloneType))
    PipelineConnect(left, right, rightOutFire, isFlush)
    right
  }

  def apply[T <: Data, FlushT <: Data](
    left: DecoupledIO[T],
    flushFunc: (T, FlushT) => Bool,
    flush: FlushT
  ): DecoupledIO[T] = {
    val right = Wire(Decoupled(left.bits.cloneType))
    PipelineConnect(left, right, flushFunc, flush, Some("buffer"))
    right
  }
}
