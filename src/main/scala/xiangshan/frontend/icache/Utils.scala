// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

// FIXME: should move to utility, or is there's an alternative in utility already?

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import utility.CircularQueuePtr

class DeMultiplexerIO[T <: Data](gen: T, n: Int) extends Bundle {
  val in:     DecoupledIO[T]      = Flipped(DecoupledIO(gen))
  val out:    Vec[DecoupledIO[T]] = Vec(n, DecoupledIO(gen))
  val chosen: UInt                = Output(UInt(log2Ceil(n).W))
}

/** Hardware module that is used to sequence 1 producer into n consumer.
 * Priority is given to lower producer.
 */
class DeMultiplexer[T <: Data](val gen: T, val n: Int) extends Module {
  require(n >= 2)
  val io: DeMultiplexerIO[T] = IO(new DeMultiplexerIO(gen, n))

  private val grant = false.B +: (1 until n).map(i => (0 until i).map(io.out(_).ready).reduce(_ || _))
  (0 until n).foreach { i =>
    io.out(i).bits  := io.in.bits
    io.out(i).valid := !grant(i) && io.in.valid
  }

  io.in.ready := grant.last || io.out.last.ready
  io.chosen   := PriorityEncoder(VecInit(io.out.map(_.ready)))
}

class MuxBundleIO[T <: Data](gen: T, n: Int) extends Bundle {
  val sel: UInt                = Input(UInt(log2Ceil(n).W))
  val in:  Vec[DecoupledIO[T]] = Flipped(Vec(n, DecoupledIO(gen)))
  val out: DecoupledIO[T]      = DecoupledIO(gen)
}

class MuxBundle[T <: Data](val gen: T, val n: Int) extends Module {
  require(n >= 2)
  val io: MuxBundleIO[T] = IO(new MuxBundleIO[T](gen, n))

  io.in <> DontCare
  io.out <> DontCare
  (0 until n).foreach { i =>
    when(io.sel === i.U) {
      io.out <> io.in(i)
    }
    io.in(i).ready := (io.sel === i.U) && io.out.ready
  }
}

class FIFORegIO[T <: Data](gen: T, hasFlush: Boolean = false) extends Bundle {
  val enq:   DecoupledIO[T] = Flipped(DecoupledIO(gen))
  val deq:   DecoupledIO[T] = DecoupledIO(gen)
  val flush: Option[Bool]   = Option.when(hasFlush)(Input(Bool()))
}

class FIFOReg[T <: Data](
    val gen:      T,
    val entries:  Int,
    val pipe:     Boolean = false,
    val hasFlush: Boolean = false
) extends Module() {
  require(entries > 0, "Queue must have non-negative number of entries")

  val io: FIFORegIO[T] = IO(new FIFORegIO(gen, hasFlush))
  private val flush = io.flush.getOrElse(false.B)

  private class FIFOPtr extends CircularQueuePtr[FIFOPtr](entries)
  private object FIFOPtr {
    def apply(f: Bool, v: UInt): FIFOPtr = {
      val ptr = Wire(new FIFOPtr)
      ptr.flag  := f
      ptr.value := v
      ptr
    }
  }

  private val regFiles = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen.cloneType))))
  private val enqPtr   = RegInit(FIFOPtr(false.B, 0.U))
  private val deqPtr   = RegInit(FIFOPtr(false.B, 0.U))

  private val empty = enqPtr === deqPtr
  private val full  = (enqPtr.value === deqPtr.value) && (enqPtr.flag ^ deqPtr.flag)

  when(io.enq.fire) {
    enqPtr := enqPtr + 1.U
  }
  when(io.deq.fire) {
    deqPtr := deqPtr + 1.U
  }
  when(flush) {
    enqPtr.value := 0.U
    enqPtr.flag  := false.B
    deqPtr.value := 0.U
    deqPtr.flag  := false.B
  }

  when(io.enq.fire) {
    regFiles(enqPtr.value) := io.enq.bits
  }
  io.deq.bits := regFiles(deqPtr.value)

  io.deq.valid := !empty
  io.enq.ready := !full
  if (pipe) {
    when(io.deq.ready)(io.enq.ready := true.B)
  }
}
