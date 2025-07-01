// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.Random.oneHot
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

//1. Ensure read priority and temporarily store write data in case of read/write conflicts
//2. Ensure that the data is up-to-date and that the data stored in the write buffer can be updated

// Abstract class for write request bundle for write buffer entry update
abstract class WriteReqBundle(implicit p: Parameters) extends BpuBundle {
  // val writeIdx: UInt = Input(UInt(idxWidth.W))
  // val writeTag: UInt = Input(UInt(tagWidth.W))
  val setIdx: UInt  // 方向由子类定义
  // val writeTag: UInt
  def tag: UInt
  // val write_data: T
}

class WriteBuffer[T <: WriteReqBundle](
    gen:          T,
    val numEntries:  Int,
    val pipe:     Boolean = false,
    val hasFlush: Boolean = false
)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  val io = IO(new Bundle {
    val enq: DecoupledIO[T]= Flipped(DecoupledIO(gen))
    val deq: DecoupledIO[T] = DecoupledIO(gen)
    val flush: Option[Bool] = Option.when(hasFlush)(Input(Bool()))
  })

  // clean write buffer when flush is true
  private val flush = io.flush.getOrElse(false.B)

  // Circular queue pointer
  private class FIFOPtr extends CircularQueuePtr[FIFOPtr](numEntries)
  private object FIFOPtr {
    def apply(f: Bool, v: UInt): FIFOPtr = {
      val ptr = Wire(new FIFOPtr)
      ptr.flag  := f
      ptr.value := v
      ptr
    }
  }

  private val valids        = RegInit(0.U.asTypeOf(Vec(numEntries, Bool())))
  private val entries = RegInit(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(gen.cloneType))))
  private val hitMask = WireDefault(VecInit(Seq.fill(numEntries)(false.B)))
  private val hit     = hitMask.reduce(_ || _)
  private val hit_idx = OHToUInt(hitMask)

  private val enq_ptr = RegInit(FIFOPtr(false.B, 0.U))
  private val deq_ptr = RegInit(FIFOPtr(false.B, 0.U))

  private val empty = enq_ptr === deq_ptr
  private val full  = (enq_ptr.value === deq_ptr.value) && (enq_ptr.flag ^ deq_ptr.flag)

  when(io.enq.fire) {
    enq_ptr := enq_ptr + 1.U
  }
  when(io.deq.fire) {
    deq_ptr               := deq_ptr + 1.U
    valids(enq_ptr.value) := false.B
  }

  when(flush) {
    enq_ptr.value := 0.U
    enq_ptr.flag  := false.B
    deq_ptr.value := 0.U
    deq_ptr.flag  := false.B
    valids.foreach(_ := false.B)
  }

  when(io.enq.fire) {
    for (i <- 0 until numEntries) {
      hitMask(i) := valids(i) && io.enq.bits.setIdx === entries(i).setIdx &&
        io.enq.bits.tag=== entries(i).tag
    }
    assert(PopCount(hitMask) <= 1.U, "WriteBuffer hitMask should be one-hot")
    when(hit) {
      // If hit, update the data
      entries(hit_idx) := io.enq.bits
    }.otherwise {
      // If miss, enqueue the new data
      entries(enq_ptr.value) := io.enq.bits
      valids(enq_ptr.value)        := true.B
    }
  }
  io.deq.bits := entries(deq_ptr.value)

  io.deq.valid := !empty
  io.enq.ready := !full

  if (pipe) {
    when(io.deq.ready)(io.enq.ready := true.B)
  }

  XSPerfAccumulate("writeBuffer_update_hit", io.enq.fire && hit)
  XSPerfAccumulate("writeBuffer_update_miss", io.enq.fire && !hit)
  XSPerfAccumulate("writeBuffer_update_full", io.enq.valid && full)

  // XSDebug(
  //   io.wen && hit,
  //   p"wrbypass hit entry #${hit_idx}, idx ${io.write_idx}" +
  //     p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n"
  // )
  // XSDebug(
  //   io.wen && !hit,
  //   p"wrbypass enq entry #${enq_idx}, idx ${io.write_idx}" +
  //     p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n"
  // )
}
