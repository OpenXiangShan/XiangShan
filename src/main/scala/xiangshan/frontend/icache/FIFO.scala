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

package  xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.{AliasField, PrefetchField}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend._
import firrtl.ir.Block
import firrtl.options.DoNotTerminateOnExit


class FIFOReg[T <: Data](
  val gen:            T,
  val entries:        Int,
  val pipe:           Boolean = false,
  val hasFlush:       Boolean = false
) extends Module() {
  require(entries > 0, "Queue must have non-negative number of entries")

  val io = IO(new Bundle {
    val enq     = Flipped(DecoupledIO(gen))
    val deq     = DecoupledIO(gen)
    val flush   = if (hasFlush) Some(Input(Bool())) else None
  })
  val flush = io.flush.getOrElse(false.B)

  class FIFOPtr() extends CircularQueuePtr[FIFOPtr](entries)

  object FIFOPtr {
    def apply(f: Bool, v: UInt)(): FIFOPtr = {
      val ptr = Wire(new FIFOPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val regFiles = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen.cloneType))))
  val enq_ptr  = RegInit(FIFOPtr(false.B, 0.U))
  val deq_ptr  = RegInit(FIFOPtr(false.B, 0.U))

  val empty = enq_ptr === deq_ptr
  val full  = (enq_ptr.value === deq_ptr.value) && (enq_ptr.flag ^ deq_ptr.flag)

  when(io.enq.fire) {
    enq_ptr := enq_ptr + 1.U
  }
  when(io.deq.fire) {
    deq_ptr := deq_ptr + 1.U
  }
  when(flush) {
    enq_ptr.value := 0.U
    enq_ptr.flag  := false.B
    deq_ptr.value := 0.U
    deq_ptr.flag  := false.B
  }

  when(io.enq.fire) {
    regFiles(enq_ptr.value) := io.enq.bits
  }
  io.deq.bits := regFiles(deq_ptr.value)

  io.deq.valid := !empty
  io.enq.ready := !full
  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }
}