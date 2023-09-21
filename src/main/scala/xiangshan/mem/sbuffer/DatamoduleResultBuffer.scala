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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3.experimental.{DataMirror, requireIsChiselType}
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache._
import difftest._

class DatamoduleResultBufferIO[T <: Data](gen: T)(implicit p: Parameters) extends XSBundle
{
  // val flush = Input(Bool())
  val enq = Vec(EnsbufferWidth, Flipped(DecoupledIO(gen)))
  val deq = Vec(EnsbufferWidth, DecoupledIO(gen))

}

class DatamoduleResultBuffer[T <: Data]
(
  gen: T,
)(implicit p: Parameters) extends XSModule {

  val genType = {
    requireIsChiselType(gen)
    gen
  }

  val io = IO(new DatamoduleResultBufferIO[T](gen))

  val data = Reg(Vec(EnsbufferWidth, genType))
  val valids = RegInit(VecInit(Seq.fill(EnsbufferWidth)(false.B)))
  val enq_flag = RegInit(0.U(log2Up(EnsbufferWidth).W)) // head is entry 0
  val deq_flag = RegInit(0.U(log2Up(EnsbufferWidth).W)) // tail is entry 0

  val entry_allowin = Wire(Vec(EnsbufferWidth, Bool()))

  (0 until EnsbufferWidth).foreach(index => {
    io.deq(index).valid := valids(deq_flag + index.U) && (if (index == 0) 1.B else io.deq(index - 1).valid)
    io.deq(index).bits := data(deq_flag + index.U)
  })

  (1 until EnsbufferWidth).foreach(i => {
    assert(!(io.deq(i).valid && !io.deq(i - 1).valid))
    assert(!(io.deq(i).ready && !io.deq(i - 1).ready))
  })

  (0 until EnsbufferWidth).foreach(
    index => entry_allowin(index) := !valids(index) || (0 until EnsbufferWidth).map(i => io.deq(i).fire && deq_flag + i.U === index.U).reduce(_ || _)
  )

  (0 until EnsbufferWidth).foreach(
    index => io.enq(index).ready := entry_allowin(enq_flag + index.U) && (if (index == 0) 1.B else io.enq(index - 1).ready)
  )

  (1 until EnsbufferWidth).foreach(i => {
    assert(!(io.enq(i).ready && !io.enq(i - 1).ready))
    assert(!(io.enq(i).valid && !io.enq(i - 1).valid))
  })

  (0 until EnsbufferWidth).foreach(index => 
    when(io.deq(index).fire) {
      valids(deq_flag + index.U) := 0.B
      if (EnsbufferWidth > 1) deq_flag := deq_flag + index.U + 1.U
    }
  )

  (0 until EnsbufferWidth).foreach(index =>
    when(io.enq(index).fire) {
      valids(enq_flag + index.U) := 1.B
      data(enq_flag + index.U) := io.enq(index).bits
      if (EnsbufferWidth > 1) enq_flag := enq_flag + index.U + 1.U
    }
  )
}
