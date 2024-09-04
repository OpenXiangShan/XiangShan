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

import org.chipsalliance.cde.config.Parameters
import chisel3.experimental.requireIsChiselType
import chisel3.reflect.DataMirror
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache._
import xiangshan.backend.datapath.NewPipelineConnect
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

  private val size = EnsbufferWidth
  private val genType = {
    requireIsChiselType(gen)
    gen
  }

  val io = IO(new DatamoduleResultBufferIO[T](gen))

  val data = Reg(Vec(size, genType))
  val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  val enq_flag = RegInit(0.U(log2Up(size).W)) // head is entry 0
  val deq_flag = RegInit(0.U(log2Up(size).W)) // tail is entry 0

  val entry_allowin = Wire(Vec(size, Bool()))
  val port_allowout = Wire(Vec(EnsbufferWidth, Decoupled(gen)))

  (0 until EnsbufferWidth).foreach(index => {
    NewPipelineConnect(
      port_allowout(index), io.deq(index), io.deq(index).fire,
      false.B,
      Option(f"DatamoduleResultBufferToSbuffer${index}")
    )
  })

  (0 until EnsbufferWidth).foreach(index => {
    port_allowout(index).valid := valids(deq_flag + index.U) && (if (index == 0) 1.B else port_allowout(index - 1).valid)
    port_allowout(index).bits := data(deq_flag + index.U)
  })

  (1 until EnsbufferWidth).foreach(i => {
    assert(!(port_allowout(i).valid && !port_allowout(i - 1).valid))
    assert(!(port_allowout(i).ready && !port_allowout(i - 1).ready))
  })

  (0 until size).foreach(
    index => entry_allowin(index) := !valids(index) || (0 until EnsbufferWidth).map(i => port_allowout(i).fire && deq_flag + i.U === index.U).reduce(_ || _)
  )

  (0 until EnsbufferWidth).foreach(
    index => io.enq(index).ready := entry_allowin(enq_flag + index.U) && (if (index == 0) 1.B else io.enq(index - 1).ready)
  )

  (1 until EnsbufferWidth).foreach(i => {
    assert(!(io.enq(i).ready && !io.enq(i - 1).ready))
    assert(!(io.enq(i).valid && !io.enq(i - 1).valid))
  })

  (0 until EnsbufferWidth).foreach(index =>
    when(port_allowout(index).fire) {
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
