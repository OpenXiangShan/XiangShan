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
import xiangshan.cache._
import difftest._

class DatamoduleResultBufferIO[T <: Data](gen: T) extends Bundle
{
  // val flush = Input(Bool())
  val enq = Vec(2, Flipped(DecoupledIO(gen)))
  val deq = Vec(2, DecoupledIO(gen))

}

class DatamoduleResultBuffer[T <: Data]
(
  gen: T,
) extends Module {

  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new DatamoduleResultBufferIO[T](gen))

  val data = Reg(Vec(2, genType))
  val valids = RegInit(VecInit(Seq.fill(2)(false.B)))
  val enq_flag = RegInit(false.B) // head is entry 0
  val deq_flag = RegInit(false.B) // tail is entry 0

  val entry_allowin = Wire(Vec(2, Bool()))

  io.deq(0).valid := Mux(deq_flag,
    valids(1),
    valids(0)
  )
  io.deq(1).valid := Mux(deq_flag,
    valids(0),
    valids(1)
  ) && io.deq(0).valid

  io.deq(0).bits := Mux(deq_flag,
    data(1),
    data(0)
  )
  io.deq(1).bits := Mux(deq_flag,
    data(0),
    data(1)
  )

  assert(!(io.deq(1).valid && !io.deq(0).valid))
  assert(!(io.deq(1).ready && !io.deq(0).ready))

  entry_allowin(0) := !valids(0) ||
    io.deq(0).fire() && !deq_flag ||
    io.deq(1).fire() && deq_flag
  entry_allowin(1) := !valids(1) ||
    io.deq(0).fire() && deq_flag ||
    io.deq(1).fire() && !deq_flag

  io.enq(0).ready := Mux(enq_flag,
    entry_allowin(1),
    entry_allowin(0)
  )
  io.enq(1).ready := Mux(enq_flag,
    entry_allowin(0),
    entry_allowin(1)
  ) && io.enq(0).ready

  assert(!(io.enq(1).ready && !io.enq(0).ready))
  assert(!(io.enq(1).valid && !io.enq(0).valid))

  when(io.deq(0).fire()){
    when(deq_flag){
      valids(1) := false.B
    }.otherwise{
      valids(0) := false.B
    }
    deq_flag := ~deq_flag
  }
  when(io.deq(1).fire()){
    when(deq_flag){
      valids(0) := false.B
    }.otherwise{
      valids(1) := false.B
    }
    deq_flag := deq_flag
  }

  when(io.enq(0).fire()){
    when(enq_flag){
      valids(1) := true.B
      data(1) := io.enq(0).bits
    }.otherwise{
      valids(0) := true.B
      data(0) := io.enq(0).bits
    }
    enq_flag := ~enq_flag
  }
  when(io.enq(1).fire()){
    when(enq_flag){
      valids(0) := true.B
      data(0) := io.enq(1).bits
    }.otherwise{
      valids(1) := true.B
      data(1) := io.enq(1).bits
    }
    enq_flag := enq_flag
  }
}
