/***************************************************************************************
 * * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * *
 * * XiangShan is licensed under Mulan PSL v2.
 * * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * * You may obtain a copy of Mulan PSL v2 at:
 * *          http://license.coscl.org.cn/MulanPSL2
 * *
 * * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * *
 * * See the Mulan PSL v2 for more details.
 * ***************************************************************************************/

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.issue.AgeDetector

class InputBuffer(numEntries: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))

    val in = Flipped(DecoupledIO(new FunctionUnitInput(XLEN)))
    val out = DecoupledIO(new FunctionUnitInput(XLEN))
  })

  val data = Reg(Vec(numEntries, new FunctionUnitInput(XLEN)))
  val emptyVec = RegInit(VecInit.fill(numEntries)(true.B))
  val emptyVecNext = WireInit(emptyVec)
  emptyVec := emptyVecNext

  val selectEnq = SelectOne("naive", emptyVec, 1).getNthOH(1)
  val hasEmpty = RegInit(true.B)
  hasEmpty := emptyVecNext.asUInt.orR
  io.in.ready := hasEmpty
  val enqVec = selectEnq._2

  // enqueue
  val doEnqueue = io.in.fire && !io.in.bits.uop.robIdx.needFlush(io.redirect)
  when (doEnqueue) {
    for (i <- 0 until numEntries) {
      when (enqVec(i)) {
        data(i) := io.in.bits
        emptyVecNext(i) := false.B
      }
    }
  }

  // dequeue
  val age = Module(new AgeDetector(numEntries, 1))
  age.io.enq(0) := Mux(doEnqueue, enqVec.asUInt, 0.U)

  val isEmpty = RegInit(false.B)
  isEmpty := !emptyVecNext.asUInt.andR
  io.out.valid := isEmpty
  io.out.bits := Mux1H(age.io.out, data)
  when (io.out.fire) {
    for (i <- 0 until numEntries) {
      when (age.io.out(i)) {
        emptyVecNext(i) := true.B
        XSError(emptyVec(i), "should not deq an empty entry\n")
      }
    }
  }

  // flush
  val flushVec = data.map(_.uop.robIdx).zip(emptyVec).map{ case (r, e) => !e && r.needFlush(io.redirect) }
  for (i <- 0 until numEntries) {
    when (flushVec(i)) {
      emptyVecNext(i) := true.B
    }
  }

  val flushDeq = VecInit(flushVec).asUInt
  age.io.deq := Mux(io.out.fire, age.io.out, 0.U) | flushDeq

  val numValid = PopCount(emptyVec.map(e => !e))
  XSPerfHistogram("num_valid", numValid, true.B, 0, numEntries, 1)
  XSPerfAccumulate("aver_num_valid", numValid)
}
