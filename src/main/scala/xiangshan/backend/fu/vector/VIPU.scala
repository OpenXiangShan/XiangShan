/****************************************************************************************
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
 ****************************************************************************************
 */


package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.backend.fu.FunctionUnit
import yunsuan.vector.VectorIntAdder

class VIPU(implicit p: Parameters) extends FunctionUnit(/*len = 128/VLEN*/) {
  val uop = io.in.bits.uop

  val AdderWidth = 64
  val NumAdder = 1 // TODO: replace with VLEN/AdderWidth when rf ready
  val adder = Seq.fill(NumAdder)(Module(new VectorIntAdder()))
  for(i <- 0 until NumAdder) {
    adder(i).io.in_0 := io.in.bits.src(0)(AdderWidth*(i+1)-1, AdderWidth*i)
    adder(i).io.in_1 := io.in.bits.src(1)(AdderWidth*(i+1)-1, AdderWidth*i)
    adder(i).io.int_format := DontCare // TODO
    adder(i).io.op_code := DontCare // TODO
    adder(i).io.carry_or_borrow_in := DontCare
  }
  val adder_result = VecInit(adder.map(_.io.out)).asUInt

  io.out.bits.data := adder_result
  io.out.bits.uop := io.in.bits.uop
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
}