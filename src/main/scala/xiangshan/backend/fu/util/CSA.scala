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

package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._

abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(Vec(m, UInt(len.W)))
    val out = Output(Vec(n, UInt(len.W)))
  })
}

class CSA2_2(len: Int) extends CarrySaveAdderMToN(2, 2)(len) {
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b) = (io.in(0)(i), io.in(1)(i))
    val sum = a ^ b
    val cout = a & b
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}

class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len){
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b = a ^ b
    val a_and_b = a & b
    val sum = a_xor_b ^ cin
    val cout = a_and_b | (a_xor_b & cin)
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}

class CSA5_3(len: Int)extends CarrySaveAdderMToN(5, 3)(len){
  val FAs = Array.fill(2)(Module(new CSA3_2(len)))
  FAs(0).io.in := io.in.take(3)
  FAs(1).io.in := VecInit(FAs(0).io.out(0), io.in(3), io.in(4))
  io.out := VecInit(FAs(1).io.out(0), FAs(0).io.out(1), FAs(1).io.out(1))
}

class C22 extends CSA2_2(1)
class C32 extends CSA3_2(1)
class C53 extends CSA5_3(1)

