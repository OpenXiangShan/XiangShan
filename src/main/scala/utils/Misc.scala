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
import chisel3.util.random.LFSR

// This gets used everywhere, so make the smallest circuit possible ...
// Given an address and size, create a mask of beatBytes size
// eg: (0x3, 0, 4) => 0001, (0x3, 1, 4) => 0011, (0x3, 2, 4) => 1111
// groupBy applies an interleaved OR reduction; groupBy=2 take 0010 => 01
object MaskGen {
  def apply(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = {
    require (groupBy >= 1 && beatBytes >= groupBy)
    require (isPow2(beatBytes) && isPow2(groupBy))
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize | 0.U(log2Up(beatBytes).W), log2Up(beatBytes)) | (groupBy*2 - 1).U

    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= lgBytes.U, true.B))
      } else {
        val sub = helper(i-1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }

    if (groupBy == beatBytes) 1.U else
      Cat(helper(lgBytes-log2Ceil(groupBy)).map(_._1).reverse)
  }
}

object Random
{
  def apply(mod: Int, random: UInt): UInt = {
    if (mod == 1) 0.U
    else if (isPow2(mod)) random(log2Ceil(mod)-1,0)
    else PriorityEncoder(partition(apply(1 << log2Up(mod*8), random), mod))
  }
  def apply(mod: Int): UInt = apply(mod, randomizer)
  def oneHot(mod: Int, random: UInt): UInt = {
    if (mod == 1) 1.U(1.W)
    else if (isPow2(mod)) UIntToOH(random(log2Up(mod)-1,0))
    else VecInit(PriorityEncoderOH(partition(apply(1 << log2Up(mod*8), random), mod))).asUInt
  }
  def oneHot(mod: Int): UInt = oneHot(mod, randomizer)

  private def randomizer = LFSR(16)
  private def partition(value: UInt, slices: Int) =
    Seq.tabulate(slices)(i => value < (((i + 1) << value.getWidth) / slices).U)
}


/**
 * Transpose a matrix of Chisel Vecs.
 */
object Transpose
{
  def apply[T <: chisel3.Data](in: Vec[Vec[T]]) = {
    val n = in(0).size
    VecInit((0 until n).map(i => VecInit(in.map(row => row(i)))))
  }
}
