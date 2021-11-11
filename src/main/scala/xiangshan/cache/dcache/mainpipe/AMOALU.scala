// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

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

package xiangshan.cache

import chisel3._
import chisel3.util._

class StoreGen(typ: UInt, addr: UInt, dat: UInt, maxSize: Int) {
  val size = typ(log2Up(log2Up(maxSize)+1)-1,0)
  def misaligned =
    (addr & ((1.U << size) - 1.U)(log2Up(maxSize)-1,0)).orR

  def mask = {
    var res = 1.U
    for (i <- 0 until log2Up(maxSize)) {
      val upper = Mux(addr(i), res, 0.U) | Mux(size >= (i+1).U, ((BigInt(1) << (1 << i))-1).U, 0.U)
      val lower = Mux(addr(i), 0.U, res)
      res = Cat(upper, lower)
    }
    res
  }

  protected def genData(i: Int): UInt =
    if (i >= log2Up(maxSize)) dat
    else Mux(size === i.U, Fill(1 << (log2Up(maxSize)-i), dat((8 << i)-1,0)), genData(i+1))

  def data = genData(0)
  def wordData = genData(2)
}

class LoadGen(typ: UInt, signed: Bool, addr: UInt, dat: UInt, zero: Bool, maxSize: Int) {
  private val size = new StoreGen(typ, addr, dat, maxSize).size

  private def genData(logMinSize: Int): UInt = {
    var res = dat
    for (i <- log2Up(maxSize)-1 to logMinSize by -1) {
      val pos = 8 << i
      val shifted = Mux(addr(i), res(2*pos-1,pos), res(pos-1,0))
      val doZero = (i == 0).B && zero
      val zeroed = Mux(doZero, 0.U, shifted)
      res = Cat(Mux(size === i.U || doZero, Fill(8*maxSize-pos, signed && zeroed(pos-1)), res(8*maxSize-1,pos)), zeroed)
    }
    res
  }

  def wordData = genData(2)
  def data = genData(0)
}

class AMOALU(operandBits: Int) extends Module
  with MemoryOpConstants {
  val minXLen = 32
  val widths = (0 to log2Ceil(operandBits / minXLen)).map(minXLen << _)

  val io = IO(new Bundle {
    val mask = Input(UInt((operandBits/8).W))
    val cmd = Input(Bits(M_SZ.W))
    val lhs = Input(Bits(operandBits.W))
    val rhs = Input(Bits(operandBits.W))
    val out = Output(Bits(operandBits.W))
    val out_unmasked = Output(Bits(operandBits.W))
  })

  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val add = io.cmd === M_XA_ADD
  val logic_and = io.cmd === M_XA_OR || io.cmd === M_XA_AND
  val logic_xor = io.cmd === M_XA_XOR || io.cmd === M_XA_OR

  val adder_out = {
    // partition the carry chain to support sub-xLen addition
    val mask = ~(0.U(operandBits.W) +: widths.init.map(w => !io.mask(w/8-1) << (w-1))).reduce(_|_)
    (io.lhs & mask) + (io.rhs & mask)
  }

  val less = {
    // break up the comparator so the lower parts will be CSE'd
    def isLessUnsigned(x: UInt, y: UInt, n: Int): Bool = {
      if (n == minXLen) x(n-1, 0) < y(n-1, 0)
      else x(n-1, n/2) < y(n-1, n/2) || x(n-1, n/2) === y(n-1, n/2) && isLessUnsigned(x, y, n/2)
    }

    def isLess(x: UInt, y: UInt, n: Int): Bool = {
      val signed = {
        val mask = M_XA_MIN ^ M_XA_MINU
        (io.cmd & mask) === (M_XA_MIN & mask)
      }
      Mux(x(n-1) === y(n-1), isLessUnsigned(x, y, n), Mux(signed, x(n-1), y(n-1)))
    }

    PriorityMux(widths.reverse.map(w => (io.mask(w/8/2), isLess(io.lhs, io.rhs, w))))
  }

  val minmax = Mux(Mux(less, min, max), io.lhs, io.rhs)
  val logic =
    Mux(logic_and, io.lhs & io.rhs, 0.U) |
    Mux(logic_xor, io.lhs ^ io.rhs, 0.U)
  val out =
    Mux(add,                    adder_out,
    Mux(logic_and || logic_xor, logic,
                                minmax))

  val wmask = FillInterleaved(8, io.mask)
  io.out := wmask & out | ~wmask & io.lhs
  io.out_unmasked := out
}
