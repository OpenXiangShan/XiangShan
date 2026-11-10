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

class AMOALU(operandBits: Int) extends Module
  with MemoryOpConstants {
  val minWidth = 8
  val comparatorLeafWidth = 32
  require(operandBits >= comparatorLeafWidth && isPow2(operandBits))
  val widths = (0 to log2Ceil(operandBits / minWidth)).map(minWidth << _)
  val narrowWidths = widths.filter(_ < comparatorLeafWidth)
  val wideWidths = widths.filter(_ >= comparatorLeafWidth)

  val io = IO(new Bundle {
    val mask = Input(UInt((operandBits/8).W))
    val cmd = Input(Bits(M_SZ.W))
    val lhs = Input(Bits(operandBits.W))
    val rhs = Input(Bits(operandBits.W))
    val out = Output(Bits(operandBits.W))
    val out_unmasked = Output(Bits(operandBits.W))
  })

  val byteCount = PopCount(io.mask)
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val add = io.cmd === M_XA_ADD
  val logicAnd = io.cmd === M_XA_OR || io.cmd === M_XA_AND
  val logicXor = io.cmd === M_XA_XOR || io.cmd === M_XA_OR
  val signed = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX

  def laneAdd(width: Int): UInt = {
    Cat((0 until operandBits / width).reverse.map { lane =>
      val lo = lane * width
      io.lhs(lo + width - 1, lo) + io.rhs(lo + width - 1, lo)
    })
  }

  val wideAdderOut = {
    // Preserve the partitioned carry chain used by the original W/D/Q paths.
    val mask = ~(0.U(operandBits.W) +: wideWidths.init.map(w => !io.mask(w / 8 - 1) << (w - 1))).reduce(_ | _)
    (io.lhs & mask) + (io.rhs & mask)
  }
  val narrowAdderOut = Mux1H(narrowWidths.map(width =>
    (byteCount === (width / 8).U) -> laneAdd(width)
  ))
  val adderOut = Mux(byteCount < (comparatorLeafWidth / 8).U, narrowAdderOut, wideAdderOut)

  val less = {
    // Break up wide comparators so their lower parts can be shared by synthesis.
    def isLessUnsigned(x: UInt, y: UInt, width: Int): Bool = {
      if (width == comparatorLeafWidth) x(width - 1, 0) < y(width - 1, 0)
      else {
        val upperLess = x(width - 1, width / 2) < y(width - 1, width / 2)
        val upperEqual = x(width - 1, width / 2) === y(width - 1, width / 2)
        upperLess || upperEqual && isLessUnsigned(x, y, width / 2)
      }
    }

    def isLess(x: UInt, y: UInt, width: Int): Bool = {
      Mux(
        x(width - 1) === y(width - 1),
        isLessUnsigned(x, y, width),
        Mux(signed, x(width - 1), y(width - 1))
      )
    }

    def narrowLess(width: Int): Bool = {
      val bytes = width / 8
      Mux1H((0 until operandBits / width).map { lane =>
        val lo = lane * width
        io.mask(lane * bytes) -> Mux(
          signed,
          io.lhs(lo + width - 1, lo).asSInt < io.rhs(lo + width - 1, lo).asSInt,
          io.lhs(lo + width - 1, lo) < io.rhs(lo + width - 1, lo)
        )
      })
    }

    val narrowLessResult = Mux1H(narrowWidths.map(width =>
      (byteCount === (width / 8).U) -> narrowLess(width)
    ))
    val wideLess = PriorityMux(wideWidths.reverse.map(width =>
      io.mask(width / 8 / 2) -> isLess(io.lhs, io.rhs, width)
    ))
    Mux(byteCount < (comparatorLeafWidth / 8).U, narrowLessResult, wideLess)
  }

  val minmax = Mux(Mux(less, min, max), io.lhs, io.rhs)
  val logic =
    Mux(logicAnd, io.lhs & io.rhs, 0.U) |
    Mux(logicXor, io.lhs ^ io.rhs, 0.U)
  val out =
    Mux(add, adderOut,
    Mux(logicAnd || logicXor, logic,
                                minmax))

  val wmask = FillInterleaved(8, io.mask)
  io.out := wmask & out | ~wmask & io.lhs
  io.out_unmasked := out
}
