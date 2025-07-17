// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.phr

import chisel3._
import chisel3.util._
import scala.math.min
import utility.ParallelXOR

trait Helpers extends HasPhrParameters {
  // folded History
  def circularShiftLeft(src: UInt, shamt: Int) = {
    val srcLen     = src.getWidth
    val srcDoubled = Cat(src, src)
    val shifted    = srcDoubled(srcLen * 2 - 1 - shamt, srcLen - shamt)
    shifted
  }
  // do xors for several bitsets at specified bits
  def bitsetsXor(len: Int, bitsets: Seq[Seq[Tuple2[Int, Bool]]], hisLen: Int, compLen: Int) = {
    val res = Wire(Vec(len, Bool()))
    // println(f"num bitsets: ${bitsets.length}")
    // println(f"bitsets $bitsets")
    val resArr = Array.fill(len)(List[Bool]())
    for (bs <- bitsets) {
      for ((n, b) <- bs) {
        resArr(n) = b :: resArr(n)
      }
    }
    // println(f"${resArr.mkString}")
    // println(f"histLen: ${this.len}, foldedLen: $folded_len")
    for (i <- 0 until len) {
      // println(f"bit[$i], ${resArr(i).mkString}")
      if (resArr(i).length == 0) {
        println(f"[error] bits $i is not assigned in folded hist update logic! histlen:$hisLen, compLen:$compLen")
      }
      res(i) := resArr(i).foldLeft(false.B)(_ ^ _)
    }
    res.asUInt
  }
  def computeFoldedHist(hist: UInt, compLen: Int)(histLen: Int): UInt =
    if (histLen > 0) {
      val nChunks     = (histLen + compLen - 1) / compLen
      val hist_chunks = (0 until nChunks) map { i => hist(min((i + 1) * compLen, histLen) - 1, i * compLen) }
      ParallelXOR(hist_chunks)
    } else 0.U

}
