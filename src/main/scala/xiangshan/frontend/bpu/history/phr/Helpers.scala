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

package xiangshan.frontend.bpu.history.phr

import chisel3._
import chisel3.util._
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.PhrHelper

trait Helpers extends HasPhrParameters with PhrHelper with HalfAlignHelper {
  // folded History
  def circularShiftLeft(src: UInt, shamt: Int): UInt = {
    val srcLen     = src.getWidth
    val srcDoubled = Cat(src, src)
    val shifted    = srcDoubled(srcLen * 2 - 1 - shamt, srcLen - shamt)
    shifted
  }

  // do xors for several bitsets at specified bits
  def bitsetsXor(info: FoldedHistoryInfo, bitsets: Seq[Seq[(Int, Bool)]]): UInt = {
    val res = Wire(Vec(info.FoldedLength, Bool()))
    // println(f"num bitsets: ${bitsets.length}")
    // println(f"bitsets $bitsets")
    val resArr = Array.fill(info.FoldedLength)(List[Bool]())
    for (bs <- bitsets) {
      for ((n, b) <- bs) {
        resArr(n) = b :: resArr(n)
      }
    }
    // println(f"${resArr.mkString}")
    // println(f"histLen: ${this.len}, foldedLen: $folded_len")
    for (i <- 0 until info.FoldedLength) {
      // println(f"bit[$i], ${resArr(i).mkString}")
      if (resArr(i).isEmpty) {
        println(f"[error] bits $i is not assigned in folded hist update logic! $info")
      }
      res(i) := resArr(i).foldLeft(false.B)(_ ^ _)
    }
    res.asUInt
  }

  def getBranchAddr(addr: PrunedAddr, cfiPosition: UInt): PrunedAddr = {
    val alignedAddr = Cat(getAlignedAddrUpper(addr), 0.U(FetchBlockAlignWidth.W))
    val brOffset    = Cat(cfiPosition, 0.U(instOffsetBits.W))
    PrunedAddrInit(alignedAddr + brOffset)
  }
}
