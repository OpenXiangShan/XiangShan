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

package xiangshan.backend.regcache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class RegCacheAgeDetector(numEntries: Int, numReplace: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ageInfo = Vec(numEntries, Vec(numEntries, Input(Bool())))
    val out = Vec(numReplace, Output(UInt(log2Up(numEntries).W)))
  })

  // age(i)(j): entry i is older than entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(true.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = {
    if (row < col)
      age(row)(col)
    else if (row == col)
      true.B
    else
      !age(col)(row)
  }

  //only for row <= col
  for((row, i) <- nextAge.zipWithIndex) {
    for((elem, j) <- row.zipWithIndex) {
      if (i == j) {
        // an entry is always older than itself
        elem := true.B
      }
      else if (i < j) {
        elem := io.ageInfo(i)(j)
      }
      else {
        elem := !nextAge(j)(i)
      }
      age(i)(j) := elem
    }
  }

  val rowOnesSum = (0 until numEntries).map(i => 
    PopCount((0 until numEntries).map(j => get_age(i, j)))
  )

  io.out.zipWithIndex.foreach { case (out, idx) =>
    out := PriorityMux(rowOnesSum.map(_ === (numEntries - idx).U).zip((0 until numEntries).map(_.U)))
    assert(PopCount(rowOnesSum.map(_ === (numEntries - idx).U)) === 1.U, s"row sum of replace entry ($idx) is not one-hot")
  }
}

object RegCacheAgeDetector {
  def apply(numEntries: Int, numReplace: Int, ageInfo: Vec[Vec[Bool]])(implicit p: Parameters): Vec[UInt] = {
    val age = Module(new RegCacheAgeDetector(numEntries, numReplace))
    age.io.ageInfo := ageInfo
    age.io.out
  }
}