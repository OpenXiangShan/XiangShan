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

import scala.math.min

/** Scan an input signal with fixed length.
 * Set 1 at the end of the length-fixed scan chain if the scanned bits meet the condition.
 *
 * @example {{{
 * val data = Seq(false.B, true.B, true.B, false.B, true.B)
 * def condition(input: Seq[Bool]) : Bool = input(0) && input(1)
 * val onesLen2EndVec = FixLengthScanSetEnd(data, 2, condition)
 * assert(VecInit(onesLen2EndVec).asUInt === VecInit(Seq(false.B, false.B, true.B, false.B, false.B)).asUInt)
 * }}}
 */
object FixedLengthScanSetEnd {
  def apply(input: Seq[Bool], scanLen: Int, condition: Seq[Bool] => Bool) : Seq[Bool] = {
    require(scanLen > 0)
    val res: Vec[Bool] = VecInit(Seq.fill(input.size)(false.B))
    for (i <- (scanLen - 1) until input.size) {
      res(i) := condition((1 - scanLen until 1).map(_ + i).map(input(_)))
    }
    res
  }
}

object ConsecutiveOnesSetEnd {
  def apply(input: Seq[Bool], thres: Int): Seq[Bool] = {
    def condition(input: Seq[Bool]): Bool = input.reduce(_ && _)
    FixedLengthScanSetEnd(input, thres, condition)
  }
}

object ConsecutiveOnes {
  def apply(input: Seq[Bool], thres: Int): Bool = {
    require(thres > 0)
    ConsecutiveOnesSetEnd(input, thres).reduce(_ || _)
  }
}