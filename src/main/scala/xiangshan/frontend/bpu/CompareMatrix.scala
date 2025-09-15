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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._

class CompareMatrix(n: Int) extends Bundle {
  val m: Vec[Vec[Bool]] = Wire(Vec(n, Vec(n, Bool())))

  def apply(i: Int): Vec[Bool] = m(i)

  /** Get the one-hot vector indicating the least element among all valid elements
   * @param valid: a vector indicating which index is valid
   * @return a one-hot vector indicating the least element among all valid elements
   *
   * @example {{{
   *   val position = VecInit(Seq(3.U, 1.U, 2.U, 4.U))
   *   val valid    = VecInit(Seq(true.B, false.B, true.B, false.B))
   *   // by default order = "<", so the "least" element is the smallest
   *   val compareMatrix  = CompareMatrix(position)
   *   val leastElementOH = compareMatrix.getLeastElementOH(valid)
   *   // leastElementOH = VecInit(Seq(false.B, false.B, true.B, false.B)) // 2.U is the least(smallest) valid
   *   val leastElementIdx = OHToUInt(leastElementOH)
   *   // leastElementIdx = 2.U
   *   val leastElementPosition = Mux1H(leastElementOH, position)
   *   // leastElementPosition = 2.U
   * }}}
   *
   * @example {{{
   *   val position = VecInit(Seq(3.U, 1.U, 2.U, 4.U))
   *   val valid    = VecInit(Seq(true.B, false.B, true.B, false.B))
   *   // we specify order = ">", so the "least" element is the greatest
   *   val compareMatrix  = CompareMatrix(position, order = (a: UInt, b: UInt) => a > b)
   *   val leastElementOH = compareMatrix.getLeastElementOH(valid)
   *   // leastElementOH = VecInit(Seq(true.B, false.B, false.B, false.B)) // 3.U is the least(greatest) valid
   *   val leastElementIdx = OHToUInt(leastElementOH)
   *   // leastElementIdx = 0.U
   *   val leastElementPosition = Mux1H(leastElementOH, position)
   *   // leastElementPosition = 3.U
   * }}}
   */
  def getLeastElementOH(valid: Vec[Bool]): Vec[Bool] = {
    require(valid.length == n, "valid length must be equal to matrix size")
    VecInit((0 until n).map { i =>
      // i must be valid
      // and, for every j != i, j must not be valid, or order(i, j) (i.e. compareMatrix(i)(j) == true)
      valid(i) && (0 until n).map(j => (i == j).B || !valid(j) || m(i)(j)).reduce(_ && _)
    })
  }

  def getLeaseElementIdx(valid: Vec[Bool]): UInt =
    OHToUInt(getLeastElementOH(valid))

  def getLeastElement[T <: Data](valid: Vec[Bool], value: Vec[T]): T =
    Mux1H(getLeastElementOH(valid), value)

  def getLeastElement[T <: Data](valid: T => Bool, value: Vec[T]): T =
    getLeastElement(VecInit(value.map(valid)), value)
}

object CompareMatrix {

  /**
   * Generate a comparison matrix for a vector of UInt
   * @param value: the vector to be compared
   * @param idx: the index mapping function, default is identity mapping
   * @param order: the comparison function, default is <
   * @return a matrix m, where m(i)(j) = order(value(idx(i)), value(idx(j))) if i != j, else false
   *
   * @example {{{
   *   val position = VecInit(Seq(3.U, 1.U, 2.U, 4.U))
   *   val compareMatrix = CompareMatrix(position)
   *   // compareMatrix = (
   *   //   ( false.B    , false.B(3<1), false.B(3<2),  true.B(3<4)),
   *   //   ( true.B(1<3),      false.B,  true.B(1<2),  true.B(1<4)),
   *   //   ( true.B(2<3), false.B(2<1),      false.B,  true.B(2<4)),
   *   //   (false.B(4<3), false.B(4<1), false.B(4<2),      false.B),
   *   // )
   * }}}
   */
  def apply(
      value: Vec[UInt],
      idx:   Int => UInt = (i: Int) => i.U,
      order: (UInt, UInt) => Bool = (a: UInt, b: UInt) => a < b
  ): CompareMatrix = {
    val n = value.length
    val m = new CompareMatrix(n)
    (0 until n).foreach { i =>
      (0 until n).foreach { j =>
        if (i == j)
          m.m(i)(j) := false.B
        else if (i < j)
          m.m(i)(j) := order(value(idx(i)), value(idx(j)))
        else // if i > j, we assume order is antisymmetric, i.e. order(a, b) => !order(b, a)
          m.m(i)(j) := !m.m(j)(i)
      }
    }
    m
  }
}
