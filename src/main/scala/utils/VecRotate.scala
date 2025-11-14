// Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package utils

import chisel3._
import chisel3.util._
import math.pow

/** Utility class for Vec[T] rotation
 * @example {{{
 *   // rotate right by 3
 *   val s0_rotateNum = 3.U(3.W)
 *   val s0_rotator = VecRotate(rotateNum, storeOneHot = true)
 *   val s0_vec = VecInit.tabulate(8)(i => i.U(8.W))      // 0,1,2,3,4,5,6,7
 *   val s0_rotatedVec = rotator.rotate(vec)              // 5,6,7,0,1,2,3,4
 *   // we can do some operations to rotatedVec, for example, vec addition
 *   adder.io.in1 := s0_rotatedVec
 *   adder.io.in2 := s0_rotatedVec
 *   // the processing may take some cycles, lets say 1 cycle here
 *   val s1_processedVec := adder.io.out                  // 10,12,14,0,2,4,6,8
 *   // register the rotator to align the pipeline stage
 *   // if we're using storeOneHot = true, here uses 8 bits of register. If not, only 3 bits are needed
 *   val s1_rotator = RegNext(s0_rotator)
 *   // if we're using storeOneHot = true, in this `revert` call, no decoder(i.e. UIntToOH) is needed
 *   val s1_finalVec = s1_rotator.revert(s1_processedVec) // 0,2,4,6,8,10,12,14
 * }}}
 */
class VecRotate(
    vecLength:   Int,
    storeOneHot: Boolean, // this is only useful when we do Reg(VecRotate)
    direction:   Boolean
) extends Bundle {
  val n: UInt = UInt((if (storeOneHot) vecLength else log2Ceil(vecLength)).W)

  private def getRotatedIdx(idx: Int, num: Int, direction: Boolean): Int =
    if (direction == VecRotate.Direction.Right)
      (idx + vecLength - num) % vecLength
    else
      (idx + num) % vecLength

  private def doRotate[T <: Data](vec: Vec[T], direction: Boolean): Vec[T] = {
    require(vec.length == vecLength, s"Input Vec length ${vec.length} does not match the required length $vecLength")
    // generate one-hot encoding of n if needed
    val idxOH = if (storeOneHot) n else UIntToOH(n)
    // generate all possible rotations
    val rotations = VecInit((0 until vecLength).map { rotateNum =>
      VecInit((0 until vecLength).map(i => vec(getRotatedIdx(i, rotateNum, direction))))
    })
    // select the correct rotation
    Mux1H(idxOH, rotations)
  }

  def rotate[T <: Data](vec: Vec[T]): Vec[T] =
    doRotate(vec, direction)

  def revert[T <: Data](vec: Vec[T]): Vec[T] =
    doRotate(vec, !direction)
}

object VecRotate {
  /* Direction constants
   * (0,1,2,3) rotated left by 1 -> (1,2,3,0)
   * (0,1,2,3) rotated right by 1 -> (3,0,1,2)
   * NOTE: If you do a Vec[Bool].asUInt.rotateLeft, it's actually doing a VecRotate(vec, Right).asUInt.
   *       This is because the MSB is on the left side in Chisel's UInt representation,
   *       but we usually think of the LSB on the left side in vector representation
   */
  object Direction {
    def Right: Boolean = false
    def Left:  Boolean = true
  }

  def apply(
      rotateNum:   UInt,
      storeOneHot: Boolean = false,
      direction:   Boolean = Direction.Right
  ): VecRotate = {
    val rotator = Wire(new VecRotate(pow(2, rotateNum.getWidth).toInt, storeOneHot, direction))
    rotator.n := (if (storeOneHot) UIntToOH(rotateNum) else rotateNum)
    rotator
  }
}
