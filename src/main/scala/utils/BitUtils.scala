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

object RegNextWithEnable {
  def apply[T <: Data](data: Valid[T], hasInit: Boolean = true): Valid[T] = {
    val next = Wire(data.cloneType)
    if (hasInit) {
      next.valid := RegNext(data.valid, false.B)
    }
    else {
      next.valid := RegNext(data.valid)
    }
    next.bits := RegEnable(data.bits, data.valid)
    next
  }
}

class CircularShift(data: UInt) {
  private def helper(step: Int, isLeft: Boolean): UInt = {
    if (step == 0) {
      data
    }
    else {
      val splitIndex = if (isLeft) {
        data.getWidth - (step % data.getWidth)
      } else {
        step % data.getWidth
      }
      Cat(data(splitIndex - 1, 0), data(data.getWidth - 1, splitIndex))
    }
  }
  def left(step: Int): UInt = helper(step, true)
  def right(step: Int): UInt = helper(step, false)
}

object CircularShift {
  def apply(data: UInt): CircularShift = new CircularShift(data)
}

object WordShift {
  def apply(data: UInt, wordIndex: UInt, step: Int): UInt = (data << (wordIndex * step.U)).asUInt
}

object MaskExpand {
  def apply(m: UInt, maskWidth: Int = 8): UInt = Cat(m.asBools.map(Fill(maskWidth, _)).reverse)
  def apply(m: Seq[Bool], maskWidth: Int): Vec[UInt] = VecInit(m.map(Fill(maskWidth, _)))
}

object MaskData {
  def apply(oldData: UInt, newData: UInt, fullmask: UInt): UInt = {
    require(oldData.getWidth <= fullmask.getWidth, s"${oldData.getWidth} < ${fullmask.getWidth}")
    require(newData.getWidth <= fullmask.getWidth, s"${newData.getWidth} < ${fullmask.getWidth}")
    (newData & fullmask) | (oldData & (~fullmask).asUInt)
  }
}

object SignExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}

object Or {
  // Fill 1s from low bits to high bits
  def leftOR(x: UInt): UInt = leftOR(x, x.getWidth, x.getWidth)
  def leftOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)
    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s+s, x | (x << s)(width-1,0))
    helper(1, x)(width-1, 0)
  }

  // Fill 1s form high bits to low bits
  def rightOR(x: UInt): UInt = rightOR(x, x.getWidth, x.getWidth)
  def rightOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)
    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s+s, x | (x >> s).asUInt)
    helper(1, x)(width-1, 0)
  }
}

object OneHot {
  def OH1ToOH(x: UInt): UInt = ((x << 1).asUInt | 1.U) & (~Cat(0.U(1.W), x)).asUInt
  def OH1ToUInt(x: UInt): UInt = OHToUInt(OH1ToOH(x))
  def UIntToOH1(x: UInt, width: Int): UInt = (~((-1).S(width.W).asUInt << x)(width-1, 0)).asUInt
  def UIntToOH1(x: UInt): UInt = UIntToOH1(x, (1 << x.getWidth) - 1)
  def checkOneHot(in: Bits): Unit = assert(PopCount(in) <= 1.U)
  def checkOneHot(in: Iterable[Bool]): Unit = assert(PopCount(in) <= 1.U)
}

object LowerMask {
  def apply(a: UInt, len: Int): UInt = {
    ParallelOR((0 until len).map(i => (a >> i).asUInt))
  }
  def apply(a: UInt): UInt = {
    apply(a, a.getWidth)
  }
}

object HigherMask {
  def apply(a: UInt, len: Int) = {
    Reverse(LowerMask(Reverse(a), len))
  }
  def apply(a: UInt): UInt = {
    apply(a, a.getWidth)
  }
}

object LowerMaskFromLowest {
  def apply(a: UInt) = {
    LowerMask(PriorityEncoderOH(a))
  }
}

object HigherMaskFromHighest {
  def apply(a: UInt) = {
    Reverse(LowerMask(PriorityEncoderOH(Reverse(a))))
  }
}

object LowestBit {
  def apply(a: UInt, len: Int) = {
    Mux(a(0), 1.U(len.W), Reverse((ParallelOR((0 until len).map(i => Reverse(a(len - 1, 0)) >> i.U)) + 1.U) >> 1.U))
  }
}

object HighestBit {
  def apply(a: UInt, len: Int) = {
    Reverse(LowestBit(Reverse(a), len))
  }
}

object GenMask {
  // generate w/r mask
  def apply(high: Int, low: Int) = {
    require(high > low)
    (VecInit(List.fill(high+1)(true.B)).asUInt >> low << low).asUInt()
  }
  def apply(pos: Int) = {
    (1.U << pos).asUInt()
  }
}

object UIntToMask {
  def apply(ptr: UInt, length: Integer) = leftmask(ptr, length)
  def reverseUInt(input: UInt): UInt = {
    VecInit(input.asBools.reverse).asUInt
  }
  def leftmask(ptr: UInt, length: Integer) = UIntToOH(ptr)(length - 1, 0) - 1.U
  def rightmask(ptr: UInt, length: Integer) = reverseUInt(reverseUInt(UIntToOH(ptr)(length - 1, 0)) - 1.U)
}

object GetEvenBits {
  def apply(input: UInt): UInt = {
    VecInit((0 until input.getWidth/2).map(i => {input(2*i)})).asUInt
  }
  def reverse(input: UInt): UInt = {
    VecInit((0 until input.getWidth * 2).map(i => {
      if(i % 2 == 0) input(i/2) else false.B 
    })).asUInt
  }
}


object GetOddBits {
  def apply(input: UInt): UInt = {
    VecInit((0 until input.getWidth/2).map(i => {input(2*i+1)})).asUInt
  }
  def reverse(input: UInt): UInt = {
    VecInit((0 until input.getWidth * 2).map(i => {
      if(i % 2 == 0) false.B else input(i/2) 
    })).asUInt
  }
}

object GetRemBits {
  def apply(div: Int)(input: UInt): Seq[UInt] = {
    (0 until div).map(rem => VecInit((0 until input.getWidth / div).map(i => input(div * i + rem))).asUInt)
  }
  def reverse(div: Int)(input: Seq[UInt]): Seq[UInt] = {
    (0 until div).map(rem => VecInit((0 until input(rem).getWidth * div).map(i => {
      if (i % div == rem) input(rem)(i / div) else 0.B
    })).asUInt)
  }
}

object XORFold {
  def apply(input: UInt, resWidth: Int): UInt = {
    require(resWidth > 0)
    val fold_range = (input.getWidth + resWidth - 1) / resWidth
    val value = ZeroExt(input, fold_range * resWidth)
    ParallelXOR((0 until fold_range).map(i => value(i*resWidth+resWidth-1, i*resWidth)))
  }
}

object OnesMoreThan {
  def apply(input: Seq[Bool], thres: Int): Bool = {
    if (thres == 0) {
      true.B
    }
    else if (input.length < thres) {
      false.B
    }
    else if (thres == 1) {
      VecInit(input).asUInt.orR
    }
    else {
      val tail = input.drop(1)
      input(0) && OnesMoreThan(tail, thres - 1) || OnesMoreThan(tail, thres)
    }
  }
}

abstract class SelectOne {
  protected val balance2 = RegInit(false.B)
  balance2 := !balance2

  // need_balance: for balanced selections only (DO NOT use this if you don't know what it is)
  def getNthOH(n: Int, need_balance: Boolean = false): (Bool, Vec[Bool])
  def getBalance2: Bool = balance2
}

class NaiveSelectOne(bits: Seq[Bool], max_sel: Int = -1) extends SelectOne {
  val n_bits = bits.length
  val n_sel = if (max_sel > 0) max_sel else n_bits
  require(n_bits > 0 && n_sel > 0 && n_bits >= n_sel)
  private val matrix = Wire(Vec(n_bits, Vec(n_sel, Bool())))
  // matrix[i][j]: first i bits has j one's
  for (i <- 0 until n_bits) {
    for (j <- 0 until n_sel) {
      if (j == 0) {
        matrix(i)(j) := (if (i == 0) true.B else !Cat(bits.take(i)).orR)
      }
      // it's impossible to select j-th one from i elements
      else if (i < j) {
        matrix(i)(j) := false.B
      }
      else {
        matrix(i)(j) := bits(i - 1) && matrix(i - 1)(j - 1) || !bits(i - 1) && matrix(i - 1)(j)
      }
    }
  }

  def getNthOH(n: Int, need_balance: Boolean = false): (Bool, Vec[Bool]) = {
    require(n > 0, s"$n should be positive to select the n-th one")
    require(n <= n_sel, s"$n should not be larger than $n_sel")
    // bits(i) is true.B and bits(i - 1, 0) has n - 1
    val selValid = OnesMoreThan(bits, n)
    val sel = VecInit(bits.zip(matrix).map{ case (b, m) => b && m(n - 1) })
    (selValid, sel)
  }
}

class CircSelectOne(bits: Seq[Bool], max_sel: Int = -1) extends SelectOne {
  val n_bits = bits.length
  val n_sel = if (max_sel > 0) max_sel else n_bits
  require(n_bits > 0 && n_sel > 0 && n_bits >= n_sel)

  val sel_forward = new NaiveSelectOne(bits, (n_sel + 1) / 2)
  val sel_backward = new NaiveSelectOne(bits.reverse, n_sel / 2)
  val moreThan = Seq(1, 2).map(i => OnesMoreThan(bits, i))

  def getNthOH(n: Int, need_balance: Boolean = false): (Bool, Vec[Bool]) = {
    require(!need_balance || max_sel == 2, s"does not support load balance between $max_sel selections")
    val selValid = if (!need_balance) {
      OnesMoreThan(bits, n)
    } else {
      if (n == 1) {
        // When balance2 bit is set, we prefer the second selection port.
        Mux(balance2, moreThan.last, moreThan.head)
      }
      else {
        require(n == 2)
        Mux(balance2, moreThan.head, moreThan.last)
      }
    }
    val sel_index = (n + 1) / 2
    if (n % 2 == 1) {
      (selValid, sel_forward.getNthOH(sel_index, need_balance)._2)
    }
    else {
      (selValid, VecInit(sel_backward.getNthOH(sel_index, need_balance)._2.reverse))
    }
  }
}

class OddEvenSelectOne(bits: Seq[Bool], max_sel: Int = -1) extends SelectOne {
  val n_bits = bits.length
  val n_sel = if (max_sel > 0) max_sel else n_bits
  require(n_bits > 0 && n_sel > 0 && n_bits >= n_sel)
  require(n_sel > 1, "Select only one entry via OddEven causes odd entries to be ignored")

  val n_even = (n_bits + 1) / 2
  val sel_even = new CircSelectOne((0 until n_even).map(i => bits(2 * i)), n_sel / 2)
  val n_odd = n_bits / 2
  val sel_odd = new CircSelectOne((0 until n_odd).map(i => bits(2 * i + 1)), (n_sel + 1) / 2)

  def getNthOH(n: Int, need_balance: Boolean = false): (Bool, Vec[Bool]) = {
    val sel_index = (n + 1) / 2
    if (n % 2 == 1) {
      val selected = sel_even.getNthOH(sel_index, need_balance)
      val sel = VecInit((0 until n_bits).map(i => if (i % 2 == 0) selected._2(i / 2) else false.B))
      (selected._1, sel)
    }
    else {
      val selected = sel_odd.getNthOH(sel_index, need_balance)
      val sel = VecInit((0 until n_bits).map(i => if (i % 2 == 1) selected._2(i / 2) else false.B))
      (selected._1, sel)
    }
  }
}

class CenterSelectOne(bits: Seq[Bool], max_sel: Int = -1) extends SelectOne {
  require(max_sel == 2, "only 2 is supported!")
  val n_bits = bits.length
  val half_index = (bits.length + 1) / 2
  def centerReverse(data: Seq[Bool]): Seq[Bool] = data.take(half_index).reverse ++ data.drop(half_index).reverse
  val select = new CircSelectOne(centerReverse(bits), max_sel)

  def getNthOH(n: Int, need_balance: Boolean): (Bool, Vec[Bool]) = {
    val selected = select.getNthOH(n)
    (selected._1, VecInit(centerReverse(selected._2)))
  }
}

object SelectOne {
  def apply(policy: String, bits: Seq[Bool], max_sel: Int = -1): SelectOne = {
    policy.toLowerCase match {
      case "naive" => new NaiveSelectOne(bits, max_sel)
      case "circ" => new CircSelectOne(bits, max_sel)
      case "oddeven" => new OddEvenSelectOne(bits, max_sel)
      case "center" => new CenterSelectOne(bits, max_sel)
      case _ => throw new IllegalArgumentException(s"unknown select policy")
    }
  }
}
