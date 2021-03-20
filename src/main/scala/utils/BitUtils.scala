package utils

import chisel3._
import chisel3.util._
import scala.math.min

object WordShift {
  def apply(data: UInt, wordIndex: UInt, step: Int) = (data << (wordIndex * step.U))
}

object MaskExpand {
 def apply(m: UInt) = Cat(m.asBools.map(Fill(8, _)).reverse)
}

object MaskData {
 def apply(oldData: UInt, newData: UInt, fullmask: UInt) = {
   (newData & fullmask) | (oldData & ~fullmask)
 }
}

object SignExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int) = {
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
      if (s >= stop) x else helper(s+s, x | (x >> s))
    helper(1, x)(width-1, 0)
  }
}

object OneHot {
  def OH1ToOH(x: UInt): UInt = (x << 1 | 1.U) & ~Cat(0.U(1.W), x)
  def OH1ToUInt(x: UInt): UInt = OHToUInt(OH1ToOH(x))
  def UIntToOH1(x: UInt, width: Int): UInt = ~((-1).S(width.W).asUInt << x)(width-1, 0)
  def UIntToOH1(x: UInt): UInt = UIntToOH1(x, (1 << x.getWidth) - 1)
  def checkOneHot(in: Bits): Unit = assert(PopCount(in) <= 1.U)
  def checkOneHot(in: Iterable[Bool]): Unit = assert(PopCount(in) <= 1.U)
}

object LowerMask {
  def apply(a: UInt, len: Int) = {
    ParallelOR((0 until len).map(i => a >> i.U))
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
  def apply(ptr: UInt, length: Integer) = UIntToOH(ptr)(length - 1, 0) - 1.U
}

object GetEvenBits {
  def apply(input: UInt): UInt = {
    VecInit((0 until input.getWidth/2).map(i => {input(2*i)})).asUInt
  }
}


object GetOddBits {
  def apply(input: UInt): UInt = {
    VecInit((0 until input.getWidth/2).map(i => {input(2*i+1)})).asUInt
  }
}

object XORFold {
  def apply(input: UInt, resWidth: Int): UInt = {
    require(resWidth > 0)
    val fold_range = input.getWidth / resWidth
    val value = ZeroExt(input, fold_range * resWidth)
    ParallelXOR((0 until fold_range).map(i => value(i*resWidth+resWidth-1, i*resWidth)))
  }
}
