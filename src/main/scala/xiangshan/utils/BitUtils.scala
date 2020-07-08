package xiangshan.utils

import chisel3._
import chisel3.util._

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
    if (aLen == len) a else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int) = {
    val aLen = a.getWidth
    if (aLen == len) a else Cat(0.U((len - aLen).W), a)
  }
}

object LowerMask {
  def apply(a: UInt, len: Int) = {
    (0 until len).map(i => a >> i.U).reduce(_|_)
  }
}

object LowestBit {
  def apply(a: UInt, len: Int) = {
    Mux(a(0), 1.U(len.W), Reverse(((0 until len).map(i => Reverse(a(len - 1, 0)) >> i.U).reduce(_|_) + 1.U) >> 1.U))
  }
}
