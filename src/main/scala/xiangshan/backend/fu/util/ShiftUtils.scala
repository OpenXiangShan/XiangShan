package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._
import utility.SignExt

import math.pow

/** Elaborate a fixed-width barrel shifter, consuming shift bits from largest to smallest.
  * The shift encoding must not request a single stage wider than the input.
  */
object doShiftLeft {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftLeft(in, shamt)
  }

  def doShiftLeft[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val amount = pow(2, length-1).intValue
    val shifted = Mux(shift.head(1).asBool, Cat(in.tail(amount), 0.U(amount.W)), in)
    if (length == 1) shifted else doShiftLeft(shifted, shift.tail(1))
  }
}

/** Arithmetic right shift with sign fill at every combinational barrel stage. */
object doShiftRightArith {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftRightArith(in, shamt)
  }

  def doShiftRightArith[TI <: Bits, TS <: Bits](in : TI, shift: TS): Bits = {
    val length = shift.getWidth
    val amount = pow(2, length-1).intValue
    val tmp = in.getWidth - amount
    val shifted = Mux(shift.head(1).asBool, Cat(Fill(amount, in.head(tmp).head(1).asBool), in.head(tmp)), in)
    if (length == 1) shifted else doShiftRightArith(shifted, shift.tail(1))
  }
}

/** Fixed-width left rotate; concatenation wraps discarded high bits into the low end. */
object doShiftRotateLeft {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftRotateLeft(in, shamt)
  }

  def doShiftRotateLeft[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val amount = pow(2, length-1).intValue
    val shifted = Mux(shift.head(1).asBool, Cat(in.tail(amount), in.head(amount)), in)
    if (length == 1) shifted else doShiftRotateLeft(shifted, shift.tail(1))
  }
}

/** Fixed-width right rotate; concatenation wraps discarded low bits into the high end. */
object doShiftRotateRight {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftRotateRight(in, shamt)
  }

  def doShiftRotateRight[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val inWidth = in.getWidth
    val amount = pow(2, length-1).intValue
    val shifted = Mux(shift.head(1).asBool, Cat(in.tail(inWidth - amount), in.head(inWidth - amount)), in)
    if (length == 1) shifted else doShiftRotateRight(shifted, shift.tail(1))
  }
}

/** Rotate a word and append a sign-filled upper word (32-bit input, 5-bit shift in ALU). */
object doShiftRotateLeftWord {
  def apply(in: UInt, shamt: UInt): Bits = {
    val length = shamt.getWidth
    val amount = pow(2, length).intValue
    val shifted = doShiftRotateLeft(in, shamt)
    Cat(Fill(amount, shifted.head(1).asBool), shifted)
  }
}

/** Word right-rotate counterpart of doShiftRotateLeftWord, with the same extension. */
object doShiftRotateRightWord {
  def apply(in: UInt, shamt: UInt): Bits = {
    val length = shamt.getWidth
    val amount = pow(2, length).intValue
    val shifted = doShiftRotateRight(in, shamt)
    Cat(Fill(amount, shifted.head(1).asBool), shifted)
  }
}
