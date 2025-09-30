package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._
import utility.SignExt

import math.pow

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

object doShiftRotateLeftWord {
  def apply(in: UInt, shamt: UInt): Bits = {
    val length = shamt.getWidth
    val amount = pow(2, length).intValue
    val shifted = doShiftRotateLeft(in, shamt)
    Cat(Fill(amount, shifted.head(1).asBool), shifted)
  }
}

object doShiftRotateRightWord {
  def apply(in: UInt, shamt: UInt): Bits = {
    val length = shamt.getWidth
    val amount = pow(2, length).intValue
    val shifted = doShiftRotateRight(in, shamt)
    Cat(Fill(amount, shifted.head(1).asBool), shifted)
  }
}
