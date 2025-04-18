package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._

package object floatPoint {

  def RNE: UInt = 0.U(3.W)
  def RTZ: UInt = 1.U(3.W)
  def RDN: UInt = 2.U(3.W)
  def RUP: UInt = 3.U(3.W)
  def RMM: UInt = 4.U(3.W)

  class FPDecodeBundle extends Bundle {
    val expNotZero = Bool()
    val expIsZero = Bool()
    val expIsOnes = Bool()
    val sigNotZero = Bool()
    val sigIsZero = Bool()
    val isSubnormal = Bool()
    val isInf = Bool()
    val isZero = Bool()
    val isNaN = Bool()
    val isSNaN = Bool()
    val isQNaN = Bool()
  }

  class FloatPoint(val expWidth: Int, val precision: Int) extends Bundle {
    def sigWidth = precision - 1
    val sign = Bool()
    val exp = UInt(expWidth.W)
    val sig = UInt(sigWidth.W)
    def decode: FPDecodeBundle = {
      val expNotZero = exp.orR
      val expIsOnes = exp.andR
      val sigNotZero = sig.orR
      val bundle = Wire(new FPDecodeBundle)
      bundle.expNotZero := expNotZero
      bundle.expIsZero := !expNotZero
      bundle.expIsOnes := expIsOnes
      bundle.sigNotZero := sigNotZero
      bundle.sigIsZero := !sigNotZero
      bundle.isSubnormal := bundle.expIsZero && sigNotZero
      bundle.isInf := bundle.expIsOnes && bundle.sigIsZero
      bundle.isZero := bundle.expIsZero && bundle.sigIsZero
      bundle.isNaN := bundle.expIsOnes && bundle.sigNotZero
      bundle.isSNaN := bundle.isNaN && !sig.head(1).asBool
      bundle.isQNaN := bundle.isNaN && sig.head(1).asBool
      bundle
    }
  }
  object FloatPoint {
    def expBias(expWidth: Int): BigInt = {
      (BigInt(1) << (expWidth - 1)) - 1
    }
    def maxNormExp(expWidth: Int): BigInt = {
      (BigInt(1) << expWidth) - 2
    }
    def fromUInt(x: UInt, expWidth: Int, pc: Int): FloatPoint = {
      val fp = Wire(new FloatPoint(expWidth, pc))
      fp.sign := x(expWidth + pc - 1)
      fp.exp := x(expWidth + pc - 2, pc - 1)
      fp.sig := x(pc - 2, 0)
      fp
    }
    def defaultNaNUInt(expWidth: Int, pc: Int): UInt = {
      Cat(0.U(1.W), Fill(expWidth + 1, 1.U(1.W)), 0.U((pc - 2).W))
    }
    def defaultNaN(expWidth: Int, pc: Int): FloatPoint = {
      fromUInt(defaultNaNUInt(expWidth, pc), expWidth, pc)
    }
  }

  class RawFloat(val expWidth: Int, val precision: Int) extends Bundle {
    val sign = Bool()
    val exp = UInt(expWidth.W)
    val sig = UInt(precision.W)
  }

  object RawFloat {
    def fromFP(fp: FloatPoint, expNotZero: Option[Bool] = None): RawFloat = {
      val inner = Wire(new RawFloat(fp.expWidth, fp.precision))
      val nz = if (expNotZero.isDefined) expNotZero.get else fp.exp.orR
      inner.sign := fp.sign
      inner.exp := fp.exp | !nz
      inner.sig := Cat(nz, fp.sig)
      inner
    }

    def fromUInt(x: UInt, expWidth: Int, precision: Int): RawFloat = {
      val fp = FloatPoint.fromUInt(x, expWidth, precision)
      val raw = fromFP(fp)
      raw
    }
  }

}
