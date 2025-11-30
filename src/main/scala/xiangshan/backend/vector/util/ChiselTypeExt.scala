package xiangshan.backend.vector.util

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

import scala.language.implicitConversions

object ChiselTypeExt {
  class BitPatExt(val bitPat: BitPat) {
    def ## (that: UInt): BitPat = this.bitPat ## BitPat(that)

    def head(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n <= this.bitPat.width)
      this.bitPat.apply(this.bitPat.width - 1, this.bitPat.width - n)
    }

    def tail(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.bitPat.width, "The width of result should be greater than 0")
      this.bitPat.apply(this.bitPat.width - n, 0)
    }

    def lsb(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n <= this.bitPat.width)
      this.bitPat.apply(n - 1, 0)
    }

    def msb(n: Int): BitPat = this.head(n)

    def drop(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.bitPat.width, "The width of result should be greater than 0")
      this.bitPat.apply(this.bitPat.width - 1, n)
    }

    def take(n: Int): BitPat = this.lsb(n)

    def pad0To(n: Int): BitPat = {
      if (bitPat.width >= n)
        this.bitPat
      else
        BitPat.N(n - bitPat.width) ## this.bitPat
    }

    def padDcTo(n: Int): BitPat = {
      if (bitPat.width >= n)
        this.bitPat
      else
        BitPat.dontCare(n - bitPat.width) ## this.bitPat
    }
  }

  class UIntExt(val value: UInt) {
    def toFixWidthBitPat(n: Int): BitPat = {
      BitPat(this.value.pad(n))
    }

    def toBitPat: BitPat = {
      BitPat(this.value)
    }

    def drop(n: Int): UInt = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.value.getWidth, "The width of result should be greater than 0")
      this.value.apply(this.value.getWidth - 1, n)
    }

    /**
     * Literal value cat
     * @param that: a literal UInt value whose width is known
     * @return a result of Literal value, the width is sum of two literal value.
     */
    def ###(that: UInt): UInt = {
      require(this.value.widthKnown && that.value.widthKnown)
      require(this.value.isLit && that.value.isLit)
      val thisWidth = this.value.getWidth
      val thatWidth = that.value.getWidth

      ((this.value.litValue << thatWidth) | that.litValue).U((thisWidth + thatWidth).W)
    }
  }

  class EnumTypeExt(val value: EnumType) {
    def toBitPat: BitPat = {
      new BitPat(value.litValue, (BigInt(1) << value.getWidth) - 1, value.getWidth)
    }
  }

  implicit def UIntToUIntField(uint: UInt): UIntExt = new UIntExt(uint)
  implicit def BitPatToExt(bitPat: BitPat): BitPatExt = new BitPatExt(bitPat)
  implicit def EnumTypeToExt(enum: EnumType): EnumTypeExt = new EnumTypeExt(enum)

  class DecodeTableExt[I <: DecodePattern](val decodeTable: DecodeTable[I]) {
    def decode(minimizer: Minimizer, input: UInt): DecodeBundle =
      chisel3.util.experimental.decode.decoder(minimizer, input, decodeTable.table).asTypeOf(decodeTable.bundle)
  }

  implicit def DecodeTableToExt[I <: DecodePattern](decodeTable: DecodeTable[I]): DecodeTableExt[I] = new DecodeTableExt(decodeTable)
}
