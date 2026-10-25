package xiangshan.backend.vector.util

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import chisel3.util.experimental.decode._

import scala.language.implicitConversions

/** Bit-pattern slicing/padding and literal-only arithmetic used during table construction. */
object ChiselTypeExt {
  class BitPatExt(val bitPat: BitPat) {
    def U: UInt = bitPatToUInt(bitPat)

    def ## (that: UInt): BitPat = this.bitPat ## BitPat(that)

    def head(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n <= this.bitPat.width)
      this.bitPat.apply(this.bitPat.width - 1, this.bitPat.width - n)
    }

    /** Preserve the existing inclusive slice [width-n:0]; this differs from UInt.tail. */
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

    /** Drop n low bits, retaining the high slice and its don't-care mask. */
    def drop(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.bitPat.width, "The width of result should be greater than 0")
      this.bitPat.apply(this.bitPat.width - 1, n)
    }

    def take(n: Int): BitPat = this.lsb(n)

    /** Extend on the high side without truncating an already wider pattern. */
    def pad0To(n: Int): BitPat = {
      if (bitPat.width >= n)
        this.bitPat
      else
        BitPat.N(n - bitPat.width) ## this.bitPat
    }

    /** Extend with unconstrained high bits without truncation. */
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

    /**
     * Literal value or
     * @param that: a literal UInt value whose width is known
     * @return a result of Literal value, the width is max of two literal value.
     */
    def #|#(that: UInt): UInt = {
      require(this.value.widthKnown && that.value.widthKnown)
      require(this.value.isLit && that.value.isLit)
      val maxWidth = this.value.getWidth max that.value.getWidth
      (this.value.litValue | that.litValue).U(maxWidth.W)
    }

    /**
     * Literal value shift left
     * @param shamt: shift amount
     * @return a result of Literal value, the width will be plused shamt.
     */
    def #<<(shamt: Int): UInt = {
      require(this.value.widthKnown)
      (this.value.litValue << shamt).U((this.value.getWidth + shamt).W)
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

  /** Select a host-side minimizer while preserving the decode table's output layout. */
  class DecodeTableExt[I <: DecodePattern](val decodeTable: DecodeTable[I]) {
    def decode(minimizer: Minimizer, input: UInt): DecodeBundle =
      chisel3.util.experimental.decode.decoder(minimizer, input, decodeTable.table).asTypeOf(decodeTable.bundle)
  }

  implicit def DecodeTableToExt[I <: DecodePattern](decodeTable: DecodeTable[I]): DecodeTableExt[I] = new DecodeTableExt(decodeTable)
}
