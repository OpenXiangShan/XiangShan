package xiangshan.backend.vector.Decoder.Split

import chisel3._
import chisel3.util._
import utils.EnumUtils.OHEnumeration
import xiangshan.backend.vector.util.BString.BinaryStringHelper

object SplitType extends ChiselEnum {
  val NONE   = Value(b"00000")
  val VVV    = Value(b"00001")
  val VVM    = Value(b"00010")
  val WVV    = Value(b"00011")
  val WVW    = Value(b"00100")
  val VVW    = Value(b"00101")
  val EXT4   = Value(b"00110")
  val EXT8   = Value(b"00111")

  val VREDU  = Value(b"01000")
  val VREDO  = Value(b"01001")
  val VWREDU = Value(b"01010")
  val VWREDO = Value(b"01011")

  val SHUFFLE_SLIDE_UP      = Value(b"01100")
  val SHUFFLE_COMPRESS_DOWN = Value(b"01101")
  val SHUFFLE_GATHER        = Value(b"01110")

  val VLSIDX_DI_RATIO_1    = Value(b"10000")
  val VLSIDX_DI_RATIO_2    = Value(b"10001")
  val VLSIDX_DI_RATIO_4    = Value(b"10010")
  val VLSIDX_DI_RATIO_8    = Value(b"10011")
  val VLSIDX_DI_RATIO_F8   = Value(b"10101")
  val VLSIDX_DI_RATIO_F4   = Value(b"10110")
  val VLSIDX_DI_RATIO_F2   = Value(b"10111")

  val VLSNONIDX            = Value(b"11000")
  val VLSNONIDXFF          = Value(b"11001")

  lazy val maxValue = this.all.map(_.litValue.toInt).max

  def typeToBitPat(t: this.Type) = {
    new BitPat(t.litValue, (BigInt(1) << this.getWidth) - 1, this.getWidth)
  }
}

object SplitTypeOH extends OHEnumeration {
  type Type = UInt

  def width = SplitType.maxValue + 1

  def apply() = UInt(width.W)

  def apply(f: SplitType.type => SplitType.Type): UInt = {
    val splitType = f(SplitType)
    (BigInt(1) << splitType.litValue.toInt).U(width.W)
  }

  def apply(splitType: SplitType.Type): UInt = {
    (BigInt(1) << splitType.litValue.toInt).U(width.W)
  }

  def decodeValue(uint: UInt): Int = {
    val litValue = uint.litValue.ensuring(x => isPow2(x))
    log2Ceil(litValue)
  }

  def decodeValue(bitPat: BitPat): Int = {
    bitPat.rawString.reverse.indexOf('1').ensuring(_ >= 0)
  }

  lazy val all: Seq[Type] = SplitType.all.map(x => this.apply(x))
  lazy val allBitPat: Seq[BitPat] = all.map(x => BitPat("b" + BitPat(x).rawString.replace('0', '?')))
}

