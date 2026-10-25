package xiangshan.backend.vector.Decoder.util

import chisel3.util.BitPat
import chisel3.{Bool, Data}

/**
 * One output field of a decoder bundle
 *
 * @tparam T pattern this field should match
 * @tparam D output type of this field
 */
trait DecodeField[-T <: DecodePattern, +D <: Data] {

  def name: String

  def chiselType: D

  /** Unlisted inputs are don't-cares unless a field overrides this default.
    * Match/legality fields must choose a defined default when used as valid bits.
    */
  def default: BitPat = dc

  final def width: Int = chiselType.getWidth

  def dc: BitPat = BitPat.dontCare(width)

  /** Return one output row at elaboration time, with the same width as chiselType. */
  def genTable(op: T): BitPat

  require(width == default.width)
}

/**
 * Special case when output type is a single Bool
 *
 * @tparam T pattern this field should match
 */
trait BoolDecodeField[T <: DecodePattern] extends DecodeField[T, Bool] {
  def chiselType: Bool = Bool()

  def y: BitPat = BitPat.Y(1)

  def n: BitPat = BitPat.N(1)
}
