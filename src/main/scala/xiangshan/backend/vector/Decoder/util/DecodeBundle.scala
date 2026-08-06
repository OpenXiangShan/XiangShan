package xiangshan.backend.vector.Decoder.util

import chisel3.{Data, Record}

import scala.collection.immutable.SeqMap

/**
 * Output of DecoderTable
 *
 * @param fields all fields to be decoded
 */
class DecodeBundle(fields: Seq[DecodeField[_, _ <: Data]]) extends Record {
  require(fields.map(_.name).distinct.size == fields.size, "Field names must be unique")
  val elements: SeqMap[String, Data] = SeqMap(fields.map(k => k.name -> k.chiselType): _*)

  /**
   * Get result of each field in decoding result
   *
   * @param field field to be queried
   * @tparam D type of field
   * @return hardware value of decoded output
   */
  def apply[D <: Data](field: DecodeField[_, D]): D = elements(field.name).asInstanceOf[D]
}

