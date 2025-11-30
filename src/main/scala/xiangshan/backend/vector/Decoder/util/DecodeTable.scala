package xiangshan.backend.vector.Decoder.util

import chisel3.util.Cat
import chisel3.util.experimental.decode.{TruthTable}
import chisel3.{Data, UInt}

/**
 * A structured way of generating large decode tables, often found in CPU instruction decoders
 *
 * Each field is a `DecoderPattern`, its genTable method will be called for each possible pattern and gives expected
 * output for this field as a `BitPat`.
 *
 * @param patterns all possible input patterns, should implement trait DecoderPattern
 * @param fields   all fields as decoded output
 * @tparam I concrete type of input patterns trait
 */
class DecodeTable[I <: DecodePattern](patterns: Seq[I], fields: Seq[DecodeField[I, _ <: Data]]) {
  require(patterns.map(_.bitPat.getWidth).distinct.size == 1, "All instructions must have the same width")

  def bundle: DecodeBundle = new DecodeBundle(fields)

  lazy val tables: Seq[TruthTable] = fields.map {
    field =>
      TruthTable(
        patterns.map {
          op => op.bitPat -> field.genTable(op)
        },
        field.default
      )
  }

  lazy val table: TruthTable = TruthTable(
    patterns.map { op => op.bitPat -> fields.reverse.map(field => field.genTable(op)).reduce(_ ## _) },
    fields.reverse.map(_.default).reduce(_ ## _)
  )

  def decode(input: UInt): DecodeBundle = {
    Cat(decoder(input, tables).reverse).asTypeOf(bundle)
  }

//  def decode(input: UInt): DecodeBundle = chisel3.util.experimental.decode.decoder(input, table).asTypeOf(bundle)

}
