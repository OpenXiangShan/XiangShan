package xiangshan.backend.vector.Decoder.util

import chisel3.util.Cat
import chisel3.util.experimental.decode.TruthTable
import chisel3.{Data, UInt}

/**
 * A structured way of generating large decode tables, often found in CPU instruction decoders
 *
 * Each DecodeField maps every DecodePattern to an output BitPat. Scala pattern
 * expansion and minimization happen at elaboration; decode emits combinational logic.
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
          op => op.bitPat -> field.genTable(op).ensuring(
            _.getWidth == field.default.getWidth,
            s"The length of each item of table($field) for pattern ${op} should be equal to the length of default(${field.default.getWidth})",
          )
        },
        field.default
      )
  }

  lazy val table: TruthTable = TruthTable(
    patterns.map { op => op.bitPat -> fields.reverse.map(field => field.genTable(op)).reduce(_ ## _) },
    fields.reverse.map(_.default).reduce(_ ## _)
  )

  /** Minimize fields independently, then restore their declared Record layout. */
  def decode(input: UInt): DecodeBundle = {
    Cat(decoder(input, tables).reverse).asTypeOf(bundle)
  }
}
