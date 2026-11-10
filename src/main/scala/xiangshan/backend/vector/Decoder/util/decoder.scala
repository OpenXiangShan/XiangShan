package xiangshan.backend.vector.Decoder.util

import chisel3._
import chisel3.util.experimental.decode.{EspressoMinimizer, TruthTable}
import chisel3.util.{BitPat, pla}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object decoder {
  /**
   * This function takes an input UInt and a sequence of TruthTables, minimizes each TruthTable using the Espresso
   * algorithm, and generates a sequence of UInt outputs based on the minimized TruthTables. The output for each
   * TruthTable is determined by the default value and the input patterns defined in the table.
   * @param input input UInt that will be used to generate the output based on the TruthTables
   * @param truthTables a sequence of TruthTables that define the logic for generating the output. Each TruthTable
   *                    contains a default value and a set of input-output patterns.
   * @return a sequence of UInt outputs corresponding to each TruthTable by the input.
   */
  def apply(input: UInt, truthTables: Seq[TruthTable]): Seq[UInt] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val futures = truthTables.map {
      table =>
        Future {
          EspressoMinimizer.minimize(table)
        }
    }

    val minimizedTables = Await.result(Future.sequence(futures), Duration.Inf)

    for (table <- minimizedTables) yield {
      val default = table.default.value.U(table.default.getWidth.W)

      if (table.table.isEmpty) {
        val output = Wire(UInt(default.getWidth.W))
        output := default
        output
      } else {
        val (plaInput, plaOutput) = pla(table.table, BitPat(default))
        plaInput := input
        plaOutput
      }
    }
  }
}
