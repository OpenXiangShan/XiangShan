package xiangshan.backend.vector.Decoder.util

import chisel3._
import chisel3.util.experimental.decode.{EspressoMinimizer, TruthTable}
import chisel3.util.{BitPat, pla}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object decoder {
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
