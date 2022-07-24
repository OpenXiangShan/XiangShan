package utils

import chisel3._
import scala.math.min

object ReuseFields {
  def connect[T <: Data] (sinks: Seq[T], source: T): Unit = {
    require(sinks.map(_.getWidth).sum >= source.getWidth)
    val totalWidth = source.getWidth
    var countWidth = 0
    var curWidth = 0
    for (sink <- sinks) {
      if (countWidth < totalWidth) {
        curWidth = sink.getWidth
        sink := source.asUInt(min(totalWidth - 1, countWidth + curWidth - 1), countWidth).asTypeOf(chiselTypeOf(sink))
        countWidth += curWidth
      }
      else{
        sink := 0.U.asTypeOf(chiselTypeOf(sink))
      }
    }
  }

  def connect[T <: Data] (sink: T, sources: Seq[T]):Unit = {
    require(sources.map(_.getWidth).sum >= sink.getWidth)
    sink := VecInit(sources).asTypeOf(chiselTypeOf(sink))
  }
}
