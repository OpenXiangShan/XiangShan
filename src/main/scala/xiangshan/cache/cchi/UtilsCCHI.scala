package xiangshan.cache

import chisel3._
import chisel3.util._
import oceanus.compactchi._

object CCHIBuffer {
  def apply[T <: Data](x: DecoupledIO[T]): DecoupledIO[T] =
    Queue(Queue(x, 2), 2)
}
