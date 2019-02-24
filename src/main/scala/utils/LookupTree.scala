package utils

import chisel3._
import chisel3.util._

object LookupTree {
  private val useMuxTree = false

  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))

  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    if (useMuxTree) apply(key, mapping) else MuxLookup(key, default, mapping.toSeq)
}
