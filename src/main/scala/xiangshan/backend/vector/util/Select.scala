package xiangshan.backend.vector.util

import chisel3._
import chisel3.experimental.noPrefix
import chisel3.util._

package object Select {
  object Mux1HOrElse {
    def apply[T <: Data](in: Iterable[(Bool, T)], _else: T): T = {
      val otherwiseCond = Cat(in.map(!_._1).toSeq).andR
      Mux1H(in.toSeq :+ (otherwiseCond -> _else))
    }
  }

  object Mux1HLookUp {
    def apply[T <: Data](key: UInt, mapping: Seq[(UInt, T)]): T = {
      Mux1H(mapping.map { case (k, v) => (key === k) -> v})
    }

    def apply[T <: Data](key: UInt, default: T)(mapping: Seq[(UInt, T)]): T = {
      Mux1HOrElse(
        mapping.map{ case (k, v) => noPrefix {
          (k === key).suggestName(s"${key.name}_is_${k}") -> v
        }},
        default,
      )
    }

    def apply[E <: EnumType, T <: Data](key: E, default: T)(mapping: Seq[(E, T)]): T = {
      Mux1HOrElse(
        mapping.map{ case (k, v) => noPrefix {
          (k === key).suggestName(s"${key.name}_is_${k}") -> v
        }},
        default,
      )
    }

    def apply[E <: EnumType, T <: Data](lookupKey: E, default: T, mapping: Iterable[(Seq[E], T)]): T = {
      Mux1HOrElse(
        mapping.flatMap { case (keys: Seq[E], v: T) => noPrefix {
          keys.map(k =>
            (k === lookupKey).suggestName(s"${lookupKey.name}_is_${k}") -> v
          )
        }},
        default,
      )
    }
  }
}
