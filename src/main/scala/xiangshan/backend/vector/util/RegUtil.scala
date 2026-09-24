package xiangshan.backend.vector.util

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.ValidIO

object RegUtil {
  /**
   * Absolutely correct GatedRegNext methods.
   *
   * [[GatedRegNext]] use `reg or next` logic as gate signal. It has two benefits.
   * 1. the reg is gated when there is an 1 signal in either reg or next
   * 2. X state will be rewritten with 1 when the first 1 comes, so it's safe with no init.
   */
  private[vector] object GatedRegNext {
    def apply(next: Bool, init: Bool): Bool = {
      val reg = RegInit(Bool(), init)

      when (reg || next) {
        reg := next
      }
      reg
    }

    def apply(next: Bool): Bool = {
      val reg = chisel3.Reg(Bool())

      when (reg || next) {
        reg := next
      }
      reg
    }

    def apply[T <: Data](next: ValidIO[T]): ValidIO[T] = {
      val reg = Reg(chiselTypeOf(next))

      when (reg.valid || next.valid) {
        reg.valid := next.valid
      }

      when (next.valid) {
        reg.bits := next.bits
      }

      reg
    }

    def apply[T <: Data](next: ValidIO[T], validInit: Bool): ValidIO[T] = {
      val reg = RegInit(chiselTypeOf(next).Lit(_.valid -> validInit))

      when (reg.valid || next.valid) {
        reg.valid := next.valid
      }

      when (next.valid) {
        reg.bits := next.bits
      }

      reg
    }
  }
}
