package utils

import chisel3._
import chisel3.util._

object BundleUtils {
  def makeValid[T <: Data](valid: Bool, bits: T): ValidIO[T] = {
    val x = Wire(ValidIO(bits.cloneType))
    x.valid := valid
    x.bits := bits
    x
  }
}
