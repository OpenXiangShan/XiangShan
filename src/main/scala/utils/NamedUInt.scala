package utils

import chisel3._

/**
  * Produce named UInt(x.W)
  *
  * @example {{{
  *   object Fflags extends NamedUInt(5)
  *   val fflags = Fflags()
  * }}}
  */
abstract class NamedUInt(int : Int) {
  def apply(): UInt = UInt(width.W)

  def width: Int = int

  protected def checkInputWidth(uint: UInt): Unit = {
    require(
      uint.getWidth == this.width,
      s"the input UInt width(${uint.getWidth}) should be ${this.width}"
    )
  }
}
