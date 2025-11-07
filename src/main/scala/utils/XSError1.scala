package utility

import chisel3._
import org.chipsalliance.cde.config.Parameters

// A temp hack for XSError
object XSError1 {
  val logLevel = XSLogLevel.ERROR

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters): Unit = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit p: Parameters): Unit = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Unit =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Unit ={
    val logOpts = p(LogUtilsOptionsKey)
    if (!logOpts.fpgaPlatform) {
      when (cond) {
        assert(false.B, pable)
      }
    }
  }
}