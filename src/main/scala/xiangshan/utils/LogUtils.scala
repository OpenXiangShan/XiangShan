package xiangshan.utils

import chisel3._
import xiangshan.HasXSParameter
import xiangshan.utils.XSLogLevel.XSLogLevel

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value("ALL")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO")
  val WARN  = Value("WARN")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF")
}

object XSLog extends HasXSParameter{
  def apply(debugLevel: XSLogLevel)
           (cond: Bool, pable: Printable)
           (implicit m: Module = null): Any = {
    if (debugLevel >= LogLevel) {
      when (cond) {
        val commonInfo = p"[$debugLevel][time=${GTimer()}] ${m.name}: "
        printf(commonInfo + pable)
      }
    }
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel) {

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit m: Module): Any =
    apply(cond, Printable.pack(fmt, data:_*))

  def apply(cond: Bool, pable: Printable)(implicit m: Module): Any = XSLog(logLevel)(cond, pable)
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
