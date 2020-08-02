package utils

import chisel3._
import chisel3.util.experimental.BoringUtils
import xiangshan.HasXSParameter
import utils.XSLogLevel.XSLogLevel

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

object XSLog {
  var generateLog: Boolean = false
  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)
           (implicit name: String): Any =
  {
    val commonInfo = p"[$debugLevel][time=${GTimer()}] $name: "
    val logEnable = WireInit(false.B)
    ExcitingUtils.addSink(logEnable, "DISPLAY_LOG_ENABLE")
    if(generateLog){
      when (cond ){//&& logEnable) {
        printf((if (prefix) commonInfo else p"") + pable)
        if (debugLevel >= XSLogLevel.ERROR) {
          assert(false.B)
        }
      }
    }
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel) extends HasXSParameter {

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit name: String): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit name: String): Any = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit name: String): Any =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit name: String): Any = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit name: String): Any =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit name: String): Any =
    XSLog(logLevel)(prefix, cond, pable)
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
