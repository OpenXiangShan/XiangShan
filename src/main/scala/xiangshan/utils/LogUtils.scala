package xiangshan.utils

import chisel3._
import chisel3.util.experimental.BoringUtils
import xiangshan.HasXSParameter
import xiangshan.utils.XSLogLevel.XSLogLevel

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value("ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

object XSLog extends HasXSParameter {

  def displayLog(): Bool = {
    val disp_begin, disp_end = WireInit(0.U(64.W))
    BoringUtils.addSink(disp_begin, "DISPALY_LOG_START")
    BoringUtils.addSink(disp_end, "DISPLAY_LOG_END")
    assert(disp_begin <= disp_end)
    (GTimer() >= disp_begin) && (GTimer() <= disp_end)
  }

  def apply(debugLevel: XSLogLevel)
           (cond: Bool, pable: Printable)
           (implicit m: Module): Any = {
    if (debugLevel >= LogLevel) {
      when (cond && displayLog()) {
        val commonInfo = p"[$debugLevel][time=${GTimer()}] ${m.name}: "
        printf(commonInfo + pable)
      }
    }
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel) extends HasXSParameter {

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit m: Module): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit m: Module): Any = XSLog(logLevel)(cond, pable)
  def apply(fmt: String, data: Bits*)(implicit m: Module): Any =
    apply(true.B, Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit m: Module): Any = XSLog(logLevel)(true.B, pable)

  // Do not use that unless you have valid reasons
  def apply(cond: Bool = true.B)(body: => Unit): Any =
    if (logLevel >= LogLevel) { when (cond && XSLog.displayLog()) { body } }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
