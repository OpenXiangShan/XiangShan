package xiangshan.utils

import chisel3._
import chisel3.util.experimental.BoringUtils
import xiangshan.HasXSParameter
import xiangshan.utils.XSLogLevel.XSLogLevel

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

  def displayLog: Bool = {
    val disp_begin, disp_end = WireInit(0.U(64.W))
    BoringUtils.addSink(disp_begin, "DISPLAY_LOG_START")
    BoringUtils.addSink(disp_end, "DISPLAY_LOG_END")
    assert(disp_begin <= disp_end)
    (GTimer() >= disp_begin) && (GTimer() <= disp_end)
  }

  def xsLogLevel: UInt = {
    val log_level = WireInit(0.U(64.W))
    BoringUtils.addSink(log_level, "DISPLAY_LOG_LEVEL")
    assert(log_level < XSLogLevel.maxId.U)
    log_level
  }

  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable)
           (implicit name: String): Any = {
    val commonInfo = p"[$debugLevel][time=${GTimer()}] $name: "
    when (debugLevel.id.U >= xsLogLevel && cond && displayLog) {
      printf((if (prefix) commonInfo else p"") + pable)
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

  // trigger log or not
  // used when user what to fine-control their printf output
  def trigger: Bool = {
    logLevel.id.U >= XSLog.xsLogLevel && XSLog.displayLog
  }

  def printPrefix()(implicit name: String): Unit = {
    val commonInfo = p"[$logLevel][time=${GTimer()}] $name: "
    when (trigger) {
      printf(commonInfo)
    }
  }

  // dump under with certain prefix
  def exec(dump: => Unit)(implicit name: String): Unit = {
    when (trigger) {
      printf("world")
      printPrefix
      dump
    }
  }

  // dump under certain condition and with certain prefix
  def exec(cond: Bool, dump: => Unit)(implicit name: String): Unit = {
    when (trigger && cond) {
      printf("hello")
      printPrefix
      dump
    }
  }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)
