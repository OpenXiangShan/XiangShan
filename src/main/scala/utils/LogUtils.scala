package utils

import chisel3._
import top.Parameters
import xiangshan.HasXSParameter
import utils.XSLogLevel.XSLogLevel
import chisel3.ExcitingUtils.ConnectionType

object XSLogLevel extends Enumeration {
  type XSLogLevel = Value

  val ALL   = Value(0, "ALL  ")
  val DEBUG = Value("DEBUG")
  val INFO  = Value("INFO ")
  val PERF  = Value("PERF ")
  val WARN  = Value("WARN ")
  val ERROR = Value("ERROR")
  val OFF   = Value("OFF  ")
}

object XSLog {
  val MagicStr = "9527"
  def apply(debugLevel: XSLogLevel)
           (prefix: Boolean, cond: Bool, pable: Printable): Any =
  {
    val logEnable = WireInit(false.B)
    val logTimestamp = WireInit(0.U(64.W))
    val enableDebug = Parameters.get.envParameters.EnableDebug && debugLevel != XSLogLevel.PERF
    val enablePerf = Parameters.get.envParameters.EnablePerfDebug && debugLevel == XSLogLevel.PERF
    if (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR) {
      ExcitingUtils.addSink(logEnable, "DISPLAY_LOG_ENABLE")
      ExcitingUtils.addSink(logTimestamp, "logTimestamp")
      val check_cond = (if (debugLevel == XSLogLevel.ERROR) true.B else logEnable) && cond
      when (check_cond) {
        val commonInfo = p"[$debugLevel][time=$logTimestamp] $MagicStr: "
        printf((if (prefix) commonInfo else p"") + pable)
        if (debugLevel >= XSLogLevel.ERROR) {
          assert(false.B)
        }
      }
    }
  }

  def displayLog: Bool = {
    val logEnable = WireInit(false.B)
    val ret = WireInit(false.B)
    if(Parameters.get.envParameters.EnableDebug) {
      ExcitingUtils.addSink(logEnable, "DISPLAY_LOG_ENABLE")
      ret := logEnable
    }
    ret
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel) extends HasXSParameter {

  def apply(cond: Bool, fmt: String, data: Bits*): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable): Any = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*): Any =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable): Any = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*): Any =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable): Any =
    XSLog(logLevel)(prefix, cond, pable)

  // trigger log or not
  // used when user what to fine-control their printf output
  def trigger: Bool = {
    XSLog.displayLog
  }

  def printPrefix(): Unit = {
    val commonInfo = p"[$logLevel][time=${GTimer()}] ${XSLog.MagicStr}: "
    when (trigger) {
      printf(commonInfo)
    }
  }

  // dump under with certain prefix
  def exec(dump: () => Unit): Unit = {
    when (trigger) {
      printPrefix
      dump
    }
  }

  // dump under certain condition and with certain prefix
  def exec(cond: Bool, dump: () => Unit): Unit = {
    when (trigger && cond) {
      printPrefix
      dump
    }
  }
}

object XSDebug extends LogHelper(XSLogLevel.DEBUG)

object XSInfo extends LogHelper(XSLogLevel.INFO)

object XSWarn extends LogHelper(XSLogLevel.WARN)

object XSError extends LogHelper(XSLogLevel.ERROR)

object XSPerf extends HasXSParameter {
  def apply(perfName: String, perfCnt: UInt, acc: Boolean = false)(implicit name: String) = {
    val counter = RegInit(0.U(64.W))
    val next_counter = WireInit(0.U(64.W))
    val logTimestamp = WireInit(0.U(64.W))
    val env = Parameters.get.envParameters
    next_counter := counter + perfCnt
    counter := next_counter

    if (env.EnablePerfDebug) {
      ExcitingUtils.addSink(logTimestamp, "logTimestamp")
      val printCond = if (PerfIntervalBits == 0) true.B else (logTimestamp(PerfIntervalBits - 1, 0) === 0.U)
      val printEnable = if (PerfRealTime) printCond else false.B
      val xstrap = WireInit(false.B)
      if (!env.FPGAPlatform && !env.DualCore) {
        ExcitingUtils.addSink(xstrap, "XSTRAP", ConnectionType.Debug)
      }
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      ExcitingUtils.addSink(perfClean, "XSPERF_CLEAN")
      ExcitingUtils.addSink(perfDump, "XSPERF_DUMP")
      when (perfClean) {
        counter := 0.U
      }
      when (printEnable) {  // interval print
        if (acc) {
          XSLog(XSLogLevel.PERF)(true, true.B, p"$perfName, $next_counter\n")
        } else {
          XSLog(XSLogLevel.PERF)(true, true.B, p"$perfName, $perfCnt\n")
        }
      }
      when (xstrap || perfDump) {  // summary print
        // dump acc counter by default
        XSLog(XSLogLevel.PERF)(true, true.B, p"$perfName, $next_counter\n")
      }
    }
  }
}
