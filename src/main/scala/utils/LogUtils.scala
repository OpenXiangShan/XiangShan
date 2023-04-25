/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import xiangshan.DebugOptionsKey
import utils.XSLogLevel.XSLogLevel
import utility.GTimer

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
           (prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Any =
  {
    val debugOpts = p(DebugOptionsKey)
    val logEnable = WireInit(false.B)
    val logTimestamp = WireInit(0.U(64.W))
    val enableDebug = debugOpts.EnableDebug && debugLevel != XSLogLevel.PERF
    val enablePerf = debugOpts.EnablePerfDebug && debugLevel == XSLogLevel.PERF
    if (!debugOpts.FPGAPlatform && (enableDebug || enablePerf || debugLevel == XSLogLevel.ERROR)) {
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

  def displayLog(implicit p: Parameters): Bool = {
    val debugOpts = p(DebugOptionsKey)
    val ret = WireInit(false.B)
    if (!debugOpts.FPGAPlatform && debugOpts.EnableDebug) {
      val logEnable = WireInit(false.B)
      ExcitingUtils.addSink(logEnable, "DISPLAY_LOG_ENABLE")
      ret := logEnable
    }
    ret
  }
}

sealed abstract class LogHelper(val logLevel: XSLogLevel){

  def apply(cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(cond, Printable.pack(fmt, data:_*))
  def apply(cond: Bool, pable: Printable)(implicit p: Parameters): Any = apply(true, cond, pable)
  def apply(fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(Printable.pack(fmt, data:_*))
  def apply(pable: Printable)(implicit p: Parameters): Any = apply(true.B, pable)
  def apply(prefix: Boolean, cond: Bool, fmt: String, data: Bits*)(implicit p: Parameters): Any =
    apply(prefix, cond, Printable.pack(fmt, data:_*))
  def apply(prefix: Boolean, cond: Bool, pable: Printable)(implicit p: Parameters): Any =
    XSLog(logLevel)(prefix, cond, pable)

  // trigger log or not
  // used when user what to fine-control their printf output
  def trigger(implicit p: Parameters): Bool = {
    XSLog.displayLog
  }

  def printPrefix()(implicit p: Parameters): Unit = {
    val commonInfo = p"[$logLevel][time=${GTimer()}] ${XSLog.MagicStr}: "
    when (trigger) {
      printf(commonInfo)
    }
  }

  // dump under with certain prefix
  def exec(dump: () => Unit)(implicit p: Parameters): Unit = {
    when (trigger) {
      printPrefix
      dump
    }
  }

  // dump under certain condition and with certain prefix
  def exec(cond: Bool, dump: () => Unit)(implicit p: Parameters): Unit = {
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
