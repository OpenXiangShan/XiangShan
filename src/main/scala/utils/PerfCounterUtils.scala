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
import chisel3.util._
import xiangshan.DebugOptionsKey
import xiangshan._

object XSPerfAccumulate {
  def apply(perfName: String, perfCnt: UInt)(implicit p: Parameters) = {
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      ExcitingUtils.addSink(logTimestamp, "logTimestamp")
      ExcitingUtils.addSink(perfClean, "XSPERF_CLEAN")
      ExcitingUtils.addSink(perfDump, "XSPERF_DUMP")

      val counter = RegInit(0.U(64.W))
      val next_counter = counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)

      when (perfDump) {
        XSPerfPrint(p"$perfName, $next_counter\n")
      }
    }
  }
}

object XSPerfHistogram {
  // instead of simply accumulating counters
  // this function draws a histogram
  def apply
  (perfName: String, perfCnt: UInt, enable: Bool, start: Int, stop: Int, step: Int)
  (implicit p: Parameters) = {
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      ExcitingUtils.addSink(logTimestamp, "logTimestamp")
      ExcitingUtils.addSink(perfClean, "XSPERF_CLEAN")
      ExcitingUtils.addSink(perfDump, "XSPERF_DUMP")

      // drop each perfCnt value into a bin
      val nBins = (stop - start) / step
      require(start >= 0)
      require(stop > start)
      require(nBins > 0)

      (0 until nBins) map { i =>
        val binRangeStart = start + i * step
        val binRangeStop = start + (i + 1) * step
        val inRange = perfCnt >= binRangeStart.U && perfCnt < binRangeStop.U

        // if perfCnt < start, it will go to the first bin
        val leftOutOfRange = perfCnt < start.U && i.U === 0.U
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = perfCnt >= stop.U && i.U === (nBins - 1).U
        val inc = inRange || leftOutOfRange || rightOutOfRange

        val counter = RegInit(0.U(64.W))
        when (perfClean) {
          counter := 0.U
        } .elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        when (perfDump) {
          XSPerfPrint(p"${perfName}_${binRangeStart}_${binRangeStop}, $counter\n")
        }
      }
    }
  }
}
object XSPerfMax {
  def apply(perfName: String, perfCnt: UInt, enable: Bool)(implicit p: Parameters) = {
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val logTimestamp = WireInit(0.U(64.W))
      val perfClean = WireInit(false.B)
      val perfDump = WireInit(false.B)
      ExcitingUtils.addSink(logTimestamp, "logTimestamp")
      ExcitingUtils.addSink(perfClean, "XSPERF_CLEAN")
      ExcitingUtils.addSink(perfDump, "XSPERF_DUMP")

      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)

      when (perfDump) {
        XSPerfPrint(p"${perfName}_max, $next_max\n")
      }
    }
  }
}

object QueuePerf {
  def apply(size: Int, utilization: UInt, full: UInt)(implicit p: Parameters) = {
    XSPerfAccumulate("utilization", utilization)
    XSPerfHistogram("util", utilization, true.B, 0, size, 1)
    XSPerfAccumulate("full", full)
    val exHalf = utilization > (size/2).U
    val empty = utilization === 0.U
    XSPerfAccumulate("exHalf", exHalf)
    XSPerfAccumulate("empty", empty)
  }
}

object TransactionLatencyCounter
{
  // count the latency between start signal and stop signal
  // whenever stop signals comes, we create a latency sample
  def apply(start: Bool, stop: Bool): (Bool, UInt) = {
    assert (!(start && stop))
    val counter = RegInit(0.U(64.W))
    val next_counter = counter + 1.U
    counter := Mux(start || stop, 0.U, next_counter)
    (stop, next_counter)
  }
}

object XSPerfPrint {
  def apply(pable: Printable)(implicit p: Parameters): Any = {
    XSLog(XSLogLevel.PERF)(true, true.B, pable)
  }
}

class PerfBundle(implicit p: Parameters) extends XSBundle {
  val incr_step  = UInt(6.W)
}

class PerfEventsBundle (val numPCnt: Int) (implicit p: Parameters)extends XSBundle{

  val perf_events = Vec(numPCnt, (new PerfBundle))
  def length = numPCnt

}

class HPerfCounter (val numPCnt: Int) (implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val hpm_event         = Input(UInt(XLEN.W))
    val events_sets      = Input(new PerfEventsBundle(numPCnt))
    val event_selected   = Output(new PerfBundle)
  })
  
  val events_incr_0 = io.events_sets.perf_events(io.hpm_event(9,0))
  val events_incr_1 = io.events_sets.perf_events(io.hpm_event(19,10))
  val events_incr_2 = io.events_sets.perf_events(io.hpm_event(29,20))
  val events_incr_3 = io.events_sets.perf_events(io.hpm_event(39,30))

    val event_op_0 = io.hpm_event(44,40)
    val event_op_1 = io.hpm_event(49,45)
    val event_op_2 = io.hpm_event(54,50)


    val event_step_0 = Mux(event_op_0(0),(events_incr_3.incr_step & events_incr_2.incr_step),
                       Mux(event_op_0(1),(events_incr_3.incr_step ^ events_incr_2.incr_step),
                       Mux(event_op_0(2),(events_incr_3.incr_step + events_incr_2.incr_step),  
                                         (events_incr_3.incr_step | events_incr_2.incr_step))))
    val event_step_1 = Mux(event_op_1(0),(events_incr_1.incr_step & events_incr_0.incr_step),
                       Mux(event_op_1(1),(events_incr_1.incr_step ^ events_incr_0.incr_step),
                       Mux(event_op_1(2),(events_incr_1.incr_step + events_incr_0.incr_step),  
                                         (events_incr_1.incr_step | events_incr_0.incr_step))))

    io.event_selected.incr_step  := Mux(event_op_1(0),(event_step_0 & event_step_1),
                                    Mux(event_op_1(1),(event_step_0 ^ event_step_1),
                                    Mux(event_op_1(2),(event_step_0 + event_step_1),
                                                      (event_step_0 | event_step_1))))
}

class HPerfmonitor (val numPCnt: Int, val numCSRPCnt: Int) (implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle {
    val hpm_event         = Input(Vec(numCSRPCnt, UInt(XLEN.W)))
    val events_sets      = Input(new PerfEventsBundle(numPCnt))
    //val Events_selected  = Output(Vec(numCSRPCnt,(new PerfBundle)))
    val events_selected  = Output(new PerfEventsBundle(numCSRPCnt))
  })
  
  for (i <- 0 until numCSRPCnt) {
    val hpc = Module(new HPerfCounter(numPCnt))
    hpc.io.events_sets       <> io.events_sets
    hpc.io.hpm_event         := io.hpm_event(i)
    hpc.io.event_selected    <> io.events_selected.perf_events(i)
  }
}
