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
  (
    perfName: String,
    perfCnt: UInt,
    enable: Bool,
    start: Int,
    stop: Int,
    step: Int,
    left_strict: Boolean = false,
    right_strict: Boolean = false
  )
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
        val leftOutOfRange = if(left_strict)
          false.B
        else
          perfCnt < start.U && i.U === 0.U
        // if perfCnt >= stop, it will go to the last bin
        val rightOutOfRange = if(right_strict)
          false.B
        else
          perfCnt >= stop.U && i.U === (nBins - 1).U
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

class PerfEvent extends Bundle {
  val value = UInt(6.W)
}

trait HasPerfEvents { this: RawModule =>
  val perfEvents: Seq[(String, UInt)]

  lazy val io_perf: Vec[PerfEvent] = IO(Output(Vec(perfEvents.length, new PerfEvent)))
  def generatePerfEvent(noRegNext: Option[Seq[Int]] = None): Unit = {
    for (((out, (name, counter)), i) <- io_perf.zip(perfEvents).zipWithIndex) {
      require(!name.contains("/"))
      out.value := RegNext(RegNext(counter))
      if (noRegNext.isDefined && noRegNext.get.contains(i)) {
        out.value := counter
      }
    }
  }
  def getPerfEvents: Seq[(String, UInt)] = {
    perfEvents.map(_._1).zip(io_perf).map(x => (x._1, x._2.value))
  }
  def getPerf: Vec[PerfEvent] = io_perf
}

class HPerfCounter(val numPCnt: Int)(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle {
    val hpm_event   = Input(UInt(XLEN.W))
    val events_sets = Input(Vec(numPCnt, new PerfEvent))
  })

  val events_incr_0 = RegNext(io.events_sets(io.hpm_event( 9,  0)))
  val events_incr_1 = RegNext(io.events_sets(io.hpm_event(19, 10)))
  val events_incr_2 = RegNext(io.events_sets(io.hpm_event(29, 20)))
  val events_incr_3 = RegNext(io.events_sets(io.hpm_event(39, 30)))

  val event_op_0 = RegNext(io.hpm_event(44, 40))
  val event_op_1 = RegNext(io.hpm_event(49, 45))
  val event_op_2 = RegNext(io.hpm_event(54, 50))

  def combineEvents(cnt_1: UInt, cnt_2: UInt, optype: UInt): UInt =
    Mux(optype(0), cnt_1 & cnt_2,
    Mux(optype(1), cnt_1 ^ cnt_2,
    Mux(optype(2), cnt_1 + cnt_2,
                   cnt_1 | cnt_2)))

  val event_step_0 = combineEvents(events_incr_0.value, events_incr_1.value, event_op_0)
  val event_step_1 = combineEvents(events_incr_2.value, events_incr_3.value, event_op_1)

  // add registers to optimize the timing (like pipelines)
  val event_op_2_reg = RegNext(event_op_2)
  val event_step_0_reg = RegNext(event_step_0)
  val event_step_1_reg = RegNext(event_step_1)
  val selected = combineEvents(event_step_0_reg, event_step_1_reg, event_op_2_reg)

  val perfEvents = Seq(("selected", selected))
  generatePerfEvent()
}

class HPerfMonitor(numCSRPCnt: Int, numPCnt: Int)(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle {
    val hpm_event   = Input(Vec(numCSRPCnt, UInt(XLEN.W)))
    val events_sets = Input(Vec(numPCnt, new PerfEvent))
  })

  val perfEvents = io.hpm_event.zipWithIndex.map{ case (hpm, i) =>
    val hpc = Module(new HPerfCounter(numPCnt))
    hpc.io.events_sets <> io.events_sets
    hpc.io.hpm_event   := hpm
    val selected = hpc.getPerfEvents.head
    (s"${selected._1}_$i", selected._2)
  }
  generatePerfEvent()
}

object HPerfMonitor {
  def apply(hpm_event: Seq[UInt], events_sets: Seq[PerfEvent])(implicit p: Parameters): HPerfMonitor = {
    val hpm = Module(new HPerfMonitor(hpm_event.length, events_sets.length))
    hpm.io.hpm_event := hpm_event
    hpm.io.events_sets := events_sets
    hpm
  }
}
