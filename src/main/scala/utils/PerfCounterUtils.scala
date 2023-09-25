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
import utility.{ChiselDB, LogPerfHelper, LogPerfIO}
import xiangshan.DebugOptionsKey
import xiangshan._
import utility.LogPerfIO

trait HasRegularPerfName {
  def judgeName(perfName: String) = {
    val regular = """(\w+)""".r
    perfName match {
      case regular(_) => true
      case _ => {
        println("PerfName " + perfName + " is not '\\w+' regular")
        require(false)
      }
    }
  }
}

object XSPerfAccumulate extends HasRegularPerfName {
  def apply(perfName: String, perfCnt: UInt)(implicit p: Parameters) = {
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      val counter = RegInit(0.U(64.W)).suggestName(perfName + "Counter")
      val next_counter = WireInit(0.U(64.W)).suggestName(perfName + "Next")
      next_counter := counter + perfCnt
      counter := Mux(perfClean, 0.U, next_counter)

      when (perfDump) {
        XSPerfPrint(p"$perfName, $next_counter\n")(helper.io)
      }
    }
  }
}

object XSPerfHistogram extends HasRegularPerfName {
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
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      val sum = RegInit(0.U(64.W)).suggestName(perfName + "Sum")
      val nSamples = RegInit(0.U(64.W)).suggestName(perfName + "NSamples")
      val underflow = RegInit(0.U(64.W)).suggestName(perfName + "Underflow")
      val overflow = RegInit(0.U(64.W)).suggestName(perfName + "Overflow")
      when (perfClean) {
        sum := 0.U
        nSamples := 0.U
        underflow := 0.U
        overflow := 0.U
      } .elsewhen (enable) {
        sum := sum + perfCnt
        nSamples := nSamples + 1.U
        when (perfCnt < start.U) {
          underflow := underflow + 1.U
        }
        when (perfCnt >= stop.U) {
          overflow := overflow + 1.U
        }
      }

      when (perfDump) {
        XSPerfPrint(p"${perfName}_mean, ${sum/nSamples}\n")(helper.io)
        XSPerfPrint(p"${perfName}_sampled, ${nSamples}\n")(helper.io)
        XSPerfPrint(p"${perfName}_underflow, ${underflow}\n")(helper.io)
        XSPerfPrint(p"${perfName}_overflow, ${overflow}\n")(helper.io)
      }
      
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

        val histName = s"${perfName}_${binRangeStart}_${binRangeStop}"
        val counter = RegInit(0.U(64.W)).suggestName(histName)
        when (perfClean) {
          counter := 0.U
        } .elsewhen(enable && inc) {
          counter := counter + 1.U
        }

        when (perfDump) {
          XSPerfPrint(p"${histName}, $counter\n")(helper.io)
        }
      }
    }
  }
}

object XSPerfMax extends HasRegularPerfName {
  def apply(perfName: String, perfCnt: UInt, enable: Bool)(implicit p: Parameters) = {
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnablePerfDebug && !env.FPGAPlatform) {
      val helper = Module(new LogPerfHelper)
      val perfClean = helper.io.clean
      val perfDump = helper.io.dump

      val max = RegInit(0.U(64.W))
      val next_max = Mux(enable && (perfCnt > max), perfCnt, max)
      max := Mux(perfClean, 0.U, next_max)

      when (perfDump) {
        XSPerfPrint(p"${perfName}_max, $next_max\n")(helper.io)
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

object TransactionLatencyCounter {
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

object XSPerfRolling extends HasRegularPerfName {

  class RollingEntry()(implicit p: Parameters) extends Bundle {
    val xAxisPt = UInt(64.W)
    val yAxisPt = UInt(64.W)

    def apply(xAxisPt: UInt, yAxisPt: UInt): RollingEntry = {
      val e = Wire(new RollingEntry())
      e.xAxisPt := xAxisPt
      e.yAxisPt := yAxisPt
      e
    }
  }

  def apply(
    perfName: String,
    perfCnt: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters): Unit = {
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnableRollingDB && !env.FPGAPlatform) {
      val tableName = perfName + "_rolling_" + p(XSCoreParamsKey).HartId.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + 1.U(64.W)  // increment per cycle
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt === granularity.U
      when(triggerDB) {
        xAxisCnt := 1.U(64.W)
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + granularity.U
        xAxisPt := xAxisPtReg + granularity.U
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }

  def apply(
    perfName: String,
    perfCnt: UInt,
    eventTrigger: UInt,
    granularity: Int,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters) = {
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnableRollingDB && !env.FPGAPlatform) {
      val tableName = perfName + "_rolling_" + p(XSCoreParamsKey).HartId.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val xAxisPtReg = RegInit(0.U(64.W))
      val xAxisPt = WireInit(0.U(64.W))
      xAxisCnt := xAxisCnt + eventTrigger // increment when event triggers
      yAxisCnt := yAxisCnt + perfCnt

      val triggerDB = xAxisCnt >= granularity.U
      when(triggerDB) {
        xAxisCnt := xAxisCnt - granularity.U + eventTrigger
        yAxisCnt := perfCnt
        xAxisPtReg := xAxisPtReg + xAxisCnt
        xAxisPt := xAxisPtReg + xAxisCnt
      }
      val rollingPt = new RollingEntry().apply(xAxisPt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }
  
  // event interval based mode
  def apply(
    perfName: String,
    perfCntX: UInt,
    perfCntY: UInt,
    granularity: Int,
    eventTrigger: UInt,
    clock: Clock,
    reset: Reset
  )(implicit p: Parameters) = {
    judgeName(perfName)
    val env = p(DebugOptionsKey)
    if (env.EnableRollingDB && !env.FPGAPlatform) {
      val tableName = perfName + "_rolling_" + p(XSCoreParamsKey).HartId.toString
      val rollingTable = ChiselDB.createTable(tableName, new RollingEntry(), basicDB=true)

      val xAxisCnt = RegInit(0.U(64.W))
      val yAxisCnt = RegInit(0.U(64.W))
      val eventCnt = RegInit(0.U(64.W))
      xAxisCnt := xAxisCnt + perfCntX
      yAxisCnt := yAxisCnt + perfCntY
      eventCnt := eventCnt + eventTrigger

      val triggerDB = eventCnt >= granularity.U
      when(triggerDB) {
        eventCnt := eventTrigger
        xAxisCnt := perfCntX
        yAxisCnt := perfCntY
      }
      val rollingPt = new RollingEntry().apply(xAxisCnt, yAxisCnt)
      rollingTable.log(rollingPt, triggerDB, "", clock, reset)
    }
  }
}

object XSPerfPrint {
  def apply(pable: Printable)(ctrlInfo: LogPerfIO)(implicit p: Parameters): Any = {
    XSLog(XSLogLevel.PERF)(ctrlInfo)(true, true.B, pable)
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


  val event_step_0 = Mux(event_op_0(0), events_incr_3.value & events_incr_2.value,
                     Mux(event_op_0(1), events_incr_3.value ^ events_incr_2.value,
                     Mux(event_op_0(2), events_incr_3.value + events_incr_2.value,
                                        events_incr_3.value | events_incr_2.value)))
  val event_step_1 = Mux(event_op_1(0), events_incr_1.value & events_incr_0.value,
                     Mux(event_op_1(1), events_incr_1.value ^ events_incr_0.value,
                     Mux(event_op_1(2), events_incr_1.value + events_incr_0.value,
                                        events_incr_1.value | events_incr_0.value)))

  val event_op_1_reg = RegNext(event_op_1)
  val event_step_0_reg = RegNext(event_step_0)
  val event_step_1_reg = RegNext(event_step_1)
  val selected = Mux(event_op_1_reg(0), event_step_0_reg & event_step_1_reg,
                 Mux(event_op_1_reg(1), event_step_0_reg ^ event_step_1_reg,
                 Mux(event_op_1_reg(2), event_step_0_reg + event_step_1_reg,
                   event_step_0_reg | event_step_1_reg)))

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
