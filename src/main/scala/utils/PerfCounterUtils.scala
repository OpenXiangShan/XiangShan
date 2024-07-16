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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._

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
