// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.TopDownCounters

class FrontendTopDownPipeline(numStage: Int)(implicit p: Parameters) extends FrontendModule {
  require(numStage > 0, "Topdown pipeline must have at least one stage")

  class PipelineIO extends Bundle {
    val in:  TopdownBundle = Input(new TopdownBundle)
    val out: TopdownBundle = Output(new TopdownBundle)

    // override any stage, use .setReason for easier
    val over: Vec[Valid[TopdownBundle]] = Vec(numStage + 1, Flipped(Valid(new TopdownBundle)))
  }

  val io: PipelineIO = IO(new PipelineIO)

  // Track lost slots on every clock edge, independently of valid-ready.
  private val stages = RegInit(VecInit.fill(numStage) {
    val init = Wire(new TopdownBundle)
    init.reason := TopDownCounters.FrontendOtherCoreStall.id.U
    init
  })

  ((io.in +: stages :+ io.out).sliding(2, 1) zip io.over).foreach { case (Seq(prev, curr), over) =>
    curr := Mux(over.valid, over.bits, prev)
  }

  private val reasonsByStage = Array.fill(numStage + 1)(Seq.empty[(Bool, UInt)])

  // Call during elaboration with (Bool, UInt) or (Bool, TopDownCounters.Value) entries.
  // Later calls take priority; within each call, the first matching event wins.
  def setReason(stage: Int, reasons: Seq[Product]): Unit = {
    require(stage >= 0 && stage <= numStage, s"Invalid topdown stage $stage")
    val accumulatedReasons = reasons.map {
      case (en: Bool, value: UInt)                  => (en, value)
      case (en: Bool, value: TopDownCounters.Value) => (en, value.id.U)
      case _ =>
        throw new IllegalArgumentException("setReason reasons must be (Bool, UInt) or (Bool, TopDownCounters.Value)")
    } ++ reasonsByStage(stage)
    reasonsByStage(stage) = accumulatedReasons
    io.over(stage).valid       := accumulatedReasons.map(_._1).foldLeft(false.B)(_ || _)
    io.over(stage).bits.reason := PriorityMux(accumulatedReasons)
  }

  // setReason for all stages
  def setReason(reasons: Seq[Product]): Unit =
    io.over.indices.foreach(setReason(_, reasons))
}
