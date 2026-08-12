/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{XORFold, XSPerfAccumulate, XSPerfHistogram}
import xiangshan.mem.HasL1PrefetchSourceParameter

case class L1DBPParams(
  sampleBits: Int = 2,
  pcPredictorEntries: Int = 8192,
  accessedIncrement: Int = 1,
  bypassHoldCycles: Int = 4,
  counterWidth: Int = 2,
  pcHash: (UInt, Int) => UInt = (pc, width) => XORFold(pc >> 1, width)
) {
  require(sampleBits >= 1, "L1DBP sampleBits must be positive")
  require(pcPredictorEntries >= 2 && isPow2(pcPredictorEntries),
    "L1DBP PC predictor entries must be a power of two and at least two")
  require(counterWidth >= 1, "L1DBP counter width must be positive")
  val counterMax = (1 << counterWidth) - 1
  require(accessedIncrement >= 1 && accessedIncrement <= counterMax,
    s"L1DBP accessed counter increment must be in [1, $counterMax]")
  require(bypassHoldCycles >= 1 && bypassHoldCycles <= 16,
    "L1DBP bypass hold cycles must be in [1, 16]")
  require(pcPredictorEntries >= 2 && isPow2(pcPredictorEntries),
    "L1DBP PC predictor entries must be a power of two and at least two")
  require(accessedIncrement >= 1 && accessedIncrement <= counterMax,
    s"L1DBP accessed counter increment must be in [1, $counterMax]")
  require(bypassHoldCycles >= 1 && bypassHoldCycles <= 16,
    "L1DBP bypass hold cycles must be in [1, 16]")
  require(counterWidth >= 1,
    "L1DBP counter width must be positive")

}

object L1DBPOrigin {
  val width = 2
  val demand = 0.U(width.W)
  val stream = 1.U(width.W)
  val stride = 2.U(width.W)
}

class L1DBP(readPorts: Int, writePorts: Int, enableBypass: Boolean = false)(implicit p: Parameters)
  extends DCacheModule {

}
