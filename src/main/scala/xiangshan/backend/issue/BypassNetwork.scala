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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._


class BypassInfo(numWays: Int, dataBits: Int) extends Bundle {
  val valid = Vec(numWays, Bool())
  val data = UInt(dataBits.W)
}

class BypassNetworkIO(numWays: Int, numBypass: Int, dataBits: Int) extends Bundle {
  val hold = Input(Bool())
  val source = Vec(numWays, Input(UInt(dataBits.W)))
  val target = Vec(numWays, Output(UInt(dataBits.W)))
  val bypass = Vec(numBypass, Input(new BypassInfo(numWays, dataBits)))
}

class BypassNetwork(numWays: Int, numBypass: Int, dataBits: Int)(implicit p: Parameters)
  extends XSModule {

  val io = IO(new BypassNetworkIO(numWays, numBypass, dataBits))

  def doBypass(bypassValid: Seq[Bool], bypassData: Seq[UInt], baseData: UInt, debugIndex: Int = 0): UInt = {
    val bypassVec = VecInit(bypassValid)
    val target = Mux(bypassVec.asUInt.orR, ParallelMux(bypassValid, bypassData), baseData)

    XSError(PopCount(bypassVec) > 1.U, p"bypass mask ${Binary(bypassVec.asUInt)} is not one-hot\n")
    bypassVec.zipWithIndex.map { case (m, i) =>
      XSDebug(bypassVec(i), p"target($debugIndex) bypassed from $i:0x${Hexadecimal(bypassData(i))}\n")
    }

    target
  }

}

// Bypass at the right: RegNext(data) and compute the bypassed data at the next clock cycle
class BypassNetworkRight(numWays: Int, numBypass: Int, dataBits: Int)(implicit p: Parameters)
  extends BypassNetwork(numWays, numBypass, dataBits) {

  val last_cycle_hold = RegInit(false.B)
  last_cycle_hold := io.hold

  val target_reg = Reg(Vec(numWays, UInt(dataBits.W)))
  val bypass_reg = Reg(Vec(numBypass, new BypassInfo(numWays, dataBits)))

  // When last cycle holds the data, no need to update it.
  when (io.hold && !last_cycle_hold) {
    bypass_reg.map(_.valid.map(_ := false.B))
    target_reg := io.target
  }.elsewhen(!io.hold) {
    target_reg := io.source
    for ((by_reg, by_io) <- bypass_reg.zip(io.bypass)) {
      by_reg.data := by_io.data
      by_reg.valid := by_io.valid
    }
  }

  // bypass data to target
  for (i <- 0 until numWays) {
    io.target(i) := doBypass(bypass_reg.map(_.valid(i)), bypass_reg.map(_.data), target_reg(i))
  }

}

// Bypass at the left: compute the bypassed data and RegNext(bypassed_data)
class BypassNetworkLeft(numWays: Int, numBypass: Int, dataBits: Int)(implicit p: Parameters)
  extends BypassNetwork(numWays, numBypass, dataBits) {

  val bypassedData = Reg(io.target.cloneType)

  when (!io.hold) {
    for ((by, i) <- bypassedData.zipWithIndex) {
      by := doBypass(io.bypass.map(_.valid(i)), io.bypass.map(_.data), io.source(i))
    }
  }

  io.target := bypassedData

}

object BypassNetwork {
  def apply(
    numWays: Int,
    numBypass: Int,
    dataBits: Int,
    optFirstStage: Boolean
  )(implicit p: Parameters): BypassNetwork = {
    if (optFirstStage) {
      Module(new BypassNetworkLeft(numWays, numBypass, dataBits))
    }
    else {
      Module(new BypassNetworkRight(numWays, numBypass, dataBits))
    }
  }
}
