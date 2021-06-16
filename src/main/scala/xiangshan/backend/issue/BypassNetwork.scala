/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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


class BypassInfo(numWays: Int, dataBits: Int, optBuf: Boolean = false) extends Bundle {
  val validWidth = (if (optBuf) dataBits else 1)

  val valid = Vec(numWays, UInt(validWidth.W))
  val data = UInt(dataBits.W)

  override def cloneType: BypassInfo.this.type =
    new BypassInfo(numWays, dataBits, optBuf).asInstanceOf[this.type]
}

class BypassNetworkIO(numWays: Int, numBypass: Int, dataBits: Int) extends Bundle {
  val hold = Input(Bool())
  val source = Vec(numWays, Input(UInt(dataBits.W)))
  val target = Vec(numWays, Output(UInt(dataBits.W)))
  val bypass = Vec(numBypass, Input(new BypassInfo(numWays, dataBits)))

  override def cloneType: BypassNetworkIO.this.type =
    new BypassNetworkIO(numWays, numBypass, dataBits).asInstanceOf[this.type]
}

class BypassNetwork(numWays: Int, numBypass: Int, dataBits: Int, optBuf: Boolean)(implicit p: Parameters)
  extends XSModule {
  val io = IO(new BypassNetworkIO(numWays, numBypass, dataBits))

  val target_reg = Reg(Vec(numWays, UInt(dataBits.W)))
  val bypass_reg = Reg(Vec(numBypass, new BypassInfo(numWays, dataBits, optBuf)))

  when (io.hold) {
    target_reg := io.target
    if (optBuf) {
      bypass_reg.map(_.valid.map(_ := 0.U))
    }
    else {
      bypass_reg.map(_.valid.map(_ := false.B))
    }
  }.otherwise {
    target_reg := io.source
    for ((by_reg, by_io) <- bypass_reg.zip(io.bypass)) {
      by_reg.data := by_io.data
      if (optBuf) {
        // duplicate bypass mask to avoid too many FO4s and hurting timing
        by_reg.valid := VecInit(by_io.valid.map(v => Cat(Seq.fill(dataBits)(v))))
      }
      else {
        by_reg.valid := by_io.valid
      }
    }
  }

  // bypass data to target
  for (i <- 0 until numWays) {
    if (optBuf) {
      val bypassData = VecInit((0 until dataBits).map(j => {
        val mask = VecInit(bypass_reg.map(_.valid(i)(j)))
        Mux(mask.asUInt.orR, Mux1H(mask, bypass_reg.map(_.data(j))), target_reg(i)(j))
      })).asUInt
      io.target(i) := bypassData
    }
    else {
      val mask = VecInit(bypass_reg.map(_.valid(i).asBool))
      io.target(i) := Mux(mask.asUInt.orR, Mux1H(mask, bypass_reg.map(_.data)), target_reg(i))

      XSError(PopCount(mask) > 1.U, p"bypass mask ${Binary(mask.asUInt)} is not one-hot\n")
      mask.zipWithIndex.map { case (m, j) =>
        XSDebug(mask(j), p"target($i) bypassed from $j:0x${Hexadecimal(bypass_reg(j).data)}\n")
      }
    }
  }
}
