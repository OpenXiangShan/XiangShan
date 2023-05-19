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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._

case class VldRSParams()

class VldRSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
  params.needScheduledBit = true
  params.exuCfg = Some(VldExeUnitCfg)
  override lazy val module = new VldRSImp(params, this)
}

class VldRSImp(params: RSParams, wrapper: VldRSWrapper)
  extends BaseReservationStationImp(params, wrapper)
    with RSImpMemAddrIOConnect {
  extra.load <> rs.flatMap(_.extra.load)
}

class VldRS(params: RSParams)(implicit p: Parameters) extends RSWithMemAddr(params) {
  require(params.numFastWakeup > 0)

  // For load instructions, if its source operand is bypassed from load,
  // we reduce its latency for one cycle since it does not need to read
  // from data array. Timing to be optimized later.

  for (i <- 0 until params.numDeq) {
    val isNormalIssue = s1_issue_oldest(i) || s1_in_selectPtrValid(i)
    val normalIssuePtrOH = Mux(s1_issue_oldest(i), s1_in_oldestPtrOH.bits, s1_in_selectPtrOH(i))
    val normalFastWakeupMatch = Mux1H(normalIssuePtrOH, fastWakeupMatch)
    val wakeupBypassMask = Wire(Vec(params.numFastWakeup, Vec(params.numSrc, Bool())))
    println("wakeupBypassmask")
    println(params.numFastWakeup)
    for (j <- 0 until params.numFastWakeup) {
      for (k <- 0 until params.numSrc) {
        wakeupBypassMask(j)(k) := Mux(isNormalIssue, normalFastWakeupMatch(k)(j), s1_fastWakeup(i)(k)(j))
      }
    }
    // Condition: wakeup by load (to select load wakeup bits)
    extra.load(i).fastMatch := Mux(s1_issuePtrOH(i).valid, VecInit(
      wakeupBypassMask.drop(exuParameters.FmacCnt).take(exuParameters.VlCnt).map(_.asUInt.orR)
    ).asUInt, 0.U)
    extra.load(i).fastImm := s1_out(i).bits.uop.ctrl.imm
  }
}