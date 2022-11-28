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
import utils._
import xiangshan._

case class LoadRSParams()

class LoadRSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
  params.needScheduledBit = true
  params.exuCfg = Some(LdExeUnitCfg)

  override lazy val module = new LoadRSImp(params, this)
}

class LoadRSImp(params: RSParams, wrapper: LoadRSWrapper)
  extends BaseReservationStationImp(params, wrapper)
  with RSImpMemAddrIOConnect {
  extra.load <> rs.flatMap(_.extra.load)
}

class LoadRS(params: RSParams)(implicit p: Parameters) extends RSWithMemAddr(params) {
  require(params.numFastWakeup > 0)

  // For load instructions, if its source operand is bypassed from load,
  // we reduce its latency for one cycle since it does not need to read
  // from data array. Timing to be optimized later.

  for (i <- 0 until params.numDeq) {
    val isNormalIssue = s1_issue_oldest(i) || s1_in_selectPtrValid(i)
    val normalIssuePtrOH = Mux(s1_issue_oldest(i), s1_in_oldestPtrOH.bits, s1_in_selectPtrOH(i))
    val normalFastWakeupMatch = Mux1H(normalIssuePtrOH, fastWakeupMatch)
    val wakeupBypassMask = Wire(Vec(params.numFastWakeup, Vec(params.numSrc, Bool())))
    for (j <- 0 until params.numFastWakeup) {
      for (k <- 0 until params.numSrc) {
        wakeupBypassMask(j)(k) := Mux(isNormalIssue, normalFastWakeupMatch(k)(j), s1_fastWakeup(i)(k)(j))
        // 在 fastWakeupMatch（和statusArray中表项匹配） 和 s1_fastWakeup（和入队表项匹配） 之间选择
      }
    }
    // Condition: wakeup by load (to select load wakeup bits)
    extra.load(i).fastMatch := Mux(s1_issuePtrOH(i).valid, VecInit(
      wakeupBypassMask.drop(exuParameters.AluCnt).take(exuParameters.LduCnt).map(_.asUInt.orR)
      // 只有来自 load单元的快速唤醒信号 才有可能 fastMatch
    ).asUInt, 0.U)
    extra.load(i).fastImm := s1_out(i).bits.uop.ctrl.imm
  }
}