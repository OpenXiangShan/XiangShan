/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mu lan PSL v2.
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

import chisel3._
import chisel3.util._
import xiangshan.MatchTriggerIO
import org.chipsalliance.cde.config.Parameters

object ChainCheck {
  def TimingCheck(prevTiming: Bool, thisTiming: Bool, chain: Bool) = !((prevTiming ^ thisTiming) && chain)
  def HitCheck(prevHit: Bool, chain: Bool) = prevHit || !chain
}

object PrintTriggerInfo {
  def apply(enable: Bool, trigger: MatchTriggerIO)(implicit p: Parameters) = {
    XSDebug(enable, p"Debug Mode: Match Type is ${trigger.matchType}; select is ${trigger.select};" +
      p"timing is ${trigger.timing}; action is ${trigger.action}; chain is ${trigger.chain};" +
      p"tdata2 is ${Hexadecimal(trigger.tdata2)}")
  }
}