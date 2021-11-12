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

object TriggerCmp {
  def apply(actual: UInt, tdata: UInt, matchType: UInt, enable: Bool) = {
    val equal = actual === tdata
    val greater = actual >= tdata
    val less = actual <= tdata
    val res = MuxLookup(matchType, false.B,
      Array(0.U -> equal,
          2.U -> greater,
          3.U -> less))
    res && enable
  }
}

object ChainCheck {
  def TimingCheck(prevTiming: Bool, thisTiming: Bool, chain: Bool) = !((prevTiming ^ thisTiming) && chain)
  def HitCheck(prevHit: Bool, chain: Bool) = prevHit || !chain
}
