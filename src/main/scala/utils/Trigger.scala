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

object TriggerCmpConsecutive {
  def apply(actual: Vec[UInt], tdata: UInt, matchType: UInt, enable: Bool, VAddrBits: Int) : Vec[Bool] = {
    // opt: only compare two possible high bits: orig and orig+1
    val len1 = actual.length
    val highPos = log2Up(len1)
    val lowPC = Wire(Vec(len1, UInt(highPos.W)))
    lowPC.zipWithIndex.map{case (h, i) => h := actual(i)(highPos - 1, 0)}
    val highPC = actual(0)(VAddrBits - 1, highPos)
    val highPC1 = actual(0)(VAddrBits - 1, highPos) + 1.U
    val highTdata = tdata(VAddrBits - 1, highPos)

    val highPCEqual = highPC === highTdata
    val highPC1Equal = highPC1 === highTdata
    val highPCGreater = highPC >= highTdata
    val highPC1Greater = highPC1 >= highTdata
    val highPCLess = highPC <= highTdata
    val highPC1Less = highPC1 <= highTdata

    val carry = Wire(Vec(len1, Bool()))
    carry.zipWithIndex.map{case (c, i) => c := actual(i)(highPos) =/= actual(0)(highPos)}

    val lowPCEqual = Wire(Vec(len1, Bool()))
    val lowPCGreater = Wire(Vec(len1, Bool()))
    val lowPCLess = Wire(Vec(len1, Bool()))

    lowPCEqual.zipWithIndex.map{case (l, i) => l := actual(i)(highPos - 1, 0) === tdata(highPos - 1, 0)}
    lowPCGreater.zipWithIndex.map{case (l, i) => l := actual(i)(highPos - 1, 0) >= tdata(highPos - 1, 0)}
    lowPCLess.zipWithIndex.map{case (l, i) => l := actual(i)(highPos - 1, 0) <= tdata(highPos - 1, 0)}

    val overallEqual = Wire(Vec(len1, Bool()))
    val overallGreater = Wire(Vec(len1, Bool()))
    val overallLess = Wire(Vec(len1, Bool()))

    overallEqual.zipWithIndex.map{case (o, i) => o := lowPCEqual(i) && highPCEqual}

    // greater: 1. highPC > highTdata; 2. highPC == highTdata && lowPC >= lowTdata
    overallGreater.zipWithIndex.map{case (o, i) => o := highPCGreater || ((!carry(i) || lowPCGreater(i)) && highPCEqual)}

    // less: 1. highPC < highTdata; 2. highPC == highTdata && lowPC <= lowTdata
    overallLess.zipWithIndex.map{case (o, i) => o := highPCLess || ((!carry(i) && lowPCLess(i)) && highPCEqual)}

    val ret = Wire(Vec(len1, Bool()))

    ret.zipWithIndex.map{case (r, i) => r := MuxLookup(matchType, false.B,
      Array(0.U -> overallEqual(i),
        2.U -> overallGreater(i),
        3.U -> overallLess(i))) && enable}
    ret
  }
}

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