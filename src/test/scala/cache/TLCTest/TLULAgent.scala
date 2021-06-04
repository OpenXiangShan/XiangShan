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

package cache.TLCTest

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import chipsalliance.rocketchip.config.Parameters

import scala.util.Random
import chisel3.util._

import scala.math.pow

class TLULMasterAgent(ID: Int, name: String, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                      , scoreboard: mutable.Map[BigInt, ScoreboardData])
                     (implicit p: Parameters)
  extends TLCAgent(ID, name, addrStateMap, serialList, scoreboard) {
  val outerGet: mutable.Map[BigInt, GetCallerTrans] = mutable.Map[BigInt, GetCallerTrans]()
  val outerPut: mutable.Map[BigInt, PutCallerTrans] = mutable.Map[BigInt, PutCallerTrans]()

  override def transStep(): Unit = {
    outerGet.foreach(_._2.step())
    outerPut.foreach(_._2.step())
  }

  var tmpA = new TLCScalaA()
  var a_cnt = 0
  var a_cnt_end = 0
  var tmpD = new TLCScalaD()
  var d_cnt = 0
  var d_cnt_end = 0
  var snapData: BigInt = 0

  def fireD(inD: TLCScalaD): Unit = {
    if (inD.opcode == AccessAckData) {
      d_cnt_end = countBeats(inD.size)
      if (d_cnt == 0) { //start burst
        val getT = outerGet(inD.source)
        snapData = insertVersionRead(getT.a.get.address, inD.param)
        tmpD = inD.copy()
        d_cnt += 1
      }
      else {
        tmpD.data = dataConcatBeat(tmpD.data, inD.data, d_cnt)
        d_cnt += 1
      }
      if (d_cnt == d_cnt_end) {
        d_cnt = 0
        val getT = outerGet(inD.source)
        getT.pairAccessAckData(tmpD)
        insertMaskedReadSnap(getT.a.get.address, tmpD.data, snapData, getT.a.get.mask)
        outerGet.remove(inD.source)
      }
    }
    else if (inD.opcode == AccessAck) {
      val putT = outerPut(inD.source)
      if (inD.param == 0) // if in l2
        insertMaskedWrite(putT.a.get.address, putT.a.get.data, putT.a.get.mask)
      outerPut.remove(inD.source)
    }
    else {
      assert(false, f"UL unhandled opcode: ${inD.opcode}")
    }
  }

  def fireA(inA: TLCScalaA): Unit = {
    if (inA.opcode == Get) {
      handleA(inA)
    }
    else if (inA.opcode == PutFullData || inA.opcode == PutPartialData) {
      if (a_cnt == 0) { //start burst
        a_cnt_end = countBeats(inA.size)
        tmpA = inA.copy()
        a_cnt += 1
      }
      else {
        tmpA.mask = maskConcatBeat(tmpA.mask, inA.mask, a_cnt)
        tmpA.data = dataConcatBeat(tmpA.data, inA.data, a_cnt)
        a_cnt += 1
      }
      if (a_cnt == a_cnt_end) {
        a_cnt = 0
        handleA(tmpA)
      }
    }
    else {
      assert(false, f"UL unhandled opcode: ${inA.opcode}")
    }
  }

  def handleA(inA: TLCScalaA): Unit = {
    if (inA.opcode == Get) {
      val getT = new GetCallerTrans()
      val beats = countBeats(inA.size)
      if (beats > 1) { //fill n mask if there are n beats
        inA.mask = (0 until beats).foldLeft(BigInt(0))(
          (cmask, i) => maskConcatBeat(cmask, inA.mask, i))
      }
      getT.pairGet(inA)
      outerGet(inA.source) = getT
    }
    else if (inA.opcode == PutFullData || inA.opcode == PutPartialData) {
      val putT = new PutCallerTrans()
      putT.pairPut(inA)
      outerPut(inA.source) = putT
    }
  }

}