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

case class StdRSParams()

class StdRSWrapper(modGen: RSMod)(implicit p: Parameters) extends BaseReservationStationWrapper(modGen) {
  params.needScheduledBit = true
  override lazy val module = new StdRSImp(params, this)
}

class StdRSImp(params: RSParams, wrapper: StdRSWrapper) extends BaseReservationStationImp(params, wrapper) {
  extra.fpRegValue <> rs.flatMap(_.extra.fpRegValue)
}

// delayedFpRf == delayedSrc
class StdRS(params: RSParams)(implicit p: Parameters) extends BaseReservationStation(params)
  with RSDropNotOnRedirect {

  // val credit = 2
  for (((statusUpdate, uop), i) <- statusArray.io.update.zip(s1_dispatchUops_dup.head).zipWithIndex) {
    statusUpdate.data.credit := Mux(s1_delayedSrc(i).asUInt.orR, 2.U, 0.U)
    when (uop.bits.needRfRPort(0, true, false)) {
      s1_delayedSrc(i)(0) := true.B
    }
  }


  /**
    * S1: Data broadcast (from Regfile and FUs) and read
    *
    * Note: this is only needed when read-before-issue
    */
  // dispatch data: the next cycle after enqueue
  for (i <- 0 until params.numEnq) {
    for (j <- 0 until params.numSrc) {
      when (s1_delayedSrc(i)(j)) {
        dataArray.io.write(i).mask(j) := false.B
      }
      dataArray.io.delayedWrite(i).data := DontCare
      dataArray.io.delayedWrite(i).mask(j) := RegNext(RegNext(s1_dispatchUops_dup.head(i).valid && s1_delayedSrc(i)(j)))
      dataArray.io.delayedWrite(i).addr    := RegNext(RegNext(dataArray.io.write(i).addr))
      dataArray.io.delayedWrite(i).data(0) := enqReverse(extra.fpRegValue)(i)
    }
  }
}