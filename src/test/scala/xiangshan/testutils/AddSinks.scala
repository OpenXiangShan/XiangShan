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

package xiangshan.testutils

import chisel3._
import chisel3.util.experimental.BoringUtils
import utils.GTimer

object AddSinks {
  def apply(dispBegin: Int = 0, dispEnd: Int = -1) = {
    val tmp = WireInit(0.U)
    val sinks = Array[String](
      "DTLBFINISH",
      "DTLBPF",
      "DTLBENABLE",
      "perfCntCondMdcacheLoss",
      "perfCntCondMl2cacheLoss",
      "perfCntCondMdcacheHit",
      "lsuMMIO",
      "perfCntCondMl2cacheHit",
      "perfCntCondMl2cacheReq",
      "mtip",
      "perfCntCondMdcacheReq",
      "meip",
      "perfCntCondMbpInstr",
      "perfCntCondMbpRight",
      "perfCntCondMbpWrong",
      "perfCntCondMbpBRight",
      "perfCntCondMbpBWrong",
      "perfCntCondMbpJRight",
      "perfCntCondMbpJWrong",
      "perfCntCondMbpIRight",
      "perfCntCondMbpIWrong",
      "perfCntCondMbpRRight",
      "perfCntCondMbpRWrong",
      "perfCntS1Right",
      "perfCntS1Wrong",
      "perfCntS2Right",
      "perfCntS2Wrong",
      "perfCntS3Right",
      "perfCntS3Wrong",
      "perfCntubtbRight",
      "perfCntubtbWrong",
      "perfCntbtbRight",
      "perfCntbtbWrong",
      "perfCnttageRight",
      "perfCnttageWrong",
      "perfCntrasRight",
      "perfCntrasWrong",
      "perfCntloopRight",
      "perfCntloopWrong",
      "perfCntLoopExit",
      "perfCntTakenAndRight",
      "perfCntTakenButWrong",
      // "CntFetchFromICache",
      // "CntFetchFromLoopBuffer",
      // "CntExitLoop1",
      // "CntExitLoop2",
      // "CntExitLoop3"
    )
    for (s <- sinks){ BoringUtils.addSink(tmp, s) }

    // val disp_enable = WireInit(dispBegin.S(64.W).asUInt() < dispEnd.S(64.W).asUInt())
    val disp_enable = WireInit(true.B)
    val time = GTimer()
    BoringUtils.addSource(disp_enable, "DISPLAY_LOG_ENABLE")
    BoringUtils.addSource(time, "logTimestamp")

  }
}
