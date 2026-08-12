// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import xiangshan.Redirect
import xiangshan.RedirectLevel
import xiangshan.Resolve
import xiangshan.frontend.FrontendRedirect

trait IfuRedirectReceiver extends HasFtqParameters {
  def receiveIfuRedirect(
      wbRedirect:      Valid[FrontendRedirect],
      specTopAddr:     UInt,
      backendRedirect: Bool
  ): (Valid[FtqPtr], Valid[Redirect], Valid[Resolve]) = {
    val redirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))
    val resolve  = WireInit(0.U.asTypeOf(Valid(new Resolve)))

    redirect.valid          := wbRedirect.valid && !backendRedirect
    redirect.bits.ftqIdx    := wbRedirect.bits.ftqIdx
    redirect.bits.ftqOffset := wbRedirect.bits.ftqOffset
    redirect.bits.level     := RedirectLevel.flushAfter
    redirect.bits.isRVC     := wbRedirect.bits.isRVC
    redirect.bits.attribute := wbRedirect.bits.attribute
    redirect.bits.pc        := wbRedirect.bits.pc
    redirect.bits.target    := Mux(wbRedirect.bits.attribute.isReturn, specTopAddr, wbRedirect.bits.target)
    redirect.bits.taken     := wbRedirect.bits.taken
    redirect.bits.isMisPred := true.B

    resolve.valid           := wbRedirect.valid && !backendRedirect && wbRedirect.bits.canTrain
    resolve.bits.ftqIdx     := wbRedirect.bits.ftqIdx
    resolve.bits.ftqOffset  := wbRedirect.bits.ftqOffset
    resolve.bits.pc         := wbRedirect.bits.pc
    resolve.bits.target     := Mux(wbRedirect.bits.attribute.isReturn, specTopAddr, wbRedirect.bits.target)
    resolve.bits.taken      := wbRedirect.bits.taken
    resolve.bits.mispredict := true.B
    resolve.bits.attribute  := wbRedirect.bits.attribute
    resolve.bits.debug_isRVC.foreach(_ := wbRedirect.bits.isRVC)

    val ftqIdx = Wire(Valid(new FtqPtr))
    ftqIdx.valid := redirect.valid
    ftqIdx.bits  := redirect.bits.ftqIdx

    (ftqIdx, RegNext(redirect), RegNext(resolve, init = 0.U.asTypeOf(Valid(new Resolve))))
  }
}
