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
import utility.XSError
import xiangshan.Redirect
import xiangshan.RedirectLevel
import xiangshan.Resolve
import xiangshan.frontend.FrontendRedirect
import xiangshan.frontend.GuardedPcInit

trait IfuRedirectReceiver extends HasFtqParameters {
  def receiveIfuRedirect(
      wbRedirect:      Valid[FrontendRedirect],
      backendRedirect: Bool
  ): (Valid[FtqPtr], Valid[Redirect], Valid[Resolve], Bool) = {
    val redirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))
    val resolve  = WireInit(0.U.asTypeOf(Valid(new Resolve)))

    redirect.valid          := wbRedirect.valid && !backendRedirect
    redirect.bits.ftqIdx    := wbRedirect.bits.ftqIdx
    redirect.bits.ftqOffset := wbRedirect.bits.ftqOffset
    redirect.bits.level     := RedirectLevel.flushAfter
    redirect.bits.isRVC     := wbRedirect.bits.isRVC
    redirect.bits.attribute := wbRedirect.bits.attribute
    redirect.bits.pc        := wbRedirect.bits.pc
    val selectedTarget = GuardedPcInit(wbRedirect.bits.target)
    redirect.bits.target    := selectedTarget.toUInt
    redirect.bits.taken     := wbRedirect.bits.taken
    redirect.bits.isMisPred := true.B

    resolve.valid           := wbRedirect.valid && !backendRedirect && wbRedirect.bits.canTrain
    resolve.bits.ftqIdx     := wbRedirect.bits.ftqIdx
    resolve.bits.ftqOffset  := wbRedirect.bits.ftqOffset
    resolve.bits.pc         := wbRedirect.bits.pc
    resolve.bits.target     := selectedTarget.unGuard
    resolve.bits.taken      := wbRedirect.bits.taken
    resolve.bits.mispredict := true.B
    resolve.bits.attribute  := wbRedirect.bits.attribute
    resolve.bits.debug_isRVC.foreach(_ := wbRedirect.bits.isRVC)

    val ftqIdx = Wire(Valid(new FtqPtr))
    ftqIdx.valid := redirect.valid
    ftqIdx.bits  := redirect.bits.ftqIdx

    val redirectValid    = RegNext(redirect.valid && !redirect.bits.attribute.isReturn && !backendRedirect, false.B)
    val redirectRasValid = RegNext(redirect.valid && redirect.bits.attribute.isReturn && !backendRedirect, false.B)
    val redirectRasValidDelayed = RegNext(redirectRasValid && !backendRedirect, false.B)

    val resolveValid    = RegNext(resolve.valid && !redirect.bits.attribute.isReturn && !backendRedirect, false.B)
    val resolveRasValid = RegNext(resolve.valid && redirect.bits.attribute.isReturn && !backendRedirect, false.B)
    val resolveRasValidDelayed = RegNext(resolveRasValid && !backendRedirect, false.B)

    val redirectDelayed = RegInit(0.U.asTypeOf(new Redirect))
    val resolveDelayed  = RegInit(0.U.asTypeOf(new Resolve))
    when(redirect.valid) {
      redirectDelayed := redirect.bits
    }
    when(resolve.valid) {
      resolveDelayed := resolve.bits
    }
    // A return redirect is delayed one cycle, during which the bits above must be kept stable. A new IFU redirect in
    // that cycle would overwrite them and emit the wrong bits, so it must not happen.
    XSError(redirectRasValid && redirect.valid, "no new IFU redirect is allowed while a return is delayed")

    val ifuRedirect = Wire(new Valid(new Redirect))
    val ifuResolve  = Wire(new Valid(new Resolve))
    ifuRedirect.valid := (redirectValid || redirectRasValidDelayed)
    ifuRedirect.bits  := redirectDelayed
    ifuResolve.valid  := (resolveValid || resolveRasValidDelayed)
    ifuResolve.bits   := resolveDelayed
    (ftqIdx, ifuRedirect, ifuResolve, redirectRasValidDelayed)
  }
}
