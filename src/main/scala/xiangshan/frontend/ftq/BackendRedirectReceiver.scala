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
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.Bundles.connectSamePort

trait BackendRedirectReceiver extends HasFtqParameters {
  def receiveBackendRedirect(
      fromBackend: CtrlToFtqIO
  ): (Valid[FtqPtr], Valid[Redirect]) = {
    // Backend sends the redirect index from bju to FTQ one cycle in advance, and FTQ uses this index to read queues.
    // Other redirect indexes are sent to FTQ when the real redirect happens. If ftqIdxInAdvance is sent in advance,
    // FTQ can process the redirect when it comes. Otherwise, FTQ has to delay the redirect for one cycle.

    // TODO: Now only channel 0 is used. Will change to single valid bundle and remove ftqIdxSelOH in the future.
    val ftqIdxInAdvance = Wire(Valid(new FtqPtr))
    ftqIdxInAdvance.valid := fromBackend.ftqIdxAhead(0).valid && !fromBackend.redirect.valid
    ftqIdxInAdvance.bits  := fromBackend.ftqIdxAhead(0).bits
    val ftqIdxInAdvanceValidNext = RegNext(ftqIdxInAdvance.valid)
    // TODO: why this is not designed as folloing two assertions?
//    XSError(
//      ftqIdxInAdvanceValidNext && !fromBackend.redirect.valid,
//      "FTQ index sent in advance, but no redirect comes in the next cycle\n"
//    )
//    XSError(
//      ftqIdxInAdvanceValidNext && fromBackend.redirect.bits.ftqIdx =/= RegNext(ftqIdxInAdvance.bits),
//      "FTQ index sent in advance, but it is not the same with redirect FTQ index\n"
//    )

    // val redirect    = fromBackend.redirect
    val redirect    = Wire(Valid(new Redirect))
    redirect.valid := fromBackend.redirect.valid
    connectSamePort(redirect.bits, fromBackend.redirect.bits)
    val redirectReg = RegNext(redirect)
    redirectReg.valid := redirect.valid && !ftqIdxInAdvanceValidNext

    val ftqIdx = Wire(Valid(new FtqPtr))
    ftqIdx.valid := redirect.valid
    ftqIdx.bits  := redirect.bits.ftqIdx

    (
      Mux(ftqIdxInAdvance.valid, ftqIdxInAdvance, ftqIdx),
      Mux(ftqIdxInAdvanceValidNext, redirect, redirectReg)
    )
  }
}
