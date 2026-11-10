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

trait BackendRedirectReceiver extends HasFtqParameters {
  def receiveBackendRedirect(
      fromBackend: CtrlToFtqIO
  ): (Valid[FtqPtr], Valid[Redirect]) = {
    // Backend sends the redirect index from bju to FTQ one cycle in advance, and FTQ uses this index to read queues.
    // Other redirect indexes are sent to FTQ when the real redirect happens. If ftqIdxAhead is sent in advance, FTQ can
    // process the redirect when it comes. Otherwise, FTQ has to delay the redirect for one cycle.

    val redirect = fromBackend.redirect

    // ftqIdxAhead has lower priority than real redirect
    // Magic! Do not touch!!!
    val ftqIdxAhead = Wire(Valid(new FtqPtr))
    ftqIdxAhead.valid := fromBackend.ftqIdxAhead.valid && !redirect.valid
    ftqIdxAhead.bits  := fromBackend.ftqIdxAhead.bits
    val ftqIdxAheadNext = RegNext(ftqIdxAhead)

    val aheadIdxMatch = ftqIdxAheadNext.valid && redirect.bits.ftqIdx === ftqIdxAheadNext.bits

    val redirectReg = RegNext(redirect)
    redirectReg.valid := redirect.valid && !aheadIdxMatch

    val ftqIdx = Wire(Valid(new FtqPtr))
    ftqIdx.valid := redirect.valid && !aheadIdxMatch
    ftqIdx.bits  := redirect.bits.ftqIdx

    (
      Mux(redirect.valid, ftqIdx, ftqIdxAhead),
      Mux(aheadIdxMatch, redirect, redirectReg)
    )
  }
}
