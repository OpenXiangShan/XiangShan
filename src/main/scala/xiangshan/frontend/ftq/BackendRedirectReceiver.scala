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
import xiangshan.HasXSParameter
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BranchPredictionRedirect

trait BackendRedirectReceiver extends HasXSParameter {
  def receiveBackendRedirect(
      fromBackend: CtrlToFtqIO
  ): (Valid[FtqPtr], Valid[BranchPredictionRedirect]) = {
    // Backend sends the redirect index from bju to FTQ one cycle in advance, and FTQ uses this index to read queues.
    // Other redirect indexes are sent to FTQ when the real redirect happens.

    // TODO: Now only channel 0 is used. Will change to single valid bundle and remove ftqIdxSelOH in the future.
    val ftqIdxInAdvance     = fromBackend.ftqIdxAhead(0)
    val ftqIdxInAdvanceNext = RegNext(ftqIdxInAdvance.valid)
    XSError(
      ftqIdxInAdvanceNext && !fromBackend.redirect.valid,
      "FTQ index sent in advance, but no redirect comes in the next cycle\n"
    )

    val redirect = Wire(Valid(new BranchPredictionRedirect))
    redirect.valid := fromBackend.redirect.valid
    redirect.bits.connectRedirect(fromBackend.redirect.bits)
    redirect.bits.BTBMissBubble := false.B // FIXME: wtf?
    val redirectReg = RegNext(redirect)
    XSError(
      ftqIdxInAdvance.valid && redirect.valid,
      "Redirect request sent but ftqIdx is also sent in advance\n"
    )

    val redirectFtqIdx = Wire(Valid(new FtqPtr))
    redirectFtqIdx.valid := redirect.valid
    redirectFtqIdx.bits  := redirect.bits.ftqIdx

    (
      Mux(ftqIdxInAdvanceNext, ftqIdxInAdvance, redirectFtqIdx),
      Mux(ftqIdxInAdvanceNext, redirect, redirectReg)
    )
  }
}
