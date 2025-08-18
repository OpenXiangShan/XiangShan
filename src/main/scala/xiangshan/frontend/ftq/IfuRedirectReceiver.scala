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
import xiangshan.frontend.PredecodeWritebackBundle

trait IfuRedirectReceiver extends HasFtqParameters {
  def receiveIfuRedirect(
      pdWb: Valid[PredecodeWritebackBundle]
  ): Valid[Redirect] = {
    val redirect = WireInit(0.U.asTypeOf(Valid(new Redirect)))

    redirect.valid          := pdWb.valid && pdWb.bits.misOffset.valid
    redirect.bits.ftqIdx    := pdWb.bits.ftqIdx
    redirect.bits.ftqOffset := pdWb.bits.ftqOffset
    redirect.bits.level     := RedirectLevel.flushAfter
    redirect.bits.pc        := pdWb.bits.pc.toUInt
    redirect.bits.target    := pdWb.bits.target.toUInt
    redirect.bits.taken     := pdWb.bits.cfiOffset.valid

    redirect
  }
}
