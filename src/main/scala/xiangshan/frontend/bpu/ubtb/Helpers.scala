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

package xiangshan.frontend.bpu.ubtb

import chisel3._
import chisel3.util._
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.TargetState

trait Helpers extends HasMicroBtbParameters {
  def getTag(vAddr: PrunedAddr): UInt =
    vAddr(TagWidth, instOffsetBits)

  def getTargetUpper(vAddr: PrunedAddr): UInt =
    vAddr(VAddrBits - 1, TargetWidth + instOffsetBits)

  def getFixedTargetUpper(startVAddr: PrunedAddr, targetState: Option[TargetState]): UInt = {
    val startVAddrUpper = getTargetUpper(startVAddr)
    if (EnableTargetFix) {
      MuxCase(
        startVAddrUpper,
        Seq(
          targetState.get.isCarry  -> (startVAddrUpper + 1.U),
          targetState.get.isBorrow -> (startVAddrUpper - 1.U)
        )
      )
    } else {
      startVAddrUpper
    }
  }

  def getFullTarget(startVAddr: PrunedAddr, target: UInt, targetState: Option[TargetState]): PrunedAddr =
    PrunedAddrInit(Cat(
      getFixedTargetUpper(startVAddr, targetState), // (VAddrBits - 1, TargetWidth + instOffsetBits)
      target,                                       // (TargetWidth + instOffsetBits - 1, instOffsetBits)
      0.U(instOffsetBits.W)                         // (instOffsetBits - 1, 0)
    ))

  def getEntryTarget(fullTarget: PrunedAddr): UInt =
    fullTarget(TargetWidth, instOffsetBits)

  def getEntryTargetState(startVAddr: PrunedAddr, fullTarget: PrunedAddr): TargetState = {
    val startVAddrUpper = getTargetUpper(startVAddr)
    val targetUpper     = getTargetUpper(fullTarget)
    MuxCase(
      TargetState.NoCarryAndBorrow,
      Seq(
        (targetUpper > startVAddrUpper) -> TargetState.Carry,
        (targetUpper < startVAddrUpper) -> TargetState.Borrow
      )
    )
  }
}
