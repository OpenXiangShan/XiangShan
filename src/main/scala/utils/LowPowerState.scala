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

package utils

import chisel3._
import chisel3.util._

/* WFI state Update */
object WfiStateNext {
  val sNORMAL :: sGCLOCK :: sAWAKE :: Nil = Enum(3)

  def apply(wfiState: UInt, isWFI: Bool, isNormal: Bool, flitpend: Bool, intSrc: UInt): UInt = {
    val nextState = MuxCase(wfiState, Array(
      (wfiState === sNORMAL && isWFI && isNormal && !intSrc.orR) -> sGCLOCK,
      (wfiState === sGCLOCK && intSrc.orR) -> sAWAKE,
      (wfiState === sGCLOCK && flitpend)   -> sNORMAL,
      (wfiState === sAWAKE && !intSrc.orR) -> sNORMAL
    ).toIndexedSeq)

    nextState
  }
}

/* Core low power state Update  */
object lpStateNext {
  val sIDLE :: sL2FLUSH :: sWAITWFI :: sEXITCO :: sPOFFREQ :: Nil = Enum(5)

  def apply(lpState: UInt, l2flush: Bool, l2FlushDone: Bool, isWFI: Bool, exitco: Bool): UInt = {
    val nextState = MuxCase(lpState, Array(
      (lpState === sIDLE && l2flush) -> sL2FLUSH,
      (lpState === sL2FLUSH && l2FlushDone) -> sWAITWFI,
      (lpState === sWAITWFI && isWFI ) -> sEXITCO,
      (lpState === sEXITCO && exitco ) -> sPOFFREQ
    ).toIndexedSeq)

    nextState
  }
}
