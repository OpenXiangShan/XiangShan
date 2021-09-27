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

// See LICENSE.Berkeley for license details.

package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan.XSBundle

trait MemoryOpConstants {
  val NUM_XA_OPS = 9
  val M_SZ      = 5
  def M_X       = BitPat("b?????")
  def M_XRD     = "b00000".U // int load
  def M_XWR     = "b00001".U // int store
  def M_PFR     = "b00010".U // prefetch with intent to read
  def M_PFW     = "b00011".U // prefetch with intent to write
  def M_XA_SWAP = "b00100".U
  def M_FLUSH_ALL = "b00101".U  // flush all lines
  def M_XLR     = "b00110".U
  def M_XSC     = "b00111".U
  def M_XA_ADD  = "b01000".U
  def M_XA_XOR  = "b01001".U
  def M_XA_OR   = "b01010".U
  def M_XA_AND  = "b01011".U
  def M_XA_MIN  = "b01100".U
  def M_XA_MAX  = "b01101".U
  def M_XA_MINU = "b01110".U
  def M_XA_MAXU = "b01111".U
  def M_FLUSH   = "b10000".U // write back dirty data and cede R/W permissions
  def M_PWR     = "b10001".U // partial (masked.U store
  def M_PRODUCE = "b10010".U // write back dirty data and cede W permissions
  def M_CLEAN   = "b10011".U // write back dirty data and retain R/W permissions
  def M_SFENCE  = "b10100".U // flush TLB
  def M_WOK     = "b10111".U // check write permissions but don't perform a write

  def isAMOLogical(cmd: UInt) = cmd === M_XA_SWAP || cmd === M_XA_XOR || cmd === M_XA_OR || cmd === M_XA_AND
  def isAMOArithmetic(cmd: UInt) = cmd === M_XA_ADD || cmd === M_XA_MIN || cmd === M_XA_MAX || cmd === M_XA_MINU || cmd === M_XA_MAXU
  def isAMO(cmd: UInt) = isAMOLogical(cmd) || isAMOArithmetic(cmd)
  def isPrefetch(cmd: UInt) = cmd === M_PFR || cmd === M_PFW
  def isRead(cmd: UInt) = cmd === M_XRD || cmd === M_XLR || cmd === M_XSC || isAMO(cmd)
  def isWrite(cmd: UInt) = cmd === M_XWR || cmd === M_PWR || cmd === M_XSC || isAMO(cmd)
  def isWriteIntent(cmd: UInt) = isWrite(cmd) || cmd === M_PFW || cmd === M_XLR
}

object MemoryOpConstants extends MemoryOpConstants {
  def getMemoryOpName(cmd: UInt): String = {
    val opNames = Map(
      M_XRD -> "M_XRD",
      M_XWR -> "M_XWR",
      M_PFR -> "M_PFR",
      M_PFW -> "M_PFW",
      M_XA_SWAP -> "M_XA_SWAP",
      M_FLUSH_ALL -> "M_FLUSH_ALL",
      M_XLR -> "M_XLR",
      M_XSC -> "M_XSC",
      M_XA_ADD -> "M_XA_ADD",
      M_XA_XOR -> "M_XA_XOR",
      M_XA_OR -> "M_XA_OR",
      M_XA_AND -> "M_XA_AND",
      M_XA_MIN -> "M_XA_MIN",
      M_XA_MAX -> "M_XA_MAX",
      M_XA_MINU -> "M_XA_MINU",
      M_XA_MAXU -> "M_XA_MAXU",
      M_FLUSH -> "M_FLUSH",
      M_PWR -> "M_PWR",
      M_PRODUCE -> "M_PRODUCE",
      M_CLEAN -> "M_CLEAN",
      M_SFENCE -> "M_SFENCE",
      M_WOK -> "M_WOK"
    )
    val opLitNames = opNames map {case (k, v) => (k.litValue.longValue, v)}
    return opLitNames(cmd.litValue.longValue)
  }
}
