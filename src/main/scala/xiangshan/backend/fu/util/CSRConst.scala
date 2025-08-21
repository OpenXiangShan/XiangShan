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

package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs

trait HasCSRConst {
  // Supervisor Custom Read/Write
  val Sbpctl        = 0x5C0
  val Spfctl        = 0x5C1
  val Slvpredctl    = 0x5C2
  val Smblockctl    = 0x5C3
  val Srnctl        = 0x5C4
  /** 0x5C5-0x5E5 for cache instruction register*/
  val Scachebase    = 0x5C5

  // Machine level PMA TODO: remove this
  val PmacfgBase    = 0x7C0
  val PmaaddrBase   = 0x7C8 // 64 entry at most

  // Machine level Bitmap Check(Custom Read/Write)
  val Mbmc = 0xBC2

  def privEcall  = 0x000.U
  def privEbreak = 0x001.U
  def privMNret  = 0x702.U
  def privMret   = 0x302.U
  def privSret   = 0x102.U
  def privUret   = 0x002.U
  def privDret   = 0x7b2.U

  def ModeM     = 0x3.U
  def ModeH     = 0x2.U
  def ModeS     = 0x1.U
  def ModeU     = 0x0.U

  def IRQ_USIP  = 0
  def IRQ_SSIP  = 1
  def IRQ_VSSIP = 2
  def IRQ_MSIP  = 3

  def IRQ_UTIP  = 4
  def IRQ_STIP  = 5
  def IRQ_VSTIP = 6
  def IRQ_MTIP  = 7

  def IRQ_UEIP  = 8
  def IRQ_SEIP  = 9
  def IRQ_VSEIP = 10
  def IRQ_MEIP  = 11

  def IRQ_SGEIP = 12
  def IRQ_DEBUG = 17

  val Hgatp_Mode_len = 4
  val Hgatp_Vmid_len = 16
  val Hgatp_Addr_len = 44

  val Satp_Mode_len = 4
  val Satp_Asid_len = 16
  val Satp_Addr_len = 44
  def satp_part_wmask(max_length: Int, length: Int) : UInt = {
    require(length > 0 && length <= max_length)
    ((1L << length) - 1).U(max_length.W)
  }

  val IntPriority = Seq(
    IRQ_DEBUG,
    IRQ_MEIP, IRQ_MSIP, IRQ_MTIP,
    IRQ_SEIP, IRQ_SSIP, IRQ_STIP,
    IRQ_UEIP, IRQ_USIP, IRQ_UTIP,
    IRQ_VSEIP, IRQ_VSSIP, IRQ_VSTIP, IRQ_SGEIP
  )

  def csrAccessPermissionCheck(addr: UInt, wen: Bool, mode: UInt, virt: Bool, hasH: Bool): UInt = {
    val readOnly = addr(11, 10) === "b11".U
    val lowestAccessPrivilegeLevel = addr(9,8)
    val priv = Mux(mode === ModeS, ModeH, mode)
    val ret = Wire(Bool()) //0.U: normal, 1.U: illegal_instruction, 2.U: virtual instruction
    when (lowestAccessPrivilegeLevel === ModeH && !hasH){
      ret := 1.U
    }.elsewhen (readOnly && wen) {
      ret := 1.U
    }.elsewhen (priv < lowestAccessPrivilegeLevel) {
      when(virt && lowestAccessPrivilegeLevel <= ModeH){
        ret := 2.U
      }.otherwise{
        ret := 1.U
      }
    }.otherwise{
      ret := 0.U
    }
    ret
  }

  def perfcntPermissionCheck(addr: UInt, mode: UInt, mmask: UInt, smask: UInt): Bool = {
    val index = UIntToOH(addr & 31.U)
    Mux(mode === ModeM, true.B, Mux(mode === ModeS, (index & mmask) =/= 0.U, (index & mmask & smask) =/= 0.U))
  }

  def dcsrPermissionCheck(addr: UInt, mModeCanWrite: UInt, debug: Bool): Bool = {
    // debug mode write only regs
    val isDebugReg = addr(11, 4) === "h7b".U
    Mux(!mModeCanWrite && isDebugReg, debug, true.B)
  }

  def triggerPermissionCheck(addr: UInt, mModeCanWrite: UInt, debug: Bool): Bool = {
    val isTriggerReg = addr(11, 4) === "h7a".U
    Mux(!mModeCanWrite && isTriggerReg, debug, true.B)
  }
}
object CSRConst extends HasCSRConst
