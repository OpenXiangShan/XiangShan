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
import utils._
import utility._
import xiangshan._
import xiangshan.backend._

trait HasCSRConst {

  // User Trap Setup
  val Ustatus       = 0x000
  val Uie           = 0x004
  val Utvec         = 0x005

  // User Trap Handling
  val Uscratch      = 0x040
  val Uepc          = 0x041
  val Ucause        = 0x042
  val Utval         = 0x043
  val Uip           = 0x044

  // User Floating-Point CSRs (not implemented)
  val Fflags        = 0x001
  val Frm           = 0x002
  val Fcsr          = 0x003

  // Vector Extension CSRs
  val Vstart        = 0x008
  val Vxsat         = 0x009
  val Vxrm          = 0x00A
  val Vcsr          = 0x00F
  val Vl            = 0xC20
  val Vtype         = 0xC21
  val Vlenb         = 0xC22

  // User Counter/Timers
  val Cycle         = 0xC00
  val Time          = 0xC01
  val Instret       = 0xC02
  val Hpmcounter3   = 0xC03
  val Hpmcounter4   = 0xC04
  val Hpmcounter5   = 0xC05
  val Hpmcounter6   = 0xC06
  val Hpmcounter7   = 0xC07
  val Hpmcounter8   = 0xC08
  val Hpmcounter9   = 0xC09
  val Hpmcounter10  = 0xC0A
  val Hpmcounter11  = 0xC0B
  val Hpmcounter12  = 0xC0C
  val Hpmcounter13  = 0xC0D
  val Hpmcounter14  = 0xC0E
  val Hpmcounter15  = 0xC0F
  val Hpmcounter16  = 0xC10
  val Hpmcounter17  = 0xC11
  val Hpmcounter18  = 0xC12
  val Hpmcounter19  = 0xC13
  val Hpmcounter20  = 0xC14
  val Hpmcounter21  = 0xC15
  val Hpmcounter22  = 0xC16
  val Hpmcounter23  = 0xC17
  val Hpmcounter24  = 0xC18
  val Hpmcounter25  = 0xC19
  val Hpmcounter26  = 0xC1A
  val Hpmcounter27  = 0xC1B
  val Hpmcounter28  = 0xC1C
  val Hpmcounter29  = 0xC1D
  val Hpmcounter30  = 0xC1E
  val Hpmcounter31  = 0xC1F

  // Supervisor Trap Setup
  val Sstatus       = 0x100
  val Sedeleg       = 0x102
  val Sideleg       = 0x103
  val Sie           = 0x104
  val Stvec         = 0x105
  val Scounteren    = 0x106

  // Supervisor Trap Handling
  val Sscratch      = 0x140
  val Sepc          = 0x141
  val Scause        = 0x142
  val Stval         = 0x143
  val Sip           = 0x144

  // Supervisor Protection and Translation
  val Satp          = 0x180

  // Supervisor Custom Read/Write
  val Sbpctl        = 0x5C0
  val Spfctl        = 0x5C1
  val Slvpredctl    = 0x5C2
  val Smblockctl    = 0x5C3
  val Srnctl        = 0x5C4
  val Scachebase    = 0x5C5
  val Sfetchctl     = 0x5C6

  /** 0x5C5-0x5E5 for cache instruction register*/

  val Sdsid         = 0x9C0

  // Machine Information Registers
  val Mvendorid     = 0xF11
  val Marchid       = 0xF12
  val Mimpid        = 0xF13
  val Mhartid       = 0xF14
  val Mconfigptr    = 0xF15

  // Machine Trap Setup
  val Mstatus       = 0x300
  val Misa          = 0x301
  val Medeleg       = 0x302
  val Mideleg       = 0x303
  val Mie           = 0x304
  val Mtvec         = 0x305
  val Mcounteren    = 0x306

  // Machine Trap Handling
  val Mscratch      = 0x340
  val Mepc          = 0x341
  val Mcause        = 0x342
  val Mtval         = 0x343
  val Mip           = 0x344

  // Machine Memory Protection
  // TBD
  val PmpcfgBase    = 0x3A0
  val PmpaddrBase   = 0x3B0
  // Machine level PMA
  val PmacfgBase    = 0x7C0
  val PmaaddrBase   = 0x7C8 // 64 entry at most

  // Machine Counter/Timers
  // Currently, we uses perfcnt csr set instead of standard Machine Counter/Timers
  // 0xB80 - 0x89F are also used as perfcnt csr
  val Mcycle   = 0xb00
  val Minstret = 0xb02

  val Mhpmcounter3  = 0xB03
  val Mhpmcounter4  = 0xB04
  val Mhpmcounter5  = 0xB05
  val Mhpmcounter6  = 0xB06
  val Mhpmcounter7  = 0xB07
  val Mhpmcounter8  = 0xB08
  val Mhpmcounter9  = 0xB09
  val Mhpmcounter10 = 0xB0A
  val Mhpmcounter11 = 0xB0B
  val Mhpmcounter12 = 0xB0C
  val Mhpmcounter13 = 0xB0D
  val Mhpmcounter14 = 0xB0E
  val Mhpmcounter15 = 0xB0F
  val Mhpmcounter16 = 0xB10
  val Mhpmcounter17 = 0xB11
  val Mhpmcounter18 = 0xB12
  val Mhpmcounter19 = 0xB13
  val Mhpmcounter20 = 0xB14
  val Mhpmcounter21 = 0xB15
  val Mhpmcounter22 = 0xB16
  val Mhpmcounter23 = 0xB17
  val Mhpmcounter24 = 0xB18
  val Mhpmcounter25 = 0xB19
  val Mhpmcounter26 = 0xB1A
  val Mhpmcounter27 = 0xB1B
  val Mhpmcounter28 = 0xB1C
  val Mhpmcounter29 = 0xB1D
  val Mhpmcounter30 = 0xB1E
  val Mhpmcounter31 = 0xB1F

  val Mcountinhibit = 0x320
  val Mhpmevent3    = 0x323
  val Mhpmevent4    = 0x324
  val Mhpmevent5    = 0x325
  val Mhpmevent6    = 0x326
  val Mhpmevent7    = 0x327
  val Mhpmevent8    = 0x328
  val Mhpmevent9    = 0x329
  val Mhpmevent10   = 0x32A
  val Mhpmevent11   = 0x32B
  val Mhpmevent12   = 0x32C
  val Mhpmevent13   = 0x32D
  val Mhpmevent14   = 0x32E
  val Mhpmevent15   = 0x32F
  val Mhpmevent16   = 0x330
  val Mhpmevent17   = 0x331
  val Mhpmevent18   = 0x332
  val Mhpmevent19   = 0x333
  val Mhpmevent20   = 0x334
  val Mhpmevent21   = 0x335
  val Mhpmevent22   = 0x336
  val Mhpmevent23   = 0x337
  val Mhpmevent24   = 0x338
  val Mhpmevent25   = 0x339
  val Mhpmevent26   = 0x33A
  val Mhpmevent27   = 0x33B
  val Mhpmevent28   = 0x33C
  val Mhpmevent29   = 0x33D
  val Mhpmevent30   = 0x33E
  val Mhpmevent31   = 0x33F

  // Debug/Trace Registers (shared with Debug Mode) (not implemented)

  // Trigger Registers
  val Tselect = 0x7A0
  val Tdata1 = 0x7A1
  val Tdata2 = 0x7A2
  val Tinfo = 0x7A4
  val Tcontrol = 0x7A5

  // Debug Mode Registers
  val Dcsr          = 0x7B0
  val Dpc           = 0x7B1
  val Dscratch0     = 0x7B2
  val Dscratch1     = 0x7B3

  def privEcall  = 0x000.U
  def privEbreak = 0x001.U
  def privMret   = 0x302.U
  def privSret   = 0x102.U
  def privUret   = 0x002.U
  def privDret   = 0x7b2.U

  def ModeM     = 0x3.U
  def ModeH     = 0x2.U
  def ModeS     = 0x1.U
  def ModeU     = 0x0.U

  def IRQ_UEIP  = 0
  def IRQ_SEIP  = 1
  def IRQ_MEIP  = 3

  def IRQ_UTIP  = 4
  def IRQ_STIP  = 5
  def IRQ_MTIP  = 7

  def IRQ_USIP  = 8
  def IRQ_SSIP  = 9
  def IRQ_MSIP  = 11

  def IRQ_DEBUG = 12

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
    IRQ_UEIP, IRQ_USIP, IRQ_UTIP
  )

  def csrAccessPermissionCheck(addr: UInt, wen: Bool, mode: UInt): Bool = {
    val readOnly = addr(11,10) === "b11".U
    val lowestAccessPrivilegeLevel = addr(9,8)
    mode >= lowestAccessPrivilegeLevel && !(wen && readOnly)
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