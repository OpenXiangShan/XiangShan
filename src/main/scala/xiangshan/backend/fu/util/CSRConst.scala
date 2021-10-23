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
import xiangshan._
import xiangshan.backend._
import utils.XSDebug

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

  // User Counter/Timers
  val Cycle         = 0xC00
  val Time          = 0xC01
  val Instret       = 0xC02

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
  /** 0x5C5-0x5E5 for cache instruction register*/

  val Sdsid         = 0x9C0

  // Machine Information Registers
  val Mvendorid     = 0xF11
  val Marchid       = 0xF12
  val Mimpid        = 0xF13
  val Mhartid       = 0xF14

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
  val PmaaddrBase   = 0x7C8 // 84 entry at most

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

  // Machine Counter Setup (not implemented)
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
  // Debug Mode Registers
  val Dcsr          = 0x7B0
  val Dpc           = 0x7B1
  val Dscratch      = 0x7B2
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

  val Asid_true_len = 16
  
  def Asid_true_mask(AsidLength : Int) : UInt = {
    val res = Wire(Vec(Asid_true_len,Bool()))
    (0 until Asid_true_len).map(i => {
      res(i) := (i <= AsidLength).B
  })
    Cat(res.reverse)
  // val zero = "h0".U(1.W)
  // val one = "h1".U(1.W)
  // val mask_high = Fill(Asid_true_len - AsidLength, zero)
  // val mask_low  = Fill(AsidLength, one)

  // Cat(mask_high, mask_low)
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
}
