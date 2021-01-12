package xiangshan.backend.fu.util

import chisel3._
import chisel3.ExcitingUtils.{ConnectionType, Debug}
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
  val Pmpcfg0       = 0x3A0
  val Pmpcfg1       = 0x3A1
  val Pmpcfg2       = 0x3A2
  val Pmpcfg3       = 0x3A3
  val PmpaddrBase   = 0x3B0

  // Machine Counter/Timers
  // Currently, we uses perfcnt csr set instead of standard Machine Counter/Timers
  // 0xB80 - 0x89F are also used as perfcnt csr

  // Machine Counter Setup (not implemented)
  // Debug/Trace Registers (shared with Debug Mode) (not implemented)
  // Debug Mode Registers (not implemented)

  def privEcall  = 0x000.U
  def privEbreak = 0x001.U
  def privMret   = 0x302.U
  def privSret   = 0x102.U
  def privUret   = 0x002.U

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

  val IntPriority = Seq(
    IRQ_MEIP, IRQ_MSIP, IRQ_MTIP,
    IRQ_SEIP, IRQ_SSIP, IRQ_STIP,
    IRQ_UEIP, IRQ_USIP, IRQ_UTIP
  )

  def csrAccessPermissionCheck(addr: UInt, wen: Bool, mode: UInt): Bool = {
    val readOnly = addr(11,10) === "b11".U
    val lowestAccessPrivilegeLevel = addr(9,8)
    mode >= lowestAccessPrivilegeLevel && !(wen && readOnly)
  }
}