package xiangshan.backend.fu.NewCSR

import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
}

class ExceptionBundle extends CSRBundle {
  val exIam    = RW(0).withDescription("Instruction address misaligned exception delegation bit.")
  val exIaf    = RW(1).withDescription("Instruction access fault exception delegation bit.")
  val exII     = RW(2).withDescription("Illegal instruction exception delegation bit.")
  val exBp     = RW(3).withDescription("Breakpoint exception delegation bit.")
  val exLam    = RW(4).withDescription("Load address misaligned exception delegation bit.")
  val exLaf    = RW(5).withDescription("Load access fault exception delegation bit.")
  val exSam    = RW(6).withDescription("Store or AMO address misaligned exception delegation bit.")
  val exSaf    = RW(7).withDescription("Store or AMO access fault exception delegation bit.")
  val exUcall  = RW(8).withDescription("Environment call from U-mode or VU-mode delegation bit.")
  val exHscall = RW(9).withDescription("Environment call from HS-mode delegation bit.")
  val exVscall = RW(10).withDescription("Environment call from VS-mode delegation bit.")
  val exMcall  = RW(11).withDescription("Environment call from M-mode delegation bit.")
  val exIpf    = RW(12).withDescription("Instruction page fault exception delegation bit.")
  val exLpf    = RW(13).withDescription("Load page fault exception delegation bit.")
  // 14 Reserved
  val exSpf    = RW(15).withDescription("Store or AMO page fault exception delegation bit.")
  // double trap
  val exDbltrp = RW(16).withDescription("Double-trap exception delegation bit.")
  // 17 Reserved
  // software check
  val exSwc    = RW(18).withDescription("Software-check exception delegation bit.")
  // hardware error
  val exHwe    = RW(19).withDescription("Hardware-error exception delegation bit.")
  val exIgpf   = RW(20).withDescription("Instruction guest-page fault exception delegation bit.")
  val exLgpf   = RW(21).withDescription("Load guest-page fault exception delegation bit.")
  val exVI     = RW(22).withDescription("Virtual-instruction exception delegation bit.")
  val exSgpf   = RW(23).withDescription("Store or AMO guest-page fault exception delegation bit.")
  // 24-31 Designated for custom use
  // 32-47 Reserved
  // 48-63 Designated for custom use
  // >= 64 Reserved

  def getAddressMisaligned = Seq(exIam, exLam, exSam)

  def getAccessFault = Seq(exIaf, exLaf, exSaf)

  def getPageFault = Seq(exIpf, exLpf, exSpf)

  def getGuestPageFault = Seq(exIgpf, exLgpf, exSgpf)

  def getLSGuestPageFault = Seq(exLgpf, exSgpf)

  def getFetchFault = Seq(exIam, exIaf, exIpf)

  def getLoadFault = Seq(exLam, exLaf, exLpf)

  def getStoreFault = Seq(exSam, exSaf, exSpf)

  def getALL = Seq(exSgpf, exVI, exLgpf, exIgpf, exHwe, exSwc, exDbltrp, exSpf, exLpf, exIpf, exMcall, exVscall,
    exHscall, exUcall, exSaf, exSam, exLaf, exLam, exBp, exII, exIaf, exIam)
}
