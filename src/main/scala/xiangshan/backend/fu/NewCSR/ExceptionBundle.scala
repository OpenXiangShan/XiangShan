package xiangshan.backend.fu.NewCSR

import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
}

class ExceptionBundle extends CSRBundle {
  val EX_IAM    = RW(0).withDescription("Instruction address misaligned exception delegation bit.")
  val EX_IAF    = RW(1).withDescription("Instruction access fault exception delegation bit.")
  val EX_II     = RW(2).withDescription("Illegal instruction exception delegation bit.")
  val EX_BP     = RW(3).withDescription("Breakpoint exception delegation bit.")
  val EX_LAM    = RW(4).withDescription("Load address misaligned exception delegation bit.")
  val EX_LAF    = RW(5).withDescription("Load access fault exception delegation bit.")
  val EX_SAM    = RW(6).withDescription("Store or AMO address misaligned exception delegation bit.")
  val EX_SAF    = RW(7).withDescription("Store or AMO access fault exception delegation bit.")
  val EX_UCALL  = RW(8).withDescription("Environment call from U-mode or VU-mode delegation bit.")
  val EX_HSCALL = RW(9).withDescription("Environment call from HS-mode delegation bit.")
  val EX_VSCALL = RW(10).withDescription("Environment call from VS-mode delegation bit.")
  val EX_MCALL  = RW(11).withDescription("Environment call from M-mode delegation bit.")
  val EX_IPF    = RW(12).withDescription("Instruction page fault exception delegation bit.")
  val EX_LPF    = RW(13).withDescription("Load page fault exception delegation bit.")
  // 14 Reserved
  val EX_SPF    = RW(15).withDescription("Store or AMO page fault exception delegation bit.")
  // double trap
  val EX_DBLTRP = RW(16).withDescription("Double-trap exception delegation bit.")
  // 17 Reserved
  // software check
  val EX_SWC    = RW(18).withDescription("Software-check exception delegation bit.")
  // hardware error
  val EX_HWE    = RW(19).withDescription("Hardware-error exception delegation bit.")
  val EX_IGPF   = RW(20).withDescription("Instruction guest-page fault exception delegation bit.")
  val EX_LGPF   = RW(21).withDescription("Load guest-page fault exception delegation bit.")
  val EX_VI     = RW(22).withDescription("Virtual-instruction exception delegation bit.")
  val EX_SGPF   = RW(23).withDescription("Store or AMO guest-page fault exception delegation bit.")
  // 24-31 Designated for custom use
  // 32-47 Reserved
  // 48-63 Designated for custom use
  // >= 64 Reserved

  def getAddressMisaligned = Seq(EX_IAM, EX_LAM, EX_SAM)

  def getAccessFault = Seq(EX_IAF, EX_LAF, EX_SAF)

  def getPageFault = Seq(EX_IPF, EX_LPF, EX_SPF)

  def getGuestPageFault = Seq(EX_IGPF, EX_LGPF, EX_SGPF)

  def getLSGuestPageFault = Seq(EX_LGPF, EX_SGPF)

  def getFetchFault = Seq(EX_IAM, EX_IAF, EX_IPF)

  def getLoadFault = Seq(EX_LAM, EX_LAF, EX_LPF)

  def getStoreFault = Seq(EX_SAM, EX_SAF, EX_SPF)

  def getALL = Seq(EX_SGPF, EX_VI, EX_LGPF, EX_IGPF, EX_HWE, EX_SWC, EX_DBLTRP, EX_SPF, EX_LPF, EX_IPF, EX_MCALL, EX_VSCALL,
    EX_HSCALL, EX_UCALL, EX_SAF, EX_SAM, EX_LAF, EX_LAM, EX_BP, EX_II, EX_IAF, EX_IAM)
}
