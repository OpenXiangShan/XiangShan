package xiangshan.backend.fu.NewCSR

import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
}

class ExceptionBundle extends CSRBundle {
  val EX_IAM    = RW(0)
  val EX_IAF    = RW(1)
  val EX_II     = RW(2)
  val EX_BP     = RW(3)
  val EX_LAM    = RW(4)
  val EX_LAF    = RW(5)
  val EX_SAM    = RW(6)
  val EX_SAF    = RW(7)
  val EX_UCALL  = RW(8)
  val EX_HSCALL = RW(9)
  val EX_VSCALL = RW(10)
  val EX_MCALL  = RW(11)
  val EX_IPF    = RW(12)
  val EX_LPF    = RW(13)
  // 14 Reserved
  val EX_SPF    = RW(15)
  // 16-19 Reserved
  val EX_IGPF   = RW(20)
  val EX_LGPF   = RW(21)
  val EX_VI     = RW(22)
  val EX_SGPF   = RW(23)
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

  def getALL = Seq(EX_SGPF, EX_VI, EX_LGPF, EX_IGPF, EX_SPF, EX_LPF, EX_IPF, EX_MCALL, EX_VSCALL,
    EX_HSCALL, EX_UCALL, EX_SAF, EX_SAM, EX_LAF, EX_LAM, EX_BP, EX_II, EX_IAF, EX_IAM)
}
