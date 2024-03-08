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
}
