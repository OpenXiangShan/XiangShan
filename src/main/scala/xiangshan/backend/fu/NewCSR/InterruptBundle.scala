package xiangshan.backend.fu.NewCSR

import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
}

class InterruptBundle extends CSRBundle {
  // Software Interrupt
  val SSI      = RW(1)
  val VSSI     = RW(2)
  val MSI      = RW(3)
  // Time Interrupt
  val STI      = RW(5)
  val VSTI     = RW(6)
  val MTI      = RW(7)
  // External Interrupt
  val SEI      = RW(9)
  val VSEI     = RW(10)
  val MEI      = RW(11)
  val SGEI     = RW(12)
  // SoC
  val COI      = RW(13) // Counter overflow interrupt
  val LPRASEI  = RW(35) // Low-priority RAS event interrupt
  val HPRASEI  = RW(43) // High-priority RAS event interrupt

  def getVS = Seq(VSSI, VSTI, VSEI)

  def getHS = Seq(SSI, STI, SEI)

  def getM = Seq(MSI, MTI, MEI)

  def getSOC = Seq(COI, LPRASEI, HPRASEI)
}

class InterruptPendingBundle extends CSRBundle {
  // Software Interrupt
  val SSIP     = RW(1)
  val VSSIP    = RW(2)
  val MSIP     = RW(3)
  // Time Interrupt
  val STIP     = RW(5)
  val VSTIP    = RW(6)
  val MTIP     = RW(7)
  // External Interrupt
  val SEIP     = RW(9)
  val VSEIP    = RW(10)
  val MEIP     = RW(11)
  val SGEIP    = RW(12)
  // SoC
  val COIP     = RW(13) // Counter overflow interrupt
  val LPRASEIP = RW(35) // Low-priority RAS event interrupt
  val HPRASEIP = RW(43) // High-priority RAS event interrupt

  def getVS = Seq(VSSIP, VSTIP, VSEIP)

  def getHS = Seq(SSIP, STIP, SEIP)

  def getM = Seq(MSIP, MTIP, MEIP)

  def getSOC = Seq(COIP, LPRASEIP, HPRASEIP)

  def getALL = Seq(SSIP, VSSIP, MSIP, STIP, VSTIP, MTIP, SEIP, VSEIP, MEIP, SGEIP, COIP, LPRASEIP, HPRASEIP)
}

class InterruptEnableBundle extends CSRBundle {
  // Software Interrupt
  val SSIE     = RW(1)
  val VSSIE    = RW(2)
  val MSIE     = RW(3)
  // Time Interrupt
  val STIE     = RW(5)
  val VSTIE    = RW(6)
  val MTIE     = RW(7)
  // External Interrupt
  val SEIE     = RW(9)
  val VSEIE    = RW(10)
  val MEIE     = RW(11)
  val SGEIE    = RW(12)
  // SoC
  val COIE     = RW(13) // Counter overflow interrupt
  val LPRASEIE = RW(35) // Low-priority RAS event interrupt
  val HPRASEIE = RW(43) // High-priority RAS event interrupt

  def getVS = Seq(VSSIE, VSTIE, VSEIE)

  def getHS = Seq(SSIE, STIE, SEIE)

  def getM = Seq(MSIE, MTIE, MEIE)

  def getALL = Seq(SSIE, VSSIE, MSIE, STIE, VSTIE, MTIE, SEIE, VSEIE, MEIE, SGEIE, COIE, LPRASEIE, HPRASEIE)
}
