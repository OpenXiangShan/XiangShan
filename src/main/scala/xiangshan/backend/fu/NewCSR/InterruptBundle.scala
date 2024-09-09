package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW}
import xiangshan.backend.fu.NewCSR.ChiselRecordForField.AddRecordSpecifyFields

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
  val LCOFI    = RW(13) // Counter overflow interrupt
  val LC14I    = RO(14)
  val LC15I    = RO(15)
  val LC16I    = RO(16)
  val LC17I    = RO(17)
  val LC18I    = RO(18)
  val LC19I    = RO(19)
  val LC20I    = RO(20)
  val LC21I    = RO(21)
  val LC22I    = RO(22)
  val LC23I    = RO(23)
  val LC24I    = RO(24)
  val LC25I    = RO(25)
  val LC26I    = RO(26)
  val LC27I    = RO(27)
  val LC28I    = RO(28)
  val LC29I    = RO(29)
  val LC30I    = RO(30)
  val LC31I    = RO(31)
  val LC32I    = RO(32)
  val LC33I    = RO(33)
  val LC34I    = RO(34)
  val LPRASEI  = RO(35)
  val LC36I    = RO(36)
  val LC37I    = RO(37)
  val LC38I    = RO(38)
  val LC39I    = RO(39)
  val LC40I    = RO(40)
  val LC41I    = RO(41)
  val LC42I    = RO(42)
  val HPRASEI  = RO(43)
  val LC44I    = RO(44)
  val LC45I    = RO(45)
  val LC46I    = RO(46)
  val LC47I    = RO(47)
  val LC48I    = RO(48)
  val LC49I    = RO(49)
  val LC50I    = RO(50)
  val LC51I    = RO(51)
  val LC52I    = RO(52)
  val LC53I    = RO(53)
  val LC54I    = RO(54)
  val LC55I    = RO(55)
  val LC56I    = RO(56)
  val LC57I    = RO(57)
  val LC58I    = RO(58)
  val LC59I    = RO(59)
  val LC60I    = RO(60)
  val LC61I    = RO(61)
  val LC62I    = RO(62)
  val LC63I    = RO(63)

  def getVS = Seq(VSSI, VSTI, VSEI)

  def getHS = Seq(SSI, STI, SEI)

  def getM = Seq(MSI, MTI, MEI)

  def getNonLocal = Seq(
    SSI, VSSI, MSI,
    STI, VSTI, MTI,
    SEI, VSEI, MEI,
    SGEI
  )

  def getLocal = Seq(
    LCOFI,LC14I,LC15I,
    LC16I,LC17I,LC18I,LC19I,LC20I,LC21I,LC22I,LC23I,
    LC24I,LC25I,LC26I,LC27I,LC28I,LC29I,LC30I,LC31I,
    LC32I,LC33I,LC34I,LPRASEI,LC36I,LC37I,LC38I,LC39I,
    LC40I,LC41I,LC42I,HPRASEI,LC44I,LC45I,LC46I,LC47I,
    LC48I,LC49I,LC50I,LC51I,LC52I,LC53I,LC54I,LC55I,
    LC56I,LC57I,LC58I,LC59I,LC60I,LC61I,LC62I,LC63I,
  )

  def getALL = getNonLocal ++ getLocal
}

class InterruptPendingBundle extends CSRBundle {
  // Software Interrupt
  val SSIP     = RO(1)
  val VSSIP    = RO(2)
  val MSIP     = RO(3)
  // Time Interrupt
  val STIP     = RO(5)
  val VSTIP    = RO(6)
  val MTIP     = RO(7)
  // External Interrupt
  val SEIP     = RO(9)
  val VSEIP    = RO(10)
  val MEIP     = RO(11)
  val SGEIP    = RO(12)
  // Local Interrupt
  val LCOFIP   = RO(13) // Counter overflow interrupt
  val LC14IP   = RO(14)
  val LC15IP   = RO(15)
  val LC16IP   = RO(16)
  val LC17IP   = RO(17)
  val LC18IP   = RO(18)
  val LC19IP   = RO(19)
  val LC20IP   = RO(20)
  val LC21IP   = RO(21)
  val LC22IP   = RO(22)
  val LC23IP   = RO(23)
  val LC24IP   = RO(24)
  val LC25IP   = RO(25)
  val LC26IP   = RO(26)
  val LC27IP   = RO(27)
  val LC28IP   = RO(28)
  val LC29IP   = RO(29)
  val LC30IP   = RO(30)
  val LC31IP   = RO(31)
  val LC32IP   = RO(32)
  val LC33IP   = RO(33)
  val LC34IP   = RO(34)
  val LPRASEIP = RO(35) // Low-priority RAS event interrupt
  val LC36IP   = RO(36)
  val LC37IP   = RO(37)
  val LC38IP   = RO(38)
  val LC39IP   = RO(39)
  val LC40IP   = RO(40)
  val LC41IP   = RO(41)
  val LC42IP   = RO(42)
  val HPRASEIP = RO(43) // High-priority RAS event interrupt
  val LC44IP   = RO(44)
  val LC45IP   = RO(45)
  val LC46IP   = RO(46)
  val LC47IP   = RO(47)
  val LC48IP   = RO(48)
  val LC49IP   = RO(49)
  val LC50IP   = RO(50)
  val LC51IP   = RO(51)
  val LC52IP   = RO(52)
  val LC53IP   = RO(53)
  val LC54IP   = RO(54)
  val LC55IP   = RO(55)
  val LC56IP   = RO(56)
  val LC57IP   = RO(57)
  val LC58IP   = RO(58)
  val LC59IP   = RO(59)
  val LC60IP   = RO(60)
  val LC61IP   = RO(61)
  val LC62IP   = RO(62)
  val LC63IP   = RO(63)

  def getVS = Seq(VSSIP, VSTIP, VSEIP)

  def getHS = Seq(SSIP, STIP, SEIP)

  def getM = Seq(MSIP, MTIP, MEIP)

  def getNonLocal = Seq(
    SSIP, VSSIP, MSIP,
    STIP, VSTIP, MTIP,
    SEIP, VSEIP, MEIP,
    SGEIP
  )

  def getLocal = Seq(
    LCOFIP,LC14IP,LC15IP,
    LC16IP,LC17IP,LC18IP,LC19IP,LC20IP,LC21IP,LC22IP,LC23IP,
    LC24IP,LC25IP,LC26IP,LC27IP,LC28IP,LC29IP,LC30IP,LC31IP,
    LC32IP,LC33IP,LC34IP,LPRASEIP,LC36IP,LC37IP,LC38IP,LC39IP,
    LC40IP,LC41IP,LC42IP,HPRASEIP,LC44IP,LC45IP,LC46IP,LC47IP,
    LC48IP,LC49IP,LC50IP,LC51IP,LC52IP,LC53IP,LC54IP,LC55IP,
    LC56IP,LC57IP,LC58IP,LC59IP,LC60IP,LC61IP,LC62IP,LC63IP,
  )

  def getALL = getNonLocal ++ getLocal
}

class InterruptEnableBundle extends CSRBundle {
  // Software Interrupt
  val SSIE     = RO(1)
  val VSSIE    = RO(2)
  val MSIE     = RO(3)
  // Time Interrupt
  val STIE     = RO(5)
  val VSTIE    = RO(6)
  val MTIE     = RO(7)
  // External Interrupt
  val SEIE     = RO(9)
  val VSEIE    = RO(10)
  val MEIE     = RO(11)
  val SGEIE    = RO(12)
  // SoC
  val LCOFIE   = RO(13) // Counter overflow interrupt
  val LC14IE   = RO(14)
  val LC15IE   = RO(15)
  val LC16IE   = RO(16)
  val LC17IE   = RO(17)
  val LC18IE   = RO(18)
  val LC19IE   = RO(19)
  val LC20IE   = RO(20)
  val LC21IE   = RO(21)
  val LC22IE   = RO(22)
  val LC23IE   = RO(23)
  val LC24IE   = RO(24)
  val LC25IE   = RO(25)
  val LC26IE   = RO(26)
  val LC27IE   = RO(27)
  val LC28IE   = RO(28)
  val LC29IE   = RO(29)
  val LC30IE   = RO(30)
  val LC31IE   = RO(31)
  val LC32IE   = RO(32)
  val LC33IE   = RO(33)
  val LC34IE   = RO(34)
  val LPRASEIE = RO(35) // Low-priority RAS event interrupt
  val LC36IE   = RO(36)
  val LC37IE   = RO(37)
  val LC38IE   = RO(38)
  val LC39IE   = RO(39)
  val LC40IE   = RO(40)
  val LC41IE   = RO(41)
  val LC42IE   = RO(42)
  val HPRASEIE = RO(43) // High-priority RAS event interrupt
  val LC44IE   = RO(44)
  val LC45IE   = RO(45)
  val LC46IE   = RO(46)
  val LC47IE   = RO(47)
  val LC48IE   = RO(48)
  val LC49IE   = RO(49)
  val LC50IE   = RO(50)
  val LC51IE   = RO(51)
  val LC52IE   = RO(52)
  val LC53IE   = RO(53)
  val LC54IE   = RO(54)
  val LC55IE   = RO(55)
  val LC56IE   = RO(56)
  val LC57IE   = RO(57)
  val LC58IE   = RO(58)
  val LC59IE   = RO(59)
  val LC60IE   = RO(60)
  val LC61IE   = RO(61)
  val LC62IE   = RO(62)
  val LC63IE   = RO(63)

  def getVS = Seq(VSSIE, VSTIE, VSEIE)

  def getHS = Seq(SSIE, STIE, SEIE)

  def getM = Seq(MSIE, MTIE, MEIE)

  def getNonVS = this.getHS ++ this.getM ++ this.getLocal :+ this.SGEIE

  def getNonLocal = Seq(
    SSIE, VSSIE, MSIE,
    STIE, VSTIE, MTIE,
    SEIE, VSEIE, MEIE,
    SGEIE
  )

  def getLocal = Seq(
    LCOFIE,LC14IE,LC15IE,
    LC16IE,LC17IE,LC18IE,LC19IE,LC20IE,LC21IE,LC22IE,LC23IE,
    LC24IE,LC25IE,LC26IE,LC27IE,LC28IE,LC29IE,LC30IE,LC31IE,
    LC32IE,LC33IE,LC34IE,LPRASEIE,LC36IE,LC37IE,LC38IE,LC39IE,
    LC40IE,LC41IE,LC42IE,HPRASEIE,LC44IE,LC45IE,LC46IE,LC47IE,
    LC48IE,LC49IE,LC50IE,LC51IE,LC52IE,LC53IE,LC54IE,LC55IE,
    LC56IE,LC57IE,LC58IE,LC59IE,LC60IE,LC61IE,LC62IE,LC63IE
  )

  def getALL = getNonLocal ++ getLocal

  def getRW = getALL.filter(_.isRW)
}

class NonMaskableIRPendingBundle extends CSRBundle {
  val NMI = RW(1).withReset(0.U)
  // reserve for more NMI type
}
object NonMaskableIRNO{
  final val NMI = 1
  // reserve for more NMI type

  val interruptDefaultPrio = Seq(
    NMI
  )
  def getIRQHigherThan(irq: Int): Seq[Int] = {
    val idx = this.interruptDefaultPrio.indexOf(irq, 0)
    require(idx != -1, s"The irq($irq) does not exists in IntPriority Seq")
    this.interruptDefaultPrio.slice(0, idx)
  }

}

object InterruptNO {
  // Software Interrupt
  final val SSI  = 1
  final val VSSI = 2
  final val MSI  = 3
  // Time Interrupt
  final val STI  = 5
  final val VSTI = 6
  final val MTI  = 7
  // External Interrupt
  final val SEI  = 9
  final val VSEI = 10
  final val MEI  = 11
  final val SGEI = 12
  // SoC
  final val COI = 13
  final val LPRASEI = 35
  final val HPRASEI = 43

  val interruptDefaultPrio = Seq(
    MEI, MSI, MTI,
    SEI, SSI, STI,
    SGEI,
    VSEI, VSSI, VSTI,
    COI,
  )

  val localHighGroup = Seq(
    47, 23, 46,
    45, 22, 44,
    HPRASEI, 21, 42,
    41, 20, 40,
  )

  val localLowGroup = Seq(
    39, 19, 38,
    37, 18, 36,
    LPRASEI, 17, 34,
    33, 16, 32,
  )

  val customHighestGroup = Seq(
    63, 31, 62,
    61, 30, 60,
  )

  val customMiddleHighGroup = Seq(
    59, 29, 58,
    57, 28, 56,
  )

  val customMiddleLowGroup = Seq(
    55, 27, 54,
    53, 26, 52,
  )

  val customLowestGroup = Seq(
    51, 25, 50,
    49, 24, 48,
  )

  def getPrioIdxInGroup(group: this.type => Seq[Int])(f: this.type => Int): Int = {
    val idx = group(this).indexOf(f(this))
    assert(idx != -1)
    idx
  }

  def getVS = Seq(VSSI, VSTI, VSEI)

  def getHS = Seq(SSI, STI, SEI)

  def getM = Seq(MSI, MTI, MEI)

  def getNonLocal = Seq(
    SSI, VSSI, MSI,
    STI, VSTI, MTI,
    SEI, VSEI, MEI,
    SGEI
  )

  def getLocal = localHighGroup ++ localLowGroup ++
                 customHighestGroup ++ customMiddleHighGroup ++
                 customMiddleLowGroup ++ customLowestGroup ++ Seq(COI)
}

trait HasIpIeBundle { self: CSRModule[_] =>
  val mideleg = IO(Input(new MidelegBundle))
  val mip     = IO(Input(new MipBundle))
  val mie     = IO(Input(new MieBundle))
  val mvip    = IO(Input(new MvipBundle))
  val mvien   = IO(Input(new MvienBundle))
  val hideleg = IO(Input(new HidelegBundle))
  val hip     = IO(Input(new HipBundle))
  val hie     = IO(Input(new HieBundle))
  val hvien   = IO(Input(new HvienBundle))
  val hvip    = IO(Input(new HvipBundle))
  val sip     = IO(Input(new SipBundle))
  val sie     = IO(Input(new SieBundle))
  val vsip    = IO(Input(new VSipBundle))
  val vsie    = IO(Input(new VSieBundle))
  val hgeip   = IO(Input(new HgeipBundle))
  val hgeie   = IO(Input(new HgeieBundle))
  val hstatusVGEIN = IO(Input(HstatusVgeinField()))
}

trait ToAliasIpLocalPart extends Bundle {
  val LCOFIP   = ValidIO(RO(13)) // Counter overflow interrupt
  val LC14IP   = ValidIO(RO(14))
  val LC15IP   = ValidIO(RO(15))
  val LC16IP   = ValidIO(RO(16))
  val LC17IP   = ValidIO(RO(17))
  val LC18IP   = ValidIO(RO(18))
  val LC19IP   = ValidIO(RO(19))
  val LC20IP   = ValidIO(RO(20))
  val LC21IP   = ValidIO(RO(21))
  val LC22IP   = ValidIO(RO(22))
  val LC23IP   = ValidIO(RO(23))
  val LC24IP   = ValidIO(RO(24))
  val LC25IP   = ValidIO(RO(25))
  val LC26IP   = ValidIO(RO(26))
  val LC27IP   = ValidIO(RO(27))
  val LC28IP   = ValidIO(RO(28))
  val LC29IP   = ValidIO(RO(29))
  val LC30IP   = ValidIO(RO(30))
  val LC31IP   = ValidIO(RO(31))
  val LC32IP   = ValidIO(RO(32))
  val LC33IP   = ValidIO(RO(33))
  val LC34IP   = ValidIO(RO(34))
  val LPRASEIP = ValidIO(RO(35)) // Low-priority RAS event interrupt
  val LC36IP   = ValidIO(RO(36))
  val LC37IP   = ValidIO(RO(37))
  val LC38IP   = ValidIO(RO(38))
  val LC39IP   = ValidIO(RO(39))
  val LC40IP   = ValidIO(RO(40))
  val LC41IP   = ValidIO(RO(41))
  val LC42IP   = ValidIO(RO(42))
  val HPRASEIP = ValidIO(RO(43)) // High-priority RAS event interrupt
  val LC44IP   = ValidIO(RO(44))
  val LC45IP   = ValidIO(RO(45))
  val LC46IP   = ValidIO(RO(46))
  val LC47IP   = ValidIO(RO(47))
  val LC48IP   = ValidIO(RO(48))
  val LC49IP   = ValidIO(RO(49))
  val LC50IP   = ValidIO(RO(50))
  val LC51IP   = ValidIO(RO(51))
  val LC52IP   = ValidIO(RO(52))
  val LC53IP   = ValidIO(RO(53))
  val LC54IP   = ValidIO(RO(54))
  val LC55IP   = ValidIO(RO(55))
  val LC56IP   = ValidIO(RO(56))
  val LC57IP   = ValidIO(RO(57))
  val LC58IP   = ValidIO(RO(58))
  val LC59IP   = ValidIO(RO(59))
  val LC60IP   = ValidIO(RO(60))
  val LC61IP   = ValidIO(RO(61))
  val LC62IP   = ValidIO(RO(62))
  val LC63IP   = ValidIO(RO(63))

  def getLocal = Seq(
    LCOFIP, LC14IP, LC15IP,
    LC16IP, LC17IP, LC18IP, LC19IP, LC20IP, LC21IP, LC22IP, LC23IP,
    LC24IP, LC25IP, LC26IP, LC27IP, LC28IP, LC29IP, LC30IP, LC31IP,
    LC32IP, LC33IP, LC34IP, LPRASEIP, LC36IP, LC37IP, LC38IP, LC39IP,
    LC40IP, LC41IP, LC42IP, HPRASEIP, LC44IP, LC45IP, LC46IP, LC47IP,
    LC48IP, LC49IP, LC50IP, LC51IP, LC52IP, LC53IP, LC54IP, LC55IP,
    LC56IP, LC57IP, LC58IP, LC59IP, LC60IP, LC61IP, LC62IP, LC63IP,
  )
}

class IeValidBundle extends Bundle with IgnoreSeqInBundle {
  val SSIE     = ValidIO(RO( 1))
  val VSSIE    = ValidIO(RO( 2))
  val MSIE     = ValidIO(RO( 3))
  val STIE     = ValidIO(RO( 5))
  val VSTIE    = ValidIO(RO( 6))
  val MTIE     = ValidIO(RO( 7))
  val SEIE     = ValidIO(RO( 9))
  val VSEIE    = ValidIO(RO(10))
  val MEIE     = ValidIO(RO(11))
  val SGEIE    = ValidIO(RO(12))

  val LCOFIE   = ValidIO(RO(13)) // Counter overflow interrupt
  val LC14IE   = ValidIO(RO(14))
  val LC15IE   = ValidIO(RO(15))
  val LC16IE   = ValidIO(RO(16))
  val LC17IE   = ValidIO(RO(17))
  val LC18IE   = ValidIO(RO(18))
  val LC19IE   = ValidIO(RO(19))
  val LC20IE   = ValidIO(RO(20))
  val LC21IE   = ValidIO(RO(21))
  val LC22IE   = ValidIO(RO(22))
  val LC23IE   = ValidIO(RO(23))
  val LC24IE   = ValidIO(RO(24))
  val LC25IE   = ValidIO(RO(25))
  val LC26IE   = ValidIO(RO(26))
  val LC27IE   = ValidIO(RO(27))
  val LC28IE   = ValidIO(RO(28))
  val LC29IE   = ValidIO(RO(29))
  val LC30IE   = ValidIO(RO(30))
  val LC31IE   = ValidIO(RO(31))
  val LC32IE   = ValidIO(RO(32))
  val LC33IE   = ValidIO(RO(33))
  val LC34IE   = ValidIO(RO(34))
  val LPRASEIE = ValidIO(RO(35)) // Low-priority RAS event interrupt
  val LC36IE   = ValidIO(RO(36))
  val LC37IE   = ValidIO(RO(37))
  val LC38IE   = ValidIO(RO(38))
  val LC39IE   = ValidIO(RO(39))
  val LC40IE   = ValidIO(RO(40))
  val LC41IE   = ValidIO(RO(41))
  val LC42IE   = ValidIO(RO(42))
  val HPRASEIE = ValidIO(RO(43)) // High-priority RAS event interrupt
  val LC44IE   = ValidIO(RO(44))
  val LC45IE   = ValidIO(RO(45))
  val LC46IE   = ValidIO(RO(46))
  val LC47IE   = ValidIO(RO(47))
  val LC48IE   = ValidIO(RO(48))
  val LC49IE   = ValidIO(RO(49))
  val LC50IE   = ValidIO(RO(50))
  val LC51IE   = ValidIO(RO(51))
  val LC52IE   = ValidIO(RO(52))
  val LC53IE   = ValidIO(RO(53))
  val LC54IE   = ValidIO(RO(54))
  val LC55IE   = ValidIO(RO(55))
  val LC56IE   = ValidIO(RO(56))
  val LC57IE   = ValidIO(RO(57))
  val LC58IE   = ValidIO(RO(58))
  val LC59IE   = ValidIO(RO(59))
  val LC60IE   = ValidIO(RO(60))
  val LC61IE   = ValidIO(RO(61))
  val LC62IE   = ValidIO(RO(62))
  val LC63IE   = ValidIO(RO(63))

  val getVS = Seq(VSSIE, VSTIE, VSEIE)

  def getHS = Seq(SSIE, STIE, SEIE)

  def getM = Seq(MSIE, MTIE, MEIE)

  def getNonLocal = Seq(
    SSIE, VSSIE, MSIE,
    STIE, VSTIE, MTIE,
    SEIE, VSEIE, MEIE,
    SGEIE
  )

  def getLocal = Seq(
    LCOFIE, LC14IE, LC15IE,
    LC16IE, LC17IE, LC18IE, LC19IE, LC20IE, LC21IE, LC22IE, LC23IE,
    LC24IE, LC25IE, LC26IE, LC27IE, LC28IE, LC29IE, LC30IE, LC31IE,
    LC32IE, LC33IE, LC34IE, LPRASEIE, LC36IE, LC37IE, LC38IE, LC39IE,
    LC40IE, LC41IE, LC42IE, HPRASEIE, LC44IE, LC45IE, LC46IE, LC47IE,
    LC48IE, LC49IE, LC50IE, LC51IE, LC52IE, LC53IE, LC54IE, LC55IE,
    LC56IE, LC57IE, LC58IE, LC59IE, LC60IE, LC61IE, LC62IE, LC63IE,
  )

  def getAll = getNonLocal ++ getLocal

  def getRW = getAll.filter(_.bits.isRW)

  def getNonRW = getAll.filterNot(_.bits.isRW)

  def getByNum(num: Int) = getAll.find(_.bits.lsb == num).get

  def connectZeroNonRW : this.type = {
    this.getNonRW.foreach(_.specifyField(
      _.valid := false.B,
      _.bits  := DontCare
    ))
    this
  }
}

class IpValidBundle extends Bundle with IgnoreSeqInBundle {
  val SSIP     = ValidIO(RO( 1))
  val VSSIP    = ValidIO(RO( 2))
  val MSIP     = ValidIO(RO( 3))
  val STIP     = ValidIO(RO( 5))
  val VSTIP    = ValidIO(RO( 6))
  val MTIP     = ValidIO(RO( 7))
  val SEIP     = ValidIO(RO( 9))
  val VSEIP    = ValidIO(RO(10))
  val MEIP     = ValidIO(RO(11))
  val SGEIP    = ValidIO(RO(12))

  val LCOFIP   = ValidIO(RO(13)) // Counter overflow interrupt
  val LC14IP   = ValidIO(RO(14))
  val LC15IP   = ValidIO(RO(15))
  val LC16IP   = ValidIO(RO(16))
  val LC17IP   = ValidIO(RO(17))
  val LC18IP   = ValidIO(RO(18))
  val LC19IP   = ValidIO(RO(19))
  val LC20IP   = ValidIO(RO(20))
  val LC21IP   = ValidIO(RO(21))
  val LC22IP   = ValidIO(RO(22))
  val LC23IP   = ValidIO(RO(23))
  val LC24IP   = ValidIO(RO(24))
  val LC25IP   = ValidIO(RO(25))
  val LC26IP   = ValidIO(RO(26))
  val LC27IP   = ValidIO(RO(27))
  val LC28IP   = ValidIO(RO(28))
  val LC29IP   = ValidIO(RO(29))
  val LC30IP   = ValidIO(RO(30))
  val LC31IP   = ValidIO(RO(31))
  val LC32IP   = ValidIO(RO(32))
  val LC33IP   = ValidIO(RO(33))
  val LC34IP   = ValidIO(RO(34))
  val LPRASEIP = ValidIO(RO(35)) // Low-priority RAS event interrupt
  val LC36IP   = ValidIO(RO(36))
  val LC37IP   = ValidIO(RO(37))
  val LC38IP   = ValidIO(RO(38))
  val LC39IP   = ValidIO(RO(39))
  val LC40IP   = ValidIO(RO(40))
  val LC41IP   = ValidIO(RO(41))
  val LC42IP   = ValidIO(RO(42))
  val HPRASEIP = ValidIO(RO(43)) // High-priority RAS event interrupt
  val LC44IP   = ValidIO(RO(44))
  val LC45IP   = ValidIO(RO(45))
  val LC46IP   = ValidIO(RO(46))
  val LC47IP   = ValidIO(RO(47))
  val LC48IP   = ValidIO(RO(48))
  val LC49IP   = ValidIO(RO(49))
  val LC50IP   = ValidIO(RO(50))
  val LC51IP   = ValidIO(RO(51))
  val LC52IP   = ValidIO(RO(52))
  val LC53IP   = ValidIO(RO(53))
  val LC54IP   = ValidIO(RO(54))
  val LC55IP   = ValidIO(RO(55))
  val LC56IP   = ValidIO(RO(56))
  val LC57IP   = ValidIO(RO(57))
  val LC58IP   = ValidIO(RO(58))
  val LC59IP   = ValidIO(RO(59))
  val LC60IP   = ValidIO(RO(60))
  val LC61IP   = ValidIO(RO(61))
  val LC62IP   = ValidIO(RO(62))
  val LC63IP   = ValidIO(RO(63))

  val getVS = Seq(VSSIP, VSTIP, VSEIP)

  def getHS = Seq(SSIP, STIP, SEIP)

  def getM = Seq(MSIP, MTIP, MEIP)

  def getNonLocal = Seq(
    SSIP, VSSIP, MSIP,
    STIP, VSTIP, MTIP,
    SEIP, VSEIP, MEIP,
    SGEIP
  )

  def getLocal = Seq(
    LCOFIP, LC14IP, LC15IP,
    LC16IP, LC17IP, LC18IP, LC19IP, LC20IP, LC21IP, LC22IP, LC23IP,
    LC24IP, LC25IP, LC26IP, LC27IP, LC28IP, LC29IP, LC30IP, LC31IP,
    LC32IP, LC33IP, LC34IP, LPRASEIP, LC36IP, LC37IP, LC38IP, LC39IP,
    LC40IP, LC41IP, LC42IP, HPRASEIP, LC44IP, LC45IP, LC46IP, LC47IP,
    LC48IP, LC49IP, LC50IP, LC51IP, LC52IP, LC53IP, LC54IP, LC55IP,
    LC56IP, LC57IP, LC58IP, LC59IP, LC60IP, LC61IP, LC62IP, LC63IP,
  )

  def getAll = getNonLocal ++ getLocal

  def getRW = getAll.filter(_.bits.isRW)

  def getNonRW = getAll.filterNot(_.bits.isRW)

  def getByNum(num: Int) = getAll.find(_.bits.lsb == num).get

  def connectZeroNonRW : this.type = {
    this.getNonRW.foreach(_.specifyField(
      _.valid := false.B,
      _.bits  := DontCare,
    ))
    this
  }
}
