package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, _}

import scala.collection.immutable.SeqMap

trait CSRAIA { self: NewCSR with HypervisorLevel =>
  val miselect = Module(new CSRModule("Miselect", new MISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := value >= 0x70.U && value < 0x100.U
    isIllegal :=
      value < 0x30.U ||
      value >= 0x30.U && value < 0x40.U && value(0) === 1.U ||
      value >= 0x40.U && value < 0x70.U ||
      value >= 0x100.U
  })
    .setAddr(CSRs.miselect)

  val mireg = Module(new CSRModule("Mireg") with HasIregSink {
    rdata := iregRead.mireg
  })
    .setAddr(CSRs.mireg)

  val mtopei = Module(new CSRModule("Mtopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.mtopei
  })
    .setAddr(CSRs.mtopei)

  val mtopi = Module(new CSRModule("Mtopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.mtopi.IID
    regOut.IPRIO := topIR.mtopi.IPRIO
  })
    .setAddr(CSRs.mtopi)

  val siselect = Module(new CSRModule("Siselect", new SISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := value >= 0x70.U && value < 0x100.U
    isIllegal :=
      value < 0x30.U ||
      value >= 0x30.U && value < 0x40.U && value(0) === 1.U ||
      value >= 0x40.U && value < 0x70.U ||
      value >= 0x100.U
  })
    .setAddr(CSRs.siselect)

  val sireg = Module(new CSRModule("Sireg") with HasIregSink {
    rdata := iregRead.sireg
  })
    .setAddr(CSRs.sireg)

  val stopei = Module(new CSRModule("Stopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.stopei
  })
    .setAddr(CSRs.stopei)

  val stopi = Module(new CSRModule("Stopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.stopi.IID
    regOut.IPRIO := topIR.stopi.IPRIO
  })
    .setAddr(CSRs.stopi)

  val vsiselect = Module(new CSRModule("VSiselect", new VSISelectBundle) with HasISelectBundle {
    private val value = reg.ALL.asUInt
    inIMSICRange := value >= 0x70.U && value < 0x100.U
    isIllegal :=
      value < 0x70.U ||
      value >= 0x100.U
  })
    .setAddr(CSRs.vsiselect)

  val vsireg    = Module(new CSRModule("VSireg") with HasIregSink {
    rdata := iregRead.sireg
  })
    .setAddr(CSRs.vsireg)

  val vstopei   = Module(new CSRModule("VStopei", new TopEIBundle) with HasAIABundle {
    regOut := aiaToCSR.vstopei
  })
    .setAddr(CSRs.vstopei)

  val vstopi = Module(new CSRModule("VStopi", new TopIBundle) with HasInterruptFilterSink {
    regOut.IID   := topIR.vstopi.IID
    regOut.IPRIO := topIR.vstopi.IPRIO
  })
    .setAddr(CSRs.vstopi)

  val miprio0 = Module(new CSRModule(s"Iprio0", new Iprio0Bundle))
    .setAddr(0x30)

  val miprio2 = Module(new CSRModule(s"Iprio2", new Iprio2Bundle))
    .setAddr(0x32)

  val miprio4 = Module(new CSRModule(s"Iprio4", new IprioBundle))
    .setAddr(0x34)

  val miprio6 = Module(new CSRModule(s"Iprio6", new IprioBundle))
    .setAddr(0x36)

  val miprio8 = Module(new CSRModule(s"Iprio8", new Iprio8Bundle))
    .setAddr(0x38)

  val miprio10 = Module(new CSRModule(s"Iprio10", new Iprio10Bundle))
    .setAddr(0x3A)

  val miprio12 = Module(new CSRModule(s"Iprio12", new IprioBundle))
    .setAddr(0x3C)

  val miprio14 = Module(new CSRModule(s"Iprio14", new IprioBundle))
    .setAddr(0x3E)

  val siprio0 = Module(new CSRModule(s"Iprio0", new Iprio0Bundle))
    .setAddr(0x30)

  val siprio2 = Module(new CSRModule(s"Iprio2", new Iprio2Bundle))
    .setAddr(0x32)

  val siprio4 = Module(new CSRModule(s"Iprio4", new IprioBundle))
    .setAddr(0x34)

  val siprio6 = Module(new CSRModule(s"Iprio6", new IprioBundle))
    .setAddr(0x36)

  val siprio8 = Module(new CSRModule(s"Iprio8", new Iprio8Bundle))
    .setAddr(0x38)

  val siprio10 = Module(new CSRModule(s"Iprio10", new Iprio10Bundle))
    .setAddr(0x3A)

  val siprio12 = Module(new CSRModule(s"Iprio12", new IprioBundle))
    .setAddr(0x3C)

  val siprio14 = Module(new CSRModule(s"Iprio14", new IprioBundle))
    .setAddr(0x3E)

  val miregiprios: Seq[CSRModule[_]] = Seq(miprio0, miprio2, miprio4, miprio6, miprio8, miprio10, miprio12, miprio14)

  val siregiprios: Seq[CSRModule[_]] = Seq(siprio0, siprio2, siprio4, siprio6, siprio8, siprio10, siprio12, siprio14)

  val aiaCSRMods = Seq(
    miselect,
    mireg,
    mtopei,
    mtopi,
    siselect,
    sireg,
    stopei,
    stopi,
    vsiselect,
    vsireg,
    vstopi,
    vstopei,
  )

  val aiaSkipCSRs = Seq(
    mtopei,
    mtopi,
    stopei,
    stopi,
    vstopi,
    vstopei,
  )

  val aiaCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val aiaCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    aiaCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )

  private val miregRData: UInt = Mux1H(
    miregiprios.map(prio => (miselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  private val siregRData: UInt = Mux1H(
    siregiprios.map(prio => (siselect.rdata.asUInt === prio.addr.U) -> prio.rdata)
  )

  aiaCSRMods.foreach { mod =>
    mod match {
      case m: HasIregSink =>
        m.iregRead.mireg := miregRData
        m.iregRead.sireg := siregRData
        m.iregRead.vsireg := 0.U // Todo: IMSIC
      case _ =>
    }
  }
}

class ISelectField(final val maxValue: Int, reserved: Seq[Range]) extends CSREnum with WARLApply {
  override def isLegal(enumeration: CSREnumType): Bool = enumeration.asUInt <= maxValue.U
}

object VSISelectField extends ISelectField(
  0x1FF,
  reserved = Seq(
    Range.inclusive(0x000, 0x02F),
    Range.inclusive(0x040, 0x06F),
    Range.inclusive(0x100, 0x1FF),
  ),
)

object MISelectField extends ISelectField(
  maxValue = 0xFF,
  reserved = Seq(
    Range.inclusive(0x00, 0x2F),
    Range.inclusive(0x40, 0x6F),
  ),
)

object SISelectField extends ISelectField(
  maxValue = 0xFF,
  reserved = Seq(
    Range.inclusive(0x00, 0x2F),
    Range.inclusive(0x40, 0x6F),
  ),
)

class VSISelectBundle extends CSRBundle {
  val ALL = VSISelectField(log2Up(0x1FF), 0, null).withReset(0.U)
}

class MISelectBundle extends CSRBundle {
  val ALL = MISelectField(log2Up(0xFF), 0, null).withReset(0.U)
}

class SISelectBundle extends CSRBundle {
  val ALL = SISelectField(log2Up(0xFF), 0, null).withReset(0.U)
}

class TopIBundle extends CSRBundle {
  val IID   = RO(27, 16)
  val IPRIO = RO(7, 0)
}

class TopEIBundle extends CSRBundle {
  val IID   = RW(26, 16)
  val IPRIO = RW(10, 0)
}

class IprioBundle extends CSRBundle {
  val ALL = RO(63, 0).withReset(0.U)
}

class Iprio0Bundle extends CSRBundle {
  val PrioSSI  = RW(15,  8).withReset(0.U)
  val PrioVSSI = RW(23, 16).withReset(0.U)
  val PrioMSI  = RW(31, 24).withReset(0.U)
  val PrioSTI  = RW(47, 40).withReset(0.U)
  val PrioVSTI = RW(55, 48).withReset(0.U)
  val PrioMTI  = RW(63, 56).withReset(0.U)
}

class Iprio2Bundle extends CSRBundle {
  val PrioSEI  = RW(15,  8).withReset(0.U)
  val PrioVSEI = RW(23, 16).withReset(0.U)
  val PrioMEI  = RW(31, 24).withReset(0.U)
  val PrioSGEI = RW(39, 32).withReset(0.U)
  val PrioCOI  = RW(47, 40).withReset(0.U)
}

class Iprio8Bundle extends CSRBundle {
  val PrioLPRASEI = RW(31, 24).withReset(0.U)
}

class Iprio10Bundle extends CSRBundle {
  val PrioHPRASEI = RW(31, 24).withReset(0.U)
}

class CSRToAIABundle extends Bundle {
  private final val AddrWidth = 12

  val addr = ValidIO(new Bundle {
    val addr = UInt(AddrWidth.W)
    val v = VirtMode()
    val prvm = PrivMode()
  })

  val vgein = UInt(VGEINWidth.W)

  val wdata = ValidIO(new Bundle {
    val op = UInt(2.W)
    val data = UInt(XLEN.W)
  })

  val mClaim = Bool()
  val sClaim = Bool()
  val vsClaim = Bool()
}

class AIAToCSRBundle extends Bundle {
  private val NumVSIRFiles = 63
  val rdata = ValidIO(new Bundle {
    val data = UInt(XLEN.W)
    val illegal = Bool()
  })
  val meip    = Bool()
  val seip    = Bool()
  val vseip   = UInt(NumVSIRFiles.W)
  val mtopei  = new TopEIBundle
  val stopei  = new TopEIBundle
  val vstopei = new TopEIBundle
}

trait HasAIABundle { self: CSRModule[_] =>
  val aiaToCSR = IO(Input(new AIAToCSRBundle))
}

trait HasInterruptFilterSink { self: CSRModule[_] =>
  val topIR = IO(new Bundle {
    val mtopi  = Input(new TopIBundle)
    val stopi  = Input(new TopIBundle)
    val vstopi = Input(new TopIBundle)
  })
}

trait HasISelectBundle { self: CSRModule[_] =>
  val inIMSICRange = IO(Output(Bool()))
  val isIllegal = IO(Output(Bool()))
}

trait HasIregSink { self: CSRModule[_] =>
  val iregRead = IO(Input(new Bundle {
    val mireg = UInt(XLEN.W) // Todo: check if use ireg bundle, and shrink the width
    val sireg = UInt(XLEN.W)
    val vsireg = UInt(XLEN.W)
  }))
}
