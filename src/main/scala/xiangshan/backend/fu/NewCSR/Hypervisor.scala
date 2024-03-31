package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  CSRWARLField => WARL,
  _
}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.CSRConfig._

import scala.collection.immutable.SeqMap

trait Hypervisor { self: NewCSR =>

  val hstatus = Module(new HstatusModule)

  val hedeleg = Module(new CSRModule("Hedeleg", new HedelegBundle) {})

  val hideleg = Module(new CSRModule("Hideleg", new HidelegBundle) {})

  val hie = Module(new CSRModule("Hie", new HieBundle) {})

  // 0x605
  val htimedelta = Module(new CSRModule("Htimedelta", new CSRBundle {
    val VALUE = RW(63, 0)
  }) {})

  // 0x606
  val hcounteren = Module(new CSRModule("Hcounteren", new CSRBundle {
    val CY = RW(0)
    val TM = RW(1)
    val IR = RW(2)
    val HPM = RW(31, 3)
  }) {})

  // 0x607
  val hgeie = Module(new CSRModule("Hgeie", new HgeieBundle) {})

  // 0x608
  val hvien = Module(new CSRModule("Hvien", new CSRBundle {
    val ien = RW(63, 13)
    // bits 12:0 read only 0
  }) {})

  // 0x609
  val hvictl = Module(new CSRModule("Hvictl", new CSRBundle {
    // Virtual Trap Interrupt control
    val VTI    = RW  (30)
    // WARL in AIA spec.
    // RW, since we support max width of IID
    val IID    = RW  (15 + HIIDWidth, 16)
    // determines the interrupt’s presumed default priority order relative to a (virtual) supervisor external interrupt (SEI), major identity 9
    // 0 = interrupt has higher default priority than an SEI
    // 1 = interrupt has lower default priority than an SEI
    // When hvictl.IID = 9, DPR is ignored.
    // Todo: sort the interrupt specified by hvictl with DPR
    val DPR    = RW  (9)
    val IPRIOM = RW  (8)
    val IPRIO  = RW  ( 7,  0)
  }) {})

  // 0x60A
  val henvcfg = Module(new CSRModule("Henvcfg", new CSRBundle {
    val FIOM  = RW(0)     // Fence of I/O implies Memory
    val CBIE  = RW(5, 4)  // Zicbom Enable
    val CBCFE = RW(6)     // Zicbom Enable
    val CBZE  = RW(7)     // Zicboz Enable
    val PBMTE = RW(62)    // Svpbmt Enable
    val STCE  = RW(63)    // Sstc Enable
  }) {})

  // 0x643
  val htval = Module(new CSRModule("Htval", new CSRBundle {
    val ALL = RW(63, 0)
  }) {})

  // 0x644
  val hip = Module(new CSRModule("Hip", new HipBundle) with HypervisorBundle {
    rdata.VSSIP := hvip.VSSIP
    rdata.VSTIP := hvip.VSTIP.asUInt.asBool | vsi.tip
    rdata.VSEIP := hvip.VSEIP.asUInt.asBool | vsi.eip | hgeip.ip.asUInt(hstatus.VGEIN.asUInt)
    rdata.SGEIP := (hgeip.ip.asUInt | hgeie.ie.asUInt).orR
  })

  // 0x645
  val hvip = Module(new CSRModule("Hvip", new CSRBundle {
    val VSSIP = RW( 2)
    val VSTIP = RW( 6)
    val VSEIP = RW(10)
  }) {})

  // 0x646
  val hviprio1 = Module(new CSRModule("Hviprio1", new CSRBundle {
    val PrioSSI = RW(15,  8)
    val PrioSTI = RW(31, 24)
    val PrioCOI = RW(47, 40)
    val Prio14  = RW(55, 48)
    val Prio15  = RW(63, 56)
  }) {})

  // 0x647
  val hviprio2 = Module(new CSRModule("Hviprio2", new CSRBundle {
    val Prio16  = RW( 7,  0)
    val Prio17  = RW(15,  8)
    val Prio18  = RW(23, 16)
    val Prio19  = RW(31, 24)
    val Prio20  = RW(39, 32)
    val Prio21  = RW(47, 40)
    val Prio22  = RW(55, 48)
    val Prio23  = RW(63, 56)
  }) {})

  // 0x64A
  val htinst = Module(new CSRModule("Htinst", new CSRBundle {
    val ALL = RO(63, 0)
  }) {})

  // 0x680
  val hgatp = Module(new CSRModule("Hgatp", new CSRBundle {
    val MODE = HgatpMode(63, 60, wNoFilter)
    // WARL in privileged spec.
    // RW, since we support max width of VMID
    val VMID = RW(44 - 1 + VMIDLEN, 44)
    val PPN = RW(43, 0)
  }) {})

  // 0xE12
  val hgeip = Module(new CSRModule("Hgeip", new HgeipBundle) {})

  hip.hvip := hvip.rdata

  val hypervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x600 -> (hstatus.w -> hstatus.rdata),
    0x602 -> (hedeleg.w -> hedeleg.rdata),
    0x603 -> (hideleg.w -> hideleg.rdata),
    0x604 -> (hie.w -> hie.rdata),
    0x605 -> (htimedelta.w, htimedelta.rdata),
    0x606 -> (hcounteren.w, hcounteren.rdata),
    0x60A -> (henvcfg.w -> henvcfg.rdata),
    0x644 -> (hip.w -> hip.rdata),
    0x645 -> (hvip.w -> hvip.rdata),
  )

  val hypervisorCSRMods: Seq[CSRModule[_]] = Seq(
    hstatus,
    hedeleg,
    hideleg,
    hie,
    htimedelta,
    hcounteren,
    henvcfg,
    hip,
    hvip,
  )
}

class HstatusBundle extends CSRBundle {

  val VSBE  = RO(5).withReset(0.U)
  val GVA   = RW(6)
  val SPV   = RW(7)
  val SPVP  = RW(8)
  val HU    = RW(9)
  val VGEIN = HstatusVgeinField(17, 12, wNoFilter, rNoFilter)
  val VTVM  = RW(20)
  val VTM   = RW(21)
  val VTSR  = RW(22)
  val VSXL  = XLENField(33, 32).withReset(XLENField.XLEN64)

}

object HstatusVgeinField extends CSREnum with CSRWLRLApply {
  override def isLegal(enum: CSREnumType): Bool = enum.asUInt <= GEILEN.U
}

class HstatusModule extends CSRModule("Hstatus", new HstatusBundle)

class HvipBundle extends CSRBundle {
  val VSSIP = RW(2)
  val VSTIP = RW(6)
  val VSEIP = RW(10)
}

class HieBundle extends CSRBundle {
  val VSSIE = RW( 2)
  val VSTIE = RW( 6)
  val VSEIE = RW(10)
  val SGEIE = RW(12)
}

class HipBundle extends CSRBundle {
  val VSSIP = RW( 2) // alias of hvip.VSSIP
  val VSTIP = RO( 6) // hvip.VSTIP |　PLIC.VSTIP
  val VSEIP = RO(10) // hvip.VSEIP | hgeip(hstatus.VGEIN) | PLIC.VSEIP
  val SGEIP = RO(12) // |(hgeip & hegie)
}

class HgeieBundle extends CSRBundle {
  val ie = RW(GEILEN, 1)
  // bit 0 is read only 0
}

class HgeipBundle extends CSRBundle {
  val ip = RW(GEILEN, 1)
  // bit 0 is read only 0
}

class HedelegBundle extends CSRBundle {
  val EX_IAM    = RW(0)
  val EX_IAF    = RW(1)
  val EX_II     = RW(2)
  val EX_BP     = RW(3)
  val EX_LAM    = RW(4)
  val EX_LAF    = RW(5)
  val EX_SAM    = RW(6)
  val EX_SAF    = RW(7)
  val EX_UCALL  = RW(8)
  val EX_HSCALL = RO(9)
  val EX_VSCALL = RO(10)
  val EX_MCALL  = RO(11)
  val EX_IPF    = RW(12)
  val EX_LPF    = RW(13)
  val EX_SPF    = RW(15)
  val EX_IGPF   = RO(20)
  val EX_LGPF   = RO(21)
  val EX_VI     = RO(22)
  val EX_SGPF   = RO(23)
}

class HidelegBundle extends CSRBundle {
  // Software Interrupt
  val IR_SSI    = RO(1)
  val IR_VSSI   = RW(2)
  val IR_MSI    = RO(3)
  // Time Interrupt
  val IR_STI    = RO(5)
  val IR_VSTI   = RW(6)
  val IR_MTI    = RO(7)
  // External Interrupt
  val IR_SEI    = RO(9)
  val IR_VSEI   = RW(10)
  val IR_MEI    = RO(11)
  val IR_SGEI   = RO(12)
  // SoC
  val IR_COI    = RW(13) // Counter overflow interrupt
  val IR_LPRASE = RW(35) // Low-priority RAS event interrupt
  val IR_HPRASE = RW(43) // High-priority RAS event interrupt
}

trait HypervisorBundle { self: CSRModule[_] =>
  val hstatus = IO(Input(new HstatusBundle))
  val hvip    = IO(Input(new HvipBundle))
  val hideleg = IO(Input(new HidelegBundle))
  val hedeleg = IO(Input(new HedelegBundle))
  val hgeip   = IO(Input(new HgeipBundle))
  val hgeie   = IO(Input(new HgeieBundle))
}