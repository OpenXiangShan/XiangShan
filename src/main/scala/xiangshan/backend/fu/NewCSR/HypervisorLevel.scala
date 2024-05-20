package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL, CSRWLRLField => WLRL, _}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSREvents.{SretEventSinkBundle, TrapEntryHSEventSinkBundle}

import scala.collection.immutable.SeqMap
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._

trait HypervisorLevel { self: NewCSR =>

  val hstatus = Module(new HstatusModule)
    .setAddr(0x600)

  val hedeleg = Module(new CSRModule("Hedeleg", new HedelegBundle))
    .setAddr(0x602)

  val hideleg = Module(new CSRModule("Hideleg", new HidelegBundle))
    .setAddr(0x603)

  val hie = Module(new CSRModule("Hie", new HieBundle) with HypervisorBundle {
    val fromVSie = IO(Flipped(new VSieToHie))
    val fromMie = IO(Flipped(new MieToHie))

    when (fromVSie.SSIE.valid) { reg.VSSIE := fromVSie.SSIE.bits }
    when (fromVSie.STIE.valid) { reg.VSTIE := fromVSie.STIE.bits }
    when (fromVSie.SEIE.valid) { reg.VSEIE := fromVSie.SEIE.bits }
    when (fromMie.VSSIE.valid) { reg.VSSIE := fromMie.VSSIE.bits }
    when (fromMie.VSTIE.valid) { reg.VSTIE := fromMie.VSTIE.bits }
    when (fromMie.VSEIE.valid) { reg.VSEIE := fromMie.VSEIE.bits }
    when (fromMie.SGEIE.valid) { reg.SGEIE := fromMie.SGEIE.bits }
  })
    .setAddr(0x604)

  hie.fromMie := mie.toHie

  val htimedelta = Module(new CSRModule("Htimedelta", new CSRBundle {
    val VALUE = RW(63, 0)
  }))
    .setAddr(0x605)

  val hcounteren = Module(new CSRModule("Hcounteren", new Counteren))
    .setAddr(0x606)

  val hgeie = Module(new CSRModule("Hgeie", new HgeieBundle))
    .setAddr(0x607)

  val hvien = Module(new CSRModule("Hvien", new CSRBundle {
    val ien = RW(63, 13)
    // bits 12:0 read only 0
  }))
    .setAddr(0x608)

  val hvictl = Module(new CSRModule("Hvictl", new HvictlBundle))
    .setAddr(0x609)

  val henvcfg = Module(new CSRModule("Henvcfg", new CSRBundle {
    val FIOM  = RW(0)     // Fence of I/O implies Memory
    val CBIE  = RW(5, 4)  // Zicbom Enable
    val CBCFE = RW(6)     // Zicbom Enable
    val CBZE  = RW(7)     // Zicboz Enable
    val PBMTE = RW(62)    // Svpbmt Enable
    val STCE  = RW(63)    // Sstc Enable
  }))
    .setAddr(0x60A)

  val htval = Module(new CSRModule("Htval", new CSRBundle {
    val ALL = RW(63, 0)
  }) with TrapEntryHSEventSinkBundle)
    .setAddr(0x643)

  val hip = Module(new CSRModule("Hip", new HipBundle) with HypervisorBundle with HasExternalInterruptBundle {
    val fromVSip = IO(Flipped(new VSipToHip))
    val toHvip = IO(new HipToHvip)

    rdataFields.VSSIP := hvip.VSSIP
    rdataFields.VSTIP := hvip.VSTIP.asUInt.asBool | platformIRP.VSTIP
    rdataFields.VSEIP := hvip.VSEIP.asUInt.asBool | platformIRP.VSEIP | hgeip.ip.asUInt(hstatus.VGEIN.asUInt)
    rdataFields.SGEIP := (hgeip.ip.asUInt | hgeie.ie.asUInt).orR

    // hip.VSEIP is read only
    // hip.VSTIP is read only
    // hip.VSSIP is alias of hvip.VSSIP
    // vsip.SSIP is alias of hip.VSSIP
    toHvip.VSSIP.valid := fromVSip.SSIP.valid || wen
    toHvip.VSSIP.bits := Mux1H(Seq(
      fromVSip.SSIP.valid -> fromVSip.SSIP.bits,
      wen                 -> wdata.VSSIP
    ))
  })
    .setAddr(0x644)

  val hvip = Module(new CSRModule("Hvip", new CSRBundle {
    val VSSIP = RW( 2)
    val VSTIP = RW( 6)
    val VSEIP = RW(10)
  }) {
    val fromHip = IO(Flipped(new HipToHvip))
    when (fromHip.VSSIP.valid) { reg.VSSIP := fromHip.VSSIP.bits }
  })
    .setAddr(0x645)

  hvip.fromHip := hip.toHvip

  val hviprio1 = Module(new CSRModule("Hviprio1", new Hviprio1Bundle))
    .setAddr(0x646)

  val hviprio2 = Module(new CSRModule("Hviprio2", new Hviprio2Bundle))
    .setAddr(0x647)

  val htinst = Module(new CSRModule("Htinst", new CSRBundle {
    val ALL = RO(63, 0)
  }) with TrapEntryHSEventSinkBundle)
    .setAddr(0x64A)

  val hgatp = Module(new CSRModule("Hgatp", new HgatpBundle) {
    // Ref: 13.2.10. Hypervisor Guest Address Translation and Protection Register (hgatp)
    // A write to hgatp with an unsupported MODE value is not ignored as it is for satp. Instead, the fields of
    // hgatp are WARL in the normal way, when so indicated.
    //
    // But we treat hgatp as the same of satp and vsatp.
    // If hgatp is written with an unsupported MODE,
    // the entire write has no effect; no fields in hgatp are modified.
    when(wen && wdata.MODE.isLegal) {
      reg := wdata
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(0x680)

  val hgeip = Module(new CSRModule("Hgeip", new HgeipBundle))
    .setAddr(0xE12)

  val hypervisorCSRMods: Seq[CSRModule[_]] = Seq(
    hstatus,
    hedeleg,
    hideleg,
    hie,
    htimedelta,
    hcounteren,
    hgeie,
    hvien,
    hvictl,
    henvcfg,
    htval,
    hip,
    hvip,
    hviprio1,
    hviprio2,
    htinst,
    hgatp,
    hgeip,
  )

  val hypervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap.from(
    hypervisorCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val hypervisorCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    hypervisorCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )
}

class HstatusBundle extends CSRBundle {

  val VSBE  = RO(5).withReset(0.U)
  val GVA   = RW(6)
  val SPV   = VirtMode(7)
  val SPVP  = RW(8)
  val HU    = RW(9)
  val VGEIN = HstatusVgeinField(17, 12, wNoFilter, rNoFilter)
  val VTVM  = RW(20)
  val VTW   = RW(21)
  val VTSR  = RW(22)
  val VSXL  = XLENField(33, 32).withReset(XLENField.XLEN64)

}

object HstatusVgeinField extends CSREnum with WLRLApply {
  override def isLegal(enum: CSREnumType): Bool = enum.asUInt <= GEILEN.U
}

class HstatusModule(implicit p: Parameters) extends CSRModule("Hstatus", new HstatusBundle)
  with SretEventSinkBundle
  with TrapEntryHSEventSinkBundle

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

class HedelegBundle extends ExceptionBundle {
  // The default configs are RW
  this.EX_HSCALL.setRO()
  this.EX_VSCALL.setRO()
  this.EX_MCALL .setRO()
  this.EX_IGPF  .setRO()
  this.EX_LGPF  .setRO()
  this.EX_VI    .setRO()
  this.EX_SGPF  .setRO()
}

class HidelegBundle extends InterruptBundle {
  // default RW
  this.SSI .setRO()
  this.MSI .setRO()
  this.STI .setRO()
  this.MTI .setRO()
  this.SEI .setRO()
  this.MEI .setRO()
  this.SGEI.setRO()
}

class HipToHvip extends Bundle {
  val VSSIP = ValidIO(RW(0))
}

class HvictlBundle extends CSRBundle {
  // Virtual Trap Interrupt control
  val VTI = RW(30)
  // WARL in AIA spec.
  // RW, since we support max width of IID
  val IID = RW(15 + HIIDWidth, 16)
  // determines the interrupt’s presumed default priority order relative to a (virtual) supervisor external interrupt (SEI), major identity 9
  // 0 = interrupt has higher default priority than an SEI
  // 1 = interrupt has lower default priority than an SEI
  // When hvictl.IID = 9, DPR is ignored.
  // Todo: sort the interrupt specified by hvictl with DPR
  val DPR = RW(9)
  val IPRIOM = RW(8)
  val IPRIO = RW(7, 0)
}

class Hviprio1Bundle extends CSRBundle {
  val PrioSSI = RW(15, 8)
  val PrioSTI = RW(31, 24)
  val PrioCOI = RW(47, 40)
  val Prio14  = RW(55, 48)
  val Prio15  = RW(63, 56)
}

class Hviprio2Bundle extends CSRBundle {
  val Prio16 = RW(7, 0)
  val Prio17 = RW(15, 8)
  val Prio18 = RW(23, 16)
  val Prio19 = RW(31, 24)
  val Prio20 = RW(39, 32)
  val Prio21 = RW(47, 40)
  val Prio22 = RW(55, 48)
  val Prio23 = RW(63, 56)
}

class HgatpBundle extends CSRBundle {
  final val PPN_msb = PAddrWidth - AddrWidthInPage - 1
  val MODE = HgatpMode(63, 60, wNoFilter).withReset(HgatpMode.Bare)
  // WARL in privileged spec.
  // RW, since we support max width of VMID
  val VMID = RW(44 - 1 + VMIDLEN, 44)
  val PPN = RW(PAddrWidth, 0)
}

trait HypervisorBundle { self: CSRModule[_] =>
  val hstatus = IO(Input(new HstatusBundle))
  val hvip    = IO(Input(new HvipBundle))
  val hideleg = IO(Input(new HidelegBundle))
  val hedeleg = IO(Input(new HedelegBundle))
  val hgeip   = IO(Input(new HgeipBundle))
  val hgeie   = IO(Input(new HgeieBundle))
  val hip     = IO(Input(new HipBundle))
  val hie     = IO(Input(new HieBundle))
}
