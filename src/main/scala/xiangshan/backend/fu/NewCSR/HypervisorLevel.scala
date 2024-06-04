package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, _}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{SretEventSinkBundle, TrapEntryHSEventSinkBundle}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.ChiselRecordForField._

import scala.collection.immutable.SeqMap

trait HypervisorLevel { self: NewCSR =>

  val hstatus = Module(new HstatusModule)
    .setAddr(CSRs.hstatus)

  val hedeleg = Module(new CSRModule("Hedeleg", new HedelegBundle))
    .setAddr(CSRs.hedeleg)

  val hideleg = Module(new CSRModule("Hideleg", new HidelegBundle))
    .setAddr(CSRs.hideleg)

  val hie = Module(new CSRModule("Hie", new HieBundle)
    with HasIpIeBundle
    with HypervisorBundle
  {
    val toMie = IO(new HieToMie)

    val mieIsAlias = mideleg

    bundle.getFields.map(_.lsb).foreach { num =>
      val wtMie  = toMie.getByNum(num)
      wtMie.specifyField(
        _.valid := wen && mieIsAlias(num) && wtMie.bits.isRW.B,
        _.bits  := wen && mieIsAlias(num) && wtMie.bits.isRW.B &< wdata(num),
      )

      regOut(num) := mieIsAlias(num) && wtMie.bits.isRW.B &< mie(num)
    }
  })
    .setAddr(CSRs.hie)

  val htimedelta = Module(new CSRModule("Htimedelta", new CSRBundle {
    val VALUE = RW(63, 0)
  }))
    .setAddr(CSRs.htimedelta)

  val hcounteren = Module(new CSRModule("Hcounteren", new Counteren))
    .setAddr(CSRs.hcounteren)

  val hgeie = Module(new CSRModule("Hgeie", new HgeieBundle))
    .setAddr(CSRs.hgeie)

  val hvien = Module(new CSRModule("Hvien", new HvienBundle))
    .setAddr(CSRs.hvien)

  val hvictl = Module(new CSRModule("Hvictl", new HvictlBundle))
    .setAddr(CSRs.hvictl)

  val henvcfg = Module(new CSRModule("Henvcfg", new HEnvCfg) with HasHypervisorEnvBundle {
    when (!menvcfg.STCE.asBool && !privState.isModeM && accessStimecmp) {
      regOut.STCE := 0.U
    }
  })
    .setAddr(CSRs.henvcfg)

  val htval = Module(new CSRModule("Htval", new CSRBundle {
    val ALL = RW(63, 0)
  }) with TrapEntryHSEventSinkBundle)
    .setAddr(CSRs.htval)

  val hip = Module(new CSRModule("Hip", new HipBundle)
    with HypervisorBundle
    with HasExternalInterruptBundle
    with HasIpIeBundle
  {
    val toHvip = IO(new HipToHvip)

    // hip.VSEIP is read-only alias of mip.VSEIP, mip.VSEIP := hvip.VSEIP|hgeip(VGEIN)|platIR.VSEIP
    // hip.VSTIP is read-only alias of mip.VSTIP, mip.VSTIP := hvip.VSTIP|time+htimedelta>=vstimecmp
    // hip.SGEIP is read-only alias of mip.SGEIP, mip.SGEIP := |(hgeip&hgeie)
    regOut.VSTIP := mip.VSTIP
    regOut.VSEIP := mip.VSEIP
    regOut.SGEIP := mip.SGEIP

    // hip.VSSIP is alias of hvip.VSSIP, writable
    toHvip.VSSIP.valid := wen
    toHvip.VSSIP.bits  := wdata.VSSIP
    regOut.VSSIP := hvip.VSSIP
    // vsip.SSIP is alias of hip.VSSIP, so vsip.SSIP is alias of hvip.VSSIP.
    // vsip.SSIP write throuth to hvip.VSSIP
  })
    .setAddr(CSRs.hip)

  val hvip = Module(new CSRModule("Hvip", new HvipBundle) {
    val fromMip  = IO(Flipped(new MipToHvip))
    val fromHip  = IO(Flipped(new HipToHvip))
    val fromVSip = IO(Flipped(new VSipToHvip))

    when (fromMip.VSSIP.valid || fromHip.VSSIP.valid || fromVSip.VSSIP.valid) {
      reg.VSSIP := Mux1H(Seq(
        fromMip.VSSIP.valid -> fromMip.VSSIP.bits,
        fromHip.VSSIP.valid -> fromHip.VSSIP.bits,
        fromVSip.VSSIP.valid -> fromVSip.VSSIP.bits,
      ))
    }

    reg.getLocal zip fromVSip.getLocal foreach { case (rLCIP, vsipLCIP) =>
      // sip should assert valid when hideleg=0 && hvien=1
      when(vsipLCIP.valid) {
        rLCIP := vsipLCIP.bits
      }
    }
  })
    .setAddr(CSRs.hvip)

  val hviprio1 = Module(new CSRModule("Hviprio1", new Hviprio1Bundle))
    .setAddr(CSRs.hviprio1)

  val hviprio2 = Module(new CSRModule("Hviprio2", new Hviprio2Bundle))
    .setAddr(CSRs.hviprio2)

  val htinst = Module(new CSRModule("Htinst", new CSRBundle {
    val ALL = RO(63, 0)
  }) with TrapEntryHSEventSinkBundle)
    .setAddr(CSRs.htinst)

  val hgatp = Module(new CSRModule("Hgatp", new HgatpBundle) {
    // Ref: 13.2.10. Hypervisor Guest Address Translation and Protection Register (hgatp)
    // A write to hgatp with an unsupported MODE value is not ignored as it is for satp. Instead, the fields of
    // hgatp are WARL in the normal way, when so indicated.
    //
    // But we treat hgatp as the same of satp and vsatp.
    // If hgatp is written with an unsupported MODE,
    // the entire write has no effect; no fields in hgatp are modified.
    when(wen && wdata.MODE.isLegal) {
      when (wdata.MODE === HgatpMode.Bare) {
        reg := 0.U
      }.otherwise {
        reg := wdata
      }
    }.elsewhen (wen && !wdata.MODE.isLegal) {
      reg.PPN := wdata.PPN
      reg.VMID := wdata.VMID
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.hgatp)

  val hgeip = Module(new CSRModule("Hgeip", new HgeipBundle))
    .setAddr(CSRs.hgeip)

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

class HvipBundle extends InterruptPendingBundle {
  // VSSIP, VSTIP, VSEIP, localIP is writable
  this.getVS.foreach(_.setRW().withReset(0.U))
  this.getLocal.foreach(_.setRW().withReset(0.U))
}

class HieBundle extends InterruptEnableBundle {
  // All bits in hie are RO, since all registers implemented in mie.
}

class HipBundle extends InterruptPendingBundle {
  this.VSSIP.setRW().withReset(0.U) // aliasRW of mip.VSSIP when mideleg=1.
  this.VSTIP.setRO().withReset(0.U) // aliasRO of mip.VSTIP when mideleg=1. (hvip.VSTIP |　PLIC.VSTIP)
  this.VSEIP.setRO().withReset(0.U) // aliasRO of mip.VSEIP when mideleg=1. (hvip.VSEIP | hgeip(hstatus.VGEIN) | PLIC.VSEIP)
  this.SGEIP.setRO().withReset(0.U) // aliasRO of mip.SGEIP (|(hgeip & hegie))
}

class HvienBundle extends InterruptEnableBundle {
  // Ref: riscv interrupt spec - 6.3.2 Virtual interrupts for VS level
  // Bits 12:0 of hvien are reserved and must be read-only zeros.
  // For interrupt numbers 13–63, implementations may freely choose which bits of hvien are writable
  // and which bits are read-only zero or one.
  this.getLocal.foreach(_.setRW().withReset(0.U))

}

class HgeieBundle extends CSRBundle {
  val ie = RW(GEILEN, 1)
  // bit 0 is read only 0
}

class HgeipBundle extends CSRBundle {
  val ip = RO(GEILEN, 1)
  // bit 0 is read only 0
}

class HedelegBundle extends ExceptionBundle {
  this.getALL.foreach(_.setRW().withReset(0.U))
  // The default configs are RW
  this.EX_HSCALL.setRO().withReset(0.U)
  this.EX_VSCALL.setRO().withReset(0.U)
  this.EX_MCALL .setRO().withReset(0.U)
  this.EX_IGPF  .setRO().withReset(0.U)
  this.EX_LGPF  .setRO().withReset(0.U)
  this.EX_VI    .setRO().withReset(0.U)
  this.EX_SGPF  .setRO().withReset(0.U)
}

class HidelegBundle extends InterruptBundle {
  this.getALL.foreach(_.setRW().withReset(0.U))
  // default RW
  this.SSI .setRO().withReset(0.U)
  this.MSI .setRO().withReset(0.U)
  this.STI .setRO().withReset(0.U)
  this.MTI .setRO().withReset(0.U)
  this.SEI .setRO().withReset(0.U)
  this.MEI .setRO().withReset(0.U)
  this.SGEI.setRO().withReset(0.U)
}

class HipToHvip extends Bundle {
  val VSSIP = ValidIO(RW(0))
}

class SipToHvip extends ToAliasIpLocalPart {

}

class HieToMie extends IeValidBundle {
  this.getVS.foreach(_.bits.setRW())
  this.SGEIE.bits.setRW()
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
  val PrioSSI = RW(15,  8)
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

class HEnvCfg extends EnvCfg {
  if (CSRConfig.EXT_SSTC) {
    this.STCE.setRW().withReset(1.U)
  }
}

trait HypervisorBundle { self: CSRModule[_] =>
  val hstatus = IO(Input(new HstatusBundle))
}

trait HasHypervisorEnvBundle { self: CSRModule[_] =>
  val menvcfg = IO(Input(new MEnvCfg))
  val privState = IO(Input(new PrivState))
  val accessStimecmp = IO(Input(Bool()))
}