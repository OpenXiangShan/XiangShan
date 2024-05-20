package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.{Cat, ValidIO}
import utility.SignExt
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL, CSRWLRLField => WLRL, _}
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSREvents.TrapEntryHSEventSinkBundle

import scala.collection.immutable.SeqMap

trait SupervisorLevel { self: NewCSR with MachineLevel =>
  val sie = Module(new CSRModule("Sie", new SieBundle) with HasMachineInterruptBundle with HasMachineDelegBundle{
    val toMie = IO(new SieToMie)
    // Ref: 7.1.3. Supervisor Interrupt Registers (sip and sie)
    // The sip and sie registers are subsets of the mip and mie registers. Reading any
    // implemented field, or writing any writable field, of sip/sie effects a read or write of the
    // homonymous field of mip/mie.
    // Ref: 3.1.9. Machine Interrupt Registers (mip and mie)
    // Restricted views of the mip and mie registers appear as the sip and sie registers for supervisor level. If
    // an interrupt is delegated to S-mode by setting a bit in the mideleg register, it becomes visible in the
    // sip register and is maskable using the sie register. Otherwise, the corresponding bits in sip and sie
    // are **read-only zero**.
    rdataFields.SSIE := Mux(mideleg.SSI.asBool, mie.SSIE.asUInt, 0.U)
    rdataFields.STIE := Mux(mideleg.STI.asBool, mie.STIE.asUInt, 0.U)
    rdataFields.SEIE := Mux(mideleg.SEI.asBool, mie.SEIE.asUInt, 0.U)

    // Sie is alias of mie.
    // There are no regs in CSR sie.
    regOut := mie.asUInt

    toMie.SSIE.valid := wen && mideleg.SSI.asBool
    toMie.STIE.valid := wen && mideleg.STI.asBool
    toMie.SEIE.valid := wen && mideleg.SEI.asBool
    toMie.SSIE.bits := wdata.SSIE
    toMie.STIE.bits := wdata.STIE
    toMie.SEIE.bits := wdata.SEIE
  })
    .setAddr(0x104)

  val stvec = Module(new CSRModule("Stvec", new XtvecBundle))
    .setAddr(0x105)

  val scounteren = Module(new CSRModule("Scounteren", new Counteren))
    .setAddr(0x106)

  val senvcfg = Module(new CSRModule("Senvcfg", new Envcfg))
    .setAddr(0x10A)

  val sscratch = Module(new CSRModule("Sscratch"))
    .setAddr(0x140)

  val sepc = Module(new CSRModule("Sepc", new Epc) with TrapEntryHSEventSinkBundle {
    rdata := SignExt(Cat(reg.epc.asUInt, 0.U(1.W)), XLEN)
  })
    .setAddr(0x141)

  val scause = Module(new CSRModule("Scause", new CauseBundle) with TrapEntryHSEventSinkBundle)
    .setAddr(0x142)

  val stval = Module(new CSRModule("Stval") with TrapEntryHSEventSinkBundle)
    .setAddr(0x143)

  val sip = Module(new CSRModule("Sip", new SipBundle) with HasMachineInterruptBundle with HasMachineDelegBundle {
    val toMip = IO(new SipToMip)

    // Ref: 7.1.3. Supervisor Interrupt Registers (sip and sie)
    // The sip and sie registers are subsets of the mip and mie registers. Reading any
    // implemented field, or writing any writable field, of sip/sie effects a read or write of the
    // homonymous field of mip/mie.
    // Ref: 3.1.9. Machine Interrupt Registers (mip and mie)
    // Restricted views of the mip and mie registers appear as the sip and sie registers for supervisor level. If
    // an interrupt is delegated to S-mode by setting a bit in the mideleg register, it becomes visible in the
    // sip register and is maskable using the sie register. Otherwise, the corresponding bits in sip and sie
    // are **read-only zero**.

    rdataFields.SSIP := Mux(mideleg.SSI.asUInt.asBool, mip.SSIP.asUInt, 0.U)
    rdataFields.STIP := Mux(mideleg.STI.asUInt.asBool, mip.STIP.asUInt, 0.U)
    rdataFields.SEIP := Mux(mideleg.SEI.asUInt.asBool, mip.SEIP.asUInt, 0.U)

    toMip.SSIP.valid := wen && mideleg.SSI.asBool
    toMip.SSIP.bits := wdata.SSIP
  })
    .setAddr(0x144)

  val stimecmp = Module(new CSRModule("Stimecmp"))
    .setAddr(0x14D)

  val satp = Module(new CSRModule("Satp", new SatpBundle) {
    // If satp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when (wen && wdata.MODE.isLegal) {
      reg := wdata
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(0x180)

  val supervisorLevelCSRMods: Seq[CSRModule[_]] = Seq(
    sie,
    stvec,
    scounteren,
    senvcfg,
    sscratch,
    sepc,
    scause,
    stval,
    sip,
    stimecmp,
    satp,
  )

  val supervisorLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x100 -> (mstatus.wAliasSstatus, mstatus.sstatus),
  ) ++ SeqMap.from(
    supervisorLevelCSRMods.map(csr => (csr.addr -> (csr.w, csr.rdata))).iterator
  )

  val supervisorLevelCSROutMap: SeqMap[Int, UInt] = SeqMap(
    0x100 -> mstatus.sstatus.asUInt,
  ) ++ SeqMap.from(
    supervisorLevelCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )
}

class SstatusBundle extends CSRBundle {
  val SIE  = CSRWARLField   (1, wNoFilter)
  val SPIE = CSRWARLField   (5, wNoFilter)
  val UBE  = CSRROField     (6)
  val SPP  = CSRWARLField   (8, wNoFilter)
  val VS   = ContextStatus  (10, 9)
  val FS   = ContextStatus  (14, 13)
  val XS   = ContextStatusRO(16, 15).withReset(0.U)
  val SUM  = CSRWARLField   (18, wNoFilter)
  val MXR  = CSRWARLField   (19, wNoFilter)
  val UXL  = XLENField      (33, 32).withReset(XLENField.XLEN64)
  val SD   = CSRROField     (63, (_, _) => FS === ContextStatus.Dirty || VS === ContextStatus.Dirty)
}

class SieBundle extends InterruptEnableBundle {
  this.getALL.foreach(_.setRO())
  this.SSIE.setRW()
  this.STIE.setRW()
  this.SEIE.setRW()
  // Todo: LCOFIE
}

class SipBundle extends InterruptPendingBundle {
  this.getALL.foreach(_.setRO())
  // If implemented, SEIP is read-only in sip, and is set and cleared by the execution environment, typically through a platform-specific interrupt controller
  // If implemented, STIP is read-only in sip, and is set and cleared by the execution environment.
  // If implemented, SSIP is writable in sip and may also be set to 1 by a platform-specific interrupt controller.
  this.SSIP.setRW()
  // Todo: LCOFIE
}

class SatpBundle extends CSRBundle {
  final val PPN_msb = PAddrWidth - AddrWidthInPage - 1
  val MODE = SatpMode(63, 60, null).withReset(SatpMode.Bare)
  // WARL in privileged spec.
  // RW, since we support max width of ASID
  val ASID = RW(44 - 1 + ASIDLEN, 44)
  val PPN  = RW(PPN_msb, 0)
}

class SieToMie extends Bundle {
  val SSIE = ValidIO(RW(0))
  val STIE = ValidIO(RW(0))
  val SEIE = ValidIO(RW(0))
}

class SipToMip extends Bundle {
  val SSIP = ValidIO(RW(0))
}
