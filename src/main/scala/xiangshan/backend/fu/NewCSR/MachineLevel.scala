package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  CSRWARLField => WARL,
  _
}

import scala.collection.immutable.SeqMap

trait MachineLevel { self: NewCSR =>
  val mstatus = Module(new MstatusModule)
    .setAddr(0x300)

  val misa = Module(new CSRModule("Misa", new MisaBundle))
    .setAddr(0x301)

  val medeleg = Module(new CSRModule("Medeleg", new MedelegBundle))
    .setAddr(0x302)

  val mideleg = Module(new CSRModule("Mideleg", new MidelegBundle))
    .setAddr(0x303)

  val mie = Module(new CSRModule("Mie", new MieBundle) with HypervisorBundle {
    val toHie = IO(new MieToHie)

    toHie.VSSIE.valid := wen
    toHie.VSTIE.valid := wen
    toHie.VSEIE.valid := wen
    toHie.SGEIE.valid := wen
    toHie.VSSIE.bits := wdata.VSSIE
    toHie.VSTIE.bits := wdata.VSTIE
    toHie.VSEIE.bits := wdata.VSEIE
    toHie.SGEIE.bits := wdata.SGEIE

    rdata.VSSIE := hie.VSSIE
    rdata.VSTIE := hie.VSTIE
    rdata.VSEIE := hie.VSEIE
    rdata.SGEIE := hie.SGEIE
  }).setAddr(0x304)

  val mtvec = Module(new CSRModule("Mtvec", new XtvecBundle))
    .setAddr(0x305)

  // Todo: support "Stimecmp/Vstimecmp" Extension, Version 1.0.0
  // Todo: support Sscounterenw Extension
  val mcounteren = Module(new CSRModule("Mcounteren", new Counteren))
    .setAddr(0x306)

  val mvien = Module(new CSRModule("Mvien", new MvienBundle))
    .setAddr(0x308)

  val mvip = Module(new CSRModule("Mvip", new MvipBundle) with HasMachineInterruptBundle {
    val toMip = IO(new MvipToMip)

    // When bit 1 of mvien is zero, bit 1(SSIP) of mvip is an alias of the same bit (SSIP) of mip.
    // But when bit 1 of mvien is one, bit 1(SSIP) of mvip is a separate writable bit independent of mip.SSIP.
    // When the value of bit 1 of mvien is changed from zero to one, the value of bit 1 of mvip becomes UNSPECIFIED.
    // XS will keep the value in mvip.SSIP when mvien.SSIE is changed from zero to one
    rdata.SSIP := Mux(!mvien.SSIE.asUInt.asBool, mip.SSIP, reg.SSIP)
    toMip.SSIP.valid := wen && !mvien.SSIE.asUInt.asBool
    toMip.SSIP.bits := wdata.SSIP
    reg.SSIP := Mux(wen && mvien.SSIE.asUInt.asBool, wdata.SSIP, reg.SSIP)

    // Bit 5 of mvip is an alias of the same bit (STIP) in mip when that bit is writable in mip.
    // When STIP is not writable in mip (such as when menvcfg.STCE = 1), bit 5 of mvip is read-only zero.
    // Todo: check mip writable when menvcfg.STCE = 1
    rdata.STIP := mip.STIP
    toMip.STIP.valid := wen
    toMip.STIP.bits := wdata.STIP

    // When bit 9 of mvien is zero, bit 9 of mvip is an alias of the software-writable bit 9 of mip (SEIP).
    // But when bit 9 of mvien is one, bit 9 of mvip is a writable bit independent of mip.SEIP.
    // Unlike for bit 1, changing the value of bit 9 of mvien does not affect the value of bit 9 of mvip.
    rdata.SEIP := Mux(!mvien.SEIE.asUInt.asBool, mip.SEIP, reg.SEIP)
    toMip.SEIP.valid := wen && !mvien.SEIE.asUInt.asBool
    toMip.SEIP.bits := wdata.SEIP
    reg.SEIP := Mux(wen && mvien.SEIE.asUInt.asBool, wdata.SEIP, reg.SEIP)
  }).setAddr(0x309)

  val mip = Module(new CSRModule("Mip", new MipBundle) with HasMachineInterruptBundle with HasExternalInterruptBundle {
    val fromMvip = IO(Flipped(new MvipToMip))

    // When bit 9 of mvien is zero, the value of bit 9 of mvip is logically ORed into the readable value of mip.SEIP.
    // when bit 9 of mvien is one, bit SEIP in mip is read-only and does not include the value of bit 9 of mvip.
    rdata.SEIP := Mux(!mvien.SEIE.asUInt.asBool, reg.SEIP.asUInt.asBool | mvip.SEIP.asUInt.asBool | SEIP, SEIP)
    when (wen && !mvien.SEIE.asUInt.asBool) { reg.SEIP := reg.SEIP }
    when (fromMvip.SSIP.valid) { reg.SSIP := fromMvip.SSIP.bits }
    when (fromMvip.STIP.valid) { reg.STIP := fromMvip.STIP.bits }
    when (fromMvip.SEIP.valid) { reg.SEIP := fromMvip.SEIP.bits }

    // MEIP is read-only in mip, and is set and cleared by a platform-specific interrupt controller.
    rdata.MEIP := MEIP
    // MTIP is read-only in mip, and is cleared by writing to the memory-mapped machine-mode timer compare register
    rdata.MTIP := MTIP
    // MSIP is read-only in mip, and is written by accesses to memory-mapped control registers,
    // which are used by remote harts to provide machine-level interprocessor interrupts.
    rdata.MSIP := MSIP
  }).setAddr(0x344)

  mip.fromMvip := mvip.toMip

  val machineLevelCSRMods: Seq[CSRModule[_]] = Seq(
    mstatus,
    misa,
    medeleg,
    mideleg,
    mie,
    mtvec,
    mcounteren,
    mvien,
    mvip,
    mip,
  )

  val machineLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap.from(
    machineLevelCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata.asInstanceOf[CSRBundle].asUInt))).iterator
  )
}

class MstatusBundle extends CSRBundle {

  val SIE  = CSRRWField     (1).withReset(0.U)
  val MIE  = CSRRWField     (3).withReset(0.U)
  val SPIE = CSRRWField     (5).withReset(0.U)
  val UBE  = CSRROField     (6).withReset(0.U)
  val MPIE = CSRRWField     (7).withReset(0.U)
  val SPP  = CSRRWField     (8).withReset(0.U)
  val VS   = ContextStatus  (10,  9).withReset(ContextStatus.Initial)
  val MPP  = PrivMode       (12, 11).withReset(PrivMode.U)
  val FS   = ContextStatus  (14, 13).withReset(ContextStatus.Initial)
  val XS   = ContextStatusRO(16, 15).withReset(0.U)
  val MPRV = CSRRWField     (17).withReset(0.U)
  val SUM  = CSRRWField     (18).withReset(0.U)
  val MXR  = CSRRWField     (19).withReset(0.U)
  val TVM  = CSRRWField     (20).withReset(0.U)
  val TW   = CSRRWField     (21).withReset(0.U)
  val TSR  = CSRRWField     (22).withReset(0.U)
  val UXL  = XLENField      (33, 32).withReset(XLENField.XLEN64)
  val SXL  = XLENField      (35, 34).withReset(XLENField.XLEN64)
  val SBE  = CSRROField     (36).withReset(0.U)
  val MBE  = CSRROField     (37).withReset(0.U)
  val GVA  = CSRRWField     (38).withReset(0.U)
  val MPV  = CSRRWField     (39).withReset(0.U)
  val SD   = CSRROField     (63,
    (_, _) => FS === ContextStatus.Dirty || VS === ContextStatus.Dirty
  )
}

class MstatusModule extends CSRModule("MStatus", new MstatusBundle) {
  val mstatus = IO(Output(bundle))
  val sstatus = IO(Output(new SstatusBundle))

  val wAliasSstatus = IO(Input(new CSRAddrWriteBundle(new SstatusBundle)))

  // write connection
  this.wfn(reg)(Seq(wAliasSstatus))

  // read connection
  mstatus :|= reg
  sstatus := mstatus
  rdata := mstatus.asUInt
}

class MisaBundle extends CSRBundle {
  // Todo: reset with ISA string
  val A = RO( 0).withReset(1.U) // Atomic extension
  val B = RO( 1).withReset(0.U) // Reserved
  val C = RO( 2).withReset(1.U) // Compressed extension
  val D = RO( 3).withReset(1.U) // Double-precision floating-point extension
  val E = RO( 4).withReset(0.U) // RV32E/64E base ISA
  val F = RO( 5).withReset(1.U) // Single-precision floating-point extension
  val G = RO( 6).withReset(0.U) // Reserved
  val H = RO( 7).withReset(1.U) // Hypervisor extension
  val I = RO( 8).withReset(1.U) // RV32I/64I/128I base ISA
  val J = RO( 9).withReset(0.U) // Reserved
  val K = RO(10).withReset(0.U) // Reserved
  val L = RO(11).withReset(0.U) // Reserved
  val M = RO(12).withReset(1.U) // Integer Multiply/Divide extensi
  val N = RO(13).withReset(0.U) // Tentatively reserved for User-Level Interrupts extension
  val O = RO(14).withReset(0.U) // Reserved
  val P = RO(15).withReset(0.U) // Tentatively reserved for Packed-SIMD extension
  val Q = RO(16).withReset(0.U) // Quad-precision floating-point extension
  val R = RO(17).withReset(0.U) // Reserved
  val S = RO(18).withReset(1.U) // Supervisor mode implemented
  val T = RO(19).withReset(0.U) // Reserved
  val U = RO(20).withReset(1.U) // User mode implemented
  val V = RO(21).withReset(1.U) // Vector extension
  val W = RO(22).withReset(0.U) // Reserved
  val X = RO(23).withReset(0.U) // Non-standard extensions present
  val Y = RO(24).withReset(0.U) // Reserved
  val Z = RO(25).withReset(0.U) // Reserved
  val MXL = XLENField(63, 62).withReset(XLENField.XLEN64)

  def getISAString = this.getFields.filterNot(_ == MXL).map(x => ('A' + x.msb).toChar).mkString
}

class MedelegBundle extends ExceptionBundle {
  this.EX_MCALL.setRO() // never delegate machine level ecall
}

class MidelegBundle extends InterruptBundle {
  // Don't delegate Machine level interrupts
  this.getM.foreach(_.setRO().withReset(0.U))
  // Ref: 13.4.2. Machine Interrupt Delegation Register (mideleg)
  // When the hypervisor extension is implemented, bits 10, 6, and 2 of mideleg (corresponding to the standard VS-level
  // interrupts) are each read-only one.
  this.getVS.foreach(_.setRO().withReset(1.U))
  // bit 12 of mideleg (corresponding to supervisor-level guest external interrupts) is also read-only one.
  // VS-level interrupts and guest external interrupts are always delegated past M-mode to HS-mode.
  this.SGEI.setRO().withReset(1.U)
}

class MieBundle extends InterruptEnableBundle {
  this.SGEIE.setRO()
  this.getVS.foreach(_.setRO())
}

class MipBundle extends InterruptPendingBundle {
  this.getM.foreach(_.setRO())
}

class MvienBundle extends CSRBundle {
  // Ref: riscv interrupt spec - 5.3 Interrupt filtering and virtual interrupts for supervisor level
  // It is strongly recommended that bit 9 of mvien be writable.
  // It is strongly recommended that bit 1 of mvien also be writable.
  val SSIE     = RW(1)
  val SEIE     = RW(9)
  val OTHERIE  = RW(63, 13)
}

class MvipBundle extends CSRBundle {
  // When bit 1 of mvien is zero, bit 1(SSIP) of mvip is an alias of the same bit (SSIP) of mip.
  // But when bit 1 of mvien is one, bit 1(SSIP) of mvip is a separate writable bit independent of mip.SSIP.
  // When the value of bit 1 of mvien is changed from zero to one, the value of bit 1 of mvip becomes UNSPECIFIED.
  val SSIP     = RW(1)
  // Bit 5 of mvip is an alias of the same bit (STIP) in mip when that bit is writable in mip.
  // When STIP is not writable in mip (such as when menvcfg.STCE = 1), bit 5 of mvip is read-only zero.
  val STIP     = RW(5)
  // When bit 9 of mvien is zero, bit 9 of mvip is an alias of the software-writable bit 9 of mip (SEIP).
  // But when bit 9 of mvien is one, bit 9 of mvip is a writable bit independent of mip.SEIP.
  // Unlike for bit 1, changing the value of bit 9 of mvien does not affect the value of bit 9 of mvip.
  val SEIP     = RW(9)
  val OTHERIP  = RW(63, 13)
}

class MieToHie extends Bundle {
  val VSSIE = ValidIO(RW(0))
  val VSTIE = ValidIO(RW(0))
  val VSEIE = ValidIO(RW(0))
  val SGEIE = ValidIO(RW(0))
}

class MvipToMip extends Bundle {
  val SSIP = ValidIO(RW(0))
  val STIP = ValidIO(RW(0))
  val SEIP = ValidIO(RW(0))
}

trait HasMachineInterruptBundle { self: CSRModule[_] =>
  val mvien = IO(Input(new MvienBundle))
  val mvip  = IO(Input(new MvipBundle))
  val mip   = IO(Input(new MipBundle))
}

trait HasExternalInterruptBundle {
  val SEIP = IO(Input(Bool()))
  val MEIP = IO(Input(Bool()))
  val MTIP = IO(Input(Bool()))
  val MSIP = IO(Input(Bool()))
}
