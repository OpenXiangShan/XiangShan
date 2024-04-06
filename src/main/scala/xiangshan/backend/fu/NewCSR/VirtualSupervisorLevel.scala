package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
}

import scala.collection.immutable.SeqMap

trait VirtualSupervisorLevel { self: NewCSR with HypervisorLevel =>

  val vsstatus = Module(new CSRModule("VSstatus", new SstatusBundle))
    .setAddr(0x200)

  val vsie = Module(new CSRModule("VSie", new VSie) with HypervisorBundle {
    val writeHie = IO(new VSieToHie)
    // read alias of hie is here, write alias will be in hie
    rdata.SEIE := Mux(hideleg.VSEI.asUInt === 0.U, 0.U, hie.VSEIE.asUInt)
    rdata.STIE := Mux(hideleg.VSTI.asUInt === 0.U, 0.U, hie.VSTIE.asUInt)
    rdata.SSIE := Mux(hideleg.VSSI.asUInt === 0.U, 0.U, hie.VSSIE.asUInt)

    writeHie.SEIE.valid := wen && hideleg.VSEI.asUInt.asBool
    writeHie.STIE.valid := wen && hideleg.VSTI.asUInt.asBool
    writeHie.SSIE.valid := wen && hideleg.VSSI.asUInt.asBool
    writeHie.SEIE.bits := wdata.SEIE
    writeHie.STIE.bits := wdata.STIE
    writeHie.SSIE.bits := wdata.SSIE
  }).setAddr(0x204)

  hie.fromVSie := vsie.writeHie

  val vstvec = Module(new CSRModule("VStvec", new XtvecBundle))
    .setAddr(0x205)

  val vsscratch = Module(new CSRModule("VSscratch"))
    .setAddr(0x240)

  val vsepc = Module(new CSRModule("VSepc"))
    .setAddr(0x241)

  val vscause = Module(new CSRModule("VScause", new CauseBundle))
    .setAddr(0x242)

  // Todo: shrink the width of vstval to the maximum width Virtual Address
  val vstval = Module(new CSRModule("VStval"))
    .setAddr(0x243)

  val vsip = Module(new CSRModule("VSip", new VSip) with HypervisorBundle {
    val writeHip = IO(new VSipToHip)
    // read alias of hip is here, write alias will be in hvip
    // hip.VSEIP is read-only
    rdata.SEIP := Mux(hideleg.VSEI.asUInt === 0.U, 0.U, hip.VSEIP.asUInt)
    // hip.VSTIP is read-only
    rdata.STIP := Mux(hideleg.VSTI.asUInt === 0.U, 0.U, hip.VSTIP.asUInt)
    // hip.VSSIP is an alias (writable) of the same bit in hvip
    rdata.SSIP := Mux(hideleg.VSSI.asUInt === 0.U, 0.U, hip.VSSIP.asUInt)

    writeHip.SEIP.valid := wen && hideleg.VSEI.asUInt.asBool
    writeHip.STIP.valid := wen && hideleg.VSTI.asUInt.asBool
    writeHip.SSIP.valid := wen && hideleg.VSSI.asUInt.asBool
    writeHip.SEIP.bits := wdata.SEIP
    writeHip.STIP.bits := wdata.STIP
    writeHip.SSIP.bits := wdata.SSIP
  }).setAddr(0x244)

  hip.fromVSip := vsip.writeHip

  val vsatp = Module(new CSRModule("VSatp", new SatpBundle) {
    // Ref: 13.2.18. Virtual Supervisor Address Translation and Protection Register (vsatp)
    // When V=0, a write to vsatp with an unsupported MODE value is either ignored as it is for satp, or the
    // fields of vsatp are treated as WARL in the normal way.
    // However, when V=1, a write to satp with an unsupported MODE value is ignored and no write to vsatp is effected.
    // if satp is written with an unsupported MODE, the entire write has no effect; no fields in satp are modified.
    //
    // We treat all circumstances as if V=1. That is if satp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when(wen && !wdata.MODE.isLegal) {
      reg.ASID := reg.ASID
      reg.PPN := reg.PPN
    }
  }).setAddr(0x280)

  val virtualSupervisorCSRMods = Seq(
    vsstatus,
    vsie,
    vstvec,
    vsscratch,
    vsepc,
    vscause,
    vstval,
    vsip,
    vsatp,
  )

  virtualSupervisorCSRMods.foreach(mod =>
    require(mod.addr > 0, s"The address of ${mod.modName} has not been set, you can use setAddr(CSRAddr) to set it."))

  val virtualSupervisorCSRMap = SeqMap.from(
    virtualSupervisorCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata.asInstanceOf[CSRBundle].asUInt)))
  )
}

class VSip extends InterruptPendingBundle {
  this.getM.foreach(_.setRO())
  this.getVS.foreach(_.setRO())
  this.getSOC.foreach(_.setRO())
  // 13.2.12. Virtual Supervisor Interrupt Registers (vsip and vsie)
  // When bit 10 of hideleg is zero, vsip.SEIP is read-only zeros.
  // Else, vsip.SEIP is alias of hip.VSEIP
  this.SEIP
  // When bit 6 of hideleg is zero, vsip.STIP is read-only zeros.
  // Else, vsip.STIP is alias of hip.VSTIP
  this.STIP
  // When bit 2 of hideleg is zero, vsip.SSIP is read-only zeros.
  // Else, vsip.SSIP is alias of hip.VSSIP
  this.SEIP
}

class VSie extends InterruptEnableBundle {
  this.getM.foreach(_.setRO())
  this.getVS.foreach(_.setRO())
  // 13.2.12. Virtual Supervisor Interrupt Registers (vsip and vsie)
  // When bit 10 of hideleg is zero, vsip.SEIE is read-only zeros.
  // Else, vsip.SEIE is alias of hip.VSEIE
  this.SEIE
  // When bit 6 of hideleg is zero, vsip.STIE is read-only zeros.
  // Else, vsip.STIE is alias of hip.VSTIE
  this.STIE
  // When bit 2 of hideleg is zero, vsip.SSIE is read-only zeros.
  // Else, vsip.SSIE is alias of hip.VSSIE
  this.SEIE
}

class VSieToHie extends Bundle {
  val SSIE: ValidIO[CSREnumType] = ValidIO(RW(0))
  val STIE: ValidIO[CSREnumType] = ValidIO(RW(0))
  val SEIE: ValidIO[CSREnumType] = ValidIO(RW(0))
}

class VSipToHip extends Bundle {
  val SSIP = ValidIO(RW(0))
  val STIP = ValidIO(RW(0))
  val SEIP = ValidIO(RW(0))
}
