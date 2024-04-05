package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  CSRWARLField => WARL,
  _
}

import scala.collection.immutable.SeqMap

trait VirtualSupervisorLevel { self: NewCSR =>

  val vsstatus = Module(new CSRModule("Vsstatus", new SstatusBundle)).setAddr(0x200)
  val vsip = Module(new CSRModule("Vsip", new Vsip) with HypervisorBundle {
    val writeHie = IO(new VsieWriteHie)
    // read alias of hip is here, write alias will be in hvip
    // hip.VSEIP is read-only
    rdata.SEIP := Mux(hideleg.VSEI === 0.U, 0.U, hip.VSEIP)
    // hip.VSTIP is read-only
    rdata.STIP := Mux(hideleg.VSTI === 0.U, 0.U, hip.VSTIP)
    // hip.VSSIP is an alias (writable) of the same bit in hvip
    rdata.SSIP := Mux(hideleg.VSSI === 0.U, 0.U, hip.VSSIP)

    writeHie.SEIP.valid := wen && hideleg.VSEI.asUInt.asBool
    writeHie.STIP.valid := wen && hideleg.VSTI.asUInt.asBool
    writeHie.SSIP.valid := wen && hideleg.VSSI.asUInt.asBool
    writeHie.SEIP.bits := wdata.SEIP
    writeHie.STIP.bits := wdata.STIP
    writeHie.SSIP.bits := wdata.SSIP
  })

  val vsie = Module(new CSRModule("Vsie", new Vsie) with HypervisorBundle {
    // read alias of hie is here, write alias will be in hip
    rdata.SEIE := Mux(hideleg.VSEI === 0.U, 0.U, hip.VSEIP)
    rdata.STIE := Mux(hideleg.VSTI === 0.U, 0.U, hip.VSTIP)
    rdata.SSIE := Mux(hideleg.VSSI === 0.U, 0.U, hip.VSSIP)
  })

  val virtualSupervisorCSRMods = Seq(
    vsstatus,
  )

  virtualSupervisorCSRMods.foreach(mod =>
    require(mod.addr > 0, s"The address of ${mod.modName} has not been set, you can use setAddr(CSRAddr) to set it."))

  val virtualSupervisorCSRMap = SeqMap.from(
    virtualSupervisorCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata.asInstanceOf[CSRBundle].asUInt)))
  )
}

class Vsip extends InterruptPendingBundle {
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

class Vsie extends InterruptEnableBundle {
  this.getM.foreach(_.setRO())
  this.getVS.foreach(_.setRO())
  this.getSOC.foreach(_.setRO())
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

class VsieWriteHie extends Bundle {
  val SSIP = ValidIO(RW(0))
  val STIP = ValidIO(RW(0))
  val SEIP = ValidIO(RW(0))
}