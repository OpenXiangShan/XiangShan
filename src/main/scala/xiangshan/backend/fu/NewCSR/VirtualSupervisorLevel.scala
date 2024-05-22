package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import utility.SignExt
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  CSRWARLField => WARL,
  _
}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast.CSREnumTypeToUInt
import xiangshan.backend.fu.NewCSR.CSREvents.{SretEventSinkBundle, TrapEntryVSEventSinkBundle}

import scala.collection.immutable.SeqMap

trait VirtualSupervisorLevel { self: NewCSR with SupervisorLevel with HypervisorLevel =>

  val vsstatus = Module(
    new CSRModule("VSstatus", new SstatusBundle)
      with SretEventSinkBundle
      with TrapEntryVSEventSinkBundle
  )
    .setAddr(0x200)

  val vsie = Module(new CSRModule("VSie", new VSie) with HypervisorBundle {
    val toHie = IO(new VSieToHie)
    // read alias of hie is here, write alias will be in hie
    regOut.SEIE := Mux(hideleg.VSEI.asUInt === 0.U, 0.U, hie.VSEIE.asUInt)
    regOut.STIE := Mux(hideleg.VSTI.asUInt === 0.U, 0.U, hie.VSTIE.asUInt)
    regOut.SSIE := Mux(hideleg.VSSI.asUInt === 0.U, 0.U, hie.VSSIE.asUInt)

    toHie.SEIE.valid := wen && hideleg.VSEI.asUInt.asBool
    toHie.STIE.valid := wen && hideleg.VSTI.asUInt.asBool
    toHie.SSIE.valid := wen && hideleg.VSSI.asUInt.asBool
    toHie.SEIE.bits := wdata.SEIE
    toHie.STIE.bits := wdata.STIE
    toHie.SSIE.bits := wdata.SSIE
  }).setAddr(0x204)

  hie.fromVSie := vsie.toHie

  val vstvec = Module(new CSRModule("VStvec", new XtvecBundle))
    .setAddr(0x205)

  val vsscratch = Module(new CSRModule("VSscratch"))
    .setAddr(0x240)

  val vsepc = Module(
    new CSRModule("VSepc", new Epc)
      with TrapEntryVSEventSinkBundle
    {
      rdata := SignExt(Cat(reg.epc.asUInt, 0.U(1.W)), XLEN)
    }
  )
    .setAddr(0x241)

  val vscause = Module(
    new CSRModule("VScause", new CauseBundle)
      with TrapEntryVSEventSinkBundle
  )
    .setAddr(0x242)

  // Todo: shrink the width of vstval to the maximum width Virtual Address
  val vstval = Module(
    new CSRModule("VStval")
      with TrapEntryVSEventSinkBundle
  )
    .setAddr(0x243)

  val vsip = Module(new CSRModule("VSip", new VSip) with HypervisorBundle {
    val toHip = IO(new VSipToHip)
    // read alias of hip is here, write alias will be in hvip
    // hip.VSEIP is read-only zero when hideleg.VSEI=0, alias if hip.VSEIP when hideleg.VSEI=1
    regOut.SEIP := Mux(hideleg.VSEI.asUInt === 0.U, 0.U, hip.VSEIP.asUInt)
    // hip.VSTIP is read-only zero when hideleg.VSTI=0, alias if hip.VSTIP when hideleg.VSTI=1
    regOut.STIP := Mux(hideleg.VSTI.asUInt === 0.U, 0.U, hip.VSTIP.asUInt)
    // hip.VSSIP is read-only zero when hideleg.VSSI=0, alias (writable) of the same bit in hvip, when hideleg.VSSI=1
    regOut.SSIP := Mux(hideleg.VSSI.asUInt === 0.U, 0.U, hip.VSSIP.asUInt)

    toHip.SEIP.valid := wen && hideleg.VSEI.asUInt.asBool
    toHip.STIP.valid := wen && hideleg.VSTI.asUInt.asBool
    toHip.SSIP.valid := wen && hideleg.VSSI.asUInt.asBool
    toHip.SEIP.bits := wdata.SEIP
    toHip.STIP.bits := wdata.STIP
    toHip.SSIP.bits := wdata.SSIP
  }).setAddr(0x244)

  hip.fromVSip := vsip.toHip

  val vstimecmp = Module(new CSRModule("VStimecmp"))
    .setAddr(0x24D)

  val vsatp = Module(new CSRModule("VSatp", new SatpBundle) {
    // Ref: 13.2.18. Virtual Supervisor Address Translation and Protection Register (vsatp)
    // When V=0, a write to vsatp with an unsupported MODE value is either ignored as it is for satp, or the
    // fields of vsatp are treated as WARL in the normal way.
    // However, when V=1, a write to satp with an unsupported MODE value is ignored and no write to vsatp is effected.
    // if satp is written with an unsupported MODE, the entire write has no effect; no fields in satp are modified.
    //
    // We treat all circumstances as if V=1. That is if vsatp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when(wen) {
      when (wdata.MODE.isLegal) {
        reg := wdata
      }
    }.otherwise {
      reg := reg
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
    vstimecmp,
    vsatp,
  )

  virtualSupervisorCSRMods.foreach(mod =>
    require(mod.addr > 0, s"The address of ${mod.modName} has not been set, you can use setAddr(CSRAddr) to set it."))

  val virtualSupervisorCSRMap = SeqMap.from(
    virtualSupervisorCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata)))
  )

  val virtualSupervisorCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    virtualSupervisorCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt))
  )

  import freechips.rocketchip.rocket.CSRs

  val sMapVS = SeqMap(
    CSRs.sstatus  -> CSRs.vsstatus,
    CSRs.sie      -> CSRs.vsie,
    CSRs.stvec    -> CSRs.vstvec,
    CSRs.sscratch -> CSRs.vsscratch,
    CSRs.sepc     -> CSRs.vsepc,
    CSRs.scause   -> CSRs.vscause,
    CSRs.stval    -> CSRs.vstval,
    CSRs.sip      -> CSRs.vsip,
    CSRs.stimecmp -> CSRs.vstimecmp,
    CSRs.siselect -> CSRs.vsiselect,
    CSRs.sireg    -> CSRs.vsireg,
    CSRs.stopei   -> CSRs.vstopei,
    CSRs.satp     -> CSRs.vsatp,
    CSRs.stopi    -> CSRs.vstopi,
  )

  val vsMapS: SeqMap[Int, Int] = SeqMap.from(sMapVS.map(x => (x._2 -> x._1)))
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
  this.getSOC.foreach(_.setRO())
  // 13.2.12. Virtual Supervisor Interrupt Registers (vsip and vsie)
  // When bit 10 of hideleg is zero, vsip.SEIE is read-only zeros.
  // Else, vsip.SEIE is alias of hip.VSEIE
  this.SSIE
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
