package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import utility.SignExt
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines.{
  CSRRWField => RW,
  CSRROField => RO,
  CSRWLRLField => WLRL,
  CSRWARLField => WARL,
  VirtMode,
  _
}
import xiangshan.backend.fu.NewCSR.CSREvents.{SretEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSRBundleImplicitCast._
import xiangshan.backend.fu.NewCSR.ChiselRecordForField._

import scala.collection.immutable.SeqMap

trait VirtualSupervisorLevel { self: NewCSR with SupervisorLevel with HypervisorLevel =>

  val vsstatus = Module(
    new CSRModule("VSstatus", new SstatusBundle)
      with SretEventSinkBundle
      with TrapEntryVSEventSinkBundle
  )
    .setAddr(0x200)

  val vsie = Module(new CSRModule("VSie", new VSieBundle)
    with HypervisorBundle
    with HasIpIeBundle
  {
    val toMie = IO(new VSieToMie)
    val toSie = IO(new VSieToSie)

    val mieIsAlias =  hideleg &  mideleg
    val sieIsAlias =  hideleg & ~mideleg & mvien
    val usingReg   = ~hideleg &            hvien

    regOut :=
      (mieIsAlias & mie) |
      (sieIsAlias & sie) |
      (usingReg   & reg)

    bundle.getFields.map(_.lsb).foreach { num =>
      val wtMie = toMie.getByNum(num)
      val wtSie = toSie.getByNum(num)
      val r = reg(num)

      wtMie.specifyField(
        _.valid := mieIsAlias(num) && wtMie.bits.isRW.B && wen,
        _.bits  := mieIsAlias(num) && wtMie.bits.isRW.B && wen &< wdata(num),
      )

      wtSie.specifyField(
        _.valid := sieIsAlias(num) && wtSie.bits.isRW.B && wen,
        _.bits  := sieIsAlias(num) && wtSie.bits.isRW.B && wen &< wdata(num),
      )

      when (wen && usingReg(num) && r.isRW.B) {
        r := wdata(num)
      }.otherwise {
        r := r
      }
    }
  }).setAddr(0x204)

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

  val vsip = Module(new CSRModule("VSip", new VSipBundle)
    with HypervisorBundle
    with HasIpIeBundle
  {
    val toMip  = IO(new VSipToMip).connectZeroNonRW
    val toMvip = IO(new VSipToMvip).connectZeroNonRW
    val toHvip = IO(new VSipToHvip).connectZeroNonRW

    val originIP = mideleg & hideleg & mip | (~mideleg & hideleg & mvien & mvip) | (~hideleg & hvien & hvip)
    val shiftedIP = Cat(originIP(63, InterruptNO.COI), 0.U(1.W), originIP(InterruptNO.SGEI, InterruptNO.SSI))

    regOut := shiftedIP
    regOut.getM.foreach(_ := 0.U)
    regOut.getHS.foreach(_ := 0.U)
    regOut.SGEIP := 0.U

    toHvip.VSSIP.valid := wen && hideleg.VSSI
    toHvip.VSSIP.bits  := wdata.SSIP

    wdata.getLocal lazyZip
      (toMip.getLocal lazyZip toMvip.getLocal lazyZip toHvip.getLocal) lazyZip
      (mideleg.getLocal lazyZip hideleg.getLocal lazyZip mvien.getLocal) foreach {
        case (wLCIP, (toMipLCIP, toMvipLCIP, toHvipLCIP), (midelegBit, hidelegBit, mvienBit)) =>
          toMipLCIP .valid := wen &&  midelegBit &&  hidelegBit
          toMvipLCIP.valid := wen && !midelegBit &&  hidelegBit &&  mvienBit
          toHvipLCIP.valid := wen &&                !hidelegBit &&  mvienBit
          toMipLCIP .bits := wLCIP
          toMvipLCIP.bits := wLCIP
          toHvipLCIP.bits := wLCIP
    }
  }).setAddr(0x244)

  val vstimecmp = Module(new CSRModule("VStimecmp", new CSRBundle {
    val vstimecmp = RW(63, 0).withReset(bitPatToUInt(BitPat.Y(64)))
  }))
    .setAddr(0x24D)

  val vsatp = Module(new CSRModule("VSatp", new SatpBundle) with VirtualSupervisorBundle {
    // Ref: 13.2.18. Virtual Supervisor Address Translation and Protection Register (vsatp)
    // When V=0, a write to vsatp with an unsupported MODE value is either ignored as it is for satp, or the
    // fields of vsatp are treated as WARL in the normal way.
    // However, when V=1, a write to satp with an unsupported MODE value is ignored and no write to vsatp is effected.
    // if satp is written with an unsupported MODE, the entire write has no effect; no fields in satp are modified.
    //
    // We treat all circumstances as if V=1. That is if vsatp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when(wen && wdata.MODE.isLegal) {
      reg := wdata
    }.elsewhen(wen && !v && !wdata.MODE.isLegal) {
      reg.PPN := wdata.PPN
      reg.ASID := wdata.ASID
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

  val virtualSupervisorCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
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

class VSipBundle extends InterruptPendingBundle {
  // All pending bits in vsip are aliases of mip/mvip/hvip or read-only 0
}

class VSieBundle extends InterruptEnableBundle {
  this.getLocal.foreach(_.setRW())
}

class VSipToMvip extends IpValidBundle {
  this.getLocal.foreach(_.bits.setRW())
}

class VSipToHvip extends IpValidBundle {
  this.VSSIP.bits.setRW()
  this.getLocal.foreach(_.bits.setRW())
}

class VSieToMie extends IeValidBundle {
  this.getVS.foreach(_.bits.setRW())
  this.getLocal.foreach(_.bits.setRW())
}

class VSieToSie extends IeValidBundle {
  this.getVS.foreach(_.bits.setRW())
  this.getLocal.foreach(_.bits.setRW())
}

class VSipToHip extends Bundle {
  val SSIP = ValidIO(RW(0))
  val STIP = ValidIO(RW(0))
  val SEIP = ValidIO(RW(0))
}

trait VirtualSupervisorBundle { self: CSRModule[_] =>
  val v = IO(Input(Bool()))
}
