package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util.{BitPat, Cat, Mux1H, MuxCase, ValidIO}
import utility.SignExt
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.fu.NewCSR.CSRBundles._
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL, CSRWLRLField => WLRL, _}
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.NewCSR.CSREvents.TrapEntryHSEventSinkBundle
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSRBundleImplicitCast._
import xiangshan.backend.fu.NewCSR.ChiselRecordForField._

import scala.collection.immutable.SeqMap

trait SupervisorLevel { self: NewCSR with MachineLevel =>
  val sie = Module(new CSRModule("Sie", new SieBundle)
    with HasIpIeBundle
  {
    val toMie    = IO(new SieToMie)
    val fromVSie = IO(Flipped(new VSieToSie))

    // Sie is alias of mie when mideleg=1.
    // Otherwise, sie has seperate writable registers
    // There are no regs in CSR sie.
    val mieIsAlias = mideleg
    val usingReg   = ~mideleg & mvien
    regOut := (mieIsAlias & mie) | (usingReg & reg)

    bundle.getFields.map(_.lsb).foreach { num =>
      val wtMie  = toMie.getByNum(num)
      val vsieWt = fromVSie.getByNum(num)

      wtMie.specifyField(
        _.valid := wen && mieIsAlias(num) && wtMie.bits.isRW.B,
        _.bits  := wen && mieIsAlias(num) && wtMie.bits.isRW.B &< wdata(num),
      )

      when (
        wen && usingReg(num) && reg(num).isRW.B ||
        vsieWt.valid && usingReg(num) && vsieWt.bits.isRW.B
      ) {
        reg(num) := Mux1H(Seq(
          wen          -> wdata(num),
          vsieWt.valid -> vsieWt.bits,
        ))
      }.otherwise {
        reg(num) := reg(num)
      }
    }

    regOut.getFields.foreach { field =>
      if (field.isHardWired) {
        field := field.getHardWireValue
      }
    }
  })
    .setAddr(CSRs.sie)

  val stvec = Module(new CSRModule("Stvec", new XtvecBundle))
    .setAddr(CSRs.stvec)

  val scounteren = Module(new CSRModule("Scounteren", new Counteren))
    .setAddr(CSRs.scounteren)

  val senvcfg = Module(new CSRModule("Senvcfg", new SEnvCfg))
    .setAddr(CSRs.senvcfg)

  val sscratch = Module(new CSRModule("Sscratch"))
    .setAddr(CSRs.sscratch)

  val sepc = Module(new CSRModule("Sepc", new Epc) with TrapEntryHSEventSinkBundle {
    rdata := SignExt(Cat(reg.epc.asUInt, 0.U(1.W)), XLEN)
  })
    .setAddr(CSRs.sepc)

  val scause = Module(new CSRModule("Scause", new CauseBundle) with TrapEntryHSEventSinkBundle)
    .setAddr(CSRs.scause)

  val stval = Module(new CSRModule("Stval") with TrapEntryHSEventSinkBundle)
    .setAddr(CSRs.stval)

  val sip = Module(new CSRModule("Sip", new SipBundle)
    with HasIpIeBundle
  {
    val toMip  = IO(new SipToMip)
    val toMvip = IO(new SipToMvip)

    // Ref: 7.1.3. Supervisor Interrupt Registers (sip and sie)
    // The sip and sie registers are subsets of the mip and mie registers. Reading any
    // implemented field, or writing any writable field, of sip/sie effects a read or write of the
    // homonymous field of mip/mie.

    // Ref: 3.1.9. Machine Interrupt Registers (mip and mie)
    // Restricted views of the mip and mie registers appear as the sip and sie registers for supervisor level. If
    // an interrupt is delegated to S-mode by setting a bit in the mideleg register, it becomes visible in the
    // sip register and is maskable using the sie register. Otherwise, the corresponding bits in sip and sie
    // are **read-only zero**.
    val mipIsAlias  = mideleg
    val mvipIsAlias = ~mideleg & mvien

    dontTouch(mvipIsAlias)

    regOut := mipIsAlias & mip | (mvipIsAlias & mvip)

    bundle.getFields.map(_.lsb).foreach { num =>
      val wtMip  = toMip.getByNum(num)
      val wtMvip = toMvip.getByNum(num)

      wtMip.specifyField(
        _.valid := wen && mipIsAlias(num) && wtMip.bits.isRW.B,
        _.bits  := wen && mipIsAlias(num) && wtMip.bits.isRW.B &< wdata(num),
      )

      wtMvip.specifyField(
        _.valid := wen && mvipIsAlias(num) && wtMvip.bits.isRW.B,
        _.bits  := wen && mvipIsAlias(num) && wtMvip.bits.isRW.B &< wdata(num),
      )
    }

    regOut.getFields.foreach { field =>
      if (field.isHardWired) {
        field := field.getHardWireValue
      }
    }
  })
    .setAddr(CSRs.sip)

  val stimecmp = Module(new CSRModule("Stimecmp", new CSRBundle {
    val stimecmp = RW(63, 0).withReset(bitPatToUInt(BitPat.Y(64)))
  }))
    .setAddr(CSRs.stimecmp)

  val satp = Module(new CSRModule("Satp", new SatpBundle) {
    // If satp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when (wen && wdata.MODE.isLegal) {
      reg := wdata
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.satp)

  val scountovf = Module(new CSRModule("Scountovf", new CSRBundle {
    val OFVEC = RO(31, 3).withReset(0.U)
  }) with HasMhpmeventOfBundle {
    reg.OFVEC := ofVec.asUInt
  }).setAddr(CSRs.scountovf)

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
    scountovf,
  )

  val supervisorLevelCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap(
    CSRs.sstatus -> (mstatus.wAliasSstatus, mstatus.sstatusRdata),
  ) ++ SeqMap.from(
    supervisorLevelCSRMods.map(csr => (csr.addr -> (csr.w, csr.rdata))).iterator
  )

  val supervisorLevelCSROutMap: SeqMap[Int, UInt] = SeqMap(
    CSRs.sstatus -> mstatus.sstatus.asUInt,
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
  this.getHS.foreach(_.setRW().withReset(0.U))
  this.STIE.setRO()
  this.getLocal.foreach(_.setRW().withReset(0.U))
  this.getM .foreach(_.setHardWired(0.U))
  this.getVS.foreach(_.setHardWired(0.U))
  this.SGEIE.setHardWired(0.U)
}

class SipBundle extends InterruptPendingBundle {
  // All pending bits in sip are aliases of mip or read-only 0
  this.getM .foreach(_.setHardWired(0.U))
  this.getVS.foreach(_.setHardWired(0.U))
  this.SGEIP.setHardWired(0.U)
}

class SatpBundle extends CSRBundle {
  final val PPN_msb = PAddrWidth - AddrWidthInPage - 1
  val MODE = SatpMode(63, 60, null).withReset(SatpMode.Bare)
  // WARL in privileged spec.
  // RW, since we support max width of ASID
  val ASID = RW(44 - 1 + ASIDLEN, 44)
  val PPN  = RW(PPN_msb, 0)
}

class SEnvCfg extends EnvCfg

class SipToMip extends IpValidBundle {
  this.SSIP.bits.setRW()
  this.LCOFIP.bits.setRW()
}

class SipToMvip extends IpValidBundle {
  this.SSIP.bits.setRW()
  this.getLocal.foreach(_.bits.setRW())
}

class SieToMie extends IeValidBundle {
  this.getHS.foreach(_.bits.setRW())
  this.getLocal.foreach(_.bits.setRW())
}

trait HasMhpmeventOfBundle { self: CSRModule[_] =>
  val ofVec = IO(Input(Vec(perfCntNum, Bool())))
}