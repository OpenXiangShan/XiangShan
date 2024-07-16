package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util.{BitPat, Cat, Fill, Mux1H, MuxCase, ValidIO}
import utility.{SignExt, ZeroExt}
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

  val stval = Module(new CSRModule("Stval", new XtvalBundle) with TrapEntryHSEventSinkBundle)
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
    val ppnMask = ZeroExt(Fill(PPNLength, 1.U(1.W)).take(PAddrBits - PageOffsetWidth), PPNLength)
    // If satp is written with an unsupported MODE,
    // the entire write has no effect; no fields in satp are modified.
    when (wen && wdata.MODE.isLegal) {
      reg.MODE := wdata.MODE
      reg.ASID := wdata.ASID
      reg.PPN := wdata.PPN & ppnMask
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.satp)

  // scountovf: This register enables supervisor-level overflow interrupt handler software to quickly and easily
  // determine which counter(s) have overflowed (without needing to make an execution environment call
  // or series of calls ultimately up to M-mode).
  val scountovf = Module(new CSRModule("Scountovf", new CSRBundle {
    override val len: Int = 32
    val OFVEC = RO(31, 3).withReset(0.U)
  }) with HasMhpmeventOfBundle {
    reg.OFVEC := ofVec
    regOut.OFVEC := Mux1H(Seq(
        privState.isModeM  -> reg.OFVEC.asUInt,
        privState.isModeHS -> (mcounteren.HPM.asUInt & reg.OFVEC.asUInt),
        privState.isModeVS -> (mcounteren.HPM.asUInt & hcounteren.HPM.asUInt & reg.OFVEC.asUInt),
      )
    )
  }).setAddr(CSRs.scountovf)

  val sstateen0 = Module(new CSRModule("Sstateen", new SstateenBundle0) with HasStateen0Bundle {
    // For every bit in an mstateen CSR that is zero (whether read-only zero or set to zero), the same bit
    // appears as read-only zero in the matching hstateen and sstateen CSRs. For every bit in an hstateen
    // CSR that is zero (whether read-only zero or set to zero), the same bit appears as read-only zero in
    // sstateen when accessed in VS-mode.
    regOut := Mux(privState.isVirtual, fromHstateen0.asUInt, fromMstateen0.asUInt) & reg.asUInt
  }).setAddr(CSRs.sstateen0)

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
    sstateen0,
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
  val UBE  = CSRROField     (6).withReset(0.U)
  val SPP  = CSRWARLField   (8, wNoFilter).withReset(0.U)
  val VS   = ContextStatus  (10, 9).withReset(ContextStatus.Off)
  val FS   = ContextStatus  (14, 13).withReset(ContextStatus.Off)
  val XS   = ContextStatusRO(16, 15).withReset(0.U)
  val SUM  = CSRWARLField   (18, wNoFilter).withReset(0.U)
  val MXR  = CSRWARLField   (19, wNoFilter).withReset(0.U)
  val UXL  = XLENField      (33, 32).withReset(XLENField.XLEN64)
  val SD   = CSRROField     (63, (_, _) => FS === ContextStatus.Dirty || VS === ContextStatus.Dirty)
}

class SieBundle extends InterruptEnableBundle {
  this.getHS.foreach(_.setRW().withReset(0.U))
  this.STIE.setRO().withReset(0.U)
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
  val MODE = SatpMode(63, 60, null).withReset(SatpMode.Bare)
  // WARL in privileged spec.
  // RW, since we support max width of ASID
  val ASID = RW(44 - 1 + ASIDLEN, 44).withReset(0.U)
  // Do WARL in SatpModule/VSatpModule
  val PPN  = RW(43, 0).withReset(0.U)
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
  val ofVec = IO(Input(UInt(perfCntNum.W)))
  val privState = IO(Input(new PrivState))
  val mcounteren = IO(Input(new Counteren))
  val hcounteren = IO(Input(new Counteren))
}