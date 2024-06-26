package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.fu.NewCSR.CSRBundles.{Counteren, PrivState}
import xiangshan.backend.fu.NewCSR.CSRDefines._

class CSRPermitModule extends Module {
  val io = IO(new CSRPermitIO)

  private val (ren, wen, addr, privState, debugMode) = (
    io.in.csrAccess.ren,
    io.in.csrAccess.wen,
    io.in.csrAccess.addr,
    io.in.privState,
    io.in.debugMode
  )

  private val csrAccess = WireInit(ren || wen)

  private val (mret, sret, dret) = (
    io.in.mret,
    io.in.sret,
    io.in.dret,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
  )

  private val (tw, vtw) = (
    io.in.status.tw,
    io.in.status.vtw
  )

  private val (tvm, vtvm) = (
    io.in.status.tvm,
    io.in.status.vtvm,
  )

  private val csrIsCustom = io.in.csrIsCustom

  private val (mcounteren, hcounteren, scounteren) = (
    io.in.status.mcounteren,
    io.in.status.hcounteren,
    io.in.status.scounteren,
  )

  private val (mstateen0, hstateen0, sstateen0) = (
    io.in.status.mstateen0,
    io.in.status.hstateen0,
    io.in.status.sstateen0,
  )

  private val (mcounterenTM, hcounterenTM) = (
    mcounteren(1),
    hcounteren(1),
  )

  private val (menvcfg, henvcfg) = (
    io.in.status.menvcfg,
    io.in.status.henvcfg,
  )

  private val (menvcfgSTCE, henvcfgSTCE) = (
    menvcfg(63),
    henvcfg(63),
  )

  private val (sFSIsOff, sVSIsOff, sOrVsFSIsOff, sOrVsVSIsOff) = (
    io.in.status.mstatusFSOff,
    io.in.status.mstatusVSOff,
    io.in.status.mstatusFSOff || io.in.status.vsstatusFSOff,
    io.in.status.mstatusVSOff || io.in.status.vsstatusVSOff,
  )

  private val csrIsRO = addr(11, 10) === "b11".U
  private val csrIsUnpriv = addr(9, 8) === "b00".U
  private val csrIsM = addr(9, 8) === "b11".U
  private val csrIsHPM = addr >= CSRs.cycle.U && addr <= CSRs.hpmcounter31.U
  private val csrIsFp = Seq(CSRs.fflags, CSRs.frm, CSRs.fcsr).map(_.U === addr).reduce(_ || _)
  private val csrIsVec = Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr, CSRs.vl, CSRs.vtype, CSRs.vlenb).map(_.U === addr).reduce(_ || _)
  private val csrIsWritableVec = Seq(CSRs.vstart, CSRs.vxsat, CSRs.vxrm, CSRs.vcsr).map(_.U === addr).reduce(_ || _)
  private val counterAddr = addr(4, 0) // 32 counters

  private val accessTable = TruthTable(Seq(
    //       V PRVM ADDR
    BitPat("b0__00___00") -> BitPat.Y(), // HU access U
    BitPat("b1__00___00") -> BitPat.Y(), // VU access U
    BitPat("b0__01___00") -> BitPat.Y(), // HS access U
    BitPat("b0__01___01") -> BitPat.Y(), // HS access S
    BitPat("b0__01___10") -> BitPat.Y(), // HS access H
    BitPat("b1__01___00") -> BitPat.Y(), // VS access U
    BitPat("b1__01___01") -> BitPat.Y(), // VS access S
    BitPat("b0__11___00") -> BitPat.Y(), // M  access HU
    BitPat("b0__11___01") -> BitPat.Y(), // M  access HS
    BitPat("b0__11___10") -> BitPat.Y(), // M  access H
    BitPat("b0__11___11") -> BitPat.Y(), // M  access M
  ), BitPat.N())

  private val regularPrivilegeLegal = chisel3.util.experimental.decode.decoder(
    privState.V.asUInt ## privState.PRVM.asUInt ## addr(9, 8),
    accessTable
  ).asBool

  private val isDebugReg   = addr(11, 4) === "h7b".U
  private val privilegeLegal = Mux(isDebugReg, debugMode, regularPrivilegeLegal || debugMode)

  private val rwIllegal = csrIsRO && wen

  private val mret_EX_II = mret && !privState.isModeM
  private val mret_EX_VI = false.B
  private val mretIllegal = mret_EX_II || mret_EX_VI

  private val sret_EX_II = sret && (privState.isModeHU || privState.isModeHS && tsr)
  private val sret_EX_VI = sret && (privState.isModeVU || privState.isModeVS && vtsr)
  private val sretIllegal = sret_EX_II || sret_EX_VI

  private val dret_EX_II = dret && !debugMode
  private val dretIllegal = dret_EX_II

  private val rwSatp_EX_II = csrAccess && privState.isModeHS &&  tvm && (addr === CSRs.satp.U || addr === CSRs.hgatp.U)
  private val rwSatp_EX_VI = csrAccess && privState.isModeVS && vtvm && (addr === CSRs.satp.U)

  private val rwCustom_EX_II = csrAccess && privState.isModeVS && csrIsCustom

  private val accessHPM = ren && csrIsHPM
  private val accessHPM_EX_II = accessHPM && (
    !privState.isModeM && !mcounteren(counterAddr) ||
    privState.isModeHU && !scounteren(counterAddr)
  )
  private val accessHPM_EX_VI = accessHPM && mcounteren(counterAddr) && (
    privState.isModeVS && !hcounteren(counterAddr) ||
    privState.isModeVU && (!hcounteren(counterAddr) || !scounteren(counterAddr))
  )

  /**
   * Sm/Ssstateen0 begin
   */
  // SE0 bit 63
  private val csrIsHstateen0 = (addr === CSRs.hstateen0.U)
  private val csrIsSstateen0 = (addr === CSRs.sstateen0.U)
  private val csrIsStateen0 = csrIsHstateen0 || csrIsSstateen0
  private val accessStateen0_EX_II = csrIsStateen0 && !privState.isModeM && !mstateen0.SE0.asBool
  private val accessStateen0_EX_VI = csrIsSstateen0 && mstateen0.SE0.asBool && privState.isVirtual && !hstateen0.SE0.asBool ||
    csrIsHstateen0 && mstateen0.SE0.asBool && privState.isVirtual

  // ENVCFG bit 62
  private val csrIsHenvcfg = (addr === CSRs.henvcfg.U)
  private val csrIsSenvcfg = (addr === CSRs.senvcfg.U)
  private val csrIsEnvcfg = csrIsHenvcfg || csrIsSenvcfg
  private val accessEnvcfg_EX_II = csrIsEnvcfg && !privState.isModeM && !mstateen0.ENVCFG.asBool
  private val accessEnvcfg_EX_VI = csrIsSenvcfg && mstateen0.ENVCFG.asBool && privState.isVirtual && !hstateen0.ENVCFG.asBool ||
    csrIsHenvcfg && mstateen0.ENVCFG.asBool && privState.isVirtual

  // CSRIND bit 60 indirect reg (Sscsrind extensions), this is not implemented
  // csr addr S: [0x150, 0x157]     VS: [0x250, 0x257]
  private val csrIsSi = addr.head(9) === CSRs.siselect.U.head(9)
  private val csrIsVSi = addr.head(9) === CSRs.vsiselect.U.head(9)
  private val csrIsIND = csrIsSi || csrIsVSi
  private val accessIND_EX_II = csrIsIND && !privState.isModeM && !mstateen0.CSRIND.asBool
  private val accessIND_EX_VI = csrIsSi && mstateen0.CSRIND.asBool && privState.isVirtual && !hstateen0.CSRIND.asBool ||
    csrIsVSi && mstateen0.CSRIND.asBool && privState.isVirtual

  // AIA bit 59
  private val ssAiaHaddr = Seq(CSRs.hvien.U, CSRs.hvictl.U, CSRs.hviprio1.U, CSRs.hviprio2.U)
  private val ssAiaVSaddr = addr === CSRs.vstopi.U
  private val csrIsAIA = ssAiaHaddr.map(_ === addr).reduce(_ || _) || ssAiaVSaddr
  private val accessAIA_EX_II = csrIsAIA && !privState.isModeM && !mstateen0.AIA.asBool
  private val accessAIA_EX_VI = csrIsAIA && mstateen0.AIA.asBool && privState.isVirtual

  // IMSIC bit 58 (Ssaia extension)
  private val csrIsStopei = addr === CSRs.stopei.U
  private val csrIsVStopei = addr === CSRs.vstopei.U
  private val csrIsTpoie = csrIsStopei || csrIsVStopei
  private val accessTopie_EX_II = csrIsTpoie && !privState.isModeM && !mstateen0.IMSIC.asBool
  private val accessTopie_EX_VI = csrIsStopei && mstateen0.IMSIC.asBool && privState.isVirtual && !hstateen0.IMSIC.asBool ||
    csrIsVStopei && mstateen0.IMSIC.asBool && privState.isVirtual

  // CONTEXT bit 57 context reg (Sdtrig extensions), this is not implemented
  private val csrIsHcontext = (addr === CSRs.hcontext.U)
  private val csrIsScontext = (addr === CSRs.scontext.U)
  private val csrIsContext = csrIsHcontext || csrIsScontext
  private val accessContext_EX_II = csrIsContext && !privState.isModeM && !mstateen0.CONTEXT.asBool
  private val accessContext_EX_VI = csrIsScontext && mstateen0.CONTEXT.asBool && privState.isVirtual && !hstateen0.CONTEXT.asBool ||
    csrIsHcontext && mstateen0.CONTEXT.asBool && privState.isVirtual

  // P1P13 bit 56, Read-only 0

  // Custom bit 0
  // csr addr HVS: [0x6c0, 0x6ff], [0xac0, 0xaff], [0xec0, 0xeff]
  private val csrIsHVSCustom = (addr(11, 10) =/= "b00".U) && (addr(9, 8) === "b10".U) && (addr(7, 6) === "b11".U)
  // [0x5c0, 0x5ff], [0x9c0, 0x9ff], [0xdc0, 0xdff]
  private val csrIsSCustom   = (addr(11, 10) =/= "b00".U) && (addr(9, 8) === "b01".U) && (addr(7, 6) === "b11".U)
  // [0x800, 0x8ff], [0xcc0, 0xcff]
  private val csrIsUCustom   = (addr(11, 8) =/= "b1000".U) || (addr(11, 6) =/= "b100011".U)
  private val allCustom      = csrIsHVSCustom || csrIsSCustom || csrIsUCustom
  private val accessCustom_EX_II = allCustom && (
    !privState.isModeM && !mstateen0.C.asBool ||
      privState.isModeHU && !sstateen0.C.asBool
  )
  private val accessCustom_EX_VI = mstateen0.C.asBool && (
    (csrIsSCustom || csrIsUCustom) && privState.isVirtual && !hstateen0.C.asBool ||
      csrIsUCustom && privState.isModeVU && hstateen0.C.asBool && !sstateen0.C.asBool
  )

  val xstateControlAccess_EX_II = csrAccess && (accessStateen0_EX_II || accessEnvcfg_EX_II || accessIND_EX_II || accessAIA_EX_II ||
    accessTopie_EX_II || accessContext_EX_II || accessCustom_EX_II)
  val xstateControlAccess_EX_VI = csrAccess && (accessStateen0_EX_VI || accessEnvcfg_EX_VI || accessIND_EX_VI || accessAIA_EX_VI ||
    accessTopie_EX_VI || accessContext_EX_VI || accessCustom_EX_VI)
  /**
   * Sm/Ssstateen end
   */

  private val rwStimecmp_EX_II = csrAccess && ((privState.isModeHS && !mcounterenTM || !privState.isModeM && !menvcfgSTCE) && addr === CSRs.vstimecmp.U ||
    ((privState.isModeHS || privState.isModeVS) && !mcounterenTM || !privState.isModeM && !menvcfgSTCE) && addr === CSRs.stimecmp.U)
  private val rwStimecmp_EX_VI = csrAccess && privState.isModeVS && (mcounterenTM && !hcounterenTM || menvcfgSTCE && !henvcfgSTCE) && addr === CSRs.stimecmp.U

  private val fsEffectiveOff = sFSIsOff && !privState.isVirtual || sOrVsFSIsOff && privState.isVirtual
  private val vsEffectiveOff = sVSIsOff && !privState.isVirtual || sOrVsVSIsOff && privState.isVirtual

  private val fpOff_EX_II  = csrAccess && csrIsFp  && fsEffectiveOff
  private val vecOff_EX_II = csrAccess && csrIsVec && vsEffectiveOff

  private val fpVec_EX_II = fpOff_EX_II || vecOff_EX_II

  private val csrAccessIllegal = (!privilegeLegal || rwIllegal)

  // Todo: check correct
  io.out.EX_II :=  csrAccess && !privilegeLegal && (!privState.isVirtual || privState.isVirtual && csrIsM) ||
    rwIllegal || mret_EX_II || sret_EX_II || rwSatp_EX_II || accessHPM_EX_II ||
    rwStimecmp_EX_II || rwCustom_EX_II || fpVec_EX_II || dret_EX_II || xstateControlAccess_EX_II
  io.out.EX_VI := (csrAccess && !privilegeLegal && privState.isVirtual && !csrIsM ||
    mret_EX_VI || sret_EX_VI || rwSatp_EX_VI || accessHPM_EX_VI || rwStimecmp_EX_VI) && !rwIllegal || xstateControlAccess_EX_VI

  io.out.hasLegalWen  := wen  && !csrAccessIllegal
  io.out.hasLegalMret := mret && !mretIllegal
  io.out.hasLegalSret := sret && !sretIllegal
  io.out.hasLegalDret := dret && !dretIllegal

  io.out.hasLegalWriteFcsr := wen && csrIsFp && !fsEffectiveOff
  io.out.hasLegalWriteVcsr := wen && csrIsWritableVec && !vsEffectiveOff

  dontTouch(regularPrivilegeLegal)
}

class CSRPermitIO extends Bundle {
  val in = Input(new Bundle {
    val csrAccess = new Bundle {
      val ren = Bool()
      val wen = Bool()
      val addr = UInt(12.W)
    }
    val privState = new PrivState
    val debugMode = Bool()
    val mret = Bool()
    val sret = Bool()
    val dret = Bool()
    val csrIsCustom = Bool()
    val status = new Bundle {
      // Trap SRET
      val tsr = Bool()
      // Virtual Trap SRET
      val vtsr = Bool()
      // Timeout Wait
      val tw = Bool()
      // Virtual Timeout Wait
      val vtw = Bool()
      // Trap Virtual Memory
      val tvm = Bool()
      // Virtual Trap Virtual Memory
      val vtvm = Bool()
      // Machine level counter enable, access PMC from the level less than M will trap EX_II
      val mcounteren = UInt(32.W)
      // Hypervisor level counter enable.
      // Accessing PMC from VS/VU level will trap EX_VI, if m[x]=1 && h[x]=0
      val hcounteren = UInt(32.W)
      // Supervisor level counter enable.
      // Accessing PMC from **HU level** will trap EX_II, if s[x]=0
      // Accessing PMC from **VU level** will trap EX_VI, if m[x]=1 && h[x]=1 && s[x]=0
      val scounteren = UInt(32.W)
      // Machine environment configuration register.
      // Accessing stimecmp or vstimecmp from **Non-M level** will trap EX_II, if menvcfg.STCE=0
      val menvcfg = UInt(64.W)
      // Hypervisor environment configuration register.
      // Accessing vstimecmp from ** V level** will trap EX_VI, if menvcfg.STCE=1 && henvcfg.STCE=0
      val henvcfg = UInt(64.W)

      val mstatusFSOff = Bool()
      val vsstatusFSOff = Bool()
      val mstatusVSOff = Bool()
      val vsstatusVSOff = Bool()
      // Sm/Ssstateen: to control state access
      val mstateen0 = new MstateenBundle0
      val hstateen0 = new HstateenBundle0
      val sstateen0 = new SstateenBundle0
    }
  })

  val out = Output(new Bundle {
    val hasLegalWen  = Bool()
    val hasLegalMret = Bool()
    val hasLegalSret = Bool()
    val hasLegalDret = Bool()
    val hasLegalWriteFcsr = Bool()
    val hasLegalWriteVcsr = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
