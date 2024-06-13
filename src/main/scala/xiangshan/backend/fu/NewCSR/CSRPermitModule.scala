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

  private val (mret, sret) = (
    io.in.mret,
    io.in.sret,
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

  private val rwStimecmp_EX_II = csrAccess && ((privState.isModeHS && !mcounterenTM || !privState.isModeM && !menvcfgSTCE) && addr === CSRs.vstimecmp.U ||
    ((privState.isModeHS || privState.isModeVS) && !mcounterenTM || !privState.isModeM && !menvcfgSTCE) && addr === CSRs.stimecmp.U)
  private val rwStimecmp_EX_VI = csrAccess && privState.isModeVS && (mcounterenTM && !hcounterenTM || menvcfgSTCE && !henvcfgSTCE) && addr === CSRs.stimecmp.U

  private val fsEffectiveOff = sFSIsOff && !privState.isVirtual || sOrVsFSIsOff && privState.isVirtual
  private val vsEffectiveOff = sVSIsOff && !privState.isVirtual || sOrVsVSIsOff && privState.isVirtual

  private val fpOff_EX_II  = csrAccess && csrIsFp  && fsEffectiveOff
  private val vecOff_EX_II = csrAccess && csrIsVec && vsEffectiveOff

  private val fpVec_EX_II = fpOff_EX_II || vecOff_EX_II

  private val csrAccessIllegal = (!privilegeLegal || rwIllegal)

  io.out.illegal := csrAccess && csrAccessIllegal

  // Todo: check correct
  io.out.EX_II := io.out.illegal && !privState.isVirtual || mret_EX_II || sret_EX_II || rwSatp_EX_II || accessHPM_EX_II || rwStimecmp_EX_II || rwCustom_EX_II || fpVec_EX_II
  io.out.EX_VI := io.out.illegal &&  privState.isVirtual || mret_EX_VI || sret_EX_VI || rwSatp_EX_VI || accessHPM_EX_VI || rwStimecmp_EX_VI

  io.out.hasLegalWen  := wen  && !csrAccessIllegal
  io.out.hasLegalMret := mret && !mretIllegal
  io.out.hasLegalSret := sret && !sretIllegal

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
    }
  })

  val out = Output(new Bundle {
    val hasLegalWen  = Bool()
    val hasLegalMret = Bool()
    val hasLegalSret = Bool()
    val hasLegalWriteFcsr   = Bool()
    val hasLegalWriteVcsr  = Bool()
    // Todo: split illegal into EX_II and EX_VI
    val illegal = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
