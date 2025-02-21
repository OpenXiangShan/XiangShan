package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.{HgatpMode, SatpMode}
import xiangshan.backend.fu.NewCSR._
import xiangshan.AddrTransType


class TrapEntryVSEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  val vsstatus = ValidIO((new SstatusBundle ).addInEvent(_.SPP, _.SPIE, _.SIE, _.SDT))
  val vsepc    = ValidIO((new Epc           ).addInEvent(_.epc))
  val vscause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val vstval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val targetPc = ValidIO(new TargetPCBundle)
}

class TrapEntryVSEventModule(implicit val p: Parameters) extends Module with CSREventBase {
  val in = IO(new TrapEntryEventInput)
  val out = IO(new TrapEntryVSEventOutput)

  when (valid) {
    assert(in.privState.isVirtual, "The mode must be VU or VS when entry VS mode")
  }

  private val current = in
  private val iMode = current.iMode
  private val dMode = current.dMode
  private val satp = current.satp
  private val vsatp = current.vsatp
  private val hgatp = current.hgatp

  private val trapCode = in.causeNO.ExceptionCode.asUInt
  private val isException = !in.causeNO.Interrupt.asBool
  private val isInterrupt = in.causeNO.Interrupt.asBool
  private val virtualInterruptIsHvictlInject = in.virtualInterruptIsHvictlInject
  private val hvictlIID = in.hvictlIID

  when(valid && isInterrupt && !virtualInterruptIsHvictlInject) {
    assert(
      (InterruptNO.getVS ++ InterruptNO.getLocal).map(_.U === trapCode).reduce(_ || _),
      "The VS mode can only handle VSEI, VSTI, VSSI and local interrupts"
    )
  }

  private val highPrioTrapNO = Mux(
    InterruptNO.getVS.map(_.U === trapCode).reduce(_ || _) && isInterrupt,
    trapCode - 1.U, // map VSSIP, VSTIP, VSEIP to SSIP, STIP, SEIP
    trapCode,
  )

  private val trapPC = genTrapVA(
    iMode,
    satp,
    vsatp,
    hgatp,
    in.trapPc,
  )

  private val trapMemVA = in.memExceptionVAddr

  private val trapMemGPA = in.memExceptionGPAddr

  private val trapInst = Mux(in.trapInst.valid, in.trapInst.bits, 0.U)

  private val fetchIsVirt = current.iMode.isVirtual
  private val memIsVirt   = current.dMode.isVirtual

  private val isFetchExcp    = isException && Seq(/*EX_IAM, */ EX_IAF, EX_IPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && Seq(EX_LAM, EX_LAF, EX_SAM, EX_SAF, EX_LPF, EX_SPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && EX_BP.U === highPrioTrapNO
  private val isFetchBkpt    = isBpExcp && in.isFetchBkpt
  private val isMemBkpt      = isBpExcp && !in.isFetchBkpt
  private val fetchCrossPage = in.isCrossPageIPF
  private val isFetchMalAddr = in.isFetchMalAddr
  private val isFetchMalAddrExcp = isException && isFetchMalAddr
  private val isIllegalInst  = isException && (EX_II.U === highPrioTrapNO || EX_VI.U === highPrioTrapNO)

  // Software breakpoint exceptions are permitted to write either 0 or the pc to xtval
  // We fill pc here
  private val tvalFillPc       = isFetchExcp && !fetchCrossPage || isFetchBkpt
  private val tvalFillPcPlus2  = isFetchExcp && fetchCrossPage
  private val tvalFillMemVaddr = isMemExcp || isMemBkpt
  private val tvalFillGVA      =
    (isFetchExcp || isFetchBkpt) && fetchIsVirt ||
    (isMemExcp || isMemBkpt) && memIsVirt
  private val tvalFillInst     = isIllegalInst

  private val tval = Mux1H(Seq(
    tvalFillPc       -> trapPC,
    tvalFillPcPlus2  -> (trapPC + 2.U),
    tvalFillMemVaddr -> trapMemVA,
    tvalFillInst     -> trapInst,
  ))

  private val instrAddrTransType = AddrTransType(
    bare = vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Bare,
    sv39 = vsatp.MODE === SatpMode.Sv39,
    sv48 = vsatp.MODE === SatpMode.Sv48,
    sv39x4 = vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4,
    sv48x4 = vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv48x4
  )

  out := DontCare

  out.privState.valid := valid

  out.vsstatus .valid := valid
  out.vsepc    .valid := valid
  out.vscause  .valid := valid
  out.vstval   .valid := valid
  out.targetPc .valid := valid

  out.privState.bits             := PrivState.ModeVS
  // vsstatus
  out.vsstatus.bits.SPP          := current.privState.PRVM.asUInt(0, 0) // SPP is not PrivMode enum type, so asUInt and shrink the width
  out.vsstatus.bits.SPIE         := current.vsstatus.SIE
  out.vsstatus.bits.SIE          := 0.U
  out.vsstatus.bits.SDT          := in.henvcfg.DTE.asBool // when DTE open set SDT to 1, else SDT is readonly 0
  // SPVP is not PrivMode enum type, so asUInt and shrink the width
  out.vsepc.bits.epc             := Mux(isFetchMalAddr, in.fetchMalTval(63, 1), trapPC(63, 1))
  out.vscause.bits.Interrupt     := isInterrupt
  out.vscause.bits.ExceptionCode := Mux(virtualInterruptIsHvictlInject, hvictlIID, highPrioTrapNO)
  out.vstval.bits.ALL            := Mux(isFetchMalAddrExcp, in.fetchMalTval, tval)
  out.targetPc.bits.pc           := in.pcFromXtvec
  out.targetPc.bits.raiseIPF     := instrAddrTransType.checkPageFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIAF     := instrAddrTransType.checkAccessFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIGPF    := instrAddrTransType.checkGuestPageFault(in.pcFromXtvec)

  dontTouch(tvalFillGVA)
}

trait TrapEntryVSEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val trapToVS = IO(Flipped(new TrapEntryVSEventOutput))

  addUpdateBundleInCSREnumType(trapToVS.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}
