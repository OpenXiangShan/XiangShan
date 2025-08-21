package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.SatpMode
import xiangshan.backend.fu.NewCSR._
import xiangshan.AddrTransType


class TrapEntryHSEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  // Todo: use sstatus instead of mstatus
  val mstatus = ValidIO((new MstatusBundle ).addInEvent(_.SPP, _.SPIE, _.SIE, _.SDT))
  val hstatus = ValidIO((new HstatusBundle ).addInEvent(_.SPV, _.SPVP, _.GVA))
  val sepc    = ValidIO((new Epc           ).addInEvent(_.epc))
  val scause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val stval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val htval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val htinst  = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val targetPc  = ValidIO(new TargetPCBundle)
}

class TrapEntryHSEventModule(implicit val p: Parameters) extends Module with CSREventBase {
  val in = IO(new TrapEntryEventInput)
  val out = IO(new TrapEntryHSEventOutput)

  private val current = in
  private val iMode = current.iMode
  private val dMode = current.dMode
  private val satp = current.satp
  private val vsatp = current.vsatp
  private val hgatp = current.hgatp

  private val highPrioTrapNO = in.causeNO.ExceptionCode.asUInt
  private val isException = !in.causeNO.Interrupt.asBool
  private val isInterrupt = in.causeNO.Interrupt.asBool

  private val trapPC = genTrapVA(
    iMode,
    satp,
    vsatp,
    hgatp,
    in.trapPc,
  )

  private val trapPCGPA = in.trapPcGPA

  private val trapMemVA = in.memExceptionVAddr

  private val trapMemGPA = in.memExceptionGPAddr

  private val trapInst = Mux(in.trapInst.valid, in.trapInst.bits, 0.U)

  private val fetchIsVirt = current.iMode.isVirtual
  private val memIsVirt   = current.dMode.isVirtual

  private val isFetchExcp    = isException && ExceptionNO.getFetchFault.map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && (ExceptionNO.getLoadFault ++ ExceptionNO.getStoreFault).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && ExceptionNO.EX_BP.U === highPrioTrapNO
  private val isFetchBkpt    = isBpExcp && in.isFetchBkpt
  private val isMemBkpt      = isBpExcp && !in.isFetchBkpt
  private val isHlsExcp      = isException && in.isHls
  private val fetchCrossPage = in.isCrossPageIPF
  private val isFetchMalAddr = in.isFetchMalAddr
  private val isFetchMalAddrExcp = isException && isFetchMalAddr
  private val isIllegalInst  = isException && (ExceptionNO.EX_II.U === highPrioTrapNO || ExceptionNO.EX_VI.U === highPrioTrapNO)

  private val isLSGuestExcp    = isException && ExceptionNO.getLSGuestPageFault.map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isFetchGuestExcp = isException && ExceptionNO.EX_IGPF.U === highPrioTrapNO
  // Software breakpoint exceptions are permitted to write either 0 or the pc to xtval
  // We fill pc here
  private val tvalFillPc       = (isFetchExcp || isFetchGuestExcp) && !fetchCrossPage || isFetchBkpt
  private val tvalFillPcPlus2  = (isFetchExcp || isFetchGuestExcp) && fetchCrossPage
  private val tvalFillMemVaddr = isMemExcp || isMemBkpt
  private val tvalFillGVA      =
    isHlsExcp && isMemExcp ||
    isLSGuestExcp|| isFetchGuestExcp ||
    (isFetchExcp || isFetchBkpt) && fetchIsVirt ||
    (isMemExcp || isMemBkpt) && memIsVirt
  private val tvalFillInst     = isIllegalInst

  private val tval = Mux1H(Seq(
    (tvalFillPc                        ) -> trapPC,
    (tvalFillPcPlus2                   ) -> (trapPC + 2.U),
    (tvalFillMemVaddr || isLSGuestExcp ) -> trapMemVA,
    (tvalFillInst                      ) -> trapInst,
  ))

  private val tval2 = Mux1H(Seq(
    (isFetchGuestExcp && isFetchMalAddr                    ) -> in.fetchMalTval,
    (isFetchGuestExcp && !isFetchMalAddr && !fetchCrossPage) -> trapPCGPA,
    (isFetchGuestExcp && !isFetchMalAddr && fetchCrossPage ) -> (trapPCGPA + 2.U),
    (isLSGuestExcp                                         ) -> trapMemGPA,
  ))

  private val instrAddrTransType = AddrTransType(
    bare = satp.MODE === SatpMode.Bare,
    sv39 = satp.MODE === SatpMode.Sv39,
    sv48 = satp.MODE === SatpMode.Sv48,
    sv39x4 = false.B,
    sv48x4 = false.B
  )

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.hstatus  .valid := valid
  out.sepc     .valid := valid
  out.scause   .valid := valid
  out.stval    .valid := valid
  out.htval    .valid := valid
  out.htinst   .valid := valid
  out.targetPc .valid := valid

  out.privState.bits            := PrivState.ModeHS
  // mstatus
  out.mstatus.bits.SPP          := current.privState.PRVM.asUInt(0, 0) // SPP is not PrivMode enum type, so asUInt and shrink the width
  out.mstatus.bits.SPIE         := current.sstatus.SIE
  out.mstatus.bits.SIE          := 0.U
  out.mstatus.bits.SDT          := in.menvcfg.DTE.asBool // when DTE open set SDT to 1, else SDT is readonly 0
  // hstatus
  out.hstatus.bits.SPV          := current.privState.V
    // SPVP is not PrivMode enum type, so asUInt and shrink the width
  out.hstatus.bits.SPVP         := Mux(!current.privState.isVirtual, in.hstatus.SPVP.asUInt, current.privState.PRVM.asUInt(0, 0))
  out.hstatus.bits.GVA          := tvalFillGVA
  out.sepc.bits.epc             := Mux(isFetchMalAddr, in.fetchMalTval(63, 1), trapPC(63, 1))
  out.scause.bits.Interrupt     := isInterrupt
  out.scause.bits.ExceptionCode := highPrioTrapNO
  out.stval.bits.ALL            := Mux(isFetchMalAddrExcp, in.fetchMalTval, tval)
  out.htval.bits.ALL            := tval2 >> 2
  out.htinst.bits.ALL           := Mux(isFetchGuestExcp && in.trapIsForVSnonLeafPTE || isLSGuestExcp && in.memExceptionIsForVSnonLeafPTE, 0x3000.U, 0.U)
  out.targetPc.bits.pc          := in.pcFromXtvec
  out.targetPc.bits.raiseIPF    := instrAddrTransType.checkPageFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIAF    := instrAddrTransType.checkAccessFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIGPF   := false.B

  dontTouch(isLSGuestExcp)
  dontTouch(tvalFillGVA)
}

trait TrapEntryHSEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val trapToHS = IO(Flipped(new TrapEntryHSEventOutput))

  addUpdateBundleInCSREnumType(trapToHS.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}
