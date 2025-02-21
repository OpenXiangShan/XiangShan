package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.SignExt
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR._
import xiangshan.AddrTransType


class TrapEntryMEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  val mstatus   = ValidIO((new MstatusBundle ).addInEvent(_.MPV, _.MPP, _.GVA, _.MPIE, _.MIE, _.MDT))
  val mepc      = ValidIO((new Epc           ).addInEvent(_.epc))
  val mcause    = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val mtval     = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtval2    = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtinst    = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val targetPc  = ValidIO(new TargetPCBundle)
}

class TrapEntryMEventModule(implicit val p: Parameters) extends Module with CSREventBase {
  val in = IO(new TrapEntryEventInput)
  val out = IO(new TrapEntryMEventOutput)

  private val current = in
  private val iMode = current.iMode
  private val dMode = current.dMode
  private val satp  = current.satp
  private val vsatp = current.vsatp
  private val hgatp = current.hgatp
  private val isDTExcp = current.hasDTExcp

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

  private val fetchIsVirt = iMode.isVirtual
  private val memIsVirt   = dMode.isVirtual

  private val isFetchExcp    = isException && ExceptionNO.getFetchFault.map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && (ExceptionNO.getLoadFault ++ ExceptionNO.getStoreFault).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && ExceptionNO.EX_BP.U === highPrioTrapNO
  private val isFetchBkpt = isBpExcp && in.isFetchBkpt
  private val isMemBkpt = isBpExcp && !in.isFetchBkpt
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

  private val precause = Cat(isInterrupt, highPrioTrapNO)

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.mepc     .valid := valid
  out.mcause   .valid := valid
  out.mtval    .valid := valid
  out.mtval2   .valid := valid
  out.mtinst   .valid := valid
  out.targetPc .valid := valid

  out.privState.bits            := PrivState.ModeM
  out.mstatus.bits.MPV          := current.privState.V
  out.mstatus.bits.MPP          := current.privState.PRVM
  out.mstatus.bits.GVA          := tvalFillGVA
  out.mstatus.bits.MPIE         := current.mstatus.MIE
  out.mstatus.bits.MIE          := 0.U
  out.mstatus.bits.MDT          := 1.U
  out.mepc.bits.epc             := Mux(isFetchMalAddr, in.fetchMalTval(63, 1), trapPC(63, 1))
  out.mcause.bits.Interrupt     := isInterrupt
  out.mcause.bits.ExceptionCode := Mux(isDTExcp, ExceptionNO.EX_DT.U, highPrioTrapNO)
  out.mtval.bits.ALL            := Mux(isFetchMalAddrExcp, in.fetchMalTval, tval)
  out.mtval2.bits.ALL           := Mux(isDTExcp, precause, tval2 >> 2)
  out.mtinst.bits.ALL           := Mux(isFetchGuestExcp && in.trapIsForVSnonLeafPTE || isLSGuestExcp && in.memExceptionIsForVSnonLeafPTE, 0x3000.U, 0.U)
  out.targetPc.bits.pc          := in.pcFromXtvec
  out.targetPc.bits.raiseIPF    := false.B
  out.targetPc.bits.raiseIAF    := AddrTransType(bare = true).checkAccessFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIGPF   := false.B

  dontTouch(isLSGuestExcp)
  dontTouch(tvalFillGVA)
}

trait TrapEntryMEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val trapToM = IO(Flipped(new TrapEntryMEventOutput))

  addUpdateBundleInCSREnumType(trapToM.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}
