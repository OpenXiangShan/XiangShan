package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.SatpMode
import xiangshan.backend.fu.NewCSR._


class TrapEntryVSEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  val vsstatus = ValidIO((new SstatusBundle ).addInEvent(_.SPP, _.SPIE, _.SIE))
  val vsepc    = ValidIO((new Epc           ).addInEvent(_.epc))
  val vscause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val vstval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val targetPc = ValidIO(UInt(VaddrMaxWidth.W))

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "vsstatus" => this.vsstatus
      case "vsepc"    => this.vsepc
      case "vscause"  => this.vscause
      case "vstval"   => this.vstval
    }
  }
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

  when(valid && isInterrupt) {
    assert(
      (InterruptNO.getVS ++ InterruptNO.getHS).map(_.U === trapCode).reduce(_ || _),
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

  private val trapMemVA = genTrapVA(
    dMode,
    satp,
    vsatp,
    hgatp,
    in.memExceptionVAddr,
  )
  private val trapMemGPA = SignExt(in.memExceptionGPAddr, XLEN)

  private val trapInst = Mux(in.trapInst.valid, in.trapInst.bits, 0.U)

  private val fetchIsVirt = current.iMode.isVirtual
  private val memIsVirt   = current.dMode.isVirtual

  private val isFetchExcp    = isException && Seq(/*EX_IAM, */ EX_IAF, EX_IPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && Seq(EX_LAM, EX_LAF, EX_SAM, EX_SAF, EX_LPF, EX_SPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && EX_BP.U === highPrioTrapNO
  private val fetchCrossPage = in.isCrossPageIPF
  private val isIllegalInst  = isException && (EX_II.U === highPrioTrapNO || EX_VI.U === highPrioTrapNO)

  // Software breakpoint exceptions are permitted to write either 0 or the pc to xtval
  // We fill pc here
  private val tvalFillPc       = isFetchExcp && !fetchCrossPage || isBpExcp
  private val tvalFillPcPlus2  = isFetchExcp && fetchCrossPage
  private val tvalFillMemVaddr = isMemExcp
  private val tvalFillGVA      =
    (isFetchExcp || isBpExcp) && fetchIsVirt ||
    isMemExcp && memIsVirt
  private val tvalFillInst     = isIllegalInst

  private val tval = Mux1H(Seq(
    (tvalFillPc                     ) -> trapPC,
    (tvalFillPcPlus2                ) -> (trapPC + 2.U),
    (tvalFillMemVaddr && !memIsVirt ) -> trapMemVA,
    (tvalFillMemVaddr &&  memIsVirt ) -> trapMemVA,
    (tvalFillInst                   ) -> trapInst,
  ))

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
  // SPVP is not PrivMode enum type, so asUInt and shrink the width
  out.vsepc.bits.epc             := trapPC(63, 1)
  out.vscause.bits.Interrupt     := isInterrupt
  out.vscause.bits.ExceptionCode := highPrioTrapNO
  out.vstval.bits.ALL            := tval
  out.targetPc.bits              := in.pcFromXtvec

  dontTouch(tvalFillGVA)
}

trait TrapEntryVSEventSinkBundle { self: CSRModule[_] =>
  val trapToVS = IO(Flipped(new TrapEntryVSEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = trapToVS.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
