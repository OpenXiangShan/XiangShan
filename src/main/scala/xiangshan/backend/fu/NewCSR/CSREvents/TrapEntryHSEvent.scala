package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.SatpMode
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.CSRConst


class TrapEntryHSEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {

  // Todo: use sstatus instead of mstatus
  val mstatus = ValidIO((new MstatusBundle ).addInEvent(_.SPP, _.SPIE, _.SIE))
  val hstatus = ValidIO((new HstatusBundle ).addInEvent(_.SPV, _.SPVP, _.GVA))
  val sepc    = ValidIO((new Epc           ).addInEvent(_.ALL))
  val scause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val stval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val htval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val htinst  = ValidIO((new OneFieldBundle).addInEvent(_.ALL))

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "mstatus" => this.mstatus
      case "hstatus" => this.hstatus
      case "sepc"    => this.sepc
      case "scause"  => this.scause
      case "stval"   => this.stval
      case "htval"   => this.htval
      case "htinst"  => this.htinst
    }
  }
}

class TrapEntryHSEventInput extends Bundle {
  val sstatus = Input(new SstatusBundle)
  val hstatus = Input(new HstatusBundle)
  val trapPc = Input(UInt(VaddrWidth.W))
  val privState = Input(new PrivState)
  val isInterrupt = Input(Bool())
  val trapVec = Input(UInt(64.W))
  val isCrossPageIPF = Input(Bool())
  val trapMemVaddr = Input(UInt(VaddrWidth.W))
  val trapMemGPA = Input(UInt(VaddrWidth.W)) // Todo: use guest physical address width
  val trapMemGVA = Input(UInt(VaddrWidth.W))
  // always current privilege
  val iMode = Input(new PrivState())
  // take MRPV into consideration
  val dMode = Input(new PrivState())
  val satp = Input(new SatpBundle)
  val vsatp = Input(new SatpBundle)
}

class TrapEntryHSEventModule extends Module with CSREventBase {
  val in = IO(new TrapEntryHSEventInput)
  val out = IO(new TrapEntryHSEventOutput)

  private val current = in

  private val trapPC = Wire(UInt(XLEN.W))
  private val ivmHS = !current.iMode.isModeHS && current.satp.MODE =/= SatpMode.Bare
  private val ivmVS = !current.iMode.isModeVS && current.vsatp.MODE =/= SatpMode.Bare
  // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
  trapPC := Mux(ivmHS || ivmVS, SignExt(in.trapPc, XLEN), ZeroExt(in.trapPc, XLEN))

  private val isInterrupt = in.isInterrupt
  private val isException = !in.isInterrupt
  private val fetchIsVirt = current.iMode.isVirtual
  private val memIsVirt   = current.dMode.isVirtual

  // Todo: support more interrupt and exception
  private val exceptionNO = ExceptionNO.priorities.foldRight(0.U)((i: Int, sum: UInt) => Mux(in.trapVec(i), i.U, sum))
  private val interruptNO = CSRConst.IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(in.trapVec(i), i.U, sum))

  private val highPrioTrapNO = Mux(isInterrupt, interruptNO, exceptionNO)

  private val isFetchExcp    = isException && Seq(/*EX_IAM, */ EX_IAF, EX_IPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isMemExcp      = isException && Seq(EX_LAM, EX_LAF, EX_SAM, EX_SAF, EX_LPF, EX_SPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  private val isBpExcp       = isException && EX_BP.U === highPrioTrapNO
  private val fetchCrossPage = in.isCrossPageIPF

  private val isGuestExcp    = isException && Seq(EX_IGPF, EX_LGPF, EX_SGPF).map(_.U === highPrioTrapNO).reduce(_ || _)
  // Software breakpoint exceptions are permitted to write either 0 or the pc to xtval
  // We fill pc here
  private val tvalFillPc       = isFetchExcp && !fetchCrossPage || isBpExcp
  private val tvalFillPcPlus2  = isFetchExcp && fetchCrossPage
  private val tvalFillMemVaddr = isMemExcp
  private val tvalFillGVA      = isGuestExcp ||
    (isFetchExcp || isBpExcp) && fetchIsVirt ||
    isMemExcp && memIsVirt

  private val tval = Mux1H(Seq(
    (tvalFillPc                     ) -> trapPC,
    (tvalFillPcPlus2                ) -> (trapPC + 2.U),
    (tvalFillMemVaddr && !memIsVirt ) -> in.trapMemVaddr,
    (tvalFillMemVaddr &&  memIsVirt ) -> in.trapMemGVA,
    (isGuestExcp                    ) -> in.trapMemGVA,
  ))

  private val tval2 = Mux(isGuestExcp, in.trapMemGPA, 0.U)

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.hstatus  .valid := valid
  out.sepc     .valid := valid
  out.scause   .valid := valid
  out.stval    .valid := valid
  out.htval    .valid := valid

  out.privState.bits            := PrivState.ModeM
  // mstatus
  out.mstatus.bits.SPP          := current.privState.PRVM.asUInt(0, 0) // SPP is not PrivMode enum type, so asUInt and shrink the width
  out.mstatus.bits.SPIE         := current.sstatus.SIE
  out.mstatus.bits.SIE          := 0.U
  // hstatus
  out.hstatus.bits.SPV          := current.privState.V
    // SPVP is not PrivMode enum type, so asUInt and shrink the width
  out.hstatus.bits.SPVP         := Mux(!current.privState.isVirtual, in.hstatus.SPVP.asUInt, current.privState.PRVM.asUInt(0, 0))
  out.hstatus.bits.GVA          := tvalFillGVA
  out.sepc.bits.ALL             := in.trapPc(in.trapPc.getWidth - 1, 1)
  out.scause.bits.Interrupt     := in.isInterrupt
  out.scause.bits.ExceptionCode := highPrioTrapNO
  out.stval.bits.ALL            := tval
  out.htval.bits.ALL            := tval2
  out.htinst.bits.ALL           := 0.U

  dontTouch(isGuestExcp)
  dontTouch(tvalFillGVA)
}

trait TrapEntryHSEventSinkBundle { self: CSRModule[_] =>
  val trapToHS = IO(Flipped(new TrapEntryHSEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = trapToHS.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
