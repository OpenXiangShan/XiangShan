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


class TrapEntryMEventOutput extends Bundle with EventUpdatePrivStateOutput {

  val mstatus = ValidIO((new MstatusBundle ).addInEvent(_.MPV, _.MPP, _.GVA, _.MPIE, _.MIE))
  val mepc    = ValidIO((new Epc           ).addInEvent(_.ALL))
  val mcause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val mtval   = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtval2  = ValidIO((new OneFieldBundle).addInEvent(_.ALL))
  val mtinst  = ValidIO((new OneFieldBundle).addInEvent(_.ALL))

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "mstatus" => this.mstatus
      case "mepc"    => this.mepc
      case "mcause"  => this.mcause
      case "mtval"   => this.mtval
      case "mtval2"  => this.mtval2
      case "mtinst"  => this.mtinst
    }
  }
}

class TrapEntryMEventInput extends Bundle {
  val mstatus = Input(new MstatusBundle)
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

class TrapEntryMEventModule extends Module {
  val valid = IO(Input(Bool()))
  val in = IO(new TrapEntryMEventInput)
  val out = IO(new TrapEntryMEventOutput)

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
  out.mepc     .valid := valid
  out.mcause   .valid := valid
  out.mtval    .valid := valid
  out.mtval2   .valid := valid

  out.privState.bits            := PrivState.ModeM
  out.mstatus.bits.MPV          := current.privState.V
  out.mstatus.bits.MPP          := current.privState.PRVM
  out.mstatus.bits.GVA          := tvalFillGVA
  out.mstatus.bits.MPIE         := current.mstatus.MIE
  out.mstatus.bits.MIE          := 0.U
  out.mepc.bits.ALL             := in.trapPc(in.trapPc.getWidth - 1, 1)
  out.mcause.bits.Interrupt     := in.isInterrupt
  out.mcause.bits.ExceptionCode := highPrioTrapNO
  out.mtval.bits.ALL            := tval
  out.mtval2.bits.ALL           := tval2
  out.mtinst.bits.ALL           := 0.U

  dontTouch(isGuestExcp)
  dontTouch(tvalFillGVA)
}

trait TrapEntryMEventSinkBundle { self: CSRModule[_] =>
  val trapToM = IO(Flipped(new TrapEntryMEventOutput))

  val updateBundle: ValidIO[CSRBundle] = trapToM.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
