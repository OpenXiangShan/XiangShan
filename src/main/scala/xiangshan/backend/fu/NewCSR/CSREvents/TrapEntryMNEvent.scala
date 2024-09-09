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

class TrapEntryMNEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase  {
  val mnstatus = ValidIO((new MnstatusBundle ).addInEvent(_.MNPP, _.MNPV, _.NMIE))
  val mnepc    = ValidIO((new Epc           ).addInEvent(_.epc))
  val mncause  = ValidIO((new CauseBundle   ).addInEvent(_.Interrupt, _.ExceptionCode))
  val targetPc = ValidIO(new TargetPCBundle)

  def getBundleByName(name: String): Valid[CSRBundle] = {
    name match {
      case "mnstatus"  => this.mnstatus
      case "mnepc"     => this.mnepc
      case "mncause"   => this.mncause
    }
  }
}

class TrapEntryMNEventModule(implicit val p: Parameters) extends Module with CSREventBase {
  val in = IO(new TrapEntryEventInput)
  val out = IO(new TrapEntryMNEventOutput)

  private val current = in
  private val iMode = current.iMode
  private val satp  = current.satp
  private val vsatp = current.vsatp
  private val hgatp = current.hgatp

  private val highPrioTrapNO = in.causeNO.ExceptionCode.asUInt
  private val isInterrupt = in.causeNO.Interrupt.asBool

  private val isFetchMalAddr = in.isFetchMalAddr

  private val trapPC = genTrapVA(
    iMode,
    satp,
    vsatp,
    hgatp,
    in.trapPc,
  )
  out := DontCare

  out.privState.valid := valid
  out.mnstatus.valid  := valid
  out.mnepc.valid     := valid
  out.mncause.valid   := valid
  out.targetPc.valid  := valid

  out.privState.bits             := PrivState.ModeM
  out.mnstatus.bits.MNPP         := current.privState.PRVM
  out.mnstatus.bits.MNPV         := current.privState.V
  out.mnstatus.bits.NMIE         := 0.U
  out.mnepc.bits.epc             := Mux(isFetchMalAddr, in.fetchMalTval(63, 1), trapPC(63, 1))
  out.mncause.bits.Interrupt     := isInterrupt
  out.mncause.bits.ExceptionCode := highPrioTrapNO
  out.targetPc.bits.pc           := in.pcFromXtvec
  out.targetPc.bits.raiseIPF     := false.B
  out.targetPc.bits.raiseIAF     := AddrTransType(bare = true).checkAccessFault(in.pcFromXtvec)
  out.targetPc.bits.raiseIGPF    := false.B

}

trait TrapEntryMNEventSinkBundle { self: CSRModule[_] =>
  val trapToMN = IO(Flipped(new TrapEntryMNEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = trapToMN.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
