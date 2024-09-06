package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.{HgatpMode, PrivMode, SatpMode, VirtMode}
import xiangshan.backend.fu.NewCSR._
import xiangshan.AddrTransType


class MNretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val mnstatus  = ValidIO((new MnstatusBundle).addInEvent(_.MNPP, _.MNPV, _.NMIE))
  val mstatus   = ValidIO((new MstatusBundle).addInEvent(_.MPRV))
  val targetPc  = ValidIO(new TargetPCBundle)

  override def getBundleByName(name: String): ValidIO[CSRBundle] = {
    name match {
      case "mnstatus"  => this.mnstatus
      case "mstatus"   => this.mstatus
    }
  }
}

class MNretEventInput extends Bundle {
  val mnstatus = Input(new MnstatusBundle)
  val mstatus  = Input(new MstatusBundle)
  val mnepc    = Input(new Epc())
  val satp     = Input(new SatpBundle)
  val vsatp    = Input(new SatpBundle)
  val hgatp    = Input(new HgatpBundle)
}

class MNretEventModule(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(new MNretEventInput)
  val out = IO(new MNretEventOutput)

  private val satp = in.satp
  private val vsatp = in.vsatp
  private val hgatp = in.hgatp
  private val nextPrivState = out.privState.bits

  private val instrAddrTransType = AddrTransType(
    bare = nextPrivState.isModeM ||
           (!nextPrivState.isVirtual && satp.MODE === SatpMode.Bare) ||
           (nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Bare),
    sv39 = !nextPrivState.isModeM && !nextPrivState.isVirtual && satp.MODE === SatpMode.Sv39 ||
           nextPrivState.isVirtual && vsatp.MODE === SatpMode.Sv39,
    sv48 = !nextPrivState.isModeM && !nextPrivState.isVirtual && satp.MODE === SatpMode.Sv48 ||
           nextPrivState.isVirtual && vsatp.MODE === SatpMode.Sv48,
    sv39x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4,
    sv48x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv48x4
  )

  out := DontCare

  out.privState.valid := valid
  out.mnstatus .valid := valid
  out.targetPc .valid := valid

  out.privState.bits.PRVM     := in.mnstatus.MNPP
  out.privState.bits.V        := Mux(in.mnstatus.MNPP === PrivMode.M, VirtMode.Off.asUInt, in.mnstatus.MNPV.asUInt)
  out.mnstatus.bits.MNPP      := PrivMode.U
  out.mnstatus.bits.MNPV      := VirtMode.Off.asUInt
  out.mnstatus.bits.NMIE      := 1.U
  out.mstatus.bits.MPRV       := Mux(in.mnstatus.MNPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  out.targetPc.bits.pc        := in.mnepc.asUInt
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(in.mnepc.asUInt)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(in.mnepc.asUInt)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(in.mnepc.asUInt)
}

trait MNretEventSinkBundle { self: CSRModule[_] =>
  val retFromMN = IO(Flipped(new MNretEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = retFromMN.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }

}