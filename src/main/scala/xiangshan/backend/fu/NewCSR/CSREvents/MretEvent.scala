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


class MretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val mstatus  = ValidIO((new MstatusBundle).addInEvent(_.MPP, _.MPV, _.MIE, _.MPIE, _.MPRV))
  val tcontrol = ValidIO((new TcontrolBundle).addInEvent(_.MTE))
  val targetPc = ValidIO(new TargetPCBundle)

  override def getBundleByName(name: String): ValidIO[CSRBundle] = {
    name match {
      case "mstatus"  => this.mstatus
      case "tcontrol" => this.tcontrol
    }
  }
}

class MretEventInput extends Bundle {
  val mstatus  = Input(new MstatusBundle)
  val mepc     = Input(new Epc())
  val tcontrol = Input(new TcontrolBundle)
  val satp     = Input(new SatpBundle)
  val vsatp    = Input(new SatpBundle)
  val hgatp    = Input(new HgatpBundle)
}

class MretEventModule(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(new MretEventInput)
  val out = IO(new MretEventOutput)

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
    sv39x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4
  )

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.tcontrol .valid := valid
  out.targetPc .valid := valid

  out.privState.bits.PRVM     := in.mstatus.MPP
  out.privState.bits.V        := Mux(in.mstatus.MPP === PrivMode.M, VirtMode.Off.asUInt, in.mstatus.MPV.asUInt)
  out.mstatus.bits.MPP        := PrivMode.U
  out.mstatus.bits.MIE        := in.mstatus.MPIE
  out.mstatus.bits.MPIE       := 1.U
  out.mstatus.bits.MPRV       := Mux(in.mstatus.MPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  out.tcontrol.bits.MTE       := in.tcontrol.MPTE
  out.targetPc.bits.pc        := in.mepc.asUInt
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(in.mepc.asUInt)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(in.mepc.asUInt)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(in.mepc.asUInt)
}

trait MretEventSinkBundle { self: CSRModule[_] =>
  val retFromM = IO(Flipped(new MretEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = retFromM.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case (sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }

}
