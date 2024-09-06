package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrMaxWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, SatpMode, VirtMode}
import xiangshan.backend.fu.NewCSR._


class MNretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val mnstatus  = ValidIO((new MnstatusBundle).addInEvent(_.MNPP, _.MNPV, _.NMIE))
  val mstatus   = ValidIO((new MstatusBundle).addInEvent(_.MPRV))
  val targetPc  = ValidIO(UInt(VaddrMaxWidth.W))

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
  val mnepc   = Input(new Epc())
}

class MNretEventModule extends Module with CSREventBase {
  val in = IO(new MNretEventInput)
  val out = IO(new MNretEventOutput)

  out := DontCare

  out.privState.valid := valid
  out.mnstatus .valid := valid
  out.targetPc .valid := valid

  out.privState.bits.PRVM := in.mnstatus.MNPP
  out.privState.bits.V    := Mux(in.mnstatus.MNPP === PrivMode.M, VirtMode.Off.asUInt, in.mnstatus.MNPV.asUInt)
  out.mnstatus.bits.MNPP  := PrivMode.U
  out.mnstatus.bits.MNPV  := VirtMode.Off.asUInt
  out.mnstatus.bits.NMIE  := 1.U
  out.mstatus.bits.MPRV   := Mux(in.mnstatus.MNPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  out.targetPc.bits       := in.mnepc.asUInt
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