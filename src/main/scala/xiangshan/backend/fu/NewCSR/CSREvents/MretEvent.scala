package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.ExceptionNO
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, OneFieldBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig.{VaddrWidth, XLEN}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, SatpMode}
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.CSRConst


class MretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val mstatus = ValidIO((new MstatusBundle).addInEvent(_.MIE, _.MPIE, _.MPRV))
  val targetPc = ValidIO(new Epc().addInEvent(_.ALL))

  override def getBundleByName(name: String): ValidIO[CSRBundle] = {
    name match {
      case "mstatus" => this.mstatus
    }
  }
}

class MretEventInput extends Bundle {
  val mstatus = Input(new MstatusBundle)
  val mepc = Input(new Epc())
}

class MretEventModule extends Module with CSREventBase {
  val in = IO(new MretEventInput)
  val out = IO(new MretEventOutput)

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.targetPc .valid := valid

  out.privState.bits.PRVM := in.mstatus.MPP
  out.privState.bits.V    := in.mstatus.MPV
  out.mstatus.bits.MIE    := in.mstatus.MPIE
  out.mstatus.bits.MPIE   := 1.U
  out.mstatus.bits.MPRV   := Mux(in.mstatus.MPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  out.targetPc.bits       := in.mepc
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
