package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.NewCSR.CSRConfig.VaddrMaxWidth
import xiangshan.backend.fu.NewCSR.CSRDefines.PrivMode
import xiangshan.backend.fu.NewCSR._


class DretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val dcsr = ValidIO((new DcsrBundle).addInEvent(_.V, _.PRV))
  val mstatus = ValidIO((new MstatusBundle).addInEvent(_.MPRV))
  val debugMode = ValidIO(Bool())
  val debugIntrEnable = ValidIO(Bool())
  val targetPc = ValidIO(UInt(VaddrMaxWidth.W))

  override def getBundleByName(name: String): ValidIO[CSRBundle] = {
    name match {
      case "dcsr" => this.dcsr
      case "mstatus" => this.mstatus
    }
  }
}

class DretEventInput extends Bundle {
  val dcsr = Input(new DcsrBundle)
  val dpc = Input(new Epc)
  val mstatus = Input(new MstatusBundle)
}

class DretEventModule extends Module with CSREventBase {
  val in = IO(new DretEventInput)
  val out = IO(new DretEventOutput)

  out := DontCare

  out.debugMode.valid       := valid
  out.privState.valid       := valid
  out.dcsr.valid            := valid
  out.mstatus.valid         := valid
  out.debugIntrEnable.valid := valid
  out.targetPc.valid        := valid

  out.privState.bits.PRVM  := in.dcsr.PRV.asUInt
  out.privState.bits.V     := in.dcsr.V
  out.mstatus.bits.MPRV    := Mux(!out.privState.bits.isModeM, 0.U, in.mstatus.MPRV.asUInt)
  out.debugMode.bits       := false.B
  out.debugIntrEnable.bits := true.B
  out.targetPc.bits        := in.dpc.asUInt
}

trait DretEventSinkBundle { self: CSRModule[_] =>
  val retFromD = IO(Flipped(new DretEventOutput))

  private val updateBundle: ValidIO[CSRBundle] = retFromD.getBundleByName(self.modName.toLowerCase())

  (reg.asInstanceOf[CSRBundle].getFields zip updateBundle.bits.getFields).foreach { case(sink, source) =>
    if (updateBundle.bits.eventFields.contains(source)) {
      when(updateBundle.valid) {
        sink := source
      }
    }
  }
}
