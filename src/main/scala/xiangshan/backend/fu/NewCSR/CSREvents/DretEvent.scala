package xiangshan.backend.fu.NewCSR.CSREvents

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRConfig.VaddrMaxWidth
import xiangshan.backend.fu.NewCSR.CSRDefines.{HgatpMode, PrivMode, SatpMode}
import xiangshan.backend.fu.NewCSR._
import xiangshan.AddrTransType


class DretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  val dcsr = ValidIO((new DcsrBundle).addInEvent(_.V, _.PRV))
  val mstatus = ValidIO((new MstatusBundle).addInEvent(_.MPRV))
  val debugMode = ValidIO(Bool())
  val debugIntrEnable = ValidIO(Bool())
  val targetPc = ValidIO(new TargetPCBundle)

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
  val satp = Input(new SatpBundle)
  val vsatp = Input(new SatpBundle)
  val hgatp = Input(new HgatpBundle)
}

class DretEventModule(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(new DretEventInput)
  val out = IO(new DretEventOutput)

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

  out.debugMode.valid       := valid
  out.privState.valid       := valid
  out.dcsr.valid            := valid
  out.mstatus.valid         := valid
  out.debugIntrEnable.valid := valid
  out.targetPc.valid        := valid

  out.privState.bits.PRVM     := in.dcsr.PRV.asUInt
  out.privState.bits.V        := in.dcsr.V
  out.mstatus.bits.MPRV       := Mux(!out.privState.bits.isModeM, 0.U, in.mstatus.MPRV.asUInt)
  out.debugMode.bits          := false.B
  out.debugIntrEnable.bits    := true.B
  out.targetPc.bits.pc        := in.dpc.asUInt
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(in.dpc.asUInt)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(in.dpc.asUInt)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(in.dpc.asUInt)
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
