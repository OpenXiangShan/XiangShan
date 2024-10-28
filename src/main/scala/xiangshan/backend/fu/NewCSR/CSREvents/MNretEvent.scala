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
  val mstatus   = ValidIO((new MstatusBundle).addInEvent(_.MPRV, _.MDT, _.SDT))
  val vsstatus  = ValidIO((new SstatusBundle).addInEvent(_.SDT))
  val targetPc  = ValidIO(new TargetPCBundle)
}

class MNretEventInput extends Bundle {
  val mnstatus = Input(new MnstatusBundle)
  val mstatus  = Input(new MstatusBundle)
  val mnepc    = Input(new Epc())
  val satp     = Input(new SatpBundle)
  val vsatp    = Input(new SatpBundle)
  val hgatp    = Input(new HgatpBundle)
  val vsstatus = Input(new SstatusBundle)
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

  val outPrivState   = Wire(new PrivState)
  outPrivState.PRVM := in.mnstatus.MNPP
  outPrivState.V    := Mux(in.mnstatus.MNPP === PrivMode.M, VirtMode.Off.asUInt, in.mnstatus.MNPV.asUInt)

  val mnretToM  = outPrivState.isModeM
  val mnretToVU = outPrivState.isModeVU

  out := DontCare

  out.privState.valid := valid
  out.mnstatus .valid := valid
  out.targetPc .valid := valid

  out.privState.bits          := outPrivState
  out.mnstatus.bits.MNPP      := PrivMode.U
  out.mnstatus.bits.MNPV      := VirtMode.Off.asUInt
  out.mnstatus.bits.NMIE      := 1.U
  out.mstatus.bits.MPRV       := Mux(in.mnstatus.MNPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  // clear MDT when mnret to below M
  out.mstatus.bits.MDT        := Mux(mnretToM, in.mstatus.MDT.asBool, 0.U)
  out.mstatus.bits.SDT        := Mux(mnretToM, in.mstatus.SDT.asBool, 0.U) // ?? return to S clear SDT?
  out.vsstatus.bits.SDT       := Mux(mnretToVU, 0.U, in.vsstatus.SDT.asBool)
  out.targetPc.bits.pc        := in.mnepc.asUInt
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(in.mnepc.asUInt)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(in.mnepc.asUInt)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(in.mnepc.asUInt)
}

trait MNretEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val retFromMN = IO(Flipped(new MNretEventOutput))

  addUpdateBundleInCSREnumType(retFromMN.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}