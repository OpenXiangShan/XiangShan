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
  val mstatus  = ValidIO((new MstatusBundle).addInEvent(_.MPP, _.MPV, _.MIE, _.MPIE, _.MPRV, _.MDT, _.SDT))
  val vsstatus = ValidIO((new SstatusBundle).addInEvent(_.SDT))
  val targetPc = ValidIO(new TargetPCBundle)
}

class MretEventInput extends Bundle {
  val mstatus  = Input(new MstatusBundle)
  val vsstatus = Input(new SstatusBundle)
  val mepc     = Input(new Epc())
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
    sv48 = !nextPrivState.isModeM && !nextPrivState.isVirtual && satp.MODE === SatpMode.Sv48 ||
           nextPrivState.isVirtual && vsatp.MODE === SatpMode.Sv48,
    sv39x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4,
    sv48x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv48x4
  )
  val outPrivState   = Wire(new PrivState)
  outPrivState.PRVM := in.mstatus.MPP
  outPrivState.V    := Mux(in.mstatus.MPP === PrivMode.M, VirtMode.Off.asUInt, in.mstatus.MPV.asUInt)

  val mretToM  = outPrivState.isModeM
  val mretToS  = outPrivState.isModeHS
  val mretToVu = outPrivState.isModeVU

  out := DontCare

  out.privState.valid := valid
  out.mstatus  .valid := valid
  out.targetPc .valid := valid

  out.privState.bits          := outPrivState
  out.mstatus.bits.MPP        := PrivMode.U
  out.mstatus.bits.MPV        := VirtMode.Off.asUInt
  out.mstatus.bits.MIE        := in.mstatus.MPIE
  out.mstatus.bits.MPIE       := 1.U
  out.mstatus.bits.MPRV       := Mux(in.mstatus.MPP =/= PrivMode.M, 0.U, in.mstatus.MPRV.asUInt)
  // clear MDT when return mret always execute in M mode
  out.mstatus.bits.MDT    := 0.U
  // clear sstatus.SDT when return mode below M and HS
  out.mstatus.bits.SDT    := Mux(mretToM || mretToS, in.mstatus.SDT.asBool, 0.U)
  // clear vsstatus.SDT when return to VU
  out.vsstatus.bits.SDT   := Mux(mretToVu, 0.U, in.vsstatus.SDT.asBool)

  out.targetPc.bits.pc        := in.mepc.asUInt
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(in.mepc.asUInt)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(in.mepc.asUInt)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(in.mepc.asUInt)
}

trait MretEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val retFromM = IO(Flipped(new MretEventOutput))

  addUpdateBundleInCSREnumType(retFromM.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}
