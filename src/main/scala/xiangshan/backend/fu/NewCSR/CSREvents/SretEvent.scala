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


class SretEventOutput extends Bundle with EventUpdatePrivStateOutput with EventOutputBase {
  // Todo: write sstatus instead of mstatus
  val mstatus = ValidIO((new MstatusBundle).addInEvent(_.SIE, _.SPIE, _.SPP, _.MPRV, _.MDT, _.SDT))
  val hstatus = ValidIO((new HstatusBundle).addInEvent(_.SPV))
  val vsstatus = ValidIO((new SstatusBundle).addInEvent(_.SIE, _.SPIE, _.SPP, _.SDT))
  val targetPc = ValidIO(new TargetPCBundle)
}

class SretEventInput extends Bundle {
  val privState = Input(new PrivState)
  val mstatus   = Input(new MstatusBundle)
  val hstatus   = Input(new HstatusBundle)
  val vsstatus  = Input(new SstatusBundle)
  val sepc      = Input(new Epc())
  val vsepc     = Input(new Epc())
  val satp      = Input(new SatpBundle)
  val vsatp     = Input(new SatpBundle)
  val hgatp     = Input(new HgatpBundle)
}

class SretEventModule(implicit p: Parameters) extends Module with CSREventBase {
  val in = IO(new SretEventInput)
  val out = IO(new SretEventOutput)

  private val satp = in.satp
  private val vsatp = in.vsatp
  private val hgatp = in.hgatp
  private val nextPrivState = out.privState.bits

  private val instrAddrTransType = AddrTransType(
    bare = (!nextPrivState.isVirtual && satp.MODE === SatpMode.Bare) ||
           (nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Bare),
    sv39 = !nextPrivState.isVirtual && satp.MODE === SatpMode.Sv39 ||
           nextPrivState.isVirtual && vsatp.MODE === SatpMode.Sv39,
    sv48 = !nextPrivState.isVirtual && satp.MODE === SatpMode.Sv48 ||
           nextPrivState.isVirtual && vsatp.MODE === SatpMode.Sv48,
    sv39x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv39x4,
    sv48x4 = nextPrivState.isVirtual && vsatp.MODE === SatpMode.Bare && hgatp.MODE === HgatpMode.Sv48x4
  )

  private val sretInM     = in.privState.isModeM
  private val sretInHS    = in.privState.isModeHS
  private val sretInHSorM = sretInM || sretInHS
  private val sretInVS    = in.privState.isModeVS

  private val xepc = Mux1H(Seq(
    sretInHSorM -> in.sepc,
    sretInVS    -> in.vsepc,
  )).asUInt


  private val outPrivState   = Wire(new PrivState)
  outPrivState.PRVM := Mux1H(Seq(
    // SPP is not PrivMode enum type, so asUInt
    sretInHSorM -> in.mstatus.SPP.asUInt,
    sretInVS    -> in.vsstatus.SPP.asUInt,
  ))
  outPrivState.V := Mux1H(Seq(
    sretInHSorM -> in.hstatus.SPV,
    sretInVS    -> in.privState.V, // keep
  ))

  private val sretToVU    = outPrivState.isModeVU
  private val sretToVS    = outPrivState.isModeVS
  private val sretToU     = outPrivState.isModeHU

  out := DontCare

  out.privState.valid := valid
  out.targetPc .valid := valid

  out.privState.bits      := outPrivState

  // hstatus
  out.hstatus.valid           := valid && sretInHSorM
  out.hstatus.bits.SPV        := VirtMode.Off

  // sstatus
  out.mstatus.valid           := valid && sretInHSorM
  out.mstatus.bits.SPP        := PrivMode.U.asUInt(0, 0) // SPP is not PrivMode enum type, so asUInt and shrink the width
  out.mstatus.bits.SIE        := in.mstatus.SPIE
  out.mstatus.bits.SPIE       := 1.U
  out.mstatus.bits.MPRV       := 0.U // sret will always leave M mode
  out.mstatus.bits.MDT        := Mux(sretInM, 0.U, in.mstatus.MDT.asBool) // when execute return in M mode, set MDT 0
  out.mstatus.bits.SDT        := MuxCase(in.mstatus.SDT.asBool, Seq(
    sretInHS   -> 0.U, // sret will alway leave M mode
    (sretInM && (sretToU || sretToVS || sretToVU))    -> 0.U
  ))


  // vsstatus
  out.vsstatus.valid          := valid && sretInVS
  out.vsstatus.bits.SPP       := PrivMode.U.asUInt(0, 0) // SPP is not PrivMode enum type, so asUInt and shrink the width
  out.vsstatus.bits.SIE       := in.vsstatus.SPIE
  out.vsstatus.bits.SPIE      := 1.U
  out.vsstatus.bits.SDT       := Mux(sretToVU || sretInVS, 0.U, in.vsstatus.SDT.asBool) // clear SDT when return to VU or sret in vs

  out.targetPc.bits.pc        := xepc
  out.targetPc.bits.raiseIPF  := instrAddrTransType.checkPageFault(xepc)
  out.targetPc.bits.raiseIAF  := instrAddrTransType.checkAccessFault(xepc)
  out.targetPc.bits.raiseIGPF := instrAddrTransType.checkGuestPageFault(xepc)

  // for better verilog
  dontTouch(sretInHSorM)
  dontTouch(sretInVS)
}

trait SretEventSinkBundle extends EventSinkBundle { self: CSRModule[_ <: CSRBundle] =>
  val retFromS = IO(Flipped(new SretEventOutput))

  addUpdateBundleInCSREnumType(retFromS.getBundleByName(self.modName.toLowerCase()))

  reconnectReg()
}
