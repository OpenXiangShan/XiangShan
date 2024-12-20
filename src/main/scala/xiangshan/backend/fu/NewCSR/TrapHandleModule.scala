package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState, XtvecBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.XtvecMode
import xiangshan.backend.fu.NewCSR.CSRBundleImplicitCast._


class TrapHandleModule extends Module {
  val io = IO(new TrapHandleIO)

  private val trapInfo = io.in.trapInfo
  private val privState = io.in.privState
  private val mstatus  = io.in.mstatus
  private val vsstatus = io.in.vsstatus
  private val mnstatus = io.in.mnstatus
  private val mideleg = io.in.mideleg.asUInt
  private val hideleg = io.in.hideleg.asUInt
  private val medeleg = io.in.medeleg.asUInt
  private val hedeleg = io.in.hedeleg.asUInt
  private val mvien = io.in.mvien.asUInt
  private val hvien = io.in.hvien.asUInt
  private val virtualInterruptIsHvictlInject = io.in.virtualInterruptIsHvictlInject

  private val hasTrap = trapInfo.valid
  private val hasNMI = hasTrap && trapInfo.bits.nmi
  private val hasIR = hasTrap && trapInfo.bits.isInterrupt
  private val hasEX = hasTrap && !trapInfo.bits.isInterrupt

  private val exceptionVec = io.in.trapInfo.bits.trapVec
  private val intrVec = io.in.trapInfo.bits.intrVec
  private val hasEXVec = Mux(hasEX, exceptionVec, 0.U)
  private val hasIRVec = Mux(hasIR, intrVec, 0.U)

  private val irToHS = io.in.trapInfo.bits.irToHS
  private val irToVS = io.in.trapInfo.bits.irToVS

  private val highestPrioEXVec = Wire(Vec(64, Bool()))
  highestPrioEXVec.zipWithIndex.foreach { case (excp, i) =>
    if (ExceptionNO.priorities.contains(i)) {
      val higherEXSeq = ExceptionNO.getHigherExcpThan(i)
      excp := (
        higherEXSeq.nonEmpty.B && Cat(higherEXSeq.map(num => !hasEXVec(num))).andR ||
        higherEXSeq.isEmpty.B
      ) && hasEXVec(i)
    } else
      excp := false.B
  }

  private val highestPrioIR  = hasIRVec.asUInt
  private val highestPrioEX  = highestPrioEXVec.asUInt

  private val mEXVec  = highestPrioEX
  private val hsEXVec = highestPrioEX & medeleg
  private val vsEXVec = highestPrioEX & medeleg & hedeleg

  // nmi handle in MMode only and default handler is mtvec
  private val  mHasIR = hasIR
  private val hsHasIR = hasIR && irToHS & !hasNMI
  private val vsHasIR = hasIR && irToVS & !hasNMI

  private val  mHasEX =  mEXVec.orR
  private val hsHasEX = hsEXVec.orR
  private val vsHasEX = vsEXVec.orR

  private val  mHasTrap =  mHasEX ||  mHasIR
  private val hsHasTrap = hsHasEX || hsHasIR
  private val vsHasTrap = vsHasEX || vsHasIR

  private val handleTrapUnderHS = !privState.isModeM && hsHasTrap
  private val handleTrapUnderVS = privState.isVirtual && vsHasTrap
  private val handleTrapUnderM = !handleTrapUnderVS && !handleTrapUnderHS

  // Todo: support more interrupt and exception
  private val exceptionRegular = OHToUInt(highestPrioEX)
  private val interruptNO = highestPrioIR
  private val exceptionNO = Mux(trapInfo.bits.singleStep, ExceptionNO.breakPoint.U, exceptionRegular)

  private val causeNO = Mux(hasIR, interruptNO, exceptionNO)

  // sm/ssdbltrp
  private val m_EX_DT  = handleTrapUnderM  && mstatus.MDT.asBool  && hasTrap
  private val s_EX_DT  = handleTrapUnderHS && mstatus.SDT.asBool  && hasTrap
  private val vs_EX_DT = handleTrapUnderVS && vsstatus.SDT.asBool && hasTrap

  private val dbltrpToMN = m_EX_DT && mnstatus.NMIE.asBool // NMI not allow double trap
  private val hasDTExcp  = m_EX_DT || s_EX_DT || vs_EX_DT

  private val trapToHS = handleTrapUnderHS && !s_EX_DT && !vs_EX_DT
  private val traptoVS = handleTrapUnderVS && !vs_EX_DT

  private val xtvec = MuxCase(io.in.mtvec, Seq(
    traptoVS -> io.in.vstvec,
    trapToHS -> io.in.stvec
  ))
  private val adjustinterruptNO = Mux(
    InterruptNO.getVS.map(_.U === interruptNO).reduce(_ || _) && vsHasIR,
    interruptNO - 1.U, // map VSSIP, VSTIP, VSEIP to SSIP, STIP, SEIP
    interruptNO,
  )
  private val pcFromXtvec = Cat(xtvec.addr.asUInt + Mux(xtvec.mode === XtvecMode.Vectored && hasIR, adjustinterruptNO(5, 0), 0.U), 0.U(2.W))

  io.out.entryPrivState := MuxCase(default = PrivState.ModeM, mapping = Seq(
    traptoVS -> PrivState.ModeVS,
    trapToHS -> PrivState.ModeHS,
  ))

  io.out.causeNO.Interrupt := hasIR
  io.out.causeNO.ExceptionCode := causeNO
  io.out.pcFromXtvec := pcFromXtvec
  io.out.hasDTExcp := hasDTExcp
  io.out.dbltrpToMN := dbltrpToMN

}

class TrapHandleIO extends Bundle {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val nmi = Bool()
      val intrVec = UInt(8.W)
      val isInterrupt = Bool()
      val singleStep = Bool()
      // trap to x mode
      val irToHS = Bool()
      val irToVS = Bool()
    })
    val privState = new PrivState
    val mstatus = new MstatusBundle
    val vsstatus = new SstatusBundle
    val mnstatus = new MnstatusBundle
    val mideleg = new MidelegBundle
    val medeleg = new MedelegBundle
    val hideleg = new HidelegBundle
    val hedeleg = new HedelegBundle
    val mvien = new MvienBundle
    val hvien = new HvienBundle
    // trap vector
    val mtvec = Input(new XtvecBundle)
    val stvec = Input(new XtvecBundle)
    val vstvec = Input(new XtvecBundle)
    // virtual interrupt is hvictl inject
    val virtualInterruptIsHvictlInject = Input(Bool())
  })

  val out = new Bundle {
    val entryPrivState = new PrivState
    val causeNO = new CauseBundle
    val dbltrpToMN = Bool()
    val hasDTExcp = Bool()
    val pcFromXtvec = UInt()
  }
}