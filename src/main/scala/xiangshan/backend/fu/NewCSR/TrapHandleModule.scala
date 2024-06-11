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
  private val mideleg = io.in.mideleg.asUInt
  private val hideleg = io.in.hideleg.asUInt
  private val medeleg = io.in.medeleg.asUInt
  private val hedeleg = io.in.hedeleg.asUInt

  private val hasTrap = trapInfo.valid
  private val hasIR = hasTrap && trapInfo.bits.isInterrupt
  private val hasEX = hasTrap && !trapInfo.bits.isInterrupt

  private val exceptionVec = io.in.trapInfo.bits.trapVec
  private val intrVec = io.in.trapInfo.bits.intrVec
  private val hasEXVec = Mux(hasEX, exceptionVec, 0.U)
  private val hasIRVec = Mux(hasIR, intrVec, 0.U)

  private val highestPrioIRVec = Wire(Vec(64, Bool()))
  highestPrioIRVec.zipWithIndex.foreach { case (irq, i) =>
    if (InterruptNO.interruptDefaultPrio.contains(i))
      irq := Cat(InterruptNO.getIRQHigherThan(i).map(num => !hasIRVec(num))).andR && hasIRVec(i)
    else
      irq := false.B
  }

  private val highestPrioEXVec = Wire(Vec(64, Bool()))
  highestPrioEXVec.zipWithIndex.foreach { case (excp, i) =>
    if (ExceptionNO.priorities.contains(i)) {
      excp := Cat(ExceptionNO.getHigherExcpThan(i).map(num => !hasEXVec(num))).andR && hasEXVec(i)
    } else
      excp := false.B
  }

  private val highestPrioIR = highestPrioIRVec.asUInt
  private val highestPrioEX = highestPrioEXVec.asUInt

  private val mIRVec  = highestPrioIR
  private val hsIRVec = highestPrioIR & mideleg
  private val vsIRVec = highestPrioIR & mideleg & hideleg

  private val mEXVec  = highestPrioEX
  private val hsEXVec = highestPrioEX & medeleg
  private val vsEXVec = highestPrioEX & medeleg & hedeleg

  private val  mHasIR =  mIRVec.orR
  private val hsHasIR = hsIRVec.orR
  private val vsHasIR = vsIRVec.orR

  private val  mHasEX =  mEXVec.orR
  private val hsHasEX = hsEXVec.orR
  private val vsHasEX = vsEXVec.orR

  private val  mHasTrap =  mHasEX ||  mHasIR
  private val hsHasTrap = hsHasEX || hsHasIR
  private val vsHasTrap = vsHasEX || vsHasIR

  private val handleTrapUnderHS = !privState.isModeM && hsHasTrap
  private val handleTrapUnderVS = privState.isVirtual && vsHasTrap

  // Todo: support more interrupt and exception
  private val exceptionRegular = OHToUInt(highestPrioEX)
  private val interruptNO = OHToUInt(highestPrioIR)
  private val exceptionNO = Mux(trapInfo.bits.singleStep || trapInfo.bits.triggerFire, ExceptionNO.breakPoint.U, exceptionRegular)

  private val causeNO = Mux(hasIR, interruptNO, exceptionNO)

  private val xtvec = MuxCase(io.in.mtvec, Seq(
    handleTrapUnderVS -> io.in.vstvec,
    handleTrapUnderHS -> io.in.stvec
  ))
  private val pcFromXtvec = Cat(xtvec.addr.asUInt + Mux(xtvec.mode === XtvecMode.Vectored && hasIR, interruptNO(5, 0), 0.U), 0.U(2.W))

  io.out.entryPrivState := MuxCase(default = PrivState.ModeM, mapping = Seq(
    handleTrapUnderVS -> PrivState.ModeVS,
    handleTrapUnderHS -> PrivState.ModeHS,
  ))

  io.out.causeNO.Interrupt := hasIR
  io.out.causeNO.ExceptionCode := causeNO
  io.out.pcFromXtvec := pcFromXtvec
}

class TrapHandleIO extends Bundle {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val intrVec = UInt(64.W)
      val isInterrupt = Bool()
      val singleStep = Bool()
      val triggerFire = Bool()
    })
    val privState = new PrivState
    val mideleg = new MidelegBundle
    val medeleg = new MedelegBundle
    val hideleg = new HidelegBundle
    val hedeleg = new HedelegBundle
    // trap vector
    val mtvec = Input(new XtvecBundle)
    val stvec = Input(new XtvecBundle)
    val vstvec = Input(new XtvecBundle)
  })

  val out = new Bundle {
    val entryPrivState = new PrivState
    val causeNO = new CauseBundle
    val pcFromXtvec = UInt()
  }
}