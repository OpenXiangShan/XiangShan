package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.{ExceptionVec, TriggerCf}
import CSRConfig._
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState}
import xiangshan.ExceptionNO
import xiangshan.backend.fu.util.CSRConst

class TrapHandleModule extends Module {
  val io = IO(new TrapHandleIO)

  private val trapInfo = io.in.trapInfo
  private val privState = io.in.privState

  private val hasTrap = trapInfo.valid
  private val hasIR = hasTrap && trapInfo.bits.isInterrupt
  private val hasEX = hasTrap && !trapInfo.bits.isInterrupt

  private val trapVec = io.in.trapInfo.bits.trapVec
  private val hasEXVec = Mux(hasEX, trapVec, 0.U)
  private val hasIRVec = Mux(hasIR, trapVec, 0.U)

  // Todo: support more interrupt and exception
  private val exceptionNO = ExceptionNO.priorities.foldRight(0.U)((i: Int, sum: UInt) => Mux(trapVec(i), i.U, sum))
  private val interruptNO = CSRConst.IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(trapVec(i), i.U, sum))

  private val causeNO = Mux(hasIR, interruptNO, exceptionNO)

  private val mdeleg = Mux(hasIR, io.in.mideleg.asUInt, io.in.medeleg.asUInt)
  private val hdeleg = Mux(hasIR, io.in.hideleg.asUInt, io.in.hedeleg.asUInt)

  private val handleTrapUnderHS = mdeleg(causeNO) && privState < PrivState.ModeM
  private val handleTrapUnderVS = mdeleg(causeNO) && hdeleg(causeNO) && privState < PrivState.ModeHS

  io.out.entryPrivState := MuxCase(default = PrivState.ModeM, mapping = Seq(
    handleTrapUnderVS -> PrivState.ModeVS,
    handleTrapUnderHS -> PrivState.ModeHS,
  ))

  io.out.causeNO.Interrupt := hasIR
  io.out.causeNO.ExceptionCode := causeNO
}

class TrapHandleIO extends Bundle {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val isInterrupt = Bool()
    })
    val privState = new PrivState
    val mideleg = new MidelegBundle
    val medeleg = new MedelegBundle
    val hideleg = new HidelegBundle
    val hedeleg = new HedelegBundle
  })

  val out = new Bundle {
    val entryPrivState = new PrivState
    val causeNO = new CauseBundle
  }
}