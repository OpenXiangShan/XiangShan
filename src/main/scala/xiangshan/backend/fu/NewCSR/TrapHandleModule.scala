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

  private val interruptGroups: Seq[(Seq[Int], String)] = Seq(
    InterruptNO.customHighestGroup    -> "customHighest",
    InterruptNO.localHighGroup        -> "localHigh",
    InterruptNO.customMiddleHighGroup -> "customMiddleHigh",
    InterruptNO.interruptDefaultPrio  -> "privArch",
    InterruptNO.customMiddleLowGroup  -> "customMiddleLow",
    InterruptNO.localLowGroup         -> "localLow",
    InterruptNO.customLowestGroup     -> "customLowest",
  )

  private val filteredIRQs: Seq[UInt] = interruptGroups.map {
    case (irqGroup, name) => (getMaskFromIRQGroup(irqGroup) & hasIRVec).suggestName(s"filteredIRQs_$name")
  }
  private val hasIRQinGroup: Seq[Bool] = interruptGroups.map {
    case (irqGroup, name) => dontTouch(Cat(filterIRQs(irqGroup, hasIRVec)).orR.suggestName(s"hasIRQinGroup_$name"))
  }

  private val highestIRQinGroup: Seq[Vec[Bool]] = interruptGroups zip filteredIRQs map {
    case ((irqGroup: Seq[Int], name), filteredIRQ: UInt) =>
      produceHighIRInGroup(irqGroup, filteredIRQ).suggestName(s"highestIRQinGroup_$name")
  }

  private val highestPrioIRVec: Vec[Bool] = MuxCase(
    0.U.asTypeOf(Vec(64, Bool())),
    hasIRQinGroup zip highestIRQinGroup map{ case (hasIRQ: Bool, highestIRQ: Vec[Bool]) => hasIRQ -> highestIRQ }
  )
  private val highestPrioNMIVec = Wire(Vec(64, Bool()))
  highestPrioNMIVec.zipWithIndex.foreach { case (irq, i) =>
    if (NonMaskableIRNO.interruptDefaultPrio.contains(i)) {
      val higherIRSeq = NonMaskableIRNO.getIRQHigherThan(i)
      irq := (
        higherIRSeq.nonEmpty.B && Cat(higherIRSeq.map(num => !hasIRVec(num))).andR ||
          higherIRSeq.isEmpty.B
        ) && hasIRVec(i)
      dontTouch(irq)
    } else
      irq := false.B
  }

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

  private val highestPrioIR  = highestPrioIRVec.asUInt
  private val highestPrioNMI = highestPrioNMIVec.asUInt
  private val highestPrioEX  = highestPrioEXVec.asUInt


  private val mIRVec  = dontTouch(WireInit(highestPrioIR))
  private val hsIRVec = (mIRVec  & mideleg) | (mIRVec  & mvien & ~mideleg)
  private val vsIRVec = (hsIRVec & hideleg) | (hsIRVec & hvien & ~hideleg)

  private val mEXVec  = highestPrioEX
  private val hsEXVec = highestPrioEX & medeleg
  private val vsEXVec = highestPrioEX & medeleg & hedeleg

  // nmi handle in MMode only and default handler is mtvec
  private val  mHasIR =  mIRVec.orR
  private val hsHasIR = hsIRVec.orR & !hasNMI
  private val vsHasIR = (vsIRVec.orR || hasIR && virtualInterruptIsHvictlInject) & !hasNMI

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
  private val interruptNO = OHToUInt(Mux(hasNMI, highestPrioNMI, highestPrioIR))
  private val exceptionNO = Mux(trapInfo.bits.singleStep, ExceptionNO.breakPoint.U, exceptionRegular)

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

  def filterIRQs(group: Seq[Int], originIRQ: UInt): Seq[Bool] = {
    group.map(irqNum => originIRQ(irqNum))
  }

  def getIRQHigherThanInGroup(group: Seq[Int])(irq: Int): Seq[Int] = {
    val idx = group.indexOf(irq, 0)
    require(idx != -1, s"The irq($irq) does not exists in IntPriority Seq")
    group.slice(0, idx)
  }

  def getMaskFromIRQGroup(group: Seq[Int]): UInt = {
    group.map(irq => BigInt(1) << irq).reduce(_ | _).U
  }

  def produceHighIRInGroup(irqGroup: Seq[Int], filteredIRVec: UInt): Vec[Bool] = {
    val irVec = Wire(Vec(64, Bool()))
    irVec.zipWithIndex.foreach { case (irq, i) =>
      if (irqGroup.contains(i)) {
        val higherIRSeq: Seq[Int] = getIRQHigherThanInGroup(irqGroup)(i)
        irq := (
          higherIRSeq.nonEmpty.B && Cat(higherIRSeq.map(num => !filteredIRVec(num))).andR ||
            higherIRSeq.isEmpty.B
          ) && filteredIRVec(i)
      } else
        irq := false.B
    }
    irVec
  }
}

class TrapHandleIO extends Bundle {
  val in = Input(new Bundle {
    val trapInfo = ValidIO(new Bundle {
      val trapVec = UInt(64.W)
      val nmi = Bool()
      val intrVec = UInt(64.W)
      val isInterrupt = Bool()
      val singleStep = Bool()
    })
    val privState = new PrivState
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
    val pcFromXtvec = UInt()
  }
}