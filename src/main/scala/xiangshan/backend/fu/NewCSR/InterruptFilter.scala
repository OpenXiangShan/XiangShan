package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import utility.DelayN
import utils._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState, XtvecBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, XtvecMode}
import xiangshan.backend.fu.util.CSRConst
import xiangshan.backend.fu.NewCSR.InterruptNO


class InterruptFilter extends Module {
  val io = IO(new InterruptFilterIO)

  val privState = io.in.privState
  val mstatusMIE = io.in.mstatusMIE
  val sstatusSIE = io.in.sstatusSIE
  val vsstatusSIE = io.in.vsstatusSIE
  val mip = io.in.mip
  val mie = io.in.mie
  val mideleg = io.in.mideleg
  val sip = io.in.sip
  val sie = io.in.sie
  val hip = io.in.hip
  val hie = io.in.hie
  val hideleg = io.in.hideleg
  val vsip = io.in.vsip
  val vsie = io.in.vsie
  val hvictl = io.in.hvictl
  val hstatus = io.in.hstatus
  val mtopei = io.in.mtopei
  val stopei = io.in.stopei
  val vstopei = io.in.vstopei
  val hviprio1 = io.in.hviprio1
  val hviprio2 = io.in.hviprio2
  val miprios = io.in.miprios
  val hsiprios = io.in.hsiprios
  val hviprios = Cat(hviprio2.asUInt, hviprio1.asUInt)


  /**
   * Sort by implemented interrupt default priority
   * index low, priority high
   */
  val mipFields = mip.asTypeOf(new MipBundle)
  val mieFields = mie.asTypeOf(new MieBundle)
  val sipFields = sip.asTypeOf(new SipBundle)
  val sieFields = sie.asTypeOf(new SieBundle)
  val hipFields = hip.asTypeOf(new HipBundle)
  val hieFields = hie.asTypeOf(new HieBundle)
  val vsipFields = vsip.asTypeOf(new VSipBundle)
  val vsieFields = vsie.asTypeOf(new VSieBundle)
  val hidelegFields = hideleg.asTypeOf(new HidelegBundle)

  private val hsip = hip.asUInt | sip.asUInt
  private val hsie = hie.asUInt | sie.asUInt

  val mtopiIsNotZero: Bool = (mip & mie & (~mideleg).asUInt) =/= 0.U
  val stopiIsNotZero: Bool = privState.isModeHS && ((hsip & hsie & (~hideleg).asUInt) =/= 0.U)

  val mIpriosIsZero : Bool = miprios  === 0.U
  val hsIpriosIsZero: Bool = hsiprios === 0.U

  val mtopigather = mip & mie
  val hstopigather = hsip & hsie
  val vstopigather = vsip & vsie
  val mipriosSort: Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(9.W)))
  val hsipriosSort: Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(9.W)))
  val hvipriosSort: Vec[UInt] = VecInit(Seq.fill(3)(0.U(9.W)))
  InterruptNO.interruptDefaultPrio.zipWithIndex.foreach { case (value, index) =>
    mipriosSort(index) := Mux(mtopigather(value), Cat(1.U, miprios(7 + 8 * value, 8 * value)), 0.U)
    hsipriosSort(index) := Mux(hstopigather(value), Cat(1.U, hsiprios(7 + 8 * value, 8 * value)), 0.U)
  }
  hvipriosSort(0) := Mux(vstopigather(1).asBool, Cat(1.U, hviprios(15, 8)), 0.U)
  hvipriosSort(1) := Mux(vstopigather(5).asBool, Cat(1.U, hviprios(31, 24)), 0.U)
  hvipriosSort(2) := Mux(vstopigather(13).asBool, Cat(1.U, hviprios(47, 40)), 0.U)

  def findNum(input: UInt): UInt = {
    val select = Mux1H(UIntToOH(input), InterruptNO.interruptDefaultPrio.map(_.U))
    select
  }

  def findIndex(input: UInt): UInt = {
    val select = WireInit(0.U(log2Up(InterruptNO.interruptDefaultPrio.length).W))
    InterruptNO.interruptDefaultPrio.zipWithIndex.foreach { case (value, i) =>
      when (input === value.U) {
        select := i.U
      }
    }
    select
  }

  // value lower, priority higher
  def minSelect(index: Vec[UInt], value: Vec[UInt]): (Vec[UInt], Vec[UInt]) = {
    value.size match {
      case 1 =>
        (index, value)
      case 2 =>
        val minIndex = Mux1H(Seq(
          (value(0)(8).asBool && (value(1)(8).asBool && (value(0)(7, 0) < value(1)(7 ,0)) || !value(1)(8).asBool)) -> index(0),
          (value(1)(8).asBool && (value(0)(8).asBool && (value(0)(7, 0) > value(1)(7, 0)) || !value(0)(8).asBool)) -> index(1),
          (value(0)(8).asBool && value(1)(8).asBool && (value(0)(7, 0) === value(1)(7, 0))) -> Mux(index(0) < index(1), index(0), index(1)),
        ))
        val minValue = Mux1H(Seq(
          (value(0)(8).asBool && (value(1)(8).asBool && (value(0)(7, 0) < value(1)(7, 0)) || !value(1)(8).asBool)) -> value(0),
          (value(1)(8).asBool && (value(0)(8).asBool && (value(0)(7, 0) > value(1)(7, 0)) || !value(0)(8).asBool)) -> value(1),
          (value(0)(8).asBool && value(1)(8).asBool && (value(0)(7, 0) === value(1)(7, 0))) -> Mux(index(0) < index(1), value(0), value(1)),
        ))
        (VecInit(minIndex), VecInit(minValue))
      case _ =>
        val (leftIndex,  leftValue)  = minSelect(VecInit(index.take((value.size + 1)/2)), VecInit(value.take((value.size + 1)/2)))
        val (rightIndex, rightValue) = minSelect(VecInit(index.drop((value.size + 1)/2)), VecInit(value.drop((value.size + 1)/2)))
        minSelect(VecInit(leftIndex ++ rightIndex), VecInit(leftValue ++ rightValue))
    }
  }

  def highIprio(iprios: Vec[UInt], vsMode: Boolean = false): (UInt, UInt) = {
    if (vsMode) {
      val index = WireInit(VecInit(Seq.fill(3)(0.U(6.W))))
      for (i <- 0 until 3) {
        index(i) := i.U
      }
      val result = minSelect(index, iprios)
      (result._1(0), result._2(0)(7, 0))
    } else {
      val index = WireInit(VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(6.W))))
      InterruptNO.interruptDefaultPrio.zipWithIndex.foreach { case (prio, i) =>
        index(i) := i.U
      }
      val result = minSelect(index, iprios)
      (result._1(0), result._2(0)(7, 0))
    }
  }

  private val (mIidIdx,  mPrioNum)  = highIprio(mipriosSort)
  private val (hsIidIdx, hsPrioNum) = highIprio(hsipriosSort)

  private val mIidNum  = findNum(mIidIdx)
  private val hsIidNum = findNum(hsIidIdx)

  private val mIidDefaultPrioHighMEI: Bool = mIidIdx < InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U
  private val mIidDefaultPrioLowMEI : Bool = mIidIdx > InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U

  private val hsIidDefaultPrioHighSEI: Bool = hsIidIdx < InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U
  private val hsIidDefaultPrioLowSEI : Bool = hsIidIdx > InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U

  val mtopiPrioNumReal = mPrioNum
  val stopiPrioNumReal = hsPrioNum

  // update mtopi
  io.out.mtopi.IID := Mux(mtopiIsNotZero, mIidNum, 0.U)
  io.out.mtopi.IPRIO := Mux(
    mtopiIsNotZero,
    Mux(
      mIpriosIsZero,
      1.U,
      Mux1H(Seq(
        (mtopiPrioNumReal >= 1.U && mtopiPrioNumReal <= 255.U) -> mtopiPrioNumReal(7, 0),
        ((mtopiPrioNumReal > 255.U) || ((mtopiPrioNumReal === 0.U) && mIidDefaultPrioLowMEI)) -> 255.U,
        ((mtopiPrioNumReal === 0.U) && mIidDefaultPrioHighMEI) -> 0.U,
      ))
    ),
    0.U
  )

  // upadte stopi
  io.out.stopi.IID := Mux(stopiIsNotZero, hsIidNum, 0.U)
  io.out.stopi.IPRIO := Mux(
    stopiIsNotZero,
    Mux(
      hsIpriosIsZero,
      1.U,
      Mux1H(Seq(
        (stopiPrioNumReal >= 1.U && stopiPrioNumReal <= 255.U) -> stopiPrioNumReal(7, 0),
        ((stopiPrioNumReal > 255.U) || ((stopiPrioNumReal === 0.U) && hsIidDefaultPrioLowSEI)) -> 255.U,
        ((stopiPrioNumReal === 0.U) && hsIidDefaultPrioHighSEI) -> 0.U,
      ))
    ),
    0.U
  )

  // refactor this code & has some problem
  val Candidate1: Bool = vsipFields.VSEIP.asBool && vsieFields.VSEIE.asBool && (hstatus.VGEIN.asUInt =/= 0.U) && (vstopei.asUInt =/= 0.U)
  val Candidate2: Bool = vsipFields.VSEIP.asBool && vsieFields.VSEIE.asBool && (hstatus.VGEIN.asUInt === 0.U) && (hvictl.IID.asUInt === 9.U) && (hvictl.IPRIO.asUInt =/= 0.U)
  val Candidate3: Bool = vsipFields.VSEIP.asBool && vsieFields.VSEIE.asBool && !Candidate1 && !Candidate2
  val Candidate4: Bool = (hvictl.VTI.asUInt === 0.U) && (vsie & vsip & "hfffffffffffffdff".U).orR
  val Candidate5: Bool = (hvictl.VTI.asUInt === 1.U) && (hvictl.IID.asUInt =/= 9.U)
  val CandidateNoValid: Bool = !Candidate1 && !Candidate2 && !Candidate3 && !Candidate4 && !Candidate5

  assert(PopCount(Cat(Candidate1, Candidate2, Candidate3)) < 2.U, "Only one Candidate could be select from Candidate1/2/3 in VS-level!")
  assert(PopCount(Cat(Candidate4, Candidate5)) < 2.U, "Only one Candidate could be select from Candidate4/5 in VS-level!")

  val VSIidNumTmp = Wire(UInt(6.W))
  val VSIidNum = Wire(UInt(6.W))
  val VSPrioNum = Wire(UInt(8.W))
  VSIidNumTmp := highIprio(hvipriosSort, vsMode = true)._1
  VSPrioNum := highIprio(hvipriosSort, vsMode = true)._2

  VSIidNum := Mux1H(Seq(
    (VSIidNumTmp === 0.U) -> 1.U,
    (VSIidNumTmp === 1.U) -> 5.U,
    ((VSIidNumTmp =/= 0.U) && (VSIidNumTmp =/= 1.U)) -> (VSIidNumTmp + 11.U),
  ))

  val iidCandidate123   = Wire(UInt(12.W))
  val iidCandidate45    = Wire(UInt(12.W))
  val iprioCandidate123 = Wire(UInt(11.W))
  val iprioCandidate45  = Wire(UInt(11.W))
  iidCandidate123 := InterruptNO.SEI.U
  iprioCandidate123 := Mux1H(Seq(
    Candidate1 -> vstopei.IPRIO.asUInt,
    Candidate2 -> hvictl.IPRIO.asUInt,
    Candidate3 -> 256.U,
  ))
  iidCandidate45 := Mux1H(Seq(
    Candidate4 -> VSIidNum,
    Candidate5 -> hvictl.IID.asUInt,
  ))
  iprioCandidate45 := Mux1H(Seq(
    Candidate4 -> VSPrioNum,
    Candidate5 -> hvictl.IPRIO.asUInt,
  ))

  val Candidate123 = Candidate1 || Candidate2 || Candidate3
  val Candidate45 = Candidate4 || Candidate5

  val Candidate123HighCandidate45 = Mux1H(Seq(
    (Candidate123 && Candidate4)   -> ((iprioCandidate123 < iprioCandidate45) || ((iprioCandidate123 === iprioCandidate45) && (findIndex(iidCandidate123) <= findIndex(iidCandidate45)))),
    (Candidate123 && Candidate5)   -> ((iprioCandidate123 < iprioCandidate45) || ((iprioCandidate123 === iprioCandidate45) && hvictl.DPR.asBool)),
    (Candidate123 && !Candidate45) -> true.B,
  ))
  val Candidate123LowCandidate45 = Mux1H(Seq(
    (Candidate123 && Candidate4)   -> ((iprioCandidate123 > iprioCandidate45) || ((iprioCandidate123 === iprioCandidate45) && (findIndex(iidCandidate123) > findIndex(iidCandidate45)))),
    (Candidate123 && Candidate5)   -> ((iprioCandidate123 > iprioCandidate45) || ((iprioCandidate123 === iprioCandidate45) && !hvictl.DPR.asBool)),
    (!Candidate123 && Candidate45) -> true.B,
  ))

  val iidCandidate = Wire(UInt(12.W))
  val iprioCandidate = Wire(UInt(11.W))
  iidCandidate := Mux1H(Seq(
    Candidate123HighCandidate45 -> iidCandidate123,
    Candidate123LowCandidate45 -> iidCandidate45,
  ))
  iprioCandidate := Mux1H(Seq(
    Candidate123HighCandidate45 -> iprioCandidate123,
    Candidate123LowCandidate45 -> iprioCandidate45,
  ))

  // update vstopi
  io.out.vstopi.IID := Mux(CandidateNoValid, 0.U, iidCandidate)
  io.out.vstopi.IPRIO := Mux1H(Seq(
    CandidateNoValid -> 0.U,
    (iprioCandidate > 255.U) -> 255.U,
    (Candidate123LowCandidate45 && Candidate5 && !hvictl.IPRIOM.asBool) -> 1.U,
    ((Candidate123HighCandidate45 && iprioCandidate <= 255.U) || (Candidate123LowCandidate45 && Candidate4) || (Candidate123LowCandidate45 && Candidate5 && hvictl.IPRIOM.asBool)) -> iprioCandidate(7, 0),
  ))

  val mIRVec = Mux(
    privState.isModeM && mstatusMIE || privState < PrivState.ModeM,
    mip.asUInt & mie.asUInt & (~(mideleg.asUInt)).asUInt,
    0.U
  )

  val hsIRVec = Mux(
    privState.isModeHS && sstatusSIE || privState < PrivState.ModeHS,
    hsip & hsie & (~(hideleg.asUInt)).asUInt,
    0.U
  )

  val vsIRVec = Mux(
    privState.isModeVS && vsstatusSIE || privState < PrivState.ModeVS,
    vsip.asUInt & vsie.asUInt,
    0.U
  )

  val vsMapHostIRVec = Cat((0 until vsIRVec.getWidth).map { num =>
    // 2,6,10
    if (InterruptNO.getVS.contains(num)) {
      // 1,5,9
      val sNum = num - 1
      vsIRVec(sNum)
    }
    // 1,5,9
    else if(InterruptNO.getHS.contains(num)) {
      0.U(1.W)
    }
    else {
      vsIRVec(num)
    }
  }.reverse)

  dontTouch(vsMapHostIRVec)

  // support debug interrupt
  // support smrnmi when NMIE is 0, all interrupt disable
  val disableInterrupt = io.in.debugMode || (io.in.dcsr.STEP.asBool && !io.in.dcsr.STEPIE.asBool) || !io.in.mnstatusNMIE
  val debugInterupt = ((io.in.debugIntr && !io.in.debugMode) << CSRConst.IRQ_DEBUG).asUInt

  val normalIntrVec = mIRVec | hsIRVec | vsMapHostIRVec | debugInterupt
  val intrVec = VecInit(Mux(io.in.nmi, io.in.nmiVec, normalIntrVec).asBools.map(IR => IR && !disableInterrupt)).asUInt
  // delay at least 6 cycles to maintain the atomic of sret/mret
  // 65bit indict current interrupt is NMI
  val intrVecReg = RegInit(0.U(64.W))
  val nmiReg = RegInit(false.B)
  intrVecReg := intrVec
  nmiReg := io.in.nmi
  val delayedIntrVec = DelayN(intrVecReg, 5)
  val delayedNMI = DelayN(nmiReg, 5)

  io.out.interruptVec.valid := delayedIntrVec.orR
  io.out.interruptVec.bits := delayedIntrVec
  io.out.nmi := delayedNMI

  dontTouch(hsip)
  dontTouch(hsie)
  dontTouch(mIRVec)
  dontTouch(hsIRVec)
  dontTouch(vsIRVec)
}

class InterruptFilterIO extends Bundle {
  val in = Input(new Bundle {
    val privState = new PrivState
    val mstatusMIE  = Bool()
    val sstatusSIE  = Bool()
    val vsstatusSIE = Bool()
    val mip = UInt(64.W)
    val mie = UInt(64.W)
    val mideleg = UInt(64.W)
    val sip = UInt(64.W)
    val sie = UInt(64.W)
    val hip = UInt(64.W)
    val hie = UInt(64.W)
    val hideleg = UInt(64.W)
    val vsip = UInt(64.W)
    val vsie = UInt(64.W)
    val hvictl = new HvictlBundle
    val hstatus = new HstatusBundle
    val mtopei = new TopEIBundle
    val stopei = new TopEIBundle
    val vstopei = new TopEIBundle
    val hviprio1 = new Hviprio1Bundle
    val hviprio2 = new Hviprio2Bundle
    val debugIntr = Bool()
    val debugMode = Bool()
    val dcsr = new DcsrBundle

    val miprios = UInt((64*8).W)
    val hsiprios = UInt((64*8).W)
    //smrnmi
    val nmi = Bool()
    val nmiVec = UInt(64.W)
    val mnstatusNMIE = Bool()
  })

  val out = Output(new Bundle {
    val nmi = Bool()
    val interruptVec = ValidIO(UInt(64.W))
    val mtopi  = new TopIBundle
    val stopi  = new TopIBundle
    val vstopi = new TopIBundle
  })
}
