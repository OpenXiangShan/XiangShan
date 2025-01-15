package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import utility.{DelayN, GatedValidRegNext}
import utils._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState, XtvecBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, XtvecMode}
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
  val vsipFields = Wire(new VSipBundle); vsipFields := vsip
  val vsieFields = Wire(new VSieBundle); vsieFields := vsie

  private val hsip = hip.asUInt | sip.asUInt
  private val hsie = hie.asUInt | sie.asUInt

  val mtopiIsNotZero: Bool = (mip & mie & (~mideleg).asUInt) =/= 0.U
  val stopiIsNotZero: Bool = (hsip & hsie & (~hideleg).asUInt) =/= 0.U

  val mIpriosIsZero : Bool = miprios  === 0.U
  val hsIpriosIsZero: Bool = hsiprios === 0.U

  val mtopigather = mip & mie & (~mideleg).asUInt
  val hstopigather = hsip & hsie & (~hideleg).asUInt
  val vstopigather = vsip & vsie
  val mipriosSort:  Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(9.W)))
  val hsipriosSort: Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(9.W)))
  val hvipriosSort: Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(9.W)))
  val indexSort   : Vec[UInt] = VecInit(Seq.fill(InterruptNO.interruptDefaultPrio.size)(0.U(6.W)))

  InterruptNO.interruptDefaultPrio.zipWithIndex.foreach { case (value, index) =>
    mipriosSort(index)  := Mux(mtopigather(value), Cat(1.U, miprios(7 + 8 * value, 8 * value)), 0.U)
    hsipriosSort(index) := Mux(hstopigather(value), Cat(1.U, hsiprios(7 + 8 * value, 8 * value)), 0.U)
    hvipriosSort(index) := Mux(vstopigather(value), Cat(1.U, 0.U(8.W)), 0.U)
    indexSort(index) := index.U
  }
  hvipriosSort(findIndex(1.U)) := Mux(vstopigather(1).asBool, Cat(1.U, hviprio1.PrioSSI.asUInt), 0.U)
  hvipriosSort(findIndex(5.U)) := Mux(vstopigather(5).asBool, Cat(1.U, hviprio1.PrioSTI.asUInt), 0.U)

  for (i <- 0 to 10) {
    hvipriosSort(findIndex((i+13).U)) := Mux(vstopigather(i+13).asBool, Cat(1.U, hviprios(7 + 8 * (i+5), 8 * (i+5))), 0.U)
  }

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
  def minSelect(index: Vec[UInt], value: Vec[UInt], xei: UInt): (Vec[UInt], Vec[UInt]) = {
    value.size match {
      case 1 =>
        (index, value)
      case 2 =>
        /**
         * default: index(0) priority > index(1) priority
         *
         * AIA Spec table 5.3/5.5
         *
         * xei is InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U for M
         * xei is InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U for S
         *
         * if index(0) enable, index(1) disable:
         *    select index(0)
         * else if index(0) disable, index(1) enable:
         *    select index(1)
         * else if index(0), index(1) all enable:
         *    if index(0), index(1) priority number all 0s:
         *      select index(0)
         *    else if index(0) priority number is 0, index(1) priority number is not 0:
         *      if index(0) <= xei, index(1) > xei:
         *        select index(0)
         *      else:
         *        select index(1)
         *    else if index(0) priority number is not 0, index(1) priority number is 0:
         *      if index(1) <= xei, index(0) > xei:
         *        select index(1)
         *      else:
         *        select index(0)
         *    else if index(0) priority number is not 0, index(1) priority number is not 0:
         *      if value(0) <= value(1):
         *        select index(0)
         *      else:
         *        select index(1)
         */
        val minIndex = Mux1H(Seq(
          ( value(0)(8).asBool && !value(1)(8).asBool) -> index(0),
          (!value(0)(8).asBool &&  value(1)(8).asBool) -> index(1),
          ( value(0)(8).asBool &&  value(1)(8).asBool) -> Mux1H(Seq(
            (!value(0)(7, 0).orR && !value(1)(7, 0).orR) -> index(0),
            (!value(0)(7, 0).orR &&  value(1)(7, 0).orR) -> Mux(index(0) <= xei && index(1) > xei, index(0), index(1)),
            ( value(0)(7, 0).orR && !value(1)(7, 0).orR) -> Mux(index(1) <= xei && index(0) > xei, index(1), index(0)),
            ( value(0)(7, 0).orR &&  value(1)(7, 0).orR) -> Mux(value(0)(7, 0) <= value(1)(7, 0), index(0), index(1)),
          ))
        ))
        val minValue = Mux1H(Seq(
          ( value(0)(8).asBool && !value(1)(8).asBool) -> value(0),
          (!value(0)(8).asBool &&  value(1)(8).asBool) -> value(1),
          ( value(0)(8).asBool &&  value(1)(8).asBool) -> Mux1H(Seq(
            (!value(0)(7, 0).orR && !value(1)(7, 0).orR) -> value(0),
            (!value(0)(7, 0).orR &&  value(1)(7, 0).orR) -> Mux(index(0) <= xei && index(1) > xei, value(0), value(1)),
            ( value(0)(7, 0).orR && !value(1)(7, 0).orR) -> Mux(index(1) <= xei && index(0) > xei, value(1), value(0)),
            ( value(0)(7, 0).orR &&  value(1)(7, 0).orR) -> Mux(value(0)(7, 0) <= value(1)(7, 0), value(0), value(1)),
          ))
        ))
        (VecInit(minIndex), VecInit(minValue))
      case _ =>
        val (leftIndex,  leftValue)  = minSelect(VecInit(index.take((value.size + 1)/2)), VecInit(value.take((value.size + 1)/2)), xei)
        val (rightIndex, rightValue) = minSelect(VecInit(index.drop((value.size + 1)/2)), VecInit(value.drop((value.size + 1)/2)), xei)
        minSelect(VecInit(leftIndex ++ rightIndex), VecInit(leftValue ++ rightValue), xei)
    }
  }

  def highIprio(index: Vec[UInt], iprios: Vec[UInt], xei: UInt = 0.U): (UInt, UInt, UInt) = {
    val result = minSelect(index, iprios, xei)
    (result._1(0), result._2(0)(8), result._2(0)(7, 0))
  }

  private val indexTmp = VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.U(6.W)))))
  (0 until 8).foreach { i =>
    val end = math.min(8*(i+1), InterruptNO.interruptDefaultPrio.size)
    val slice = indexSort.slice(8*i, end).map(_.asUInt)
    val paddingSlice = slice ++ Seq.fill(8 - slice.length)(0.U(6.W))
    indexTmp(i) := VecInit(paddingSlice)
  }

  private val mipriosSortTmp = VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.U(9.W)))))
  (0 until 8).foreach { i =>
    val end = math.min(8*(i+1), InterruptNO.interruptDefaultPrio.size)
    val slice = mipriosSort.slice(8*i, end).map(_.asUInt)
    val paddingSlice = slice ++ Seq.fill(8 - slice.length)(0.U(9.W))
    mipriosSortTmp(i) := VecInit(paddingSlice)
  }

  private val hsipriosSortTmp = VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.U(9.W)))))
  (0 until 8).foreach { i =>
    val end = math.min(8*(i+1), InterruptNO.interruptDefaultPrio.size)
    val slice = hsipriosSort.slice(8 * i, end).map(_.asUInt)
    val paddingSlice = slice ++ Seq.fill(8 - slice.length)(0.U(9.W))
    hsipriosSortTmp(i) := VecInit(paddingSlice)
  }

  private val mIidIdx   = VecInit(Seq.fill(8)(0.U(6.W)))
  private val hsIidIdx  = VecInit(Seq.fill(8)(0.U(6.W)))
  private val mEnable   = VecInit(Seq.fill(8)(0.U(1.W)))
  private val hsEnable  = VecInit(Seq.fill(8)(0.U(1.W)))
  private val mPrioNum  = VecInit(Seq.fill(8)(0.U(8.W)))
  private val hsPrioNum = VecInit(Seq.fill(8)(0.U(8.W)))

  indexTmp.zip(mipriosSortTmp).zipWithIndex.foreach { case ((index, iprios), i) =>
    val (iidTmp, enableTmp, prioTmp) = highIprio(index, iprios, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U)
    mIidIdx(i) := iidTmp
    mEnable(i) := enableTmp
    mPrioNum(i) := prioTmp
  }


  indexTmp.zip(hsipriosSortTmp).zipWithIndex.foreach { case ((index, iprios), i) =>
    val (iidTmp, enableTmp, prioTmp) = highIprio(index, iprios, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U)
    hsIidIdx(i) := iidTmp
    hsEnable(i) := enableTmp
    hsPrioNum(i) := prioTmp
  }

  private val mIndexReg = RegInit(VecInit(Seq.fill(8)(0.U(6.W))))
  (0 until 8).foreach(i => mIndexReg(i) := mIidIdx(i))

  private val hsIndexReg = RegInit(VecInit(Seq.fill(8)(0.U(6.W))))
  (0 until 8).foreach(i => hsIndexReg(i) := hsIidIdx(i))

  private val mipriosSortReg = RegInit(VecInit(Seq.fill(8)(0.U(9.W))))
  (0 until 8).foreach(i => mipriosSortReg(i) := Cat(mEnable(i), mPrioNum(i)))

  private val hsipriosSortReg = RegInit(VecInit(Seq.fill(8)(0.U(9.W))))
  (0 until 8).foreach(i => hsipriosSortReg(i) := Cat(hsEnable(i), hsPrioNum(i)))


  private val (mIidIdxReg, mEnableReg, mPrioNumReg)  = highIprio(mIndexReg, mipriosSortReg, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U)
  private val (hsIidIdxReg, hsEnableReg, hsPrioNumReg) = highIprio(hsIndexReg, hsipriosSortReg, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U)


  private val mIidNum  = findNum(mIidIdxReg)
  private val hsIidNum = findNum(hsIidIdxReg)

  private val mIidDefaultPrioHighMEI: Bool = mIidIdxReg < InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U
  private val mIidDefaultPrioLowMEI : Bool = mIidIdxReg > InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U

  private val hsIidDefaultPrioHighSEI: Bool = hsIidIdxReg < InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U
  private val hsIidDefaultPrioLowSEI : Bool = hsIidIdxReg > InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U

  val mtopiPrioNumReal = mPrioNumReg
  val stopiPrioNumReal = hsPrioNumReg

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
  val Candidate1: Bool = vsip.SEIP && vsie.SEIE && (hstatus.VGEIN.asUInt =/= 0.U) && (vstopei.asUInt =/= 0.U)
  val Candidate2: Bool = vsip.SEIP && vsie.SEIE && (hstatus.VGEIN.asUInt === 0.U) && (hvictl.IID.asUInt === 9.U) && (hvictl.IPRIO.asUInt =/= 0.U)
  val Candidate3: Bool = vsip.SEIP && vsie.SEIE && !Candidate1 && !Candidate2
  val Candidate4: Bool = (hvictl.VTI.asUInt === 0.U) && (vsie & vsip & "hfffffffffffffdff".U).orR
  val Candidate5: Bool = (hvictl.VTI.asUInt === 1.U) && (hvictl.IID.asUInt =/= 9.U)
  val CandidateNoValid: Bool = !Candidate1 && !Candidate2 && !Candidate3 && !Candidate4 && !Candidate5

  assert(PopCount(Cat(Candidate1, Candidate2, Candidate3)) < 2.U, "Only one Candidate could be select from Candidate1/2/3 in VS-level!")
  assert(PopCount(Cat(Candidate4, Candidate5)) < 2.U, "Only one Candidate could be select from Candidate4/5 in VS-level!")

  private val hvipriosSortTmp = VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.U(9.W)))))
  (0 until 8).foreach { i =>
    val end = math.min(8*(i+1), InterruptNO.interruptDefaultPrio.size)
    val slice = hvipriosSort.slice(8*i, end).map(_.asUInt)
    val paddingSlice = slice ++ Seq.fill(8 - slice.length)(0.U(9.W))
    hvipriosSortTmp(i) := VecInit(paddingSlice)
  }

  private val vsIidIdx  = VecInit(Seq.fill(8)(0.U(6.W)))
  private val vsEnable  = VecInit(Seq.fill(8)(0.U(1.W)))
  private val vsPrioNum = VecInit(Seq.fill(8)(0.U(8.W)))

  indexTmp.zip(hvipriosSortTmp).zipWithIndex.foreach { case ((index, iprios), i) =>
    val (iidTmp, enableTmp, prioTmp) = highIprio(index, iprios, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.VSEI).U)
    vsIidIdx(i) := iidTmp
    vsEnable(i) := enableTmp
    vsPrioNum(i) := prioTmp
  }

  private val vsIndexReg = RegInit(VecInit(Seq.fill(8)(0.U(6.W))))
  (0 until 8).foreach(i => vsIndexReg(i) := vsIidIdx(i))

  private val hvipriosSortReg = RegInit(VecInit(Seq.fill(8)(0.U(9.W))))
  (0 until 8).foreach(i => hvipriosSortReg(i) := Cat(vsEnable(i), vsPrioNum(i)))

  private val (vsIidIdxReg, vsEnableReg, vsPrioNumReg) = highIprio(vsIndexReg, hvipriosSortReg, InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.VSEI).U)

  private val vsIidNum = findNum(vsIidIdxReg)


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
    Candidate4 -> vsIidNum,
    Candidate5 -> hvictl.IID.asUInt,
  ))
  iprioCandidate45 := Mux1H(Seq(
    Candidate4 -> vsPrioNumReg,
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

  val mIRVecTmp = Mux(
    privState.isModeM && mstatusMIE || privState < PrivState.ModeM,
    io.out.mtopi.IID.asUInt,
    0.U
  )

  val hsIRVecTmp = Mux(
    privState.isModeHS && sstatusSIE || privState < PrivState.ModeHS,
    io.out.stopi.IID.asUInt,
    0.U
  )

  val vsIRVecTmp = Mux(
    privState.isModeVS && vsstatusSIE || privState < PrivState.ModeVS,
    io.out.vstopi.IID.asUInt,
    0.U
  )

  val mIRNotZero  = mIRVecTmp.orR
  val hsIRNotZero = hsIRVecTmp.orR
  val vsIRNotZero = vsIRVecTmp.orR

  val irToHS = !mIRNotZero && hsIRNotZero
  val irToVS = !mIRNotZero && !hsIRNotZero && vsIRNotZero

  val mIRVec  = Mux(mIRNotZero, mIRVecTmp, 0.U)
  val hsIRVec = Mux(irToHS, hsIRVecTmp, 0.U)
  val vsIRVec = Mux(irToVS, UIntToOH(vsIRVecTmp, 64), 0.U)

  val vsMapHostIRVecTmp = Cat((0 until vsIRVec.getWidth).map { num =>
    // 2,6,10
    if (InterruptNO.getVS.contains(num)) {
      // 1,5,9
      val sNum = num - 1
      vsIRVec(sNum)
    }
    // 1,5,9
    else if (InterruptNO.getHS.contains(num)) {
      0.U(1.W)
    }
    else {
      vsIRVec(num)
    }
  }.reverse)

  val vsMapHostIRVec = OHToUInt(vsMapHostIRVecTmp)

  dontTouch(vsMapHostIRVec)

  val nmiVecTmp = Wire(Vec(64, Bool()))
  nmiVecTmp.zipWithIndex.foreach { case (irq, i) =>
    if (NonMaskableIRNO.interruptDefaultPrio.contains(i)) {
      val higherIRSeq = NonMaskableIRNO.getIRQHigherThan(i)
      irq := (
        higherIRSeq.nonEmpty.B && Cat(higherIRSeq.map(num => !io.in.nmiVec(num))).andR ||
          higherIRSeq.isEmpty.B
        ) && io.in.nmiVec(i)
      dontTouch(irq)
    } else
      irq := false.B
  }
  val nmiVec = OHToUInt(nmiVecTmp)

  // support debug interrupt
  // support smrnmi when NMIE is 0, all interrupt disable
  val disableDebugIntr = io.in.debugMode || (io.in.dcsr.STEP.asBool && !io.in.dcsr.STEPIE.asBool)
  val enableDebugIntr = io.in.debugIntr && !disableDebugIntr

  val disableAllIntr = disableDebugIntr || !io.in.mnstatusNMIE

  val normalIntrVec = mIRVec | hsIRVec | vsMapHostIRVec
  val intrVec = Mux(disableAllIntr, 0.U, Mux(io.in.nmi, nmiVec, normalIntrVec))

  // virtual interrupt with hvictl injection
  val vsIRModeCond = privState.isModeVS && vsstatusSIE || privState < PrivState.ModeVS
  val SelectCandidate5 = Candidate123LowCandidate45 && Candidate5
  // delay at least 6 cycles to maintain the atomic of sret/mret
  // 65bit indict current interrupt is NMI
  val intrVecReg = RegInit(0.U(8.W))
  val debugIntrReg = RegInit(false.B)
  val nmiReg = RegInit(false.B)
  val viIsHvictlInjectReg = RegInit(false.B)
  val irToHSReg = RegInit(false.B)
  val irToVSReg = RegInit(false.B)
  intrVecReg := intrVec
  debugIntrReg := enableDebugIntr
  nmiReg := io.in.nmi
  viIsHvictlInjectReg := vsIRModeCond && SelectCandidate5
  irToHSReg := irToHS
  irToVSReg := irToVS
  val delayedIntrVec = DelayN(intrVecReg, 5)
  val delayedDebugIntr = DelayN(debugIntrReg, 5)
  val delayedNMI = DelayN(nmiReg, 5)
  val delayedVIIsHvictlInjectReg = DelayN(viIsHvictlInjectReg, 5)
  val delayedIRToHS = DelayN(irToHSReg, 5)
  val delayedIRToVS = DelayN(irToVSReg, 5)

  io.out.interruptVec.valid := delayedIntrVec.orR || delayedDebugIntr || delayedVIIsHvictlInjectReg
  io.out.interruptVec.bits := delayedIntrVec
  io.out.debug := delayedDebugIntr
  io.out.nmi := delayedNMI
  io.out.virtualInterruptIsHvictlInject := delayedVIIsHvictlInjectReg & !delayedNMI
  io.out.irToHS := delayedIRToHS & !delayedNMI
  io.out.irToVS := delayedIRToVS & !delayedNMI

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
    val mip = new MipBundle
    val mie = new MieBundle
    val mideleg = new MidelegBundle
    val sip = new SipBundle
    val sie = new SieBundle
    val hip = new HipBundle
    val hie = new HieBundle
    val hideleg = new HidelegBundle
    val vsip = new VSipBundle
    val vsie = new VSieBundle
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
    val debug = Bool()
    val nmi = Bool()
    val interruptVec = ValidIO(UInt(8.W))
    val mtopi  = new TopIBundle
    val stopi  = new TopIBundle
    val vstopi = new TopIBundle
    val virtualInterruptIsHvictlInject = Bool()
    val irToHS = Bool()
    val irToVS = Bool()
  })
}
