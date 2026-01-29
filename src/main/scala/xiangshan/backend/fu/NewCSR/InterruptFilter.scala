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
  val fromAIAValid = io.in.fromAIA.meip || io.in.fromAIA.seip || io.in.fromAIA.notice_pending
  val platformValid = io.in.platform.meip || io.in.platform.seip

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
  val mtopiIsNotZeroReg: Bool = GatedValidRegNext(mtopiIsNotZero)
  val stopiIsNotZeroReg: Bool = GatedValidRegNext(stopiIsNotZero)

  val NoSEIMask = (~(BigInt(1) << InterruptNO.SEI).U(64.W)).asUInt
  val mtopigather = mip & mie & (~mideleg).asUInt
  val hstopigather = hsip & hsie & (~hideleg).asUInt
  val vstopigather = vsip & vsie & NoSEIMask

  val flag = RegInit(false.B)
  when (platformValid) {
    flag := true.B
  }.elsewhen(fromAIAValid) {
    flag := false.B
  }

  val mipriosSort = Wire(Vec(InterruptNO.interruptDefaultPrio.size, new IpriosSort))
  mipriosSort.zip(InterruptNO.interruptDefaultPrio).zipWithIndex.foreach { case ((iprio, defaultPrio), i) =>
    iprio.idx := i.U
    when (mtopigather(defaultPrio)) {
      iprio.enable := true.B
      when (defaultPrio.U === InterruptNO.MEI.U) {
        iprio.isZero := platformValid || flag
        val mtopeiGreaterThan255 = mtopei.IPRIO.asUInt(10, 8).orR
        iprio.greaterThan255 := mtopeiGreaterThan255
        iprio.prioNum := mtopei.IPRIO.asUInt(7, 0)
      }.otherwise {
        iprio.isZero := !miprios(7 + 8 * defaultPrio, 8 * defaultPrio).orR
        iprio.greaterThan255 := false.B
        iprio.prioNum := miprios(7 + 8 * defaultPrio, 8 * defaultPrio)
      }
    }.otherwise {
      iprio.enable := false.B
      iprio.isZero := false.B
      iprio.greaterThan255 := false.B
      iprio.prioNum := 0.U
    }
  }
  val hsipriosSort = Wire(Vec(InterruptNO.interruptDefaultPrio.size, new IpriosSort))
  hsipriosSort.zip(InterruptNO.interruptDefaultPrio).zipWithIndex.foreach { case ((iprio, defaultPrio), i) =>
    iprio.idx := i.U
    when (hstopigather(defaultPrio)) {
      iprio.enable := true.B
      when (defaultPrio.U === InterruptNO.SEI.U) {
        iprio.isZero := platformValid || flag
        val stopeiGreaterThan255 = stopei.IPRIO.asUInt(10, 8).orR
        iprio.greaterThan255 := stopeiGreaterThan255
        iprio.prioNum := stopei.IPRIO.asUInt(7, 0)
      }.otherwise {
        iprio.isZero := !hsiprios(7 + 8 * defaultPrio, 8 * defaultPrio).orR
        iprio.greaterThan255 := false.B
        iprio.prioNum := hsiprios(7 + 8 * defaultPrio, 8 * defaultPrio)
      }
    }.otherwise {
      iprio.enable := false.B
      iprio.isZero := false.B
      iprio.greaterThan255 := false.B
      iprio.prioNum := 0.U
    }
  }
  val hvipriosSort = Wire(Vec(InterruptNO.interruptDefaultPrio.size, new IpriosSort))
  hvipriosSort.zip(InterruptNO.interruptDefaultPrio).zipWithIndex.foreach { case ((iprio, defaultPrio), i) =>
    iprio.idx := i.U
    when(vstopigather(defaultPrio)) {
      iprio.enable := true.B
      iprio.isZero := true.B
      iprio.greaterThan255 := false.B
      iprio.prioNum := 0.U
    }.otherwise {
      iprio.enable := false.B
      iprio.isZero := false.B
      iprio.greaterThan255 := false.B
      iprio.prioNum := 0.U
    }
  }
  when(vstopigather(1).asBool) {
    hvipriosSort(findIndex(1.U)).isZero := !hviprio1.PrioSSI.asUInt.orR
    hvipriosSort(findIndex(1.U)).prioNum := hviprio1.PrioSSI.asUInt
  }

  when(vstopigather(5).asBool) {
    hvipriosSort(findIndex(5.U)).isZero := !hviprio1.PrioSTI.asUInt.orR
    hvipriosSort(findIndex(5.U)).prioNum := hviprio1.PrioSTI.asUInt
  }

  for (i <- 0 to 10) {
    when(vstopigather(i + 13).asBool) {
      hvipriosSort(findIndex((i + 13).U)).isZero := !hviprios(7 + 8 * (i + 5), 8 * (i + 5)).orR
      hvipriosSort(findIndex((i + 13).U)).prioNum := hviprios(7 + 8 * (i + 5), 8 * (i + 5))
    }
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
  def minSelect(iprios: Vec[IpriosSort], xei: UInt): Vec[IpriosSort] = {
    iprios.size match {
      case 1 =>
        iprios
      case 2 =>
        val left = iprios(0)
        val right = iprios(1)
        val minIprio = Mux1H(Seq(
          (left.enable && !right.enable) -> left,
          (!left.enable && right.enable) -> right,
          (left.enable && right.enable) -> Mux1H(Seq(
            (left.isZero && right.isZero) -> left,
            (left.isZero && !right.isZero) -> Mux(left.idx <= xei, left, right),
            (!left.isZero && right.isZero) -> Mux(right.idx <= xei, right, left),
            (!left.isZero && !right.isZero) -> Mux1H(Seq(
              (left.greaterThan255 && !right.greaterThan255) -> right,
              (!left.greaterThan255 && right.greaterThan255) -> left,
              (!left.greaterThan255 && !right.greaterThan255) -> Mux(left.prioNum <= right.prioNum, left, right),
            ))
          ))
        ))
        VecInit(minIprio)
      case _ =>
        val leftIprio = minSelect(VecInit(iprios.take((iprios.size + 1) / 2)), xei)
        val rightIprio = minSelect(VecInit(iprios.drop((iprios.size + 1) / 2)), xei)
        minSelect(VecInit(leftIprio ++ rightIprio), xei)
    }
  }

  def highIprio(iprios: Vec[IpriosSort], xei: UInt): IpriosSort = {
    val result = minSelect(iprios, xei)
    result(0)
  }

  def sliceIpriosSort(ipriosSort: Vec[IpriosSort]): Vec[Vec[IpriosSort]] = {
    val ipriosSortTmp = Wire(Vec(8, Vec(8, new IpriosSort)))
    for (i <- 0 until 8) {
      val end = math.min(8 * (i + 1), InterruptNO.interruptDefaultPrio.size)
      val ipriosSlice = ipriosSort.slice(8 * i, end)
      val paddingSlice = ipriosSlice ++ Seq.fill(8 - ipriosSlice.length)(0.U.asTypeOf(new IpriosSort))
      ipriosSortTmp(i) := VecInit(paddingSlice)
    }
    ipriosSortTmp
  }

  private val mipriosSortTmp  = sliceIpriosSort(mipriosSort)
  private val hsipriosSortTmp = sliceIpriosSort(hsipriosSort)
  private val hvipriosSortTmp = sliceIpriosSort(hvipriosSort)

  private val meiPrioIdx = InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.MEI).U
  private val seiPrioIdx = InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.SEI).U
  private val vseiPrioIdx = InterruptNO.getPrioIdxInGroup(_.interruptDefaultPrio)(_.VSEI).U

  private val mipriosTmp = Wire(Vec(8, new IpriosSort))
  mipriosSortTmp.zipWithIndex.foreach { case (iprios, i) =>
    val ipriosTmp = highIprio(iprios, meiPrioIdx)
    mipriosTmp(i) := ipriosTmp
  }

  private val hsipriosTmp = Wire(Vec(8, new IpriosSort))
  hsipriosSortTmp.zipWithIndex.foreach { case (iprios, i) =>
    val ipriosTmp = highIprio(iprios, seiPrioIdx)
    hsipriosTmp(i) := ipriosTmp
  }

  private val hvipriosTmp = Wire(Vec(8, new IpriosSort))
  hvipriosSortTmp.zipWithIndex.foreach { case (iprios, i) =>
    val ipriosTmp = highIprio(iprios, vseiPrioIdx)
    hvipriosTmp(i) := ipriosTmp
  }

  private val mipriosReg = Reg(Vec(8, new IpriosSort))
  private val hsipriosReg = Reg(Vec(8, new IpriosSort))
  private val hvipriosReg = Reg(Vec(8, new IpriosSort))

  for (i <- 0 until 8) {
    mipriosReg(i) := mipriosTmp(i)
    hsipriosReg(i) := hsipriosTmp(i)
    hvipriosReg(i) := hvipriosTmp(i)
  }

  private val mipriosRegTmp = highIprio(mipriosReg, meiPrioIdx)
  private val hsipriosRegTmp = highIprio(hsipriosReg, seiPrioIdx)
  private val hvipriosRegTmp = highIprio(hvipriosReg, vseiPrioIdx)

  private val mIidIdxReg = mipriosRegTmp.idx
  private val hsIidIdxReg = hsipriosRegTmp.idx
  private val vsIidIdxReg = hvipriosRegTmp.idx

  private val mIidNum  = findNum(mIidIdxReg)
  private val hsIidNum = findNum(hsIidIdxReg)
  private val vsIidNum = findNum(vsIidIdxReg)

  private val mIidDefaultPrioHighMEI: Bool = mIidIdxReg < meiPrioIdx
  private val mIidDefaultPrioLowMEI : Bool = mIidIdxReg > meiPrioIdx

  private val hsIidDefaultPrioHighSEI: Bool = hsIidIdxReg < seiPrioIdx
  private val hsIidDefaultPrioLowSEI : Bool = hsIidIdxReg > seiPrioIdx

  // update mtopi
  io.out.mtopi.IID := Mux(mtopiIsNotZeroReg, mIidNum, 0.U)
  io.out.mtopi.IPRIO := Mux(mtopiIsNotZeroReg,
    Mux1H(Seq(
      (!mipriosRegTmp.isZero && !mipriosRegTmp.greaterThan255) -> mipriosRegTmp.prioNum,
      (mipriosRegTmp.greaterThan255 || mipriosRegTmp.isZero && mIidDefaultPrioLowMEI) -> 255.U,
      (mipriosRegTmp.isZero && mIidDefaultPrioHighMEI) -> 0.U,
    )),
    0.U
  )

  // upadte stopi
  io.out.stopi.IID := Mux(stopiIsNotZeroReg, hsIidNum, 0.U)
  io.out.stopi.IPRIO := Mux(stopiIsNotZeroReg,
    Mux1H(Seq(
      (!hsipriosRegTmp.isZero && !hsipriosRegTmp.greaterThan255) -> hsipriosRegTmp.prioNum,
      (hsipriosRegTmp.greaterThan255 || hsipriosRegTmp.isZero && hsIidDefaultPrioLowSEI) -> 255.U,
      (hsipriosRegTmp.isZero && hsIidDefaultPrioHighSEI) -> 0.U,
    )),
    0.U
  )

  // refactor this code & has some problem
  val Candidate1: Bool = vsip.SEIP && vsie.SEIE && (hstatus.VGEIN.asUInt =/= 0.U) && (vstopei.asUInt =/= 0.U)
  val Candidate2: Bool = vsip.SEIP && vsie.SEIE && (hstatus.VGEIN.asUInt === 0.U) && (hvictl.IID.asUInt === 9.U) && (hvictl.IPRIO.asUInt =/= 0.U)
  val Candidate3: Bool = vsip.SEIP && vsie.SEIE && !Candidate1 && !Candidate2
  val Candidate4: Bool = (hvictl.VTI.asUInt === 0.U) && vstopigather.orR
  val Candidate5: Bool = (hvictl.VTI.asUInt === 1.U) && (hvictl.IID.asUInt =/= 9.U)
  val CandidateNoValid: Bool = !Candidate1 && !Candidate2 && !Candidate3 && !Candidate4 && !Candidate5

  assert(PopCount(Cat(Candidate1, Candidate2, Candidate3)) < 2.U, "Only one Candidate could be select from Candidate1/2/3 in VS-level!")
  assert(PopCount(Cat(Candidate4, Candidate5)) < 2.U, "Only one Candidate could be select from Candidate4/5 in VS-level!")
  assert(PopCount(Cat(Candidate2, Candidate5)) < 2.U, "Candidate2 and candidate5 cannot be true at the same time. ")

  val Candidate123 = Candidate1 || Candidate2 || Candidate3
  val Candidate45 = Candidate4 || Candidate5

  // Candidate2,Candidate5 不可能同时成立
  val onlyC1Enable = Candidate1 & !Candidate45
  val onlyC2Enable = Candidate2 & !Candidate45
  val onlyC3Enable = Candidate3 & !Candidate123
  val onlyC4Enable = Candidate4 & !Candidate123
  val onlyC5Enable = Candidate5 & !Candidate123
  val C1C4Enable   = Candidate1 & Candidate4
  val C1C5Enable   = Candidate1 & Candidate5
  val C2C4Enable   = Candidate2 & Candidate4
  val C3C4Enable   = Candidate3 & Candidate4
  val C3C5Enable   = Candidate3 & Candidate5

  val CandidateNoValidReg: Bool = GatedValidRegNext(CandidateNoValid)
  val Candidate123Reg = GatedValidRegNext(Candidate123)
  val NoCandidate45Reg = GatedValidRegNext(!Candidate45)
  val onlyC1EnableReg = GatedValidRegNext(onlyC1Enable)
  val onlyC2EnableReg = GatedValidRegNext(onlyC2Enable)
  val onlyC3EnableReg = GatedValidRegNext(onlyC3Enable)
  val onlyC4EnableReg = GatedValidRegNext(onlyC4Enable)
  val onlyC5EnableReg = GatedValidRegNext(onlyC5Enable)
  val C1C4EnableReg = GatedValidRegNext(C1C4Enable)
  val C1C5EnableReg = GatedValidRegNext(C1C5Enable)
  val C2C4EnableReg = GatedValidRegNext(C2C4Enable)
  val C3C4EnableReg = GatedValidRegNext(C3C4Enable)
  val C3C5EnableReg = GatedValidRegNext(C3C5Enable)

  val hvictlReg = RegNext(hvictl)
  val vstopeiReg = RegNext(vstopei)

  val iidOnlyC1 = Wire(UInt(12.W))
  val iidOnlyC4 = Wire(UInt(12.W))
  val iidOnlyC5 = Wire(UInt(12.W))
  val iprioOnlyC1 = Wire(UInt(8.W))
  val iprioOnlyC2 = Wire(UInt(8.W))
  val iprioOnlyC3 = Wire(UInt(8.W))
  val iprioOnlyC4 = Wire(UInt(8.W))
  val iprioOnlyC5 = Wire(UInt(8.W))
  val iidC1C4 = Wire(UInt(12.W))
  val iidC1C5 = Wire(UInt(12.W))
  val iidC2C4 = Wire(UInt(12.W))
  val iidC3C4 = Wire(UInt(12.W))
  val iidC3C5 = Wire(UInt(12.W))
  val iprioC1C4 = Wire(UInt(8.W))
  val iprioC1C5 = Wire(UInt(8.W))
  val iprioC2C4 = Wire(UInt(8.W))
  val iprioC3C4 = Wire(UInt(8.W))
  val iprioC3C5 = Wire(UInt(8.W))

  iidOnlyC1 := InterruptNO.SEI.U
  iidOnlyC4 := vsIidNum
  iidOnlyC5 := hvictlReg.IID.asUInt

  val iidC4Idx = Wire(UInt(12.W))
  iidC4Idx := vsIidIdxReg

  val iprioC1   = Wire(UInt(11.W))
  val iprioC2C5 = Wire(UInt(11.W))
  val iprioC4   = Wire(UInt(11.W))
  val iprioC1Tmp = Wire(UInt(8.W))
  val iprioC4Tmp = Wire(UInt(8.W))
  val iprioC3C5Tmp = Wire(UInt(8.W))

  val hvictlDPR = Mux(hvictlReg.DPR.asBool, 255.U, 0.U)
  val C1GreaterThan255 = vstopeiReg.IPRIO.asUInt(10, 8).orR
  val C4IsZero = !hvipriosRegTmp.prioNum.orR
  val C2C5IsZero = !hvictlReg.IPRIO.asUInt.orR
  val C4HighVSEI = iidC4Idx < findIndex(InterruptNO.VSEI.U)
  val SEIHighC4 = findIndex(InterruptNO.SEI.U) < iidC4Idx
  val iprioC1GreaterThan255 = Mux(C1GreaterThan255, 255.U, iprioC1Tmp)

  iprioC1 := vstopeiReg.IPRIO.asUInt
  iprioC2C5 := hvictlReg.IPRIO.asUInt
  iprioC4 := hvipriosRegTmp.prioNum

  iprioC1Tmp := iprioC1(7, 0)
  iprioC4Tmp := Mux(C4IsZero, Mux(C4HighVSEI, 0.U, 255.U), iprioC4)
  iprioC3C5Tmp := Mux(C2C5IsZero, hvictlDPR, iprioC2C5)

  iprioOnlyC1 := iprioC1GreaterThan255
  iprioOnlyC2 := iprioC2C5
  iprioOnlyC3 := 255.U
  iprioOnlyC4 := iprioC4Tmp
  iprioOnlyC5 := iprioC3C5Tmp

  // C1,C4 enable
  when(C4IsZero) {
    iidC1C4 := Mux(C4HighVSEI, iidOnlyC4, iidOnlyC1)
    iprioC1C4 := Mux(C4HighVSEI, 0.U, iprioC1GreaterThan255)
  }.elsewhen(iprioC1 < iprioC4) {
    iidC1C4 := iidOnlyC1
    iprioC1C4 := iprioC1Tmp
  }.elsewhen(iprioC1 === iprioC4) {
    iidC1C4 := Mux(SEIHighC4, iidOnlyC1, iidOnlyC4)
    iprioC1C4 := Mux(SEIHighC4, iprioC1Tmp, iprioC4)
  }.otherwise {
    iidC1C4 := iidOnlyC4
    iprioC1C4 := iprioC4
  }

  // C1,C5 enable
  iidC1C5 := Mux(hvictlReg.DPR.asBool, iidOnlyC1, iidOnlyC5)
  when(C2C5IsZero) {
    iprioC1C5 := Mux(hvictlReg.DPR.asBool, iprioC1GreaterThan255, 0.U)
  }.elsewhen(iprioC1 < iprioC2C5) {
    iidC1C5 := iidOnlyC1
    iprioC1C5 := iprioC1Tmp
  }.elsewhen(iprioC1 === iprioC2C5) {
    iprioC1C5 := Mux(hvictlReg.DPR.asBool, iprioC1Tmp, iprioC2C5)
  }.otherwise {
    iidC1C5 := iidOnlyC5
    iprioC1C5 := iprioC3C5Tmp
  }
  
  // C2,C4 enable
  when(C4IsZero) {
    iidC2C4 := Mux(C4HighVSEI, iidOnlyC4, iidOnlyC1)
    iprioC2C4 := Mux(C4HighVSEI, 0.U, iprioC2C5)
  }.elsewhen(iprioC2C5 < iprioC4) {
    iidC2C4 := iidOnlyC1
    iprioC2C4 := iprioC2C5
  }.elsewhen(iprioC2C5 === iprioC4) {
    iidC2C4 := Mux(SEIHighC4, iidOnlyC1, iidOnlyC4)
    iprioC2C4 := Mux(SEIHighC4, iprioC2C5, iprioC4)
  }.otherwise {
    iidC2C4 := iidOnlyC4
    iprioC2C4 := iprioC4
  }

  // C3,C4 enable
  iidC3C4 := Mux(C4IsZero, Mux(C4HighVSEI, iidOnlyC4, iidOnlyC1), iidOnlyC4)
  iprioC3C4 := iprioC4Tmp
  // C3,C5 enable
  iidC3C5 := Mux(C2C5IsZero, Mux(hvictlReg.DPR.asBool, iidOnlyC5, iidOnlyC1), iidOnlyC5)
  iprioC3C5 := iprioC3C5Tmp

  // update vstopi
  io.out.vstopi.IID := Mux(CandidateNoValidReg,
    0.U,
    Mux1H(Seq(
      (Candidate123Reg & NoCandidate45Reg) -> iidOnlyC1,
      onlyC4EnableReg -> iidOnlyC4,
      onlyC5EnableReg -> iidOnlyC5,
      C1C4EnableReg -> iidC1C4,
      C1C5EnableReg -> iidC1C5,
      C2C4EnableReg -> iidC2C4,
      C3C4EnableReg -> iidC3C4,
      C3C5EnableReg -> iidC3C5,
    ))
  )
  io.out.vstopi.IPRIO := Mux(CandidateNoValidReg,
    0.U,
    Mux(!hvictlReg.IPRIOM.asBool, 1.U,
      Mux1H(Seq(
        onlyC1EnableReg -> iprioOnlyC1,
        onlyC2EnableReg -> iprioOnlyC2,
        onlyC3EnableReg -> iprioOnlyC3,
        onlyC4EnableReg -> iprioOnlyC4,
        onlyC5EnableReg -> iprioOnlyC5,
        C1C4EnableReg -> iprioC1C4,
        C1C5EnableReg -> iprioC1C5,
        C2C4EnableReg -> iprioC2C4,
        C3C4EnableReg -> iprioC3C4,
        C3C5EnableReg -> iprioC3C5,
      ))
    )
  )

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

  val mIRVec  = mIRVecTmp
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
  val SelectCandidate5 = onlyC5EnableReg || C3C5EnableReg ||
                         C1C5EnableReg && (iprioC1 === iprioC2C5 && !hvictlReg.DPR.asBool || iprioC1 > iprioC2C5)
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
  viIsHvictlInjectReg := vsIRModeCond && SelectCandidate5 && io.in.mnstatusNMIE
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
    val platform = new Bundle {
      val meip = Bool()
      val seip = Bool()
    }
    val fromAIA = new Bundle {
      val meip = Bool()
      val seip = Bool()
      val notice_pending = Bool()
    }
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


class IpriosSort extends Bundle {
  val idx = UInt(6.W)
  val enable = Bool()
  val isZero = Bool()
  val greaterThan255 = Bool()
  val prioNum = UInt(8.W)
}
