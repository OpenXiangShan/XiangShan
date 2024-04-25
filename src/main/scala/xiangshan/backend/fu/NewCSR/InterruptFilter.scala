package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
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

  val mipFields = mip.asTypeOf(new MipBundle)
  val mieFields = mie.asTypeOf(new MieBundle)
  val sipFields = sip.asTypeOf(new SipBundle)
  val sieFields = sie.asTypeOf(new SieBundle)
  val hipFields = hip.asTypeOf(new HipBundle)
  val hieFields = hie.asTypeOf(new HieBundle)
  val hidelegFields = hideleg.asTypeOf(new HidelegBundle)

  private val hsip = hip.asUInt | sip.asUInt
  private val hsie = hie.asUInt | sie.asUInt

  val mtopiIsNotZero: Bool = (mip & mie & (~mideleg).asUInt) =/= 0.U
  val stopiIsNotZero: Bool = privState.isModeHS && ((hsip & hsie & (~hideleg).asUInt) =/= 0.U)

  val mIpriosIsZero : Bool = miprios  === 0.U
  val hsIpriosIsZero: Bool = hsiprios === 0.U

  def findIndex(input: UInt): UInt = {
    val select = WireInit(0.U(log2Up(InterruptNO.interruptDefaultPrio.length).W))
    for (i <- 0 until InterruptNO.interruptDefaultPrio.length) {
      when(input === InterruptNO.interruptDefaultPrio(i).U) {
        select := i.U
      }
    }
    select
  }

  // value lower, priority higher
  def minSelect(index: Seq[UInt], value: Seq[UInt]): (Seq[UInt], Seq[UInt]) = {
    value.size match {
      case 1 =>
        (index, value)
      case 2 =>
        val minIndex = Mux1H(
          Seq(
            value.head < value.drop(1).head,
            value.head === value.drop(1).head,
            value.head > value.drop(1).head,
          ),
          Seq(
            index.head,
            Mux(findIndex(index.head) < findIndex(index.drop(1).head), index.head, index.drop(1).head),
            index.drop(1).head,
          )
        )
        val minValue = Mux1H(
          Seq(
            value.head < value.drop(1).head,
            value.head === value.drop(1).head,
            value.head > value.drop(1).head,
          ),
          Seq(
            value.head,
            Mux(findIndex(index.head) < findIndex(index.drop(1).head), value.head, value.drop(1).head),
            value.drop(1).head,
          )
        )
        (Seq(minIndex), Seq(minValue))
      case _ =>
        val (leftIndex, leftValue) = minSelect(index.take(value.size/2), value.take(value.size/2))
        val (rightIndex, rightValue) = minSelect(index.drop(value.size/2), value.drop(value.size/2))
        minSelect(leftIndex ++ rightIndex, leftValue ++ rightValue)
    }
  }

  def highIprio(iprios: UInt, vsMode: Boolean = false): (UInt, UInt) = {
    if (vsMode) {
      val index = WireInit(VecInit(Seq.fill(13)(0.U(6.W))))
      val value = WireInit(VecInit(Seq.fill(13)(0.U(6.W))))
      for (i <- 0 until 13) {
        index(i) := i.U
        value(i) := iprios(8 * (i + 1) - 1, 8 * i)
      }
      val result = minSelect(index, value)
      (result._1.head, result._2.head)
    } else {
      val index = WireInit(VecInit(Seq.fill(64)(0.U(6.W))))
      val value = WireInit(VecInit(Seq.fill(64)(0.U(8.W))))
      for (i <- 0 until 64) {
        index(i) := i.U
        value(i) := iprios(8 * (i + 1) - 1, 8 * i)
      }
      val result = minSelect(index, value)
      (result._1.head, result._2.head)
    }
  }

  private val (mIidNum,  mPrioNum)  = highIprio(miprios)
  private val (hsIidNum, hsPrioNum) = highIprio(hsiprios)

  private val mIidIdx  = findIndex(mIidNum)
  private val hsIidIdx = findIndex(hsIidNum)

  private val mIidDefaultPrioHighMEI: Bool = mIidIdx < InterruptNO.getPrioIdx(_.MEI).U
  private val mIidDefaultPrioLowMEI : Bool = mIidIdx > InterruptNO.getPrioIdx(_.MEI).U

  private val hsIidDefaultPrioHighSEI: Bool = hsIidIdx < InterruptNO.getPrioIdx(_.SEI).U
  private val hsIidDefaultPrioLowSEI : Bool = hsIidIdx > InterruptNO.getPrioIdx(_.SEI).U

  private val mIrIsEI  = ( mIidNum === InterruptNO.SEI.U) || ( mIidNum === InterruptNO.MEI.U)
  private val hsIrIsEI = (hsIidNum === InterruptNO.SEI.U) || (hsIidNum === InterruptNO.MEI.U)

  val mtopiPrioNumReal = Mux( mIrIsEI, mtopei.IPRIO.asUInt,  mPrioNum)
  val stopiPrioNumReal = Mux(hsIrIsEI, stopei.IPRIO.asUInt, hsPrioNum)

  // update mtopi
  io.out.mtopi.IID := Mux(mtopiIsNotZero, mIidNum, 0.U)
  io.out.mtopi.IPRIO := Mux(mtopiIsNotZero, Mux(mIpriosIsZero, 1.U,
    Mux1H(
      Seq(
        mtopiPrioNumReal >= 1.U && mtopiPrioNumReal <= 255.U,
        (mtopiPrioNumReal > 255.U) || ((mtopiPrioNumReal === 0.U) && mIidDefaultPrioLowMEI),
        (mtopiPrioNumReal === 0.U) && mIidDefaultPrioHighMEI,
      ),
      Seq(
        mtopiPrioNumReal(7, 0),
        255.U,
        0.U,
      )
    )),
    0.U
  )
  // upadte stopi
  io.out.stopi.IID := Mux(stopiIsNotZero, hsIidNum, 0.U)
  io.out.stopi.IPRIO := Mux(stopiIsNotZero, Mux(hsIpriosIsZero, 1.U,
    Mux1H(
      Seq(
        stopiPrioNumReal >= 1.U && stopiPrioNumReal <= 255.U,
        (stopiPrioNumReal > 255.U) || ((stopiPrioNumReal === 0.U) && hsIidDefaultPrioLowSEI),
        (stopiPrioNumReal === 0.U) && hsIidDefaultPrioHighSEI,
      ),
      Seq(
        stopiPrioNumReal(7, 0),
        255.U,
        0.U,
      )
    )),
    0.U
  )

  // refactor this code & has some problem
  val VSCandidate1: Bool = hidelegFields.VSEI && hipFields.VSEIP && hieFields.VSEIE.asBool && (hstatus.VGEIN.asUInt =/= 0.U) && (vstopei.asUInt =/= 0.U)
  val VSCandidate2: Bool = hidelegFields.VSEI && hipFields.VSEIP && hieFields.VSEIE.asBool && (hstatus.VGEIN.asUInt === 0.U) && (hvictl.IID.asUInt === 9.U) && (hvictl.IPRIO.asUInt =/= 0.U)
  val VSCandidate3: Bool = hidelegFields.VSEI && hipFields.VSEIP && hieFields.VSEIE.asBool && !VSCandidate1 && !VSCandidate2
  val VSCandidate4: Bool = hvictl.VTI.asUInt === 0.U
  val VSCandidate5: Bool = (hvictl.VTI.asUInt === 1.U) && (hvictl.IID.asUInt =/= 9.U)

  assert(PopCount(Cat(VSCandidate1, VSCandidate2, VSCandidate3)) < 2.U, "Only one Candidate could be select from Candidate1/2/3 in VS-level!")
  assert(PopCount(Cat(VSCandidate4, VSCandidate5)) < 2.U, "Only one Candidate could be select from Candidate4/5 in VS-level!")

  val VSIidNumTmp = Wire(UInt(6.W))
  val VSIidNum = Wire(UInt(6.W))
  val VSPrioNum = Wire(UInt(8.W))
  VSIidNumTmp := highIprio(hviprios, vsMode = true)._1
  VSPrioNum := highIprio(hviprios, vsMode = true)._2

  VSIidNum := Mux1H(Seq(
    (VSIidNumTmp === 0.U) -> 1.U,
    (VSIidNumTmp === 1.U) -> 5.U,
    ((VSIidNumTmp =/= 0.U) && (VSIidNumTmp =/= 1.U)) -> (VSIidNumTmp + 11.U),
  ))

  val iidVSCandidate1 = Wire(UInt(12.W))
  val iidVSCandidate2 = Wire(UInt(12.W))
  val iidVSCandidate3 = Wire(UInt(12.W))
  val iidVSCandidate4 = Wire(UInt(12.W))
  val iidVSCandidate5 = Wire(UInt(12.W))
  val iprioVSCandidate1 = Wire(UInt(11.W))
  val iprioVSCandidate2 = Wire(UInt(11.W))
  val iprioVSCandidate3 = Wire(UInt(11.W))
  val iprioVSCandidate4 = Wire(UInt(11.W))
  val iprioVSCandidate5 = Wire(UInt(11.W))

  iidVSCandidate1 := InterruptNO.SEI.U
  iidVSCandidate2 := InterruptNO.SEI.U
  iidVSCandidate3 := InterruptNO.SEI.U
  iidVSCandidate4 := VSIidNum
  iidVSCandidate5 := hvictl.IID.asUInt

  iprioVSCandidate1 := vstopei.IPRIO.asUInt
  iprioVSCandidate2 := hvictl.IPRIO.asUInt
  iprioVSCandidate3 := 256.U
  iprioVSCandidate4 := VSPrioNum
  iprioVSCandidate5 := hvictl.IPRIO.asUInt

  def iprioComp(iidA: UInt, iprioA: UInt, iidB: UInt, iprioB: UInt): (UInt, UInt) = {
    val iprioAHighIprioB: Bool = (iprioA < iprioB) || ((iprioA === iprioB) && (findIndex(iidA) < findIndex(iidB))) || ((iprioA === iprioB) && (findIndex(iidA) === findIndex(iidB)))
    val iprioBHighIprioA: Bool = (iprioA > iprioB) || ((iprioA === iprioB) && (findIndex(iidA) > findIndex(iidB)))
    val iid = Mux1H(Seq(
      iprioAHighIprioB -> iidA,
      iprioBHighIprioA -> iidB,
    ))
    val iprio = Mux1H(Seq(
      iprioAHighIprioB -> iprioA,
      iprioBHighIprioA -> iprioB,
    ))
    (iid, iprio)
  }

  def iprioCompWithDPR(iidA: UInt, iprioA: UInt, iidB: UInt, iprioB: UInt, DPR: Bool): (UInt, UInt) = {
    val iprioAHighIprioB: Bool = (iprioA < iprioB) || ((iprioA === iprioB) && !DPR)
    val iprioBHighIprioA: Bool = (iprioA > iprioB) || ((iprioA === iprioB) && DPR)
    val iid = Mux1H(Seq(
      iprioAHighIprioB -> iidA,
      iprioBHighIprioA -> iidB,
    ))
    val iprio = Mux1H(Seq(
      iprioAHighIprioB -> iprioA,
      iprioBHighIprioA -> iprioB,
    ))
    (iid, iprio)
  }

  val VSCandidate14 = iprioComp(iidVSCandidate1, iprioVSCandidate1, iidVSCandidate4, iprioVSCandidate4)
  val VSCandidate24 = iprioComp(iidVSCandidate2, iprioVSCandidate2, iidVSCandidate4, iprioVSCandidate4)
  val VSCandidate34 = iprioComp(iidVSCandidate3, iprioVSCandidate3, iidVSCandidate4, iprioVSCandidate4)
  val VSCandidate15 = iprioCompWithDPR(iidVSCandidate5, iprioVSCandidate5, iidVSCandidate1, iprioVSCandidate1, hvictl.DPR.asBool)
  val VSCandidate25 = iprioCompWithDPR(iidVSCandidate5, iprioVSCandidate5, iidVSCandidate2, iprioVSCandidate2, hvictl.DPR.asBool)
  val VSCandidate35 = iprioCompWithDPR(iidVSCandidate5, iprioVSCandidate5, iidVSCandidate3, iprioVSCandidate3, hvictl.DPR.asBool)

  val Candidate1Valid  = VSCandidate1 && !VSCandidate4 && !VSCandidate5
  val Candidate2Valid  = VSCandidate2 && !VSCandidate4 && !VSCandidate5
  val Candidate3Valid  = VSCandidate3 && !VSCandidate4 && !VSCandidate5
  val Candidate4Valid  = VSCandidate4 && !VSCandidate1 && !VSCandidate2 && !VSCandidate3
  val Candidate5Valid  = VSCandidate5 && !VSCandidate1 && !VSCandidate2 && !VSCandidate3
  val Candidate14Valid = VSCandidate1 && VSCandidate4
  val Candidate15Valid = VSCandidate1 && VSCandidate5
  val Candidate24Valid = VSCandidate2 && VSCandidate4
  val Candidate25Valid = VSCandidate2 && VSCandidate5
  val Candidate34Valid = VSCandidate3 && VSCandidate4
  val Candidate35Valid = VSCandidate3 && VSCandidate5
  val CandidateNoValid = !VSCandidate1 && !VSCandidate2 && !VSCandidate3 && !VSCandidate4 && !VSCandidate5

  // update vstopi
  io.out.vstopi.IID := Mux1H(Seq(
    Candidate14Valid -> VSCandidate14._1,
    Candidate15Valid -> VSCandidate15._1,
    Candidate24Valid -> VSCandidate24._1,
    Candidate25Valid -> VSCandidate25._1,
    Candidate34Valid -> VSCandidate34._1,
    Candidate35Valid -> VSCandidate35._1,
    CandidateNoValid -> 0.U,
  ))
  io.out.vstopi.IPRIO := Mux1H(Seq(
    Candidate14Valid -> VSCandidate14._2(7, 0),
    Candidate15Valid -> Mux(hvictl.IPRIOM.asBool, VSCandidate15._2(7, 0), 1.U),
    Candidate24Valid -> VSCandidate24._2(7, 0),
    Candidate25Valid -> Mux(hvictl.IPRIOM.asBool, VSCandidate25._2(7, 0), 1.U),
    Candidate34Valid -> Mux(VSCandidate34._2 > 255.U, 255.U, VSCandidate34._2(7, 0)),
    Candidate35Valid -> Mux(hvictl.IPRIOM.asBool, Mux(VSCandidate34._2 > 255.U, 255.U, VSCandidate35._2(7, 0)), 1.U),
    CandidateNoValid -> 0.U,
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

  // todo: support debug interrupt
  // Cat(debugIntr && !debugMode, mie.rdata.asUInt(11, 0) & mip.rdata.asUInt & intrVecEnable.asUInt)
  val intrVec = mIRVec | hsIRVec | vsIRVec

  io.out.interruptVec.valid := intrVec.orR
  io.out.interruptVec.bits := intrVec

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

    val miprios = UInt((64*8).W)
    val hsiprios = UInt((64*8).W)
  })

  val out = Output(new Bundle {
    val interruptVec = ValidIO(UInt(64.W))
    val mtopi  = new TopIBundle
    val stopi  = new TopIBundle
    val vstopi = new TopIBundle
  })
}
