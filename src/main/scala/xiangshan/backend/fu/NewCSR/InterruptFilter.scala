package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState, XtvecBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, XtvecMode}
import xiangshan.backend.fu.util.CSRConst
import xiangshan.backend.fu.NewCSR.InterruptNO


class InterruptFilter extends Module {
  val io = IO(new InterruptFilterIO)

  val mstatusMIE = io.in.mstatusMIE
  val sstatusSIE = io.in.sstatusSIE
  val vsstatusSIE = io.in.vsstatusSIE
  val mip = io.in.mip
  val mie = io.in.mie
  val mideleg = io.in.mideleg
  val privState = io.in.privState
  val hip = io.in.hip
  val hie = io.in.hie
  val hideleg = io.in.hideleg
  val hvictl = io.in.hvictl
  val hstatus = io.in.hstatus
  val mtopei = io.in.mtopei
  val stopei = io.in.stopei
  val vstopei = io.in.vstopei
  val hviprio1 = io.in.hviprio1
  val hviprio2 = io.in.hviprio2
  val iprios = io.in.iprios
  val hviprios = Cat(hviprio2.asUInt, hviprio1.asUInt)

  val mtopiIsNotZero: Bool = (mip.asUInt & mie.asUInt & (~mideleg.asUInt).asUInt) =/= 0.U
  val stopiIsNotZero: Bool = privState.isModeHS & ((((mip.asUInt | hip.asUInt) & (mie.asUInt | hie.asUInt)) & (~hideleg.asUInt).asUInt) =/= 0.U)

  val ipriosIsZero: Bool = !iprios.orR

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

  val iidNum = Wire(UInt(6.W))
  val prioNum = Wire(UInt(8.W))
  iidNum := highIprio(iprios)._1
  prioNum := highIprio(iprios)._2

  val iidDefaultPrioHighMEI: Bool = findIndex(iidNum) < 1.U
  val iidDefaultPrioLowMEI:  Bool = findIndex(iidNum) > 1.U
  val iidDefaultPrioHighSEI: Bool = findIndex(iidNum) < 4.U
  val iidDefaultPrioLowSEI:  Bool = findIndex(iidNum) > 4.U

  val intrIsEI: Bool = (iidNum === InterruptNO.SEI.U) || (iidNum === InterruptNO.MEI.U)

  val mtopiPrioNumReal = Mux(intrIsEI, mtopei.IPRIO.asUInt, prioNum)
  val stopiPrioNumReal = Mux(intrIsEI, stopei.IPRIO.asUInt, prioNum)

  // update mtopi
  io.out.mtopi.IID := Mux(mtopiIsNotZero, iidNum, 0.U)
  io.out.mtopi.IPRIO := Mux(mtopiIsNotZero, Mux(ipriosIsZero, 1.U,
    Mux1H(
      Seq(
        mtopiPrioNumReal >= 1.U && mtopiPrioNumReal <= 255.U,
        (mtopiPrioNumReal > 255.U) || ((mtopiPrioNumReal === 0.U) && iidDefaultPrioLowMEI),
        (mtopiPrioNumReal === 0.U) && iidDefaultPrioHighMEI,
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
  io.out.stopi.IID := Mux(stopiIsNotZero, iidNum, 0.U)
  io.out.stopi.IPRIO := Mux(stopiIsNotZero, Mux(ipriosIsZero, 1.U,
    Mux1H(
      Seq(
        stopiPrioNumReal >= 1.U && stopiPrioNumReal <= 255.U,
        (stopiPrioNumReal > 255.U) || ((stopiPrioNumReal === 0.U) && iidDefaultPrioLowSEI),
        (stopiPrioNumReal === 0.U) && iidDefaultPrioHighSEI,
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
  val VSIValid0: Bool = (hideleg.VSEI.asUInt =/= 0.U) && hip.VSEIP.asBool && hie.VSEIE.asBool && (hstatus.VGEIN.asUInt =/= 0.U) && (vstopei.asUInt =/= 0.U)
  val VSIValid1: Bool = (hideleg.VSEI.asUInt =/= 0.U) && hip.VSEIP.asBool && hie.VSEIE.asBool && (hstatus.VGEIN.asUInt === 0.U) && (hvictl.IID.asUInt === 9.U) && (hvictl.IPRIO.asUInt =/= 0.U)
  val VSIValid2: Bool = (hideleg.VSEI.asUInt =/= 0.U) && hip.VSEIP.asBool && hie.VSEIE.asBool && !VSIValid0 && !VSIValid1
  val VSIValid3: Bool = hvictl.VTI.asUInt === 0.U
  val VSIValid4: Bool = (hvictl.VTI.asUInt === 1.U) && (hvictl.IID.asUInt =/= 9.U)

  val VSIidNumTmp = Wire(UInt(6.W))
  val VSIidNum = Wire(UInt(6.W))
  val VSPrioNum = Wire(UInt(8.W))
  VSIidNumTmp := highIprio(hviprios, vsMode = true)._1
  VSPrioNum := highIprio(hviprios, vsMode = true)._2

  VSIidNum := Mux1H(
    Seq(
      VSIidNumTmp === 0.U,
      VSIidNumTmp === 1.U,
      (VSIidNumTmp =/= 0.U) && (VSIidNumTmp =/= 1.U),
    ),
    Seq(
      1.U,
      5.U,
      VSIidNumTmp + 11.U,
    )
  )

  // update vstopi
  io.out.vstopi.IID := Mux1H(
    Seq(
      VSIValid0 || VSIValid2,
      VSIValid1 || VSIValid4,
      VSIValid3,
      !VSIValid0 && !VSIValid1 && !VSIValid2 && !VSIValid3 && !VSIValid4
    ),
    Seq(
      InterruptNO.VSEI.U,
      hvictl.IID.asUInt,
      VSIidNum,
      0.U,
    )
  )
  io.out.vstopi.IPRIO := Mux1H(
    Seq(
      VSIValid0,
      VSIValid1,
      VSIValid2,
      VSIValid3,
      VSIValid4,
      !VSIValid0 && !VSIValid1 && !VSIValid2 && !VSIValid3 && !VSIValid4
    ),
    Seq(
      vstopei.IPRIO.asUInt(7, 0),
      hvictl.IPRIO.asUInt,
      255.U, // why 256, width = 8?
      VSPrioNum,
      Mux(hvictl.DPR.asBool, InterruptNO.VSEI.asUInt, hvictl.IPRIO.asUInt),
      0.U,
    )
  )

  val ideleg = mideleg.asUInt & mip.asUInt
  def priviledgeEnableDetect(x: Bool): Bool = Mux(x, ((privState.PRVM === PrivMode.S) && sstatusSIE) || (privState.PRVM < PrivMode.S),
    ((privState.PRVM === PrivMode.M) && mstatusMIE) || (privState.PRVM < PrivMode.M))
  val intrVecEnable = Wire(Vec(64, Bool()))
  intrVecEnable.zip(ideleg.asBools).map { case (x, y) => x := priviledgeEnableDetect(y) } // todo: !disableInterrupt
  val intrVec = mie.asUInt & mip.asUInt & intrVecEnable.asUInt // todo: Cat(debugIntr && !debugMode, mie.rdata.asUInt(11, 0) & mip.rdata.asUInt & intrVecEnable.asUInt)

  io.out.interruptVec.valid := intrVec.orR
  io.out.interruptVec.bits := intrVec
}

class InterruptFilterIO extends Bundle {
  val in = Input(new Bundle {
    val mstatusMIE  = Bool()
    val sstatusSIE  = Bool()
    val vsstatusSIE = Bool()
    val mip = new MipBundle
    val mie = new MieBundle
    val mideleg = new MidelegBundle
    val privState = new PrivState
    val hip = new HipBundle
    val hie = new HieBundle
    val hideleg = new HidelegBundle
    val hvictl = new HvictlBundle
    val hstatus = new HstatusBundle
    val mtopei = new TopEIBundle
    val stopei = new TopEIBundle
    val vstopei = new TopEIBundle
    val hviprio1 = new Hviprio1Bundle
    val hviprio2 = new Hviprio2Bundle
    val iprios = UInt((64*8).W)
  })

  val out = Output(new Bundle {
    val interruptVec = ValidIO(UInt(64.W))
    val mtopi = new TopIBundle
    val stopi = new TopIBundle
    val vstopi = new TopIBundle
  })
}