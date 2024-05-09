package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{Constantin, SelectOne}
import utils._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO
import xiangshan.backend.Bundles.{DynInst, ExuOH}
import xiangshan.backend.datapath.DataSource
import xiangshan.backend.fu.FuType.FuTypeOrR

import scala.collection._

class Dispatch2Iq(val schdBlockParams : SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val issueBlockParams = schdBlockParams.issueBlockParams

  val numIn = schdBlockParams.numUopIn
  require(issueBlockParams.size > 0, "issueBlock is null or the enq size of all issueBlock not be the same all\n")
  val numOut = issueBlockParams.head.numEnq

  // Deq for std's IQ is not assigned in Dispatch2Iq, so add one more src for it.
  val numRegSrc: Int = issueBlockParams.map(_.exuBlockParams.map(
    x => if (x.hasStoreAddrFu) x.numRegSrc + 1 else x.numRegSrc
  ).max).max

  val numIntStateRead = schdBlockParams.schdType match {
    case IntScheduler() | MemScheduler() => numRegSrc * numIn
    case _ => 0
  }
  val numVfStateRead = schdBlockParams.schdType match {
    case VfScheduler() | MemScheduler() => numRegSrc * numIn
    case _ => 0
  }

  val isMem = schdBlockParams.schdType == MemScheduler()

  lazy val module: Dispatch2IqImp = schdBlockParams.schdType match {
    case IntScheduler() => new Dispatch2IqIntImp(this)(p, schdBlockParams)
    case MemScheduler() => new Dispatch2IqMemImp(this)(p, schdBlockParams)
    case VfScheduler() => new Dispatch2IqArithImp(this)(p, schdBlockParams)
    case _ => null
  }
}

abstract class Dispatch2IqImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val numRegSrc = wrapper.numRegSrc
  val numIntStateRead = wrapper.numIntStateRead
  val numVfStateRead = wrapper.numVfStateRead
  val numIssueBlock = wrapper.issueBlockParams.size

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val readVfState = if (numVfStateRead > 0) Some(Vec(numVfStateRead, Flipped(new BusyTableReadIO))) else None
    val out = MixedVec(params.issueBlockParams.filter(iq => iq.StdCnt == 0).map(x => Vec(x.numEnq, DecoupledIO(new DynInst))))
    val enqLsqIO = if (wrapper.isMem) Some(Flipped(new LsqEnqIO)) else None
    val iqValidCnt = MixedVec(params.issueBlockParams.filter(_.StdCnt == 0).map(x => Input(UInt(log2Ceil(x.numEntries).W))))
    val IQValidNumVec = if (params.isIntSchd) Some(Input(MixedVec(backendParams.genIQValidNumBundle))) else None
  })


  /**
    *
    * @param portFuSets portFuSet(i): the ith port can accept the set including [[FuType]]
    * @return set of the [[FuType]] can deq by port num
    */
  def getFuDeqMap[T](portFuSets: Seq[Set[T]]): Map[T, Seq[Int]] = {
    val res: mutable.Map[T, Seq[Int]] = mutable.Map()
    for ((set, i) <- portFuSets.zipWithIndex) {
      for (fuType <- set) {
        if (res.contains(fuType)) {
          res(fuType) :+= i
        } else {
          res += (fuType -> Seq(i))
        }
      }
    }
    res.toMap
  }

  def mergeFuDeqMap[T](map: Map[T, Seq[Int]]) = {
    val res: mutable.Map[Seq[Int], Seq[T]] = mutable.Map()
    for ((k, v) <- map) {
      if (res.contains(v)) {
        res(v) :+= k
      } else {
        res += (v -> Seq(k))
      }
    }
    res.map(x => (x._2, x._1))
  }

  def expendFuDeqMap[T](map: Map[Seq[T], Seq[Int]], numEnqs: Seq[Int]) = {
    val res: mutable.Map[Seq[T], Seq[Int]] = mutable.Map()
    val portSum: Seq[Int] = numEnqs.indices.map(x => numEnqs.slice(0, x).sum)
    for ((fuType, iqIdxSeq) <- map) {
      val portIdxSeq = iqIdxSeq.flatMap(x => Seq.range(portSum(x), portSum(x) + numEnqs(x)))
      res += (fuType -> portIdxSeq)
    }
    res
  }

  def expendPortSel(map: Map[Seq[Int], Vec[ValidIO[UInt]]]) = {
    val res : mutable.Map[Int, Seq[ValidIO[UInt]]]= mutable.Map()
    for((k, v) <- map) {
      for(i <- 0 until k.size) {
        if(res.contains(k(i))) {
          res(k(i)) :+= v(i)
        } else {
          res += (k(i) -> Seq(v(i)))
        }
      }
    }
    res
  }

  def canAccept(acceptVec: Seq[BigInt], fuType: UInt): Bool = {
    (acceptVec.reduce(_ | _).U & fuType).orR
  }

  def canAccept(acceptVec: Seq[Seq[BigInt]], fuType: UInt): Vec[Bool] = {
    VecInit(acceptVec.map(x => canAccept(x, fuType)).toSeq)
  }

  def filterCanAccept(fuConfigs: Seq[FuConfig], fuType: UInt, canAcceptAlu: Boolean): Bool = {
    if(canAcceptAlu) {
      Cat(fuConfigs.map(_.fuType.U === fuType).toSeq).orR
    }
    else{
      Mux(fuType === FuType.alu.U, false.B, Cat(fuConfigs.map(_.fuType.U === fuType).toSeq).orR)
    }
  }
}

class Dispatch2IqIntImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {
  val IQValidNumVec = io.IQValidNumVec.get
  // numEnq = 4 + 4 + 2
  private val numEnq = io.in.size

  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
  val numInPorts = io.in.size
  val outs = io.out.flatten
  require(outs.size == uopsIn.size, "Dispatch2IqInt outs.size =/= uopsIn.size")
  uopsIn <> io.in
  val uopsInDq0Num = uopsIn.size / 2
  val uopsInDq1Num = uopsIn.size / 2
  val uopsInDq0 = uopsIn.take(uopsInDq0Num)
  val uopsInDq1 = uopsIn.drop(uopsInDq0Num).take(uopsInDq1Num)
  val uopsOutDq0 = outs.take(uopsInDq0Num)
  val uopsOutDq1 = outs.drop(uopsInDq0Num).take(uopsInDq1Num)
  val IQ0Deq0Num = IQValidNumVec(0)(0)
  val IQ0Deq1Num = IQValidNumVec(0)(1)
  val IQ1Deq0Num = IQValidNumVec(1)(0)
  val IQ1Deq1Num = IQValidNumVec(1)(1)
  val IQ2Deq0Num = IQValidNumVec(2)(0)
  val IQ2Deq1Num = IQValidNumVec(2)(1)
  val IQ3Deq0Num = IQValidNumVec(3)(0)
  val IQ3Deq1Num = IQValidNumVec(3)(1)

  val IQ0Deq0IsLess = IQ1Deq0Num > IQ0Deq0Num
  val IQ0Deq1IsLess = IQ1Deq1Num > IQ0Deq1Num
  val diffIQ01Deq0 = Mux(IQ0Deq0IsLess, IQ1Deq0Num - IQ0Deq0Num, IQ0Deq0Num - IQ1Deq0Num)
  val diffIQ01Deq1 = Mux(IQ0Deq1IsLess, IQ1Deq1Num - IQ0Deq1Num, IQ0Deq1Num - IQ1Deq1Num)
  val IQ0Deq0IsLess2 = IQ0Deq0IsLess && (diffIQ01Deq0 >= 2.U)
  val IQ0Deq1IsLess2 = IQ0Deq1IsLess && (diffIQ01Deq1 >= 2.U)
  val IQ1Deq0IsLess2 = !IQ0Deq0IsLess && (diffIQ01Deq0 >= 2.U)
  val IQ1Deq1IsLess2 = !IQ0Deq1IsLess && (diffIQ01Deq1 >= 2.U)
  val IQ0Deq0IsEqual = IQ1Deq0Num === IQ0Deq0Num
  val IQ0Deq1IsEqual = IQ1Deq1Num === IQ0Deq1Num
  val isDq0Deq0 = VecInit(uopsInDq0.map{case in => in.valid && FuType.isIntDq0Deq0(in.bits.fuType)})
  val isDq0Deq1 = VecInit(uopsInDq0.map{case in => in.valid && FuType.isIntDq0Deq1(in.bits.fuType)})
  val popDq0Deq0 = isDq0Deq0.zipWithIndex.map { case (_, i) => PopCount(isDq0Deq0.take(i + 1)) }
  val popDq0Deq1 = isDq0Deq1.zipWithIndex.map { case (_, i) => PopCount(isDq0Deq1.take(i + 1)) }
  val isDq0Deq0Num = popDq0Deq0.last
  val isDq0Deq1Num = popDq0Deq1.last
  val IQ0Deq0IsLessSeq = VecInit(popDq0Deq0.map(p => (p <= diffIQ01Deq0) && IQ0Deq0IsLess))
  val IQ0Deq1IsLessSeq = VecInit(popDq0Deq1.map(p => (p <= diffIQ01Deq1) && IQ0Deq1IsLess))
  val lastAlu0SelectIQ0Enq0 = RegInit(false.B)
  val lastBrh0SelectIQ0Enq0 = RegInit(false.B)
  when(uopsOutDq0.head.ready && uopsOutDq0.last.ready && IQ0Deq0IsEqual && (popDq0Deq0(3)(0) === 1.U)) {
    lastAlu0SelectIQ0Enq0 := !lastAlu0SelectIQ0Enq0
  }
  when(uopsOutDq0.head.ready && uopsOutDq0.last.ready && IQ0Deq1IsEqual && (popDq0Deq1(3)(0) === 1.U)) {
    lastBrh0SelectIQ0Enq0 := !lastBrh0SelectIQ0Enq0
  }
  val IQ0FuCfgs = params.issueBlockParams(0).getFuCfgs
  val IQ1FuCfgs = params.issueBlockParams(1).getFuCfgs
  val IQ01BothfuCfgs = IQ0FuCfgs.intersect(IQ1FuCfgs)
  val IQ0FuTypes = IQ0FuCfgs.map(_.fuType)
  val IQ1FuTypes = IQ1FuCfgs.map(_.fuType)
  val IQ0OnlyFuTypes = IQ0FuCfgs.diff(IQ1FuCfgs).map(_.fuType)
  val IQ1OnlyFuTypes = IQ1FuCfgs.diff(IQ0FuCfgs).map(_.fuType)
  val isOnlyIQ0 = uopsInDq0.map(x => x.valid && FuTypeOrR(x.bits.fuType, IQ0OnlyFuTypes))
  val isOnlyIQ1 = uopsInDq0.map(x => x.valid && FuTypeOrR(x.bits.fuType, IQ1OnlyFuTypes))
  val isBothIQ01_0 = uopsInDq0(0).valid && FuTypeOrR(uopsInDq0(0).bits.fuType, IQ01BothfuCfgs.map(_.fuType))
  val isBothIQ01_1 = uopsInDq0(1).valid && FuTypeOrR(uopsInDq0(1).bits.fuType, IQ01BothfuCfgs.map(_.fuType))
  val alu0SelectIQ0Enq0 = Mux(IQ0Deq0IsEqual, !lastAlu0SelectIQ0Enq0, IQ0Deq0IsLess)
  val brh0SelectIQ0Enq0 = Mux(IQ0Deq1IsEqual, !lastBrh0SelectIQ0Enq0, IQ0Deq1IsLess)
  val uop0SelectIQ0Enq0 = Mux(isDq0Deq0(0), alu0SelectIQ0Enq0, brh0SelectIQ0Enq0) || isOnlyIQ0(0)
  val alu1SelectIQ0Enq0 = !uop0SelectIQ0Enq0 && (IQ0Deq0IsLess2 || Mux(IQ0Deq0IsEqual, Mux(popDq0Deq0(1) === 1.U, !lastAlu0SelectIQ0Enq0, lastAlu0SelectIQ0Enq0), isDq0Deq0(0) || IQ0Deq0IsLess))
  val brh1SelectIQ0Enq0 = !uop0SelectIQ0Enq0 && (IQ0Deq1IsLess2 || Mux(IQ0Deq1IsEqual, Mux(popDq0Deq1(1) === 1.U, !lastBrh0SelectIQ0Enq0, lastBrh0SelectIQ0Enq0), isDq0Deq1(0) || IQ0Deq1IsLess))
  val uop1SelectIQ0Enq0 = Mux(isDq0Deq0(1), alu1SelectIQ0Enq0, brh1SelectIQ0Enq0)
  val alu1SelectIQ0Enq1 = isDq0Deq0(0) && IQ0Deq0IsLess2 || isDq0Deq1(0) && IQ0Deq0IsLess
  val brh1SelectIQ0Enq1 = isDq0Deq1(0) && IQ0Deq1IsLess2 || isDq0Deq0(0) && IQ0Deq1IsLess
  val uop1SelectIQ0Enq1 = uop0SelectIQ0Enq0 && Mux(isDq0Deq0(1), alu1SelectIQ0Enq1, brh1SelectIQ0Enq1)
  val alu1SelectIQ1Enq0 = uop0SelectIQ0Enq0 && (IQ1Deq0IsLess2 || Mux(IQ0Deq0IsEqual, Mux(popDq0Deq0(1) === 1.U, lastAlu0SelectIQ0Enq0, !lastAlu0SelectIQ0Enq0), isDq0Deq0(0) || !IQ0Deq0IsLess))
  val brh1SelectIQ1Enq0 = uop0SelectIQ0Enq0 && (IQ1Deq1IsLess2 || Mux(IQ0Deq1IsEqual, Mux(popDq0Deq1(1) === 1.U, lastBrh0SelectIQ0Enq0, !lastBrh0SelectIQ0Enq0), isDq0Deq1(0) || !IQ0Deq1IsLess))
  val uop1SelectIQ1Enq0 = !uop1SelectIQ0Enq0 && !uop1SelectIQ0Enq1 && Mux(isDq0Deq0(1), alu1SelectIQ1Enq0, brh1SelectIQ1Enq0)
  val uop1SelectIQ1Enq1 = !uop1SelectIQ0Enq0 && !uop1SelectIQ0Enq1 && !uop1SelectIQ1Enq0
  val alu2SelectIQ0Enq1 = Mux(popDq0Deq0(2) === 2.U, Mux(IQ0Deq0IsLess2, true.B, isDq0Deq0(0) && !alu0SelectIQ0Enq0 || isDq0Deq1(0) && brh0SelectIQ0Enq0), alu0SelectIQ0Enq0)
  val brh2SelectIQ0Enq1 = Mux(popDq0Deq1(2) === 2.U, Mux(IQ0Deq1IsLess2, true.B, isDq0Deq1(0) && !brh0SelectIQ0Enq0 || isDq0Deq0(0) && alu0SelectIQ0Enq0), brh0SelectIQ0Enq0)
  val uop2SelectIQ0Enq1 = Mux(isDq0Deq0(2), alu2SelectIQ0Enq1, brh2SelectIQ0Enq1) || isOnlyIQ0(2)
  val IQ0Enq0Select = Wire(Vec(4, Bool()))
  val IQ0Enq1Select = Wire(Vec(4, Bool()))
  val IQ1Enq0Select = Wire(Vec(4, Bool()))
  val IQ1Enq1Select = Wire(Vec(4, Bool()))
  val uopsInDq0Valid = VecInit(uopsInDq0.map(_.valid))
  val IQ0DeqIsLess = VecInit(isDq0Deq0.map(x => Mux(x, Mux(IQ0Deq0IsEqual, IQ0Deq1IsLess, IQ0Deq0IsLess),  Mux(IQ0Deq1IsEqual, IQ0Deq0IsLess, IQ0Deq1IsLess))))
  val IQ0DeqIsEqual = IQ0Deq0IsEqual && IQ0Deq1IsEqual
  val IQ0Enq0Select0 = isOnlyIQ0(0) || isBothIQ01_0 && uop0SelectIQ0Enq0
  val IQ1Enq0Select0 = !IQ0Enq0Select0 && (isOnlyIQ1(0) || isBothIQ01_0 && uopsInDq0(0).valid)
  val IQ0Enq0Select1 = !IQ0Enq0Select0 && (isOnlyIQ0(1) || isBothIQ01_1 && (uop1SelectIQ0Enq0 || isOnlyIQ1(0)))
  val IQ0Enq1Select1 = !IQ0Enq0Select1 && (isOnlyIQ0(1) || isBothIQ01_1 && uop1SelectIQ0Enq1)
  val IQ1Enq0Select1 = !IQ0Enq0Select1 && !IQ0Enq1Select1 && !IQ1Enq0Select0 && (isOnlyIQ1(1) || isBothIQ01_1 && (uop1SelectIQ1Enq0 || isOnlyIQ0(0)))
  val IQ1Enq1Select1 = !IQ0Enq0Select1 && !IQ0Enq1Select1 && !IQ1Enq0Select1 && (isOnlyIQ1(1) || isBothIQ01_1 && uop1SelectIQ1Enq1)
  val IQ0Enq0Select2 = !IQ0Enq0Select0 && !IQ0Enq0Select1 && uopsInDq0(2).valid
  val IQ0Enq1Select2 = !IQ0Enq1Select1 && !IQ0Enq0Select2 && uopsInDq0(2).valid && uop2SelectIQ0Enq1
  val IQ1Enq0Select2 = !IQ1Enq0Select0 && !IQ1Enq0Select1 && !IQ0Enq0Select2 && !IQ0Enq1Select2 && uopsInDq0(2).valid
  val IQ1Enq1Select2 = !IQ0Enq0Select2 && !IQ0Enq1Select2 && !IQ1Enq0Select2 && uopsInDq0(2).valid
  val IQ0Enq1Select3 = !IQ0Enq1Select1 && !IQ0Enq1Select2 && uopsInDq0(3).valid
  val IQ1Enq1Select3 = !IQ1Enq1Select1 && !IQ1Enq1Select2 && uopsInDq0(3).valid
  when(uopsOutDq0.head.ready && uopsOutDq0.last.ready){
    IQ0Enq0Select := Cat(false.B, IQ0Enq0Select2, IQ0Enq0Select1, IQ0Enq0Select0).asBools
    IQ0Enq1Select := Cat(IQ0Enq1Select3, IQ0Enq1Select2, IQ0Enq1Select1, false.B).asBools
    IQ1Enq0Select := Cat(false.B, IQ1Enq0Select2, IQ1Enq0Select1, IQ1Enq0Select0).asBools
    IQ1Enq1Select := Cat(IQ1Enq1Select3, IQ1Enq1Select2, IQ1Enq1Select1, false.B).asBools
  }.elsewhen(uopsOutDq0.head.ready){
    IQ0Enq0Select := Cat(false.B, false.B, false.B, true.B).asBools
    IQ0Enq1Select := Cat(false.B, false.B, true.B, false.B).asBools
    IQ1Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ1Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
  }.elsewhen(uopsOutDq0.last.ready){
    IQ0Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ0Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ1Enq0Select := Cat(false.B, false.B, false.B, true.B).asBools
    IQ1Enq1Select := Cat(false.B, false.B, true.B, false.B).asBools
  }.otherwise{
    IQ0Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ0Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ1Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ1Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
  }
  assert(PopCount(IQ0Enq0Select) < 2.U, "IQ0Enq0Select is not one hot")
  assert(PopCount(IQ0Enq1Select) < 2.U, "IQ0Enq1Select is not one hot")
  assert(PopCount(IQ1Enq0Select) < 2.U, "IQ1Enq0Select is not one hot")
  assert(PopCount(IQ1Enq1Select) < 2.U, "IQ1Enq1Select is not one hot")
  assert(PopCount(IQ0Enq0Select) < 2.U, "IQ0Enq0Select is not one hot")
  assert(PopCount(IQ0Enq1Select) < 2.U, "IQ0Enq1Select is not one hot")
  assert(PopCount(IQ1Enq0Select) < 2.U, "IQ1Enq0Select is not one hot")
  assert(PopCount(IQ1Enq1Select) < 2.U, "IQ1Enq1Select is not one hot")
  assert(PopCount(VecInit(IQ0Enq0Select(0), IQ0Enq1Select(0), IQ1Enq0Select(0), IQ1Enq1Select(0))) < 2.U, "Dq1Uop0Select is not one hot")
  assert(PopCount(VecInit(IQ0Enq0Select(1), IQ0Enq1Select(1), IQ1Enq0Select(1), IQ1Enq1Select(1))) < 2.U, "Dq1Uop1Select is not one hot")
  assert(PopCount(VecInit(IQ0Enq0Select(2), IQ0Enq1Select(2), IQ1Enq0Select(2), IQ1Enq1Select(2))) < 2.U, "Dq1Uop2Select is not one hot")
  assert(PopCount(VecInit(IQ0Enq0Select(3), IQ0Enq1Select(3), IQ1Enq0Select(3), IQ1Enq1Select(3))) < 2.U, "Dq1Uop3Select is not one hot")
  val dq0Block = Wire(Vec(uopsInDq0Num, Bool()))
  if (IQ0FuCfgs.size == IQ1FuCfgs.size && IQ0FuCfgs.size == IQ01BothfuCfgs.size) {
    dq0Block := Cat(false.B, false.B, false.B, false.B).asBools
  }
  else {
    val fuTypes = uopsInDq0.map(_.bits.fuType)
    val iqNotReady0 = (FuTypeOrR(fuTypes(0),IQ0OnlyFuTypes) && !uopsOutDq0.head.ready) || (FuTypeOrR(fuTypes(0),IQ1OnlyFuTypes) && !uopsOutDq0.last.ready)
    val iqNotReady1 = (FuTypeOrR(fuTypes(1),IQ0OnlyFuTypes) && !uopsOutDq0.head.ready) || (FuTypeOrR(fuTypes(1),IQ1OnlyFuTypes) && !uopsOutDq0.last.ready)
    val iqNotReady2 = (FuTypeOrR(fuTypes(2),IQ0OnlyFuTypes) && !uopsOutDq0.head.ready) || (FuTypeOrR(fuTypes(2),IQ1OnlyFuTypes) && !uopsOutDq0.last.ready)
    val iqNotReady3 = (FuTypeOrR(fuTypes(3),IQ0OnlyFuTypes) && !uopsOutDq0.head.ready) || (FuTypeOrR(fuTypes(3),IQ1OnlyFuTypes) && !uopsOutDq0.last.ready)
    val conflict2 = ( (IQ0Enq0Select(2) || IQ0Enq1Select(2)) && FuTypeOrR(fuTypes(2),IQ1OnlyFuTypes)) || ( (IQ1Enq0Select(2) || IQ1Enq1Select(2)) && FuTypeOrR(fuTypes(2),IQ0OnlyFuTypes))
    val conflict3 = ( (IQ0Enq0Select(3) || IQ0Enq1Select(3)) && FuTypeOrR(fuTypes(3),IQ1OnlyFuTypes)) || ( (IQ1Enq0Select(3) || IQ1Enq1Select(3)) && FuTypeOrR(fuTypes(3),IQ0OnlyFuTypes))
    dq0Block(0) := iqNotReady0
    dq0Block(1) := dq0Block(0) || iqNotReady1
    dq0Block(2) := dq0Block(0) || dq0Block(1) || conflict2
    dq0Block(3) := dq0Block(0) || dq0Block(1) || dq0Block(2) || conflict3
  }
  uopsInDq0.zipWithIndex.map { case (u, i) =>
    u.ready := (IQ0Enq0Select(i) || IQ0Enq1Select(i) || IQ1Enq0Select(i) || IQ1Enq1Select(i)) && !dq0Block(i)
  }
  uopsOutDq0(0).bits :=  Mux1H(IQ0Enq0Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.bits))
  uopsOutDq0(1).bits :=  Mux1H(IQ0Enq1Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.bits))
  uopsOutDq0(2).bits :=  Mux1H(IQ1Enq0Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.bits))
  uopsOutDq0(3).bits :=  Mux1H(IQ1Enq1Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.bits))
  uopsOutDq0(0).valid := Mux1H(IQ0Enq0Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.valid))
  uopsOutDq0(1).valid := Mux1H(IQ0Enq1Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.valid))
  uopsOutDq0(2).valid := Mux1H(IQ1Enq0Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.valid))
  uopsOutDq0(3).valid := Mux1H(IQ1Enq1Select.zip(dq0Block).map(x => x._1 && !x._2), uopsInDq0.map(_.valid))


  val IQ2Deq0IsLess = IQ3Deq0Num > IQ2Deq0Num
  val IQ2Deq1IsLess = IQ3Deq1Num > IQ2Deq1Num
  val diffIQ23Deq0 = Mux(IQ2Deq0IsLess, IQ3Deq0Num - IQ2Deq0Num, IQ2Deq0Num - IQ3Deq0Num)
  val diffIQ23Deq1 = Mux(IQ2Deq1IsLess, IQ3Deq1Num - IQ2Deq1Num, IQ2Deq1Num - IQ3Deq1Num)
  val IQ2Deq0IsLess2 = IQ2Deq0IsLess && (diffIQ23Deq0 >= 2.U)
  val IQ2Deq1IsLess2 = IQ2Deq1IsLess && (diffIQ23Deq1 >= 2.U)
  val IQ3Deq0IsLess2 = !IQ2Deq0IsLess && (diffIQ23Deq0 >= 2.U)
  val IQ3Deq1IsLess2 = !IQ2Deq1IsLess && (diffIQ23Deq1 >= 2.U)
  val IQ2Deq0IsEqual = IQ3Deq0Num === IQ2Deq0Num
  val IQ2Deq1IsEqual = IQ3Deq1Num === IQ2Deq1Num
  val isDq1Deq0 = VecInit(uopsInDq1.map { case in => in.valid && FuType.isIntDq1Deq0(in.bits.fuType) })
  val isDq1Deq1 = VecInit(uopsInDq1.map { case in => in.valid && FuType.isIntDq1Deq1(in.bits.fuType) })
  val popDq1Deq0 = isDq1Deq0.zipWithIndex.map { case (_, i) => PopCount(isDq1Deq0.take(i + 1)) }
  val popDq1Deq1 = isDq1Deq1.zipWithIndex.map { case (_, i) => PopCount(isDq1Deq1.take(i + 1)) }
  val isDq1Deq0Num = popDq1Deq0.last
  val isDq1Deq1Num = popDq1Deq1.last
  val IQ2Deq0IsLessSeq = VecInit(popDq1Deq0.map(p => (p <= diffIQ23Deq0) && IQ2Deq0IsLess))
  val IQ2Deq1IsLessSeq = VecInit(popDq1Deq1.map(p => (p <= diffIQ23Deq1) && IQ2Deq1IsLess))
  val lastAlu0SelectIQ2Enq0 = RegInit(false.B)
  val lastBrh0SelectIQ2Enq0 = RegInit(false.B)
  when(uopsOutDq1.head.ready && uopsOutDq1.last.ready && IQ2Deq0IsEqual && (popDq1Deq0(3)(0) === 1.U)) {
    lastAlu0SelectIQ2Enq0 := !lastAlu0SelectIQ2Enq0
  }
  when(uopsOutDq1.head.ready && uopsOutDq1.last.ready && IQ2Deq1IsEqual && (popDq1Deq1(3)(0) === 1.U)) {
    lastBrh0SelectIQ2Enq0 := !lastBrh0SelectIQ2Enq0
  }
  val IQ2FuCfgs = params.issueBlockParams(2).getFuCfgs
  val IQ3FuCfgs = params.issueBlockParams(3).getFuCfgs
  val IQ23BothfuCfgs = IQ2FuCfgs.intersect(IQ3FuCfgs)
  val IQ2FuTypes = IQ2FuCfgs.map(_.fuType)
  val IQ3FuTypes = IQ3FuCfgs.map(_.fuType)
  val IQ2OnlyFuTypes = IQ2FuCfgs.diff(IQ3FuCfgs).map(_.fuType)
  val IQ3OnlyFuTypes = IQ3FuCfgs.diff(IQ2FuCfgs).map(_.fuType)
  val isOnlyIQ2 = uopsInDq1.map(x => x.valid && FuTypeOrR(x.bits.fuType, IQ2OnlyFuTypes))
  val isOnlyIQ3 = uopsInDq1.map(x => x.valid && FuTypeOrR(x.bits.fuType, IQ3OnlyFuTypes))
  val isBothIQ23_0 = uopsInDq1(0).valid && FuTypeOrR(uopsInDq1(0).bits.fuType, IQ23BothfuCfgs.map(_.fuType))
  val isBothIQ23_1 = uopsInDq1(1).valid && FuTypeOrR(uopsInDq1(1).bits.fuType, IQ23BothfuCfgs.map(_.fuType))
  val alu0SelectIQ2Enq0 = Mux(IQ2Deq0IsEqual, !lastAlu0SelectIQ2Enq0, IQ2Deq0IsLess)
  val brh0SelectIQ2Enq0 = Mux(IQ2Deq1IsEqual, !lastBrh0SelectIQ2Enq0, IQ2Deq1IsLess)
  val uop0SelectIQ2Enq0 = Mux(isDq1Deq0(0), alu0SelectIQ2Enq0, brh0SelectIQ2Enq0) || isOnlyIQ2(0)
  val alu1SelectIQ2Enq0 = !uop0SelectIQ2Enq0 && (IQ2Deq0IsLess2 || Mux(IQ2Deq0IsEqual, Mux(popDq1Deq0(1) === 1.U, !lastAlu0SelectIQ2Enq0, lastAlu0SelectIQ2Enq0), isDq1Deq0(0) || IQ2Deq0IsLess))
  val brh1SelectIQ2Enq0 = !uop0SelectIQ2Enq0 && (IQ2Deq1IsLess2 || Mux(IQ2Deq1IsEqual, Mux(popDq1Deq1(1) === 1.U, !lastBrh0SelectIQ2Enq0, lastBrh0SelectIQ2Enq0), isDq1Deq1(0) || IQ2Deq1IsLess))
  val uop1SelectIQ2Enq0 = Mux(isDq1Deq0(1), alu1SelectIQ2Enq0, brh1SelectIQ2Enq0)
  val alu1SelectIQ2Enq1 = isDq1Deq0(0) && IQ2Deq0IsLess2 || isDq1Deq1(0) && IQ2Deq0IsLess
  val brh1SelectIQ2Enq1 = isDq1Deq1(0) && IQ2Deq1IsLess2 || isDq1Deq0(0) && IQ2Deq1IsLess
  val uop1SelectIQ2Enq1 = uop0SelectIQ2Enq0 && Mux(isDq1Deq0(1), alu1SelectIQ2Enq1, brh1SelectIQ2Enq1)
  val alu1SelectIQ3Enq0 = uop0SelectIQ2Enq0 && (IQ3Deq0IsLess2 || Mux(IQ2Deq0IsEqual, Mux(popDq1Deq0(1) === 1.U, lastAlu0SelectIQ2Enq0, !lastAlu0SelectIQ2Enq0), isDq1Deq0(0) || !IQ2Deq0IsLess))
  val brh1SelectIQ3Enq0 = uop0SelectIQ2Enq0 && (IQ3Deq1IsLess2 || Mux(IQ2Deq1IsEqual, Mux(popDq1Deq1(1) === 1.U, lastBrh0SelectIQ2Enq0, !lastBrh0SelectIQ2Enq0), isDq1Deq1(0) || !IQ2Deq1IsLess))
  val uop1SelectIQ3Enq0 = !uop1SelectIQ2Enq0 && !uop1SelectIQ2Enq1 && Mux(isDq1Deq0(1), alu1SelectIQ3Enq0, brh1SelectIQ3Enq0)
  val uop1SelectIQ3Enq1 = !uop1SelectIQ2Enq0 && !uop1SelectIQ2Enq1 && !uop1SelectIQ3Enq0
  val alu2SelectIQ2Enq1 = Mux(popDq1Deq0(2) === 2.U, Mux(IQ2Deq0IsLess2, true.B, isDq1Deq0(0) && !alu0SelectIQ2Enq0 || isDq1Deq1(0) && brh0SelectIQ2Enq0), alu0SelectIQ2Enq0)
  val brh2SelectIQ2Enq1 = Mux(popDq1Deq1(2) === 2.U, Mux(IQ2Deq1IsLess2, true.B, isDq1Deq1(0) && !brh0SelectIQ2Enq0 || isDq1Deq0(0) && alu0SelectIQ2Enq0), brh0SelectIQ2Enq0)
  val uop2SelectIQ2Enq1 = Mux(isDq1Deq0(2), alu2SelectIQ2Enq1, brh2SelectIQ2Enq1) || isOnlyIQ2(2)
  val IQ2Enq0Select = Wire(Vec(4, Bool()))
  val IQ2Enq1Select = Wire(Vec(4, Bool()))
  val IQ3Enq0Select = Wire(Vec(4, Bool()))
  val IQ3Enq1Select = Wire(Vec(4, Bool()))
  val IQ2Enq0Select0 = isOnlyIQ2(0) || isBothIQ23_0 && uop0SelectIQ2Enq0
  val IQ3Enq0Select0 = !IQ2Enq0Select0 && (isOnlyIQ3(0) || isBothIQ23_0 && uopsInDq1(0).valid)
  val IQ2Enq0Select1 = !IQ2Enq0Select0 && (isOnlyIQ2(1) || isBothIQ23_1 && (uop1SelectIQ2Enq0 || isOnlyIQ3(0)))
  val IQ2Enq1Select1 = !IQ2Enq0Select1 && (isOnlyIQ2(1) || isBothIQ23_1 && uop1SelectIQ2Enq1)
  val IQ3Enq0Select1 = !IQ2Enq0Select1 && !IQ2Enq1Select1 && !IQ3Enq0Select0 && (isOnlyIQ3(1) || isBothIQ23_1 && (uop1SelectIQ3Enq0 || isOnlyIQ3(0)))
  val IQ3Enq1Select1 = !IQ2Enq0Select1 && !IQ2Enq1Select1 && !IQ3Enq0Select1 && (isOnlyIQ3(1) || isBothIQ23_1 && uop1SelectIQ3Enq1)
  val IQ2Enq0Select2 = !IQ2Enq0Select0 && !IQ2Enq0Select1 && uopsInDq1(2).valid
  val IQ2Enq1Select2 = !IQ2Enq1Select1 && !IQ2Enq0Select2 && uopsInDq1(2).valid && uop2SelectIQ2Enq1
  val IQ3Enq0Select2 = !IQ3Enq0Select0 && !IQ3Enq0Select1 && !IQ2Enq0Select2 && !IQ2Enq1Select2 && uopsInDq1(2).valid
  val IQ3Enq1Select2 = !IQ2Enq0Select2 && !IQ2Enq1Select2 && !IQ3Enq0Select2 && uopsInDq1(2).valid
  val IQ2Enq1Select3 = !IQ2Enq1Select1 && !IQ2Enq1Select2 && uopsInDq1(3).valid
  val IQ3Enq1Select3 = !IQ3Enq1Select1 && !IQ3Enq1Select2 && uopsInDq1(3).valid
  when(uopsOutDq1.head.ready && uopsOutDq1.last.ready) {
    IQ2Enq0Select := Cat(false.B, IQ2Enq0Select2, IQ2Enq0Select1, IQ2Enq0Select0).asBools
    IQ2Enq1Select := Cat(IQ2Enq1Select3, IQ2Enq1Select2, IQ2Enq1Select1, false.B).asBools
    IQ3Enq0Select := Cat(false.B, IQ3Enq0Select2, IQ3Enq0Select1, IQ3Enq0Select0).asBools
    IQ3Enq1Select := Cat(IQ3Enq1Select3, IQ3Enq1Select2, IQ3Enq1Select1, false.B).asBools
  }.elsewhen(uopsOutDq1.head.ready) {
    IQ2Enq0Select := Cat(false.B, false.B, false.B, true.B).asBools
    IQ2Enq1Select := Cat(false.B, false.B, true.B, false.B).asBools
    IQ3Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ3Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
  }.elsewhen(uopsOutDq1.last.ready) {
    IQ2Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ2Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ3Enq0Select := Cat(false.B, false.B, false.B, true.B).asBools
    IQ3Enq1Select := Cat(false.B, false.B, true.B, false.B).asBools
  }.otherwise {
    IQ2Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ2Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ3Enq0Select := Cat(false.B, false.B, false.B, false.B).asBools
    IQ3Enq1Select := Cat(false.B, false.B, false.B, false.B).asBools
  }
  assert(PopCount(IQ2Enq0Select) < 2.U, "IQ2Enq0Select is not one hot")
  assert(PopCount(IQ2Enq1Select) < 2.U, "IQ2Enq1Select is not one hot")
  assert(PopCount(IQ3Enq0Select) < 2.U, "IQ3Enq0Select is not one hot")
  assert(PopCount(IQ3Enq1Select) < 2.U, "IQ3Enq1Select is not one hot")
  assert(PopCount(IQ2Enq0Select) < 2.U, "IQ2Enq0Select is not one hot")
  assert(PopCount(IQ2Enq1Select) < 2.U, "IQ2Enq1Select is not one hot")
  assert(PopCount(IQ3Enq0Select) < 2.U, "IQ3Enq0Select is not one hot")
  assert(PopCount(IQ3Enq1Select) < 2.U, "IQ3Enq1Select is not one hot")
  assert(PopCount(VecInit(IQ2Enq0Select(0), IQ2Enq1Select(0), IQ3Enq0Select(0), IQ3Enq1Select(0))) < 2.U, "Dq1Uop0Select is not one hot")
  assert(PopCount(VecInit(IQ2Enq0Select(1), IQ2Enq1Select(1), IQ3Enq0Select(1), IQ3Enq1Select(1))) < 2.U, "Dq1Uop1Select is not one hot")
  assert(PopCount(VecInit(IQ2Enq0Select(2), IQ2Enq1Select(2), IQ3Enq0Select(2), IQ3Enq1Select(2))) < 2.U, "Dq1Uop2Select is not one hot")
  assert(PopCount(VecInit(IQ2Enq0Select(3), IQ2Enq1Select(3), IQ3Enq0Select(3), IQ3Enq1Select(3))) < 2.U, "Dq1Uop3Select is not one hot")
  val dq1Block = Wire(Vec(uopsInDq1Num, Bool()))
  if (IQ2FuCfgs.size == IQ3FuCfgs.size && IQ2FuCfgs.size == IQ23BothfuCfgs.size) {
    dq1Block := Cat(false.B, false.B, false.B, false.B).asBools
  }
  else {
    val fuTypes = uopsInDq1.map(_.bits.fuType)
    val iqNotReady0 = (FuTypeOrR(fuTypes(0), IQ2OnlyFuTypes) && !uopsOutDq1.head.ready) || (FuTypeOrR(fuTypes(0), IQ3OnlyFuTypes) && !uopsOutDq1.last.ready)
    val iqNotReady1 = (FuTypeOrR(fuTypes(1), IQ2OnlyFuTypes) && !uopsOutDq1.head.ready) || (FuTypeOrR(fuTypes(1), IQ3OnlyFuTypes) && !uopsOutDq1.last.ready)
    val iqNotReady2 = (FuTypeOrR(fuTypes(2), IQ2OnlyFuTypes) && !uopsOutDq1.head.ready) || (FuTypeOrR(fuTypes(2), IQ3OnlyFuTypes) && !uopsOutDq1.last.ready)
    val iqNotReady3 = (FuTypeOrR(fuTypes(3), IQ2OnlyFuTypes) && !uopsOutDq1.head.ready) || (FuTypeOrR(fuTypes(3), IQ3OnlyFuTypes) && !uopsOutDq1.last.ready)
    val conflict2 = ( (IQ2Enq0Select(2) || IQ2Enq1Select(2)) && FuTypeOrR(fuTypes(2), IQ3OnlyFuTypes)) || ( (IQ3Enq0Select(2) || IQ3Enq1Select(2)) && FuTypeOrR(fuTypes(2), IQ2OnlyFuTypes))
    val conflict3 = ( (IQ2Enq0Select(3) || IQ2Enq1Select(3)) && FuTypeOrR(fuTypes(3), IQ3OnlyFuTypes)) || ( (IQ3Enq0Select(3) || IQ3Enq1Select(3)) && FuTypeOrR(fuTypes(3), IQ2OnlyFuTypes))
    dq1Block(0) := iqNotReady0
    dq1Block(1) := dq1Block(0) || iqNotReady1
    dq1Block(2) := dq1Block(0) || dq1Block(1) || conflict2
    dq1Block(3) := dq1Block(0) || dq1Block(1) || dq1Block(2) || conflict3
  }
  uopsInDq1.zipWithIndex.map { case (u, i) =>
    u.ready := (IQ2Enq0Select(i) || IQ2Enq1Select(i) || IQ3Enq0Select(i) || IQ3Enq1Select(i)) && !dq1Block(i)
  }
  uopsOutDq1(0).bits :=  Mux1H(IQ2Enq0Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.bits))
  uopsOutDq1(1).bits :=  Mux1H(IQ2Enq1Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.bits))
  uopsOutDq1(2).bits :=  Mux1H(IQ3Enq0Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.bits))
  uopsOutDq1(3).bits :=  Mux1H(IQ3Enq1Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.bits))
  uopsOutDq1(0).valid := Mux1H(IQ2Enq0Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.valid))
  uopsOutDq1(1).valid := Mux1H(IQ2Enq1Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.valid))
  uopsOutDq1(2).valid := Mux1H(IQ3Enq0Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.valid))
  uopsOutDq1(3).valid := Mux1H(IQ3Enq1Select.zip(dq1Block).map(x => x._1 && !x._2), uopsInDq1.map(_.valid))


  private val reqPsrcVec: IndexedSeq[UInt] = uopsIn.flatMap(in => in.bits.psrc.take(numRegSrc))
  private val intSrcStateVec = if (io.readIntState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val vfSrcStateVec  = if (io.readVfState.isDefined)  Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val intSrcLoadDependency = OptionWrapper(io.readIntState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  private val vfSrcLoadDependency = OptionWrapper(io.readVfState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    require(io.readIntState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readIntState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readIntState.get.map(_.loadDependency).zip(intSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }

  uopsIn
    .flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq)) zip vfSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
    )
    .foreach {
      case ((state: UInt, srcType), (intState, vfState)) =>
        state := Mux1H(Seq(
          SrcType.isXp(srcType) -> intState,
          SrcType.isVfp(srcType) -> vfState,
          SrcType.isNotReg(srcType) -> true.B,
        ))
    }
  uopsIn
    .flatMap(x => x.bits.srcLoadDependency.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq)) zip vfSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq))
    )
    .foreach {
      case ((ldp, srcType), (intLdp, vfLdp)) =>
        when(SrcType.isXp(srcType)) {
          ldp := intLdp
        }.elsewhen(SrcType.isVfp(srcType)) {
          ldp := vfLdp
        }.otherwise {
          ldp := 0.U.asTypeOf(ldp)
        }
    }

  XSPerfAccumulate("not_ready_iq0", PopCount(!uopsOutDq0(0).ready))
  XSPerfAccumulate("not_ready_iq1", PopCount(!uopsOutDq0(2).ready))
  XSPerfAccumulate("not_ready_iq2", PopCount(!uopsOutDq1(0).ready))
  XSPerfAccumulate("not_ready_iq3", PopCount(!uopsOutDq1(2).ready))
  XSPerfAccumulate("not_ready_iq01", PopCount(!uopsOutDq0(0).ready && !uopsOutDq0(2).ready))
  XSPerfAccumulate("not_ready_iq23", PopCount(!uopsOutDq1(0).ready && !uopsOutDq1(2).ready))
  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("out_valid", PopCount(io.out.flatMap(_.map(_.valid))))
  XSPerfAccumulate("out_fire", PopCount(io.out.flatMap(_.map(_.fire))))
}

class Dispatch2IqArithImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  private val numEnq = io.in.size

  val portFuSets = params.issueBlockParams.map(_.exuBlockParams.flatMap(_.fuConfigs).map(_.fuType).toSet)
  println(s"[Dispatch2IqArithImp] portFuSets: $portFuSets")
  val fuDeqMap = getFuDeqMap(portFuSets)
  println(s"[Dispatch2IqArithImp] fuDeqMap: $fuDeqMap")
  val mergedFuDeqMap = mergeFuDeqMap(fuDeqMap)
  println(s"[Dispatch2IqArithImp] mergedFuDeqMap: $mergedFuDeqMap")
  val expendedFuDeqMap = expendFuDeqMap(mergedFuDeqMap, params.issueBlockParams.map(_.numEnq))
  println(s"[Dispatch2IqArithImp] expendedFuDeqMap: $expendedFuDeqMap")

  // sort by count of port. Port less, priority higher.
  val finalFuDeqMap = expendedFuDeqMap.toSeq.sortBy(_._2.length)
  println(s"[Dispatch2IqArithImp] finalFuDeqMap: $finalFuDeqMap")

  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
  val numInPorts = io.in.size
  val outs = io.out.flatten
  val outReadyMatrix = Wire(Vec(outs.size, Vec(numInPorts, Bool())))
  outReadyMatrix.foreach(_.foreach(_ := false.B))
  val selIdxOH = Wire(MixedVec(finalFuDeqMap.map(x => Vec(x._2.size, ValidIO(UInt(uopsIn.size.W))))))
  selIdxOH.foreach(_.foreach(_ := 0.U.asTypeOf(ValidIO(UInt(uopsIn.size.W)))))

  finalFuDeqMap.zipWithIndex.foreach { case ((fuTypeSeq, deqPortIdSeq), i) =>
    val maxSelNum = wrapper.numIn
    val selNum = deqPortIdSeq.length
    val portReadyVec = deqPortIdSeq.map(x => outs(x).ready)
    val canAcc = uopsIn.map(in => canAccept(fuTypeSeq.map(x => x.ohid), in.bits.fuType) && in.valid)
    if(selNum <= maxSelNum) {
      val selPort = SelectOne("circ", portReadyVec.toSeq, selNum)
      val select = SelectOne("naive", canAcc, selNum)
      for ((portId, j) <- deqPortIdSeq.zipWithIndex) {
        val (selPortReady, selPortIdxOH) = selPort.getNthOH(j + 1)
        val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
        when(selPortReady && selectValid) {
          selIdxOH(i)(OHToUInt(selPortIdxOH)).valid := selectValid
          selIdxOH(i)(OHToUInt(selPortIdxOH)).bits := selectIdxOH.asUInt
        }
      }
    } else {
      val selPort = SelectOne("circ", portReadyVec.toSeq, maxSelNum)
      val select = SelectOne("naive", canAcc, maxSelNum)
      for(j <- 0 until maxSelNum) {
        val (selPortReady, selPortIdxOH) = selPort.getNthOH(j + 1)
        val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
        when(selPortReady && selectValid) {
          selIdxOH(i)(OHToUInt(selPortIdxOH)).valid := selectValid
          selIdxOH(i)(OHToUInt(selPortIdxOH)).bits := selectIdxOH.asUInt
        }
      }
    }
  }

  val portSelIdxOH: Map[Seq[Int], Vec[ValidIO[UInt]]] = finalFuDeqMap.zip(selIdxOH).map{ case ((fuTypeSeq, deqPortIdSeq), selIdxOHSeq) => (deqPortIdSeq, selIdxOHSeq)}.toMap
  println(s"[Dispatch2IQ] portSelIdxOH: $portSelIdxOH")
  val finalportSelIdxOH: mutable.Map[Int, Seq[ValidIO[UInt]]] = expendPortSel(portSelIdxOH)
  println(s"[Dispatch2IQ] finalportSelIdxOH: $finalportSelIdxOH")
  finalportSelIdxOH.foreach{ case (portId, selSeq) =>
    val finalSelIdxOH: UInt = PriorityMux(selSeq.map(_.valid).toSeq, selSeq.map(_.bits).toSeq)
    outs(portId).valid := selSeq.map(_.valid).reduce(_ | _)
    outs(portId).bits := Mux1H(finalSelIdxOH, uopsIn.map(_.bits))
    when(outs(portId).valid) {
      outReadyMatrix(portId).zipWithIndex.foreach { case (inReady, i) =>
        when(finalSelIdxOH(i)) {
          inReady := outs(portId).ready
        }
      }
    }
  }

  uopsIn <> io.in
  uopsIn.foreach(_.ready := false.B)
  uopsIn.zipWithIndex.foreach{ case (uopIn, idx) => uopIn.ready := outReadyMatrix.map(_(idx)).reduce(_ | _) }

  private val reqPsrcVec: IndexedSeq[UInt] = uopsIn.flatMap(in => in.bits.psrc.take(numRegSrc))

  private val intSrcStateVec = if (io.readIntState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val vfSrcStateVec  = if (io.readVfState.isDefined)  Some(Wire(Vec(numEnq * numRegSrc, SrcState()))) else None
  private val intSrcLoadDependency = OptionWrapper(io.readIntState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  private val vfSrcLoadDependency = OptionWrapper(io.readVfState.isDefined, Wire(Vec(numEnq * numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    require(io.readIntState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readIntState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readIntState.get.map(_.loadDependency).zip(intSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }

  if (io.readVfState.isDefined) {
    require(io.readVfState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readVfState.get.size: ${io.readVfState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readVfState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readVfState.get.map(_.resp).zip(vfSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readVfState.get.map(_.loadDependency).zip(vfSrcLoadDependency.get).foreach(x => x._2 := x._1)
  }

  uopsIn
    .flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq)) zip vfSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
    )
    .foreach {
      case ((state: UInt, srcType), (intState, vfState)) =>
        state := Mux1H(Seq(
          SrcType.isXp(srcType) -> intState,
          SrcType.isVfp(srcType) -> vfState,
          SrcType.isNotReg(srcType) -> true.B,
        ))
  }
  uopsIn
    .flatMap(x => x.bits.srcLoadDependency.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq)) zip vfSrcLoadDependency.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))).toSeq))
    )
    .foreach {
      case ((ldp, srcType), (intLdp, vfLdp)) =>
        when(SrcType.isXp(srcType)) {
          ldp := intLdp
        }.elsewhen(SrcType.isVfp(srcType)) {
          ldp := vfLdp
        }.otherwise {
          ldp := 0.U.asTypeOf(ldp)
        }
    }


  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("out_valid", PopCount(io.out.flatMap(_.map(_.valid))))
  XSPerfAccumulate("out_fire", PopCount(io.out.flatMap(_.map(_.fire))))
}

/**
  *
  * @param numIn
  * @param dispatchCfg Seq[Seq[FuType], dispatch limits]
  */
class Dispatch2IqSelect(numIn: Int, dispatchCfg: Seq[(Seq[BigInt], Int)])(implicit p: Parameters) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Vec(numIn, ValidIO(new DynInst)))
    val out = MixedVec(dispatchCfg.map(x => Vec(x._2, ValidIO(new DynInst))).toSeq)
    val mapIdxOH = Output(MixedVec(dispatchCfg.map(x => Vec(x._2, UInt(in.size.W))).toSeq)) // OH mapping of in ports to out ports
  })

  val issuePortFuType: Seq[Seq[BigInt]] = dispatchCfg.map(_._1)

  val numOutKinds = io.out.size
  val numInPorts = io.in.size
  val numPortsOfKind = io.out.map(_.size)

  val canAcceptMatrix = Wire(Vec(numOutKinds, Vec(numInPorts, Bool())))

  for (inIdx <- 0 until numInPorts) {
    for (kindIdx <- io.out.indices) {
      canAcceptMatrix(kindIdx)(inIdx) := io.in(inIdx).valid && canAccept(issuePortFuType(kindIdx), io.in(inIdx).bits.fuType)
    }
  }

  val selectedIdxVec = canAcceptMatrix.zipWithIndex.map { case (outCanAcceptVec, kindIdx) =>
    val select = SelectOne("naive", outCanAcceptVec, numPortsOfKind(kindIdx))
    for (portIdx <- 0 until numPortsOfKind(kindIdx)) {
      val (selectValid, selectIdxOH) = select.getNthOH(portIdx + 1)
      io.out(kindIdx)(portIdx).valid := selectValid
      io.out(kindIdx)(portIdx).bits := Mux1H(selectIdxOH, io.in.map(_.bits))
      io.mapIdxOH(kindIdx)(portIdx) := selectIdxOH.asUInt
    }
  }

  def canAccept(acceptVec: Seq[BigInt], fuType: UInt): Bool = {
    (acceptVec.reduce(_ | _).U & fuType).orR
  }

  def canAccept(acceptVec: Seq[Seq[BigInt]], fuType: UInt): Vec[Bool] = {
    VecInit(acceptVec.map(x => canAccept(x, fuType)).toSeq)
  }
}

/**
  * @author Yinan Xu, Xuan Hu
  */
class Dispatch2IqMemImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  import FuType._

  private val enqLsqIO = io.enqLsqIO.get

  private val numLoadDeq = LSQLdEnqWidth
  private val numStoreAMODeq = LSQStEnqWidth
  private val numVLoadDeq = LoadPipelineWidth
  private val numDeq = enqLsqIO.req.size
  private val numEnq = io.in.size
  private val iqAllReady = Cat(io.out.map(_.map(_.ready)).flatten.toSeq).andR
  private val lsqCanAccept = enqLsqIO.canAccept

  private val isLoadVec = VecInit(io.in.map(x => x.valid && FuType.isLoad(x.bits.fuType)))
  private val isStoreVec = VecInit(io.in.map(x => x.valid && FuType.isStore(x.bits.fuType)))
  private val isAMOVec = io.in.map(x => x.valid && FuType.isAMO(x.bits.fuType))
  private val isStoreAMOVec = io.in.map(x => x.valid && (FuType.isStore(x.bits.fuType) || FuType.isAMO(x.bits.fuType)))
  private val isVLoadVec = VecInit(io.in.map(x => x.valid && FuType.isVLoad(x.bits.fuType)))
  private val isVStoreVec = VecInit(io.in.map(x => x.valid && FuType.isVStore(x.bits.fuType)))

  private val loadCntVec = VecInit(isLoadVec.indices.map(x => PopCount(isLoadVec.slice(0, x + 1))))
  private val storeAMOCntVec = VecInit(isStoreAMOVec.indices.map(x => PopCount(isStoreAMOVec.slice(0, x + 1))))
  private val vloadCntVec = VecInit(isVLoadVec.indices.map(x => PopCount(isVLoadVec.slice(0, x + 1))))

  private val s0_enqLsq_resp = Wire(enqLsqIO.resp.cloneType)
  private val s0_blockedVec = Wire(Vec(io.in.size, Bool()))

  val loadBlockVec = VecInit(loadCntVec.map(_ > numLoadDeq.U))
  val storeAMOBlockVec = VecInit(storeAMOCntVec.map(_ > numStoreAMODeq.U))
  val vloadBlockVec = VecInit(vloadCntVec.map(_ > numVLoadDeq.U))
  val lsStructBlockVec = VecInit((loadBlockVec.zip(storeAMOBlockVec)).zip(vloadBlockVec).map(x => x._1._1 || x._1._2 || x._2))
  if(backendParams.debugEn) {
    dontTouch(loadBlockVec)
    dontTouch(storeAMOBlockVec)
    dontTouch(lsStructBlockVec)
    dontTouch(vloadBlockVec)
    dontTouch(isLoadVec)
    dontTouch(isVLoadVec)
    dontTouch(loadCntVec)
  }

  for (i <- 0 until numEnq) {
    if (i >= numDeq) {
      s0_blockedVec(i) := true.B
    } else {
      s0_blockedVec(i) := lsStructBlockVec(i)
    }
  }

  // enqLsq io
  require(enqLsqIO.req.size == enqLsqIO.resp.size)
  for (i <- enqLsqIO.req.indices) {
    when(!io.in(i).fire/* || io.in(i).bits.uopIdx =/= 0.U*/) {
      enqLsqIO.needAlloc(i) := 0.U
    }.elsewhen(isStoreVec(i) || isVStoreVec(i)) {
      enqLsqIO.needAlloc(i) := 2.U // store | vstore
    }.otherwise {
      enqLsqIO.needAlloc(i) := 1.U // load | vload
    }
    enqLsqIO.req(i).valid := io.in(i).fire && !isAMOVec(i)
    enqLsqIO.req(i).bits := io.in(i).bits
    s0_enqLsq_resp(i) := enqLsqIO.resp(i)
  }

  val portFuSets = params.issueBlockParams.map(_.exuBlockParams.filterNot(_.hasStdFu).flatMap(_.fuConfigs).map(_.fuType).toSet)
  println(s"[Dispatch2IqMemImp] portFuSets: $portFuSets")
  val fuDeqMap = getFuDeqMap(portFuSets)
  println(s"[Dispatch2IqMemImp] fuDeqMap: $fuDeqMap")
  val mergedFuDeqMap = mergeFuDeqMap(fuDeqMap)
  println(s"[Dispatch2IqMemImp] mergedFuDeqMap: $mergedFuDeqMap")
  val expendedFuDeqMap = expendFuDeqMap(mergedFuDeqMap, params.issueBlockParams.map(_.numEnq))
  println(s"[Dispatch2IqMemImp] expendedFuDeqMap: $expendedFuDeqMap")

  // sort by count of port. Port less, priority higher.
  val finalFuDeqMap = expendedFuDeqMap.toSeq.sortBy(_._2.length)
  println(s"[Dispatch2IqMemImp] finalFuDeqMap: $finalFuDeqMap")

  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
  val numInPorts = io.in.size
  val outs = io.out.flatten
  val selIdxOH = Wire(MixedVec(finalFuDeqMap.map(x => Vec(x._2.size, ValidIO(UInt(uopsIn.size.W))))))
  selIdxOH.foreach(_.foreach(_ := 0.U.asTypeOf(ValidIO(UInt(uopsIn.size.W)))))

  dontTouch(selIdxOH)

  // decide the priority of hyu for load instructions
  val hyuPriorityCtr = RegInit(0x100.U(9.W))
  val addCtr = io.in.map(in => Mux(in.fire && FuType.isLoad(in.bits.fuType), Constantin.createRecord("hyuPriorityAddCtr", 2)(1, 0), 0.U)).reduce(_ +& _) // loadCnt * 2
  val subCtr = io.in.map(in => Mux(in.fire && FuType.isStore(in.bits.fuType), Constantin.createRecord("hyuPrioritySubCtr", 5)(2, 0), 0.U)).reduce(_ +& _) // storeCnt * 5
  val nextCtr = hyuPriorityCtr + addCtr - subCtr
  hyuPriorityCtr := Mux(addCtr > subCtr && hyuPriorityCtr > nextCtr, 0x1FF.U(9.W),
                    Mux(addCtr < subCtr && hyuPriorityCtr < nextCtr, 0x000.U(9.W),
                                                                     nextCtr))
  val useHyuForLoadMore = hyuPriorityCtr.asBools.last

  // index of issue block
  private val issueBlockParamsWithoutStd = params.issueBlockParams.filter(_.StdCnt == 0)
  val loadIqIdx = issueBlockParamsWithoutStd.zipWithIndex.filter(_._1.LduCnt != 0).unzip._2
  val storeIqIdx = issueBlockParamsWithoutStd.zipWithIndex.filter(_._1.StaCnt != 0).unzip._2
  val hybridIqIdx = issueBlockParamsWithoutStd.zipWithIndex.filter(_._1.HyuCnt != 0).unzip._2
  val stHyIqIdx = issueBlockParamsWithoutStd.zipWithIndex.filter(x => x._1.StaCnt != 0 || x._1.HyuCnt != 0).unzip._2

  val Seq(loadIqsEnqPorts, storeIqsEnqPorts, hybridIqsEnqPorts) = Seq(loadIqIdx, storeIqIdx, hybridIqIdx).map(_.map(idx =>
    (0 until issueBlockParamsWithoutStd(idx).numEnq).map(_ + issueBlockParamsWithoutStd.take(idx).map(_.numEnq).fold(0)(_ + _))
  ).transpose)
  val stHyIqsEnqPorts = (storeIqsEnqPorts.transpose ++ hybridIqsEnqPorts.transpose.map(_.reverse).reverse).transpose

  // deq port priority sequence
  val loadMoreHyuDeq = (loadIqsEnqPorts.take(loadIqsEnqPorts.length / 2) ++ hybridIqsEnqPorts.take(hybridIqsEnqPorts.length / 2) ++
                        loadIqsEnqPorts.drop(loadIqsEnqPorts.length / 2) ++ hybridIqsEnqPorts.drop(hybridIqsEnqPorts.length / 2)).flatten
  val loadLessHyuDeq = (loadIqsEnqPorts ++ hybridIqsEnqPorts).flatten
  val storeDeq = (storeIqsEnqPorts.take(storeIqsEnqPorts.length / 2) ++ hybridIqsEnqPorts.map(_.reverse).reverse.take(hybridIqsEnqPorts.length / 2) ++
                  storeIqsEnqPorts.drop(storeIqsEnqPorts.length / 2) ++ hybridIqsEnqPorts.map(_.reverse).reverse.drop(hybridIqsEnqPorts.length / 2)).flatten

  val expendedStuDeq = expendedFuDeqMap.get(Seq(stu)).getOrElse(Seq()) ++ expendedFuDeqMap.get(Seq(stu, mou)).getOrElse(Seq())

  require(loadMoreHyuDeq.sorted == expendedFuDeqMap(Seq(ldu)).sorted)
  require(loadLessHyuDeq.sorted == expendedFuDeqMap(Seq(ldu)).sorted)
  require(storeDeq.sorted == expendedStuDeq.sorted)

  // Seq(storeCnt)(priority)
  val loadMoreHyuDeqSeq: Seq[Seq[Int]] = Seq.fill(numEnq + 1)(loadMoreHyuDeq)
  val loadLessHyuDeqSeq: Seq[Seq[Int]] = Seq.fill(numEnq + 1)(loadLessHyuDeq)
  val storeDeqSeq: Seq[Seq[Int]] = Seq.fill(numEnq + 1)(storeDeq)

  require(expendedFuDeqMap(Seq(ldu)).max - expendedFuDeqMap(Seq(ldu)).min == expendedFuDeqMap(Seq(ldu)).length - 1)
  require(expendedStuDeq.max - expendedStuDeq.min == expendedStuDeq.length - 1)

  private abstract class LoadOrStore(val isStore: Boolean) { def isLoad = !isStore }
  private case class Load() extends LoadOrStore(false)
  private case class Store() extends LoadOrStore(true)

  private val allLSPatern = Seq.tabulate(numEnq + 1)(i => (Seq.fill(i)(Load()) ++ Seq.fill(numEnq - i)(Store())).toSeq.permutations).flatten.zipWithIndex.toSeq

  val inIsStoreAmoVec = Cat(isStoreAMOVec)
  val inIsNotLoadVec = ~Cat(isLoadVec)
  object LoadValidTable {
    val default = BitPat("b" + "0" * numEnq)
    val table = allLSPatern.map { case (pattern, index) =>
      pattern.zipWithIndex.filter(_._1.isLoad).map(x => BitPat((1 << x._2).U(numEnq.W))) ++
      pattern.filterNot(_.isLoad).map(_ => BitPat("b" + "0" * numEnq)) map
      (BitPat(pattern.map(s => if (s.isStore) "1" else "0").mkString("b", "", "")) -> _)
    }.transpose
    val truthTable = table.map(TruthTable(_, default))
  }
  object StoreValidTable {
    val default = BitPat("b" + "0" * numEnq)
    val table = allLSPatern.map { case (pattern, index) =>
      pattern.zipWithIndex.filter(_._1.isStore).map(x => BitPat((1 << x._2).U(numEnq.W))) ++
      pattern.filterNot(_.isStore).map(_ => BitPat("b" + "0" * numEnq)) map
      (BitPat(pattern.map(s => if (s.isStore) "1" else "0").mkString("b", "", "")) -> _)
    }.transpose
    val truthTable = table.map(TruthTable(_, default))
  }
  object LoadMoreHyuReadyTable {
    val default = BitPat("b" + "0" * loadMoreHyuDeqSeq.head.length)
    val table = allLSPatern.map { case (pattern, index) =>
      loadMoreHyuDeqSeq(pattern.count(_.isStore)).map(x => BitPat((1 << x - loadMoreHyuDeqSeq.flatten.min).U(loadMoreHyuDeqSeq.head.length.W))) map
      (BitPat(pattern.map(s => if (s.isStore) "1" else "0").mkString("b", "", "")) -> _)
    }.transpose
    val truthTable = table.map(TruthTable(_, default))
  }
  object LoadLessHyuReadyTable {
    val default = BitPat("b" + "0" * loadLessHyuDeqSeq.head.length)
    val table = allLSPatern.map { case (pattern, index) =>
      loadLessHyuDeqSeq(pattern.count(_.isStore)).map(x => BitPat((1 << x - loadLessHyuDeqSeq.flatten.min).U(loadLessHyuDeqSeq.head.length.W))) map
      (BitPat(pattern.map(s => if (s.isStore) "1" else "0").mkString("b", "", "")) -> _)
    }.transpose
    val truthTable = table.map(TruthTable(_, default))
  }
  object StoreReadyTable {
    val default = BitPat("b" + "0" * storeDeqSeq.head.length)
    val table = allLSPatern.map { case (pattern, index) =>
      storeDeqSeq(pattern.count(_.isStore)).map(x => BitPat((1 << x - storeDeqSeq.flatten.min).U(storeDeqSeq.head.length.W))) map
      (BitPat(pattern.map(s => if (s.isStore) "1" else "0").mkString("b", "", "")) -> _)
    }.transpose
    val truthTable = table.map(TruthTable(_, default))
  }

  // port flip for load/store load balance
  val loadSwapMap = (loadIqsEnqPorts.map(ports => ports.zipWithIndex.map { case (port, idx) => (port -> ports(ports.length - 1 - idx)) }) ++
                     hybridIqsEnqPorts.map(_.map(port => (port -> port)))).flatten.sortBy(_._1).unzip._2
  val storeSwapMap = stHyIqsEnqPorts.map(ports => ports.zipWithIndex.map { case (port, idx) => (port -> ports(ports.length - 1 - idx)) }).flatten.sortBy(_._1).unzip._2

  val loadFlipMap = loadSwapMap.map(x => (1 << x - loadSwapMap.min).U(loadSwapMap.length.W))
  val storeFlipMap = storeSwapMap.map(x => (1 << x - storeSwapMap.min).U(storeSwapMap.length.W))

  val loadIqValidCnt = loadIqIdx.map(io.iqValidCnt)
  val sthyIqValidCnt = stHyIqIdx.map(io.iqValidCnt)

  val loadDeqNeedFlip = RegNext(loadIqValidCnt.last < loadIqValidCnt.head) && Constantin.createRecord("enableLoadBalance", true)
  val storeDeqNeedFlip = RegNext(sthyIqValidCnt.last < sthyIqValidCnt.head) && Constantin.createRecord("enableStoreBalance", true)
  val loadValidDecoder = LoadValidTable.truthTable.map(decoder(EspressoMinimizer, inIsNotLoadVec, _))
  val storeValidDecoder = StoreValidTable.truthTable.map(decoder(EspressoMinimizer, inIsStoreAmoVec, _))

  val loadMoreHyuReadyDecoderOriginal = LoadMoreHyuReadyTable.truthTable.map(decoder(EspressoMinimizer, inIsNotLoadVec, _))
  val loadMoreHyuReadyDecoderFlipped = loadMoreHyuReadyDecoderOriginal.map(Mux1H(_, loadFlipMap))
  val loadLessHyuReadyDecoderOriginal = LoadLessHyuReadyTable.truthTable.map(decoder(EspressoMinimizer, inIsNotLoadVec, _))
  val loadLessHyuReadyDecoderFlipped = loadLessHyuReadyDecoderOriginal.map(Mux1H(_, loadFlipMap))
  val loadReadyDecoderOriginal = loadMoreHyuReadyDecoderOriginal zip loadLessHyuReadyDecoderOriginal map (x => Mux(useHyuForLoadMore, x._1, x._2))
  val loadReadyDecoderFlipped = loadMoreHyuReadyDecoderFlipped zip loadLessHyuReadyDecoderFlipped map (x => Mux(useHyuForLoadMore, x._1, x._2))
  val storeReadyDecoderOriginal = StoreReadyTable.truthTable.map(decoder(EspressoMinimizer, inIsStoreAmoVec, _))
  val storeReadyDecoderFlipped = storeReadyDecoderOriginal.map(Mux1H(_, storeFlipMap))

  val loadReadyDecoder = loadReadyDecoderFlipped zip loadReadyDecoderOriginal map (x => Mux(loadDeqNeedFlip, x._1, x._2))
  val storeReadyDecoder = storeReadyDecoderFlipped zip storeReadyDecoderOriginal map (x => Mux(storeDeqNeedFlip, x._1, x._2))

  finalFuDeqMap.zipWithIndex.foreach {
    case ((Seq(FuType.ldu), deqPortIdSeq), i) =>
      val maxSelNum = wrapper.numIn
      val selNum = deqPortIdSeq.length
      val portReadyVec = loadReadyDecoder.map(Mux1H(_, deqPortIdSeq.map(outs(_).ready).toSeq))
      val canAcc = loadValidDecoder.map(Mux1H(_, uopsIn.map(_.valid)))
      val selPort = SelectOne("naive", portReadyVec.toSeq, selNum)
      val select = SelectOne("naive", canAcc, selNum)
      for ((portId, j) <- deqPortIdSeq.zipWithIndex) {
        val (selPortReady, selPortIdxOH) = selPort.getNthOH(j + 1)
        val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
        when(selPortReady && selectValid) {
          selIdxOH(i)(OHToUInt(Mux1H(selPortIdxOH, loadReadyDecoder))).valid := selectValid
          selIdxOH(i)(OHToUInt(Mux1H(selPortIdxOH, loadReadyDecoder))).bits := Mux1H(selectIdxOH, loadValidDecoder)
        }
      }
    case ((Seq(FuType.stu, FuType.mou), deqPortIdSeq), i) =>
      val maxSelNum = wrapper.numIn
      val selNum = deqPortIdSeq.length
      val portReadyVec = storeReadyDecoder.map(Mux1H(_, deqPortIdSeq.map(outs(_).ready).toSeq))
      val canAcc = storeValidDecoder.map(Mux1H(_, uopsIn.map(_.valid)))
      val selPort = SelectOne("naive", portReadyVec.toSeq, selNum)
      val select = SelectOne("naive", canAcc, selNum)
      for ((portId, j) <- deqPortIdSeq.zipWithIndex) {
        val (selPortReady, selPortIdxOH) = selPort.getNthOH(j + 1)
        val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
        when(selPortReady && selectValid) {
          selIdxOH(i)(OHToUInt(Mux1H(selPortIdxOH, storeReadyDecoder))).valid := selectValid
          selIdxOH(i)(OHToUInt(Mux1H(selPortIdxOH, storeReadyDecoder))).bits := Mux1H(selectIdxOH, storeValidDecoder)
        }
      }
    case ((fuTypeSeq, deqPortIdSeq), i) =>
      val maxSelNum = wrapper.numIn
      val selNum = deqPortIdSeq.length
      val portReadyVec = deqPortIdSeq.map(x => outs(x).ready)
      val canAcc = uopsIn.map(in => canAccept(fuTypeSeq.map(x => x.ohid), in.bits.fuType) && in.valid)
      val selPort = SelectOne("circ", portReadyVec.toSeq, selNum)
      val select = SelectOne("naive", canAcc, selNum)
      for ((portId, j) <- deqPortIdSeq.zipWithIndex) {
        val (selPortReady, selPortIdxOH) = selPort.getNthOH(j + 1)
        val (selectValid, selectIdxOH) = select.getNthOH(j + 1)
        when(selPortReady && selectValid) {
          selIdxOH(i)(OHToUInt(selPortIdxOH)).valid := selectValid
          selIdxOH(i)(OHToUInt(selPortIdxOH)).bits := selectIdxOH.asUInt
        }
      }
  }

  val portSelIdxOH: Map[Seq[Int], Vec[ValidIO[UInt]]] = finalFuDeqMap.zip(selIdxOH).map { case ((fuTypeSeq, deqPortIdSeq), selIdxOHSeq) => (deqPortIdSeq, selIdxOHSeq) }.toMap
  println(s"[Dispatch2IQ] portSelIdxOH: $portSelIdxOH")
  val deqSelIdxOHSeq: mutable.Map[Int, Seq[ValidIO[UInt]]] = expendPortSel(portSelIdxOH)
  println(s"[Dispatch2IQ] finalportSelIdxOH: $deqSelIdxOHSeq")

  // Todo: split this matrix into more deq parts
  // deqSelIdxVec(deqIdx)(enqIdx): enqIdx uop can be accepted by deqIdx
  val deqSelIdxVec: Vec[UInt] = VecInit(deqSelIdxOHSeq.map {
    case (deqIdx, seq) => PriorityEncoderOH(Mux1H(seq.map(x => (x.valid, x.bits))))
  }.toSeq)
  deqSelIdxOHSeq.foreach { case (deqIdx, seq) =>
    val block_by_conflict = (PopCount(Mux1H(seq.map(x => (x.valid, x.bits)))) > 1.U).asUInt
    XSPerfAccumulate(s"block_by_conflict_${deqIdx}", block_by_conflict)
  }

  // enqSelIdxVec(enqIdx)(deqIdx): enqIdx uop can be accepted by deqIdx
  // Maybe one port has been dispatched more than 1 uop.
  // Select the oldest one
  val enqSelIdxOHVec: Vec[Vec[Bool]] = VecInit(deqSelIdxVec.map(_.asBools).transpose.map(VecInit(_)))
  // Check if enq uops have deq port can accept
  val enqAcceptedVec = VecInit(enqSelIdxOHVec.map(_.asUInt.orR))
  // Check if uop will be blocked by the uops before it
  val continousNotBlockVec = VecInit((0 until numEnq).map(enqIdx => enqAcceptedVec.slice(0, enqIdx).fold(true.B)(_ && _)))
  // mask off not continous uops
  val enqMapDeqMatrix: Vec[Vec[Bool]] = VecInit(enqSelIdxOHVec.zipWithIndex.map {
    case (enqSelOH, idx) => VecInit(enqSelOH.map(_ && continousNotBlockVec(idx) && !lsStructBlockVec(idx)))
  })

  val deqMapEnqMatrix: Vec[Vec[Bool]] = VecInit(enqMapDeqMatrix.transpose.map(VecInit(_)))

  dontTouch(deqSelIdxVec)
  dontTouch(enqSelIdxOHVec)
  dontTouch(enqAcceptedVec)
  dontTouch(continousNotBlockVec)
  dontTouch(enqMapDeqMatrix)
  dontTouch(deqMapEnqMatrix)

  deqMapEnqMatrix.zipWithIndex.foreach { case (deqOH, deqIdx) =>
    outs(deqIdx).valid := deqOH.asUInt.orR && lsqCanAccept
    outs(deqIdx).bits := Mux1H(deqOH, uopsIn.map(_.bits))
  }

  uopsIn <> io.in
  uopsIn.foreach(_.ready := false.B)
  uopsIn.zipWithIndex.foreach { case (uopIn, idx) =>
    uopIn.ready := enqMapDeqMatrix(idx).asUInt.orR && lsqCanAccept
    uopIn.bits.lqIdx := s0_enqLsq_resp(idx).lqIdx
    uopIn.bits.sqIdx := s0_enqLsq_resp(idx).sqIdx
  }

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  val reqPsrc = io.in.flatMap(in => in.bits.psrc.take(numRegSrc))
  require(io.readIntState.get.size >= reqPsrc.size, s"io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrc}")
  require(io.readVfState.get.size >= reqPsrc.size, s"io.readFpState.get.size: ${io.readVfState.get.size}, psrc size: ${reqPsrc}")
  io.readIntState.get.map(_.req).zip(reqPsrc).foreach(x => x._1 := x._2)
  io.readVfState.get.map(_.req).zip(reqPsrc).foreach(x => x._1 := x._2)

  val intSrcStateVec = Wire(Vec(numEnq, Vec(numRegSrc, SrcState())))
  val vfSrcStateVec = Wire(Vec(numEnq, Vec(numRegSrc, SrcState())))
  val intSrcLoadDependency = Wire(Vec(numEnq, Vec(numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))
  val vfSrcLoadDependency = Wire(Vec(numEnq, Vec(numRegSrc, Vec(LoadPipelineWidth, UInt(LoadDependencyWidth.W)))))

  // srcState is read from outside and connected directly
  io.readIntState.get.map(_.resp).zip(intSrcStateVec.flatten).foreach(x => x._2 := x._1)
  io.readVfState.get.map(_.resp).zip(vfSrcStateVec.flatten).foreach(x => x._2 := x._1)
  io.readIntState.get.map(_.loadDependency).zip(intSrcLoadDependency.flatten).foreach(x => x._2 := x._1)
  io.readVfState.get.map(_.loadDependency).zip(vfSrcLoadDependency.flatten).foreach(x => x._2 := x._1)

  uopsIn.flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intSrcStateVec.flatten zip vfSrcStateVec.flatten).foreach {
    case ((state: UInt, srcType), (intState, vfState)) =>
      state := Mux1H(Seq(
        SrcType.isXp(srcType) -> intState,
        SrcType.isVfp(srcType) -> vfState,
        SrcType.isNotReg(srcType) -> true.B,
      ))
  }

  uopsIn
    .flatMap(x => x.bits.srcLoadDependency.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcLoadDependency.flatten zip vfSrcLoadDependency.flatten
    )
    .foreach {
      case ((ldp, srcType), (intLdp, vfLdp)) =>
        when(SrcType.isXp(srcType)) {
          ldp := intLdp
        }.elsewhen(SrcType.isVfp(srcType)) {
          ldp := vfLdp
        }.otherwise {
          ldp := 0.U.asTypeOf(ldp)
        }
    }
}
