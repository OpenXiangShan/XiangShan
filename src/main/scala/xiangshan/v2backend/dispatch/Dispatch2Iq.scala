package xiangshan.v2backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO
import xiangshan.v2backend.Bundles.DynInst
import xiangshan.v2backend._

import scala.collection._

class Dispatch2Iq(val schdBlockParams : SchdBlockParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val issueBlockParams = schdBlockParams.issueBlockParams

  val numIn = schdBlockParams.numUopIn
  require(issueBlockParams.size > 0 && issueBlockParams.forall(_.numEnq == issueBlockParams.head.numEnq), "issueBlock is null or the enq size of all issueBlock not be the same all\n")
  val numOut = issueBlockParams.head.numEnq
  val numIntSrc = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).max)
  val numIntStateRead = numIntSrc.max * numIn

  val numFpSrc = issueBlockParams.map(_.exuBlockParams.map(_.numFpSrc).max)
  val numFpStateRead = numFpSrc.max * numIn

  val isMem = schdBlockParams.schdType == MemScheduler()

  lazy val module = schdBlockParams.schdType match {
    case IntScheduler() => new Dispatch2IqArithImp(this)(p, schdBlockParams)
    case MemScheduler() => new Dispatch2IqMemImp(this)(p, schdBlockParams)
    case VfScheduler() => new Dispatch2IqArithImp(this)(p, schdBlockParams)
    case _ => null
  }
}

abstract class Dispatch2IqImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends LazyModuleImp(wrapper) with HasXSParameter {

  val numIntSrc = wrapper.numIntSrc.max
  val numIntStateRead = wrapper.numIntStateRead
  val numFpStateRead = wrapper.numFpStateRead
  val numIssueBlock = wrapper.issueBlockParams.size

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
    val readIntState = if (numIntStateRead > 0) Some(Vec(numIntStateRead, Flipped(new BusyTableReadIO))) else None
    val readFpState = if (numFpStateRead > 0) Some(Vec(numFpStateRead, Flipped(new BusyTableReadIO))) else None
    val out = Vec(wrapper.issueBlockParams.size, Vec(wrapper.numOut, DecoupledIO(new DynInst)))
    val enqLsq = if (wrapper.isMem) Some(Flipped(new LsqEnqIO)) else None
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

  def canAccept(acceptVec: Seq[Int], fuType: UInt): Bool = {
    (acceptVec.reduce(_ | _).U & fuType).orR
  }

  def canAccept(acceptVec: Seq[Seq[Int]], fuType: UInt): Vec[Bool] = {
    VecInit(acceptVec.map(x => canAccept(x, fuType)))
  }

  def filterCanAccept(fuConfigs: Seq[FuConfig], fuType: UInt, canAcceptAlu: Boolean): Bool = {
    if(canAcceptAlu) {
      Cat(fuConfigs.map(_.fuType.U === fuType)).orR
    }
    else{
      Mux(fuType === FuType.alu.U, false.B, Cat(fuConfigs.map(_.fuType.U === fuType)).orR)
    }
  }
}

class Dispatch2IqArithImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  val portFuSets = params.issueBlockParams.map(_.exuBlockParams.flatMap(_.fuConfigs).map(_.name).toSet)
  println(s"portFuSets: $portFuSets")
  val fuDeqMap = getFuDeqMap(portFuSets)
  println(s"fuDeqMap: $fuDeqMap")
  val mergedFuDeqMap = mergeFuDeqMap(fuDeqMap)
  println(s"mergedFuDeqMap: $mergedFuDeqMap")
  val expendedFuDeqMap = expendFuDeqMap(mergedFuDeqMap, params.issueBlockParams.map(_.numEnq))
  println(s"expendedFuDeqMap: $expendedFuDeqMap")

  // sort by count of port. Port less, priority higher.
  val finalFuDeqMap = expendedFuDeqMap.toSeq.sortBy(_._2.length)

  val issuePortFuType: Seq[Seq[Int]] = params.issueBlockParams.map(_.getFuCfgs.map(_.fuType))

  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))

  val numOutPorts = io.out.map(_.size).sum
  val numInPorts = io.in.size

  val canAcceptMatrix = Wire(Vec(numOutPorts, Vec(numInPorts, Bool())))

  for (inIdx <- 0 until numInPorts) {
    var outIdx = 0
    for (iqIdx <- io.out.indices) {
      for (portIdx <- io.out(iqIdx).indices) {
        canAcceptMatrix(outIdx)(inIdx) := canAccept(issuePortFuType(iqIdx), uopsIn(inIdx).bits.fuType)
        outIdx += 1
      }
    }
  }

  uopsIn <> io.in
  uopsIn(0).ready := io.out.map(_(0).ready).reduce(_ && _) // Todo: more port
  uopsIn.slice(1, io.in.size).foreach(_.ready := false.B)

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    val reqPsrc = uopsIn.flatMap(in => in.bits.psrc.take(numIntSrc))
    require(io.readIntState.get.size >= reqPsrc.size, s"io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrc}")
    io.readIntState.get.map(_.req).zip(reqPsrc).foreach(x => x._1 := x._2)
  }

  val intSrcStateVec = Wire(Vec(uopsIn.size, Vec(numIntSrc, SrcState())))

  // srcState is read from outside and connected directly
  if (io.readIntState.isDefined) {
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.flatten).foreach(x => x._2 := x._1)
  }

  var outIdx = 0
  for ((iqPort, iqIdx) <- io.out.zipWithIndex) {
    for ((port, portIdx) <- iqPort.zipWithIndex) {
      if (portIdx > 0) {
        port.valid := false.B
      }
      else {
        port.valid := uopsIn(0).valid && canAcceptMatrix(outIdx)(0) // Todo: more
        port.bits := Mux(port.valid, uopsIn(0).bits, 0.U.asTypeOf(port.bits))
        for (i <- port.bits.srcState.indices) {
          if (i < numIntSrc) {
            port.bits.srcState(i) := Mux(port.valid, intSrcStateVec(0)(i), 0.U.asTypeOf(port.bits.srcState(i)))
          } else {
            port.bits.srcState(i) := false.B
          }
        }
      }
      outIdx += 1
    }
  }

  XSPerfAccumulate("in_valid", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire", PopCount(io.in.map(_.fire)))
//  XSPerfAccumulate("out_valid", PopCount(io.out.map(_.valid)))
//  XSPerfAccumulate("out_fire", PopCount(io.out.map(_.fire)))
}

/**
  * @author Yinan Xu, Xuan Hu
  */
class Dispatch2IqMemImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  
}
