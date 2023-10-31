package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.SelectOne
import utils._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.rename.BusyTableReadIO
import xiangshan.mem.LsqEnqIO
import xiangshan.backend.Bundles.{DynInst, ExuOH}
import xiangshan.backend.datapath.DataSource

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
    case IntScheduler() => new Dispatch2IqArithImp(this)(p, schdBlockParams)
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
  private val intDataSourceVec = if (io.readIntState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, DataSource()))) else None
  private val vfDataSourceVec = if (io.readVfState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, DataSource()))) else None
  private val intL1ExuOHVec = if (io.readIntState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, ExuOH()))) else None
  private val vfL1ExuOHVec = if (io.readVfState.isDefined) Some(Wire(Vec(numEnq * numRegSrc, ExuOH()))) else None

  // We always read physical register states when in gives the instructions.
  // This usually brings better timing.
  if (io.readIntState.isDefined) {
    require(io.readIntState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readIntState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readIntState.get.map(_.dataSource).zip(intDataSourceVec.get).foreach(x => x._2.value := x._1.value)
    io.readIntState.get.map(_.l1ExuOH).zip(intL1ExuOHVec.get).foreach(x => x._2 := x._1)
  }

  if (io.readVfState.isDefined) {
    require(io.readVfState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readVfState.get.size: ${io.readVfState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readVfState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readVfState.get.map(_.resp).zip(vfSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readVfState.get.map(_.dataSource).zip(vfDataSourceVec.get).foreach(x => x._2.value := x._1.value)
    io.readVfState.get.map(_.l1ExuOH).zip(vfL1ExuOHVec.get).foreach(x => x._2 := x._1)
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
    .flatMap(x => x.bits.dataSource.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intDataSourceVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(DataSource())).toSeq)) zip vfDataSourceVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(DataSource())).toSeq))
    )
    .foreach {
      case ((dataSource, srcType), (intSource, vfSource)) =>
        dataSource.value := Mux1H(Seq(
          SrcType.isXp(srcType) -> intSource.value,
          SrcType.isVfp(srcType) -> vfSource.value,
          SrcType.isNotReg(srcType) -> 0.U,
        ))
    }
  uopsIn
    .flatMap(x => x.bits.l1ExuOH.take(numRegSrc) zip x.bits.srcType.take(numRegSrc))
    .zip(
      intL1ExuOHVec.getOrElse(VecInit.fill(numEnq * numRegSrc)(0.U.asTypeOf(ExuOH()))) zip vfL1ExuOHVec.getOrElse(VecInit.fill(numEnq * numRegSrc)(0.U.asTypeOf(ExuOH())))
    )
    .foreach {
      case ((l1ExuOH: UInt, srcType), (intL1ExuOH, vfL1ExuOH)) =>
        l1ExuOH := Mux1H(Seq(
          SrcType.isXp(srcType) -> intL1ExuOH,
          SrcType.isVfp(srcType) -> vfL1ExuOH,
          SrcType.isNotReg(srcType) -> 0.U,
        ))
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

  private val numLoadDeq = LoadPipelineWidth
  private val numStoreAMODeq = StorePipelineWidth
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
  val lsIQEnqBlockVec = Wire(Vec(io.in.size, Bool()))
  dontTouch(loadBlockVec)
  dontTouch(storeAMOBlockVec)
  dontTouch(lsStructBlockVec)
  dontTouch(vloadBlockVec)
  dontTouch(isLoadVec)
  dontTouch(isVLoadVec)
  dontTouch(loadCntVec)
  dontTouch(lsIQEnqBlockVec)

  for (i <- 0 until numEnq) {
    if (i >= numDeq) {
      s0_blockedVec(i) := true.B
    } else {
      s0_blockedVec(i) := lsStructBlockVec(i) || lsIQEnqBlockVec(i)
    }
  }

  // enqLsq io
  require(enqLsqIO.req.size == enqLsqIO.resp.size)
  for (i <- enqLsqIO.req.indices) {
    when(!io.in(i).fire) {
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
  val outReadyMatrix = Wire(Vec(outs.size, Vec(numInPorts, Bool())))
  outReadyMatrix.foreach(_.foreach(_ := false.B))
  val selIdxOH = Wire(MixedVec(finalFuDeqMap.map(x => Vec(x._2.size, ValidIO(UInt(uopsIn.size.W))))))
  selIdxOH.foreach(_.foreach(_ := 0.U.asTypeOf(ValidIO(UInt(uopsIn.size.W)))))

  dontTouch(outReadyMatrix)
  dontTouch(selIdxOH)

  finalFuDeqMap.zipWithIndex.foreach { case ((fuTypeSeq, deqPortIdSeq), i) =>
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
  val finalportSelIdxOH: mutable.Map[Int, Seq[ValidIO[UInt]]] = expendPortSel(portSelIdxOH)
  println(s"[Dispatch2IQ] finalportSelIdxOH: $finalportSelIdxOH")
  finalportSelIdxOH.foreach { case (portId, selSeq) =>
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

  private val uopCanEnqIQVec: Vec[Bool] = VecInit((0 until numEnq).map(i => outReadyMatrix.map(_(i)).reduce(_ | _)))
  lsIQEnqBlockVec.zipWithIndex.foreach { case (iqEnqBlock, i) =>
    iqEnqBlock := !uopCanEnqIQVec.slice(0, i).fold(true.B)(_ && _)
  }
  dontTouch(uopCanEnqIQVec)

  uopsIn <> io.in
  uopsIn.foreach(_.ready := false.B)
  uopsIn.zipWithIndex.foreach { case (uopIn, idx) =>
    uopIn.ready := uopCanEnqIQVec(idx) && !s0_blockedVec(idx) && iqAllReady && lsqCanAccept
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
  val intDataSourceVec = Wire(Vec(numEnq, Vec(numRegSrc, DataSource())))
  val vfDataSourceVec = Wire(Vec(numEnq, Vec(numRegSrc, DataSource())))
  val intL1ExuOHVec = Wire(Vec(numEnq, Vec(numRegSrc, ExuOH())))
  val vfL1ExuOHVec = Wire(Vec(numEnq, Vec(numRegSrc, ExuOH())))

  // srcState is read from outside and connected directly
  io.readIntState.get.map(_.resp).zip(intSrcStateVec.flatten).foreach(x => x._2 := x._1)
  io.readVfState.get.map(_.resp).zip(vfSrcStateVec.flatten).foreach(x => x._2 := x._1)
  io.readIntState.get.map(_.dataSource).zip(intDataSourceVec.flatten).foreach(x => x._2.value := x._1.value)
  io.readVfState.get.map(_.dataSource).zip(vfDataSourceVec.flatten).foreach(x => x._2.value := x._1.value)
  io.readIntState.get.map(_.l1ExuOH).zip(intL1ExuOHVec.flatten).foreach(x => x._2 := x._1)
  io.readVfState.get.map(_.l1ExuOH).zip(vfL1ExuOHVec.flatten).foreach(x => x._2 := x._1)

  uopsIn.flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intSrcStateVec.flatten zip vfSrcStateVec.flatten).foreach {
    case ((state: UInt, srcType), (intState, vfState)) =>
      state := Mux1H(Seq(
        SrcType.isXp(srcType) -> intState,
        SrcType.isVfp(srcType) -> vfState,
        SrcType.isNotReg(srcType) -> true.B,
      ))
  }
  uopsIn.flatMap(x => x.bits.dataSource.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intDataSourceVec.flatten zip vfDataSourceVec.flatten).foreach {
    case ((dataSource, srcType), (intSource, vfSource)) =>
      dataSource.value := Mux1H(Seq(
        SrcType.isXp(srcType) -> intSource.value,
        SrcType.isVfp(srcType) -> vfSource.value,
        SrcType.isNotReg(srcType) -> 0.U,
      ))
  }
  uopsIn.flatMap(x => x.bits.l1ExuOH.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intL1ExuOHVec.flatten zip vfL1ExuOHVec.flatten).foreach {
    case ((l1ExuOH, srcType), (intL1ExuOH, vfL1ExuOH)) =>
      l1ExuOH := Mux1H(Seq(
        SrcType.isXp(srcType) -> intL1ExuOH,
        SrcType.isVfp(srcType) -> vfL1ExuOH,
        SrcType.isNotReg(srcType) -> 0.U,
      ))
  }
}
