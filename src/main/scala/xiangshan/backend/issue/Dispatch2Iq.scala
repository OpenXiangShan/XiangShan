package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
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

  def getIQ2PortMap(numEnqs: Seq[Int]): Map[Int, Seq[Int]] = {
    val res: mutable.Map[Int, Seq[Int]] = mutable.Map()
    val portSum: Seq[Int] = numEnqs.indices.map(x => numEnqs.slice(0, x).sum)
    for ((numEnq, i) <- numEnqs.zipWithIndex) {
      val portIdxSeq = (portSum(i) until (portSum(i) + numEnq)).toList
      res += (i -> portIdxSeq)
    }
    res
  }

  case class DpFuIQPortMap[T](
    fuType: Seq[T],
    iqIdx: Seq[Int],
    deqPortId: Seq[Int]
  )

  case class DpFuIQPortPrioMap[T](
    me: DpFuIQPortMap[T],
    hiPrio: Seq[DpFuIQPortMap[T]]
  )

  def expendFuDeqMap[T](
    map: Map[Seq[T], Seq[Int]],
    iqPortMap: Map[Int, Seq[Int]]
  ): Seq[DpFuIQPortMap[T]] = {
    val res: mutable.Map[Seq[T], Seq[Int]] = mutable.Map()
    for ((fuType, iqIdxSeq) <- map) {
      val portIdxSeq = iqIdxSeq.flatMap(x => iqPortMap(x)).sorted
      res += (fuType -> (portIdxSeq))
    }
    res.toSeq
      .zip(map.map(_._2/*iqIdxSeq*/))
      .map{ case ((fuTypeSeq, deqPortIdSeq), iqIdxSeq) => DpFuIQPortMap(fuTypeSeq, iqIdxSeq, deqPortIdSeq) }
      .sortBy(_.deqPortId.length)
  }

  // This return type may be too complex
  // Input: Seq[(fuTypeSeq, deqPortIdSeq)]
  // Output: Seq[(fuTypeSeq, deqPortIdSeq, higherPrio)]
  // higherPrio is a Seq of T that like Input: Seq[(fuTypeSeq, deqPortIdSeq)]
  def addHigherPriority[T](
    map: Seq[DpFuIQPortMap[T]],
    numEnqs: Seq[Int]
  ): Seq[DpFuIQPortPrioMap[T]] = {

    val res: Seq[DpFuIQPortPrioMap[T]] = map.map{ case me =>
      var higherPrio: Seq[DpFuIQPortMap[T]] = Seq()
      for (other <- map) {
        val conflict = other.deqPortId.intersect(me.deqPortId).nonEmpty
        val others_hi_prio = other.deqPortId.length < me.deqPortId.length
        if (others_hi_prio && conflict) {
          require(other.deqPortId.map(me.deqPortId.contains(_)).reduce(_ | _), "other.deqPortId should be a subset of deqPortIdSeq")
          higherPrio :+= other
        }
      }
      DpFuIQPortPrioMap(me, higherPrio)
    }
    res
  }

  def reverseDpIqMap[T](map: Seq[DpFuIQPortPrioMap[T]], num: Int): Seq[Seq[Boolean]] = {
    (0 until num).map( iqPortIdx =>
      map.map(_.me.deqPortId).map(_.contains(iqPortIdx))
    )
  }

  def OHFromInt(idx: Int, width: Int): UInt = {
    require(width > 0)
    require(idx < width)
    VecInit((Seq.fill(idx)(false.B) ++ Seq(true.B) ++ Seq.fill(width-idx-1)(false.B)).toSeq).asUInt
  }

  def isOH(oh: UInt): Bool = {
    // naive implementation
    val ohSeq = oh.asBools.indices.map(OHFromInt(_, oh.getWidth))
    ParallelOR(ohSeq.map(_ === oh)).asBool
  }
  def noMoreThanOH(oh: UInt): Bool = {
    isOH(oh) | (oh === 0.U(oh.getWidth.W))
  }

  // circularRotation(Seq(0, 1, 2, 3), 2) = Seq(2, 3, 0, 1)
  def circularRotation(seq: Seq[Int], offset: Int): Seq[Int] = {
    val res = mutable.ArrayBuffer[Int]()
    for (i <- 0 until seq.length) {
      res += seq((i + offset) % seq.length)
    }
    res.toSeq
  }

  // e.g.
  // Input: iqPort: Seq(A, B)
  // Output: Seq(A.0, B.0, A.1, B.1)
  def getNaiveChoice(
    iqSeq: Seq[Int],
    iqPortMap: Map[Int, Seq[Int]],
    lowFirst: Boolean = true
  ): Seq[UInt] = {
    if (iqSeq.isEmpty) Seq()
    else {
      val entrySeqSeq = (0 until iqPortMap.values.map(_.size).max).map(i =>
        iqSeq.filter(iqPortMap(_).size > i).map(iqPortMap(_)(i).U)
      ).toSeq

      if (lowFirst) entrySeqSeq.flatten
      else entrySeqSeq.reverse.flatten
    }
  }

  // Round-Robin choices:
  // e.g. For Issue Queue A/B with 2 entry port
  // Seq(
  //   A.0-B.0-A.1-B.1,
  //   B.0-A.0-B.1-A.1
  // )
  // @lowFirst: High priority from the low index; low priority from the high index
  def getRoundRobinChoices[T](
    map: DpFuIQPortMap[T],
    iqPortMap: Map[Int, Seq[Int]],
    lowFirst: Boolean = true
  ): Vec[Vec[UInt]] = {
    val (fuTypeSeq, iqIdxSeq, deqPortIdSeq) = (map.fuType, map.iqIdx, map.deqPortId)
    VecInit(iqIdxSeq.indices.map{i =>
      VecInit(getNaiveChoice(circularRotation(iqIdxSeq, i), iqPortMap, lowFirst).toSeq)
    })
  }

  // Prefer choices:
  // e.g. For Issue Queue A/B with 2 entry port
  // Seq(
  //   A.0-A.1-B.0-C.0-B.1-C.1,
  //   B.0-B.1-C.0-A.0-C.1-A.1, // still keep the circular order
  //   C.0-C.1-A.0-B.0-A.1-B.1
  // )
  def getPreferChoices[T](
    map: DpFuIQPortMap[T],
    iqPortMap: Map[Int, Seq[Int]],
    lowFirst: Boolean = true
  ): Vec[Vec[UInt]] = {
    val (fuTypeSeq, iqIdxSeq, deqPortIdSeq) = (map.fuType, map.iqIdx, map.deqPortId)

    // if (iqIdxSeq.size < 2) Seq(Seq())
    // else {
      VecInit(iqIdxSeq.indices.map{i =>
        VecInit((iqPortMap(iqIdxSeq(i)).map(_.U) ++ getNaiveChoice(circularRotation(iqIdxSeq, i+1).init, iqPortMap, lowFirst)).toSeq)
      })
    // }
  }

  // combine Round-Robin and Prefer
  // def getRRPreferChoices[T](
  //   map: DpFuIQPortMap[T],
  //   iqPortMap: Map[Int, Seq[Int]],
  //   lowFirst: Boolean = true
  // ): Seq[Seq[Int]] = {

  // }

  // PreferNot choices:
  // e.g. For Issue Queue A/B with 2 entry port
  // Seq(
  //   B.0-C.0-B.1-C.1-A.0-A.1,
  //   C.0-A.0-C.1-A.1-B.0-B.1,
  //   A.0-B.0-A.1-B.1-C.0-C.1
  // )
  def getPreferNotChoices[T](
    map: DpFuIQPortMap[T],
    iqPortMap: Map[Int, Seq[Int]],
    lowFirst: Boolean = true
  ): Vec[Vec[UInt]] = {
    val (fuTypeSeq, iqIdxSeq, deqPortIdSeq) = (map.fuType, map.iqIdx, map.deqPortId)
    // if (iqIdxSeq.size < 2) Seq(Seq())
    // else {
      VecInit(iqIdxSeq.indices.map{i =>
        VecInit((getNaiveChoice(circularRotation(iqIdxSeq, i+1).init, iqPortMap, lowFirst) ++ iqPortMap(iqIdxSeq(i)).map(_.U)).toSeq)
      })
    // }
  }

  // NOTE: instruction type conflict may be simpiled modled by RR or Prefer
  // def getConflictChoices[T](){}
  case class DpChoicesVec(
    rr: Vec[Vec[UInt]],
    prefer: Vec[Vec[UInt]],
    preferNot: Vec[Vec[UInt]]
    // conflict: Seq[Seq[Int]]
  )

  def getChoices[T](
    map: DpFuIQPortPrioMap[T],
    iqPortMap: Map[Int, Seq[Int]]
  ): DpChoicesVec = {
    val rr = getRoundRobinChoices(map.me, iqPortMap, map.hiPrio.isEmpty)
    val prefer = getPreferChoices(map.me, iqPortMap, map.hiPrio.isEmpty)
    val preferNot = getPreferNotChoices(map.me, iqPortMap, map.hiPrio.isEmpty)
    DpChoicesVec(rr, prefer, preferNot)
  }

  def getChoices[T](
    map: Seq[DpFuIQPortPrioMap[T]],
    iqPortMap: Map[Int, Seq[Int]]
  ): Seq[DpChoicesVec] = {
    map.map(getChoices(_, iqPortMap))
  }

  def getOrder(seq: Seq[Bool]): Seq[Seq[Bool]] = {
    val maxNum = seq.size
    def onesNum(seq: Seq[Bool]): UInt = {
      if (seq.isEmpty) 0.U
      else PopCount(seq)
    }
    (0 until maxNum).map{ uop_idx =>
      (0 until maxNum).map{ order_idx =>
        // TODO: optimize PopCount: more faster or logic reuse
        // itself valid && there are order_idx uops before it
        if (uop_idx < order_idx) false.B
        else seq(uop_idx) && (onesNum(seq.take(uop_idx)) === order_idx.U)
      }.toSeq
    }.toSeq
  }

  def getInstrTypeOrder[T](
    map: Seq[DpFuIQPortMap[T]],
    accInfo: Seq[Seq[Bool]]
  ): Seq[Seq[Seq[Bool]]] = {
    // val dpWidth = accInfo(0).size
    (map zip accInfo).map{ case (m,a) =>
      // TODO: explicitly drop useless order info: the i-th uop could not be i+1-th instr
      // val typeAcceptWidth = m.deqPortId.size
      // val maxNum = math.min(typeAcceptWidth, dpWidth)
      getOrder(a)
    }
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

  def canAccept(fuTypeList: Seq[BigInt], fuType: UInt): Bool = {
    (fuTypeList.reduce(_ | _).U & fuType).orR
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

  class DpIqConectBundle extends XSBundle {
    val valid = Bool()
    val dpUopId = UInt(io.in.size.W)
    val deqPortId = UInt(io.out.flatten.size.W)
  }

  // Add SelIdxOH's re-map for round-robin
  // 1. normal-only roundRobin: change between A.0 B.0 A.1 B.1 with B.0 A.0 B.1 A.0
  // 2. less is higher        : when A is less than B, A.0 B.0 A.1 B.1
  // 3. much less is higher   : when A is much less than B, A.0 A.1 B.0 B.1
  // Choose one from three

  // Simplest implementation
  // Three selIdxOHs: selIdxOH, selIdxOH1, selIdxOH2
  // selIdxFinal = Mux(much less, selIdxOH2, Mux(less, selIdxOH1, Mux(roundRobin, selIdxOH0-1, selIdxOH0-2)))
  // Use selIdxFinal for deq

  // TODO: replace portReadyVec's ready with PopCounter(ValidVec)===0.U/=== 1.U
  // Is that better?

  // TODO:
  // 1. only one iq, does not need Select(mul/bku, misc), index first
  // 2. more than one iq & high priority, round-robin & less is higher
  // 3. more than one iq & less priority, round-robin & less is higher

  // TODO: mv canAccept to Before-Dispatch and store into DispatchQueue
  // replace area for latency

  // new implementation
  // 1. generate(hard-written) many 'choices' for each dp(instr) type to choose
  //    A choice is the list of issue port with particulr order
  // 2. deal with the parameter by instr type and the conflict connect between them, sort by the priority
  // 3. for each instr type, select one 'choice', assign each instr of this type by the choice
  //    Need pre-generated order info for each instr type
  // 4. issue queue part: use the selIdxOH to generate valid & bits(ParallelProrityMux, select oldest uop)
  // 5. dispatch queue part: how do the dq need that is it accepted?
  //    This will cause long latency.
  // 5.1. For each type, will not conflict inside the type(but will not issue queue ready)
  // 5.2. Higher priority will stall low priority, need check high priority that the iq port num is the same.

  // Priority: when conflict, the less issue queue is higher priority
  // Chained-Priority is forbidden, like A is higher than B, B is higher than C. C should concern A. Too complex.
  // Avoid many level Mux(when) for lower latency.
}

class Dispatch2IqArithImp(override val wrapper: Dispatch2Iq)(implicit p: Parameters, params: SchdBlockParams)
  extends Dispatch2IqImp(wrapper)
    with HasXSParameter {

  private val numEnq = io.in.size
  val uopsIn = Wire(Vec(wrapper.numIn, DecoupledIO(new DynInst)))
  val numInPorts = io.in.size
  val outs = io.out.flatten
  // val outReadyMatrix = Wire(Vec(outs.size, Vec(numInPorts, Bool())))
  // outReadyMatrix.foreach(_.foreach(_ := false.B))

  val portFuSets = params.issueBlockParams.map(_.exuBlockParams.flatMap(_.fuConfigs).map(_.fuType).toSet)
  println(s"[IssueQueueImp] portFuSets: $portFuSets")
  val iqFuMap = portFuSets.zipWithIndex.map(x => (x._2 + 1) -> x._1).toMap
  println(s"[IssueQueueImp] iqFuMap: $iqFuMap")
  val iqPortIdxMap = getIQ2PortMap(params.issueBlockParams.map(_.numEnq))
  println(s"[IssueQueueImp] iqPortIdxMap: $iqPortIdxMap]")
  val fuDeqMap = getFuDeqMap(portFuSets)
  println(s"[IssueQueueImp] fuDeqMap: $fuDeqMap")
  val mergedFuDeqMap = mergeFuDeqMap(fuDeqMap)
  println(s"[IssueQueueImp] mergedFuDeqMap: $mergedFuDeqMap")
  // val expendedFuDeqMap = expendFuDeqMap(mergedFuDeqMap, params.issueBlockParams.map(_.numEnq))
  val expendedFuDeqMap = expendFuDeqMap(mergedFuDeqMap, iqPortIdxMap)
  println(s"[IssueQueueImp] expendedFuDeqMap: $expendedFuDeqMap")

  // sort by count of port. Port less, priority higher.
  val finalFuDeqMap = addHigherPriority(expendedFuDeqMap, params.issueBlockParams.map(_.numEnq))
  println(s"[IssueQueueImp] finalFuDeqpMap: $finalFuDeqMap")
  // each iq port can accept which instr type bundle
  val iqDpTypeMap = reverseDpIqMap(finalFuDeqMap, io.out.flatten.size)
  println(s"[IssueQueueImp] iqDpMap: $iqDpTypeMap")


  val dpChoices: Seq[DpChoicesVec] = getChoices(finalFuDeqMap, iqPortIdxMap)
  println(s"[IssueQueueImp] dpChoices: $dpChoices")

  // Level1-Seq: lists divided by finalFuDeqMap
  // Level2-Seq: lists of Bool of uopsIn that canAccepted by fuTypeList
  val typeAcceptInfo = finalFuDeqMap.map(_.me.fuType).map(fuTypeList => uopsIn.map(uop =>
    canAccept(fuTypeList.map(_.ohid), uop.bits.fuType) && uop.valid).toSeq)
  // Level1-Seq: fuType lists divided by finalFuDeqMap(i)
  // Level2-Seq: lists of dispatch uop(j)
  // Level3-Seq: order info(k)
  //   The j-th dp-uop is k-th fuType(i) uop
  //   Start from 0. Valid is concerned
  val typeOrderInfo = getInstrTypeOrder(finalFuDeqMap.map(_.me), typeAcceptInfo)

  // generate
  val selectMatrix = Wire(MixedVec(finalFuDeqMap.map{x =>
    Vec(math.min(io.in.size, x.me.deqPortId.size), new Bundle{
      // if this connection is valid
      val valid = Bool()
      // issue queu port id(among the issue queues side)
      val deqPortId = UInt(outs.size.W)
      // uop id(among the dispatch side)
      val dpUopId = UInt(io.in.size.W)
    })
  }.toSeq))

  val rrCounterSeq = finalFuDeqMap.map(_.me).map{
    me =>
      if (me.iqIdx.size == 1) 1.U(1.W)
      else {
        val update = VecInit((me.deqPortId.map(a => outs(a).fire)).toSeq).asUInt.orR
        Counter(update, me.iqIdx.size)._1
      }
  }

  finalFuDeqMap.zip(dpChoices).zip(typeOrderInfo).zipWithIndex.foreach {
    case (((dpTypeInfo, choices), orders), dpTypeIdx) =>
    // generate final Choice for later use
    def chooseFromChoice(ch: DpChoicesVec): Seq[UInt] =
      if (ch.rr.size > 1) ch.rr(rrCounterSeq(dpTypeIdx))
      else ch.rr.head
    val finalChoice = chooseFromChoice(choices).map(x => UIntToOH(x)) // Just run, change it for better policy
    // TODO: remote UIntToOH

    selectMatrix(dpTypeIdx).zipWithIndex.foreach { case (con, con_idx) =>
      con.valid := orders.map(_(con_idx)).reduce(_ | _)
      // NOTE: deqPortId use OH, not UInt, in order to ParallelMux(outs.ready)
      con.deqPortId := finalChoice(con_idx)
      // NOTE: dpUopId use OH, not UInt, in order to ParallelMux(uopsIn)
      con.dpUopId := ParallelMux(orders.map(_(con_idx)).toSeq, VecInit(uopsIn.indices.map(OHFromInt(_, uopsIn.size))))
    }
  }

  // DontTouch for debug
  dontTouch(selectMatrix)
  // dontTouch(typeOrderInfo)

  //============================================

  // for each issue port: each dp-type-bundle can give a uop to this issue port
  // dp-issue-bundle has priority, the high priority override low priority
  val iqPortSelectVecMid = Wire(Vec(outs.size, Vec(finalFuDeqMap.size, Valid(UInt(io.in.size.W)))))
  // select result from the issue queue view, used to generate valid
  val iqPortSelectVec = Wire(Vec(outs.size, Vec(io.in.size, Bool())))
  iqPortSelectVec.map(_.map(_ := false.B))
  // dontTouch for debug
  dontTouch(iqPortSelectVecMid)
  dontTouch(iqPortSelectVec)

  // for each issue queue, check the uop of each dp-type-bundle
  iqDpTypeMap.zipWithIndex.foreach{ case (dpTypeMask, iqPortIdx) =>
    // finalDepMap is sort by the priority(deqPortId.size).
    // so first check low priority, then check high priority.
    // let the high priority override low priority.
    require(dpTypeMask.filter(_ == true).size <= 2, "Only 2 dpTypeBundle can be accepted by one issue queue")
    dpTypeMask.indices.foreach{ dpTypeIdx =>
      if (dpTypeMask(dpTypeIdx)) {
      // the dpType Bundle can be accepted by this issue queue
        val selectValidVec = selectMatrix(dpTypeIdx).map(con =>
          con.valid && (con.deqPortId === OHFromInt(iqPortIdx, outs.size)))
        iqPortSelectVecMid(iqPortIdx)(dpTypeIdx).valid := selectValidVec.reduce(_ | _)
        iqPortSelectVecMid(iqPortIdx)(dpTypeIdx).bits  := ParallelMux(selectValidVec, selectMatrix(dpTypeIdx).map(_.dpUopId))

        assert(PopCount(selectValidVec) <= 1.U, "one dpTypeBundle can only give one uop to one issue queue")
      } else {
        iqPortSelectVecMid(iqPortIdx)(dpTypeIdx) := DontCare
        iqPortSelectVecMid(iqPortIdx)(dpTypeIdx).valid := false.B
        // just set false, do not care the bits. No Mux needed
      }
    }
  }

  // for higher priorty, just ignore conflict(PopCount > 1)
  // for lower  prioryt, stall by conflict(PopCount > 1)
  iqPortSelectVec.zip(iqPortSelectVecMid).foreach{
    case (res, mid) => {
      // Actually, at most 2 for PriorityMux, others would be hard-written to false
      assert(PopCount(mid.map(_.valid)) <= 2.U, "one dpTypeBundle can only be assigned to one issue queue")
      res := ParallelPriorityMux(mid.map(_.valid), mid.map(_.bits)).asBools
    }
  }
  // Multiple dispatch uop may be assigned to one issue port,
  // use Priority to choose a higher priority one

  // ===============================
  // almost the same logic, but from the dispatch view
  // Process in Parallel for better latency

  // select result from the dp view, used to generate ready
  // each dpUOp have 'finalPrioFuDeqpMap.size' dispatch result, but only one is Valid(OneHot)
  // If multiply dpUops have the same result(iqPort), the low-priority is false
  // NOTE: as we all know, the best choices is that the higher-index has the low-priority
  //   But, for issue queue view, it causes worse latency to compare dpUop index
  //   So, just use dpType for priority compare(which only needs a Mux)
  //   And, issue queue ready is ignored at select stage
  val dpUopSelectVecMid = Wire(Vec(io.in.size, Vec(finalFuDeqMap.size, Valid(UInt(outs.size.W)))))
  val dpUopSelectVec = Wire(Vec(io.in.size, Vec(outs.size, Bool())))
  dpUopSelectVec.map(_.map(_ := false.B))
  dontTouch(dpUopSelectVecMid)
  dontTouch(dpUopSelectVec)

  dpUopSelectVecMid.indices.foreach{ case uopIdx =>
    finalFuDeqMap.indices.foreach{ case dpTypeIdx =>
      val selectValidVec = selectMatrix(dpTypeIdx).map(con =>
        con.valid && (con.dpUopId === OHFromInt(uopIdx, io.in.size)))
      dpUopSelectVecMid(uopIdx)(dpTypeIdx).valid := selectValidVec.reduce(_ | _)
      dpUopSelectVecMid(uopIdx)(dpTypeIdx).bits  := ParallelMux(selectValidVec, selectMatrix(dpTypeIdx).map(_.deqPortId))
    }
  }

  // TODO: only a few dpTypeBundle and at most 2-level conflicts
  // Use it to remove use-less Mux(Only One-level + ParallelOR is OK)
  // divide the dpType into multi List by Priority(actually two level)
  // Use ParallelMux inside each level for latency
  dpUopSelectVec.zip(dpUopSelectVecMid).foreach{
    case (res, mid) => {
      assert(PopCount(mid.map(_.valid)) <= 1.U, "one dpUop can only be assigned to one issue queue")
      res := ParallelMux(mid.map(_.valid), mid.map(_.bits)).asBools
    }
  }
  val dpUopIqPortReady = VecInit(dpUopSelectVec.map{ case iqPortOH =>
    ParallelOR(iqPortOH.zip(outs.map(_.ready)).map{
        case (chosen, ready) => chosen && ready
    })
  })
  val dpUopBlockByIqPortReady = VecInit(dpUopIqPortReady.map(!_))
  dontTouch(dpUopIqPortReady)
  dontTouch(dpUopBlockByIqPortReady)

  // second-low-priority handling
  // Add a dpType-oritented Vec for conflict-checking
  // Vec(dpType/dpTypePrioLevel, Vec(in.size, Vec(out.size, Bool)))
  // Vec(dpUop.size, Valid(dpTypeIdx/dpTypeLevel/OH))
  // dp_stall = is_low_level & low_level_valid & high_level_valid &  conflict
  // Or Just a conflict Matrix( result is equal)
  // when dpUops' iqIdx is the same, the high-prio is ok, the low-prio is stall
  // Because dpUops can dequeue seperately, so this is OK
  // when not seperately, need better choices generated to avoid dead-clock
  val dpTypeConflictVecMid: Vec[Vec[Bool]] = Wire(Vec(outs.size, Vec(io.in.size, Bool())))
  dpTypeConflictVecMid.map(_.map(_ := false.B))
  val dpTypeConflictVec: Vec[Vec[Bool]] = Wire(Vec(outs.size, Vec(io.in.size, Bool())))
  dontTouch(dpTypeConflictVecMid)
  dontTouch(dpTypeConflictVec)

  dpTypeConflictVecMid.zip(iqPortSelectVecMid).foreach{
    case (conflict, mid) =>
      // actually, at most 2 for ParallelMux, others would be hard-written to false
      // TODO: check it at verilog result for Mux depth
      conflict := ParallelMux(mid.map(_.valid), mid.map(_.bits)).asBools
  }
  dpTypeConflictVec.zip(dpTypeConflictVecMid).foreach {
    case (con, mid) =>
      // TODO: OneHot.checkOneHot use PopCount, may not optimal latency
      // mid.map(_.valid) has a implicit feature that no more than 2 bits are 1
      // Try to use the feature to optimize latency
      val valid = !noMoreThanOH(mid.asUInt)
      con := Mux(valid, ParallelPosteriorityMux(mid, mid.indices.map(OHFromInt(_, mid.size))),
                     0.U(mid.size.W)).asBools
  }
  // each bit for dpUop, true for block
  val dpUopBlockByConflict = ParallelOR(dpTypeConflictVec)
  val dpUopBlockVec = VecInit(dpUopBlockByIqPortReady.zip(dpUopBlockByConflict).map(x => x._1 | x._2))
  dontTouch(dpUopBlockVec)
  dontTouch(dpUopBlockByConflict)

  // use dispatch result for in-out connect
  // connect out.ready to in.ready
  uopsIn <> io.in
  uopsIn.zip(dpUopBlockVec).foreach{ x => x._1.ready := !x._2 }
  // connect in.valid & bits to out.valid & bits
  outs.zip(iqPortSelectVec).foreach{ case (out, select) =>
    out.valid := ParallelOR(select)
    out.bits := ParallelMux(select, uopsIn.map(_.bits))
  }

  // Read BusyTable from the dispatch side
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
    dontTouch(io.readIntState.get)
    require(io.readIntState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readIntState.get.size: ${io.readIntState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readIntState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readIntState.get.map(_.resp).zip(intSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readIntState.get.map(_.dataSource).zip(intDataSourceVec.get).foreach(x => x._2.value := x._1.value)
    io.readIntState.get.map(_.l1ExuOH).zip(intL1ExuOHVec.get).foreach(x => x._2 := x._1)
  }

  if (io.readVfState.isDefined) {
    dontTouch(io.readVfState.get)
    require(io.readVfState.get.size >= reqPsrcVec.size,
      s"[Dispatch2IqArithImp] io.readVfState.get.size: ${io.readVfState.get.size}, psrc size: ${reqPsrcVec.size}")
    io.readVfState.get.map(_.req).zip(reqPsrcVec).foreach(x => x._1 := x._2)
    io.readVfState.get.map(_.resp).zip(vfSrcStateVec.get).foreach(x => x._2 := x._1)
    io.readVfState.get.map(_.dataSource).zip(vfDataSourceVec.get).foreach(x => x._2.value := x._1.value)
    io.readVfState.get.map(_.l1ExuOH).zip(vfL1ExuOHVec.get).foreach(x => x._2 := x._1)
  }

  uopsIn
    .flatMap(x =>
      x.bits.srcState.take(numRegSrc) zip
      x.bits.srcType.take(numRegSrc))
    .zip(
      intSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq)) zip
      vfSrcStateVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(SrcState.busy).toSeq))
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
    .flatMap(x =>
      x.bits.dataSource.take(numRegSrc) zip
      x.bits.srcType.take(numRegSrc))
    .zip(
      intDataSourceVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(DataSource())).toSeq)) zip
      vfDataSourceVec.getOrElse(VecInit(Seq.fill(numEnq * numRegSrc)(0.U.asTypeOf(DataSource())).toSeq))
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
    .flatMap(x =>
      x.bits.l1ExuOH.take(numRegSrc) zip
      x.bits.srcType.take(numRegSrc))
    .zip(
      intL1ExuOHVec.getOrElse(VecInit.fill(numEnq * numRegSrc)(0.U.asTypeOf(ExuOH()))) zip
      vfL1ExuOHVec.getOrElse(VecInit.fill(numEnq * numRegSrc)(0.U.asTypeOf(ExuOH())))
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
  private val dispatchCfg: Seq[(Seq[BigInt], Int)] = Seq(
    (Seq(ldu.ohid), 2),
    (Seq(stu.ohid, mou.ohid), 2),
    (Seq(vldu.ohid), 2),
  )

  private val enqLsqIO = io.enqLsqIO.get

  private val numLoadDeq = LoadPipelineWidth
  private val numStoreAMODeq = StorePipelineWidth
  private val numVLoadDeq = LoadPipelineWidth
  private val numDeq = enqLsqIO.req.size
  private val numEnq = io.in.size

  val dispatchSelect = Module(new Dispatch2IqSelect(numIn = io.in.size, dispatchCfg = dispatchCfg))
  dispatchSelect.io.in := io.in
  private val selectOut = dispatchSelect.io.out
  private val selectIdxOH = dispatchSelect.io.mapIdxOH

  private val s0_in = Wire(io.in.cloneType)
  private val s0_enqLsq_resp = Wire(enqLsqIO.resp.cloneType)
  private val s0_out = Wire(io.out.cloneType)
  private val s0_blockedVec = Wire(Vec(io.in.size, Bool()))

  val iqNotAllReady = !Cat(s0_out.map(_.map(_.ready)).flatten.toSeq).andR
  val lsqCannotAccept = !enqLsqIO.canAccept

  private val isLoadVec = VecInit(io.in.map(x => x.valid && FuType.isLoad(x.bits.fuType)))
  private val isStoreVec = VecInit(io.in.map(x => x.valid && FuType.isStore(x.bits.fuType)))
  private val isAMOVec = io.in.map(x => x.valid && FuType.isAMO(x.bits.fuType))
  private val isStoreAMOVec = io.in.map(x => x.valid && (FuType.isStore(x.bits.fuType) || FuType.isAMO(x.bits.fuType)))
  private val isVLoadVec = VecInit(io.in.map(x => x.valid && FuType.isVLoad(x.bits.fuType)))
  private val isVStoreVec = VecInit(io.in.map(x => x.valid && FuType.isVStore(x.bits.fuType)))

  private val loadCntVec = VecInit(isLoadVec.indices.map(x => PopCount(isLoadVec.slice(0, x + 1))))
  private val storeAMOCntVec = VecInit(isStoreAMOVec.indices.map(x => PopCount(isStoreAMOVec.slice(0, x + 1))))
  private val vloadCntVec = VecInit(isVLoadVec.indices.map(x => PopCount(isVLoadVec.slice(0, x + 1))))

  val loadBlockVec = VecInit(loadCntVec.map(_ > numLoadDeq.U))
  val storeAMOBlockVec = VecInit(storeAMOCntVec.map(_ > numStoreAMODeq.U))
  val vloadBlockVec = VecInit(vloadCntVec.map(_ > numVLoadDeq.U))
  val lsStructBlockVec = VecInit((loadBlockVec.zip(storeAMOBlockVec)).zip(vloadBlockVec).map(x => x._1._1 || x._1._2 || x._2))
  dontTouch(loadBlockVec)
  dontTouch(storeAMOBlockVec)
  dontTouch(lsStructBlockVec)
  dontTouch(vloadBlockVec)
  dontTouch(isLoadVec)
  dontTouch(isVLoadVec)
  dontTouch(loadCntVec)

  s0_in <> io.in

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
    when (!io.in(i).fire) {
      enqLsqIO.needAlloc(i) := 0.U
    }.elsewhen(isStoreVec(i) || isVStoreVec(i)) {
      enqLsqIO.needAlloc(i) := 2.U // store | vstore
    }.otherwise {
      enqLsqIO.needAlloc(i) := 1.U // load | vload
    }
    enqLsqIO.req(i).valid := io.in(i).fire && !FuType.isAMO(io.in(i).bits.fuType)
    enqLsqIO.req(i).bits := io.in(i).bits
    s0_enqLsq_resp(i) := enqLsqIO.resp(i)
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

  s0_in.flatMap(x => x.bits.srcState.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intSrcStateVec.flatten zip vfSrcStateVec.flatten).foreach {
    case ((state: UInt, srcType), (intState, vfState)) =>
      state := Mux1H(Seq(
        SrcType.isXp(srcType) -> intState,
        SrcType.isVfp(srcType) -> vfState,
        SrcType.isNotReg(srcType) -> true.B,
      ))
  }
  s0_in.flatMap(x => x.bits.dataSource.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intDataSourceVec.flatten zip vfDataSourceVec.flatten).foreach {
    case ((dataSource, srcType), (intSource, vfSource)) =>
      dataSource.value := Mux1H(Seq(
        SrcType.isXp(srcType) -> intSource.value,
        SrcType.isVfp(srcType) -> vfSource.value,
        SrcType.isNotReg(srcType) -> 0.U,
      ))
  }
  s0_in.flatMap(x => x.bits.l1ExuOH.take(numRegSrc) zip x.bits.srcType.take(numRegSrc)).zip(intL1ExuOHVec.flatten zip vfL1ExuOHVec.flatten).foreach {
    case ((l1ExuOH, srcType), (intL1ExuOH, vfL1ExuOH)) =>
      l1ExuOH := Mux1H(Seq(
        SrcType.isXp(srcType) -> intL1ExuOH,
        SrcType.isVfp(srcType) -> vfL1ExuOH,
        SrcType.isNotReg(srcType) -> 0.U,
      ))
  }

  val a: IndexedSeq[Vec[DataSource]] = s0_in.map(_.bits.dataSource)

  for ((iqPorts, iqIdx) <- s0_out.zipWithIndex) {
    for ((port, portIdx) <- iqPorts.zipWithIndex) {
      println(s"[Dispatch2MemIQ] (iqIdx, portIdx): ($iqIdx, $portIdx)")
      when (iqNotAllReady || lsqCannotAccept) {
        s0_out.foreach(_.foreach(_.valid := false.B))
        s0_out.foreach(_.foreach(x => x.bits := 0.U.asTypeOf(x.bits)))
      }.otherwise {
        s0_out(iqIdx)(portIdx).valid := selectOut(iqIdx)(portIdx).valid && !Mux1H(selectIdxOH(iqIdx)(portIdx), s0_blockedVec)
        s0_out(iqIdx)(portIdx).bits := selectOut(iqIdx)(portIdx).bits // the same as Mux1H(selectIdxOH(iqIdx)(portIdx), s0_in.map(_.bits))
        s0_out(iqIdx)(portIdx).bits.srcState := Mux1H(selectIdxOH(iqIdx)(portIdx), s0_in.map(_.bits.srcState))
        s0_out(iqIdx)(portIdx).bits.dataSource := Mux1H(selectIdxOH(iqIdx)(portIdx), s0_in.map(_.bits.dataSource))
        s0_out(iqIdx)(portIdx).bits.l1ExuOH := Mux1H(selectIdxOH(iqIdx)(portIdx), s0_in.map(_.bits.l1ExuOH))
        s0_out(iqIdx)(portIdx).bits.lqIdx := Mux1H(selectIdxOH(iqIdx)(portIdx), s0_enqLsq_resp.map(_.lqIdx))
        s0_out(iqIdx)(portIdx).bits.sqIdx := Mux1H(selectIdxOH(iqIdx)(portIdx), s0_enqLsq_resp.map(_.sqIdx))
      }
    }
  }

  // outToInMap(inIdx)(outIdx): the inst numbered inIdx will be accepted by port numbered outIdx
  val outToInMap: Vec[Vec[Bool]] = VecInit(selectIdxOH.flatten.map(x => x.asBools).transpose.map(x => VecInit(x.toSeq)).toSeq)
  val outReadyVec: Vec[Bool] = VecInit(s0_out.map(_.map(_.ready)).flatten.toSeq)
  dontTouch(outToInMap)
  dontTouch(outReadyVec)

  s0_in.zipWithIndex.zip(outToInMap).foreach { case ((in, inIdx), outVec) =>
    when (iqNotAllReady || lsqCannotAccept) {
      in.ready := false.B
    }.otherwise {
      in.ready := (Cat(outVec) & Cat(outReadyVec)).orR && !s0_blockedVec(inIdx)
    }
  }

  io.out <> s0_out
}
