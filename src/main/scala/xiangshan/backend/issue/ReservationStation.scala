/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import utils._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.fu.FuConfig
import xiangshan.mem.{SqPtr, MemWaitUpdateReq}
import xiangshan.backend.fu.fpu.{FMAMidResult, FMAMidResultIO}

import scala.math.max

case class RSParams
(
  var numEntries: Int = 0,
  var numEnq: Int = 0,
  var numDeq: Int = 0,
  var numSrc: Int = 0,
  var dataBits: Int = 0,
  var dataIdBits: Int = 0,
  var numFastWakeup: Int = 0,
  var numWakeup: Int = 0,
  var hasFeedback: Boolean = false,
  var fixedLatency: Int = -1,
  var checkWaitBit: Boolean = false,
  var optBuf: Boolean = false,
  // special cases
  var isJump: Boolean = false,
  var isAlu: Boolean = false,
  var isStore: Boolean = false,
  var isMul: Boolean = false,
  var isLoad: Boolean = false,
  var isStoreData: Boolean = false,
  var exuCfg: Option[ExuConfig] = None
){
  def allWakeup: Int = numFastWakeup + numWakeup
  def indexWidth: Int = log2Up(numEntries)
  // oldestFirst: (Enable_or_not, Need_balance, Victim_index)
  def oldestFirst: (Boolean, Boolean, Int) = (true, !isLoad, if (isLoad) 0 else numDeq - 1)
  def hasMidState: Boolean = exuCfg.get == FmacExeUnitCfg
  def delayedRf: Boolean = exuCfg.get == StdExeUnitCfg
  def needScheduledBit: Boolean = hasFeedback || delayedRf || hasMidState
  def needBalance: Boolean = exuCfg.get.needLoadBalance
  def numSelect: Int = numDeq + numEnq + (if (oldestFirst._1) 1 else 0)
  def dropOnRedirect: Boolean = !(isLoad || isStore || isStoreData)

  override def toString: String = {
    s"type ${exuCfg.get.name}, size $numEntries, enq $numEnq, deq $numDeq, numSrc $numSrc, fast $numFastWakeup, wakeup $numWakeup"
  }
}

class ReservationStationWrapper(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val params = new RSParams

  def addIssuePort(cfg: ExuConfig, deq: Int): Unit = {
    require(params.numEnq == 0, "issue ports should be added before dispatch ports")
    params.dataBits = XLEN
    params.dataIdBits = PhyRegIdxWidth
    params.numEntries += IssQueSize * deq
    params.numDeq = deq
    params.numSrc = max(params.numSrc, max(cfg.intSrcCnt, cfg.fpSrcCnt))
    params.exuCfg = Some(cfg)
    cfg match {
      case JumpCSRExeUnitCfg => params.isJump = true
      case AluExeUnitCfg => params.isAlu = true
      case StaExeUnitCfg => params.isStore = true
      case StdExeUnitCfg => params.isStoreData = true
      case MulDivExeUnitCfg => params.isMul = true
      case LdExeUnitCfg => params.isLoad = true
      case _ =>
    }
    // TODO: why jump needs two sources?
    if (cfg == JumpCSRExeUnitCfg) {
      params.numSrc = 2
    }
    if (cfg == StaExeUnitCfg || cfg == LdExeUnitCfg) {
      params.hasFeedback = true
      params.checkWaitBit = true
    }
    if (cfg.hasCertainLatency) {
      params.fixedLatency = if (cfg == MulDivExeUnitCfg) mulCfg.latency.latencyVal.get else cfg.latency.latencyVal.get
    }
  }

  def addDispatchPort(): Seq[FuConfig] = {
    params.numEnq += 1
    params.exuCfg.get.fuConfigs
  }
  def addEarlyWakeup(num: Int) = {
    params.numFastWakeup += num
  }
  def addWakeup(num: Int) = {
    params.numWakeup += num
  }
  def canAccept(fuType: UInt): Bool = params.exuCfg.get.canAccept(fuType)
  def intSrcCnt = params.exuCfg.get.intSrcCnt
  def fpSrcCnt = params.exuCfg.get.fpSrcCnt
  def numOutFastWakeupPort: Int = if (params.fixedLatency >= 0) params.numDeq else 0
  def numExtFastWakeupPort: Int = if (params.exuCfg.get == LdExeUnitCfg) params.numDeq else 0
  def numAllFastWakeupPort: Int = numOutFastWakeupPort + numExtFastWakeupPort
  def numIntWbPort: Int = {
    val privatePort = params.exuCfg.get.writeIntRf && params.exuCfg.get.wbIntPriority <= 1
    if (privatePort) params.numDeq else 0
  }
  def numFpWbPort: Int = {
    val privatePort = params.exuCfg.get.writeFpRf && params.exuCfg.get.wbFpPriority <= 1
    if (privatePort) params.numDeq else 0
  }
  def wbIntPriority: Int = params.exuCfg.get.wbIntPriority
  def wbFpPriority: Int = params.exuCfg.get.wbFpPriority

  override def toString: String = params.toString
  // for better timing, we limits the size of RS to 2-deq
  val maxRsDeq = 2
  def numRS = (params.numDeq + (maxRsDeq - 1)) / maxRsDeq

  lazy val module = new LazyModuleImp(this) with HasPerfEvents {
    require(params.numEnq < params.numDeq || params.numEnq % params.numDeq == 0)
    require(params.numEntries % params.numDeq == 0)
    val rs = (0 until numRS).map(i => {
      val numDeq = Seq(params.numDeq - maxRsDeq * i, maxRsDeq).min
      val numEnq = params.numEnq / numRS
      val numEntries = numDeq * params.numEntries / params.numDeq
      val rsParam = params.copy(numEnq = numEnq, numDeq = numDeq, numEntries = numEntries)
      val updatedP = p.alter((site, here, up) => {
        case XSCoreParamsKey => up(XSCoreParamsKey).copy(
          IssQueSize = numEntries
        )
      })
      Module(new ReservationStation(rsParam)(updatedP))
    })

    val updatedP = p.alter((site, here, up) => {
      case XSCoreParamsKey => up(XSCoreParamsKey).copy(
        IssQueSize = rs.map(_.size).max
      )
    })
    val io = IO(new ReservationStationIO(params)(updatedP))

    rs.foreach(_.io.redirect <> io.redirect)
    io.numExist <> rs.map(_.io.numExist).reduce(_ +& _)
    io.fromDispatch <> rs.flatMap(_.io.fromDispatch)
    io.srcRegValue <> rs.flatMap(_.io.srcRegValue)
    if (io.fpRegValue.isDefined) {
      io.fpRegValue.get <> rs.flatMap(_.io.fpRegValue.get)
    }
    io.deq <> rs.flatMap(_.io.deq)
    rs.foreach(_.io.fastUopsIn <> io.fastUopsIn)
    rs.foreach(_.io.fastDatas <> io.fastDatas)
    rs.foreach(_.io.slowPorts <> io.slowPorts)
    if (io.fastWakeup.isDefined) {
      io.fastWakeup.get <> rs.flatMap(_.io.fastWakeup.get)
    }
    if (io.jump.isDefined) {
      rs.foreach(_.io.jump.get <> io.jump.get)
    }
    if (io.feedback.isDefined) {
      io.feedback.get <> rs.flatMap(_.io.feedback.get)
    }
    if (io.checkwait.isDefined) {
     rs.foreach(_.io.checkwait.get <> io.checkwait.get)
    }
    if (io.load.isDefined) {
      io.load.get.fastMatch <> rs.flatMap(_.io.load.get.fastMatch)
    }
    if (io.fmaMid.isDefined) {
      io.fmaMid.get <> rs.flatMap(_.io.fmaMid.get)
    }

    val perfEvents = rs.flatMap(_.getPerfEvents)
    generatePerfEvent()
  }

  var fastWakeupIdx = 0
  def connectFastWakeup(uop: ValidIO[MicroOp], data: UInt): Unit = {
    module.io.fastUopsIn(fastWakeupIdx) := uop
    module.io.fastDatas(fastWakeupIdx) := data
    fastWakeupIdx += 1
  }
  def connectFastWakeup(uop: Seq[ValidIO[MicroOp]], data: Seq[UInt]): Unit = {
    for ((u, d) <- uop.zip(data)) {
      connectFastWakeup(u, d)
    }
  }
}

class ReservationStationIO(params: RSParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  val numExist = Output(UInt(log2Up(params.numEntries + 1).W))
  // enq
  val fromDispatch = Vec(params.numEnq, Flipped(DecoupledIO(new MicroOp)))
  val srcRegValue = Vec(params.numEnq, Input(Vec(params.numSrc, UInt(params.dataBits.W))))
  val fpRegValue = if (params.delayedRf) Some(Vec(params.numEnq, Input(UInt(params.dataBits.W)))) else None
  // deq
  val deq = Vec(params.numDeq, DecoupledIO(new ExuInput))
  // wakeup
  val fastUopsIn = Vec(params.numFastWakeup, Flipped(ValidIO(new MicroOp)))
  val fastDatas = Vec(params.numFastWakeup, Input(UInt(params.dataBits.W)))
  val slowPorts = Vec(params.numWakeup, Flipped(ValidIO(new ExuOutput)))
  // extra
  val fastWakeup = if (params.fixedLatency >= 0) Some(Vec(params.numDeq, ValidIO(new MicroOp))) else None
  val jump = if (params.isJump) Some(new Bundle {
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
  }) else None
  val feedback = if (params.hasFeedback) Some(Vec(params.numDeq,
    Flipped(new MemRSFeedbackIO)
  )) else None
  val checkwait = if (params.checkWaitBit) Some(new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput)))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }) else None
  val load = if (params.isLoad) Some(new Bundle {
    val fastMatch = Vec(params.numDeq, Output(UInt(exuParameters.LduCnt.W)))
  }) else None
  val fmaMid = if (params.exuCfg.get == FmacExeUnitCfg) Some(Vec(params.numDeq, Flipped(new FMAMidResultIO))) else None
}

class ReservationStation(params: RSParams)(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new ReservationStationIO(params))

  val statusArray = Module(new StatusArray(params))
  val select = Module(new SelectPolicy(params))
  val dataArray = Module(new DataArray(params))
  val payloadArray = Module(new PayloadArray(new MicroOp, params))

  val s2_deq = Wire(io.deq.cloneType)

  /**
    * S0: Update status (from wakeup) and schedule possible instructions to issue.
    * Instructions from dispatch will be always latched and bypassed to S1.
    */
  // common data
  val s0_allocatePtrOH = VecInit(select.io.allocate.map(_.bits))
  val s0_allocatePtr = VecInit(s0_allocatePtrOH.map(ptrOH => OHToUInt(ptrOH)))
  val s0_enqFlushed = Wire(Vec(params.numEnq, Bool()))
  val s0_enqWakeup = Wire(Vec(params.numEnq, Vec(params.numSrc, UInt(params.numWakeup.W))))
  val s0_enqDataCapture = Wire(Vec(params.numEnq, Vec(params.numSrc, UInt(params.numWakeup.W))))
  val s0_fastWakeup = Wire(Vec(params.numEnq, Vec(params.numSrc, Vec(params.numFastWakeup, Bool()))))
  val s0_doEnqueue = Wire(Vec(params.numEnq, Bool()))

  // Allocation: uops from dispatch
  val validAfterAllocate = RegInit(0.U(params.numEntries.W))
  val validUpdateByAllocate = ParallelMux(s0_doEnqueue, s0_allocatePtrOH)
  validAfterAllocate := statusArray.io.isValidNext | validUpdateByAllocate
  select.io.validVec := validAfterAllocate

  for (i <- 0 until params.numEnq) {
    io.fromDispatch(i).ready := select.io.allocate(i).valid
    s0_enqFlushed(i) := (if (params.dropOnRedirect) io.redirect.valid else io.fromDispatch(i).bits.robIdx.needFlush(io.redirect))
    s0_doEnqueue(i) := io.fromDispatch(i).fire && !s0_enqFlushed(i)
    val wakeup = io.slowPorts.map(_.bits.uop.wakeup(io.fromDispatch(i).bits, params.exuCfg.get))
    val slowWakeup = io.slowPorts.map(_.bits.uop.wakeup(io.fromDispatch(i).bits, params.exuCfg.get))
    val fastWakeup = io.fastUopsIn.map(_.bits.wakeup(io.fromDispatch(i).bits, params.exuCfg.get))
    for (j <- 0 until params.numSrc) {
      val (slowStateMatch, slowDataMatch) = slowWakeup.map(_(j)).unzip
      s0_enqWakeup(i)(j) := VecInit(io.slowPorts.zip(slowStateMatch).map(x => x._1.valid && x._2)).asUInt
      s0_enqDataCapture(i)(j) := VecInit(io.slowPorts.zip(slowDataMatch).map(x => x._1.valid && x._2)).asUInt
      val (_, fastDataMatch) = fastWakeup.map(_(j)).unzip
      s0_fastWakeup(i)(j) := io.fastUopsIn.zip(fastDataMatch).map(x => x._1.valid && x._2)
    }
  }
  io.numExist := PopCount(validAfterAllocate)


  // Wakeup: uop from fastPort and exuOutput from slowPorts
  val wakeupValid = io.fastUopsIn.map(_.valid) ++ io.slowPorts.map(_.valid)
  val wakeupDest = io.fastUopsIn.map(_.bits) ++ io.slowPorts.map(_.bits.uop)
  for ((wakeup, (valid, dest)) <- statusArray.io.wakeup.zip(wakeupValid.zip(wakeupDest))) {
    wakeup.valid := valid
    wakeup.bits := dest
  }

  // select the issue instructions
  // Option 1: normal selection (do not care about the age)
  select.io.request := statusArray.io.canIssue
  // Option 2: select the oldest
  val enqVec = VecInit(s0_doEnqueue.zip(s0_allocatePtrOH).map{ case (d, b) => RegNext(Mux(d, b, 0.U)) })
  val s0_oldestSel = AgeDetector(params.numEntries, enqVec, statusArray.io.flushed, statusArray.io.canIssue)

  // send address to read uop and data
  // For better timing, we read the payload array before we determine which instruction to issue.
  // In this way, selection and payload read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    payloadArray.io.read(i).addr := select.io.grant(i).bits
  }
  payloadArray.io.read.last.addr := s0_oldestSel.bits

  // For better timing, we add one more read port to data array when oldestFirst is enabled,
  // and select data after the arbiter decides which one to issue.
  // In this way, selection and data read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    dataArray.io.read(i).addr := select.io.grant(i).bits
  }
  dataArray.io.read.last.addr := s0_oldestSel.bits

  /**
    * S1: read uop and data
    */
  val s1_slowPorts = RegNext(io.slowPorts)
  val s1_fastUops = RegNext(io.fastUopsIn)
  val s1_dispatchUops = Reg(Vec(params.numEnq, Valid(new MicroOp)))
  val s1_allocatePtrOH = RegNext(s0_allocatePtrOH)
  val s1_allocatePtr = RegNext(s0_allocatePtr)
  val s1_enqWakeup = RegNext(s0_enqWakeup)
  val s1_enqDataCapture = RegNext(s0_enqDataCapture)
  val s1_fastWakeup = RegNext(s0_fastWakeup)
  val s1_in_selectPtr = RegNext(select.io.grant)
  val s1_in_selectPtrValid = s1_in_selectPtr.map(_.valid)
  val s1_in_selectPtrOH = s1_in_selectPtr.map(_.bits)
  val s1_in_oldestPtrOH = RegNext(s0_oldestSel)
  val s1_issue_oldest = Wire(Vec(params.numDeq, Bool()))
  val s1_issue_dispatch = Wire(Vec(params.numDeq, Bool()))
  val s1_out = Wire(Vec(params.numDeq, Decoupled(new ExuInput)))
  val s1_issuePtrOH = Wire(Vec(params.numDeq, Valid(UInt(params.numEntries.W))))
  val s1_issuePtr = s1_issuePtrOH.map(iss => OHToUInt(iss.bits))

  // Allocation: store dispatch uops into payload and data array
  s1_dispatchUops.zip(io.fromDispatch).zipWithIndex.foreach{ case ((uop, in), i) =>
    val s0_valid = in.fire && !s0_enqFlushed(i)
    uop.valid := s0_valid
    when (s0_valid) {
      uop.bits := in.bits
    }
  }
  // update status and payload array
  statusArray.io.redirect := io.redirect
  val needFpSource = s1_dispatchUops.map(_.bits.needRfRPort(0, true, false))
  for ((statusUpdate, i) <- statusArray.io.update.zipWithIndex) {
    statusUpdate.enable := s1_dispatchUops(i).valid
    statusUpdate.addr := s1_allocatePtrOH(i)
    statusUpdate.data.valid := true.B
    val waitForFpSource = if (params.delayedRf) needFpSource(i) else false.B
    statusUpdate.data.scheduled := waitForFpSource
    statusUpdate.data.blocked := params.checkWaitBit.B && s1_dispatchUops(i).bits.cf.loadWaitBit
    statusUpdate.data.credit := Mux(waitForFpSource, 2.U, 0.U)
    for (j <- 0 until params.numSrc) {
      statusUpdate.data.srcState(j) := s1_dispatchUops(i).bits.srcIsReady(j) || s1_enqWakeup(i)(j).asUInt.orR || s1_fastWakeup(i)(j).asUInt.orR
    }
    statusUpdate.data.midState := false.B
    statusUpdate.data.psrc := s1_dispatchUops(i).bits.psrc.take(params.numSrc)
    statusUpdate.data.srcType := s1_dispatchUops(i).bits.ctrl.srcType.take(params.numSrc)
    statusUpdate.data.robIdx := s1_dispatchUops(i).bits.robIdx
    statusUpdate.data.sqIdx := s1_dispatchUops(i).bits.sqIdx
    statusUpdate.data.waitForSqIdx := DontCare // generated by sq, will be updated later
    statusUpdate.data.waitForRobIdx := s1_dispatchUops(i).bits.cf.waitForRobIdx // generated by mdp
    statusUpdate.data.waitForStoreData := false.B
    statusUpdate.data.strictWait := s1_dispatchUops(i).bits.cf.loadWaitStrict
    statusUpdate.data.isFirstIssue := true.B
  }
  // We need to block issue until the corresponding store issues.
  if (io.checkwait.isDefined) {
    statusArray.io.stIssuePtr := io.checkwait.get.stIssuePtr
    statusArray.io.memWaitUpdateReq := io.checkwait.get.memWaitUpdateReq
  }
  for ((payloadWrite, i) <- payloadArray.io.write.zipWithIndex) {
    payloadWrite.enable := s1_dispatchUops(i).valid
    payloadWrite.addr := s1_allocatePtrOH(i)
    payloadWrite.data := s1_dispatchUops(i).bits
    payloadWrite.data.debugInfo.enqRsTime := GTimer()
  }

  // Issue with priorities: (1) oldest uop; (2) selected uops; (3) dispatched uops.

  for ((issueGrant, i) <- statusArray.io.issueGranted.take(params.numEnq).zipWithIndex) {
    issueGrant.valid := (if (i >= params.numDeq) false.B else s1_issue_dispatch(i) && s1_out(i).ready)
    issueGrant.bits := s1_allocatePtrOH(i)
  }
  for ((issueGrant, i) <- statusArray.io.issueGranted.drop(params.numEnq).take(params.numDeq).zipWithIndex) {
    issueGrant.valid := s1_in_selectPtrValid(i) && !s1_issue_oldest(i) && s1_out(i).ready
    issueGrant.bits := s1_in_selectPtrOH(i)
  }
  if (params.oldestFirst._1) {
    statusArray.io.issueGranted.last.valid := ParallelMux(s1_issue_oldest, s1_out.map(_.ready))
    statusArray.io.issueGranted.last.bits := s1_in_oldestPtrOH.bits
  }

  s1_issue_oldest.foreach(_ := false.B)
  if (params.oldestFirst._1) {
    // When the reservation station has oldestFirst, we need to issue the oldest instruction if possible.
    // However, in this case, the select policy always selects at maximum numDeq instructions to issue.
    // Thus, we need an arbitration between the numDeq + 1 possibilities.
    val oldestSelection = Module(new OldestSelection(params))
    oldestSelection.io.in := s1_in_selectPtr
    oldestSelection.io.oldest := s1_in_oldestPtrOH
    // By default, we use the default victim index set in parameters.
    oldestSelection.io.canOverride := (0 until params.numDeq).map(_ == params.oldestFirst._3).map(_.B)
    // When deq width is two, we have a balance bit to indicate selection priorities.
    // For better performance, we decide the victim according to selection priorities.
    if (params.needBalance && params.oldestFirst._2 && params.numDeq == 2) {
      // When balance2 bit is set, selection prefers the second selection port.
      // Thus, the first is the victim if balance2 bit is set.
      oldestSelection.io.canOverride(0) := select.io.grantBalance
      oldestSelection.io.canOverride(1) := !select.io.grantBalance
    }
    s1_issue_oldest := oldestSelection.io.isOverrided
  }

  // Do the read data arbitration
  val s1_is_first_issue = Wire(Vec(params.numDeq, Bool()))
  val s1_all_src_ready = Wire(Vec(params.numDeq, Bool()))
  for (i <- 0 until params.numDeq) {
    val canBypass = s1_dispatchUops(i).valid && statusArray.io.update(i).data.canIssue
    s1_issue_dispatch(i) := canBypass && !s1_issue_oldest(i) && !s1_in_selectPtrValid(i)

    s1_issuePtrOH(i).valid := s1_issue_oldest(i) || s1_in_selectPtrValid(i) || canBypass
    s1_issuePtrOH(i).bits := Mux(s1_issue_oldest(i), s1_in_oldestPtrOH.bits,
      Mux(s1_in_selectPtrValid(i), s1_in_selectPtrOH(i), s1_allocatePtrOH(i)))

    s1_out(i).bits.uop := Mux(s1_issue_oldest(i), payloadArray.io.read.last.data,
      Mux(s1_in_selectPtrValid(i), payloadArray.io.read(i).data, s1_dispatchUops(i).bits))
    s1_is_first_issue(i) := Mux(s1_issue_oldest(i), statusArray.io.isFirstIssue.last,
      Mux(s1_in_selectPtrValid(i), statusArray.io.isFirstIssue(params.numEnq + i),
        statusArray.io.update(i).data.isFirstIssue))
    s1_all_src_ready(i) := Mux(s1_issue_oldest(i), statusArray.io.allSrcReady.last,
        Mux(s1_in_selectPtrValid(i), statusArray.io.allSrcReady(params.numEnq + i),
          statusArray.io.update(i).data.allSrcReady))
  }
  s1_out.foreach(_.bits.uop.debugInfo.selectTime := GTimer())

  for (i <- 0 until params.numDeq) {
    s1_out(i).valid := s1_issuePtrOH(i).valid && !s1_out(i).bits.uop.robIdx.needFlush(io.redirect)
    if (io.feedback.isDefined) {
      // feedbackSlow
      statusArray.io.deqResp(2*i).valid := io.feedback.get(i).feedbackSlow.valid
      statusArray.io.deqResp(2*i).bits.rsMask := UIntToOH(io.feedback.get(i).feedbackSlow.bits.rsIdx)
      statusArray.io.deqResp(2*i).bits.success := io.feedback.get(i).feedbackSlow.bits.hit
      statusArray.io.deqResp(2*i).bits.resptype := io.feedback.get(i).feedbackSlow.bits.sourceType
      statusArray.io.deqResp(2*i).bits.dataInvalidSqIdx := io.feedback.get(i).feedbackSlow.bits.dataInvalidSqIdx
      // feedbackFast, for load pipeline only
      statusArray.io.deqResp(2*i+1).valid := io.feedback.get(i).feedbackFast.valid
      statusArray.io.deqResp(2*i+1).bits.rsMask := UIntToOH(io.feedback.get(i).feedbackFast.bits.rsIdx)
      statusArray.io.deqResp(2*i+1).bits.success := io.feedback.get(i).feedbackFast.bits.hit
      statusArray.io.deqResp(2*i+1).bits.resptype := io.feedback.get(i).feedbackFast.bits.sourceType
      statusArray.io.deqResp(2*i+1).bits.dataInvalidSqIdx := DontCare
    } else {
      // For FMAs that can be scheduled multiple times, only when
      // all source operands are ready we dequeue the instruction.
      val allSrcReady = if (params.hasMidState) s1_all_src_ready(i) else true.B
      statusArray.io.deqResp(i).valid := s1_issuePtrOH(i).valid && s1_out(i).ready && allSrcReady
      statusArray.io.deqResp(i).bits.rsMask := s1_issuePtrOH(i).bits
      statusArray.io.deqResp(i).bits.success := s2_deq(i).ready
      statusArray.io.deqResp(i).bits.resptype := DontCare
      statusArray.io.deqResp(i).bits.dataInvalidSqIdx := DontCare
    }

    if (io.fastWakeup.isDefined) {
      val wakeupQueue = Module(new WakeupQueue(params.fixedLatency))
      val fuCheck = if (params.isMul) s1_out(i).bits.uop.ctrl.fuType === FuType.mul else true.B
      // TODO: optimize timing here since ready may be slow
      wakeupQueue.io.in.valid := s1_issuePtrOH(i).valid && s1_out(i).ready && fuCheck
      wakeupQueue.io.in.bits := s1_out(i).bits.uop
      wakeupQueue.io.in.bits.debugInfo.issueTime := GTimer() + 1.U
      wakeupQueue.io.redirect := io.redirect
      io.fastWakeup.get(i) := wakeupQueue.io.out
      XSPerfAccumulate(s"fast_blocked_$i", s1_issuePtrOH(i).valid && fuCheck && !s1_out(i).ready)
    }
  }
  statusArray.io.updateMidState := 0.U

  // select whether the source is from (whether slowPorts, regfile or imm)
  // for read-after-issue, it's done over the selected uop
  // for read-before-issue, it's done over the enqueue uop (and store the imm in dataArray to save space)
  // TODO: need to bypass data here.
  val immBypassedData = Wire(Vec(params.numEnq, Vec(params.numSrc, UInt(params.dataBits.W))))
  for (((uop, data), bypass) <- s1_dispatchUops.map(_.bits).zip(io.srcRegValue).zip(immBypassedData)) {
    val jumpPc = if (io.jump.isDefined) Some(io.jump.get.jumpPc) else None
    val jalr_target = if (io.jump.isDefined) Some(io.jump.get.jalr_target) else None
    bypass := ImmExtractor(params, uop, data, jumpPc, jalr_target)
  }

  /**
    * S1: Data broadcast (from Regfile and FUs) and read
    *
    * Note: this is only needed when read-before-issue
    */
  // dispatch data: the next cycle after enqueue
  for (i <- 0 until params.numEnq) {
    dataArray.io.write(i).enable := s1_dispatchUops(i).valid
    dataArray.io.write(i).mask := s1_dispatchUops(i).bits.srcIsReady.take(params.numSrc)
    if (params.delayedRf) {
      when (needFpSource(i)) {
        dataArray.io.write(i).mask.head := false.B
      }
    }
    dataArray.io.write(i).addr := s1_allocatePtrOH(i)
    dataArray.io.write(i).data := immBypassedData(i)
    if (params.delayedRf) {
      dataArray.io.delayedWrite(i).valid := RegNext(s1_dispatchUops(i).valid && needFpSource(i))
      dataArray.io.delayedWrite(i).bits := io.fpRegValue.get(i)
    }
  }
  // data broadcast: from function units (only slow wakeup date are needed)
  val broadcastValid = io.slowPorts.map(_.valid)
  val broadcastValue = VecInit(io.slowPorts.map(_.bits.data))
  require(broadcastValid.size == params.numWakeup)
  require(broadcastValue.size == params.numWakeup)
  val slowWakeupMatchVec = Reg(Vec(params.numEntries, Vec(params.numSrc, UInt(params.numWakeup.W))))
  for (i <- 0 until params.numEntries) {
    for (j <- 0 until params.numSrc) {
      slowWakeupMatchVec(i)(j) := statusArray.io.wakeupMatch(i)(j)(params.allWakeup - 1, params.numFastWakeup)
    }
  }
  dataArray.io.multiWrite.zipWithIndex.foreach { case (w, i) =>
    w.enable := RegNext(broadcastValid(i))
    for (j <- 0 until params.numSrc) {
      val allocateValid = s1_enqDataCapture.zip(s1_dispatchUops).map(x => x._1(j)(i) && x._2.valid)
      val allocateDataCapture = ParallelMux(allocateValid, s1_allocatePtrOH)
      w.addr(j) := VecInit(slowWakeupMatchVec.map(_(j)(i))).asUInt | allocateDataCapture
    }
    w.data := RegEnable(broadcastValue(i), broadcastValid(i))
  }

  /**
    * S1: read data from regfile
    */
  // Do the read data arbitration
  class DataSelect(implicit p: Parameters) extends XSModule {
    val io = IO(new Bundle {
      // one for override data, the others for original data
      val doOverride = Vec(params.numDeq, Input(Bool()))
      val readData = Vec(dataArray.io.read.length, Vec(params.numSrc, Input(UInt(params.dataBits.W))))
      // for data bypass from slowPorts
      val fromSlowPorts = Vec(params.numDeq, Vec(params.numSrc, Input(UInt(dataArray.io.multiWrite.length.W))))
      val slowData = Vec(dataArray.io.multiWrite.length, Input(UInt(params.dataBits.W)))
      // for enq data
      val enqBypass = Vec(params.numDeq, Vec(params.numEnq, Input(Bool())))
      val enqData = Vec(params.numEnq, Vec(params.numSrc, Flipped(ValidIO(UInt(params.dataBits.W)))))
      // deq data
      val deqData = Vec(params.numDeq, Vec(params.numSrc, Output(UInt(params.dataBits.W))))
    })

    for ((deq, i) <- io.deqData.zipWithIndex) {
      // default deq data is selected from data array
      deq := Mux(io.doOverride(i), io.readData.last, io.readData(i))
      // when instructions are selected for dequeue after enq, we need to bypass data.
      val bypassData = Mux1H(io.enqBypass(i), io.enqData)
      io.fromSlowPorts(i).zip(bypassData).zip(io.deqData(i)).foreach{ case ((bySlow, byData), deq) =>
        when (byData.valid && io.enqBypass(i).asUInt.orR) {
          deq := byData.bits
        }
        when (bySlow.orR) {
          deq := Mux1H(bySlow, io.slowData)
        }
      }
    }
  }

  // for read-before-issue, we need to bypass the enqueue data here
  // for read-after-issue, we need to bypass the imm here
  s1_out.foreach(_.bits.src := DontCare)
  // check enq data bypass (another form of broadcast except that we know where it hits) here
  val s1_select_bypass_s0 = Wire(Vec(params.numDeq, Vec(params.numEnq, Bool())))
  for ((bypass, i) <- s1_select_bypass_s0.zipWithIndex) {
    // bypass: Vec(config.numEnq, Bool())
    bypass.foreach(_ := false.B)
    bypass(i) := s1_issue_dispatch(i)
  }

  val dataSelect = Module(new DataSelect)
  dataSelect.io.doOverride := s1_issue_oldest
  dataSelect.io.readData := dataArray.io.read.map(_.data)
  for ((port, issuePtrOH) <- dataSelect.io.fromSlowPorts.zip(s1_issuePtrOH)) {
    for (j <- 0 until params.numSrc) {
      port(j) := VecInit(dataArray.io.multiWrite.map(w => w.enable && Mux1H(issuePtrOH.bits, w.addr(j)))).asUInt
    }
  }
  dataSelect.io.slowData := dataArray.io.multiWrite.map(_.data)
  dataSelect.io.enqBypass := s1_select_bypass_s0
  for ((enq, i) <- dataSelect.io.enqData.zipWithIndex) {
    for (j <- 0 until params.numSrc) {
      enq(j).valid := RegNext(io.fromDispatch(i).bits.srcIsReady(j))
      enq(j).bits := immBypassedData(i)(j)
    }
  }
  for (i <- 0 until params.numDeq) {
    for (j <- 0 until params.numSrc) {
      s1_out(i).bits.src(j) := dataSelect.io.deqData(i)(j)
    }
  }

  /**
    * S1: detect bypass from fast wakeup
    */
  // control: check the fast wakeup match
  val fastWakeupMatch = Reg(Vec(params.numEntries, Vec(params.numSrc, Vec(params.numFastWakeup, Bool()))))
  for (i <- 0 until params.numEntries) {
    for (j <- 0 until params.numSrc) {
      fastWakeupMatch(i)(j) := statusArray.io.wakeupMatch(i)(j).asBools.take(params.numFastWakeup)
    }
  }

  /**
    * S2: to function units
    */
  val s1_out_fire = s1_out.zip(s2_deq).map(x => x._1.valid && x._2.ready)
  val s2_issuePtr = s1_issuePtr.zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  val s2_issuePtrOH = s1_issuePtrOH.map(_.bits).zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  val s2_first_issue = s1_is_first_issue.zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  val s2_all_src_ready = s1_all_src_ready.zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  for (i <- 0 until params.numDeq) {
    // payload: send to function units
    // TODO: these should be done outside RS
    PipelineConnect(s1_out(i), s2_deq(i), s2_deq(i).ready || s2_deq(i).bits.uop.robIdx.needFlush(io.redirect), false.B)
    if (params.hasFeedback) {
      io.feedback.get(i).rsIdx := s2_issuePtr(i)
      io.feedback.get(i).isFirstIssue := s2_first_issue(i)
    }
    if (params.hasMidState) {
      io.fmaMid.get(i).waitForAdd := !s2_all_src_ready(i)
      io.fmaMid.get(i).in.valid := !s2_first_issue(i)
      XSPerfAccumulate(s"fma_partial2_issue_$i", io.deq(i).fire && io.fmaMid.get(i).waitForAdd)
      XSPerfAccumulate(s"fma_final_issue_$i", io.deq(i).fire && io.fmaMid.get(i).in.valid)
    }
    s2_deq(i).ready := !s2_deq(i).valid || io.deq(i).ready
    io.deq(i).valid := s2_deq(i).valid
    io.deq(i).bits := s2_deq(i).bits

    // data: send to bypass network
    // TODO: these should be done outside RS
    if (params.numFastWakeup > 0) {
      val isNormalIssue = s1_issue_oldest(i) || s1_in_selectPtrValid(i)
      val normalIssuePtrOH = Mux(s1_issue_oldest(i), s1_in_oldestPtrOH.bits, s1_in_selectPtrOH(i))
      val normalFastWakeupMatch = Mux1H(normalIssuePtrOH, fastWakeupMatch)
      val wakeupBypassMask = Wire(Vec(params.numFastWakeup, Vec(params.numSrc, Bool())))
      for (j <- 0 until params.numFastWakeup) {
        for (k <- 0 until params.numSrc) {
          wakeupBypassMask(j)(k) := Mux(isNormalIssue, normalFastWakeupMatch(k)(j), s1_fastWakeup(i)(k)(j))
        }
      }

      val bypassNetwork = BypassNetwork(params.numSrc, params.numFastWakeup, params.dataBits, params.optBuf)
      bypassNetwork.io.hold := !s2_deq(i).ready
      bypassNetwork.io.source := s1_out(i).bits.src.take(params.numSrc)
      bypassNetwork.io.bypass.zip(wakeupBypassMask.zip(io.fastDatas)).foreach { case (by, (m, d)) =>
        by.valid := m
        by.data := d
      }
      bypassNetwork.io.target <> s2_deq(i).bits.src.take(params.numSrc)

      // For load instructions, if its source operand is bypassed from load,
      // we reduce its latency for one cycle since it does not need to read
      // from data array. Timing to be optimized later.
      if (params.isLoad) {
        if (EnableLoadToLoadForward) {
          val ldFastDeq = Wire(io.deq(i).cloneType)
          // Condition: wakeup by load (to select load wakeup bits)
          val ldCanBeFast = VecInit(
            wakeupBypassMask.drop(exuParameters.AluCnt).take(exuParameters.LduCnt).map(_.asUInt.orR)
          ).asUInt
          ldFastDeq.valid := s1_issuePtrOH(i).valid && ldCanBeFast.orR
          ldFastDeq.ready := true.B
          ldFastDeq.bits.src := DontCare
          ldFastDeq.bits.uop := s1_out(i).bits.uop
          // when last cycle load has fast issue, cancel this cycle's normal issue and let it go
          val lastCycleLdFire = RegNext(ldFastDeq.valid && !s2_deq(i).valid && io.deq(i).ready)
          when (lastCycleLdFire) {
            s2_deq(i).valid := false.B
            s2_deq(i).ready := true.B
          }
          // For now, we assume deq.valid has higher priority than ldFastDeq.
          when (!s2_deq(i).valid) {
            io.deq(i).valid := ldFastDeq.valid
            io.deq(i).bits := ldFastDeq.bits
            s2_deq(i).ready := true.B
          }
          io.load.get.fastMatch(i) := Mux(s2_deq(i).valid, 0.U, ldCanBeFast)
          when (!s2_deq(i).valid) {
            io.feedback.get(i).rsIdx := s1_issuePtr(i)
            io.feedback.get(i).isFirstIssue := s1_is_first_issue(i)
          }
          XSPerfAccumulate(s"fast_load_deq_valid_$i", !s2_deq(i).valid && ldFastDeq.valid)
          XSPerfAccumulate(s"fast_load_deq_fire_$i", !s2_deq(i).valid && ldFastDeq.valid && io.deq(i).ready)
        } else {
          io.load.get.fastMatch(i) := DontCare
        }
      }

      io.deq(i).bits.uop.debugInfo.issueTime := GTimer()

      for (j <- 0 until params.numFastWakeup) {
        XSPerfAccumulate(s"source_bypass_${j}_$i", s1_out(i).fire && wakeupBypassMask(j).asUInt.orR)
      }
    }
  }

  if (params.hasMidState) {
    // For FMA instrutions whose third operand is not ready, once they are successfully issued (T0),
    // the FMUL intermediate result will be ready in two clock cycles (T2).
    // If the third operand is ready at T2, this instruction will be selected in T3 and issued at T4.
    // Note that at cycle T4, FMUL finishes as well and it is able to proceed to FADD.
    // Thus, we can set the midState to true two cycles earlier at T0 and forward the result if possible.
    val midFinished2 = io.fmaMid.get.zip(io.deq).map(x => x._1.waitForAdd && x._2.fire)
    val updateMid = ParallelMux(midFinished2, s2_issuePtrOH)
    statusArray.io.updateMidState := updateMid

    // FMUL intermediate results are ready in two cycles
    for (i <- 0 until params.numDeq) {
      dataArray.io.partialWrite(i).enable := RegNext(RegNext(midFinished2(i)))
      dataArray.io.partialWrite(i).mask := DontCare
      dataArray.io.partialWrite(i).addr := RegNext(RegNext(s2_issuePtrOH(i)))
      val writeData = io.fmaMid.get(i).out.bits.asUInt
      require(writeData.getWidth <= 2 * params.dataBits, s"why ${writeData.getWidth}???")
      require(writeData.getWidth > params.dataBits, s"why ${writeData.getWidth}???")
      dataArray.io.partialWrite(i).data(0) := writeData(params.dataBits - 1, 0)
      dataArray.io.partialWrite(i).data(1) := writeData(writeData.getWidth - 1, params.dataBits)
      val readData = Cat(io.deq(i).bits.src(1), io.deq(i).bits.src(0))
      io.fmaMid.get(i).in.bits := readData.asTypeOf(io.fmaMid.get(i).in.bits.cloneType)
    }

    // How to forward intermediate results:
    // (1) T0 issued FMA is selected at T1 and issued at T2: forward from FMUL results
    //     NOTE: In this case, this instruction has been issued and the entry is freed.
    //           Do NOT write data back to data array.
    // (2) T0 issued FMA is selected at T2: RegNext FMUL result at the issue stage
    // Thus, at issue stage:
    // (1.1) If the instruction matches FMA/FMUL two cycles ealier, we issue it and it goes to FADD
    // (1.2) If the instruction matches FMA/FMUL two cycles ealier and it's blocked, we need to hold the result
    // At select stage: (2) bypass FMUL intermediate results from write ports if possible.
    val issuedAtT0 = midFinished2.zip(s2_issuePtr).map(x => (RegNext(RegNext(x._1)), RegNext(RegNext(x._2))))
    for (i <- 0 until params.numDeq) {
      // cond11: condition (1.1) from different issue ports
      val cond11 = issuedAtT0.map(x => x._1 && x._2 === s2_issuePtr(i))
      for ((c, j) <- cond11.zipWithIndex) {
        when (c) {
          io.fmaMid.get(i).in.bits := io.fmaMid.get(j).out.bits
          // We should NOT write the intermediate result back to DataArray,
          // when this entry has been selected and arrived at the issue stage.
          // This entry may be allocated for new instructions from dispatch.
          when (io.deq(i).valid) {
            dataArray.io.partialWrite(j).enable := false.B
          }
        }
      }
      val cond11Issued = io.deq(i).fire && io.fmaMid.get(i).in.valid && VecInit(cond11).asUInt.orR
      XSPerfAccumulate(s"fma_final_issue_cond11_$i", cond11Issued)
      // cond12: blocked at the issue stage
      val cond12 = cond11.map(_ && io.deq(i).valid && !io.deq(i).ready)
      val hasCond12 = VecInit(cond12).asUInt.orR
      val hasCond12Reg = RegInit(false.B)
      when (hasCond12) {
        hasCond12Reg := true.B
      }.elsewhen (io.deq(i).ready) {
        hasCond12Reg := false.B
      }
      when (hasCond12Reg) {
        // TODO: remove these unnecessary registers (use pipeline registers instead)
        io.fmaMid.get(i).in.bits := RegEnable(Mux1H(cond12, io.fmaMid.get.map(_.out.bits)), hasCond12)
      }
      val cond12Issued = io.deq(i).fire && io.fmaMid.get(i).in.valid && hasCond12Reg
      XSPerfAccumulate(s"fma_final_issue_cond12_$i", cond12Issued)
      // cond2: selected at the select stage
      val cond2 = issuedAtT0.map(x => x._1 && x._2 === s1_issuePtr(i))
      for ((c, j) <- cond2.zipWithIndex) {
        when (c) {
          s1_out(i).bits.src(0) := dataArray.io.partialWrite(j).data(0)
          s1_out(i).bits.src(1) := dataArray.io.partialWrite(j).data(1)
        }
      }
      val cond2Selected = s1_out_fire(i) && VecInit(cond2).asUInt.orR
      XSPerfAccumulate(s"fma_final_selected_cond2_$i", cond2Selected)
    }
  }

  if (params.isJump) {
    val pcMem = Reg(Vec(params.numEntries, UInt(VAddrBits.W)))
    for (i <- 0 until params.numEntries) {
      val writeEn = VecInit(dataArray.io.write.map(w => w.enable && w.addr(i))).asUInt.orR
      when (writeEn) {
        pcMem(i) := io.jump.get.jumpPc
      }
    }
    for (i <- 0 until params.numDeq) {
      // currently we assert there's only one enqueue.
      require(params.numDeq == 1, "only one jump now")
      val pcRead = Mux(s1_select_bypass_s0.asUInt.orR, io.jump.get.jumpPc, Mux1H(s1_issuePtrOH(i).bits, pcMem))
      io.deq(i).bits.uop.cf.pc := RegEnable(pcRead, s1_out_fire(i))
    }
  }

  // logs
  for ((dispatch, i) <- io.fromDispatch.zipWithIndex) {
    XSDebug(dispatch.valid && !dispatch.ready, p"enq blocked, robIdx ${dispatch.bits.robIdx}\n")
    XSDebug(dispatch.fire, p"enq fire, robIdx ${dispatch.bits.robIdx}, srcState ${Binary(dispatch.bits.srcState.asUInt)}\n")
    XSPerfAccumulate(s"allcoate_fire_$i", dispatch.fire)
    XSPerfAccumulate(s"allocate_valid_$i", dispatch.valid)
    XSPerfAccumulate(s"srcState_ready_$i", PopCount(dispatch.bits.srcState.map(_ === SrcState.rdy)))
    if (params.checkWaitBit) {
      XSPerfAccumulate(s"load_wait_$i", dispatch.fire && dispatch.bits.cf.loadWaitBit)
    }
  }

  for ((deq, i) <- io.deq.zipWithIndex) {
    XSDebug(deq.fire, p"deq fire, robIdx ${deq.bits.uop.robIdx}\n")
    XSDebug(deq.valid && !deq.ready, p"deq blocked, robIdx ${deq.bits.uop.robIdx}\n")
    XSPerfAccumulate(s"deq_fire_$i", deq.fire)
    XSPerfAccumulate(s"deq_valid_$i", deq.valid)
    if (params.hasFeedback) {
      XSPerfAccumulate(s"deq_not_first_issue_$i", deq.fire && !io.feedback.get(i).isFirstIssue)
    }
  }

  for (i <- 0 until params.numEntries) {
    val isSelected = VecInit(s1_issuePtrOH.map(s => s.valid && s.bits(i))).asUInt.orR
    XSPerfAccumulate(s"select_$i", isSelected)
    val isIssued = VecInit(s1_issuePtrOH.zip(s1_out_fire).map(s => s._2 && s._1.bits(i))).asUInt.orR
    XSPerfAccumulate(s"issue_$i", isIssued)
    for (j <- 0 until params.numSrc) {
      XSPerfAccumulate(s"num_wakeup_${i}_$j", slowWakeupMatchVec(i)(j).asUInt.orR)
    }
  }

  XSPerfAccumulate("redirect_num", io.redirect.valid)
  XSPerfHistogram("allocate_num", PopCount(io.fromDispatch.map(_.valid)), true.B, 0, params.numEnq, 1)
  XSPerfHistogram("issue_num", PopCount(io.deq.map(_.valid)), true.B, 0, params.numDeq, 1)

  def size: Int = params.numEntries

  val perfEvents = Seq(("full", statusArray.io.isValid.andR))
  generatePerfEvent()
}
