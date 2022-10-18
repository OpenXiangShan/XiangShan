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
import chisel3.ExcitingUtils

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
  def numSelect: Int = numDeq + (if (oldestFirst._1) 1 else 0)

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

    if (params.isJump)      rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"jumpRS_${index}") }
    if (params.isAlu)       rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"aluRS_${index}") }
    if (params.isStore)     rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"staRS_${index}") }
    if (params.isStoreData) rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"stdRS_${index}") }
    if (params.isMul)       rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"mulRS_${index}") }
    if (params.isLoad)      rs.zipWithIndex.foreach { case (rs, index) => rs.suggestName(s"loadRS_${index}") }

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
    io.full <> rs.map(_.io.full).reduce(_ && _)
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
    val stIssuePtr = Input(new SqPtr())
    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput)))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }) else None
  val load = if (params.isLoad) Some(new Bundle() {
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

  io.numExist := PopCount(statusArray.io.isValid)

  val perfEvents = Seq(("full", statusArray.io.isValid.andR))
  generatePerfEvent()
  XSPerfAccumulate("full", statusArray.io.isValid.andR)

  io.full := statusArray.io.isValid.andR

  statusArray.io.redirect := io.redirect

  /**
    * S0: Update status (from dispatch and wakeup) and schedule possible instructions to issue.
    */
  // enqueue from dispatch
  select.io.validVec := statusArray.io.isValid
  // agreement with dispatch: don't enqueue when io.redirect.valid
  val doEnqueue = VecInit(io.fromDispatch.map(_.fire && !io.redirect.valid))
  val enqShouldNotFlushed = io.fromDispatch.map(d => d.fire && !d.bits.robIdx.needFlush(io.redirect))
  XSPerfAccumulate("wrong_stall", Mux(io.redirect.valid, PopCount(enqShouldNotFlushed), 0.U))
  val needFpSource = io.fromDispatch.map(_.bits.needRfRPort(0, true, false))
  for (i <- 0 until params.numEnq) {
    io.fromDispatch(i).ready := select.io.allocate(i).valid
    // for better timing, we update statusArray no matter there's a flush or not
    statusArray.io.update(i).enable := io.fromDispatch(i).fire()
    statusArray.io.update(i).addr := select.io.allocate(i).bits
    statusArray.io.update(i).data.valid := true.B
    statusArray.io.update(i).data.scheduled := params.delayedRf.B && needFpSource(i)
    statusArray.io.update(i).data.blocked := params.checkWaitBit.B && io.fromDispatch(i).bits.cf.loadWaitBit
    statusArray.io.update(i).data.credit := Mux(params.delayedRf.B && needFpSource(i), 3.U, 0.U)
    statusArray.io.update(i).data.srcState := VecInit(io.fromDispatch(i).bits.srcIsReady.take(params.numSrc))
    statusArray.io.update(i).data.midState := false.B
    statusArray.io.update(i).data.psrc := VecInit(io.fromDispatch(i).bits.psrc.take(params.numSrc))
    statusArray.io.update(i).data.srcType := VecInit(io.fromDispatch(i).bits.ctrl.srcType.take(params.numSrc))
    statusArray.io.update(i).data.robIdx := io.fromDispatch(i).bits.robIdx
    statusArray.io.update(i).data.sqIdx := io.fromDispatch(i).bits.sqIdx
    statusArray.io.update(i).data.waitForSqIdx := DontCare // generated by sq, will be updated later
    statusArray.io.update(i).data.waitForRobIdx := io.fromDispatch(i).bits.cf.waitForRobIdx // generated by mdp
    statusArray.io.update(i).data.waitForStoreData := false.B
    statusArray.io.update(i).data.strictWait := io.fromDispatch(i).bits.cf.loadWaitStrict
    statusArray.io.update(i).data.isFirstIssue := true.B
    // for better power, we don't write payload array when there's a redirect
    payloadArray.io.write(i).enable := doEnqueue(i)
    payloadArray.io.write(i).addr := select.io.allocate(i).bits
    payloadArray.io.write(i).data := io.fromDispatch(i).bits
    payloadArray.io.write(i).data.debugInfo.enqRsTime := GTimer()
  }

  // when config.checkWaitBit is set, we need to block issue until the corresponding store issues
  if (params.checkWaitBit) {
    statusArray.io.stIssuePtr := io.checkwait.get.stIssuePtr
    statusArray.io.memWaitUpdateReq := io.checkwait.get.memWaitUpdateReq
  }
  // wakeup from other RS or function units
  val wakeupValid = io.fastUopsIn.map(_.valid) ++ io.slowPorts.map(_.valid)
  val wakeupDest = io.fastUopsIn.map(_.bits) ++ io.slowPorts.map(_.bits.uop)
  for (i <- 0 until params.numFastWakeup + params.numWakeup) {
    statusArray.io.wakeup(i).valid := wakeupValid(i)
    statusArray.io.wakeup(i).bits := wakeupDest(i)
  }

  // select the issue instructions
  // Option 1: normal selection (do not care about the age)
  select.io.request := statusArray.io.canIssue
  // Option 2: select the oldest
  val enqVec = VecInit(doEnqueue.zip(select.io.allocate.map(_.bits)).map{ case (d, b) => Mux(d, b, 0.U) })
  val oldestSel = AgeDetector(params.numEntries, enqVec, statusArray.io.flushed, statusArray.io.canIssue)

  // send address to read uop and data
  // For better timing, we read the payload array before we determine which instruction to issue.
  // In this way, selection and payload read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    payloadArray.io.read(i).addr := select.io.grant(i).bits
  }
  payloadArray.io.read(params.numDeq).addr := oldestSel.bits

  // For better timing, we add one more read port to data array when oldestFirst is enabled,
  // and select data after the arbiter decides which one to issue.
  // In this way, selection and data read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    dataArray.io.read(i).addr := select.io.grant(i).bits
  }
  dataArray.io.read.last.addr := oldestSel.bits

  /**
    * S1: read uop and data
    */
  // pipeline registers for stage one
  val s1_do_enqueue = RegNext(doEnqueue)
  val s1_out = Wire(Vec(params.numDeq, Decoupled(new ExuInput)))
  for (i <- 0 until params.numDeq) {
    statusArray.io.issueGranted(i).valid := RegNext(select.io.grant(i).valid) && s1_out(i).ready
    statusArray.io.issueGranted(i).bits := RegNext(select.io.grant(i).bits)
  }
  val issueVec = Wire(Vec(params.numDeq, Valid(UInt(params.numEntries.W))))
  val oldestOverride = Wire(Vec(params.numDeq, Bool()))
  if (params.oldestFirst._1) {
    // When the reservation station has oldestFirst, we need to issue the oldest instruction if possible.
    // However, in this case, the select policy always selects at maximum numDeq instructions to issue.
    // Thus, we need an arbitration between the numDeq + 1 possibilities.
    val oldestSelection = Module(new OldestSelection(params))
    oldestSelection.io.in := RegNext(select.io.grant)
    oldestSelection.io.oldest := RegNext(oldestSel)
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
    issueVec := oldestSelection.io.out
    oldestOverride := oldestSelection.io.isOverrided
    // The oldest must be selected, though it may be the same as others.
    val oldestReady = Mux1H(oldestOverride, s1_out.map(_.ready))
    statusArray.io.issueGranted.last.valid := oldestSelection.io.oldest.valid && oldestReady
    statusArray.io.issueGranted.last.bits := oldestSelection.io.oldest.bits
    for (i <- 0 until params.numDeq) {
      when (oldestSelection.io.isOverrided(i)) {
        statusArray.io.issueGranted(i).valid := false.B
      }
    }
  }
  else {
    issueVec := RegNext(select.io.grant)
    oldestOverride.foreach(_ := false.B)
  }

  // Do the read data arbitration
  val s1_is_first_issue = Wire(Vec(params.numDeq, Bool()))
  val s1_all_src_ready = Wire(Vec(params.numDeq, Bool()))
  for ((doOverride, i) <- oldestOverride.zipWithIndex) {
    s1_out(i).bits.uop := Mux(doOverride, payloadArray.io.read.last.data, payloadArray.io.read(i).data)
    s1_is_first_issue(i) := Mux(doOverride, statusArray.io.isFirstIssue.last, statusArray.io.isFirstIssue(i))
    s1_all_src_ready(i) := Mux(doOverride, statusArray.io.allSrcReady.last, statusArray.io.allSrcReady(i))
  }
  s1_out.foreach(_.bits.uop.debugInfo.selectTime := GTimer())

  for (i <- 0 until params.numDeq) {
    s1_out(i).valid := issueVec(i).valid && !s1_out(i).bits.uop.robIdx.needFlush(io.redirect)
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
      statusArray.io.deqResp(i).valid := issueVec(i).valid && s1_out(i).ready && allSrcReady
      statusArray.io.deqResp(i).bits.rsMask := issueVec(i).bits
      statusArray.io.deqResp(i).bits.success := s2_deq(i).ready
      statusArray.io.deqResp(i).bits.resptype := DontCare
      statusArray.io.deqResp(i).bits.dataInvalidSqIdx := DontCare
    }

    if (io.fastWakeup.isDefined) {
      val wakeupQueue = Module(new WakeupQueue(params.fixedLatency))
      val fuCheck = if (params.isMul) s1_out(i).bits.uop.ctrl.fuType === FuType.mul else true.B
      // TODO: optimize timing here since ready may be slow
      wakeupQueue.io.in.valid := issueVec(i).valid && s1_out(i).ready && fuCheck
      wakeupQueue.io.in.bits := s1_out(i).bits.uop
      wakeupQueue.io.in.bits.debugInfo.issueTime := GTimer() + 1.U
      wakeupQueue.io.redirect := io.redirect
      io.fastWakeup.get(i) := wakeupQueue.io.out
      XSPerfAccumulate(s"fast_blocked_$i", issueVec(i).valid && fuCheck && !s1_out(i).ready)
    }
  }
  statusArray.io.updateMidState := 0.U

  // select whether the source is from (whether regfile or imm)
  // for read-after-issue, it's done over the selected uop
  // for read-before-issue, it's done over the enqueue uop (and store the imm in dataArray to save space)
  // lastAllocateUop: Vec(config.numEnq, new MicroOp)
  val lastAllocateUop = RegNext(VecInit(io.fromDispatch.map(_.bits)))
  val immBypassedData = Wire(Vec(params.numEnq, Vec(params.numSrc, UInt(params.dataBits.W))))
  for (((uop, data), bypass) <- lastAllocateUop.zip(io.srcRegValue).zip(immBypassedData)) {
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
    dataArray.io.write(i).enable := s1_do_enqueue(i)
    dataArray.io.write(i).mask := RegNext(statusArray.io.update(i).data.srcState)
    dataArray.io.write(i).addr := RegNext(select.io.allocate(i).bits)
    dataArray.io.write(i).data := immBypassedData(i)
    if (params.delayedRf) {
      dataArray.io.delayedWrite(i).valid := RegNext(s1_do_enqueue(i) && RegNext(needFpSource(i)))
      dataArray.io.delayedWrite(i).bits := io.fpRegValue.get(i)
    }
  }
  // data broadcast: from function units (only slow wakeup date are needed)
  val broadcastValid = io.slowPorts.map(_.valid)
  val broadcastValue = VecInit(io.slowPorts.map(_.bits.data))
  require(broadcastValid.size == params.numWakeup)
  require(broadcastValue.size == params.numWakeup)
  val slowWakeupMatchVec = Wire(Vec(params.numEntries, Vec(params.numSrc, Vec(params.numWakeup, Bool()))))
  for (i <- 0 until params.numEntries) {
    for (j <- 0 until params.numSrc) {
      slowWakeupMatchVec(i)(j) := statusArray.io.wakeupMatch(i)(j).asBools.drop(params.numFastWakeup)
    }
  }
  dataArray.io.multiWrite.zipWithIndex.foreach { case (w, i) =>
    w.enable := broadcastValid(i)
    for (j <- 0 until params.numSrc) {
      w.addr(j) := VecInit(slowWakeupMatchVec.map(_(j)(i))).asUInt
    }
    w.data := broadcastValue(i)
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
      bypassData.zip(io.deqData(i)).foreach{ case (byData, deq) =>
        when (byData.valid && io.enqBypass(i).asUInt.orR) {
          deq := byData.bits
        }
      }
    }
  }

  // for read-before-issue, we need to bypass the enqueue data here
  // for read-after-issue, we need to bypass the imm here
  s1_out.foreach(_.bits.src := DontCare)
  // check enq data bypass (another form of broadcast except that we know where it hits) here
  val s1_allocate_index = select.io.allocate.map(a => RegNext(OHToUInt(a.bits)))
  val s1_issue_index = issueVec.map(iss => OHToUInt(iss.bits))
  val s1_select_bypass_s0 = Wire(Vec(params.numDeq, Vec(params.numEnq, Bool())))
  for ((bypass, i) <- s1_select_bypass_s0.zipWithIndex) {
    // bypass: Vec(config.numEnq, Bool())
    bypass := s1_do_enqueue.zip(s1_allocate_index).map{ case (enq, idx) => enq && idx === s1_issue_index(i) }
  }

  val dataSelect = Module(new DataSelect)
  dataSelect.io.doOverride := oldestOverride
  dataSelect.io.readData := dataArray.io.read.map(_.data)
  dataSelect.io.enqBypass := s1_select_bypass_s0
  for ((enq, i) <- dataSelect.io.enqData.zipWithIndex) {
    for (j <- 0 until params.numSrc) {
      enq(j).valid := RegNext(statusArray.io.update(i).data.srcState(j))
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
  val fastWakeupMatchVec = Wire(Vec(params.numEntries, Vec(params.numSrc, Vec(params.numFastWakeup, Bool()))))
  for (i <- 0 until params.numEntries) {
    for (j <- 0 until params.numSrc) {
      fastWakeupMatchVec(i)(j) := statusArray.io.wakeupMatch(i)(j).asBools.take(params.numFastWakeup)
    }
  }
  val fastWakeupMatchRegVec = RegNext(fastWakeupMatchVec)

  /**
    * S2: to function units
    */
  val s1_out_fire = s1_out.zip(s2_deq).map(x => x._1.valid && x._2.ready)
  val s2_issue_index = s1_issue_index.zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  val s2_first_issue = s1_is_first_issue.zip(s1_out_fire).map(x => RegEnable(x._1, x._2))
  for (i <- 0 until params.numDeq) {
    // payload: send to function units
    // TODO: these should be done outside RS
    PipelineConnect(s1_out(i), s2_deq(i), s2_deq(i).ready || s2_deq(i).bits.uop.robIdx.needFlush(io.redirect), false.B)
    if (params.hasFeedback) {
      io.feedback.get(i).rsIdx := s2_issue_index(i)
      io.feedback.get(i).isFirstIssue := s2_first_issue(i)
    }
    if (params.hasMidState) {
      io.fmaMid.get(i).waitForAdd := !RegEnable(s1_all_src_ready(i), s1_out_fire(i))
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
      val targetFastWakeupMatch = Mux1H(issueVec(i).bits, fastWakeupMatchRegVec)
      val wakeupBypassMask = Wire(Vec(params.numFastWakeup, Vec(params.numSrc, Bool())))
      for (j <- 0 until params.numFastWakeup) {
        wakeupBypassMask(j) := VecInit(targetFastWakeupMatch.map(_(j)))
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
          ldFastDeq.valid := issueVec(i).valid && ldCanBeFast.orR
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
            io.feedback.get(i).rsIdx := s1_issue_index(i)
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
        XSPerfAccumulate(s"source_bypass_${j}_$i", s1_out(i).fire() && wakeupBypassMask(j).asUInt().orR())
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
    val issuedRsIdxOH = issueVec.zip(s1_out_fire).map(x => RegEnable(x._1.bits, x._2))
    val updateMid = midFinished2.zip(issuedRsIdxOH).map(x => Mux(x._1, x._2, 0.U)).reduce(_ | _)
    statusArray.io.updateMidState := updateMid

    // FMUL intermediate results are ready in two cycles
    for (i <- 0 until params.numDeq) {
      dataArray.io.partialWrite(i).enable := RegNext(RegNext(midFinished2(i)))
      dataArray.io.partialWrite(i).mask := DontCare
      dataArray.io.partialWrite(i).addr := RegNext(RegNext(issuedRsIdxOH(i)))
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
    val selectedRsIdx = issueVec.map(iss => OHToUInt(iss.bits))
    val issuedRsIdx = s1_out_fire.zip(selectedRsIdx).map(x => RegEnable(x._2, x._1))
    val issuedAtT0 = midFinished2.zip(issuedRsIdx).map(x => (RegNext(RegNext(x._1)), RegNext(RegNext(x._2))))
    for (i <- 0 until params.numDeq) {
      // cond11: condition (1.1) from different issue ports
      val cond11 = issuedAtT0.map(x => x._1 && x._2 === issuedRsIdx(i))
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
      val cond2 = issuedAtT0.map(x => x._1 && x._2 === selectedRsIdx(i))
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
    for (i <- 0 until params.numEnq) {
      when (dataArray.io.write(i).enable) {
        pcMem(s1_allocate_index(i)) := io.jump.get.jumpPc
      }
    }
    for (i <- 0 until params.numDeq) {
      // currently we assert there's only one enqueue.
      require(params.numDeq == 1, "only one jump now")
      val pcRead = Mux(s1_select_bypass_s0.asUInt.orR, io.jump.get.jumpPc, pcMem(s1_issue_index(i)))
      io.deq(i).bits.uop.cf.pc := RegEnable(pcRead, s1_out_fire(i))
    }
  }

  // logs
  for ((dispatch, i) <- io.fromDispatch.zipWithIndex) {
    XSDebug(dispatch.valid && !dispatch.ready, p"enq blocked, robIdx ${dispatch.bits.robIdx}\n")
    XSDebug(dispatch.fire(), p"enq fire, robIdx ${dispatch.bits.robIdx}, srcState ${Binary(dispatch.bits.srcState.asUInt)}\n")
    XSPerfAccumulate(s"allcoate_fire_$i", dispatch.fire())
    XSPerfAccumulate(s"allocate_valid_$i", dispatch.valid)
    XSPerfAccumulate(s"srcState_ready_$i", PopCount(dispatch.bits.srcState.map(_ === SrcState.rdy)))
    if (params.checkWaitBit) {
      XSPerfAccumulate(s"load_wait_$i", dispatch.fire() && dispatch.bits.cf.loadWaitBit)
    }
  }

  for ((deq, i) <- io.deq.zipWithIndex) {
    XSDebug(deq.fire(), p"deq fire, robIdx ${deq.bits.uop.robIdx}\n")
    XSDebug(deq.valid && !deq.ready, p"deq blocked, robIdx ${deq.bits.uop.robIdx}\n")
    XSPerfAccumulate(s"deq_fire_$i", deq.fire())
    XSPerfAccumulate(s"deq_valid_$i", deq.valid)
    if (params.hasFeedback) {
      XSPerfAccumulate(s"deq_not_first_issue_$i", deq.fire() && !io.feedback.get(i).isFirstIssue)
    }
  }

  for (i <- 0 until params.numEntries) {
    val isSelected = VecInit(issueVec.map(s => s.valid && s.bits(i))).asUInt().orR()
    XSPerfAccumulate(s"select_$i", isSelected)
    val isIssued = VecInit(issueVec.zip(s1_out_fire).map(s => s._2 && s._1.bits(i))).asUInt().orR()
    XSPerfAccumulate(s"issue_$i", isIssued)
    for (j <- 0 until params.numSrc) {
      XSPerfAccumulate(s"num_wakeup_${i}_$j", slowWakeupMatchVec(i)(j).asUInt().orR())
    }
  }

  if (env.EnableTopDown && params.isLoad) {
    val l1d_loads_bound = WireDefault(0.B)
    ExcitingUtils.addSink(l1d_loads_bound, "l1d_loads_bound", ExcitingUtils.Perf)
    val mshrFull = statusArray.io.rsFeedback(RSFeedbackType.mshrFull.litValue.toInt)
    val tlbMiss = !mshrFull && statusArray.io.rsFeedback(RSFeedbackType.tlbMiss.litValue.toInt)
    val dataInvalid = !mshrFull && !tlbMiss && statusArray.io.rsFeedback(RSFeedbackType.dataInvalid.litValue.toInt)
    val bankConflict = !mshrFull && !tlbMiss && !dataInvalid && statusArray.io.rsFeedback(RSFeedbackType.bankConflict.litValue.toInt)
    val ldVioCheckRedo = !mshrFull && !tlbMiss && !dataInvalid && !bankConflict && statusArray.io.rsFeedback(RSFeedbackType.ldVioCheckRedo.litValue.toInt)
    XSPerfAccumulate("l1d_loads_mshr_bound", l1d_loads_bound && mshrFull)
    XSPerfAccumulate("l1d_loads_tlb_bound", l1d_loads_bound && tlbMiss)
    XSPerfAccumulate("l1d_loads_store_data_bound", l1d_loads_bound && dataInvalid)
    XSPerfAccumulate("l1d_loads_bank_conflict_bound", l1d_loads_bound && bankConflict)
    XSPerfAccumulate("l1d_loads_vio_check_redo_bound", l1d_loads_bound && ldVioCheckRedo)
  }

  XSPerfAccumulate("redirect_num", io.redirect.valid)
  XSPerfHistogram("allocate_num", PopCount(io.fromDispatch.map(_.valid)), true.B, 0, params.numEnq, 1)
  XSPerfHistogram("issue_num", PopCount(io.deq.map(_.valid)), true.B, 0, params.numDeq, 1)

  def size: Int = params.numEntries
}
