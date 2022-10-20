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
import utils._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FMAMidResultIO
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}

import scala.math.max
import com.fasterxml.jackson.databind.JsonSerializable.Base

case class RSMod
(
  var rsWrapperGen: (RSMod, Parameters) => BaseReservationStationWrapper,// =
    // (p: Parameters) => new BaseReservationStationWrapper()(p),
  var rsGen: (RSParams, Parameters) => BaseReservationStation,// =
    // (params, p) => new BaseReservationStation(params)(p),
  var immExtractorGen: (Int, Int, Parameters) => ImmExtractor =
    (src: Int, width: Int, p: Parameters) => new ImmExtractor(src,width)(p),
)

case class RSParams
(
  var subMod: RSMod,
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
  //
  var needScheduledBit: Boolean = false,
  // special cases
  var isJump: Boolean = false,
  var isMul: Boolean = false,
  var isLoad: Boolean = false,
  var isStd: Boolean = false,
  var isMemAddr: Boolean = false,
  var isFMA: Boolean = false,
  var exuCfg: Option[ExuConfig] = None
){
  def allWakeup: Int = numFastWakeup + numWakeup
  def indexWidth: Int = log2Up(numEntries)
  // oldestFirst: (Enable_or_not, Need_balance, Victim_index)
  def oldestFirst: (Boolean, Boolean, Int) = (true, false, 0)
  def hasMidState: Boolean = exuCfg.get == FmacExeUnitCfg
  def delayedSrc: Boolean = exuCfg.get == StdExeUnitCfg
  def needBalance: Boolean = exuCfg.get.needLoadBalance && exuCfg.get != LdExeUnitCfg
  def numSelect: Int = numDeq + numEnq + (if (oldestFirst._1) 1 else 0)
  def optDeqFirstStage: Boolean = !exuCfg.get.readFpRf

  override def toString: String = {
    s"type ${exuCfg.get.name}, size $numEntries, enq $numEnq, deq $numDeq, numSrc $numSrc, fast $numFastWakeup, wakeup $numWakeup"
  }
}

abstract class RSModule(implicit p: Parameters) extends XSModule with HasPerfEvents

class BaseReservationStationWrapper(modGen: RSMod)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val params = new RSParams(subMod = modGen)

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
      case MulDivExeUnitCfg => params.isMul = true
      case LdExeUnitCfg => params.isLoad = true
      case StdExeUnitCfg => params.isStd = true
      case FmacExeUnitCfg => params.isFMA = true
      case _ =>
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

  // for better timing, we limits the size of RS to 2-deq
  // duplicate with ModuleImp, fix it later
  val maxRsDeq = 2
  def numRS = (params.numDeq + (maxRsDeq - 1)) / maxRsDeq

  lazy val module = new BaseReservationStationImp(params, this)
}

class BaseReservationStationImp(params: RSParams, wrapper: BaseReservationStationWrapper) extends LazyModuleImp(wrapper) with HasPerfEvents {
  // for better timing, we limits the size of RS to 2-deq
  val maxRsDeq = 2
  def numRS = (params.numDeq + (maxRsDeq - 1)) / maxRsDeq
  def isJump = params.isJump
  def isLoad = params.isLoad
  def isStd  = params.isStd
  def checkWaitBit = params.checkWaitBit
  def hasFeedback = params.hasFeedback

  require(params.numEnq < params.numDeq || params.numEnq % params.numDeq == 0)
  require(params.numEntries % params.numDeq == 0)
  val rsParams = (0 until numRS).map(i => {
    val numDeq = Seq(params.numDeq - maxRsDeq * i, maxRsDeq).min
    val numEnq = params.numEnq / numRS
    val numEntries = numDeq * params.numEntries / params.numDeq
    val rsParam = params.copy(numEnq = numEnq, numDeq = numDeq, numEntries = numEntries)
    val updatedP = p.alter((site, here, up) => {
      case XSCoreParamsKey => up(XSCoreParamsKey).copy(
        IssQueSize = numEntries
      )
    })
    (rsParam, updatedP)
  })
  val rs = rsParams.map(rsP => Module(params.subMod.rsGen(rsP._1, rsP._2)))
  rs.foreach(_.extra <> DontCare)

  val updatedP = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = rsParams.map(rsP => rsP._1.numEntries).max
    )
  })
  val io = IO(new ReservationStationIO(params)(updatedP))
  val extra = IO(new RSExtraIO(params)(updatedP))
  extra <> DontCare

  rs.foreach(_.io.redirect := RegNextWithEnable(io.redirect))
  io.fromDispatch <> rs.flatMap(_.io.fromDispatch)
  io.srcRegValue <> rs.flatMap(_.io.srcRegValue)
  io.deq <> rs.flatMap(_.io.deq)
  rs.foreach(_.io.fastUopsIn <> io.fastUopsIn)
  rs.foreach(_.io.fastDatas <> io.fastDatas)
  rs.foreach(_.io.slowPorts <> io.slowPorts)
  if (io.fastWakeup.isDefined) {
    io.fastWakeup.get <> rs.flatMap(_.io.fastWakeup.get)
  }

  val perfEvents = rs.flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class ReservationStationIO(params: RSParams)(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
  // enq
  val fromDispatch = Vec(params.numEnq, Flipped(DecoupledIO(new MicroOp)))
  val srcRegValue = Vec(params.numEnq, Input(Vec(params.numSrc, UInt(params.dataBits.W))))
  // deq
  val deq = Vec(params.numDeq, DecoupledIO(new ExuInput))
  // wakeup
  val fastUopsIn = Vec(params.numFastWakeup, Flipped(ValidIO(new MicroOp)))
  val fastDatas = Vec(params.numFastWakeup, Input(UInt(params.dataBits.W)))
  val slowPorts = Vec(params.numWakeup, Flipped(ValidIO(new ExuOutput)))
  // extra
  val fastWakeup = if (params.fixedLatency >= 0) Some(Vec(params.numDeq, ValidIO(new MicroOp))) else None
}

class RSExtraIO(params: RSParams)(implicit p: Parameters) extends XSBundle {
  val jump = new Bundle {
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
  }
  val load = Vec(params.numDeq, new Bundle {
    val fastMatch = Output(UInt(exuParameters.LduCnt.W))
    val fastImm = Output(UInt(12.W))
  })
  val fpRegValue = Vec(params.numEnq, Input(UInt(params.dataBits.W)))
  val feedback = Vec(params.numDeq, Flipped(new MemRSFeedbackIO))
  val checkwait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput)))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
  val fmaMid = Vec(params.numDeq, Flipped(new FMAMidResultIO))
}

class BaseReservationStation(params: RSParams)(implicit p: Parameters) extends RSModule
  with HasCircularQueuePtrHelper
{
  val io = IO(new ReservationStationIO(params)(p))
  val extra = IO(new RSExtraIO(params))
  // DontCare here
  extra <> DontCare

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

  // FIXME: this allocation ready bits can be used with naive/circ selection policy only.
  val dispatchReady = Wire(Vec(params.numEnq, Bool()))
  if (params.numEnq == 4) {
    require(params.numEnq == 4, "4 fast ready only supported")
    for (i <- 0 until 2) {
      val bitFunc = if (i == 0) (x: UInt) => GetEvenBits(x) else (x: UInt) => GetOddBits(x)
      val numEmptyEntries = PopCount(bitFunc(statusArray.io.isValid).asBools.map(v => !v))
      val numAllocateS1 = PopCount(statusArray.io.update.map(u => u.enable && bitFunc(u.addr).orR))
      val realNumEmptyAfterS1 = numEmptyEntries - numAllocateS1
      val numEmptyAfterS1 = Wire(UInt(3.W)) // max: 4
      val highBits = (realNumEmptyAfterS1 >> 2).asUInt
      numEmptyAfterS1 := Mux(highBits.orR, 4.U, realNumEmptyAfterS1(1, 0))
      val numDeq = PopCount(statusArray.io.deqResp.map(r => r.valid && r.bits.success && bitFunc(r.bits.rsMask).orR))
      val emptyThisCycle = Reg(UInt(3.W)) // max: 6?
      emptyThisCycle := numEmptyAfterS1 + numDeq
      val numAllocateS0 = PopCount(s0_doEnqueue.zip(s0_allocatePtrOH).map(x => x._1 && bitFunc(x._2).orR))
      for (j <- 0 until 2) {
        val allocateThisCycle = Reg(UInt(2.W))
        allocateThisCycle := numAllocateS0 +& j.U
        dispatchReady(2 * j + i) := emptyThisCycle > allocateThisCycle
      }
    }
  }
  else if (params.numEnq <= 2) {
    val numEmptyEntries = PopCount(statusArray.io.isValid.asBools.map(v => !v))
    val numAllocateS1 = PopCount(statusArray.io.update.map(_.enable))
    val realNumEmptyAfterS1 = numEmptyEntries - numAllocateS1
    val numEmptyAfterS1 = Wire(UInt(3.W)) // max: 4
    val highBits = (realNumEmptyAfterS1 >> 2).asUInt
    numEmptyAfterS1 := Mux(highBits.orR, 4.U, realNumEmptyAfterS1(1, 0))
    val numDeq = PopCount(VecInit(statusArray.io.deqResp.map(resp => resp.valid && resp.bits.success)))
    val emptyThisCycle = Reg(UInt(3.W)) // max: 6?
    emptyThisCycle := numEmptyAfterS1 + numDeq // max: 3 + numDeq = 5?
    val numAllocateS0 = PopCount(s0_doEnqueue)
    for (i <- 0 until params.numEnq) {
      val allocateThisCycle = Reg(UInt(2.W))
      allocateThisCycle := numAllocateS0 +& i.U
      dispatchReady(i) := emptyThisCycle > allocateThisCycle
    }
  }
  else {
    dispatchReady := select.io.allocate.map(_.valid)
  }

  s0_enqFlushed.map(_ := io.redirect.valid)
  for (i <- 0 until params.numEnq) {
    io.fromDispatch(i).ready := dispatchReady(i)
    XSError(s0_doEnqueue(i) && !select.io.allocate(i).valid, s"port $i should not enqueue\n")
    XSError(!RegNext(io.redirect.valid) && select.io.allocate(i).valid =/= dispatchReady(i), s"port $i performance deviation\n")
    s0_doEnqueue(i) := io.fromDispatch(i).fire && !s0_enqFlushed(i)
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

  // select.io.balance
  // Option 2: select the oldest
  val enqVec = VecInit(s0_doEnqueue.zip(s0_allocatePtrOH).map{ case (d, b) => RegNext(Mux(d, b, 0.U)) })
  val s1_oldestSel = AgeDetector(params.numEntries, enqVec, statusArray.io.flushed, statusArray.io.canIssue)

  // send address to read uop and data
  // For better timing, we read the payload array before we determine which instruction to issue.
  // In this way, selection and payload read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    payloadArray.io.read(i).addr := select.io.grant(i).bits
  }
  payloadArray.io.read.last.addr := s1_oldestSel.bits

  // For better timing, we add one more read port to data array when oldestFirst is enabled,
  // and select data after the arbiter decides which one to issue.
  // In this way, selection and data read happen simultaneously.
  for (i <- 0 until params.numDeq) {
    dataArray.io.read(i).addr := select.io.grant(i).bits
  }
  dataArray.io.read.last.addr := s1_oldestSel.bits

  def enqReverse[T <: Data](in: Seq[T]): Seq[T] = {
    if (params.numDeq == 2) {
      in.take(params.numDeq).reverse ++ in.drop(params.numDeq)
    }
    else in
  }
  /**
    * S1: read uop and data
    */
  val s1_slowPorts = RegNext(io.slowPorts)
  val s1_fastUops = RegNext(io.fastUopsIn)
  val s1_dispatchUops_dup = Reg(Vec(3, Vec(params.numEnq, Valid(new MicroOp))))
  val s1_delayedSrc = Wire(Vec(params.numEnq, Vec(params.numSrc, Bool())))
  val s1_allocatePtrOH_dup = RegNext(VecInit.fill(3)(VecInit(enqReverse(s0_allocatePtrOH))))
  val s1_allocatePtr = RegNext(VecInit(enqReverse(s0_allocatePtr)))
  val s1_enqWakeup = RegNext(VecInit(enqReverse(s0_enqWakeup)))
  val s1_enqDataCapture = RegNext(VecInit(enqReverse(s0_enqDataCapture)))
  val s1_fastWakeup = RegNext(VecInit(enqReverse(s0_fastWakeup)))
  val s1_in_selectPtr = select.io.grant
  val s1_in_selectPtrValid = s1_in_selectPtr.map(_.valid)
  val s1_in_selectPtrOH = s1_in_selectPtr.map(_.bits)
  val s1_in_oldestPtrOH = s1_oldestSel
  val s1_issue_oldest = Wire(Vec(params.numDeq, Bool()))
  val s1_issue_dispatch = Wire(Vec(params.numDeq, Bool()))
  val s1_out = Wire(Vec(params.numDeq, Decoupled(new ExuInput)))
  val s1_issuePtrOH = Wire(Vec(params.numDeq, Valid(UInt(params.numEntries.W))))
  val s1_issuePtr = s1_issuePtrOH.map(iss => OHToUInt(iss.bits))

  val numSelected = PopCount(s1_issuePtrOH.map(_.valid))
  val numReadyEntries = PopCount(statusArray.io.canIssue)
  val shouldSelected = Mux(numReadyEntries > params.numDeq.U, params.numDeq.U, numReadyEntries)
  XSError(numSelected < shouldSelected,
    p"performance regression: only $numSelected out of $shouldSelected selected (total: $numReadyEntries)\n")

  // Allocation: store dispatch uops into payload and data array
  s1_dispatchUops_dup.foreach(_.zip(enqReverse(io.fromDispatch)).zipWithIndex.foreach{ case ((uop, in), i) =>
    val s0_valid = in.fire && !enqReverse(s0_enqFlushed)(i)
    uop.valid := s0_valid
    when (s0_valid) {
      uop.bits := in.bits
      uop.bits.debugInfo.enqRsTime := GTimer()
    }
  })

  // update status and payload array
  statusArray.io.redirect := io.redirect
  s1_delayedSrc.map(s => s.foreach(_ := false.B))
  for (((statusUpdate, uop), i) <- statusArray.io.update.zip(s1_dispatchUops_dup.head).zipWithIndex) {
    statusUpdate.enable := uop.valid
    statusUpdate.addr := s1_allocatePtrOH_dup.head(i)
    statusUpdate.data.valid := true.B
    statusUpdate.data.scheduled := s1_delayedSrc(i).asUInt.orR
    statusUpdate.data.blocked := false.B // for checkWaitBit
    statusUpdate.data.credit := Mux(s1_delayedSrc(i).asUInt.orR, 1.U, 0.U) // credit = 1
    for (j <- 0 until params.numSrc) {
      statusUpdate.data.srcState(j) := uop.bits.srcIsReady(j) || s1_enqWakeup(i)(j).asUInt.orR || s1_fastWakeup(i)(j).asUInt.orR
    }
    statusUpdate.data.midState := false.B
    statusUpdate.data.psrc := uop.bits.psrc.take(params.numSrc)
    statusUpdate.data.srcType := uop.bits.ctrl.srcType.take(params.numSrc)
    statusUpdate.data.robIdx := uop.bits.robIdx
    statusUpdate.data.sqIdx := uop.bits.sqIdx
    statusUpdate.data.waitForSqIdx := DontCare // generated by sq, will be updated later
    statusUpdate.data.waitForRobIdx := uop.bits.cf.waitForRobIdx // generated by mdp
    statusUpdate.data.waitForStoreData := false.B
    statusUpdate.data.strictWait := uop.bits.cf.loadWaitStrict
    statusUpdate.data.isFirstIssue := true.B
  }

  for ((payloadWrite, i) <- payloadArray.io.write.zipWithIndex) {
    payloadWrite.enable := s1_dispatchUops_dup(1)(i).valid
    payloadWrite.addr := s1_allocatePtrOH_dup(1)(i)
    payloadWrite.data := s1_dispatchUops_dup(1)(i).bits
  }

  // Issue with priorities: (1) oldest uop; (2) selected uops; (3) dispatched uops.

  for ((issueGrant, i) <- statusArray.io.issueGranted.take(params.numEnq).zipWithIndex) {
    issueGrant.valid := (if (i >= params.numDeq) false.B else s1_issue_dispatch(i) && s1_out(i).ready)
    issueGrant.bits := s1_allocatePtrOH_dup.head(i)
    XSPerfAccumulate(s"deq_dispatch_bypass_$i", issueGrant.valid)
  }
  for ((issueGrant, i) <- statusArray.io.issueGranted.drop(params.numEnq).take(params.numDeq).zipWithIndex) {
    issueGrant.valid := s1_in_selectPtrValid(i) && !s1_issue_oldest(i) && s1_out(i).ready
    issueGrant.bits := s1_in_selectPtrOH(i)
    XSPerfAccumulate(s"deq_select_$i", issueGrant.valid)
  }
  if (params.oldestFirst._1) {
    statusArray.io.issueGranted.last.valid := ParallelMux(s1_issue_oldest, s1_out.map(_.ready))
    statusArray.io.issueGranted.last.bits := s1_in_oldestPtrOH.bits
    XSPerfAccumulate(s"deq_oldest", statusArray.io.issueGranted.last.valid)
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
    s1_issue_oldest := oldestSelection.io.isOverrided
  }

  // Do the read data arbitration
  val s1_is_first_issue = Wire(Vec(params.numDeq, Bool()))
  val s1_all_src_ready = Wire(Vec(params.numDeq, Bool()))
  for (i <- 0 until params.numDeq) {
    val canBypass = s1_dispatchUops_dup.head(i).valid && statusArray.io.update(i).data.canIssue
    s1_issue_dispatch(i) := canBypass && !s1_issue_oldest(i) && !s1_in_selectPtrValid(i)

    s1_issuePtrOH(i).valid := s1_issue_oldest(i) || s1_in_selectPtrValid(i) || canBypass
    s1_issuePtrOH(i).bits := Mux(s1_issue_oldest(i), s1_in_oldestPtrOH.bits,
      Mux(s1_in_selectPtrValid(i), s1_in_selectPtrOH(i), s1_allocatePtrOH_dup.head(i)))

    s1_out(i).bits.uop := Mux(s1_issue_oldest(i), payloadArray.io.read.last.data,
      Mux(s1_in_selectPtrValid(i), payloadArray.io.read(i).data, s1_dispatchUops_dup.head(i).bits))
    s1_is_first_issue(i) := Mux(s1_issue_oldest(i), statusArray.io.isFirstIssue.last,
      Mux(s1_in_selectPtrValid(i), statusArray.io.isFirstIssue(params.numEnq + i),
        statusArray.io.update(i).data.isFirstIssue))
    s1_all_src_ready(i) := Mux(s1_issue_oldest(i), statusArray.io.allSrcReady.last,
        Mux(s1_in_selectPtrValid(i), statusArray.io.allSrcReady(params.numEnq + i),
          statusArray.io.update(i).data.allSrcReady))

    XSPerfAccumulate(s"deq_oldest_override_select_$i", s1_issue_oldest(i) && s1_in_selectPtrValid(i) && s1_out(i).ready)
  }
  s1_out.foreach(_.bits.uop.debugInfo.selectTime := GTimer())

  // WireInit for override at RSFMA
  val allSrcReady = (0 until params.numDeq).map(_ => WireInit(true.B))
  val allSrcReady1 = (0 until params.numDeq).map(_ => WireInit(true.B))
  for (i <- 0 until params.numDeq) {
    s1_out(i).valid := s1_issuePtrOH(i).valid && !s1_out(i).bits.uop.robIdx.needFlush(io.redirect)
    // For FMAs that can be scheduled multiple times, only when
    // all source operands are ready we dequeue the instruction.
    statusArray.io.deqResp(2*i).valid := s1_in_selectPtrValid(i) && !s1_issue_oldest(i) && s1_out(i).ready && allSrcReady(i)
    statusArray.io.deqResp(2*i).bits.rsMask := s1_in_selectPtrOH(i)
    statusArray.io.deqResp(2*i).bits.success := s2_deq(i).ready
    statusArray.io.deqResp(2*i).bits.resptype := DontCare
    statusArray.io.deqResp(2*i).bits.dataInvalidSqIdx := DontCare
    statusArray.io.deqResp(2*i+1).valid := s1_issue_dispatch(i) && s1_out(i).ready && allSrcReady1(i)
    statusArray.io.deqResp(2*i+1).bits.rsMask := s1_allocatePtrOH_dup.head(i)
    statusArray.io.deqResp(2*i+1).bits.success := s2_deq(i).ready
    statusArray.io.deqResp(2*i+1).bits.resptype := DontCare
    statusArray.io.deqResp(2*i+1).bits.dataInvalidSqIdx := DontCare

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
  // fma midstats is different
  val allSrcReadyLast = WireInit(true.B)
  statusArray.io.deqResp.last.valid := s1_issue_oldest.asUInt.orR && ParallelMux(s1_issue_oldest, s1_out.map(_.ready)) && allSrcReadyLast
  statusArray.io.deqResp.last.bits.rsMask := s1_in_oldestPtrOH.bits
  statusArray.io.deqResp.last.bits.success := ParallelMux(s1_issue_oldest, s2_deq.map(_.ready))
  statusArray.io.deqResp.last.bits.resptype := DontCare
  statusArray.io.deqResp.last.bits.dataInvalidSqIdx := DontCare
  statusArray.io.updateMidState := 0.U

  // select whether the source is from (whether slowPorts, regfile or imm)
  // for read-after-issue, it's done over the selected uop
  // for read-before-issue, it's done over the enqueue uop (and store the imm in dataArray to save space)
  // TODO: need to bypass data here.
  val immBypassedData = Wire(Vec(params.numEnq, Vec(params.numSrc, UInt(params.dataBits.W))))
  val immExts = s1_dispatchUops_dup(2).map(_.bits)
    .zip(enqReverse(io.srcRegValue))
    .zip(immBypassedData).map{ case ((uop, data), bypass) =>
    val immExt = ImmExtractor(params, uop, data)
    bypass := immExt.io.data_out
    immExt
  }

  /**
    * S1: Data broadcast (from Regfile and FUs) and read
    *
    * Note: this is only needed when read-before-issue
    */
  // dispatch data: the next cycle after enqueue
  for (i <- 0 until params.numEnq) {
    dataArray.io.write(i).enable := s1_dispatchUops_dup(2)(i).valid
    dataArray.io.write(i).mask := s1_dispatchUops_dup(2)(i).bits.srcIsReady.take(params.numSrc)
    dataArray.io.write(i).addr := s1_allocatePtrOH_dup(2)(i)
    dataArray.io.write(i).data := immBypassedData(i)
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
      val allocateValid = s1_enqDataCapture.zip(s1_dispatchUops_dup(2)).map(x => x._1(j)(i) && x._2.valid)
      val allocateDataCapture = ParallelMux(allocateValid, s1_allocatePtrOH_dup(2))
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
      val fromSlowPorts = Vec(dataArray.io.read.length + params.numEnq, Vec(params.numSrc, Input(UInt(dataArray.io.multiWrite.length.W))))
      val slowData = Vec(dataArray.io.multiWrite.length, Input(UInt(params.dataBits.W)))
      // for enq data
      val enqBypass = Vec(params.numDeq, Vec(params.numEnq, Input(Bool())))
      val enqData = Vec(params.numEnq, Vec(params.numSrc, Flipped(ValidIO(UInt(params.dataBits.W)))))
      // deq data
      val deqData = Vec(params.numDeq, Vec(params.numSrc, Output(UInt(params.dataBits.W))))
    })

    val slowCapture = io.fromSlowPorts.map(_.map(bySlow => (bySlow.orR, Mux1H(bySlow, io.slowData))))
    val realEnqData = io.enqData.zip(slowCapture.takeRight(params.numEnq)).map{ case (e, c) =>
      e.zip(c).map(x => Mux(x._2._1, x._2._2, x._1.bits))
    }

    for ((deq, i) <- io.deqData.zipWithIndex) {
      for (j <- 0 until params.numSrc) {
        // default deq data is selected from data array or from slow
        val normalData = Mux(slowCapture(i)(j)._1, slowCapture(i)(j)._2, io.readData(i)(j))
        val oldestData = Mux(slowCapture(params.numDeq)(j)._1, slowCapture(params.numDeq)(j)._2, io.readData.last(j))
        deq(j) := Mux(io.doOverride(i), oldestData, normalData)
        // when instructions are selected for dequeue after enq, we need to bypass data.
        when (io.enqBypass(i).asUInt.orR) {
          deq(j) := Mux1H(io.enqBypass(i), realEnqData.map(_(j)))
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
  val dataSlowCaptureAddr = dataArray.io.read.map(_.addr) ++ dataArray.io.write.map(_.addr)
  for ((port, addr) <- dataSelect.io.fromSlowPorts.zip(dataSlowCaptureAddr)) {
    for (j <- 0 until params.numSrc) {
      port(j) := VecInit(dataArray.io.multiWrite.map(w => w.enable && (addr & w.addr(j)).asUInt.orR)).asUInt
    }
  }
  dataSelect.io.slowData := dataArray.io.multiWrite.map(_.data)
  dataSelect.io.enqBypass := s1_select_bypass_s0
  for ((enq, i) <- dataSelect.io.enqData.zipWithIndex) {
    for (j <- 0 until params.numSrc) {
      enq(j).valid := RegNext(enqReverse(io.fromDispatch)(i).bits.srcIsReady(j))
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

    s2_deq(i).ready := !s2_deq(i).valid || io.deq(i).ready
    io.deq(i).valid := s2_deq(i).valid
    io.deq(i).bits := s2_deq(i).bits
    io.deq(i).bits.uop.debugInfo.issueTime := GTimer()

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

      val bypassNetwork = BypassNetwork(params.numSrc, params.numFastWakeup, params.dataBits, params.optDeqFirstStage)
      bypassNetwork.io.hold := !s2_deq(i).ready || !s1_out(i).valid
      bypassNetwork.io.source := s1_out(i).bits.src.take(params.numSrc)
      bypassNetwork.io.bypass.zip(wakeupBypassMask.zip(io.fastDatas)).foreach { case (by, (m, d)) =>
        by.valid := m
        by.data := d
      }
      bypassNetwork.io.target <> s2_deq(i).bits.src.take(params.numSrc)

      for (j <- 0 until params.numFastWakeup) {
        XSPerfAccumulate(s"source_bypass_${j}_$i", s1_out(i).fire && wakeupBypassMask(j).asUInt.orR)
      }

    }
  }


  if (select.io.balance.isDefined) {
    require(params.numDeq == 2)
    val balance = select.io.balance.get
    balance.tick := (balance.out && !s1_out(0).fire && s1_out(1).fire) ||
      (!balance.out && s1_out(0).fire && !s1_out(1).fire && !io.fromDispatch(0).fire)
  }

  // logs
  for ((dispatch, i) <- io.fromDispatch.zipWithIndex) {
    XSDebug(dispatch.valid && !dispatch.ready, p"enq blocked, robIdx ${dispatch.bits.robIdx}\n")
    XSDebug(dispatch.fire, p"enq fire, robIdx ${dispatch.bits.robIdx}, srcState ${Binary(dispatch.bits.srcState.asUInt)}\n")
    XSPerfAccumulate(s"allocate_fire_$i", dispatch.fire)
    XSPerfAccumulate(s"allocate_valid_$i", dispatch.valid)
    XSPerfAccumulate(s"srcState_ready_$i", PopCount(dispatch.bits.srcState.map(_ === SrcState.rdy)))
  }

  for ((deq, i) <- io.deq.zipWithIndex) {
    XSDebug(deq.fire, p"deq fire, robIdx ${deq.bits.uop.robIdx}\n")
    XSDebug(deq.valid && !deq.ready, p"deq blocked, robIdx ${deq.bits.uop.robIdx}\n")
    XSPerfAccumulate(s"deq_fire_$i", deq.fire)
    XSPerfAccumulate(s"deq_valid_$i", deq.valid)
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
  XSPerfAccumulate("allocate_num", PopCount(s0_doEnqueue))
  XSPerfHistogram("issue_num", PopCount(io.deq.map(_.valid)), true.B, 0, params.numDeq, 1)

  val perfEvents = Seq(("full", statusArray.io.isValid.andR))
  generatePerfEvent()
}