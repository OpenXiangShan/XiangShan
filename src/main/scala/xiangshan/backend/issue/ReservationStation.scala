/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan._
import utils._
import xiangshan.backend.decode.{ImmUnion, Imm_U}
import xiangshan.backend.exu.{Exu, ExuConfig}
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.{SqPtr, StoreDataBundle}

import scala.math.max

case class RSConfig (
  name: String,
  numEntries: Int,
  numEnq: Int,
  numDeq: Int,
  numSrc: Int,
  dataBits: Int,
  dataIdBits: Int,
  numFastWakeup: Int,
  numWakeup: Int,
  numValueBroadCast: Int,
  hasFeedback: Boolean = false,
  delayedRf: Boolean = false,
  fixedLatency: Int = -1,
  checkWaitBit: Boolean = false,
  optBuf: Boolean = false
)

class ReservationStation
(
  myName : String,
  val exuCfg: ExuConfig,
  iqSize : Int,
  srcLen: Int,
  fastPortsCnt: Int,
  slowPortsCnt: Int,
  fixedDelay: Int,
  fastWakeup: Boolean,
  feedback: Boolean,
  enqNum: Int,
  deqNum: Int
)(implicit p: Parameters) extends XSModule {
  val iqIdxWidth = log2Up(iqSize+1)
  val nonBlocked = if (exuCfg == MulDivExeUnitCfg) false else fixedDelay >= 0
  val srcNum = if (exuCfg == JumpExeUnitCfg) 2 else max(exuCfg.intSrcCnt, exuCfg.fpSrcCnt)

  // require(nonBlocked==fastWakeup)
  val config = RSConfig(
    name = myName,
    numEntries = iqSize,
    numEnq = enqNum,
    numDeq = deqNum,
    numSrc = srcNum,
    dataBits = srcLen,
    dataIdBits = PhyRegIdxWidth,
    numFastWakeup = fastPortsCnt,
    // for now alu and fmac are not in slowPorts
    numWakeup = fastPortsCnt + (4 + slowPortsCnt),
    numValueBroadCast = (4 + slowPortsCnt),
    hasFeedback = feedback,
    delayedRf = exuCfg == StExeUnitCfg,
    fixedLatency = fixedDelay,
    checkWaitBit = if (exuCfg == LdExeUnitCfg || exuCfg == StExeUnitCfg) true else false,
    optBuf = if (exuCfg == AluExeUnitCfg) true else false
  )

  val io = IO(new Bundle {
    val numExist = Output(UInt(iqIdxWidth.W))
    // enq
    val fromDispatch = Vec(config.numEnq, Flipped(DecoupledIO(new MicroOp)))
    val srcRegValue = Vec(config.numEnq, Input(Vec(srcNum, UInt(srcLen.W))))
    val fpRegValue = if (config.delayedRf) Input(UInt(srcLen.W)) else null
    // deq
    val deq = Vec(config.numDeq, DecoupledIO(new ExuInput))
    val stData = if (exuCfg == StExeUnitCfg) ValidIO(new StoreDataBundle) else null

    val stIssuePtr = if (config.checkWaitBit) Input(new SqPtr()) else null

    val jumpPc = if(exuCfg == JumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null
    val jalr_target = if(exuCfg == JumpExeUnitCfg) Input(UInt(VAddrBits.W)) else null

    val fastUopOut = Vec(config.numDeq, ValidIO(new MicroOp))
    val fastUopsIn = Vec(config.numFastWakeup, Flipped(ValidIO(new MicroOp)))
    val fastDatas = Vec(config.numFastWakeup, Input(UInt(srcLen.W)))
    val slowPorts = Vec(slowPortsCnt, Flipped(ValidIO(new ExuOutput)))

    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())

    val memfeedback = if (config.hasFeedback) Flipped(ValidIO(new RSFeedback)) else null
    val rsIdx = if (config.hasFeedback) Output(UInt(log2Up(iqSize).W)) else null
    val isFirstIssue = if (config.hasFeedback) Output(Bool()) else null // NOTE: just use for tlb perf cnt
  })

  val statusArray = Module(new StatusArray(config))
  val select = Module(new SelectPolicy(config))
  val dataArray = Module(new DataArray(config))
  val payloadArray = Module(new PayloadArray(new MicroOp, config))

  io.numExist := PopCount(statusArray.io.isValid)
  statusArray.io.redirect := io.redirect
  statusArray.io.flush := io.flush

  /**
   * S0: Update status (from dispatch and wakeup)
   */
  // enqueue from dispatch
  select.io.validVec := statusArray.io.isValid
  val doEnqueue = Wire(Vec(config.numEnq, Bool()))
  val needFpSource = Wire(Vec(config.numEnq, Bool()))
  for (i <- 0 until config.numEnq) {
    io.fromDispatch(i).ready := select.io.allocate(i).valid
    // agreement with dispatch: don't enqueue when io.redirect.valid
    doEnqueue(i) := io.fromDispatch(i).fire() && !io.redirect.valid && !io.flush
    select.io.allocate(i).ready := doEnqueue(i)
    statusArray.io.update(i).enable := doEnqueue(i)
    statusArray.io.update(i).addr := select.io.allocate(i).bits
    statusArray.io.update(i).data.valid := true.B
    needFpSource(i) := io.fromDispatch(i).bits.needRfRPort(1, 1, false)
    statusArray.io.update(i).data.scheduled := (if (config.delayedRf) needFpSource(i) else false.B)
    statusArray.io.update(i).data.blocked := (if (config.checkWaitBit) io.fromDispatch(i).bits.cf.loadWaitBit else false.B)
    statusArray.io.update(i).data.credit := (if (config.delayedRf) Mux(needFpSource(i), 2.U, 0.U) else 0.U)
    statusArray.io.update(i).data.srcState := VecInit(io.fromDispatch(i).bits.srcIsReady.take(config.numSrc))
    statusArray.io.update(i).data.psrc := VecInit(io.fromDispatch(i).bits.psrc.take(config.numSrc))
    statusArray.io.update(i).data.srcType := VecInit(io.fromDispatch(i).bits.ctrl.srcType.take(config.numSrc))
    statusArray.io.update(i).data.roqIdx := io.fromDispatch(i).bits.roqIdx
    statusArray.io.update(i).data.sqIdx := io.fromDispatch(i).bits.sqIdx
    payloadArray.io.write(i).enable := doEnqueue(i)
    payloadArray.io.write(i).addr := select.io.allocate(i).bits
    payloadArray.io.write(i).data := io.fromDispatch(i).bits
  }
  // when config.checkWaitBit is set, we need to block issue until the corresponding store issues
  if (config.checkWaitBit) {
    statusArray.io.stIssuePtr := io.stIssuePtr
  }
  // wakeup from other RS or function units
  val fastNotInSlowWakeup = exuCfg match {
    case LdExeUnitCfg => io.fastUopsIn.drop(2).take(4)
    case StExeUnitCfg => io.fastUopsIn.drop(2)
    case JumpExeUnitCfg => io.fastUopsIn.drop(2)
    case MulDivExeUnitCfg => io.fastUopsIn.drop(2)
    case AluExeUnitCfg => io.fastUopsIn.drop(2).take(4)
    case _ => io.fastUopsIn
  }
  val fastNotInSlowData = exuCfg match {
    case LdExeUnitCfg => io.fastDatas.drop(2).take(4)
    case StExeUnitCfg => io.fastDatas.drop(2)
    case JumpExeUnitCfg => io.fastDatas.drop(2)
    case MulDivExeUnitCfg => io.fastDatas.drop(2)
    case AluExeUnitCfg => io.fastDatas.drop(2).take(4)
    case _ => io.fastDatas
  }
  val wakeupValid = io.fastUopsIn.map(_.valid) ++ RegNext(VecInit(fastNotInSlowWakeup.map(_.valid))) ++ io.slowPorts.map(_.valid)
  val wakeupDest = io.fastUopsIn.map(_.bits) ++ RegNext(VecInit(fastNotInSlowWakeup.map(_.bits))) ++ io.slowPorts.map(_.bits.uop)
  require(wakeupValid.size == config.numWakeup)
  require(wakeupDest.size == config.numWakeup)
  for (i <- 0 until config.numWakeup) {
    statusArray.io.wakeup(i).valid := wakeupValid(i)
    statusArray.io.wakeup(i).bits := wakeupDest(i)
  }

  /**
    * S1: scheduler (and regfile read)
    */
  // select the issue instructions
  select.io.request := statusArray.io.canIssue
  for (i <- 0 until config.numDeq) {
    select.io.grant(i).ready := io.deq(i).ready
    if (config.hasFeedback) {
      require(config.numDeq == 1)
      statusArray.io.issueGranted(0).valid := select.io.grant(0).fire
      statusArray.io.issueGranted(0).bits := select.io.grant(0).bits
      statusArray.io.deqResp(0).valid := io.memfeedback.valid
      statusArray.io.deqResp(0).bits.rsMask := UIntToOH(io.memfeedback.bits.rsIdx)
      statusArray.io.deqResp(0).bits.success := io.memfeedback.bits.hit
    }
    else {
      statusArray.io.issueGranted(i).valid := select.io.grant(i).fire
      statusArray.io.issueGranted(i).bits := select.io.grant(i).bits
      statusArray.io.deqResp(i).valid := select.io.grant(i).fire
      statusArray.io.deqResp(i).bits.rsMask := select.io.grant(i).bits
      statusArray.io.deqResp(i).bits.success := io.deq(i).ready
    }
    payloadArray.io.read(i).addr := select.io.grant(i).bits
    if (fixedDelay >= 0) {
      val wakeupQueue = Module(new WakeupQueue(fixedDelay))
      val fuCheck = (if (exuCfg == MulDivExeUnitCfg) payloadArray.io.read(i).data.ctrl.fuType === FuType.mul else true.B)
      wakeupQueue.io.in.valid := select.io.grant(i).fire && fuCheck
      wakeupQueue.io.in.bits := payloadArray.io.read(i).data
      wakeupQueue.io.redirect := io.redirect
      wakeupQueue.io.flush := io.flush
      io.fastUopOut(i) := wakeupQueue.io.out
    }
    else {
      io.fastUopOut(i).valid := false.B
      io.fastUopOut(i).bits := DontCare
    }
  }
  // select whether the source is from (whether regfile or imm)
  // for read-after-issue, it's done over the selected uop
  // for read-before-issue, it's done over the enqueue uop (and store the imm in dataArray to save space)
  def extractImm(uop: MicroOp) : Vec[Valid[UInt]] = {
    val data = Wire(Vec(config.numSrc, Valid(UInt(config.dataBits.W))))
    data := DontCare
    data.map(_.valid := false.B)
    if (exuCfg == JumpExeUnitCfg) {
        data(0).valid := SrcType.isPc(uop.ctrl.srcType(0))
        data(0).bits := SignExt(io.jumpPc, XLEN)
        data(1).valid := true.B
        data(1).bits := io.jalr_target
    }
    // alu only need U type and I type imm
    else if (exuCfg == AluExeUnitCfg) {
      data(1).valid := SrcType.isImm(uop.ctrl.srcType(1))
      val imm32 = Mux(uop.ctrl.selImm === SelImm.IMM_U,
        ImmUnion.U.toImm32(uop.ctrl.imm),
        ImmUnion.I.toImm32(uop.ctrl.imm)
      )
      data(1).bits := SignExt(imm32, XLEN)
    }
    data
  }
  // lastAllocateUop: Vec(config.numEnq, new MicroOp)
  val lastAllocateUop = RegNext(VecInit(io.fromDispatch.map(_.bits)))
  val immBypassedData = Wire(Vec(config.numEnq, Vec(config.numSrc, UInt(config.dataBits.W))))
  for (((uop, data), bypass) <- lastAllocateUop.zip(io.srcRegValue).zip(immBypassedData)) {
    bypass := extractImm(uop).zip(data).map {
      case (imm, reg_data) => Mux(imm.valid, imm.bits, reg_data)
    }
  }

  /**
   * S1: Data broadcast (from Regfile and FUs) and read
   *
   * Note: this is only needed when read-before-issue
   */
  // dispatch data: the next cycle after enqueue
  for (i <- 0 until config.numEnq) {
    dataArray.io.write(i).enable := RegNext(doEnqueue(i))
    dataArray.io.write(i).mask := RegNext(statusArray.io.update(i).data.srcState)
    dataArray.io.write(i).addr := RegNext(select.io.allocate(i).bits)
    dataArray.io.write(i).data := immBypassedData(i)
    if (config.delayedRf) {
      dataArray.io.delayedWrite(i).valid := RegNext(RegNext(doEnqueue(i) && needFpSource(i)))
      dataArray.io.delayedWrite(i).bits := io.fpRegValue
    }
  }
  // data broadcast: from function units (only slow wakeup date are needed)
  val broadcastValid = RegNext(VecInit(fastNotInSlowWakeup.map(_.valid))) ++ io.slowPorts.map(_.valid)
  val broadcastValue = fastNotInSlowData ++ VecInit(io.slowPorts.map(_.bits.data))
  require(broadcastValid.size == config.numValueBroadCast)
  require(broadcastValue.size == config.numValueBroadCast)
  val slowWakeupMatchVec = Wire(Vec(config.numEntries, Vec(config.numSrc, Vec(config.numValueBroadCast, Bool()))))
  for (i <- 0 until config.numEntries) {
    for (j <- 0 until config.numSrc) {
      slowWakeupMatchVec(i)(j) := statusArray.io.wakeupMatch(i)(j).asBools.drop(config.numFastWakeup)
    }
  }
  dataArray.io.multiWrite.zipWithIndex.map { case (w, i) =>
    w.enable := broadcastValid(i)
    for (j <- 0 until config.numSrc) {
      w.addr(j) := VecInit(slowWakeupMatchVec.map(_(j)(i))).asUInt
    }
    w.data := broadcastValue(i)
  }

  /**
   * S1: read data from regfile
   */
  val s1_out = Wire(Vec(config.numDeq, Decoupled(new ExuInput)))
  for (i <- 0 until config.numDeq) {
    dataArray.io.read(i).addr := select.io.grant(i).bits
    // for read-before-issue, we need to bypass the enqueue data here
    // for read-after-issue, we need to bypass the imm here
    // check enq data bypass (another form of broadcast except that we know where it hits) here
    // enqRegSelected: Vec(config.numEnq, Bool())
    val enqRegSelected = VecInit(select.io.allocate.map(a => RegNext(a.bits) === select.io.grant(i).bits))
    // enqSrcStateReg: Vec(config.numEnq, Vec(config.numSrc, Bool()))
    // [i][j]: i-th enqueue, j-th source state
    val enqSrcStateReg = RegNext(VecInit(statusArray.io.update.map(_.data.srcState)))
    // enqBypassValid: Vec(config.numEnq, Vec(config.numSrc, Bool()))
    val enqBypassValid = enqSrcStateReg.zip(enqRegSelected).map{ case (state, sel) => VecInit(state.map(_ && sel)) }

    // bypass data for config.numDeq
    val deqBypassValid = Mux1H(enqRegSelected, enqBypassValid)
    val deqBypassData = Mux1H(enqRegSelected, immBypassedData)
    // dequeue data should be bypassed
    val deqUop = payloadArray.io.read(i).data
    val deqDataRead = dataArray.io.read(i).data
    val deqData = VecInit(deqBypassValid.zip(deqBypassData).zip(deqDataRead).map {
      case ((v, d), r) => Mux(v, d, r)
    })

    s1_out(i).valid := select.io.grant(i).valid && !deqUop.roqIdx.needFlush(io.redirect, io.flush)
    s1_out(i).bits := DontCare
    for (j <- 0 until config.numSrc) {
      s1_out(i).bits.src(j) := deqData(j)
    }
    s1_out(i).bits.uop := deqUop
  }


  /**
   * S1: detect bypass from fast wakeup
   */
  // control: check the fast wakeup match
  val fastWakeupMatchVec = Wire(Vec(config.numEntries, Vec(config.numSrc, Vec(config.numFastWakeup, Bool()))))
  for (i <- 0 until config.numEntries) {
    for (j <- 0 until config.numSrc) {
      fastWakeupMatchVec(i)(j) := statusArray.io.wakeupMatch(i)(j).asBools.take(config.numFastWakeup)
    }
  }
  val fastWakeupMatchRegVec = RegNext(fastWakeupMatchVec)
  for (i <- 0 until config.numDeq) {
    val targetFastWakeupMatch = Mux1H(select.io.grant(i).bits, fastWakeupMatchRegVec)
    val wakeupBypassMask = Wire(Vec(config.numFastWakeup, Vec(config.numSrc, Bool())))
    for (j <- 0 until config.numFastWakeup) {
      wakeupBypassMask(j) := VecInit(targetFastWakeupMatch.map(_(j)))
    }
    // data: send to bypass network
    // TODO: these should be done outside RS
    val bypassNetwork = Module(new BypassNetwork(config.numSrc, config.numFastWakeup, config.dataBits, config.optBuf))
    bypassNetwork.io.hold := !io.deq(i).ready
    bypassNetwork.io.source := s1_out(i).bits.src.take(config.numSrc)
    bypassNetwork.io.bypass.zip(wakeupBypassMask.zip(io.fastDatas)).map { case (by, (m, d)) =>
      by.valid := m
      by.data := d
    }

    /**
      * S2: to function units
      */
    // payload: send to function units
    // TODO: these should be done outside RS
    PipelineConnect(s1_out(i), io.deq(i), io.deq(i).ready || io.deq(i).bits.uop.roqIdx.needFlush(io.redirect, io.flush), false.B)
    val pipeline_fire = s1_out(i).valid && io.deq(i).ready
    if (config.hasFeedback) {
      io.rsIdx := RegEnable(OHToUInt(select.io.grant(i).bits), pipeline_fire)
      io.isFirstIssue := false.B
    }

    for (j <- 0 until config.numSrc) {
      io.deq(i).bits.src(j) := bypassNetwork.io.target(j)
    }

    // legacy things
    if (exuCfg == StExeUnitCfg) {
      io.stData.valid := io.deq(i).valid
      io.stData.bits.data := io.deq(i).bits.src(1)
      io.stData.bits.uop := io.deq(i).bits.uop
    }
  }

  // logs
  for (dispatch <- io.fromDispatch) {
    XSDebug(dispatch.valid && !dispatch.ready, p"enq blocked, roqIdx ${dispatch.bits.roqIdx}\n")
    XSDebug(dispatch.fire(), p"enq fire, roqIdx ${dispatch.bits.roqIdx}, srcState ${Binary(dispatch.bits.srcState.asUInt)}\n")
  }
  for (deq <- io.deq) {
    XSDebug(deq.fire(), p"deq fire, roqIdx ${deq.bits.uop.roqIdx}\n")
    XSDebug(deq.valid && !deq.ready, p"deq blocked, roqIdx ${deq.bits.uop.roqIdx}\n")
  }
}
