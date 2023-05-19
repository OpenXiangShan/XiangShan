<<<<<<< HEAD
=======
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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState, DifftestArchVecRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import xiangshan.backend.dispatch.Dispatch2Rs
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.issue.{BaseReservationStationWrapper, RSParams, RSMod}
import xiangshan.backend.regfile.{IntRegFile, VfRegFile, RfReadPort}
import xiangshan.backend.rename.{BusyTable, BusyTableReadIO}
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, MemWaitUpdateReq, SqPtr}
import chisel3.ExcitingUtils

class DispatchArbiter(func: Seq[MicroOp => Bool])(implicit p: Parameters) extends XSModule {
  val numTarget = func.length

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MicroOp))
    val out = Vec(numTarget, DecoupledIO(new MicroOp))
  })

  io.out.zip(func).foreach{ case (o, f) =>
    o.valid := io.in.valid && f(io.in.bits)
    o.bits := io.in.bits
  }

  io.in.ready := VecInit(io.out.map(_.fire())).asUInt.orR
}

object DispatchArbiter {
  def apply(in: DecoupledIO[MicroOp], func: Seq[MicroOp => Bool])(implicit p: Parameters) = {
    val arbiter = Module(new DispatchArbiter(func))
    arbiter.io.in <> in
    arbiter.io.out
  }
}

trait HasExuWbHelper {
  def findInWbPorts(wb: Seq[Seq[ExuConfig]], target: ExuConfig) : Seq[Int] = {
    wb.zipWithIndex.filter(_._1.contains(target)).map(_._2)
  }

  def findInWbPorts(wb: Seq[Seq[ExuConfig]], targets: Seq[ExuConfig]) : Seq[Int] = {
    targets.map(findInWbPorts(wb, _)).fold(Seq())(_ ++ _)
  }

  def getFastWakeupIndex(cfg: ExuConfig, intSource: Seq[Int], fpSource: Seq[Int], offset: Int) : Seq[Int] = {
    val sources = Seq(
      (cfg.readIntRf, intSource),
      (cfg.readFpVecRf, fpSource.map(_ + offset))
    )
    sources.map(c => if (c._1) c._2 else Seq()).reduce(_ ++ _)
  }
  def fpUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.fpVecWen
    uop
  }
  def fpOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && x.bits.uop.ctrl.fpVecWen
    out
  }
  def fpOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && x.bits.uop.ctrl.fpVecWen
    out
  }
  def intUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.rfWen
    uop
  }
  def intOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && !x.bits.uop.ctrl.fpVecWen
    out
  }
  def intOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && !x.bits.uop.ctrl.fpVecWen
    out
  }
  def decoupledIOToValidIO[T <: Data](d: DecoupledIO[T]): Valid[T] = {
    val v = Wire(Valid(d.bits.cloneType))
    v.valid := d.valid
    v.bits := d.bits
    v
  }

  def validIOToDecoupledIO[T <: Data](v: Valid[T]): DecoupledIO[T] = {
    val d = Wire(DecoupledIO(v.bits.cloneType))
    d.valid := v.valid
    d.ready := true.B
    d.bits := v.bits
    d
  }
}

/** A Scheduler lane includes:
  * 1. ports of dispatch
  * 2. issue unit
  * 3. exe unit
  **/
case class ScheLaneConfig (
  rsModGen: RSMod,
  exuConfig: ExuConfig,
  numDeq: Int,
  intFastWakeupTarget: Seq[ExuConfig] = Seq(),
  fpFastWakeupTarget: Seq[ExuConfig] = Seq()
){
  def name: String = exuConfig.name
  override def toString: String = {
    s"${name}*${numDeq} " +
    (if(intFastWakeupTarget.nonEmpty) "intFastWakeupTarget" + intFastWakeupTarget.map(_.name).foldLeft("")(_+" "+_) else "" ) +
    (if(fpFastWakeupTarget.nonEmpty) "fpFastWakeupTarget" + fpFastWakeupTarget.map(_.name).foldLeft("")(_+_) else "" ) +
    s"ExuConfig ${exuConfig}"
  }
}


case class DpPortMapConfig(rsIdx: Int, dpIdx: Int) {
  override def toString: String = {
    s"rsIdx ${rsIdx} dpIdx ${dpIdx}"
  }
}

abstract class Scheduler(
  val configs: Seq[ScheLaneConfig],
  val dpPorts: Seq[Seq[DpPortMapConfig]],
  val intRfWbPorts: Seq[Seq[ExuConfig]],
  val fpRfWbPorts: Seq[Seq[ExuConfig]],
  val outFastPorts: Seq[Seq[Int]],
  val outIntRfReadPorts: Int,
  val outFpRfReadPorts: Int,
  val hasIntRf: Boolean,
  val hasFpRf: Boolean
)(implicit p: Parameters) extends LazyModule with HasXSParameter with HasExuWbHelper {
  val numDpPorts = dpPorts.length
  // Each dispatch port has several rs, which responses to its own exu config
  val dpExuConfigs = dpPorts.map(port => port.map(_.rsIdx).map(configs(_).exuConfig))
  val dispatch2: Seq[Dispatch2Rs]

  // regfile parameters: overall read and write ports
  val numIntRfWritePorts = intRfWbPorts.length
  val numFpRfWritePorts = fpRfWbPorts.length

  // reservation station parameters: dispatch, regfile, issue, wakeup, fastWakeup
  // instantiate reservation stations and connect the issue ports
  val wakeupPorts = configs.map(_.exuConfig).map(config => {
    val numInt = if (config.intSrcCnt > 0) numIntRfWritePorts else 0
    val numFp = if (config.fpVecSrcCnt > 0) numFpRfWritePorts else 0
    numInt + numFp
  })
  val innerIntFastSources: Seq[Seq[(ScheLaneConfig, Int)]] = configs.map(_.exuConfig).map{ cfg =>
    configs.zipWithIndex.filter{ case (c, i) => c.intFastWakeupTarget.contains(cfg) && c.exuConfig.wakeupFromRS }
  }
  val innerFpFastSources: Seq[Seq[(ScheLaneConfig, Int)]] = configs.map(_.exuConfig).map{ cfg =>
    configs.zipWithIndex.filter{ case (c, i) => c.fpFastWakeupTarget.contains(cfg) && c.exuConfig.wakeupFromRS }
  }
  val innerFastPorts: Seq[Seq[Int]] = configs.map(_.exuConfig).zipWithIndex.map{ case (config, i) =>
    val intSource = findInWbPorts(intRfWbPorts, innerIntFastSources(i).map(_._1.exuConfig))
    val fpSource = findInWbPorts(fpRfWbPorts, innerFpFastSources(i).map(_._1.exuConfig))
    getFastWakeupIndex(config, intSource, fpSource, numIntRfWritePorts)
  }
  println(s"inner fast: $innerFastPorts")
  val numAllFastPorts: Seq[Int] = innerFastPorts.zip(outFastPorts).map{ case (i, o) => i.length + o.length }
  val reservationStations: Seq[BaseReservationStationWrapper] = configs.zipWithIndex.map{ case (cfg, i) =>
    val rs = LazyModule(cfg.rsModGen.rsWrapperGen(cfg.rsModGen, p))
    // rs.addModGen(cfg.rsModGen)
    rs.addIssuePort(cfg.exuConfig, cfg.numDeq)
    rs.addWakeup(wakeupPorts(i))
    rs.addEarlyWakeup(numAllFastPorts(i))
    rs.suggestName(s"rs${cfg.name}_Wrapper")
    rs
  }
  // connect to dispatch
  val dpFuConfigs: Seq[Seq[FuConfig]] = dpPorts.map(_.map(p => reservationStations(p.rsIdx).addDispatchPort()).reduce(_ ++ _))

  val numIssuePorts: Int = configs.map(_.numDeq).sum
  val numReplayPorts: Int = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numDeq).sum
  val memRsNum: Seq[Int] = reservationStations.filter(_.params.hasFeedback == true).map(_.numRS)
  val numLsqReplayPorts = reservationStations.filter(_.params.lsqFeedback == true).map(_.params.numDeq).sum
  val memRsEntries = reservationStations.filter(_.params.lsqFeedback == true).map(_.params.numEntries)
  val getMemRsEntries: Int = {
    require(memRsEntries.isEmpty || memRsEntries.max == memRsEntries.min, "different indexes not supported")
    require(memRsNum.isEmpty || memRsNum.max == memRsNum.min, "different num not supported")
    require(memRsNum.isEmpty || memRsNum.min != 0, "at least 1 memRs required")
    if (memRsNum.isEmpty) memRsEntries.max else if (memRsEntries.isEmpty) 0 else (memRsEntries.max / memRsNum.max)
  }
  val numSTDPorts: Int = reservationStations.filter(_.params.exuCfg.get == StdExeUnitCfg).map(_.params.numDeq).sum

  val numIntRfReadPorts: Int = reservationStations.map(p => (p.params.numDeq) * p.numIntRfPorts).sum + outIntRfReadPorts

  val numFpRfReadPorts: Int = reservationStations.map(p => (p.params.numDeq) * p.numFpRfPorts).sum + outFpRfReadPorts

  val hasVPU = configs.map(_.exuConfig.isVPU).reduce(_ || _)

  def canAccept(fuType: UInt): Bool = VecInit(configs.map(_.exuConfig.canAccept(fuType))).asUInt.orR
  def numRs: Int = reservationStations.map(_.numRS).sum

  println(s"InScheduler: memRs ${memRsNum} lsqReplay ${numLsqReplayPorts} memRsEntries ${memRsEntries} getMemRsEntries ${getMemRsEntries} STD     ${numSTDPorts} IntRfRead ${numIntRfReadPorts} FpRfRead ${numFpRfReadPorts}")

  lazy val module = new SchedulerImp(this)
}

trait SchedulerImpMethod { this: SchedulerImp =>

  def getIntBusyTable() = {
    val busyTable = Module(new BusyTable(readIntState.length, intRfWritePorts))
    busyTable.io.allocPregs.zip(io.allocPregs).foreach{ case (pregAlloc, allocReq) =>
      pregAlloc.valid := allocReq.isInt
      pregAlloc.bits := allocReq.preg
    }
    busyTable.io.wbPregs.zip(io.writebackInt).foreach{ case (pregWb, exuWb) =>
      pregWb.valid := exuWb.valid && exuWb.bits.uop.ctrl.rfWen
      pregWb.bits := exuWb.bits.uop.pdest
    }
    busyTable.io.read <> readIntState
    Some(busyTable)
  }

  def getFpBusyTable() = {
    // Some fp states are read from outside
    val numInFpStateRead = 0//io.extra.fpStateReadIn.getOrElse(Seq()).length
    // The left read requests are serviced by internal busytable
    val numBusyTableRead = readFpState.length - numInFpStateRead
    println(s"Scheduler: fpBusyTable: InFp ${numInFpStateRead} BusyTableRead ${numBusyTableRead}")
    val busyTable = if (numBusyTableRead > 0) {
      println(s"Scheduler: Gen FP BusyTable Read ${numBusyTableRead} Write ${fpRfWritePorts}")
      val busyTable = Module(new BusyTable(numBusyTableRead, fpRfWritePorts))
      busyTable.io.allocPregs.zip(io.allocPregs).foreach { case (pregAlloc, allocReq) =>
        pregAlloc.valid := allocReq.isFp
        pregAlloc.bits := allocReq.preg
      }
      busyTable.io.wbPregs.zip(io.writebackFp).foreach { case (pregWb, exuWb) =>
        pregWb.valid := exuWb.valid && exuWb.bits.uop.ctrl.fpVecWen
        pregWb.bits := exuWb.bits.uop.pdest
      }
      busyTable.io.read <> readFpState.take(numBusyTableRead)
      busyTable.io.read <> readFpState
      Some(busyTable)
    } else None
    if (io.extra.fpStateReadIn.isDefined && numInFpStateRead > 0) {
      println(s"Scheduler: FP Out read ${numInFpStateRead}")
      io.extra.fpStateReadIn.get <> readFpState.takeRight(numInFpStateRead)
    }
    busyTable
  }

  def extractIntReadRf(): Seq[UInt] = {
    rs_all.flatMap(_.module.readIntRf_asyn).map(_.addr)
  }
  def extractFpReadRf(): Seq[UInt] = {
    rs_all.flatMap(_.module.readFpRf_asyn).map(_.addr)
  }

  def genRegfile(): Seq[UInt] = Seq() // Implemented at sub-class
}

class SchedulerImp(outer: Scheduler) extends LazyModuleImp(outer) with HasXSParameter with HasPerfEvents with SchedulerImpMethod {
  val memRsEntries = outer.getMemRsEntries
  val updatedP = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = memRsEntries
    )
  })
  val intRfWritePorts = outer.numIntRfWritePorts
  val fpRfWritePorts = outer.numFpRfWritePorts
  val intRfConfig = (outer.numIntRfReadPorts > 0 && outer.hasIntRf, outer.numIntRfReadPorts, intRfWritePorts)
  val fpRfConfig = (outer.numFpRfReadPorts > 0 && outer.hasFpRf, outer.numFpRfReadPorts, fpRfWritePorts)

  val rs_all = outer.reservationStations
  rs_all.foreach(_.module.extra <> DontCare)
  val numLoadPorts = rs_all.filter(_.params.isLoad).map(_.module.extra.load).map(_.length).sum


  // print rs info
  println("Scheduler: ")
  println(s"  number of issue ports: ${outer.numIssuePorts}")
  println(s"  number of replay ports: ${outer.numReplayPorts}")
  println(s"  size of load and store RSes: ${outer.getMemRsEntries}")
  println(s"  number of std ports: ${outer.numSTDPorts}")
  println(s"  number of load ports: ${numLoadPorts}")
  println(s"  hasIntRf ${outer.hasIntRf} IntRfRead ${outer.numIntRfReadPorts} IntRfWrite ${outer.numIntRfWritePorts} outIntRfRead ${outer.outIntRfReadPorts}")
  println(s"  hasFpRf ${outer.hasFpRf} FpRfRead ${outer.numFpRfReadPorts} FpRfWrite ${outer.numFpRfWritePorts} outFpRfRead ${outer.outFpRfReadPorts}")
  if (intRfConfig._1) {
    println(s"INT Regfile: ${intRfConfig._2}R${intRfConfig._3}W")
  }
  if (fpRfConfig._1) {
    println(s"FP  Regfile: ${fpRfConfig._2}R${fpRfConfig._3}W")
  }
  for ((rs, i) <- rs_all.zipWithIndex) {
    println(s"RS $i: $rs")
    println(s"  innerIntUop: ${outer.innerIntFastSources(i).map(_._2)}")
    println(s"  innerFpUop: ${outer.innerFpFastSources(i).map(_._2)}")
    println(s"  innerFastPorts: ${outer.innerFastPorts(i)}")
    println(s"  outFastPorts: ${outer.outFastPorts(i)}")
    println(s"  loadBalance: ${rs_all(i).params.needBalance}")
  }

  class SchedulerExtraIO extends XSBundle {
    // feedback to dispatch
    val rsReady = Vec(outer.dispatch2.map(_.module.io.out.length).sum, Output(Bool()))
    // feedback ports
    val feedback = if (outer.numReplayPorts > 0) Some(Vec(outer.numReplayPorts, Flipped(new MemRSFeedbackIO()(updatedP)))) else None
    // special ports for RS that needs to read from other schedulers
    // In: read response from other schedulers
    // Out: read request to other schedulers
    val intRfReadIn = if (!outer.hasIntRf && outer.numIntRfReadPorts > 0) Some(Vec(outer.numIntRfReadPorts, Flipped(new RfReadPort(XLEN, IntPregIdxWidth)))) else None
    val intRfReadOut = if (outer.outIntRfReadPorts > 0) Some(Vec(outer.outIntRfReadPorts, new RfReadPort(XLEN, IntPregIdxWidth))) else None
    val fpRfReadIn = if (!outer.hasFpRf && outer.numFpRfReadPorts > 0) Some(Vec(outer.numFpRfReadPorts, Flipped(new RfReadPort(VLEN, VfPregIdxWidth)))) else None
    val fpStateReadIn = if (!outer.hasFpRf && outer.numFpRfReadPorts > 0) Some(Vec(outer.numFpRfReadPorts, Flipped(new BusyTableReadIO))) else None
    val fpRfReadOut = if (outer.outFpRfReadPorts > 0) Some(Vec(outer.outFpRfReadPorts, new RfReadPort(VLEN, VfPregIdxWidth))) else None
    val fpStateReadOut = if (outer.outFpRfReadPorts > 0) Some(Vec(outer.outFpRfReadPorts, new BusyTableReadIO)) else None
    val loadFastMatch = if (numLoadPorts > 0) Some(Vec(numLoadPorts, Output(UInt(exuParameters.LduCnt.W)))) else None
    val loadFastImm = if (numLoadPorts > 0) Some(Vec(numLoadPorts, Output(UInt(12.W)))) else None
    // for vset
    val archVconfigReadPort = if(outer.hasIntRf) Some(new RfReadPort(XLEN, IntPregIdxWidth)) else None
    val diffVconfigReadData = if(outer.hasIntRf) Some(Output(new VConfig)) else None
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // special ports for load / store rs
    val enqLsq = if (outer.numLsqReplayPorts > 0) Some(Flipped(new LsqEnqIO)) else None
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
    // from lsq
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueFlagSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_vconfig_rat = Input(UInt(PhyRegIdxWidth.W))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_vec_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    // perf
    val sqFull = Input(Bool())
    val lqFull = Input(Bool())
  }

  // val numFma = outer.configs.filter(_.exuConfig == FmacExeUnitCfg).map(_.numDeq).sum

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    // dispatch and issue ports
    // val allocate = Vec(outer.numDpPorts, Flipped(DecoupledIO(new MicroOp)))
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val in = Vec(dpParams.IntDqDeqWidth * outer.dispatch2.length, Flipped(DecoupledIO(new MicroOp)))
    val issue = Vec(outer.numIssuePorts, DecoupledIO(new ExuInput(outer.hasVPU)))
    val fastUopOut = Vec(outer.numIssuePorts, ValidIO(new MicroOp))
    // wakeup-related ports
    val writebackInt = Vec(intRfWritePorts, Flipped(ValidIO(new ExuOutput(false))))
    val writebackFp = Vec(fpRfWritePorts, Flipped(ValidIO(new ExuOutput(true))))
    val fastUopIn = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new MicroOp)))
    // misc ports
    val extra = new SchedulerExtraIO
  })

  // To reduce fanout, we add registers here for redirect.
  val redirect = RegNextWithEnable(io.redirect)

  /** Dispatch2Rs:
    */
  val dispatch2 = outer.dispatch2.map(_.module)
  dispatch2.foreach(_.io.redirect := redirect)

  io.in <> dispatch2.flatMap(_.io.in)
  io.extra.rsReady := outer.dispatch2.flatMap(_.module.io.out.map(_.ready))

  /** BusyTable:
    * Read regfile state when deq from dispatch2 and enq to rs
    * Store-Data-RS reads fp Regfile state(Cross Domain). But for timing optimization,
    * Store-Data-RS reads from local busytable copy.
    */
  val readIntState = dispatch2.flatMap(_.io.readIntState.getOrElse(Seq()))
  val readFpState = io.extra.fpStateReadOut.getOrElse(Seq()) ++ dispatch2.flatMap(_.io.readFpState.getOrElse(Seq()))

  val intBusyTable = if (readIntState.nonEmpty) getIntBusyTable() else None
  val fpBusyTable = if (readFpState.nonEmpty) getFpBusyTable() else None
  val allocate = dispatch2.flatMap(_.io.out)

  /** Regfile:
    * Currently, read regfile when at select stage of rs, get data at the same cycle.
    * Store-Data-RS reads fp Regfile (Cross Domain).
    */
  def readIntRf: Seq[UInt] = extractIntReadRf() ++ io.extra.intRfReadOut.getOrElse(Seq()).map(_.addr) :+ io.extra.archVconfigReadPort.get.addr
  def readFpRf: Seq[UInt] = extractFpReadRf() ++ io.extra.fpRfReadOut.getOrElse(Seq()).map(_.addr)

  val intRfReadData_asyn = if (intRfConfig._1) genRegfile()
    else io.extra.intRfReadIn.getOrElse(Seq()).map(_.data)
  val fpRfReadData_asyn = if (fpRfConfig._1) genRegfile()
    else VecInit(io.extra.fpRfReadIn.getOrElse(Seq()).map(_.data))

  rs_all.flatMap(_.module.readIntRf_asyn.map(_.data))
    .zip(intRfReadData_asyn)
    .foreach{ case (a, b) => a := b}
  rs_all.flatMap(_.module.readFpRf_asyn).map(_.data)
    .zip(fpRfReadData_asyn)
    .foreach{ case (a, b) => a := b}

  /** Connect Dispatch2Rs with RS
    */
  for ((dp, i) <- outer.dpPorts.zipWithIndex) {
    // dp connects only one rs: don't use arbiter
    if (dp.length == 1)
      rs_all(dp.head.rsIdx).module.io.fromDispatch(dp.head.dpIdx) <> allocate(i)
    // dp connects more than one rs: use arbiter to route uop to the correct rs
    else {
      val func = dp.map(rs => (op: MicroOp) => rs_all(rs.rsIdx).canAccept(op.ctrl.fuType))
      val arbiterOut = DispatchArbiter(allocate(i), func)
      val rsIn = VecInit(dp.map(rs => rs_all(rs.rsIdx).module.io.fromDispatch(rs.dpIdx)))
      rsIn <> arbiterOut
    }
  }

  /** Connect RS with:
    * 1. each other's wakeup(MicroOp)
    * 2. FU's writeback(ExuOutput)
    * 3. FU's ExuInput
    */
  var issueIdx = 0
  io.fastUopOut := DontCare
  for (((node, cfg), i) <- rs_all.zip(outer.configs.map(_.exuConfig)).zipWithIndex) {
    val rs = node.module
    rs.io.redirect <> io.redirect

    val issueWidth = rs.io.deq.length
    rs.io.deq <> io.issue.slice(issueIdx, issueIdx + issueWidth)
    if (rs.io.fastWakeup.isDefined) {
      rs.io.fastWakeup.get <> io.fastUopOut.slice(issueIdx, issueIdx + issueWidth)
    }
    issueIdx += issueWidth

    val intWriteback = io.writebackInt
    val fpWriteback  = io.writebackFp
    val writebackTotal = intWriteback ++ fpWriteback
    (cfg.intSrcCnt > 0, cfg.fpVecSrcCnt > 0) match {
      case (true,  false) => rs.io.slowPorts := intWriteback
      case (false, true) => rs.io.slowPorts := fpWriteback
      // delay fp for extra one cycle
      case (true,  true) => rs.io.slowPorts := intWriteback ++ fpWriteback
      case _ => throw new RuntimeException("unknown wakeup source")
    }

    val innerIntUop = outer.innerIntFastSources(i).map(_._2).map(rs_all(_).module.io.fastWakeup.get).fold(Seq())(_ ++ _)
    val innerFpUop = outer.innerFpFastSources(i).map(_._2).map(rs_all(_).module.io.fastWakeup.get).fold(Seq())(_ ++ _)
    val innerUop = innerIntUop ++ innerFpUop
    val innerData = outer.innerFastPorts(i).map(writebackTotal(_).bits.data)
    node.connectFastWakeup(innerUop, innerData)
    require(innerUop.length == innerData.length)

    val outerUop = outer.outFastPorts(i).map(io.fastUopIn(_))
    val outerData = outer.outFastPorts(i).map(writebackTotal(_).bits.data)
    node.connectFastWakeup(outerUop, outerData)
    require(outerUop.length == outerData.length)
  }
  require(issueIdx == io.issue.length)

  XSPerfAccumulate("allocate_valid", PopCount(allocate.map(_.valid)))
  XSPerfAccumulate("allocate_fire", PopCount(allocate.map(_.fire)))
  XSPerfAccumulate("issue_valid", PopCount(io.issue.map(_.valid)))
  XSPerfAccumulate("issue_fire", PopCount(io.issue.map(_.fire)))

  val lastCycleAllocate = RegNext(VecInit(allocate.map(_.fire)))
  val lastCycleIssue = RegNext(VecInit(io.issue.map(_.fire)))
  val schedulerPerf = Seq(
    ("sche_allocate_fire", PopCount(lastCycleAllocate)),
    ("sche_issue_fire",    PopCount(lastCycleIssue)   )
  )
  val intBtPerf = if (intBusyTable.isDefined) intBusyTable.get.getPerfEvents else Seq()
  val fpBtPerf = if (fpBusyTable.isDefined && !io.extra.fpStateReadIn.isDefined) fpBusyTable.get.getPerfEvents else Seq()
  val perfEvents = schedulerPerf ++ intBtPerf ++ fpBtPerf ++ rs_all.flatMap(_.module.getPerfEvents)
  generatePerfEvent()
}

trait IntSchedulerImpMethod { this: IntSchedulerImp =>
  override def genRegfile(): Seq[UInt] = {
    val wbPorts = io.writebackInt
    val waddr = wbPorts.map(_.bits.uop.pdest)
    val wdata = wbPorts.map(_.bits.data)
    val debugRead = io.extra.debug_int_rat :+ io.extra.debug_vconfig_rat
    val wen = wbPorts.map(wb =>wb.valid && wb.bits.uop.ctrl.rfWen)
    IntRegFile("IntRegFile", IntPhyRegs, readIntRf, wen, waddr, wdata, debugReadAddr = Some(debugRead))
  }
}

class IntScheduler(
  val configVec: Seq[ScheLaneConfig],
  val dpPortVec: Seq[Seq[DpPortMapConfig]],
  val intRfWbPortVec: Seq[Seq[ExuConfig]],
  val fpRfWbPortVec: Seq[Seq[ExuConfig]],
  val outFastPortVec: Seq[Seq[Int]],
  val outIntRfReadPortVec: Int,
  val outFpRfReadPortVec: Int
)(implicit p: Parameters) extends Scheduler(
  configVec, dpPortVec,
  intRfWbPortVec, fpRfWbPortVec,
  outFastPortVec, outIntRfReadPortVec, outFpRfReadPortVec,
  true, false
) {
  val dispatch2 = Seq(
    LazyModule(new Dispatch2Rs(dpExuConfigs.take(exuParameters.AluCnt))),
    LazyModule(new Dispatch2Rs(dpExuConfigs.drop(exuParameters.AluCnt)))
  )
  override lazy val module = new IntSchedulerImp(this)
}

class IntSchedulerImp(outer: Scheduler)(implicit p: Parameters) extends SchedulerImp(outer) with IntSchedulerImpMethod{
  // dirty code for ls dp
  dispatch2.foreach(dp => if (dp.io.enqLsq.isDefined) {
    val lsqCtrl = Module(new LsqEnqCtrl)
    lsqCtrl.io.redirect <> redirect
    lsqCtrl.io.enq <> dp.io.enqLsq.get
    lsqCtrl.io.lcommit := io.extra.lcommit
    lsqCtrl.io.scommit := io.extra.scommit
    lsqCtrl.io.lqCancelCnt := io.extra.lqCancelCnt
    lsqCtrl.io.sqCancelCnt := io.extra.sqCancelCnt
    io.extra.enqLsq.get <> lsqCtrl.io.enqLsq
    dontTouch(dp.io.enqLsq.get)
    dontTouch(lsqCtrl.io)
  })

  /** Read Regfile Cross Domain
    * The IntRfReadIn/Out is not used for now. But the dummp implementation of
    * Vector may need.
    * Currently, Store rs/iq read fp data.
    */
  if (io.extra.fpRfReadIn.isDefined) {
    println("Scheduler: has fpRfReadIn")
    // Due to distance issues, we RegNext the address for cross-block regfile read
    io.extra.fpRfReadIn.get.map(_.addr).zip(readFpRf).foreach{ case (r, addr) => r := addr}
    require(io.extra.fpRfReadIn.get.length == readFpRf.length)
  }

  if (io.extra.intRfReadOut.isDefined) {
    println("Scheduler: has IntRfReadOut")
    val extraIntReadData = intRfReadData_asyn.dropRight(34).takeRight(outer.outIntRfReadPorts)
    io.extra.intRfReadOut.get.map(_.data).zip(extraIntReadData).foreach{ case (a, b) => a := b }
    require(io.extra.intRfReadOut.get.length == extraIntReadData.length)
  }

  if (io.extra.loadFastMatch.isDefined) {
    val allLoadRS = outer.reservationStations.filter(_.params.isLoad).map(_.module.extra.load)
    io.extra.loadFastMatch.get := allLoadRS.map(_.map(_.fastMatch)).fold(Seq())(_ ++ _)
    io.extra.loadFastImm.get := allLoadRS.map(_.map(_.fastImm)).fold(Seq())(_ ++ _)
  }

  if(io.extra.archVconfigReadPort.isDefined) {
    io.extra.archVconfigReadPort.get.data := intRfReadData_asyn.dropRight(33).last
  }

  if (io.extra.diffVconfigReadData.isDefined) {
    io.extra.diffVconfigReadData.get := intRfReadData_asyn.last(15, 0).asTypeOf(new VConfig)
  }

  var feedbackIdx = 0
  var stDataIdx = 0
  for (((node, cfg), i) <- rs_all.zip(outer.configs.map(_.exuConfig)).zipWithIndex) {
    val rs = node.module

    if (rs.isJump) {
      val jumpFire = VecInit(rs.io.fromDispatch.map(dp => dp.fire && dp.bits.isJump)).asUInt.orR
      rs.extra.jump.jumpPc := RegEnable(io.extra.jumpPc, jumpFire)
      rs.extra.jump.jalr_target := io.extra.jalr_target
    }
    if (rs.checkWaitBit) {
      rs.extra.checkwait.stIssuePtr <> io.extra.stIssuePtr
      rs.extra.checkwait.memWaitUpdateReq <> io.extra.memWaitUpdateReq
    }
    if (rs.hasFeedback) {
      val width = rs.extra.feedback.length
      val feedback = io.extra.feedback.get.slice(feedbackIdx, feedbackIdx + width)
      require(feedback(0).rsIdx.getWidth == rs.extra.feedback(0).rsIdx.getWidth)
      rs.extra.feedback.zip(feedback).foreach{ case (r, f) =>
        r.feedbackFast <> f.feedbackFast
        r.feedbackSlow <> f.feedbackSlow
        r.rsIdx <> f.rsIdx
        r.isFirstIssue <> f.isFirstIssue
      }
      feedbackIdx += width
    }
  }

  if ((env.AlwaysBasicDiff || env.EnableDifftest) && intRfConfig._1) {
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := io.hartId
    difftest.io.gpr := RegNext(RegNext(VecInit(intRfReadData_asyn.takeRight(33).take(32))))
  }
  if (env.EnableTopDown && rs_all.exists(_.params.isLoad)) {
    val stall_ls_dq = WireDefault(0.B)
    ExcitingUtils.addSink(stall_ls_dq, "stall_ls_dq", ExcitingUtils.Perf)
    val ld_rs_full = !rs_all.filter(_.params.isLoad).map(_.module.io.fromDispatch.map(_.ready).reduce(_ && _)).reduce(_ && _)
    val st_rs_full = !rs_all.filter(rs => rs.params.isSta || rs.params.isStd).map(_.module.io.fromDispatch.map(_.ready).reduce(_ && _)).reduce(_ && _)
    val stall_stores_bound = stall_ls_dq && (st_rs_full || io.extra.sqFull)
    val stall_loads_bound = stall_ls_dq && (ld_rs_full || io.extra.lqFull)
    val stall_ls_bandwidth_bound = stall_ls_dq && !(st_rs_full || io.extra.sqFull) && !(ld_rs_full || io.extra.lqFull)
    ExcitingUtils.addSource(stall_loads_bound, "stall_loads_bound", ExcitingUtils.Perf)
    XSPerfAccumulate("stall_loads_bound", stall_loads_bound)
    XSPerfAccumulate("stall_stores_bound", stall_stores_bound)
    XSPerfAccumulate("stall_ls_bandwidth_bound", stall_ls_bandwidth_bound)
  }
}


trait VecSchedulerImpMethod { this: VecSchedulerImp =>
  override def genRegfile(): Seq[UInt] = {
    val wbPorts = io.writebackFp
    val waddr = wbPorts.map(_.bits.uop.pdest)
    val wdata = wbPorts.map(_.bits.data)
    val debugRead = io.extra.debug_fp_rat ++ io.extra.debug_vec_rat
    // For floating-point function units, every instruction writes either int or fp regfile.
    // Multi-wen for each regfile
    val wen = Seq.fill(VLEN/XLEN)(wbPorts.map(_.valid))
    val widenWdata = wdata.map(ZeroExt(_, VLEN))
    VfRegFile("VecFpRegFile", VfPhyRegs, VLEN/XLEN, readFpRf, wen, waddr, widenWdata, debugReadAddr = Some(debugRead))
  }
}
class VecScheduler(
  val configVec: Seq[ScheLaneConfig],
  val dpPortVec: Seq[Seq[DpPortMapConfig]],
  val intRfWbPortVec: Seq[Seq[ExuConfig]],
  val fpRfWbPortVec: Seq[Seq[ExuConfig]],
  val outFastPortVec: Seq[Seq[Int]],
  val outIntRfReadPortVec: Int,
  val outFpRfReadPortVec: Int
)(implicit p: Parameters) extends Scheduler(
  configVec, dpPortVec,
  intRfWbPortVec, fpRfWbPortVec,
  outFastPortVec, outIntRfReadPortVec, outFpRfReadPortVec,
  false, true
) {
  val dispatch2 = Seq(LazyModule(new Dispatch2Rs(dpExuConfigs.take(exuParameters.FmacCnt))),
                      LazyModule(new Dispatch2Rs(dpExuConfigs.drop(exuParameters.FmacCnt))))

  override lazy val module = new VecSchedulerImp(this)
}

class VecSchedulerImp(outer: Scheduler)(implicit p: Parameters) extends SchedulerImp(outer) with VecSchedulerImpMethod {
  // dirty code for ls dp
  dispatch2.foreach(dp => if (dp.io.enqLsq.isDefined) {
    val lsqCtrl = Module(new LsqEnqCtrl)
    lsqCtrl.io.redirect <> redirect
    lsqCtrl.io.enq <> dp.io.enqLsq.get
    lsqCtrl.io.lcommit := io.extra.lcommit
    lsqCtrl.io.scommit := io.extra.scommit
    lsqCtrl.io.lqCancelCnt := io.extra.lqCancelCnt
    lsqCtrl.io.sqCancelCnt := io.extra.sqCancelCnt
    io.extra.enqLsq.get <> lsqCtrl.io.enqLsq
  })

  /** Read Regfile Cross Domain
    * The IntRfReadIn/Out is not used for now. But the dummp implementation of
    * Vector may need.
    * Currently, Store rs/iq read fp data.
    */
  if (io.extra.intRfReadIn.isDefined) {
    println("Scheduler: has IntRfReadIn")
    io.extra.intRfReadIn.get.map(_.addr).zip(readIntRf).foreach{ case (r, addr) => r := addr}
    require(io.extra.intRfReadIn.get.length == readIntRf.length)
  }

  if (io.extra.fpRfReadOut.isDefined) {
    println("Scheduler: has fpRfReadOut")
    val extraFpReadData = fpRfReadData_asyn.dropRight(64).takeRight(outer.outFpRfReadPorts)
    io.extra.fpRfReadOut.get.map(_.data).zip(extraFpReadData).foreach{ case (a, b) => a := b }
    require(io.extra.fpRfReadOut.get.length == extraFpReadData.length)
  }

  if (io.extra.loadFastMatch.isDefined) {
    val allVecLoadRS = outer.reservationStations.filter(_.params.isVecLoad).map(_.module.extra.load)
    io.extra.loadFastMatch.get := allVecLoadRS.map(_.map(_.fastMatch)).fold(Seq())(_ ++ _)
    io.extra.loadFastImm.get := allVecLoadRS.map(_.map(_.fastImm)).fold(Seq())(_ ++ _)
  }

  if ((env.AlwaysBasicDiff || env.EnableDifftest) && fpRfConfig._1) {
    val fpReg = fpRfReadData_asyn.takeRight(64).take(32)
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock := clock
    difftest.io.coreid := io.hartId
    difftest.io.fpr.zip(fpReg).map(r => r._1 := RegNext(RegNext(r._2(XLEN-1, 0))))
  }
  if ((env.AlwaysBasicDiff || env.EnableDifftest) && fpRfConfig._1) {
    val vecReg = fpRfReadData_asyn.takeRight(32)
    val difftest = Module(new DifftestArchVecRegState)
    difftest.io.clock := clock
    difftest.io.coreid := io.hartId
    for (i <- 0 until 32)
      for (j <- 0 until (VLEN/XLEN))
        difftest.io.vpr((VLEN/XLEN)*i +j) := RegNext(RegNext(vecReg(i)(XLEN*(j+1)-1, XLEN*j)))
  }
}
>>>>>>> vlsu-uop-230301
