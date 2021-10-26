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

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import difftest.{DifftestArchFpRegState, DifftestArchIntRegState}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan._
import utils._
import xiangshan.backend.dispatch.Dispatch2Rs
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.fu.fpu.FMAMidResultIO
import xiangshan.backend.issue.{ReservationStation, ReservationStationWrapper, RsPerfCounter}
import xiangshan.backend.regfile.{Regfile, RfReadPort, RfWritePort}
import xiangshan.backend.rename.{BusyTable, BusyTableReadIO}
import xiangshan.mem.{LsqEnqIO, MemWaitUpdateReq, SqPtr, StoreDataBundle}

import scala.collection.mutable.ArrayBuffer

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

trait HasExuWbMappingHelper {
  def findInWbPorts(wb: Seq[Seq[ExuConfig]], target: ExuConfig) : Seq[Int] = {
    wb.zipWithIndex.filter(_._1.contains(target)).map(_._2)
  }
  def findInWbPorts(wb: Seq[Seq[ExuConfig]], targets: Seq[ExuConfig]) : Seq[Int] = {
    targets.map(findInWbPorts(wb, _)).fold(Seq())(_ ++ _)
  }
  def getFastWakeupIndex(cfg: ExuConfig, intSource: Seq[Int], fpSource: Seq[Int], offset: Int) : Seq[Int] = {
    val sources = Seq(
      (cfg.readIntRf, intSource),
      (cfg.readFpRf, fpSource.map(_ + offset))
    )
    sources.map(c => if (c._1) c._2 else Seq()).reduce(_ ++ _)
  }
}

class Scheduler(
  val configs: Seq[(ExuConfig, Int, Seq[ExuConfig], Seq[ExuConfig])],
  val dpPorts: Seq[Seq[(Int, Int)]],
  val intRfWbPorts: Seq[Seq[ExuConfig]],
  val fpRfWbPorts: Seq[Seq[ExuConfig]],
  val outFastPorts: Seq[Seq[Int]],
  val outIntRfReadPorts: Int,
  val outFpRfReadPorts: Int,
  val hasIntRf: Boolean,
  val hasFpRf: Boolean
)(implicit p: Parameters) extends LazyModule with HasXSParameter with HasExuWbMappingHelper {
  val numDpPorts = dpPorts.length
  val dpExuConfigs = dpPorts.map(port => port.map(_._1).map(configs(_)._1))
  def getDispatch2 = {
    if (dpExuConfigs.length > exuParameters.AluCnt) {
      val intDispatch = LazyModule(new Dispatch2Rs(dpExuConfigs.take(exuParameters.AluCnt)))
      val lsDispatch = LazyModule(new Dispatch2Rs(dpExuConfigs.drop(exuParameters.AluCnt)))
      Seq(intDispatch, lsDispatch)
    }
    else {
      val fpDispatch = LazyModule(new Dispatch2Rs(dpExuConfigs))
      Seq(fpDispatch)
    }
  }
  val dispatch2 = getDispatch2

  // regfile parameters: overall read and write ports
  val numIntRfWritePorts = intRfWbPorts.length
  val numFpRfWritePorts = fpRfWbPorts.length

  // reservation station parameters: dispatch, regfile, issue, wakeup, fastWakeup
  // instantiate reservation stations and connect the issue ports
  val wakeupPorts = configs.map(_._1).map(config => {
    val numInt = if (config.intSrcCnt > 0) numIntRfWritePorts else 0
    val numFp = if (config.fpSrcCnt > 0) numFpRfWritePorts else 0
    numInt + numFp
  })
  val innerIntFastSources = configs.map(_._1).map(cfg => configs.zipWithIndex.filter(c => c._1._3.contains(cfg) && c._1._1.wakeupFromRS))
  val innerFpFastSources = configs.map(_._1).map(cfg => configs.zipWithIndex.filter(c => c._1._4.contains(cfg) && c._1._1.wakeupFromRS))
  val innerFastPorts = configs.map(_._1).zipWithIndex.map{ case (config, i) =>
    val intSource = findInWbPorts(intRfWbPorts, innerIntFastSources(i).map(_._1._1))
    val fpSource = findInWbPorts(fpRfWbPorts, innerFpFastSources(i).map(_._1._1))
    getFastWakeupIndex(config, intSource, fpSource, numIntRfWritePorts)
  }
  println(s"inner fast: $innerFastPorts")
  val numAllFastPorts = innerFastPorts.zip(outFastPorts).map{ case (i, o) => i.length + o.length }
  val reservationStations = configs.zipWithIndex.map{ case ((config, numDeq, _, _), i) =>
    val rs = LazyModule(new ReservationStationWrapper())
    rs.addIssuePort(config, numDeq)
    rs.addWakeup(wakeupPorts(i))
    rs.addEarlyWakeup(numAllFastPorts(i))
    rs
  }
  // connect to dispatch
  val dpFuConfigs = dpPorts.map(_.map(p => reservationStations(p._1).addDispatchPort()).reduce(_ ++ _))

  val numIssuePorts = configs.map(_._2).sum
  val numReplayPorts = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numDeq).sum
  val memRsEntries = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numEntries)
  val getMemRsEntries = {
    require(memRsEntries.isEmpty || memRsEntries.max == memRsEntries.min, "different indexes not supported")
    if (memRsEntries.isEmpty) 0 else memRsEntries.max
  }
  val numSTDPorts = reservationStations.filter(_.params.exuCfg.get == StdExeUnitCfg).map(_.params.numDeq).sum

  val numDpPortIntRead = dpPorts.map(_.map(_._1).map(configs(_)._1.intSrcCnt).max)
  val numIntRfReadPorts = numDpPortIntRead.sum + outIntRfReadPorts
  val numDpPortFpRead = dpPorts.map(_.map(_._1).map(configs(_)._1.fpSrcCnt).max)
  val numFpRfReadPorts = numDpPortFpRead.sum + outFpRfReadPorts

  lazy val module = new SchedulerImp(this)

  def canAccept(fuType: UInt): Bool = {
    VecInit(configs.map(_._1.canAccept(fuType))).asUInt.orR
  }
}

class SchedulerImp(outer: Scheduler) extends LazyModuleImp(outer) with HasXSParameter {
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
  val numPerfPorts = outer.reservationStations.map(_.module.perf.length).sum

  // print rs info
  println("Scheduler: ")
  println(s"  number of issue ports: ${outer.numIssuePorts}")
  println(s"  number of replay ports: ${outer.numReplayPorts}")
  println(s"  size of load and store RSes: ${outer.getMemRsEntries}")
  println(s"  number of std ports: ${outer.numSTDPorts}")
  val numLoadPorts = outer.reservationStations.map(_.module.io.load).filter(_.isDefined).map(_.get.fastMatch.length).sum
  println(s"  number of load ports: ${numLoadPorts}")
  println(s"  number of perf ports: ${numPerfPorts}")
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
    // feedback ports
    val feedback = if (outer.numReplayPorts > 0) Some(Vec(outer.numReplayPorts, Flipped(new MemRSFeedbackIO()(updatedP)))) else None
    // special ports for RS that needs to read from other schedulers
    // In: read response from other schedulers
    // Out: read request to other schedulers
    val intRfReadIn = if (!outer.hasIntRf && outer.numIntRfReadPorts > 0) Some(Vec(outer.numIntRfReadPorts, Flipped(new RfReadPort(XLEN)))) else None
    val intRfReadOut = if (outer.outIntRfReadPorts > 0) Some(Vec(outer.outIntRfReadPorts, new RfReadPort(XLEN))) else None
    val fpRfReadIn = if (!outer.hasFpRf && outer.numFpRfReadPorts > 0) Some(Vec(outer.numFpRfReadPorts, Flipped(new RfReadPort(XLEN)))) else None
    val fpStateReadIn = if (!outer.hasFpRf && outer.numFpRfReadPorts > 0) Some(Vec(outer.numFpRfReadPorts, Flipped(new BusyTableReadIO))) else None
    val fpRfReadOut = if (outer.outFpRfReadPorts > 0) Some(Vec(outer.outFpRfReadPorts, new RfReadPort(XLEN))) else None
    val fpStateReadOut = if (outer.outFpRfReadPorts > 0) Some(Vec(outer.outFpRfReadPorts, new BusyTableReadIO)) else None
    val loadFastMatch = if (numLoadPorts > 0) Some(Vec(numLoadPorts, Output(UInt(exuParameters.LduCnt.W)))) else None
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // special ports for load / store rs
    val enqLsq = if (outer.numReplayPorts > 0) Some(Flipped(new LsqEnqIO)) else None
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val perf = Vec(numPerfPorts, Output(new RsPerfCounter))

    override def cloneType: SchedulerExtraIO.this.type =
      new SchedulerExtraIO().asInstanceOf[this.type]
  }

  val numFma = outer.reservationStations.map(_.module.io.fmaMid.getOrElse(Seq()).length).sum

  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    // dispatch and issue ports
    // val allocate = Vec(outer.numDpPorts, Flipped(DecoupledIO(new MicroOp)))
    val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq))
    val in = Vec(dpParams.IntDqDeqWidth * outer.dispatch2.length, Flipped(DecoupledIO(new MicroOp)))
    val issue = Vec(outer.numIssuePorts, DecoupledIO(new ExuInput))
    val fastUopOut = Vec(outer.numIssuePorts, ValidIO(new MicroOp))
    // wakeup-related ports
    val writeback = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new ExuOutput)))
    val fastUopIn = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new MicroOp)))
    // misc ports
    val extra = new SchedulerExtraIO
    val fmaMid = if (numFma > 0) Some(Vec(numFma, Flipped(new FMAMidResultIO))) else None
  })

  val dispatch2 = outer.dispatch2.map(_.module)

  // dirty code for ls dp
  dispatch2.foreach(dp => if (dp.io.enqLsq.isDefined) dp.io.enqLsq.get <> io.extra.enqLsq.get)

  io.in <> dispatch2.flatMap(_.io.in)
  val readIntState = dispatch2.flatMap(_.io.readIntState.getOrElse(Seq()))
  val intbtperfEvents = Wire(new PerfEventsBundle(4))
  val fpbtperfEvents = Wire(new PerfEventsBundle(4))
  intbtperfEvents := DontCare
  fpbtperfEvents  := DontCare
  if (readIntState.nonEmpty) {
    val busyTable = Module(new BusyTable(readIntState.length, intRfWritePorts))
    busyTable.io.allocPregs.zip(io.allocPregs).foreach{ case (pregAlloc, allocReq) =>
      pregAlloc.valid := allocReq.isInt
      pregAlloc.bits := allocReq.preg
    }
    busyTable.io.wbPregs.zip(io.writeback.take(intRfWritePorts)).foreach{ case (pregWb, exuWb) =>
      pregWb.valid := exuWb.valid && exuWb.bits.uop.ctrl.rfWen
      pregWb.bits := exuWb.bits.uop.pdest
    }
    intbtperfEvents <> busyTable.perfinfo.perfEvents
    busyTable.io.read <> readIntState
  }
  val readFpState = io.extra.fpStateReadOut.getOrElse(Seq()) ++ dispatch2.flatMap(_.io.readFpState.getOrElse(Seq()))
  if (readFpState.nonEmpty) {
    // Some fp states are read from outside
    val numInFpStateRead = io.extra.fpStateReadIn.getOrElse(Seq()).length
    // The left read requests are serviced by internal busytable
    val numBusyTableRead = readFpState.length - numInFpStateRead
    if (numBusyTableRead > 0) {
      val busyTable = Module(new BusyTable(numBusyTableRead, fpRfWritePorts))
      busyTable.io.allocPregs.zip(io.allocPregs).foreach { case (pregAlloc, allocReq) =>
        pregAlloc.valid := allocReq.isFp
        pregAlloc.bits := allocReq.preg
      }
      busyTable.io.wbPregs.zip(io.writeback.drop(intRfWritePorts)).foreach { case (pregWb, exuWb) =>
        pregWb.valid := exuWb.valid && exuWb.bits.uop.ctrl.fpWen
        pregWb.bits := exuWb.bits.uop.pdest
      }
      busyTable.io.read <> readFpState.take(numBusyTableRead)
      fpbtperfEvents <> busyTable.perfinfo.perfEvents
      busyTable.io.read <> readFpState
    }
    if (io.extra.fpStateReadIn.isDefined) {
      io.extra.fpStateReadIn.get <> readFpState.takeRight(numInFpStateRead)
    }
  }
  val allocate = dispatch2.flatMap(_.io.out)

  if (io.fmaMid.isDefined) {
    io.fmaMid.get <> outer.reservationStations.flatMap(_.module.io.fmaMid.getOrElse(Seq()))
  }

  def extraReadRf(numRead: Seq[Int]): Seq[UInt] = {
    require(numRead.length == allocate.length)
    allocate.map(_.bits.psrc).zip(numRead).flatMap{ case (src, num) => src.take(num) }
  }
  def readIntRf: Seq[UInt] = extraReadRf(outer.numDpPortIntRead) ++ io.extra.intRfReadOut.getOrElse(Seq()).map(_.addr)
  def readFpRf: Seq[UInt] = extraReadRf(outer.numDpPortFpRead) ++ io.extra.fpRfReadOut.getOrElse(Seq()).map(_.addr)

  def genRegfile(isInt: Boolean): Seq[UInt] = {
    val wbPorts = if (isInt) io.writeback.take(intRfWritePorts) else io.writeback.drop(intRfWritePorts)
    val waddr = wbPorts.map(_.bits.uop.pdest)
    val wdata = wbPorts.map(_.bits.data)
    val debugRead = if (isInt) io.extra.debug_int_rat else io.extra.debug_fp_rat
    if (isInt) {
      val wen = wbPorts.map(wb => wb.valid && wb.bits.uop.ctrl.rfWen)
      Regfile(NRPhyRegs, readIntRf, wen, waddr, wdata, true, debugRead = Some(debugRead))
    }
    else {
      // For floating-point function units, every instruction writes either int or fp regfile.
      val wen = wbPorts.map(_.valid)
      Regfile(NRPhyRegs, readFpRf, wen, waddr, wdata, false, debugRead = Some(debugRead))
    }
  }

  val intRfReadData = if (intRfConfig._1) genRegfile(true) else io.extra.intRfReadIn.getOrElse(Seq()).map(_.data)
  val fpRfReadData = if (fpRfConfig._1) genRegfile(false) else io.extra.fpRfReadIn.getOrElse(Seq()).map(_.data)

  if (io.extra.intRfReadIn.isDefined) {
    io.extra.intRfReadIn.get.map(_.addr).zip(readIntRf).foreach{ case (r, addr) => r := addr}
    require(io.extra.intRfReadIn.get.length == readIntRf.length)
  }

  if (io.extra.fpRfReadIn.isDefined) {
    io.extra.fpRfReadIn.get.map(_.addr).zip(readFpRf).foreach{ case (r, addr) => r := addr}
    require(io.extra.fpRfReadIn.get.length == readFpRf.length)
  }

  if (io.extra.intRfReadOut.isDefined) {
    val extraIntReadData = intRfReadData.dropRight(32).takeRight(outer.outIntRfReadPorts)
    io.extra.intRfReadOut.get.map(_.data).zip(extraIntReadData).foreach{ case (a, b) => a := b }
    require(io.extra.intRfReadOut.get.length == extraIntReadData.length)
  }

  if (io.extra.fpRfReadOut.isDefined) {
    val extraFpReadData = fpRfReadData.dropRight(32).takeRight(outer.outFpRfReadPorts)
    io.extra.fpRfReadOut.get.map(_.data).zip(extraFpReadData).foreach{ case (a, b) => a := b }
    require(io.extra.fpRfReadOut.get.length == extraFpReadData.length)
  }

  var issueIdx = 0
  var feedbackIdx = 0
  var stDataIdx = 0
  var fastUopOutIdx = 0
  io.fastUopOut := DontCare
  for (((node, cfg), i) <- rs_all.zip(outer.configs.map(_._1)).zipWithIndex) {
    val rs = node.module

    rs.io.redirect <> io.redirect

    val issueWidth = rs.io.deq.length
    rs.io.deq <> io.issue.slice(issueIdx, issueIdx + issueWidth)
    if (rs.io.fastWakeup.isDefined) {
      rs.io.fastWakeup.get <> io.fastUopOut.slice(issueIdx, issueIdx + issueWidth)
    }
    issueIdx += issueWidth

    if (rs.io.jump.isDefined) {
      rs.io.jump.get.jumpPc := io.extra.jumpPc
      rs.io.jump.get.jalr_target := io.extra.jalr_target
    }
    if (rs.io.checkwait.isDefined) {
      rs.io.checkwait.get.stIssuePtr <> io.extra.stIssuePtr
      rs.io.checkwait.get.memWaitUpdateReq <> io.extra.memWaitUpdateReq
    }
    if (rs.io.feedback.isDefined) {
      val width = rs.io.feedback.get.length
      val feedback = io.extra.feedback.get.slice(feedbackIdx, feedbackIdx + width)
      require(feedback(0).rsIdx.getWidth == rs.io.feedback.get(0).rsIdx.getWidth)
      rs.io.feedback.get.zip(feedback).foreach{ case (r, f) =>
        r.feedbackFast <> f.feedbackFast
        r.feedbackSlow <> f.feedbackSlow
        r.rsIdx <> f.rsIdx
        r.isFirstIssue <> f.isFirstIssue
      }
      feedbackIdx += width
    }

    (cfg.intSrcCnt > 0, cfg.fpSrcCnt > 0) match {
      case (true,  false) => rs.io.slowPorts := io.writeback.take(intRfWritePorts)
      case (false, true) => rs.io.slowPorts := io.writeback.drop(intRfWritePorts)
      case (true,  true) => rs.io.slowPorts := io.writeback
      case _ => throw new RuntimeException("unknown wakeup source")
    }

    val innerIntUop = outer.innerIntFastSources(i).map(_._2).map(rs_all(_).module.io.fastWakeup.get).fold(Seq())(_ ++ _)
    val innerFpUop = outer.innerFpFastSources(i).map(_._2).map(rs_all(_).module.io.fastWakeup.get).fold(Seq())(_ ++ _)
    val innerUop = innerIntUop ++ innerFpUop
    val innerData = outer.innerFastPorts(i).map(io.writeback(_).bits.data)
    node.connectFastWakeup(innerUop, innerData)
    require(innerUop.length == innerData.length)

    val outerUop = outer.outFastPorts(i).map(io.fastUopIn(_))
    val outerData = outer.outFastPorts(i).map(io.writeback(_).bits.data)
    node.connectFastWakeup(outerUop, outerData)
    require(outerUop.length == outerData.length)
  }
  require(issueIdx == io.issue.length)
  if (io.extra.loadFastMatch.isDefined) {
    val allLoadRS = outer.reservationStations.map(_.module.io.load).filter(_.isDefined)
    io.extra.loadFastMatch.get := allLoadRS.map(_.get.fastMatch).fold(Seq())(_ ++ _)
  }
  io.extra.perf <> rs_all.flatMap(_.module.perf)

  var intReadPort = 0
  var fpReadPort = 0
  for ((dp, i) <- outer.dpPorts.zipWithIndex) {
    // dp connects only one rs: don't use arbiter
    if (dp.length == 1) {
      rs_all(dp.head._1).module.io.fromDispatch(dp.head._2) <> allocate(i)
    }
    // dp connects more than one rs: use arbiter to route uop to the correct rs
    else {
      val func = dp.map(rs => (op: MicroOp) => rs_all(rs._1).canAccept(op.ctrl.fuType))
      val arbiterOut = DispatchArbiter(allocate(i), func)
      val rsIn = VecInit(dp.map(rs => rs_all(rs._1).module.io.fromDispatch(rs._2)))
      rsIn <> arbiterOut
    }

    val numIntRfPorts = dp.map(_._1).map(rs_all(_).intSrcCnt).max
    if (numIntRfPorts > 0) {
      val intRfPorts = VecInit(intRfReadData.slice(intReadPort, intReadPort + numIntRfPorts))
      for ((rs, idx) <- dp) {
        val target = rs_all(rs).module.io.srcRegValue(idx)
        target := intRfPorts.take(target.length)
      }
      intReadPort += numIntRfPorts
    }

    val numFpRfPorts = dp.map(_._1).map(rs_all(_).fpSrcCnt).max
    if (numFpRfPorts > 0) {
      val fpRfPorts = VecInit(fpRfReadData.slice(fpReadPort, fpReadPort + numFpRfPorts))
      for ((rs, idx) <- dp) {
        val mod = rs_all(rs).module
        val target = mod.io.srcRegValue(idx)
        // dirty code for store
        val isFp = RegNext(mod.io.fromDispatch(idx).bits.ctrl.srcType(0) === SrcType.fp)
        val fromFp = if (numIntRfPorts > 0) isFp else false.B
        if (numIntRfPorts > 0) {
          require(numFpRfPorts == 1 && numIntRfPorts == 1)
        }
        when (fromFp) {
          target := fpRfPorts.take(target.length)
        }
      }
      fpReadPort += numFpRfPorts
    }
  }

  if (!env.FPGAPlatform && intRfConfig._1) {
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr := intRfReadData.takeRight(32)
  }
  if (!env.FPGAPlatform && fpRfConfig._1) {
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.fpr := fpRfReadData.takeRight(32)
  }

  XSPerfAccumulate("allocate_valid", PopCount(allocate.map(_.valid)))
  XSPerfAccumulate("allocate_fire", PopCount(allocate.map(_.fire())))
  XSPerfAccumulate("issue_valid", PopCount(io.issue.map(_.valid)))
  XSPerfAccumulate("issue_fire", PopCount(io.issue.map(_.fire)))
  val perfEvents_list = Wire(new PerfEventsBundle(2))
  val perfEvents = Seq(
    ("sche_allocate_fire    ", PopCount(allocate.map(_.fire()))   ),
    ("sche_issue_fire       ", PopCount(io.issue.map(_.fire))     ),
  )
  for (((perf_out,(perf_name,perf)),i) <- perfEvents_list.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }

  val perf_list =  perfEvents_list.perf_events ++ intbtperfEvents.perf_events ++ fpbtperfEvents.perf_events
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(perf_list.length))
  })
  perfinfo.perfEvents.perf_events := perf_list
}
