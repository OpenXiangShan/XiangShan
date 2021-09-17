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
import xiangshan.backend.exu.ExuConfig
import xiangshan.backend.issue.{ReservationStation, ReservationStationWrapper}
import xiangshan.backend.regfile.{Regfile, RfReadPort, RfWritePort}
import xiangshan.mem.{SqPtr, StoreDataBundle}

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
  val outFpRfReadPorts: Int
)(implicit p: Parameters) extends LazyModule with HasXSParameter with HasExuWbMappingHelper {
  val numDpPorts = dpPorts.length

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
  val numIntRfReadPorts = numDpPortIntRead.sum
  val numDpPortFpRead = dpPorts.map(_.map(_._1).map(configs(_)._1.fpSrcCnt).max)
  val numFpRfReadPorts = numDpPortFpRead.sum - numSTDPorts + outFpRfReadPorts

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
  val intRfConfig = (outer.numIntRfReadPorts > 0, outer.numIntRfReadPorts, intRfWritePorts)
  val fpRfConfig = (outer.numFpRfReadPorts > 0, outer.numFpRfReadPorts, fpRfWritePorts)

  val rs_all = outer.reservationStations

  // print rs info
  println("Scheduler: ")
  for ((rs, i) <- rs_all.zipWithIndex) {
    println(s"RS $i: $rs")
    println(s"  innerIntUop: ${outer.innerIntFastSources(i).map(_._2)}")
    println(s"  innerFpUop: ${outer.innerFpFastSources(i).map(_._2)}")
    println(s"  innerFastPorts: ${outer.innerFastPorts(i)}")
    println(s"  outFastPorts: ${outer.outFastPorts(i)}")
  }
  println(s"  number of issue ports: ${outer.numIssuePorts}")
  println(s"  number of replay ports: ${outer.numReplayPorts}")
  println(s"  size of load and store RSes: ${outer.getMemRsEntries}")
  println(s"  number of std ports: ${outer.numSTDPorts}")
  val numLoadPorts = outer.reservationStations.map(_.module.io.load).filter(_.isDefined).map(_.get.fastMatch.length).sum
  println(s"  number of load ports: ${numLoadPorts}")
  if (intRfConfig._1) {
    println(s"INT Regfile: ${intRfConfig._2}R${intRfConfig._3}W")
  }
  if (fpRfConfig._1) {
    println(s"FP  Regfile: ${fpRfConfig._2}R${fpRfConfig._3}W")
  }

  class SchedulerExtraIO extends XSBundle {
    // feedback ports
    val feedback = if (outer.numReplayPorts > 0) Some(Vec(outer.numReplayPorts, new Bundle {
      val replay = Flipped(ValidIO(new RSFeedback()(updatedP)))
      val rsIdx = Output(UInt(log2Up(memRsEntries).W))
      val isFirstIssue = Output(Bool())
    })) else None
    // special ports for store
    val stData = if (outer.numSTDPorts > 0) Some(Vec(outer.numSTDPorts, ValidIO(new StoreDataBundle))) else None
    val fpRfReadIn = if (outer.numSTDPorts > 0) Some(Vec(outer.numSTDPorts, Flipped(new RfReadPort(XLEN)))) else None
    val fpRfReadOut = if (outer.outFpRfReadPorts > 0) Some(Vec(outer.outFpRfReadPorts, new RfReadPort(XLEN))) else None
    val loadFastMatch = if (numLoadPorts > 0) Some(Vec(numLoadPorts, Output(UInt(exuParameters.LduCnt.W)))) else None
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))

    override def cloneType: SchedulerExtraIO.this.type =
      new SchedulerExtraIO().asInstanceOf[this.type]
  }

  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // dispatch and issue ports
    val allocate = Vec(outer.numDpPorts, Flipped(DecoupledIO(new MicroOp)))
    val issue = Vec(outer.numIssuePorts, DecoupledIO(new ExuInput))
    val fastUopOut = Vec(outer.numIssuePorts, ValidIO(new MicroOp))
    // wakeup-related ports
    val writeback = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new ExuOutput)))
    val fastUopIn = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new MicroOp)))
    // feedback ports
    val extra = new SchedulerExtraIO
  })

  def extraReadRf(numRead: Seq[Int]): Seq[UInt] = {
    require(numRead.length == io.allocate.length)
    val enq = io.allocate.map(_.bits.psrc)
    // TODO: for store, fp is located at the second operand
    // currently use numInt>0 && numFp>0. should make this configurable
    val containsStore = outer.dpFuConfigs.map(_.contains(staCfg))
    enq.zip(numRead).zip(containsStore).map{ case ((src, num), hasStore) =>
      src.take(num)
    }.fold(Seq())(_ ++ _)
  }
  def readIntRf: Seq[UInt] = extraReadRf(outer.numDpPortIntRead)
  def readFpRf: Seq[UInt] = extraReadRf(outer.numDpPortFpRead) ++ io.extra.fpRfReadOut.getOrElse(Seq()).map(_.addr)
  def stData: Seq[ValidIO[StoreDataBundle]] = io.extra.stData.getOrElse(Seq())

  def regfile(raddr: Seq[UInt], numWrite: Int, hasZero: Boolean, len: Int): Option[Regfile] = {
    val numReadPorts = raddr.length
    if (numReadPorts > 0) {
      val rf = Module(new Regfile(numReadPorts, numWrite, hasZero, len))
      rf.io.readPorts.map(_.addr).zip(raddr).foreach{ case (r1, r2) => r1 := r2 }
      rf.io.debug_rports := DontCare
      Some(rf)
    }
    else {
      None.asInstanceOf[Option[Regfile]]
    }
  }

  val intRf = regfile(readIntRf, intRfWritePorts, true, XLEN)
  val fpRf = if (outer.numFpRfReadPorts > 0) regfile(readFpRf, fpRfWritePorts, false, XLEN) else None
  val intRfReadData = if (intRf.isDefined) intRf.get.io.readPorts.map(_.data) else Seq()
  val fpRfReadData = if (fpRf.isDefined) fpRf.get.io.readPorts.map(_.data) else io.extra.fpRfReadIn.getOrElse(Seq()).map(_.data)

  // write ports: 0-3 ALU, 4-5 MUL, 6-7 LOAD
  // regfile write ports
  if (intRf.isDefined) {
    intRf.get.io.writePorts.zip(io.writeback.take(intRfWritePorts)).foreach {
      case (rf, wb) =>
        rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
        rf.addr := wb.bits.uop.pdest
        rf.data := wb.bits.data
    }
  }
  if (fpRf.isDefined) {
    fpRf.get.io.writePorts.zip(io.writeback.drop(intRfWritePorts)).foreach {
      case (rf, wb) =>
        rf.wen := wb.valid
        rf.addr := wb.bits.uop.pdest
        rf.data := wb.bits.data
    }
  }

  if (io.extra.fpRfReadIn.isDefined) {
    io.extra.fpRfReadIn.get.map(_.addr).zip(readFpRf).foreach{ case (r, addr) => r := addr}
  }

  if (io.extra.fpRfReadOut.isDefined) {
    io.extra.fpRfReadOut.get.map(_.data).zip(fpRfReadData.takeRight(outer.outFpRfReadPorts)).foreach{ case (a, b) => a := b}
  }
  var issueIdx = 0
  var feedbackIdx = 0
  var stDataIdx = 0
  var fastUopOutIdx = 0
  io.fastUopOut := DontCare
  for (((node, cfg), i) <- rs_all.zip(outer.configs.map(_._1)).zipWithIndex) {
    val rs = node.module

    rs.io.redirect <> io.redirect
    rs.io.redirect <> io.redirect
    rs.io.flush <> io.flush

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
    }
    if (rs.io.feedback.isDefined) {
      val width = rs.io.feedback.get.length
      val feedback = io.extra.feedback.get.slice(feedbackIdx, feedbackIdx + width)
      require(feedback(0).rsIdx.getWidth == rs.io.feedback.get(0).rsIdx.getWidth)
      rs.io.feedback.get.zip(feedback).foreach{ case (r, f) =>
        r.memfeedback <> f.replay
        r.rsIdx <> f.rsIdx
        r.isFirstIssue <> f.isFirstIssue
      }
      feedbackIdx += width
    }
    if (false && rs.io.store.isDefined) {
      val width = rs.io.store.get.stData.length
      rs.io.store.get.stData <> stData.slice(stDataIdx, stDataIdx + width)
      stDataIdx += width
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

  var intReadPort = 0
  var fpReadPort = 0
  for ((dp, i) <- outer.dpPorts.zipWithIndex) {
    // dp connects only one rs: don't use arbiter
    if (dp.length == 1) {
      rs_all(dp.head._1).module.io.fromDispatch(dp.head._2) <> io.allocate(i)
    }
    // dp connects more than one rs: use arbiter to route uop to the correct rs
    else {
      val func = dp.map(rs => (op: MicroOp) => rs_all(rs._1).canAccept(op.ctrl.fuType))
      val arbiterOut = DispatchArbiter(io.allocate(i), func)
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

  if (!env.FPGAPlatform && intRf.isDefined) {
    for ((rport, rat) <- intRf.get.io.debug_rports.zip(io.extra.debug_int_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr := VecInit(intRf.get.io.debug_rports.map(_.data))
  }
  if (!env.FPGAPlatform && fpRf.isDefined) {
    for ((rport, rat) <- fpRf.get.io.debug_rports.zip(io.extra.debug_fp_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.fpr := VecInit(fpRf.get.io.debug_rports.map(_.data))
  }

  XSPerfAccumulate("allocate_valid", PopCount(io.allocate.map(_.valid)))
  XSPerfAccumulate("allocate_fire", PopCount(io.allocate.map(_.fire())))
  XSPerfAccumulate("issue_valid", PopCount(io.issue.map(_.valid)))
  XSPerfAccumulate("issue_fire", PopCount(io.issue.map(_.fire)))
}
