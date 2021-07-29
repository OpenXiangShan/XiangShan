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
import xiangshan.backend.issue.ReservationStation
import xiangshan.backend.regfile.{Regfile, RfWritePort}
import xiangshan.mem.{SqPtr, StoreDataBundle}

import scala.collection.mutable.ArrayBuffer

class DispatchArbiter(func: Seq[MicroOp => Bool])(implicit p: Parameters) extends XSModule {
  val numTarget = func.length

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MicroOp))
    val out = Vec(numTarget, DecoupledIO(new MicroOp))
  })

  io.out.zip(func).foreach{ case (o, f) => {
    o.valid := io.in.valid && f(io.in.bits)
    o.bits := io.in.bits
  }}

  io.in.ready := VecInit(io.out.map(_.fire())).asUInt.orR
}

object DispatchArbiter {
  def apply(in: DecoupledIO[MicroOp], func: Seq[MicroOp => Bool])(implicit p: Parameters) = {
    val arbiter = Module(new DispatchArbiter(func))
    arbiter.io.in <> in
    arbiter.io.out
  }
}

class Scheduler(
  val configs: Seq[(ExuConfig, Int, Seq[Int], Seq[Int])],
  val dpPorts: Seq[Seq[(Int, Int)]]
)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val numDpPorts = dpPorts.length

  // instantiate reservation stations and connect the issue ports
  val reservationStations = configs.map{ case (config, numDeq, _, _) => {
    val rs = LazyModule(new ReservationStation())
    rs.addIssuePort(config, numDeq)
    rs
  }}

  // generate read and write ports for Regfile
  // per-rs information
  val rsIntRfWritePort = configs.indices.map(i => {
    val priority = reservationStations(i).wbIntPriority
    val higher = reservationStations.filter(_.wbIntPriority < priority).map(_.numIntWbPort).sum
    val same = reservationStations.take(i).filter(_.wbIntPriority == priority).map(_.numIntWbPort).sum
    higher + same
  })
  val rsFpRfWritePort = configs.indices.map(i => {
    val priority = reservationStations(i).wbFpPriority
    val higher = reservationStations.filter(_.wbFpPriority < priority).map(_.numFpWbPort).sum
    val same = reservationStations.take(i).filter(_.wbFpPriority == priority).map(_.numFpWbPort).sum
    higher + same
  })
  // overall read and write ports
  val intRfReadPorts = dpPorts.map(_.map(_._1).map(reservationStations(_).intSrcCnt).max).sum
  val fpRfReadPorts = dpPorts.map(_.map(_._1).map(reservationStations(_).fpSrcCnt).max).sum
  val intRfWritePorts = reservationStations.map(_.numIntWbPort).sum
  val fpRfWritePorts = reservationStations.map(_.numFpWbPort).sum


  // connect to dispatch
  val dpFuConfigs = dpPorts.map(_.map(p => reservationStations(p._1).addDispatchPort()).reduce(_ ++ _))

  for (((_, _, fastIntPorts, fastFpPorts), rs) <- configs.zip(reservationStations)) {
    // connect fast wakeup ports to target rs
    fastIntPorts.foreach(reservationStations(_).addEarlyWakeup(rs.numAllFastWakeupPort))
    fastFpPorts.foreach(reservationStations(_).addEarlyWakeup(rs.numAllFastWakeupPort))

    // connect wakeup ports to itself
    if (rs.intSrcCnt > 0) {
      rs.addWakeup(intRfWritePorts)
    }
    if (rs.fpSrcCnt > 0) {
      rs.addWakeup(fpRfWritePorts)
    }
  }

  val numIssuePorts = configs.map(_._2).sum

  val numReplayPorts = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numDeq).sum
  val memRsEntries = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numEntries)
  def getMemRsEntries = {
    require(memRsEntries.isEmpty || memRsEntries.max == memRsEntries.min, "different indexes not supported")
    if (memRsEntries.isEmpty) 0 else memRsEntries.max
  }
  val numSTDPorts = reservationStations.filter(_.params.isStore == true).map(_.params.numDeq).sum
  val numOutsideWakeup = reservationStations.map(_.numExtFastWakeupPort).sum

  var outerIntRfWrite = 0
  def addIntWritebackPorts(n: Int) = {
    reservationStations.foreach{ rs => {
      if (rs.intSrcCnt > 0) {
        rs.addWakeup(n)
      }
    }}
    outerIntRfWrite += n
    this
  }

  var outerFpRfWrite = 0
  def addFpWritebackPorts(n: Int) = {
    reservationStations.foreach{ rs => {
      if (rs.fpSrcCnt > 0) {
        rs.addWakeup(n)
      }
    }}
    outerFpRfWrite += n
    this
  }

  lazy val module = new SchedulerImp(this)
}

class SchedulerImp(outer: Scheduler) extends LazyModuleImp(outer) with HasXSParameter {
  val memRsEntries = outer.getMemRsEntries
  val updatedP = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = memRsEntries
    )
  })
  val intRfWritePorts = outer.intRfWritePorts + outer.outerIntRfWrite
  val fpRfWritePorts = outer.fpRfWritePorts + outer.outerFpRfWrite
  val intRfConfig = (outer.intRfReadPorts > 0, outer.intRfReadPorts, intRfWritePorts)
  val fpRfConfig = (outer.fpRfReadPorts > 0, outer.fpRfReadPorts, fpRfWritePorts)

  val rs_all = outer.reservationStations

  // print rs info
  println("Scheduler: ")
  for ((rs, i) <- rs_all.zipWithIndex) {
    println(s"RS $i: $rs")
  }
  println(s"  number of issue ports: ${outer.numIssuePorts}")
  println(s"  number of replay ports: ${outer.numReplayPorts}")
  println(s"  size of load and store RSes: ${outer.getMemRsEntries}")
  println(s"  number of std ports: ${outer.numSTDPorts}")
  println(s"  number of outside fast wakeup ports: ${outer.numOutsideWakeup}")
  if (intRfConfig._1) {
    println(s"INT Regfile: ${intRfConfig._2}R${intRfConfig._3}W")
  }
  if (fpRfConfig._1) {
    println(s"FP  Regfile: ${fpRfConfig._2}R${fpRfConfig._3}W")
  }

  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // dispatch and issue ports
    val allocate = Vec(outer.numDpPorts, Flipped(DecoupledIO(new MicroOp)))
    val issue = Vec(outer.numIssuePorts, DecoupledIO(new ExuInput))
    val writeback = Vec(intRfWritePorts + fpRfWritePorts, Flipped(ValidIO(new ExuOutput)))
    // fast wakeup from outside execution units and rs
    val fastUopIn = if (outer.numOutsideWakeup > 0) Some(Vec(outer.numOutsideWakeup, Flipped(ValidIO(new MicroOp)))) else None
    // read regfile
    val readIntRf = if (intRfConfig._1) Some(Vec(intRfConfig._2, Input(UInt(PhyRegIdxWidth.W)))) else None
    val readFpRf = if (fpRfConfig._1) Some(Vec(fpRfConfig._2, Input(UInt(PhyRegIdxWidth.W)))) else None
    // feedback ports
    val feedback = if (outer.numReplayPorts > 0) Some(Vec(outer.numReplayPorts, new Bundle {
      val replay = Flipped(ValidIO(new RSFeedback()(updatedP)))
      val rsIdx = Output(UInt(log2Up(memRsEntries).W))
      val isFirstIssue = Output(Bool())
    })) else None
    // special ports for store
    val stData = if (outer.numSTDPorts > 0) Some(Vec(outer.numSTDPorts, ValidIO(new StoreDataBundle))) else None
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
  })

  def readIntRf: Seq[UInt] = io.readIntRf.getOrElse(Seq())
  def readFpRf: Seq[UInt] = io.readFpRf.getOrElse(Seq())
  def stData: Seq[ValidIO[StoreDataBundle]] = io.stData.getOrElse(Seq())

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
  val fpRf = regfile(readFpRf, fpRfWritePorts, false, XLEN)
  val intRfReadData = if (intRf.isDefined) intRf.get.io.readPorts.map(_.data) else Seq()
  val fpRfReadData = if (fpRf.isDefined) fpRf.get.io.readPorts.map(_.data) else Seq()

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

  var issueIdx = 0
  var feedbackIdx = 0
  var stDataIdx = 0
  var otherFastUopIdx = 0
  for ((rs, i) <- rs_all.zipWithIndex) {
    rs.module.io.redirect <> io.redirect
    rs.module.io.redirect <> io.redirect
    rs.module.io.flush <> io.flush

    val issueWidth = rs.module.io.deq.length
    rs.module.io.deq <> io.issue.slice(issueIdx, issueIdx + issueWidth)
    issueIdx += issueWidth
    if (rs.module.io_jump.isDefined) {
      rs.module.io_jump.get.jumpPc := io.jumpPc
      rs.module.io_jump.get.jalr_target := io.jalr_target
    }
    if (rs.module.io_checkwait.isDefined) {
      rs.module.io_checkwait.get.stIssuePtr <> io.stIssuePtr
    }
    if (rs.module.io_feedback.isDefined) {
      val width = rs.module.io_feedback.get.memfeedback.length
      val feedback = io.feedback.get.slice(feedbackIdx, feedbackIdx + width)
      require(feedback(0).rsIdx.getWidth == rs.module.io_feedback.get.rsIdx(0).getWidth)
      rs.module.io_feedback.get.memfeedback <> feedback.map(_.replay)
      rs.module.io_feedback.get.rsIdx <> feedback.map(_.rsIdx)
      rs.module.io_feedback.get.isFirstIssue <> feedback.map(_.isFirstIssue)
      feedbackIdx += width
    }
    if (rs.module.io_store.isDefined) {
      val width = rs.module.io_store.get.stData.length
      rs.module.io_store.get.stData <> stData.slice(stDataIdx, stDataIdx + width)
      stDataIdx += width
    }

    (rs.intSrcCnt > 0, rs.fpSrcCnt > 0) match {
      case (true,  false) => rs.module.io.slowPorts := io.writeback.take(intRfWritePorts)
      case (false, true) => rs.module.io.slowPorts := io.writeback.drop(intRfWritePorts)
      case (true,  true) => rs.module.io.slowPorts := io.writeback
      case _ => throw new RuntimeException("unknown wakeup source")
    }

    if (rs.numAllFastWakeupPort > 0) {
      // currently only support either fast from RS or fast from pipeline
      val fromRS = rs.numOutFastWakeupPort != 0
      val fromOther = rs.numExtFastWakeupPort != 0
      require(!(fromRS && fromOther))
      val otherUop = io.fastUopIn.getOrElse(Seq()).drop(otherFastUopIdx).take(rs.numAllFastWakeupPort)
      val uop = if (fromOther) otherUop else rs.module.io_fastWakeup.get
      val allData = io.writeback.map(_.bits.data)
      if (rs.numIntWbPort > 0 && outer.configs(i)._3.nonEmpty) {
        val dataBegin = outer.rsIntRfWritePort(i)
        val dataEnd = dataBegin + rs.numAllFastWakeupPort
        val data = allData.slice(dataBegin, dataEnd)
        outer.configs(i)._3.foreach(rs_all(_).connectFastWakeup(uop, data))
        println(s"Fast wakeup: RS ${i} -> ${outer.configs(i)._3}, source: [$dataBegin,$dataEnd)")
        if (fromOther) {
          otherFastUopIdx += rs.numIntWbPort
        }
      }
      if (rs.numFpWbPort > 0 && outer.configs(i)._4.nonEmpty) {
        val dataBegin = intRfWritePorts + outer.rsFpRfWritePort(i)
        val dataEnd = dataBegin + rs.numAllFastWakeupPort
        val data = allData.slice(dataBegin, dataEnd)
        outer.configs(i)._4.foreach(rs_all(_).connectFastWakeup(uop, data))
        println(s"Fast wakeup: RS ${i} -> ${outer.configs(i)._4}, source [$dataBegin, $dataEnd)")
        if (fromOther) {
          otherFastUopIdx += rs.numFpWbPort
        }
      }
    }
  }
  require(issueIdx == io.issue.length)

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
        if (numIntRfPorts > 0) {
          require(numFpRfPorts == 1)
          require(numIntRfPorts == 2)
          when(RegNext(mod.io.fromDispatch(idx).bits.ctrl.srcType(1) === SrcType.fp)) {
            target(1) := fpRfPorts(0)
          }
        }
        else {
          target := fpRfPorts.take(target.length)
        }
      }
      fpReadPort += numFpRfPorts
    }
  }

  if (!env.FPGAPlatform && intRf.isDefined) {
    for ((rport, rat) <- intRf.get.io.debug_rports.zip(io.debug_int_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr := VecInit(intRf.get.io.debug_rports.map(_.data))
  }
  if (!env.FPGAPlatform && fpRf.isDefined) {
    for ((rport, rat) <- fpRf.get.io.debug_rports.zip(io.debug_fp_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.fpr := VecInit(fpRf.get.io.debug_rports.map(_.data))
  }
}
