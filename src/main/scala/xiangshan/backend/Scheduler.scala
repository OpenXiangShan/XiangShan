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
import xiangshan.backend.regfile.Regfile
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
  println(s"INT Regfile: ${intRfReadPorts}R${intRfWritePorts}W")
  println(s"FP  Regfile: ${fpRfReadPorts}R${fpRfWritePorts}W")

  // connect to dispatch
  val dpFuConfigs = dpPorts.map(_.map(p => reservationStations(p._1).addDispatchPort()).reduce(_ ++ _))

  for (((_, _, fastIntPorts, fastFpPorts), rs) <- configs.zip(reservationStations)) {
    // connect fast wakeup ports to target rs
    fastIntPorts.map(reservationStations(_).addEarlyWakeup(rs.numAllFastWakeupPort))
    fastFpPorts.map(reservationStations(_).addEarlyWakeup(rs.numAllFastWakeupPort))

    // connect wakeup ports to itself
    if (rs.intSrcCnt > 0) {
      rs.addWakeup(intRfWritePorts)
    }
    if (rs.fpSrcCnt > 0) {
      rs.addWakeup(fpRfWritePorts)
    }
  }

  // print rs info
  for ((rs, i) <- reservationStations.zipWithIndex) {
    println(s"RS $i: $rs")
  }

  println("Scheduler: ")
  val numIssuePorts = configs.map(_._2).sum
  println(s"  number of issue ports: ${numIssuePorts}")
  val numReplayPorts = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numDeq).sum
  println(s"  number of replay ports: ${numReplayPorts}")
  val memRsEntries = reservationStations.filter(_.params.hasFeedback == true).map(_.params.numEntries)
  require(memRsEntries.max == memRsEntries.min, "different indexes not supported")
  println(s"  size of load and store RSes: ${memRsEntries.max}")
  val numSTDPorts = reservationStations.filter(_.params.isStore == true).map(_.params.numDeq).sum
  println(s"  number of std ports: ${numSTDPorts}")
  val numOutsideWakeup = reservationStations.map(_.numExtFastWakeupPort).sum
  println(s"  number of outside fast wakeup ports: ${numOutsideWakeup}")

  lazy val module = new SchedulerImp(this)
}

class SchedulerImp(outer: Scheduler) extends LazyModuleImp(outer) with HasXSParameter {
  val memRsEntries = outer.memRsEntries.max
  val updatedP = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = memRsEntries
    )
  })

  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // dispatch and issue ports
    val allocate = Vec(outer.numDpPorts, Flipped(DecoupledIO(new MicroOp)))
    // read regfile
    val readIntRf = Vec(outer.intRfReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(outer.fpRfReadPorts, Input(UInt(PhyRegIdxWidth.W)))
    val issue = Vec(outer.numIssuePorts, DecoupledIO(new ExuInput))
    val writeback = Vec(outer.intRfWritePorts + outer.fpRfWritePorts, Flipped(ValidIO(new ExuOutput)))
    val replay = Vec(outer.numReplayPorts, Flipped(ValidIO(new RSFeedback()(updatedP))))
    val rsIdx = Vec(outer.numReplayPorts, Output(UInt(log2Up(memRsEntries).W)))
    val isFirstIssue = Vec(outer.numReplayPorts, Output(Bool()))
    val stData = Vec(outer.numSTDPorts, ValidIO(new StoreDataBundle))
    // 2LOAD, data is selected from writeback ports
    val otherFastWakeup = Vec(outer.numOutsideWakeup, Flipped(ValidIO(new MicroOp)))
    // misc
    val jumpPc = Input(UInt(VAddrBits.W))
    val jalr_target = Input(UInt(VAddrBits.W))
    val stIssuePtr = Input(new SqPtr())
    // debug
    val debug_int_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Input(UInt(PhyRegIdxWidth.W)))
  })

  val rs_all = outer.reservationStations

  // write ports: 0-3 ALU, 4-5 MUL, 6-7 LOAD
  val intRf = Module(new Regfile(
    numReadPorts = outer.intRfReadPorts,
    numWirtePorts = outer.intRfWritePorts,
    hasZero = true,
    len = XLEN
  ))
  // write ports: 0-3 FMA 4-5 FMISC, 6-7 LOAD
  val fpRf = Module(new Regfile(
    numReadPorts = outer.fpRfReadPorts,
    numWirtePorts = outer.fpRfWritePorts,
    hasZero = false,
    len = XLEN
  ))
  io.readIntRf <> intRf.io.readPorts.map(_.addr)
  io.readFpRf <> fpRf.io.readPorts.map(_.addr)

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
      require(io.rsIdx(0).getWidth == rs.module.io_feedback.get.rsIdx(0).getWidth)
      val width = rs.module.io_feedback.get.memfeedback.length
      rs.module.io_feedback.get.memfeedback <> io.replay.slice(feedbackIdx, feedbackIdx + width)
      rs.module.io_feedback.get.rsIdx <> io.rsIdx.slice(feedbackIdx, feedbackIdx + width)
      rs.module.io_feedback.get.isFirstIssue <> io.isFirstIssue.slice(feedbackIdx, feedbackIdx + width)
      feedbackIdx += width
    }
    if (rs.module.io_store.isDefined) {
      val width = rs.module.io_store.get.stData.length
      rs.module.io_store.get.stData <> io.stData.slice(stDataIdx, stDataIdx + width)
      stDataIdx += width
    }

    (rs.intSrcCnt > 0, rs.fpSrcCnt > 0) match {
      case (true,  false) => rs.module.io.slowPorts := io.writeback.take(outer.intRfWritePorts)
      case (false, true) => rs.module.io.slowPorts := io.writeback.drop(outer.intRfWritePorts)
      case (true,  true) => rs.module.io.slowPorts := io.writeback
      case _ => throw new RuntimeException("unknown wakeup source")
    }

    if (rs.numAllFastWakeupPort > 0) {
      // currently only support either fast from RS or fast from pipeline
      val fromRS = rs.numOutFastWakeupPort != 0
      val fromOther = rs.numExtFastWakeupPort != 0
      require(!(fromRS && fromOther))
      val otherUop = io.otherFastWakeup.drop(otherFastUopIdx).take(rs.numAllFastWakeupPort)
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
        val dataBegin = outer.intRfWritePorts + outer.rsFpRfWritePort(i)
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
      val intRfPorts = VecInit(intRf.io.readPorts.slice(intReadPort, intReadPort + numIntRfPorts).map(_.data))
      for ((rs, idx) <- dp) {
        val target = rs_all(rs).module.io.srcRegValue(idx)
        target := intRfPorts.take(target.length)
      }
      intReadPort += numIntRfPorts
    }

    val numFpRfPorts = dp.map(_._1).map(rs_all(_).fpSrcCnt).max
    if (numFpRfPorts > 0) {
      val fpRfPorts = VecInit(fpRf.io.readPorts.slice(fpReadPort, fpReadPort + numFpRfPorts).map(_.data))
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


  // regfile write ports
  intRf.io.writePorts.zip(io.writeback.take(outer.intRfWritePorts)).foreach {
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
  fpRf.io.writePorts.zip(io.writeback.drop(outer.intRfWritePorts)).foreach {
    case (rf, wb) =>
      rf.wen := wb.valid
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }

  intRf.io.debug_rports := DontCare
  fpRf.io.debug_rports := DontCare

  if (!env.FPGAPlatform) {
    for ((rport, rat) <- intRf.io.debug_rports.zip(io.debug_int_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchIntRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.gpr := VecInit(intRf.io.debug_rports.map(_.data))
  }
  if (!env.FPGAPlatform) {
    for ((rport, rat) <- fpRf.io.debug_rports.zip(io.debug_fp_rat)) {
      rport.addr := rat
    }
    val difftest = Module(new DifftestArchFpRegState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.fpr := VecInit(fpRf.io.debug_rports.map(_.data))
  }
}
