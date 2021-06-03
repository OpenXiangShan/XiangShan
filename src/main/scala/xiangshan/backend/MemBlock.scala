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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.L1CacheErrorInfo
import xiangshan._
import xiangshan.backend.roq.{RoqLsqIO, RoqPtr}
import xiangshan.backend.exu._
import xiangshan.cache._
import xiangshan.mem._
import xiangshan.backend.fu.{FenceToSbuffer, HasExceptionNO}
import xiangshan.backend.issue.ReservationStation
import xiangshan.backend.regfile.RfReadPort
import utils._

class LsBlockToCtrlIO(implicit p: Parameters) extends XSBundle {
  val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
  val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.LsExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
  val replay = ValidIO(new Redirect)
}

class IntBlockToMemBlockIO(implicit p: Parameters) extends XSBundle {
  val readIntRf = Vec(NRMemReadPorts, new RfReadPort(XLEN))
}

class FpBlockToMemBlockIO(implicit p: Parameters) extends XSBundle {
  val readFpRf = Vec(exuParameters.StuCnt, new RfReadPort(XLEN + 1))
}

class MemBlock(
  val fastWakeUpIn: Seq[ExuConfig],
  val slowWakeUpIn: Seq[ExuConfig],
  val fastWakeUpOut: Seq[ExuConfig],
  val slowWakeUpOut: Seq[ExuConfig],
  val numIntWakeUpFp: Int
)(implicit p: Parameters) extends LazyModule {

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())

  lazy val module = new MemBlockImp(this)
}

class MemBlockImp(outer: MemBlock) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasExceptionNO
  with HasFPUParameters
  with HasExeBlockHelper
  with HasFpLoadHelper
{

  val fastWakeUpIn = outer.fastWakeUpIn
  val slowWakeUpIn = outer.slowWakeUpIn
  val fastWakeUpOut = outer.fastWakeUpOut
  val slowWakeUpOut = outer.slowWakeUpOut
  val numIntWakeUpFp = outer.numIntWakeUpFp

  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToLsBlockIO)
    val fromIntBlock = Flipped(new IntBlockToMemBlockIO)
    val fromFpBlock = Flipped(new FpBlockToMemBlockIO)
    val toCtrlBlock = new LsBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val intWakeUpFp = Vec(numIntWakeUpFp, Flipped(DecoupledIO(new ExuOutput)))
    val wakeUpOutInt = Flipped(new WakeUpBundle(fastWakeUpOut.size, slowWakeUpOut.size))
    val wakeUpOutFp = Flipped(new WakeUpBundle(fastWakeUpOut.size, slowWakeUpOut.size))

    val ldFastWakeUpInt = Flipped(new WakeUpBundle(exuParameters.LduCnt, 0))
    val intWbOut = Vec(4, Flipped(ValidIO(new ExuOutput)))
    val fpWbOut = Vec(8, Flipped(ValidIO(new ExuOutput)))

    val ptw = new TlbPtwIO(LoadPipelineWidth + StorePipelineWidth)
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)

    val lsqio = new Bundle {
      val exceptionAddr = new ExceptionAddrIO // to csr
      val roq = Flipped(new RoqLsqIO) // roq to lsq
    }

    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val error = new L1CacheErrorInfo
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }
  })

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  io.error <> RegNext(RegNext(dcache.io.error))

  val redirect = io.fromCtrlBlock.redirect

  val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  val exeUnits = loadUnits ++ storeUnits

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))

  val atomicsUnit = Module(new AtomicsUnit)

  val loadWritebackOverride  = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out.bits, loadUnits.head.io.ldout.bits)
  val ldOut0 = Wire(Decoupled(new ExuOutput))
  ldOut0.valid := atomicsUnit.io.out.valid || loadUnits.head.io.ldout.valid
  ldOut0.bits  := loadWritebackOverride
  atomicsUnit.io.out.ready := ldOut0.ready
  loadUnits.head.io.ldout.ready := ldOut0.ready

  val exeWbReqs = ldOut0 +: loadUnits.tail.map(_.io.ldout)
  // 'wakeUpFp' is 1 cycle later than 'exeWbReqs'
  val wakeUpFp = Wire(Vec(exuParameters.LduCnt, Decoupled(new ExuOutput)))

  val readPortIndex = Seq(0, 1, 2, 4)
  io.fromIntBlock.readIntRf.foreach(_.addr := DontCare)
  io.fromFpBlock.readFpRf.foreach(_.addr := DontCare)
  val reservationStations = (loadExuConfigs ++ storeExuConfigs).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf
    val readFpRf = cfg.readFpRf

    // load has uncertain latency, so only use external wake up data
    val fastDatas = fastWakeUpIn.zip(io.wakeUpIn.fast)
      .filter(x => (x._1.writeIntRf && readIntRf) || (x._1.writeFpRf && readFpRf))
      .map(a => (a._1, a._2.bits.data)) ++
      (if (cfg == LdExeUnitCfg && EnableLoadFastWakeUp) loadExuConfigs.zip(loadUnits.map(_.io.ldout.bits.data)) else Seq())

    val fastPortsCnt = fastDatas.length

    val slowPorts = if (cfg == StExeUnitCfg) io.intWbOut ++ io.fpWbOut else io.intWbOut
    val slowPortsCnt = slowPorts.length

    // if tlb miss, replay
    val feedback = true

    println(s"${i}: exu:${cfg.name} fastPortsCnt: ${fastPortsCnt} slowPorts: ${slowPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStation(s"rs_${cfg.name}", cfg, IssQueSize, XLEN,
      fastDatas.map(_._1).length,
      slowPorts.length,
      fixedDelay = certainLatency,
      fastWakeup = certainLatency >= 0,
      feedback = feedback, 1, 1)
    )

    rs.io.redirect <> redirect // TODO: remove it
    rs.io.flush    <> io.fromCtrlBlock.flush // TODO: remove it
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.fromDispatch  <> VecInit(io.fromCtrlBlock.enqIqCtrl(i))

    rs.io.srcRegValue(0)(0) := io.fromIntBlock.readIntRf(readPortIndex(i)).data
    if (i >= exuParameters.LduCnt) {
      rs.io.srcRegValue(0)(1) := io.fromIntBlock.readIntRf(readPortIndex(i) + 1).data
      rs.io.fpRegValue := io.fromFpBlock.readFpRf(i - exuParameters.LduCnt).data
    }

    rs.io.fastDatas <> fastDatas.map(_._2)
    rs.io.slowPorts <> slowPorts

    // exeUnits(i).io.redirect <> redirect
    // exeUnits(i).io.fromInt <> rs.io.deq
    rs.io.memfeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservationStations){
    rs.io.fastUopsIn <> fastWakeUpIn.zip(io.wakeUpIn.fastUops)
      .filter(x => (x._1.writeIntRf && rs.exuCfg.readIntRf) || (x._1.writeFpRf && rs.exuCfg.readFpRf))
      .map(_._2) ++
      (if (rs.exuCfg == LdExeUnitCfg && EnableLoadFastWakeUp) loadUnits.map(_.io.fastUop) else Seq())
  }

  wakeUpFp.zip(exeWbReqs).foreach{
    case(w, e) =>
      val r = RegNext(e.bits)
      w.bits := r
      w.valid := RegNext(e.valid && !e.bits.uop.roqIdx.needFlush(redirect, io.fromCtrlBlock.flush))
      e.ready := true.B
      assert(w.ready === true.B)
  }

  io.ldFastWakeUpInt.fastUops <> loadUnits.map(_.io.fastUop)
  io.ldFastWakeUpInt.fast <> loadUnits.map(_.io.ldout).map(decoupledIOToValidIO)
  io.wakeUpOutInt.slow <> exeWbReqs
  io.wakeUpOutFp.slow <> wakeUpFp
  io.wakeUpIn.slow.foreach(_.ready := true.B)
  io.intWakeUpFp.foreach(_.ready := true.B)

  val dtlb    = Module(new TLB(Width = DTLBWidth, isDtlb = true))
  val lsq     = Module(new LsqWrappper)
  val sbuffer = Module(new NewSbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)

  // dtlb
  io.ptw         <> dtlb.io.ptw
  dtlb.io.sfence <> RegNext(io.sfence)
  dtlb.io.csr    <> RegNext(io.tlbCsr)

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect      <> io.fromCtrlBlock.redirect
    loadUnits(i).io.flush         <> io.fromCtrlBlock.flush
    loadUnits(i).io.rsFeedback    <> reservationStations(i).io.memfeedback
    loadUnits(i).io.rsIdx         := reservationStations(i).io.rsIdx // TODO: beautify it
    loadUnits(i).io.isFirstIssue  := reservationStations(i).io.isFirstIssue // NOTE: just for dtlb's perf cnt
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> reservationStations(i).io.deq(0)
    // dcache access
    loadUnits(i).io.dcache        <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // Lsq to load unit's rs
    reservationStations(i).io.stIssuePtr := lsq.io.issuePtrExt

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
    lsq.io.loadDataForwarded(i)   <> loadUnits(i).io.lsq.loadDataForwarded

    // update waittable
    // TODO: read pc
    io.fromCtrlBlock.memPredUpdate(i) := DontCare
    lsq.io.needReplayFromRS(i)    <> loadUnits(i).io.lsq.needReplayFromRS
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)
    val rs = reservationStations(exuParameters.LduCnt + i)
    val dtlbReq = dtlb.io.requestor(exuParameters.LduCnt + i)

    stu.io.redirect    <> io.fromCtrlBlock.redirect
    stu.io.flush       <> io.fromCtrlBlock.flush
    stu.io.rsFeedback <> rs.io.memfeedback
    stu.io.rsIdx       <> rs.io.rsIdx
    stu.io.isFirstIssue <> rs.io.isFirstIssue // NOTE: just for dtlb's perf cnt
    stu.io.dtlb        <> dtlbReq
    stu.io.stin        <> rs.io.deq(0)
    stu.io.lsq         <> lsq.io.storeIn(i)

    // Lsq to load unit's rs
    rs.io.stIssuePtr := lsq.io.issuePtrExt
    // rs.io.storeData <> lsq.io.storeDataIn(i)
    lsq.io.storeDataIn(i) := rs.io.stData

    // sync issue info to rs
    lsq.io.storeIssue(i).valid := rs.io.deq(0).valid
    lsq.io.storeIssue(i).bits := rs.io.deq(0).bits

    // sync issue info to store set LFST
    io.toCtrlBlock.stIn(i).valid := rs.io.deq(0).valid
    io.toCtrlBlock.stIn(i).bits := rs.io.deq(0).bits

    io.toCtrlBlock.stOut(i).valid := stu.io.stout.valid
    io.toCtrlBlock.stOut(i).bits  := stu.io.stout.bits
    stu.io.stout.ready := true.B
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when (lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    io.toCtrlBlock.stOut(0).valid := true.B
    io.toCtrlBlock.stOut(0).bits  := lsq.io.mmioStout.bits
    lsq.io.mmioStout.ready := true.B
  }

  // Lsq
  lsq.io.roq            <> io.lsqio.roq
  lsq.io.enq            <> io.fromCtrlBlock.enqLsq
  lsq.io.brqRedirect    <> io.fromCtrlBlock.redirect
  lsq.io.flush          <> io.fromCtrlBlock.flush
  io.toCtrlBlock.replay <> lsq.io.rollback
  lsq.io.uncache        <> uncache.io.lsq
  // delay dcache refill for 1 cycle for better timing
  // TODO: remove RegNext after fixing refill paddr timing
  // lsq.io.dcache         <> dcache.io.lsu.lsq
  lsq.io.dcache         := RegNext(dcache.io.lsu.lsq)

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqempty        <> sbuffer.io.sqempty

  // Sbuffer
  sbuffer.io.csrCtrl    <> RegNext(io.csrCtrl)
  sbuffer.io.dcache     <> dcache.io.lsu.store
  sbuffer.io.dcache.resp.valid := RegNext(dcache.io.lsu.store.resp.valid)
  sbuffer.io.dcache.resp.bits := RegNext(dcache.io.lsu.store.resp.bits)
  assert(sbuffer.io.dcache.resp.ready === true.B)

  // flush sbuffer
  val fenceFlush = io.fenceToSbuffer.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  io.fenceToSbuffer.sbIsEmpty := RegNext(sbuffer.io.flush.empty)
  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush)

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal :: s_atomics_0 :: s_atomics_1 :: Nil = Enum(3)
  val state = RegInit(s_normal)

  val atomic_rs0  = exuParameters.LduCnt + 0
  val atomic_rs1  = exuParameters.LduCnt + 1
  val st0_atomics = reservationStations(atomic_rs0).io.deq(0).valid && FuType.storeIsAMO(reservationStations(atomic_rs0).io.deq(0).bits.uop.ctrl.fuType)
  val st1_atomics = reservationStations(atomic_rs1).io.deq(0).valid && FuType.storeIsAMO(reservationStations(atomic_rs1).io.deq(0).bits.uop.ctrl.fuType)

  val st0_data_atomics = reservationStations(atomic_rs0).io.stData.valid && FuType.storeIsAMO(reservationStations(atomic_rs0).io.stData.bits.uop.ctrl.fuType)
  val st1_data_atomics = reservationStations(atomic_rs1).io.stData.valid && FuType.storeIsAMO(reservationStations(atomic_rs1).io.stData.bits.uop.ctrl.fuType)

  when (st0_atomics) {
    reservationStations(atomic_rs0).io.deq(0).ready := atomicsUnit.io.in.ready
    storeUnits(0).io.stin.valid := false.B

    state := s_atomics_0
    assert(!st1_atomics)
  }
  when (st1_atomics) {
    reservationStations(atomic_rs1).io.deq(0).ready := atomicsUnit.io.in.ready
    storeUnits(1).io.stin.valid := false.B

    state := s_atomics_1
    assert(!st0_atomics)
  }
  when (atomicsUnit.io.out.valid) {
    assert(state === s_atomics_0 || state === s_atomics_1)
    state := s_normal
  }

  atomicsUnit.io.in.valid := st0_atomics || st1_atomics
  atomicsUnit.io.in.bits  := Mux(st0_atomics, reservationStations(atomic_rs0).io.deq(0).bits, reservationStations(atomic_rs1).io.deq(0).bits)
  atomicsUnit.io.storeDataIn.valid := st0_data_atomics || st1_data_atomics
  atomicsUnit.io.storeDataIn.bits  := Mux(st0_data_atomics, reservationStations(atomic_rs0).io.stData.bits, reservationStations(atomic_rs1).io.stData.bits)
  atomicsUnit.io.rsIdx    := Mux(st0_atomics, reservationStations(atomic_rs0).io.rsIdx, reservationStations(atomic_rs1).io.rsIdx)
  atomicsUnit.io.redirect <> io.fromCtrlBlock.redirect
  atomicsUnit.io.flush <> io.fromCtrlBlock.flush

  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := dtlb.io.requestor(0).req.ready

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty

  // for atomicsUnit, it uses loadUnit(0)'s TLB port
  when (state === s_atomics_0 || state === s_atomics_1) {
    atomicsUnit.io.dtlb <> dtlb.io.requestor(0)

    loadUnits(0).io.dtlb.resp.valid := false.B
    loadUnits(0).io.ldout.ready := false.B

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.dtlb.req.valid)
    assert(!loadUnits(0).io.ldout.valid)
  }

  when (state === s_atomics_0) {
    atomicsUnit.io.rsFeedback <> reservationStations(atomic_rs0).io.memfeedback

    assert(!storeUnits(0).io.rsFeedback.valid)
  }
  when (state === s_atomics_1) {
    atomicsUnit.io.rsFeedback <> reservationStations(atomic_rs1).io.memfeedback

    assert(!storeUnits(1).io.rsFeedback.valid)
  }

  lsq.io.exceptionAddr.lsIdx  := io.lsqio.exceptionAddr.lsIdx
  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsq.io.exceptionAddr.vaddr)

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  val ldDeqCount = PopCount(reservationStations.take(2).map(_.io.deq(0).valid))
  val stDeqCount = PopCount(reservationStations.drop(2).map(_.io.deq(0).valid))
  val rsDeqCount = ldDeqCount + stDeqCount
  XSPerfAccumulate("load_rs_deq_count", ldDeqCount)
  XSPerfHistogram("load_rs_deq_count", ldDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("store_rs_deq_count", stDeqCount)
  XSPerfHistogram("store_rs_deq_count", stDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("ls_rs_deq_count", rsDeqCount)
}
