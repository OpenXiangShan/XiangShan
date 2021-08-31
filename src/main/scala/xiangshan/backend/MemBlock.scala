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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.L1CacheErrorInfo
import xiangshan._
import xiangshan.backend.roq.RoqLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{TLB, TlbPtwIO}
import xiangshan.mem._
import xiangshan.backend.fu.{FenceToSbuffer, FunctionUnit, HasExceptionNO}
import utils._

class Std(implicit p: Parameters) extends FunctionUnit {
  io.in.ready := true.B
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := io.in.bits.src(0)
}

class MemBlock()(implicit p: Parameters) extends LazyModule {

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

  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // in
    val issue = Vec(exuParameters.LsExuCnt, Flipped(DecoupledIO(new ExuInput)))
    val replay = Vec(exuParameters.LsExuCnt, ValidIO(new RSFeedback))
    val rsIdx = Vec(exuParameters.LsExuCnt, Input(UInt(log2Up(IssQueSize).W)))
    val isFirstIssue = Vec(exuParameters.LsExuCnt, Input(Bool()))
    val stData = Vec(exuParameters.StuCnt, Flipped(ValidIO(new StoreDataBundle)))
    val stIssuePtr = Output(new SqPtr())
    // out
    val writeback = Vec(exuParameters.LsExuCnt, DecoupledIO(new ExuOutput))
    val otherFastWakeup = Vec(exuParameters.LduCnt + exuParameters.StuCnt, ValidIO(new MicroOp))
    // misc
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
    val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput))
    val memoryViolation = ValidIO(new Redirect)
    val ptw = new TlbPtwIO(LoadPipelineWidth + StorePipelineWidth)
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)
    val enqLsq = new LsqEnqIO
    val memPredUpdate = Vec(StorePipelineWidth, Input(new MemPredUpdateReq))
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
  io.writeback <> exeWbReqs ++ VecInit(storeUnits.map(_.io.stout))
  io.otherFastWakeup := DontCare
  io.otherFastWakeup.take(2).zip(loadUnits.map(_.io.fastUop)).foreach{case(a,b)=> a := b}

  // TODO: fast load wakeup

  val dtlb    = Module(new TLB(Width = DTLBWidth, isDtlb = true))
  val lsq     = Module(new LsqWrappper)
  val sbuffer = Module(new NewSbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)
  io.stIssuePtr := lsq.io.issuePtrExt

  // dtlb
  io.ptw         <> dtlb.io.ptw
  dtlb.io.sfence <> RegNext(io.sfence)
  dtlb.io.csr    <> RegNext(io.tlbCsr)

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect      <> io.redirect
    loadUnits(i).io.flush         <> io.flush
    loadUnits(i).io.rsFeedback    <> io.replay(i)
    loadUnits(i).io.rsIdx         := io.rsIdx(i) // TODO: beautify it
    loadUnits(i).io.isFirstIssue  := io.isFirstIssue(i) // NOTE: just for dtlb's perf cnt
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> io.issue(i)
    // dcache access
    loadUnits(i).io.dcache        <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // Lsq to load unit's rs

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
    lsq.io.loadDataForwarded(i)   <> loadUnits(i).io.lsq.loadDataForwarded

    // update waittable
    // TODO: read pc
    io.memPredUpdate(i) := DontCare
    lsq.io.needReplayFromRS(i)    <> loadUnits(i).io.lsq.needReplayFromRS
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)
    val dtlbReq = dtlb.io.requestor(exuParameters.LduCnt + i)

    stu.io.redirect    <> io.redirect
    stu.io.flush       <> io.flush
    stu.io.rsFeedback <> io.replay(exuParameters.LduCnt + i)
    stu.io.rsIdx       <> io.rsIdx(exuParameters.LduCnt + i)
    // NOTE: just for dtlb's perf cnt
    stu.io.isFirstIssue <> io.isFirstIssue(exuParameters.LduCnt + i)
    stu.io.dtlb        <> dtlbReq
    stu.io.stin        <> io.issue(exuParameters.LduCnt + i)
    stu.io.lsq         <> lsq.io.storeIn(i)

    // Lsq to load unit's rs
    // rs.io.storeData <> lsq.io.storeDataIn(i)
    lsq.io.storeDataIn(i) := io.stData(i)

    // sync issue info to store set LFST
    io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits

    io.stOut(i).valid := stu.io.stout.valid
    io.stOut(i).bits  := stu.io.stout.bits
    stu.io.stout.ready := true.B
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when (lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    io.stOut(0).valid := true.B
    io.stOut(0).bits  := lsq.io.mmioStout.bits
    lsq.io.mmioStout.ready := true.B
  }

  // Lsq
  lsq.io.roq            <> io.lsqio.roq
  lsq.io.enq            <> io.enqLsq
  lsq.io.brqRedirect    <> io.redirect
  lsq.io.flush          <> io.flush
  io.memoryViolation    <> lsq.io.rollback
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
  val st0_atomics = io.issue(atomic_rs0).valid && FuType.storeIsAMO(io.issue(atomic_rs0).bits.uop.ctrl.fuType)
  val st1_atomics = io.issue(atomic_rs1).valid && FuType.storeIsAMO(io.issue(atomic_rs1).bits.uop.ctrl.fuType)

  val st0_data_atomics = io.stData(0).valid && FuType.storeIsAMO(io.stData(0).bits.uop.ctrl.fuType)
  val st1_data_atomics = io.stData(1).valid && FuType.storeIsAMO(io.stData(1).bits.uop.ctrl.fuType)

  when (st0_atomics) {
    io.issue(atomic_rs0).ready := atomicsUnit.io.in.ready
    storeUnits(0).io.stin.valid := false.B

    state := s_atomics_0
    assert(!st1_atomics)
  }
  when (st1_atomics) {
    io.issue(atomic_rs1).ready := atomicsUnit.io.in.ready
    storeUnits(1).io.stin.valid := false.B

    state := s_atomics_1
    assert(!st0_atomics)
  }
  when (atomicsUnit.io.out.valid) {
    assert(state === s_atomics_0 || state === s_atomics_1)
    state := s_normal
  }

  atomicsUnit.io.in.valid := st0_atomics || st1_atomics
  atomicsUnit.io.in.bits  := Mux(st0_atomics, io.issue(atomic_rs0).bits, io.issue(atomic_rs1).bits)
  atomicsUnit.io.storeDataIn.valid := st0_data_atomics || st1_data_atomics
  atomicsUnit.io.storeDataIn.bits  := Mux(st0_data_atomics, io.stData(0).bits, io.stData(1).bits)
  atomicsUnit.io.rsIdx    := Mux(st0_atomics, io.rsIdx(atomic_rs0), io.rsIdx(atomic_rs1))
  atomicsUnit.io.redirect <> io.redirect
  atomicsUnit.io.flush <> io.flush

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
    atomicsUnit.io.rsFeedback <> io.replay(atomic_rs0)

    assert(!storeUnits(0).io.rsFeedback.valid)
  }
  when (state === s_atomics_1) {
    atomicsUnit.io.rsFeedback <> io.replay(atomic_rs1)

    assert(!storeUnits(1).io.rsFeedback.valid)
  }

  lsq.io.exceptionAddr.lsIdx  := io.lsqio.exceptionAddr.lsIdx
  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsUnit.io.exceptionAddr.valid, atomicsUnit.io.exceptionAddr.bits, lsq.io.exceptionAddr.vaddr)

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  val ldDeqCount = PopCount(io.issue.take(2).map(_.valid))
  val stDeqCount = PopCount(io.issue.drop(2).map(_.valid))
  val rsDeqCount = ldDeqCount + stDeqCount
  XSPerfAccumulate("load_rs_deq_count", ldDeqCount)
  XSPerfHistogram("load_rs_deq_count", ldDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("store_rs_deq_count", stDeqCount)
  XSPerfHistogram("store_rs_deq_count", stDeqCount, true.B, 1, 2, 1)
  XSPerfAccumulate("ls_rs_deq_count", rsDeqCount)
}
