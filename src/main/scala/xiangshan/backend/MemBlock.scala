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
import xiangshan._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{BTlbPtwIO, PtwResp, TLB, TlbReplace}
import xiangshan.mem._
import xiangshan.backend.fu.{FenceToSbuffer, FunctionUnit, HasExceptionNO, PMP, PMPChecker, PMPModule, PFEvent}
import utils._
import xiangshan.backend.exu.StdExeUnit

class Std(implicit p: Parameters) extends FunctionUnit {
  io.in.ready := true.B
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := io.in.bits.src(0)
}

class AmoData(implicit p: Parameters) extends FunctionUnit {
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
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val redirect = Flipped(ValidIO(new Redirect))
    // in
    val issue = Vec(exuParameters.LsExuCnt + 2, Flipped(DecoupledIO(new ExuInput)))
    val loadFastMatch = Vec(exuParameters.LduCnt, Input(UInt(exuParameters.LduCnt.W)))
    val rsfeedback = Vec(exuParameters.LsExuCnt, new MemRSFeedbackIO)
    val stIssuePtr = Output(new SqPtr())
    // out
    val writeback = Vec(exuParameters.LsExuCnt + 2, DecoupledIO(new ExuOutput))
    val otherFastWakeup = Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new MicroOp))
    // misc
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
    val stOut = Vec(exuParameters.StuCnt, ValidIO(new ExuOutput))
    val memoryViolation = ValidIO(new Redirect)
    val ptw = new BTlbPtwIO(exuParameters.LduCnt + exuParameters.StuCnt)
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)
    val enqLsq = new LsqEnqIO
    val memPredUpdate = Vec(exuParameters.StuCnt, Input(new MemPredUpdateReq))
    val lsqio = new Bundle {
      val exceptionAddr = new ExceptionAddrIO // to csr
      val rob = Flipped(new RobLsqIO) // rob to lsq
    }
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val csrUpdate = new DistributedCSRUpdateReq
    val error = new L1CacheErrorInfo
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }
  })

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  dcache.io.csr.distribute_csr <> io.csrCtrl.distribute_csr
  io.csrUpdate <> dcache.io.csr.update
  io.error <> RegNext(RegNext(dcache.io.error))

  val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StdExeUnit))
  val stData = stdExeUnits.map(_.stData.get)
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

  val ldExeWbReqs = ldOut0 +: loadUnits.tail.map(_.io.ldout)
  io.writeback <> ldExeWbReqs ++ VecInit(storeUnits.map(_.io.stout)) ++ VecInit(stdExeUnits.map(_.io.out))
  io.otherFastWakeup := DontCare
  io.otherFastWakeup.take(2).zip(loadUnits.map(_.io.fastUop)).foreach{case(a,b)=> a := b}

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrappper)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)
  io.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  // dtlb
  val sfence = RegNext(io.sfence)
  val tlbcsr = RegNext(io.tlbCsr)
  val dtlb_ld = VecInit(Seq.fill(exuParameters.LduCnt){
    val tlb_ld = Module(new TLB(1, ldtlbParams))
    tlb_ld.io // let the module have name in waveform
  })
  val dtlb_st = VecInit(Seq.fill(exuParameters.StuCnt){
    val tlb_st = Module(new TLB(1 , sttlbParams))
    tlb_st.io // let the module have name in waveform
  })
  dtlb_ld.map(_.sfence := sfence)
  dtlb_st.map(_.sfence := sfence)
  dtlb_ld.map(_.csr := tlbcsr)
  dtlb_st.map(_.csr := tlbcsr)
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(exuParameters.LduCnt + exuParameters.StuCnt, ldtlbParams))
    replace.io.apply_sep(dtlb_ld.map(_.replace) ++ dtlb_st.map(_.replace), io.ptw.resp.bits.data.entry.tag)
  } else {
    if (ldtlbParams.outReplace) {
      val replace_ld = Module(new TlbReplace(exuParameters.LduCnt, ldtlbParams))
      replace_ld.io.apply_sep(dtlb_ld.map(_.replace), io.ptw.resp.bits.data.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replace_st = Module(new TlbReplace(exuParameters.StuCnt, sttlbParams))
      replace_st.io.apply_sep(dtlb_st.map(_.replace), io.ptw.resp.bits.data.entry.tag)
    }
  }
  val dtlb = dtlb_ld ++ dtlb_st

  (dtlb_ld.map(_.ptw.req) ++ dtlb_st.map(_.ptw.req)).zipWithIndex.map{ case (tlb, i) =>
    tlb(0) <> io.ptw.req(i)
  }
  dtlb_ld.map(_.ptw.resp.bits := io.ptw.resp.bits.data)
  dtlb_st.map(_.ptw.resp.bits := io.ptw.resp.bits.data)
  if (refillBothTlb) {
    dtlb_ld.map(_.ptw.resp.valid := io.ptw.resp.valid && Cat(io.ptw.resp.bits.vector).orR)
    dtlb_st.map(_.ptw.resp.valid := io.ptw.resp.valid && Cat(io.ptw.resp.bits.vector).orR)
  } else {
    dtlb_ld.map(_.ptw.resp.valid := io.ptw.resp.valid && Cat(io.ptw.resp.bits.vector.take(exuParameters.LduCnt)).orR)
    dtlb_st.map(_.ptw.resp.valid := io.ptw.resp.valid && Cat(io.ptw.resp.bits.vector.drop(exuParameters.LduCnt)).orR)
  }
  io.ptw.resp.ready := true.B

  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> io.csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(exuParameters.LduCnt + exuParameters.StuCnt)(Module(new PMPChecker(3)).io))
  for ((p,d) <- pmp_check zip dtlb.map(_.pmp(0))) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }
  val tdata = Reg(Vec(6, new MatchTriggerIO))
  val tEnable = RegInit(VecInit(Seq.fill(6)(false.B)))
  val en = io.csrCtrl.trigger_enable
  tEnable := VecInit(en(2), en (3), en(7), en(4), en(5), en(9))
  when(io.csrCtrl.mem_trigger.t.valid) {
    tdata(io.csrCtrl.mem_trigger.t.bits.addr) := io.csrCtrl.mem_trigger.t.bits.tdata
  }
  val lTriggerMapping = Map(0 -> 4, 1 -> 5, 2 -> 9)
  val sTriggerMapping = Map(0 -> 2, 1 -> 3, 2 -> 7)
  val lChainMapping = Map(0 -> 2)
  val sChainMapping = Map(0 -> 1)

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect      <> io.redirect
    loadUnits(i).io.feedbackSlow  <> io.rsfeedback(i).feedbackSlow
    loadUnits(i).io.feedbackFast  <> io.rsfeedback(i).feedbackFast
    loadUnits(i).io.rsIdx         := io.rsfeedback(i).rsIdx
    loadUnits(i).io.isFirstIssue  := io.rsfeedback(i).isFirstIssue // NOTE: just for dtlb's perf cnt
    loadUnits(i).io.loadFastMatch <> io.loadFastMatch(i)
    // get input form dispatch
    loadUnits(i).io.ldin          <> io.issue(i)
    // dcache access
    loadUnits(i).io.dcache        <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward   <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.loadViolationQuery <> lsq.io.loadViolationQuery(i)
    loadUnits(i).io.csrCtrl <> io.csrCtrl
    // dtlb
    loadUnits(i).io.tlb           <> dtlb_ld(i).requestor(0)
    // pmp
    loadUnits(i).io.pmp           <> pmp_check(i).resp

    // laod to load fast forward
    for (j <- 0 until exuParameters.LduCnt) {
      loadUnits(i).io.fastpathIn(j)  <> loadUnits(j).io.fastpathOut
    }

    // Lsq to load unit's rs

    // passdown to lsq
    lsq.io.loadIn(i)              <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i)               <> loadUnits(i).io.lsq.ldout
    lsq.io.loadDataForwarded(i)   <> loadUnits(i).io.lsq.loadDataForwarded

    // update mem dependency predictor
    io.memPredUpdate(i) := DontCare
    lsq.io.needReplayFromRS(i)    <> loadUnits(i).io.lsq.needReplayFromRS

    // Trigger Regs
    // addr: 0-2 for store, 3-5 for load


    // TODO: load trigger, a total of 3
      for (j <- 0 until 10) {
        io.writeback(i).bits.uop.cf.trigger.triggerHitVec(j) := false.B
        io.writeback(i).bits.uop.cf.trigger.triggerTiming(j) := false.B
        if (lChainMapping.contains(j)) io.writeback(i).bits.uop.cf.trigger.triggerChainVec(j) := false.B
      }
    when(ldExeWbReqs(i).fire()){
      // load data, we need to delay cmp for 1 cycle for better timing
//      ldExeWbReqs(i).bits.data
      // TriggerCmp(ldExeWbReqs(i).bits.data, DontCare, DontCare, DontCare) 
      // load vaddr 
//      ldExeWbReqs(i).bits.debug.vaddr
      // TriggerCmp(ldExeWbReqs(i).bits.debug.vaddr, DontCare, DontCare, DontCare)
      for (j <- 0 until 3) {
        val hit = Mux(tdata(j+3).select, TriggerCmp(ldExeWbReqs(i).bits.data, tdata(j+3).tdata2, tdata(j+3).matchType, tEnable(j+3)),
          TriggerCmp(ldExeWbReqs(i).bits.debug.vaddr, tdata(j+3).tdata2, tdata(j+3).matchType, tEnable(j+3)))
        io.writeback(i).bits.uop.cf.trigger.triggerHitVec(lTriggerMapping(j)) := hit
        io.writeback(i).bits.uop.cf.trigger.triggerTiming(lTriggerMapping(j)) := hit && tdata(j+3).timing
        if (lChainMapping.contains(j)) io.writeback(i).bits.uop.cf.trigger.triggerChainVec(lChainMapping(j)) := hit && tdata(j+3).chain
      }
    }
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)

    stdExeUnits(i).io.redirect <> io.redirect
    stdExeUnits(i).io.fromInt <> io.issue(i + exuParameters.LduCnt + exuParameters.StuCnt)
    stdExeUnits(i).io.fromFp := DontCare
    stdExeUnits(i).io.out := DontCare

    stu.io.redirect     <> io.redirect
    stu.io.feedbackSlow <> io.rsfeedback(exuParameters.LduCnt + i).feedbackSlow
    stu.io.rsIdx        <> io.rsfeedback(exuParameters.LduCnt + i).rsIdx
    // NOTE: just for dtlb's perf cnt
    stu.io.isFirstIssue <> io.rsfeedback(exuParameters.LduCnt + i).isFirstIssue
    stu.io.stin         <> io.issue(exuParameters.LduCnt + i)
    stu.io.lsq          <> lsq.io.storeIn(i)
    stu.io.lsq_replenish <> lsq.io.storeInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_st(i).requestor(0)
    stu.io.pmp          <> pmp_check(i+exuParameters.LduCnt).resp

    // store unit does not need fast feedback
    io.rsfeedback(exuParameters.LduCnt + i).feedbackFast := DontCare

    // Lsq to load unit's rs
    lsq.io.storeDataIn(i) := stData(i)

    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits

    io.stOut(i).valid := stu.io.stout.valid
    io.stOut(i).bits  := stu.io.stout.bits
    stu.io.stout.ready := true.B

    // TODO: debug trigger
    // store vaddr 
    when(io.stOut(i).fire()){
      io.stOut(i).bits.debug.vaddr
      // TriggerCmp(io.stOut(i).bits.debug.vaddr, DontCare, DontCare, DontCare)
      for (j <- 0 until 10) {
          io.stOut(i).bits.uop.cf.trigger.triggerHitVec(j) := false.B
          io.stOut(i).bits.uop.cf.trigger.triggerTiming(j) := false.B
          if (sChainMapping.contains(j)) io.stOut(i).bits.uop.cf.trigger.triggerChainVec(j) := false.B
      }
      for (j <- 0 until 3) {
        when(!tdata(j).select) {
          val hit = TriggerCmp(io.stOut(i).bits.data, tdata(j).tdata2, tdata(j).matchType, tEnable(j))
          io.stOut(i).bits.uop.cf.trigger.triggerHitVec(sTriggerMapping(j)) := hit
          io.stOut(i).bits.uop.cf.trigger.triggerTiming(sTriggerMapping(j)) := hit && tdata(j + 3).timing
          if (sChainMapping.contains(j)) io.stOut(i).bits.uop.cf.trigger.triggerChainVec(sChainMapping(j)) := hit && tdata(j + 3).chain
        }
      }
    }
    // store data
    when(lsq.io.storeDataIn(i).fire()){
      lsq.io.storeDataIn(i).bits.data(XLEN-1, 0)
      for (j <- 0 until 10) {
          lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerHitVec(j) := false.B
          lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerTiming(j) := false.B
          if (sChainMapping.contains(j)) lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerChainVec(j) := false.B
      }
      // TriggerCmp(lsq.io.storeDataIn(i).bits.data(XLEN-1, 0), DontCare, DontCare, DontCare)
      for (j <- 0 until 3) {
        when(tdata(j).select) {
          val hit = TriggerCmp(lsq.io.storeDataIn(i).bits.data, tdata(j).tdata2, tdata(j).matchType, tEnable(j))
          lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerHitVec(sTriggerMapping(j)) := hit
          lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerTiming(sTriggerMapping(j)) := hit && tdata(j + 3).timing
          if (sChainMapping.contains(j)) lsq.io.storeDataIn(i).bits.uop.cf.trigger.triggerChainVec(sChainMapping(j)) := hit && tdata(j + 3).chain
        }
      }
    }
  }

  // mmio store writeback will use store writeback port 0
  lsq.io.mmioStout.ready := false.B
  when (lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid) {
    io.stOut(0).valid := true.B
    io.stOut(0).bits  := lsq.io.mmioStout.bits
    lsq.io.mmioStout.ready := true.B
  }

  // Lsq
  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.enqLsq
  lsq.io.brqRedirect    <> io.redirect
  io.memoryViolation    <> lsq.io.rollback
  lsq.io.uncache        <> uncache.io.lsq
  // delay dcache refill for 1 cycle for better timing
  // TODO: remove RegNext after fixing refill paddr timing
  // lsq.io.dcache         <> dcache.io.lsu.lsq
  lsq.io.dcache         := RegNext(dcache.io.lsu.lsq)
  lsq.io.release        := dcache.io.lsu.release

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqempty        <> sbuffer.io.sqempty

  // Sbuffer
  sbuffer.io.csrCtrl    <> RegNext(io.csrCtrl)
  sbuffer.io.dcache     <> dcache.io.lsu.store
  // TODO: if dcache sbuffer resp needs to ne delayed 
  // sbuffer.io.dcache.pipe_resp.valid := RegNext(dcache.io.lsu.store.pipe_resp.valid)
  // sbuffer.io.dcache.pipe_resp.bits := RegNext(dcache.io.lsu.store.pipe_resp.bits)

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

  val st0_data_atomics = stData(0).valid && FuType.storeIsAMO(stData(0).bits.uop.ctrl.fuType)
  val st1_data_atomics = stData(1).valid && FuType.storeIsAMO(stData(1).bits.uop.ctrl.fuType)

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
  atomicsUnit.io.storeDataIn.bits  := Mux(st0_data_atomics, stData(0).bits, stData(1).bits)
  atomicsUnit.io.rsIdx    := Mux(st0_atomics, io.rsfeedback(atomic_rs0).rsIdx, io.rsfeedback(atomic_rs1).rsIdx)
  atomicsUnit.io.redirect <> io.redirect

  // TODO: complete amo's pmp support
  val amoTlb = dtlb_ld(0).requestor(0)
  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := amoTlb.req.ready
  atomicsUnit.io.pmpResp := pmp_check(0).resp

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbuffer.io.flush.empty

  // for atomicsUnit, it uses loadUnit(0)'s TLB port

  when (state === s_atomics_0 || state === s_atomics_1) {
    loadUnits(0).io.ldout.ready := false.B
    atomicsUnit.io.dtlb <> amoTlb

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.ldout.valid)
  }

  when (state === s_atomics_0) {
    atomicsUnit.io.feedbackSlow <> io.rsfeedback(atomic_rs0).feedbackSlow

    assert(!storeUnits(0).io.feedbackSlow.valid)
  }
  when (state === s_atomics_1) {
    atomicsUnit.io.feedbackSlow <> io.rsfeedback(atomic_rs1).feedbackSlow

    assert(!storeUnits(1).io.feedbackSlow.valid)
  }

  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  // Address is delayed by one cycle, so does the atomics address
  val atomicsException = RegNext(atomicsUnit.io.exceptionAddr.valid)
  val atomicsExceptionAddress = RegNext(atomicsUnit.io.exceptionAddr.bits)
  io.lsqio.exceptionAddr.vaddr := Mux(atomicsException, atomicsExceptionAddress, lsq.io.exceptionAddr.vaddr)

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

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)
  val perfinfo = IO(new Bundle(){
    val perfEvents        = Output(new PerfEventsBundle(csrevents.length))
    val perfEventsPTW     = Input(new PerfEventsBundle(19))
  })
  val perfEvents_list = Wire(new PerfEventsBundle(2))
  val perfEvents = Seq(
    ("ldDeqCount                   ", ldDeqCount      ),
    ("stDeqCount                   ", stDeqCount      ),
  ) 
  for (((perf_out,(perf_name,perf)),i) <- perfEvents_list.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }

  if(print_perfcounter){
    val ldu0_perf     = loadUnits(0).perfEvents.map(_._1).zip(loadUnits(0).perfinfo.perfEvents.perf_events)
    val ldu1_perf     = loadUnits(1).perfEvents.map(_._1).zip(loadUnits(1).perfinfo.perfEvents.perf_events)
    val sbuf_perf     = sbuffer.perfEvents.map(_._1).zip(sbuffer.perfinfo.perfEvents.perf_events)
    val lsq_perf      = lsq.perfEvents.map(_._1).zip(lsq.perfinfo.perfEvents.perf_events)
    val dc_perf       = dcache.perfEvents.map(_._1).zip(dcache.perfinfo.perfEvents.perf_events)
    val mem_perf = perfEvents ++ ldu0_perf ++ ldu1_perf ++ sbuf_perf ++ lsq_perf ++ dc_perf
    for (((perf_name,perf),i) <- mem_perf.zipWithIndex) {
      println(s"lsu perf $i: $perf_name")
    }
  }

  val hpmEvents = perfEvents_list.perf_events ++ loadUnits(0).perfinfo.perfEvents.perf_events ++ 
                  loadUnits(1).perfinfo.perfEvents.perf_events ++ sbuffer.perfinfo.perfEvents.perf_events ++ 
                  lsq.perfinfo.perfEvents.perf_events ++ dcache.perfinfo.perfEvents.perf_events ++ 
                  perfinfo.perfEventsPTW.perf_events
  val perf_length = hpmEvents.length

  val hpm_lsu = Module(new HPerfmonitor(perf_length,csrevents.length))
  hpm_lsu.io.hpm_event := csrevents
  hpm_lsu.io.events_sets.perf_events := hpmEvents
  perfinfo.perfEvents := RegNext(hpm_lsu.io.events_selected)
}
