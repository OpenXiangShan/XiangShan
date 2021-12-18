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
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import utils._
import xiangshan._
import xiangshan.backend.exu.StdExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{BTlbPtwIO, TLB, TlbReplace}
import xiangshan.mem._

class Std(implicit p: Parameters) extends FunctionUnit {
  io.in.ready := true.B
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := io.in.bits.src(0)
}

class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter with HasWritebackSource {

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())

  lazy val module = new MemBlockImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = (loadExuConfigs ++ storeExuConfigs).map(cfg => Seq(cfg))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class MemBlockImp(outer: MemBlock) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasWritebackSourceImp
  with HasPerfEvents
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val redirect = Flipped(ValidIO(new Redirect))
    // in
    val issue = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuInput)))
    val loadFastMatch = Vec(exuParameters.LduCnt, Input(UInt(exuParameters.LduCnt.W)))
    val rsfeedback = Vec(exuParameters.LsExuCnt, new MemRSFeedbackIO)
    val stIssuePtr = Output(new SqPtr())
    // out
    val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuOutput))
    val otherFastWakeup = Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new MicroOp))
    // misc
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
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
    val perfEventsPTW = Input(Vec(19, new PerfEvent))
  })

  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.writeback))

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  val csrCtrl = DelayN(io.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  io.csrUpdate := RegNext(dcache.io.csr.update)
  io.error <> RegNext(RegNext(dcache.io.error))

  val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StdExeUnit))
  val stData = stdExeUnits.map(_.io.out)
  val exeUnits = loadUnits ++ storeUnits

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))

  val atomicsUnit = Module(new AtomicsUnit)

  // Atom inst comes from sta / std, then its result 
  // will be writebacked using load writeback port
  // 
  // However, atom exception will be writebacked to rob
  // using store writeback port

  val loadWritebackOverride  = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out.bits, loadUnits.head.io.ldout.bits)
  val ldOut0 = Wire(Decoupled(new ExuOutput))
  ldOut0.valid := atomicsUnit.io.out.valid || loadUnits.head.io.ldout.valid
  ldOut0.bits  := loadWritebackOverride
  atomicsUnit.io.out.ready := ldOut0.ready
  loadUnits.head.io.ldout.ready := ldOut0.ready
  when(atomicsUnit.io.out.valid){
    ldOut0.bits.uop.cf.exceptionVec := 0.U(16.W).asBools // exception will be writebacked via store wb port
  }

  val ldExeWbReqs = ldOut0 +: loadUnits.tail.map(_.io.ldout)
  io.writeback <> ldExeWbReqs ++ VecInit(storeUnits.map(_.io.stout)) ++ VecInit(stdExeUnits.map(_.io.out))
  io.otherFastWakeup := DontCare
  io.otherFastWakeup.take(2).zip(loadUnits.map(_.io.fastUop)).foreach{case(a,b)=> a := b}
  val stOut = io.writeback.drop(exuParameters.LduCnt).dropRight(exuParameters.StuCnt)

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

  val ptw_resp_next = RegEnable(io.ptw.resp.bits, io.ptw.resp.valid)
  val ptw_resp_v = RegNext(io.ptw.resp.valid && !(sfence.valid && tlbcsr.satp.changed), init = false.B)
  io.ptw.resp.ready := true.B

  (dtlb_ld.map(_.ptw.req) ++ dtlb_st.map(_.ptw.req)).zipWithIndex.map{ case (tlb, i) =>
    tlb(0) <> io.ptw.req(i)
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < exuParameters.LduCnt) Cat(ptw_resp_next.vector.take(exuParameters.LduCnt)).orR
      else Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt)).orR
    io.ptw.req(i).valid := tlb(0).valid && !(ptw_resp_v && vector_hit &&
      ptw_resp_next.data.entry.hit(tlb(0).bits.vpn, tlbcsr.satp.asid, allType = true, ignoreAsid = true))
  }
  dtlb_ld.map(_.ptw.resp.bits := ptw_resp_next.data)
  dtlb_st.map(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb_ld.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
    dtlb_st.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.take(exuParameters.LduCnt)).orR)
    dtlb_st.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt)).orR)
  }


  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(exuParameters.LduCnt + exuParameters.StuCnt)(Module(new PMPChecker(3)).io))
  for ((p,d) <- pmp_check zip dtlb.map(_.pmp(0))) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }
  val pmp_check_ptw = Module(new PMPCheckerv2(lgMaxSize = 3, sameCycle = false, leaveHitMux = true))
  pmp_check_ptw.io.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, io.ptw.resp.valid,
    Cat(io.ptw.resp.bits.data.entry.ppn, 0.U(12.W)).asUInt)
  dtlb_ld.map(_.ptw_replenish := pmp_check_ptw.io.resp)
  dtlb_st.map(_.ptw_replenish := pmp_check_ptw.io.resp)

  val tdata = Reg(Vec(6, new MatchTriggerIO))
  val tEnable = RegInit(VecInit(Seq.fill(6)(false.B)))
  val en = csrCtrl.trigger_enable
  tEnable := VecInit(en(2), en (3), en(7), en(4), en(5), en(9))
  when(csrCtrl.mem_trigger.t.valid) {
    tdata(csrCtrl.mem_trigger.t.bits.addr) := csrCtrl.mem_trigger.t.bits.tdata
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
    loadUnits(i).io.csrCtrl       <> csrCtrl
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

    stu.io.stout.ready := true.B

    // TODO: debug trigger
    // store vaddr 
    when (stOut(i).fire()) {
      for (j <- 0 until 10) {
          stOut(i).bits.uop.cf.trigger.triggerHitVec(j) := false.B
          stOut(i).bits.uop.cf.trigger.triggerTiming(j) := false.B
          if (sChainMapping.contains(j)) stOut(i).bits.uop.cf.trigger.triggerChainVec(j) := false.B
      }
      for (j <- 0 until 3) {
        when(!tdata(j).select) {
          val hit = TriggerCmp(stOut(i).bits.data, tdata(j).tdata2, tdata(j).matchType, tEnable(j))
          stOut(i).bits.uop.cf.trigger.triggerHitVec(sTriggerMapping(j)) := hit
          stOut(i).bits.uop.cf.trigger.triggerTiming(sTriggerMapping(j)) := hit && tdata(j + 3).timing
          if (sChainMapping.contains(j)) stOut(i).bits.uop.cf.trigger.triggerChainVec(sChainMapping(j)) := hit && tdata(j + 3).chain
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
    stOut(0).valid := true.B
    stOut(0).bits  := lsq.io.mmioStout.bits
    lsq.io.mmioStout.ready := true.B
  }

  // atom inst will use store writeback port 0 to writeback exception info
  when (atomicsUnit.io.out.valid) {
    stOut(0).valid := true.B
    stOut(0).bits  := atomicsUnit.io.out.bits
    assert(!lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid)
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
  sbuffer.io.csrCtrl    <> csrCtrl
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

  atomicsUnit.io.csrCtrl := csrCtrl

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
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val memBlockPerfEvents = Seq(
    ("ldDeqCount", ldDeqCount),
    ("stDeqCount", stDeqCount),
  )
  val allPerfEvents = memBlockPerfEvents ++ (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val hpmEvents = allPerfEvents.map(_._2.asTypeOf(new PerfEvent)) ++ io.perfEventsPTW
  val perfEvents = HPerfMonitor(csrevents, hpmEvents).getPerfEvents
  generatePerfEvent()
}
