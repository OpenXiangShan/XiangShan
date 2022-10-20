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
import utility._
import xiangshan._
import xiangshan.backend.exu.StdExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.cache.mmu.{VectorTlbPtwIO, TLBNonBlock, TlbReplace}
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
    val loadFastImm = Vec(exuParameters.LduCnt, Input(UInt(12.W)))
    val rsfeedback = Vec(exuParameters.StuCnt, new MemRSFeedbackIO)
    val stIssuePtr = Output(new SqPtr())
    val int2vlsu = Flipped(new Int2VLSUIO)
    val vec2vlsu = Flipped(new Vec2VLSUIO)
    // out
    val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuOutput))
    val s3_delayed_load_error = Vec(exuParameters.LduCnt, Output(Bool()))
    val otherFastWakeup = Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new MicroOp))
    val vlsu2vec = new VLSU2VecIO
    val vlsu2int = new VLSU2IntIO
    val vlsu2ctrl = new VLSU2CtrlIO
    // prefetch to l1 req
    val prefetch_req = Flipped(DecoupledIO(new L1PrefetchReq))
    // misc
    val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
    val memoryViolation = ValidIO(new Redirect)
    val ptw = new VectorTlbPtwIO(exuParameters.LduCnt + exuParameters.StuCnt)
    val sfence = Input(new SfenceBundle)
    val tlbCsr = Input(new TlbCsrBundle)
    val fenceToSbuffer = Flipped(new FenceToSbuffer)
    val enqLsq = new LsqEnqIO
    // val memPredUpdate = Vec(exuParameters.StuCnt, Input(new MemPredUpdateReq))
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
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val perfEventsPTW = Input(Vec(19, new PerfEvent))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  })

  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.writeback))

  val redirect = RegNextWithEnable(io.redirect)

  val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  val delayedDcacheRefill = RegNext(dcache.io.lsu.lsq)

  val csrCtrl = DelayN(io.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  io.csrUpdate := RegNext(dcache.io.csr.update)
  io.error <> RegNext(RegNext(dcache.io.error))
  when(!csrCtrl.cache_error_enable){
    io.error.report_to_beu := false.B
    io.error.valid := false.B
  }

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

  // prefetch to l1 req
  loadUnits.map(load_unit => {
    load_unit.io.prefetch_req.valid <> io.prefetch_req.valid
    load_unit.io.prefetch_req.bits <> io.prefetch_req.bits
  })
  // when loadUnits(0) stage 0 is busy, hw prefetch will never use that pipeline
  loadUnits(0).io.prefetch_req.bits.confidence := 0.U

  io.prefetch_req.ready := (io.prefetch_req.bits.confidence > 0.U) ||
    loadUnits.map(!_.io.ldin.valid).reduce(_ || _)

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrappper)
  val vlsq    = Module(new DummyVectorLsq)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer) // out of date now
  io.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  // dtlb
  val sfence = RegNext(RegNext(io.sfence))
  val tlbcsr = RegNext(RegNext(io.tlbCsr))
  val dtlb_ld = VecInit(Seq.fill(1){
    val tlb_ld = Module(new TLBNonBlock(exuParameters.LduCnt, 2, ldtlbParams))
    tlb_ld.io // let the module have name in waveform
  })
  val dtlb_st = VecInit(Seq.fill(1){
    val tlb_st = Module(new TLBNonBlock(exuParameters.StuCnt, 1, sttlbParams))
    tlb_st.io // let the module have name in waveform
  })
  val dtlb = dtlb_ld ++ dtlb_st
  val dtlb_reqs = dtlb.map(_.requestor).flatten
  val dtlb_pmps = dtlb.map(_.pmp).flatten
  dtlb.map(_.sfence := sfence)
  dtlb.map(_.csr := tlbcsr)
  dtlb.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
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

  val ptw_resp_next = RegEnable(io.ptw.resp.bits, io.ptw.resp.valid)
  val ptw_resp_v = RegNext(io.ptw.resp.valid && !(sfence.valid && tlbcsr.satp.changed), init = false.B)
  io.ptw.resp.ready := true.B

  (dtlb.map(a => a.ptw.req.map(b => b)))
    .flatten
    .zipWithIndex
    .map{ case (tlb, i) =>
    tlb <> io.ptw.req(i)
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < exuParameters.LduCnt) Cat(ptw_resp_next.vector.take(exuParameters.LduCnt)).orR
      else Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt)).orR
    io.ptw.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit &&
      ptw_resp_next.data.entry.hit(tlb.bits.vpn, tlbcsr.satp.asid, allType = true, ignoreAsid = true))
  }
  dtlb.map(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.take(exuParameters.LduCnt)).orR)
    dtlb_st.map(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt)).orR)
  }


  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(exuParameters.LduCnt + exuParameters.StuCnt)(Module(new PMPChecker(3)).io))
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }
  val pmp_check_ptw = Module(new PMPCheckerv2(lgMaxSize = 3, sameCycle = false, leaveHitMux = true))
  pmp_check_ptw.io.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, io.ptw.resp.valid,
    Cat(io.ptw.resp.bits.data.entry.ppn, 0.U(12.W)).asUInt)
  dtlb.map(_.ptw_replenish := pmp_check_ptw.io.resp)

  val tdata = RegInit(VecInit(Seq.fill(6)(0.U.asTypeOf(new MatchTriggerIO))))
  val tEnable = RegInit(VecInit(Seq.fill(6)(false.B)))
  val en = csrCtrl.trigger_enable
  tEnable := VecInit(en(2), en (3), en(4), en(5), en(7), en(9))
  when(csrCtrl.mem_trigger.t.valid) {
    tdata(csrCtrl.mem_trigger.t.bits.addr) := csrCtrl.mem_trigger.t.bits.tdata
  }
  val lTriggerMapping = Map(0 -> 2, 1 -> 3, 2 -> 5)
  val sTriggerMapping = Map(0 -> 0, 1 -> 1, 2 -> 4)
  val lChainMapping = Map(0 -> 2)
  val sChainMapping = Map(0 -> 1)
  XSDebug(tEnable.asUInt.orR, "Debug Mode: At least one store trigger is enabled\n")
  for(j <- 0 until 3)
    PrintTriggerInfo(tEnable(j), tdata(j))

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect <> redirect
    loadUnits(i).io.rsIdx := DontCare
    loadUnits(i).io.isFirstIssue := DontCare
    // get input form dispatch
    loadUnits(i).io.ldin <> io.issue(i)
    // dcache access
    loadUnits(i).io.dcache <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    loadUnits(i).io.tlDchannel := dcache.io.lsu.forward_D(i)
    loadUnits(i).io.forward_mshr <> dcache.io.lsu.forward_mshr(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.loadViolationQuery <> lsq.io.loadViolationQuery(i)
    loadUnits(i).io.csrCtrl       <> csrCtrl
    // dcache refill req
    loadUnits(i).io.refill           <> delayedDcacheRefill
    // dtlb
    loadUnits(i).io.tlb <> dtlb_reqs.take(exuParameters.LduCnt)(i)
    // pmp
    loadUnits(i).io.pmp <> pmp_check(i).resp
    // st-ld violation query 
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.reExecuteQuery(s) := storeUnits(s).io.reExecuteQuery
    }
    // load to load fast forward: load(i) prefers data(i)
    val fastPriority = (i until exuParameters.LduCnt) ++ (0 until i)
    val fastValidVec = fastPriority.map(j => loadUnits(j).io.fastpathOut.valid)
    val fastDataVec = fastPriority.map(j => loadUnits(j).io.fastpathOut.data)
    val fastMatchVec = fastPriority.map(j => io.loadFastMatch(i)(j))
    loadUnits(i).io.fastpathIn.valid := VecInit(fastValidVec).asUInt.orR
    loadUnits(i).io.fastpathIn.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    loadUnits(i).io.loadFastMatch := fastMatch
    loadUnits(i).io.loadFastImm := io.loadFastImm(i)

    // Lsq to load unit's rs

    // passdown to lsq (load s1)
    lsq.io.loadPaddrIn(i) <> loadUnits(i).io.lsq.loadPaddrIn
    lsq.io.loadVaddrIn(i) <> loadUnits(i).io.lsq.loadVaddrIn

    lsq.io.replayFast(i) := loadUnits(i).io.lsq.replayFast
    lsq.io.replaySlow(i) := loadUnits(i).io.lsq.replaySlow

    loadUnits(i).io.lsqOut       <> lsq.io.loadOut(i)

    // passdown to lsq (load s2)
    lsq.io.loadIn(i) <> loadUnits(i).io.lsq.loadIn
    lsq.io.ldout(i) <> loadUnits(i).io.lsq.ldout
    lsq.io.ldRawDataOut(i) <> loadUnits(i).io.lsq.ldRawData
    lsq.io.s2_load_data_forwarded(i) <> loadUnits(i).io.lsq.s2_load_data_forwarded
    lsq.io.trigger(i) <> loadUnits(i).io.lsq.trigger

    // passdown to lsq (load s3)
    lsq.io.s2_dcache_require_replay(i) <> loadUnits(i).io.lsq.s2_dcache_require_replay
    lsq.io.s3_replay_from_fetch(i) <> loadUnits(i).io.lsq.s3_replay_from_fetch
    lsq.io.s3_delayed_load_error(i) <> loadUnits(i).io.s3_delayed_load_error

    // alter writeback exception info
    io.s3_delayed_load_error(i) := loadUnits(i).io.lsq.s3_delayed_load_error

    // update mem dependency predictor
    // io.memPredUpdate(i) := DontCare

    // --------------------------------
    // Load Triggers
    // --------------------------------
    val hit = Wire(Vec(3, Bool()))
    for (j <- 0 until 3) {
      loadUnits(i).io.trigger(j).tdata2 := tdata(lTriggerMapping(j)).tdata2
      loadUnits(i).io.trigger(j).matchType := tdata(lTriggerMapping(j)).matchType
      loadUnits(i).io.trigger(j).tEnable := tEnable(lTriggerMapping(j))
      // Just let load triggers that match data unavailable
      hit(j) := loadUnits(i).io.trigger(j).addrHit && !tdata(lTriggerMapping(j)).select // Mux(tdata(j + 3).select, loadUnits(i).io.trigger(j).lastDataHit, loadUnits(i).io.trigger(j).addrHit)
      io.writeback(i).bits.uop.cf.trigger.backendHit(lTriggerMapping(j)) := hit(j)
//      io.writeback(i).bits.uop.cf.trigger.backendTiming(lTriggerMapping(j)) := tdata(lTriggerMapping(j)).timing
      //      if (lChainMapping.contains(j)) io.writeback(i).bits.uop.cf.trigger.triggerChainVec(lChainMapping(j)) := hit && tdata(j+3).chain
    }
    when(tdata(2).chain) {
      io.writeback(i).bits.uop.cf.trigger.backendHit(2) := hit(0) && hit(1)
      io.writeback(i).bits.uop.cf.trigger.backendHit(3) := hit(0) && hit(1)
    }
    when(!io.writeback(i).bits.uop.cf.trigger.backendEn(1)) {
      io.writeback(i).bits.uop.cf.trigger.backendHit(5) := false.B
    }

    XSDebug(io.writeback(i).bits.uop.cf.trigger.getHitBackend && io.writeback(i).valid, p"Debug Mode: Load Inst No.${i}" +
    p"has trigger hit vec ${io.writeback(i).bits.uop.cf.trigger.backendHit}\n")

  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)

    stdExeUnits(i).io.redirect <> redirect
    stdExeUnits(i).io.fromInt <> io.issue(i + exuParameters.LduCnt + exuParameters.StuCnt)
    stdExeUnits(i).io.fromFp := DontCare
    stdExeUnits(i).io.out := DontCare

    stu.io.redirect     <> redirect
    stu.io.feedbackSlow <> io.rsfeedback(i).feedbackSlow
    stu.io.rsIdx        <> io.rsfeedback(i).rsIdx
    // NOTE: just for dtlb's perf cnt
    stu.io.isFirstIssue <> io.rsfeedback(i).isFirstIssue
    stu.io.stin         <> io.issue(exuParameters.LduCnt + i)
    stu.io.lsq          <> lsq.io.storeIn(i)
    stu.io.lsq_replenish <> lsq.io.storeInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_reqs.drop(exuParameters.LduCnt)(i)
    stu.io.pmp          <> pmp_check(i+exuParameters.LduCnt).resp

    // store unit does not need fast feedback
    io.rsfeedback(i).feedbackFast := DontCare

    // Lsq to sta unit
    lsq.io.storeMaskIn(i) <> stu.io.storeMaskOut

    // Lsq to std unit's rs
    lsq.io.storeDataIn(i) := stData(i)


    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    // io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    // io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits
    io.stIn(i).valid := stu.io.issue.valid 
    io.stIn(i).bits := stu.io.issue.bits 

    stu.io.stout.ready := true.B

    // -------------------------
    // Store Triggers
    // -------------------------
    when(stOut(i).fire()){
      val hit = Wire(Vec(3, Bool()))
      for (j <- 0 until 3) {
         hit(j) := !tdata(sTriggerMapping(j)).select && TriggerCmp(
           stOut(i).bits.debug.vaddr,
           tdata(sTriggerMapping(j)).tdata2,
           tdata(sTriggerMapping(j)).matchType,
           tEnable(sTriggerMapping(j))
         )
       stOut(i).bits.uop.cf.trigger.backendHit(sTriggerMapping(j)) := hit(j)
     }

     when(tdata(0).chain) {
       io.writeback(i).bits.uop.cf.trigger.backendHit(0) := hit(0) && hit(1)
       io.writeback(i).bits.uop.cf.trigger.backendHit(1) := hit(0) && hit(1)
     }

     when(!stOut(i).bits.uop.cf.trigger.backendEn(0)) {
       stOut(i).bits.uop.cf.trigger.backendHit(4) := false.B
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

  // atomic exception / trigger writeback
  when (atomicsUnit.io.out.valid) {
    // atom inst will use store writeback port 0 to writeback exception info
    stOut(0).valid := true.B
    stOut(0).bits  := atomicsUnit.io.out.bits
    assert(!lsq.io.mmioStout.valid && !storeUnits(0).io.stout.valid)

    // when atom inst writeback, surpress normal load trigger
    (0 until exuParameters.LduCnt).map(i => {
      io.writeback(i).bits.uop.cf.trigger.backendHit := VecInit(Seq.fill(6)(false.B))
    })
  }
  
  // Uncahce
  uncache.io.enableOutstanding := io.csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.hartId
  lsq.io.uncacheOutstanding := io.csrCtrl.uncache_write_outstanding_enable

  // Lsq
  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.enqLsq
  lsq.io.brqRedirect    <> redirect
  io.memoryViolation    <> lsq.io.rollback
  // lsq.io.uncache        <> uncache.io.lsq
  AddPipelineReg(lsq.io.uncache.req, uncache.io.lsq.req, false.B)
  AddPipelineReg(uncache.io.lsq.resp, lsq.io.uncache.resp, false.B)
  // delay dcache refill for 1 cycle for better timing
  lsq.io.refill         := delayedDcacheRefill
  lsq.io.release        := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.lqCancelCnt
  lsq.io.sqCancelCnt <> io.sqCancelCnt
  lsq.io.sqDeq <> io.sqDeq

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqempty        <> sbuffer.io.sqempty

  // Sbuffer
  sbuffer.io.csrCtrl    <> csrCtrl
  sbuffer.io.dcache     <> dcache.io.lsu.store

  // flush sbuffer
  val fenceFlush = io.fenceToSbuffer.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.fenceToSbuffer.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // Vector Load/Store Queue
  vlsq.io.int2vlsu <> io.int2vlsu
  vlsq.io.vec2vlsu <> io.vec2vlsu
  vlsq.io.vlsu2vec <> io.vlsu2vec
  vlsq.io.vlsu2int <> io.vlsu2int
  vlsq.io.vlsu2ctrl <> io.vlsu2ctrl

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(exuParameters.StuCnt + 1)
  val state = RegInit(s_normal)

  val atomic_rs = (0 until exuParameters.StuCnt).map(exuParameters.LduCnt + _)
  val atomic_replay_port_idx = (0 until exuParameters.StuCnt)
  val st_atomics = Seq.tabulate(exuParameters.StuCnt)(i =>
    io.issue(atomic_rs(i)).valid && FuType.storeIsAMO((io.issue(atomic_rs(i)).bits.uop.ctrl.fuType))
  )

  val st_data_atomics = Seq.tabulate(exuParameters.StuCnt)(i =>
    stData(i).valid && FuType.storeIsAMO(stData(i).bits.uop.ctrl.fuType)
  )

  for (i <- 0 until exuParameters.StuCnt) when(st_atomics(i)) {
    io.issue(atomic_rs(i)).ready := atomicsUnit.io.in.ready
    storeUnits(i).io.stin.valid := false.B

    state := s_atomics(i)
    if (exuParameters.StuCnt > 1)
      assert(!st_atomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _))
  }
  when (atomicsUnit.io.out.valid) {
    assert((0 until exuParameters.StuCnt).map(state === s_atomics(_)).reduce(_ || _))
    state := s_normal
  }

  atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
  atomicsUnit.io.in.bits  := Mux1H(Seq.tabulate(exuParameters.StuCnt)(i =>
    st_atomics(i) -> io.issue(atomic_rs(i)).bits))
  atomicsUnit.io.storeDataIn.valid := st_data_atomics.reduce(_ || _)
  atomicsUnit.io.storeDataIn.bits  := Mux1H(Seq.tabulate(exuParameters.StuCnt)(i =>
    st_data_atomics(i) -> stData(i).bits))
  atomicsUnit.io.rsIdx    := Mux1H(Seq.tabulate(exuParameters.StuCnt)(i =>
    st_atomics(i) -> io.rsfeedback(atomic_replay_port_idx(i)).rsIdx))
  atomicsUnit.io.redirect <> redirect

  // TODO: complete amo's pmp support
  val amoTlb = dtlb_ld(0).requestor(0)
  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.dtlb.req.ready  := amoTlb.req.ready
  atomicsUnit.io.pmpResp := pmp_check(0).resp

  atomicsUnit.io.dcache <> dcache.io.lsu.atomics
  atomicsUnit.io.flush_sbuffer.empty := stIsEmpty

  atomicsUnit.io.csrCtrl := csrCtrl

  // for atomicsUnit, it uses loadUnit(0)'s TLB port

  when (state === s_atomics_0 || state === s_atomics_1) {
    // use store wb port instead of load
    loadUnits(0).io.ldout.ready := false.B
    // use load_0's TLB
    atomicsUnit.io.dtlb <> amoTlb

    // hw prefetch should be disabled while executing atomic insts
    loadUnits.map(i => i.io.prefetch_req.valid := false.B)

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.ldout.valid)
  }

  for (i <- 0 until exuParameters.StuCnt) when (state === s_atomics(i)) {
    atomicsUnit.io.feedbackSlow <> io.rsfeedback(atomic_replay_port_idx(i)).feedbackSlow

    assert(!storeUnits(i).io.feedbackSlow.valid)
  }

  lsq.io.exceptionAddr.isStore := io.lsqio.exceptionAddr.isStore
  // Exception address is used several cycles after flush.
  // We delay it by 10 cycles to ensure its flush safety.
  val atomicsException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (atomicsUnit.io.exceptionAddr.valid) {
    atomicsException := true.B
  }
  val atomicsExceptionAddress = RegEnable(atomicsUnit.io.exceptionAddr.bits, atomicsUnit.io.exceptionAddr.valid)
  io.lsqio.exceptionAddr.vaddr := RegNext(Mux(atomicsException, atomicsExceptionAddress, lsq.io.exceptionAddr.vaddr))
  XSError(atomicsException && atomicsUnit.io.in.valid, "new instruction before exception triggers\n")

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  io.lqFull := lsq.io.lqFull
  io.sqFull := lsq.io.sqFull

  val ldDeqCount = PopCount(io.issue.take(exuParameters.LduCnt).map(_.valid))
  val stDeqCount = PopCount(io.issue.drop(exuParameters.LduCnt).map(_.valid))
  val rsDeqCount = ldDeqCount + stDeqCount
  XSPerfAccumulate("load_rs_deq_count", ldDeqCount)
  XSPerfHistogram("load_rs_deq_count", ldDeqCount, true.B, 0, exuParameters.LduCnt, 1)
  XSPerfAccumulate("store_rs_deq_count", stDeqCount)
  XSPerfHistogram("store_rs_deq_count", stDeqCount, true.B, 0, exuParameters.StuCnt, 1)
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
