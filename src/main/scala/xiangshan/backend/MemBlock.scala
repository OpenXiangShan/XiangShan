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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.PrefetchRecv
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink.TLBuffer
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.exu.MemExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.prefetch.{BasePrefecher, L1Prefetcher, SMSParams, SMSPrefetcher}

trait HasMemBlockParameters extends HasXSParameter {
  // number of memory units
  val LduCnt  = backendParams.LduCnt
  val StaCnt  = backendParams.StaCnt
  val StdCnt  = backendParams.StdCnt
  val HyuCnt  = backendParams.HyuCnt
  val VlduCnt = backendParams.VlduCnt
  val VstuCnt = backendParams.VstuCnt

  val LdExuCnt  = LduCnt + HyuCnt
  val StAddrCnt = StaCnt + HyuCnt
  val StDataCnt = StdCnt
  val MemExuCnt = LduCnt + StaCnt + StdCnt + HyuCnt
  val MemAddrExtCnt = LdExuCnt + StaCnt
  val MemVExuCnt = VlduCnt + VstuCnt
}

abstract class MemBlockBundle(implicit val p: Parameters) extends Bundle with HasMemBlockParameters

class Std(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits := 0.U.asTypeOf(io.out.bits)
  io.out.bits.res.data := io.in.bits.data.src(0)
  io.out.bits.ctrl.robIdx := io.in.bits.ctrl.robIdx
}

class ooo_to_mem(implicit p: Parameters) extends MemBlockBundle {
  val loadFastMatch = Vec(LdExuCnt, Input(UInt(LdExuCnt.W)))
  val loadFastFuOpType = Vec(LdExuCnt, Input(FuOpType()))
  val loadFastImm = Vec(LdExuCnt, Input(UInt(12.W)))
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
   val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
   val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
   val pendingld = Input(Bool())
   val pendingst = Input(Bool())
   val commit = Input(Bool())
   val pendingPtr = Input(new RobPtr)
   val pendingPtrNext = Input(new RobPtr)
  }

  val isStore = Input(Bool())
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val enqLsq = new LsqEnqIO
  val flushSb = Input(Bool())

  val loadPc = Vec(LduCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val storePc = Vec(StaCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val hybridPc = Vec(HyuCnt, Input(UInt(VAddrBits.W))) // for hw prefetch

  val issueLda = MixedVec(Seq.fill(LduCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueSta = MixedVec(Seq.fill(StaCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueStd = MixedVec(Seq.fill(StdCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueHya = MixedVec(Seq.fill(HyuCnt)(Flipped(DecoupledIO(new MemExuInput))))
  val issueVldu = MixedVec(Seq.fill(VlduCnt)(Flipped(DecoupledIO(new MemExuInput(isVector=true)))))

  def issueUops = issueLda ++ issueSta ++ issueStd ++ issueHya ++ issueVldu
}

class mem_to_ooo(implicit p: Parameters) extends MemBlockBundle {
  val otherFastWakeup = Vec(LdExuCnt, ValidIO(new DynInst))
  val csrUpdate = new DistributedCSRUpdateReq
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  // used by VLSU issue queue, the vector store would wait all store before it, and the vector load would wait all load
  val sqDeqPtr = Output(new SqPtr)
  val lqDeqPtr = Output(new LqPtr)
  val stIn = Vec(StAddrCnt, ValidIO(new MemExuInput))
  val stIssuePtr = Output(new SqPtr())

  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty = Output(Bool())

  val lsTopdownInfo = Vec(LdExuCnt, Output(new LsTopdownInfo))

  val lsqio = new Bundle {
    val vaddr = Output(UInt(VAddrBits.W))
    val mmio = Output(Vec(LoadPipelineWidth, Bool()))
    val uop = Output(Vec(LoadPipelineWidth, new DynInst))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
  }
  val writebackLda = Vec(LduCnt, DecoupledIO(new MemExuOutput))
  val writebackSta = Vec(StaCnt, DecoupledIO(new MemExuOutput))
  val writebackStd = Vec(StdCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuLda = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackHyuSta = Vec(HyuCnt, DecoupledIO(new MemExuOutput))
  val writebackVldu = Vec(VlduCnt, DecoupledIO(new MemExuOutput(isVector = true)))

  val ldaIqFeedback = Vec(LduCnt, new MemRSFeedbackIO)
  val staIqFeedback = Vec(StaCnt, new MemRSFeedbackIO)
  val hyuIqFeedback = Vec(HyuCnt, new MemRSFeedbackIO)
  val ldCancel = Vec(LdExuCnt, new LoadCancelIO)

  val s3_delayed_load_error = Vec(LdExuCnt, Output(Bool()))
}

class MemCoreTopDownIO extends Bundle {
  val robHeadMissInDCache = Output(Bool())
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
}

class fetch_to_mem(implicit p: Parameters) extends XSBundle{
  val itlb = Flipped(new TlbPtwIO())
}


class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  val l2_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  )

  if (!coreParams.softPTW) {
    ptw_to_l2_buffer.node := ptw.node
  }

  lazy val module = new MemBlockImp(this)

}

class MemBlockImp(outer: MemBlock) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasFPUParameters
  with HasPerfEvents
  with HasL1PrefetchSourceParameter
  with HasMemBlockParameters
{
  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val redirect = Flipped(ValidIO(new Redirect))

    val ooo_to_mem = new ooo_to_mem
    val mem_to_ooo = new mem_to_ooo
    val fetch_to_mem = new fetch_to_mem

    // misc
    val error = new L1CacheErrorInfo
    val memInfo = new Bundle {
      val sqFull = Output(Bool())
      val lqFull = Output(Bool())
      val dcacheMSHRFull = Output(Bool())
    }
    val debug_ls = new DebugLSIO
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val l2PfqBusy = Input(Bool())

    val debugTopDown = new Bundle {
      val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
      val toCore = new MemCoreTopDownIO
    }
    val debugRolling = Flipped(new RobDebugRollingIO)
  })

  val redirect = RegNextWithEnable(io.redirect)

  private val dcache = outer.dcache.module
  val uncache = outer.uncache.module

  val delayedDcacheRefill = RegNext(dcache.io.lsu.lsq)

  val csrCtrl = DelayN(io.ooo_to_mem.csrCtrl, 2)
  dcache.io.csr.distribute_csr <> csrCtrl.distribute_csr
  dcache.io.l2_pf_store_only := RegNext(io.ooo_to_mem.csrCtrl.l2_pf_store_only, false.B)
  io.mem_to_ooo.csrUpdate := RegNext(dcache.io.csr.update)
  io.error <> RegNext(RegNext(dcache.io.error))
  when(!csrCtrl.cache_error_enable){
    io.error.report_to_beu := false.B
    io.error.valid := false.B
  }

  val loadUnits = Seq.fill(LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(StaCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(StdCnt)(Module(new MemExeUnit(backendParams.memSchdParams.get.issueBlockParams(4).exuBlockParams.head)))
  val hybridUnits = Seq.fill(HyuCnt)(Module(new HybridUnit)) // Todo: replace it with HybridUnit
  val stData = stdExeUnits.map(_.io.out)
  val exeUnits = loadUnits ++ storeUnits
  val vlWrapper = Module(new VectorLoadWrapper)
  val vsUopQueue = Module(new VsUopQueue)
  val vsFlowQueue = Module(new VsFlowQueue)

  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _: SMSParams =>
      val sms = Module(new SMSPrefetcher())
      sms.io_agt_en := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := false.B
      sms
  }
  prefetcherOpt.foreach{ pf => pf.io.l1_req.ready := false.B }
  val l1PrefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _ =>
      val l1Prefetcher = Module(new L1Prefetcher())
      l1Prefetcher.io.enable := WireInit(Constantin.createRecord("enableL1StreamPrefetcher" + p(XSCoreParamsKey).HartId.toString, initValue = 1.U)) === 1.U
      l1Prefetcher.pf_ctrl <> dcache.io.pf_ctrl
      l1Prefetcher.l2PfqBusy := io.l2PfqBusy

      // stride will train on miss or prefetch hit
      for (i <- 0 until LduCnt) {
        val source = loadUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train(i).bits := source.bits
        l1Prefetcher.stride_train(i).bits.uop.pc := Mux(loadUnits(i).io.s2_ptr_chasing, io.ooo_to_mem.loadPc(i), RegNext(io.ooo_to_mem.loadPc(i)))
      }
      for (i <- 0 until HyuCnt) {
        val source = hybridUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train.drop(LduCnt)(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train.drop(LduCnt)(i).bits := source.bits
        l1Prefetcher.stride_train.drop(LduCnt)(i).bits.uop.pc := Mux(hybridUnits(i).io.ldu_io.s2_ptr_chasing, io.ooo_to_mem.hybridPc(i), RegNext(io.ooo_to_mem.hybridPc(i)))
      }
      l1Prefetcher
  }
  // load prefetch to l1 Dcache
  l1PrefetcherOpt match {
    case Some(pf) => l1_pf_req <> pf.io.l1_req
    case None =>
      l1_pf_req.valid := false.B
      l1_pf_req.bits := DontCare
  }
  val pf_train_on_hit = RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_train_on_hit, 2, Some(true.B))

  loadUnits.zipWithIndex.map(x => x._1.suggestName("LoadUnit_"+x._2))
  storeUnits.zipWithIndex.map(x => x._1.suggestName("StoreUnit_"+x._2))
  hybridUnits.zipWithIndex.map(x => x._1.suggestName("HybridUnit_"+x._2))
  val atomicsUnit = Module(new AtomicsUnit)

  val hyuLdaWritebackOverride  = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out.bits, hybridUnits.head.io.ldout.bits)
  val hyuLdOut = Wire(Decoupled(new MemExuOutput))
  hyuLdOut.valid := atomicsUnit.io.out.valid || hybridUnits.head.io.ldout.valid
  hyuLdOut.bits  := hyuLdaWritebackOverride
  atomicsUnit.io.out.ready := hyuLdOut.ready
  hybridUnits.head.io.ldout.ready := hyuLdOut.ready

  val hyuLdExeWbReqs = hyuLdOut +: hybridUnits.tail.map(_.io.ldout)
  io.mem_to_ooo.writebackLda <> loadUnits.map(_.io.ldout)
  io.mem_to_ooo.writebackSta <> storeUnits.map(_.io.stout)
  io.mem_to_ooo.writebackStd <> stdExeUnits.map(_.io.out)
  io.mem_to_ooo.writebackHyuLda <> hyuLdExeWbReqs
  io.mem_to_ooo.writebackHyuSta <> hybridUnits.map(_.io.stout)
  io.mem_to_ooo.otherFastWakeup := DontCare
  io.mem_to_ooo.otherFastWakeup.take(LduCnt).zip(loadUnits.map(_.io.fast_uop)).foreach{case(a,b)=> a := b}
  io.mem_to_ooo.otherFastWakeup.drop(LduCnt).take(HyuCnt).zip(hybridUnits.map(_.io.ldu_io.fast_uop)).foreach{case(a,b)=> a:=b}
  val stOut = io.mem_to_ooo.writebackSta ++ io.mem_to_ooo.writebackHyuSta

  // prefetch to l1 req
  // Stream's confidence is always 1
  // (LduCnt + HyuCnt) l1_pf_reqs ?
  loadUnits.foreach(load_unit => {
    load_unit.io.prefetch_req.valid <> l1_pf_req.valid
    load_unit.io.prefetch_req.bits <> l1_pf_req.bits
  })
  hybridUnits.foreach(hybrid_unit => {
    hybrid_unit.io.ldu_io.prefetch_req.valid <> l1_pf_req.valid
    hybrid_unit.io.ldu_io.prefetch_req.bits <> l1_pf_req.bits
  })
  // NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1)
  // when loadUnits(0) stage 0 is busy, hw prefetch will never use that pipeline
  val LowConfPort = 0
  loadUnits(LowConfPort).io.prefetch_req.bits.confidence := 0.U

  val canAcceptHighConfPrefetch = loadUnits.map(_.io.canAcceptHighConfPrefetch) ++
                                  hybridUnits.map(_.io.canAcceptLowConfPrefetch)
  val canAcceptLowConfPrefetch = loadUnits.map(_.io.canAcceptLowConfPrefetch) ++
                                 hybridUnits.map(_.io.canAcceptLowConfPrefetch)
  l1_pf_req.ready := (0 until LduCnt + HyuCnt).map{
    case i => {
      if(i == LowConfPort) {
        loadUnits(i).io.canAcceptLowConfPrefetch
      }else {
        Mux(l1_pf_req.bits.confidence === 1.U, canAcceptHighConfPrefetch(i), canAcceptLowConfPrefetch(i))
      }
    }
  }.reduce(_ || _)

  // l1 pf fuzzer interface
  val DebugEnableL1PFFuzzer = false
  if (DebugEnableL1PFFuzzer) {
    // l1 pf req fuzzer
    val fuzzer = Module(new L1PrefetchFuzzer())
    fuzzer.io.vaddr := DontCare
    fuzzer.io.paddr := DontCare

    // override load_unit prefetch_req
    loadUnits.foreach(load_unit => {
      load_unit.io.prefetch_req.valid <> fuzzer.io.req.valid
      load_unit.io.prefetch_req.bits <> fuzzer.io.req.bits
    })

    // override hybrid_unit prefetch_req
    hybridUnits.foreach(hybrid_unit => {
      hybrid_unit.io.ldu_io.prefetch_req.valid <> fuzzer.io.req.valid
      hybrid_unit.io.ldu_io.prefetch_req.bits <> fuzzer.io.req.bits
    })

    fuzzer.io.req.ready := l1_pf_req.ready
  }

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrapper)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer) // out of date now
  io.mem_to_ooo.stIssuePtr := lsq.io.issuePtrExt

  dcache.io.hartId := io.hartId
  lsq.io.hartId := io.hartId
  sbuffer.io.hartId := io.hartId
  atomicsUnit.io.hartId := io.hartId

  dcache.io.lqEmpty := lsq.io.lqEmpty

  // load/store prefetch to l2 cache
  prefetcherOpt.foreach(sms_pf => {
    l1PrefetcherOpt.foreach(l1_pf => {
      val sms_pf_to_l2 = ValidIODelay(sms_pf.io.l2_req, 2)
      val l1_pf_to_l2 = ValidIODelay(l1_pf.io.l2_req, 2)

      outer.l2_pf_sender_opt.get.out.head._1.addr_valid := sms_pf_to_l2.valid || l1_pf_to_l2.valid
      outer.l2_pf_sender_opt.get.out.head._1.addr := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.addr, sms_pf_to_l2.bits.addr)
      outer.l2_pf_sender_opt.get.out.head._1.pf_source := Mux(l1_pf_to_l2.valid, l1_pf_to_l2.bits.source, sms_pf_to_l2.bits.source)
      outer.l2_pf_sender_opt.get.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 2, Some(true.B))

      sms_pf.io.enable := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable, 2, Some(false.B))

      val l2_trace = Wire(new LoadPfDbBundle)
      l2_trace.paddr := outer.l2_pf_sender_opt.get.out.head._1.addr
      val table = ChiselDB.createTable("L2PrefetchTrace"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
      table.log(l2_trace, l1_pf_to_l2.valid, "StreamPrefetchTrace", clock, reset)
      table.log(l2_trace, !l1_pf_to_l2.valid && sms_pf_to_l2.valid, "L2PrefetchTrace", clock, reset)

      val l1_pf_to_l3 = ValidIODelay(l1_pf.io.l3_req, 4)
      outer.l3_pf_sender_opt.get.out.head._1.addr_valid := l1_pf_to_l3.valid
      outer.l3_pf_sender_opt.get.out.head._1.addr := l1_pf_to_l3.bits
      outer.l3_pf_sender_opt.get.out.head._1.l2_pf_en := RegNextN(io.ooo_to_mem.csrCtrl.l2_pf_enable, 4, Some(true.B))

      val l3_trace = Wire(new LoadPfDbBundle)
      l3_trace.paddr := outer.l3_pf_sender_opt.get.out.head._1.addr
      val l3_table = ChiselDB.createTable("L3PrefetchTrace"+ p(XSCoreParamsKey).HartId.toString, new LoadPfDbBundle, basicDB = false)
      l3_table.log(l3_trace, l1_pf_to_l3.valid, "StreamPrefetchTrace", clock, reset)

      XSPerfAccumulate("prefetch_fire_l2", outer.l2_pf_sender_opt.get.out.head._1.addr_valid)
      XSPerfAccumulate("prefetch_fire_l3", outer.l3_pf_sender_opt.get.out.head._1.addr_valid)
      XSPerfAccumulate("l1pf_fire_l2", l1_pf_to_l2.valid)
      XSPerfAccumulate("sms_fire_l2", !l1_pf_to_l2.valid && sms_pf_to_l2.valid)
      XSPerfAccumulate("sms_block_by_l1pf", l1_pf_to_l2.valid && sms_pf_to_l2.valid)
    })
  })

  // ptw
  val sfence = RegNext(RegNext(io.ooo_to_mem.sfence))
  val tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))
  private val ptw = outer.ptw.module
  private val ptw_to_l2_buffer = outer.ptw_to_l2_buffer.module
  ptw.io.hartId := io.hartId
  ptw.io.sfence <> sfence
  ptw.io.csr.tlb <> tlbcsr
  ptw.io.csr.distribute_csr <> csrCtrl.distribute_csr
  ptw.io.tlb(0) <> io.fetch_to_mem.itlb

  val perfEventsPTW = Wire(Vec(19, new PerfEvent))
  if (!coreParams.softPTW) {
    perfEventsPTW := ptw.getPerf
  } else {
    perfEventsPTW := DontCare
  }

  // dtlb
  val dtlb_ld = VecInit(Seq.fill(1){
    val tlb_ld = Module(new TLBNonBlock(LduCnt + 1, 2, ldtlbParams))
    tlb_ld.io // let the module have name in waveform
  })
  val dtlb_st = VecInit(Seq.fill(1){
    val tlb_st = Module(new TLBNonBlock(StaCnt, 1, sttlbParams))
    tlb_st.io // let the module have name in waveform
  })
  val dtlb_hy = VecInit(Seq.fill(1){
    val tlb_hy = Module(new TLBNonBlock(HyuCnt, 2, hytlbParams))
    tlb_hy.io // let the module have same in waveform
  })
  val dtlb_prefetch = VecInit(Seq.fill(1){
    val tlb_prefetch = Module(new TLBNonBlock(1, 2, pftlbParams))
    tlb_prefetch.io // let the module have name in waveform
  })
  val dtlb = dtlb_ld ++ dtlb_st ++ dtlb_hy ++ dtlb_prefetch
  val ptwio = Wire(new VectorTlbPtwIO(LduCnt + StaCnt + HyuCnt + 2)) // load + store + hybrid + hw prefetch
  val dtlb_reqs = dtlb.map(_.requestor).flatten
  val dtlb_pmps = dtlb.map(_.pmp).flatten
  dtlb.map(_.hartId := io.hartId)
  dtlb.map(_.sfence := sfence)
  dtlb.map(_.csr := tlbcsr)
  dtlb.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace == hytlbParams.outReplace)
    require(ldtlbParams.outReplace == pftlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(LduCnt + StaCnt + HyuCnt + 2, ldtlbParams))
    replace.io.apply_sep(dtlb_ld.map(_.replace) ++ dtlb_st.map(_.replace) ++ dtlb_hy.map(_.replace) ++ dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.entry.tag)
  } else {
    if (ldtlbParams.outReplace) {
      val replace_ld = Module(new TlbReplace(LduCnt + 1, ldtlbParams))
      replace_ld.io.apply_sep(dtlb_ld.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replace_st = Module(new TlbReplace(StaCnt, sttlbParams))
      replace_st.io.apply_sep(dtlb_st.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
    if (hytlbParams.outReplace) {
      val replace_hy = Module(new TlbReplace(HyuCnt, hytlbParams))
      replace_hy.io.apply_sep(dtlb_hy.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
    if (pftlbParams.outReplace) {
      val replace_pf = Module(new TlbReplace(1, pftlbParams))
      replace_pf.io.apply_sep(dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
  }

  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed), init = false.B)
  ptwio.resp.ready := true.B

  dtlb.flatMap(a => a.ptw.req)
    .zipWithIndex
    .foreach{ case (tlb, i) =>
      tlb.ready := ptwio.req(i).ready
      ptwio.req(i).bits := tlb.bits
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < (LduCnt + 1)) Cat(ptw_resp_next.vector.take(LduCnt + 1)).orR
      else if (i < (LduCnt + 1 + StaCnt)) Cat(ptw_resp_next.vector.drop(LduCnt + 1).take(StaCnt)).orR
      else if (i < (LduCnt + 1 + StaCnt + HyuCnt)) Cat(ptw_resp_next.vector.drop(LduCnt + 1 + StaCnt).take(HyuCnt)).orR
      else Cat(ptw_resp_next.vector.drop(LduCnt + StaCnt + HyuCnt + 1)).orR
    ptwio.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit &&
      ptw_resp_next.data.hit(tlb.bits.vpn, tlbcsr.satp.asid, allType = true, ignoreAsid = true))
  }
  dtlb.foreach(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.take(LduCnt + 1)).orR)
    dtlb_st.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(LduCnt + 1).take(StaCnt)).orR)
    dtlb_hy.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(LduCnt + StaCnt + 1).take(HyuCnt)).orR)
    dtlb_prefetch.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(LduCnt + StaCnt + HyuCnt + 1)).orR)
  }

  val dtlbRepeater1  = PTWFilter(ldtlbParams.fenceDelay, ptwio, sfence, tlbcsr, l2tlbParams.dfilterSize)
  val dtlbRepeater2  = PTWRepeaterNB(passReady = false, ldtlbParams.fenceDelay, dtlbRepeater1.io.ptw, ptw.io.tlb(1), sfence, tlbcsr)
  val itlbRepeater2 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater1.io.rob_head_miss_in_tlb

  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(LduCnt + StaCnt + HyuCnt + 2)(Module(new PMPChecker(3)).io))
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  for (i <- 0 until LduCnt) {
    io.debug_ls.debugLsInfo(i) := loadUnits(i).io.debug_ls
  }
  for (i <- 0 until StaCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt)(i) := storeUnits(i).io.debug_ls
  }
  for (i <- 0 until HyuCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt + StaCnt)(i) := hybridUnits(i).io.ldu_io.debug_ls
  }
  for (i <- 0 until HyuCnt) {
    io.debug_ls.debugLsInfo.drop(LduCnt + StaCnt + HyuCnt)(i) := hybridUnits(i).io.stu_io.debug_ls
  }

  io.mem_to_ooo.lsTopdownInfo := loadUnits.map(_.io.lsTopdownInfo) ++ hybridUnits.map(_.io.ldu_io.lsTopdownInfo)

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
  class BalanceEntry extends XSBundle {
    val balance = Bool()
    val req = new LqWriteBundle
    val port = UInt(log2Up(LoadPipelineWidth).W)
  }

  def balanceReOrder(sel: Seq[ValidIO[BalanceEntry]]): Seq[ValidIO[BalanceEntry]] = {
    require(sel.length > 0)
    val balancePick = ParallelPriorityMux(sel.map(x => (x.valid && x.bits.balance) -> x))
    val reorderSel = Wire(Vec(sel.length, ValidIO(new BalanceEntry)))
    (0 until sel.length).map(i =>
      if (i == 0) {
        when (balancePick.valid && balancePick.bits.balance) {
          reorderSel(i) := balancePick
        } .otherwise {
          reorderSel(i) := sel(i)
        }
      } else {
        when (balancePick.valid && balancePick.bits.balance && i.U === balancePick.bits.port) {
          reorderSel(i) := sel(0)
        } .otherwise {
          reorderSel(i) := sel(i)
        }
      }
    )
    reorderSel
  }

  val lduFastReplaySel = loadUnits.map(_.io).zipWithIndex.map { case (ldu, i) => {
    val wrapper = Wire(Valid(new BalanceEntry))
    wrapper.valid        := ldu.fast_rep_out.valid
    wrapper.bits.req     := ldu.fast_rep_out.bits
    wrapper.bits.balance := ldu.fast_rep_out.bits.rep_info.bank_conflict
    wrapper.bits.port    := i.U
    wrapper
  }}
  val hyuFastReplaySel = hybridUnits.map(_.io.ldu_io).zipWithIndex.map { case (ldu, i) => {
    val wrapper = Wire(Valid(new BalanceEntry))
    wrapper.valid        := ldu.fast_rep_out.valid
    wrapper.bits.req     := ldu.fast_rep_out.bits
    wrapper.bits.balance := ldu.fast_rep_out.bits.rep_info.bank_conflict
    wrapper.bits.port    := (LduCnt + i).U
    wrapper
  }}
  val fastReplaySel = lduFastReplaySel ++ hyuFastReplaySel
  val balanceFastReplaySel = balanceReOrder(fastReplaySel)

  val correctMissTrain = WireInit(Constantin.createRecord("CorrectMissTrain" + p(XSCoreParamsKey).HartId.toString, initValue = 0.U)) === 1.U

  for (i <- 0 until LduCnt) {
    loadUnits(i).io.redirect <> redirect

    // get input form dispatch
    loadUnits(i).io.ldin <> io.ooo_to_mem.issueLda(i)
    loadUnits(i).io.feedback_slow <> io.mem_to_ooo.ldaIqFeedback(i).feedbackSlow
    loadUnits(i).io.feedback_fast <> io.mem_to_ooo.ldaIqFeedback(i).feedbackFast
    loadUnits(i).io.correctMissTrain := correctMissTrain
    io.mem_to_ooo.ldCancel(i) := loadUnits(i).io.ldCancel

    // vector
    loadUnits(i).io.vecldin <> vlWrapper.io.pipeIssue(i)
    vlWrapper.io.pipeReplay(i) <> loadUnits(i).io.vecReplay
    vlWrapper.io.pipeResult(i) <> loadUnits(i).io.vecldout
    loadUnits(i).io.vec_forward <> vsFlowQueue.io.forward(i)

    // fast replay
    loadUnits(i).io.fast_rep_in.valid := balanceFastReplaySel(i).valid
    loadUnits(i).io.fast_rep_in.bits := balanceFastReplaySel(i).bits.req

    loadUnits(i).io.fast_rep_out.ready := false.B
    val fast_rep_in = loadUnits.map(_.io.fast_rep_in) ++ hybridUnits.map(_.io.ldu_io.fast_rep_in)
    for (j <- 0 until LduCnt + HyuCnt) {
      when (balanceFastReplaySel(j).valid && balanceFastReplaySel(j).bits.port === i.U) {
        loadUnits(i).io.fast_rep_out.ready := fast_rep_in(j).ready
      }
    }

    // dcache access
    loadUnits(i).io.dcache <> dcache.io.lsu.load(i)
    // forward
    loadUnits(i).io.lsq.forward <> lsq.io.forward(i)
    loadUnits(i).io.sbuffer <> sbuffer.io.forward(i)
    loadUnits(i).io.tl_d_channel := dcache.io.lsu.forward_D(i)
    loadUnits(i).io.forward_mshr <> dcache.io.lsu.forward_mshr(i)
    // ld-ld violation check
    loadUnits(i).io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(i)
    loadUnits(i).io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(i)
    loadUnits(i).io.csrCtrl       <> csrCtrl
    // dcache refill req
    loadUnits(i).io.refill           <> delayedDcacheRefill
    // dtlb
    loadUnits(i).io.tlb <> dtlb_reqs.take(LduCnt)(i)
    // pmp
    loadUnits(i).io.pmp <> pmp_check(i).resp
    // st-ld violation query
    val stld_nuke_query = storeUnits.map(_.io.stld_nuke_query) ++ hybridUnits.map(_.io.stu_io.stld_nuke_query)
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.stld_nuke_query(s) := stld_nuke_query(s)
    }
    loadUnits(i).io.lq_rep_full <> lsq.io.lq_rep_full
    // load prefetch train
    prefetcherOpt.foreach(pf => {
      // sms will train on all miss load sources
      val source = loadUnits(i).io.prefetch_train
      pf.io.ld_in(i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      )
      pf.io.ld_in(i).bits := source.bits
      pf.io.ld_in(i).bits.uop.pc := Mux(loadUnits(i).io.s2_ptr_chasing, io.ooo_to_mem.loadPc(i), RegNext(io.ooo_to_mem.loadPc(i)))
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = loadUnits(i).io.prefetch_train_l1
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue
      pf.io.ld_in(i).bits := source.bits
    })

    // load to load fast forward: load(i) prefers data(i)
    val l2l_fwd_out = loadUnits.map(_.io.l2l_fwd_out) ++ hybridUnits.map(_.io.ldu_io.l2l_fwd_out)
    val fastPriority = (i until LduCnt + HyuCnt) ++ (0 until i)
    val fastValidVec = fastPriority.map(j => l2l_fwd_out(j).valid)
    val fastDataVec = fastPriority.map(j => l2l_fwd_out(j).data)
    val fastErrorVec = fastPriority.map(j => l2l_fwd_out(j).dly_ld_err)
    val fastMatchVec = fastPriority.map(j => io.ooo_to_mem.loadFastMatch(i)(j))
    loadUnits(i).io.l2l_fwd_in.valid := VecInit(fastValidVec).asUInt.orR
    loadUnits(i).io.l2l_fwd_in.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    loadUnits(i).io.l2l_fwd_in.dly_ld_err := ParallelPriorityMux(fastValidVec, fastErrorVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    loadUnits(i).io.ld_fast_match := fastMatch
    loadUnits(i).io.ld_fast_imm := io.ooo_to_mem.loadFastImm(i)
    loadUnits(i).io.ld_fast_fuOpType := io.ooo_to_mem.loadFastFuOpType(i)
    loadUnits(i).io.replay <> lsq.io.replay(i)

    loadUnits(i).io.l2_hint <> io.l2_hint

    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(i) <> loadUnits(i).io.lsq.ldin
    lsq.io.ldout(i) <> loadUnits(i).io.lsq.uncache
    lsq.io.ld_raw_data(i) <> loadUnits(i).io.lsq.ld_raw_data
    lsq.io.trigger(i) <> loadUnits(i).io.lsq.trigger

    lsq.io.l2_hint.valid := io.l2_hint.valid
    lsq.io.l2_hint.bits.sourceId := io.l2_hint.bits.sourceId

    // alter writeback exception info
    io.mem_to_ooo.s3_delayed_load_error(i) := loadUnits(i).io.s3_dly_ld_err

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
      io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit(lTriggerMapping(j)) := hit(j)
//      io.writeback(i).bits.uop.cf.trigger.backendTiming(lTriggerMapping(j)) := tdata(lTriggerMapping(j)).timing
      //      if (lChainMapping.contains(j)) io.writeback(i).bits.uop.cf.trigger.triggerChainVec(lChainMapping(j)) := hit && tdata(j+3).chain
    }
    when(tdata(2).chain) {
      io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit(2) := hit(0) && hit(1)
      io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit(3) := hit(0) && hit(1)
    }
    when(!io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendEn(1)) {
      io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit(5) := false.B
    }

    XSDebug(io.mem_to_ooo.writebackLda(i).bits.uop.trigger.getHitBackend && io.mem_to_ooo.writebackLda(i).valid, p"Debug Mode: Load Inst No.${i}" +
    p"has trigger hit vec ${io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit}\n")

  }

  for (i <- 0 until HyuCnt) {
    hybridUnits(i).io.redirect <> redirect

    // get input from dispatch
    hybridUnits(i).io.lsin <> io.ooo_to_mem.issueHya(i)
    hybridUnits(i).io.feedback_slow <> io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow
    hybridUnits(i).io.feedback_fast <> io.mem_to_ooo.hyuIqFeedback(i).feedbackFast
    hybridUnits(i).io.correctMissTrain := correctMissTrain
    io.mem_to_ooo.ldCancel.drop(LduCnt)(i) := hybridUnits(i).io.ldu_io.ldCancel

    // ------------------------------------
    //  Load Port
    // ------------------------------------
    // fast replay
    hybridUnits(i).io.ldu_io.fast_rep_in.valid := balanceFastReplaySel.drop(LduCnt)(i).valid
    hybridUnits(i).io.ldu_io.fast_rep_in.bits := balanceFastReplaySel.drop(LduCnt)(i).bits.req

    hybridUnits(i).io.ldu_io.fast_rep_out.ready := false.B
    val fast_rep_in = loadUnits.map(_.io.fast_rep_in) ++ hybridUnits.map(_.io.ldu_io.fast_rep_in)
    for (j <- 0 until LduCnt + HyuCnt) {
      when (balanceFastReplaySel(j).valid && balanceFastReplaySel(j).bits.port === (LduCnt + i).U) {
        hybridUnits(i).io.ldu_io.fast_rep_out.ready := fast_rep_in(j).ready
      }
    }

    // get input from dispatch
    hybridUnits(i).io.ldu_io.dcache <> dcache.io.lsu.load(LduCnt + i)
    hybridUnits(i).io.stu_io.dcache <> dcache.io.lsu.sta(StaCnt + i)

    // dcache access
    hybridUnits(i).io.ldu_io.lsq.forward <> lsq.io.forward(LduCnt + i)
    // forward
    hybridUnits(i).io.ldu_io.sbuffer <> sbuffer.io.forward(LduCnt + i)
    hybridUnits(i).io.ldu_io.tl_d_channel := dcache.io.lsu.forward_D(LduCnt + i)
    hybridUnits(i).io.ldu_io.forward_mshr <> dcache.io.lsu.forward_mshr(LduCnt + i)
    // ld-ld violation check
    hybridUnits(i).io.ldu_io.lsq.ldld_nuke_query <> lsq.io.ldu.ldld_nuke_query(LduCnt + i)
    hybridUnits(i).io.ldu_io.lsq.stld_nuke_query <> lsq.io.ldu.stld_nuke_query(LduCnt + i)
    hybridUnits(i).io.csrCtrl <> csrCtrl
    // dcache refill req
    hybridUnits(i).io.ldu_io.refill <> delayedDcacheRefill
    // dtlb
    hybridUnits(i).io.tlb <> dtlb_reqs.drop(LduCnt + StaCnt + 1)(i)
    // pmp
    hybridUnits(i).io.pmp <> pmp_check.drop(LduCnt + StaCnt + 1)(i).resp
    // st-ld violation query
    val stld_nuke_query = VecInit(storeUnits.map(_.io.stld_nuke_query) ++ hybridUnits.map(_.io.stu_io.stld_nuke_query))
    hybridUnits(i).io.ldu_io.stld_nuke_query := stld_nuke_query
    hybridUnits(i).io.ldu_io.lq_rep_full <> lsq.io.lq_rep_full
    // load prefetch train
    prefetcherOpt.foreach(pf => {
      val source = hybridUnits(i).io.prefetch_train
      pf.io.ld_in(LduCnt + i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      )
      pf.io.ld_in(LduCnt + i).bits := source.bits
      pf.io.ld_in(LduCnt + i).bits.uop.pc := Mux(hybridUnits(i).io.ldu_io.s2_ptr_chasing, io.ooo_to_mem.hybridPc(i), RegNext(io.ooo_to_mem.hybridPc(i)))
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = hybridUnits(i).io.prefetch_train_l1
      pf.io.ld_in(LduCnt + i).valid := source.valid && source.bits.isFirstIssue &&
                                       FuType.isLoad(source.bits.uop.fuType)
      pf.io.ld_in(LduCnt + i).bits := source.bits
      pf.io.st_in(StaCnt + i).valid := false.B
      pf.io.st_in(StaCnt + i).bits := DontCare
    })
    prefetcherOpt.foreach(pf => {
      val source = hybridUnits(i).io.prefetch_train
      pf.io.st_in(StaCnt + i).valid := Mux(pf_train_on_hit,
        source.valid,
        source.valid && source.bits.isFirstIssue && source.bits.miss
      ) && FuType.isStore(source.bits.uop.fuType)
      pf.io.st_in(StaCnt + i).bits := source.bits
      pf.io.st_in(StaCnt + i).bits.uop.pc := RegNext(io.ooo_to_mem.hybridPc(i))
    })

    // load to load fast forward: load(i) prefers data(i)
    val l2l_fwd_out = loadUnits.map(_.io.l2l_fwd_out) ++ hybridUnits.map(_.io.ldu_io.l2l_fwd_out)
    val fastPriority = (LduCnt + i until LduCnt + HyuCnt) ++ (0 until LduCnt + i)
    val fastValidVec = fastPriority.map(j => l2l_fwd_out(j).valid)
    val fastDataVec = fastPriority.map(j => l2l_fwd_out(j).data)
    val fastErrorVec = fastPriority.map(j => l2l_fwd_out(j).dly_ld_err)
    val fastMatchVec = fastPriority.map(j => io.ooo_to_mem.loadFastMatch(LduCnt + i)(j))
    hybridUnits(i).io.ldu_io.l2l_fwd_in.valid := VecInit(fastValidVec).asUInt.orR
    hybridUnits(i).io.ldu_io.l2l_fwd_in.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    hybridUnits(i).io.ldu_io.l2l_fwd_in.dly_ld_err := ParallelPriorityMux(fastValidVec, fastErrorVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    hybridUnits(i).io.ldu_io.ld_fast_match := fastMatch
    hybridUnits(i).io.ldu_io.ld_fast_imm := io.ooo_to_mem.loadFastImm(LduCnt + i)
    hybridUnits(i).io.ldu_io.ld_fast_fuOpType := io.ooo_to_mem.loadFastFuOpType(LduCnt + i)
    hybridUnits(i).io.ldu_io.replay <> lsq.io.replay(LduCnt + i)
    hybridUnits(i).io.ldu_io.l2_hint <> io.l2_hint

    // uncache
    lsq.io.ldout.drop(LduCnt)(i) <> hybridUnits(i).io.ldu_io.lsq.uncache
    lsq.io.ld_raw_data.drop(LduCnt)(i) <> hybridUnits(i).io.ldu_io.lsq.ld_raw_data


    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(LduCnt + i) <> hybridUnits(i).io.ldu_io.lsq.ldin
    lsq.io.trigger(LduCnt + i) <> hybridUnits(i).io.ldu_io.lsq.trigger
    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(StaCnt + i) <> hybridUnits(i).io.stu_io.st_mask_out

    // Lsq to std unit's rs
    lsq.io.std.storeDataIn(StaCnt + i) := stData(StaCnt + i)
    // prefetch
    hybridUnits(i).io.stu_io.prefetch_req <> sbuffer.io.store_prefetch(StaCnt + i)

    io.mem_to_ooo.s3_delayed_load_error(LduCnt + i) := hybridUnits(i).io.ldu_io.s3_dly_ld_err

    // --------------------------------
    // Load Triggers
    // --------------------------------
    val hit = Wire(Vec(3, Bool()))
    for (j <- 0 until 3) {
      hybridUnits(i).io.ldu_io.trigger(j).tdata2 := tdata(lTriggerMapping(j)).tdata2
      hybridUnits(i).io.ldu_io.trigger(j).matchType := tdata(lTriggerMapping(j)).matchType
      hybridUnits(i).io.ldu_io.trigger(j).tEnable := tEnable(lTriggerMapping(j))
      // Just let load triggers that match data unavailable
      hit(j) := hybridUnits(i).io.ldu_io.trigger(j).addrHit && !tdata(lTriggerMapping(j)).select
      io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit(lTriggerMapping(j)) := hit(j)
    }
    when (tdata(2).chain) {
      io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit(2) := hit(0) && hit(1)
      io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit(3) := hit(0) && hit(1)
    }
    when (!io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendEn(1)) {
      io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit(5) := false.B
    }
    XSDebug(io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.getHitBackend && io.mem_to_ooo.writebackHyuLda(i).valid, p"Debug Mode: Hybrid Inst No.${i}" +
    p"has trigger hit vec ${io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit}\n")

    // ------------------------------------
    //  Store Port
    // ------------------------------------
    hybridUnits(i).io.stu_io.lsq <> lsq.io.sta.storeAddrIn.takeRight(HyuCnt)(i)
    hybridUnits(i).io.stu_io.lsq_replenish <> lsq.io.sta.storeAddrInRe.takeRight(HyuCnt)(i)

    lsq.io.sta.storeMaskIn.takeRight(HyuCnt)(i) <> hybridUnits(i).io.stu_io.st_mask_out
    io.mem_to_ooo.stIn.takeRight(HyuCnt)(i).valid := hybridUnits(i).io.stu_io.issue.valid
    io.mem_to_ooo.stIn.takeRight(HyuCnt)(i).bits := hybridUnits(i).io.stu_io.issue.bits

    // -------------------------
    // Store Triggers
    // -------------------------
    val hyuOut = io.mem_to_ooo.writebackHyuSta(i)
    when (hybridUnits(i).io.stout.fire &&
          FuType.isStore(hybridUnits(i).io.stout.bits.uop.fuType)) {
      val hit = Wire(Vec(3, Bool()))
      for (j <- 0 until 3) {
        hit(j) := !tdata(sTriggerMapping(j)).select && TriggerCmp(
          hyuOut.bits.debug.vaddr,
          tdata(sTriggerMapping(j)).tdata2,
          tdata(sTriggerMapping(j)).matchType,
          tEnable(sTriggerMapping(j))
        )
        hyuOut.bits.uop.trigger.backendHit(sTriggerMapping(j)) := hit(j)
      }

      when (tdata(0).chain) {
        io.mem_to_ooo.writebackHyuSta(i).bits.uop.trigger.backendHit(0) := hit(0) && hit(1)
        io.mem_to_ooo.writebackHyuSta(i).bits.uop.trigger.backendHit(1) := hit(0) && hit(1)
      }

      when (!hyuOut.bits.uop.trigger.backendEn(0)) {
        hyuOut.bits.uop.trigger.backendHit(4) := false.B
      }
    }
  }

  // Prefetcher
  val StreamDTLBPortIndex = LduCnt
  val PrefetcherDTLBPortIndex = LduCnt + StaCnt + HyuCnt + 1
  prefetcherOpt match {
  case Some(pf) => dtlb_reqs(PrefetcherDTLBPortIndex) <> pf.io.tlb_req
  case None =>
    dtlb_reqs(PrefetcherDTLBPortIndex) := DontCare
    dtlb_reqs(PrefetcherDTLBPortIndex).req.valid := false.B
    dtlb_reqs(PrefetcherDTLBPortIndex).resp.ready := true.B
  }
  l1PrefetcherOpt match {
    case Some(pf) => dtlb_reqs(StreamDTLBPortIndex) <> pf.io.tlb_req
    case None =>
        dtlb_reqs(StreamDTLBPortIndex) := DontCare
        dtlb_reqs(StreamDTLBPortIndex).req.valid := false.B
        dtlb_reqs(StreamDTLBPortIndex).resp.ready := true.B
  }

  // StoreUnit
  for (i <- 0 until StdCnt) {
    stdExeUnits(i).io.flush <> redirect
    stdExeUnits(i).io.in.valid := io.ooo_to_mem.issueStd(i).valid
    io.ooo_to_mem.issueStd(i).ready := stdExeUnits(i).io.in.ready
    stdExeUnits(i).io.in.bits := io.ooo_to_mem.issueStd(i).bits
  }

  for (i <- 0 until StaCnt) {
    val stu = storeUnits(i)

    stu.io.redirect      <> redirect
    stu.io.dcache        <> dcache.io.lsu.sta(i)
    stu.io.feedback_slow <> io.mem_to_ooo.staIqFeedback(i).feedbackSlow
    stu.io.stin         <> io.ooo_to_mem.issueSta(i)
    stu.io.lsq          <> lsq.io.sta.storeAddrIn(i)
    stu.io.lsq_replenish <> lsq.io.sta.storeAddrInRe(i)
    stu.io.lsq_vec       <> lsq.io.sta.vecStoreAddrIn(i)
    // dtlb
    stu.io.tlb          <> dtlb_reqs.drop(LduCnt + 1)(i)
    stu.io.pmp          <> pmp_check(LduCnt + 1 + i).resp

    // prefetch
    stu.io.prefetch_req <> sbuffer.io.store_prefetch(i)

    // store unit does not need fast feedback
    io.mem_to_ooo.staIqFeedback(i).feedbackFast := DontCare

    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(i) <> stu.io.st_mask_out

    // Lsq to std unit's rs
    lsq.io.std.storeDataIn(i) := stData(i)

    // store prefetch train
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    })

    prefetcherOpt.foreach(pf => {
      pf.io.st_in(i).valid := Mux(pf_train_on_hit,
        stu.io.prefetch_train.valid,
        stu.io.prefetch_train.valid && stu.io.prefetch_train.bits.isFirstIssue && (
          stu.io.prefetch_train.bits.miss
          )
      )
      pf.io.st_in(i).bits := stu.io.prefetch_train.bits
      pf.io.st_in(i).bits.uop.pc := RegNext(io.ooo_to_mem.storePc(i))
    })

    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    // io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    // io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits
    io.mem_to_ooo.stIn(i).valid := stu.io.issue.valid
    io.mem_to_ooo.stIn(i).bits := stu.io.issue.bits

    stu.io.stout.ready := true.B

    // vector
    stu.io.vecstin <> vsFlowQueue.io.pipeIssue(i)
    vsFlowQueue.io.pipeFeedback(i) <> stu.io.vec_feedback_slow
    stu.io.vec_isFirstIssue := true.B // TODO
    // -------------------------
    // Store Triggers
    // -------------------------
    when(stOut(i).fire){
      val hit = Wire(Vec(3, Bool()))
      for (j <- 0 until 3) {
         hit(j) := !tdata(sTriggerMapping(j)).select && TriggerCmp(
           stOut(i).bits.debug.vaddr,
           tdata(sTriggerMapping(j)).tdata2,
           tdata(sTriggerMapping(j)).matchType,
           tEnable(sTriggerMapping(j))
         )
       stOut(i).bits.uop.trigger.backendHit(sTriggerMapping(j)) := hit(j)
     }

     when(tdata(0).chain) {
       io.mem_to_ooo.writebackSta(i).bits.uop.trigger.backendHit(0) := hit(0) && hit(1)
       io.mem_to_ooo.writebackSta(i).bits.uop.trigger.backendHit(1) := hit(0) && hit(1)
     }

     when(!stOut(i).bits.uop.trigger.backendEn(0)) {
       stOut(i).bits.uop.trigger.backendHit(4) := false.B
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

  when (atomicsUnit.io.out.valid) {
    // when atom inst writeback, surpress normal load trigger
    (0 until LduCnt).map(i => {
      io.mem_to_ooo.writebackLda(i).bits.uop.trigger.backendHit := VecInit(Seq.fill(6)(false.B))
    })
    (0 until HyuCnt).map(i => {
      io.mem_to_ooo.writebackHyuLda(i).bits.uop.trigger.backendHit := VecInit(Seq.fill(6)(false.B))
    })
  }

  // Uncahce
  uncache.io.enableOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable
  uncache.io.hartId := io.hartId
  lsq.io.uncacheOutstanding := io.ooo_to_mem.csrCtrl.uncache_write_outstanding_enable

  // Lsq
  io.mem_to_ooo.lsqio.mmio       := lsq.io.rob.mmio
  io.mem_to_ooo.lsqio.uop        := lsq.io.rob.uop
  lsq.io.rob.lcommit             := io.ooo_to_mem.lsqio.lcommit
  lsq.io.rob.scommit             := io.ooo_to_mem.lsqio.scommit
  lsq.io.rob.pendingld           := io.ooo_to_mem.lsqio.pendingld
  lsq.io.rob.pendingst           := io.ooo_to_mem.lsqio.pendingst
  lsq.io.rob.commit              := io.ooo_to_mem.lsqio.commit
  lsq.io.rob.pendingPtr          := io.ooo_to_mem.lsqio.pendingPtr
  lsq.io.rob.pendingPtrNext      := io.ooo_to_mem.lsqio.pendingPtrNext

//  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.ooo_to_mem.enqLsq
  lsq.io.brqRedirect    <> redirect
  io.mem_to_ooo.memoryViolation    <> lsq.io.rollback
  io.mem_to_ooo.lsqio.lqCanAccept  := lsq.io.lqCanAccept
  io.mem_to_ooo.lsqio.sqCanAccept  := lsq.io.sqCanAccept
  // lsq.io.uncache        <> uncache.io.lsq
  AddPipelineReg(lsq.io.uncache.req, uncache.io.lsq.req, false.B)
  AddPipelineReg(uncache.io.lsq.resp, lsq.io.uncache.resp, false.B)
  // delay dcache refill for 1 cycle for better timing
  lsq.io.refill         := delayedDcacheRefill
  lsq.io.release        := dcache.io.lsu.release
  lsq.io.lqCancelCnt <> io.mem_to_ooo.lqCancelCnt
  lsq.io.sqCancelCnt <> io.mem_to_ooo.sqCancelCnt
  lsq.io.lqDeq <> io.mem_to_ooo.lqDeq
  lsq.io.sqDeq <> io.mem_to_ooo.sqDeq
  // Todo: assign these
  io.mem_to_ooo.sqDeqPtr := ???
  io.mem_to_ooo.lqDeqPtr := ???
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel

  // LSQ to store buffer
  // lsq.io.sbuffer        <> sbuffer.io.in
  val sbReq = Seq(vsFlowQueue.io.sbuffer, lsq.io.sbuffer)
  val sbReqValid = sbReq.map(_.head.valid)
  sbuffer.io.in.zipWithIndex.foreach { case (in, i) =>
    in.valid := ParallelPriorityMux(sbReqValid, sbReq.map(_(i).valid))
    in.bits := ParallelPriorityMux(sbReqValid, sbReq.map(_(i).bits))
  }
  for (i <- 0 until sbuffer.io.in.length) {
    for (j <- 0 until sbReq.length) {
        sbReq(j)(i).ready := sbuffer.io.in(i).ready && (if (j == 0) true.B else !sbReqValid.take(j).reduce(_||_))
    }
  }
  lsq.io.sqEmpty        <> sbuffer.io.sqempty
  dcache.io.force_write := lsq.io.force_write
  lsq.io.vecStoreRetire <> vsFlowQueue.io.sqRelease
  lsq.io.vecWriteback.valid := vlWrapper.io.uopWriteback.valid &&
    vlWrapper.io.uopWriteback.bits.uop.lastUop
  lsq.io.vecWriteback.bits := vlWrapper.io.uopWriteback.bits

  // vector
  vlWrapper.io.redirect <> redirect
  vsUopQueue.io.redirect <> redirect
  vsFlowQueue.io.redirect <> redirect
  vlWrapper.io.loadRegIn.valid := io.ooo_to_mem.issueVldu.head.valid && FuType.isVLoad(io.ooo_to_mem.issueVldu.head.bits.uop.fuType)
  vlWrapper.io.loadRegIn.bits := io.ooo_to_mem.issueVldu.head.bits
  vsUopQueue.io.storeIn.valid := io.ooo_to_mem.issueVldu.head.valid && FuType.isVStore(io.ooo_to_mem.issueVldu.head.bits.uop.fuType)
  vsUopQueue.io.storeIn.bits := io.ooo_to_mem.issueVldu.head.bits
  io.ooo_to_mem.issueVldu.head.ready := vlWrapper.io.loadRegIn.ready && vsUopQueue.io.storeIn.ready
  vsFlowQueue.io.flowIn <> vsUopQueue.io.flowIssue
  vsUopQueue.io.flowWriteback <> vsFlowQueue.io.flowWriteback
  vsFlowQueue.io.rob.lcommit := io.ooo_to_mem.lsqio.lcommit
  vsFlowQueue.io.rob.scommit := io.ooo_to_mem.lsqio.scommit
  vsFlowQueue.io.rob.pendingld := io.ooo_to_mem.lsqio.pendingld
  vsFlowQueue.io.rob.pendingst := io.ooo_to_mem.lsqio.pendingst
  vsFlowQueue.io.rob.commit := io.ooo_to_mem.lsqio.commit
  vsFlowQueue.io.rob.pendingPtr := io.ooo_to_mem.lsqio.pendingPtr
  vsFlowQueue.io.rob.pendingPtrNext := io.ooo_to_mem.lsqio.pendingPtrNext
  io.mem_to_ooo.writebackVldu.head.valid := vlWrapper.io.uopWriteback.valid || vsUopQueue.io.uopWriteback.valid
  io.mem_to_ooo.writebackVldu.head.bits := Mux1H(Seq(
    vlWrapper.io.uopWriteback.valid -> vlWrapper.io.uopWriteback.bits,
    vsUopQueue.io.uopWriteback.valid -> vsUopQueue.io.uopWriteback.bits,
  ))
  vlWrapper.io.uopWriteback.ready := io.mem_to_ooo.writebackVldu.head.ready
  vsUopQueue.io.uopWriteback.ready := io.mem_to_ooo.writebackVldu.head.ready

  // Sbuffer
  sbuffer.io.csrCtrl    <> csrCtrl
  sbuffer.io.dcache     <> dcache.io.lsu.store
  sbuffer.io.memSetPattenDetected := dcache.io.memSetPattenDetected
  sbuffer.io.force_write <> lsq.io.force_write
  // flush sbuffer
  val fenceFlush = io.ooo_to_mem.flushSb
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  val stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
  io.mem_to_ooo.sbIsEmpty := RegNext(stIsEmpty)

  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := RegNext(fenceFlush || atomicsFlush)
  uncache.io.flush.valid := sbuffer.io.flush.valid

  // AtomicsUnit: AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val s_normal +: s_atomics = Enum(StaCnt + HyuCnt + 1)
  val state = RegInit(s_normal)

  val atomic_rs = (0 until StaCnt).map(LduCnt + _)
  val atomic_replay_port_idx = (0 until StaCnt)
  val st_atomics = Seq.tabulate(StaCnt)(i =>
    io.ooo_to_mem.issueSta(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueSta(i).bits.uop.fuType))
  ) ++ Seq.tabulate(HyuCnt)(i =>
    io.ooo_to_mem.issueHya(i).valid && FuType.storeIsAMO((io.ooo_to_mem.issueHya(i).bits.uop.fuType))
  )

  val st_data_atomics = Seq.tabulate(StdCnt)(i =>
    stData(i).valid && FuType.storeIsAMO(stData(i).bits.uop.fuType)
  )

  for (i <- 0 until StaCnt) when(st_atomics(i)) {
    io.ooo_to_mem.issueSta(i).ready := atomicsUnit.io.in.ready
    storeUnits(i).io.stin.valid := false.B

    state := s_atomics(i)
    assert(!st_atomics.zipWithIndex.filterNot(_._2 == i).unzip._1.reduce(_ || _))
  }
  for (i <- 0 until HyuCnt) when(st_atomics(StaCnt + i)) {
    io.ooo_to_mem.issueHya(i).ready := atomicsUnit.io.in.ready
    hybridUnits(i).io.lsin.valid := false.B

    state := s_atomics(StaCnt + i)
    assert(!st_atomics.zipWithIndex.filterNot(_._2 == StaCnt + i).unzip._1.reduce(_ || _))
  }
  when (atomicsUnit.io.out.valid) {
    assert((0 until StaCnt + HyuCnt).map(state === s_atomics(_)).reduce(_ || _))
    state := s_normal
  }

  atomicsUnit.io.in.valid := st_atomics.reduce(_ || _)
  atomicsUnit.io.in.bits  := Mux1H(Seq.tabulate(StaCnt)(i =>
    st_atomics(i) -> io.ooo_to_mem.issueSta(i).bits) ++
    Seq.tabulate(HyuCnt)(i => st_atomics(StaCnt+i) -> io.ooo_to_mem.issueHya(i).bits))
  atomicsUnit.io.storeDataIn.valid := st_data_atomics.reduce(_ || _)
  atomicsUnit.io.storeDataIn.bits  := Mux1H(Seq.tabulate(StdCnt)(i =>
    st_data_atomics(i) -> stData(i).bits))
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

  when (state =/= s_normal) {
    // use store wb port instead of load
    loadUnits(0).io.ldout.ready := false.B
    // use load_0's TLB
    atomicsUnit.io.dtlb <> amoTlb

    // hw prefetch should be disabled while executing atomic insts
    loadUnits.map(i => i.io.prefetch_req.valid := false.B)

    // make sure there's no in-flight uops in load unit
    assert(!loadUnits(0).io.ldout.valid)
  }

  for (i <- 0 until StaCnt) {
    when (state === s_atomics(i)) {
      io.mem_to_ooo.staIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
      assert(!storeUnits(i).io.feedback_slow.valid)
    }
  }
  for (i <- 0 until HyuCnt) {
    when (state === s_atomics(StaCnt + i)) {
      io.mem_to_ooo.hyuIqFeedback(i).feedbackSlow := atomicsUnit.io.feedbackSlow
      assert(!hybridUnits(i).io.feedback_slow.valid)
    }
  }

  lsq.io.exceptionAddr.isStore := io.ooo_to_mem.isStore
  // Exception address is used several cycles after flush.
  // We delay it by 10 cycles to ensure its flush safety.
  val atomicsException = RegInit(false.B)
  when (DelayN(redirect.valid, 10) && atomicsException) {
    atomicsException := false.B
  }.elsewhen (atomicsUnit.io.exceptionAddr.valid) {
    atomicsException := true.B
  }
  val atomicsExceptionAddress = RegEnable(atomicsUnit.io.exceptionAddr.bits, atomicsUnit.io.exceptionAddr.valid)
  io.mem_to_ooo.lsqio.vaddr := RegNext(Mux(atomicsException, atomicsExceptionAddress, lsq.io.exceptionAddr.vaddr))
  XSError(atomicsException && atomicsUnit.io.in.valid, "new instruction before exception triggers\n")

  io.memInfo.sqFull := RegNext(lsq.io.sqFull)
  io.memInfo.lqFull := RegNext(lsq.io.lqFull)
  io.memInfo.dcacheMSHRFull := RegNext(dcache.io.mshrFull)

  // top-down info
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  dtlbRepeater1.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling

  val hyLdDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isLoad(x.bits.uop.fuType)))
  val hyStDeqCount = PopCount(io.ooo_to_mem.issueHya.map(x => x.valid && FuType.isStore(x.bits.uop.fuType)))
  val ldDeqCount = PopCount(io.ooo_to_mem.issueLda.map(_.valid)) + hyLdDeqCount
  val stDeqCount = PopCount(io.ooo_to_mem.issueSta.take(StaCnt).map(_.valid)) + hyStDeqCount
  val iqDeqCount = ldDeqCount + stDeqCount
  XSPerfAccumulate("load_iq_deq_count", ldDeqCount)
  XSPerfHistogram("load_iq_deq_count", ldDeqCount, true.B, 0, LduCnt, 1)
  XSPerfAccumulate("store_iq_deq_count", stDeqCount)
  XSPerfHistogram("store_iq_deq_count", stDeqCount, true.B, 0, StaCnt, 1)
  XSPerfAccumulate("ls_iq_deq_count", iqDeqCount)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrCtrl.distribute_csr
  val csrevents = pfevent.io.hpmevent.slice(16,24)

  val memBlockPerfEvents = Seq(
    ("ldDeqCount", ldDeqCount),
    ("stDeqCount", stDeqCount),
  )
  val allPerfEvents = memBlockPerfEvents ++ (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val hpmEvents = allPerfEvents.map(_._2.asTypeOf(new PerfEvent)) ++ perfEventsPTW
  val perfEvents = HPerfMonitor(csrevents, hpmEvents).getPerfEvents
  generatePerfEvent()
}
