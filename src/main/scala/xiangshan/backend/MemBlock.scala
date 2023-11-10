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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.tilelink._
import coupledL2.PrefetchRecv
import utils._
import utility._
import xiangshan._
import xiangshan.backend.exu.StdExeUnit
import xiangshan.backend.fu._
import xiangshan.backend.rob.{DebugLSIO, LsTopdownInfo, RobLsqIO, RobPtr, RobDebugRollingIO}
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.prefetch.{BasePrefecher, SMSParams, SMSPrefetcher, L1Prefetcher}

class Std(implicit p: Parameters) extends FunctionUnit {
  io.in.ready := true.B
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := io.in.bits.src(0)
}

class ooo_to_mem(implicit p: Parameters) extends XSBundle {
  val loadFastMatch = Vec(exuParameters.LduCnt, Input(UInt(exuParameters.LduCnt.W)))
  val loadFastFuOpType = Vec(exuParameters.LduCnt, Input(FuOpType()))
  val loadFastImm = Vec(exuParameters.LduCnt, Input(UInt(12.W)))
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  val lsqio = new Bundle {
   val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
   val scommit = Input(UInt(log2Up(CommitWidth + 1).W))
   val pendingld = Input(Bool())
   val pendingst = Input(Bool())
   val commit = Input(Bool())
   val pendingPtr = Input(new RobPtr)
  }

  val isStore = Input(Bool())
  val csrCtrl = Flipped(new CustomCSRCtrlIO)
  val enqLsq = new LsqEnqIO
  val flushSb = Input(Bool())
  val loadPc = Vec(exuParameters.LduCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val storePc = Vec(exuParameters.StuCnt, Input(UInt(VAddrBits.W))) // for hw prefetch
  val issue = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuInput)))
}

class mem_to_ooo(implicit p: Parameters ) extends XSBundle {
  val otherFastWakeup = Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new MicroOp))
  val csrUpdate = new DistributedCSRUpdateReq
  val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
  val stIn = Vec(exuParameters.StuCnt, ValidIO(new ExuInput))
  val stIssuePtr = Output(new SqPtr())

  val memoryViolation = ValidIO(new Redirect)
  val sbIsEmpty = Output(Bool())

  val lsTopdownInfo = Vec(exuParameters.LduCnt, Output(new LsTopdownInfo))

  val lsqio = new Bundle {
    val vaddr = Output(UInt(VAddrBits.W))
    val mmio = Output(Vec(LoadPipelineWidth, Bool()))
    val uop = Output(Vec(LoadPipelineWidth, new MicroOp))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
  }
  val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuOutput))
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

class InstrUncacheBuffer()(implicit p: Parameters) extends LazyModule {
  val node = new TLBufferNode(BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default, BufferParams.default)
  lazy val module = new InstrUncacheBufferImpl

  class InstrUncacheBufferImpl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> BufferParams.default(BufferParams.default(in.a))
      in.d <> BufferParams.default(BufferParams.default(out.d))

      // only a.valid, a.ready, a.address are used, so we assign 0 to the following
      // hoping that them would be optimized to keep MemBlock port unchanged after adding buffer
      out.a.bits.data := 0.U
      out.a.bits.mask := 0.U
      out.a.bits.opcode := 0.U
      out.a.bits.size := 0.U
      out.a.bits.source := 0.U
    }
  }
}

// Frontend bus goes through MemBlock
class FrontendBridge()(implicit p: Parameters) extends LazyModule {
  val icache_node = LazyModule(new TLBuffer()).suggestName("icache").node// to keep IO port name
  val instr_uncache_node = LazyModule(new InstrUncacheBuffer()).suggestName("instr_uncache").node
  lazy val module = new LazyModuleImp(this) {
  }
}

class MemBlock()(implicit p: Parameters) extends LazyModule
  with HasXSParameter with HasWritebackSource {
  override def shouldBeInlined: Boolean = false

  val dcache = LazyModule(new DCacheWrapper())
  val uncache = LazyModule(new Uncache())
  val ptw = LazyModule(new L2TLBWrapper())
  val ptw_to_l2_buffer = if (!coreParams.softPTW) LazyModule(new TLBuffer) else null
  val l1d_to_l2_buffer = if (coreParams.dcacheParametersOpt.nonEmpty) LazyModule(new TLBuffer) else null
  val dcache_port = TLNameNode("dcache_client") // to keep dcache-L2 port name
  val l2_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new PrefetchRecv)
  )
  val l3_pf_sender_opt = coreParams.prefetcher.map(_ =>
    BundleBridgeSource(() => new huancun.PrefetchRecv)
  )
  val frontendBridge = LazyModule(new FrontendBridge)
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))

  if (!coreParams.softPTW) {
    ptw_to_l2_buffer.node := ptw.node
  }

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
  with HasL1PrefetchSourceParameter
  with HasCircularQueuePtrHelper
{

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val redirect = Flipped(ValidIO(new Redirect))

    val ooo_to_mem = new ooo_to_mem
    val mem_to_ooo = new mem_to_ooo
    val fetch_to_mem = new fetch_to_mem

    val rsfeedback = Vec(exuParameters.LsExuCnt, new MemRSFeedbackIO)
    val int2vlsu = Flipped(new Int2VLSUIO)
    val vec2vlsu = Flipped(new Vec2VLSUIO)
    // out
    val s3_delayed_load_error = Vec(exuParameters.LduCnt, Output(Bool()))
    val vlsu2vec = new VLSU2VecIO
    val vlsu2int = new VLSU2IntIO
    val vlsu2ctrl = new VLSU2CtrlIO
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

    // All the signals from/to frontend/backend to/from bus will go through MemBlock
    val externalInterrupt = Flipped(new ExternalInterruptIO)
    val inner_hartId = Output(UInt(64.W))
    val inner_reset_vector = Output(UInt(PAddrBits.W))
    val outer_reset_vector = Input(UInt(PAddrBits.W))
    val inner_cpu_halt = Input(Bool())
    val outer_cpu_halt = Output(Bool())
    val inner_beu_errors_icache = Input(new L1BusErrorUnitInfo)
    val outer_beu_errors_icache = Output(new L1BusErrorUnitInfo)
    val inner_l2_pf_enable = Input(Bool())
    val outer_l2_pf_enable = Output(Bool())
    // val inner_hc_perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    // val outer_hc_perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
  })

  // reset signals of frontend & backend are generated in memblock
  val reset_io_frontend = IO(Output(Reset()))
  val reset_io_backend = IO(Output(Reset()))

  dontTouch(io.externalInterrupt)
  dontTouch(io.inner_hartId)
  dontTouch(io.inner_reset_vector)
  dontTouch(io.outer_reset_vector)
  dontTouch(io.inner_cpu_halt)
  dontTouch(io.outer_cpu_halt)
  dontTouch(io.inner_beu_errors_icache)
  dontTouch(io.outer_beu_errors_icache)
  dontTouch(io.inner_l2_pf_enable)
  dontTouch(io.outer_l2_pf_enable)
  // dontTouch(io.inner_hc_perfEvents)
  // dontTouch(io.outer_hc_perfEvents)

  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.mem_to_ooo.writeback))

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

  val loadUnits = Seq.fill(exuParameters.LduCnt)(Module(new LoadUnit))
  val storeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StoreUnit))
  val stdExeUnits = Seq.fill(exuParameters.StuCnt)(Module(new StdExeUnit))
  val stData = stdExeUnits.map(_.io.out)
  val exeUnits = loadUnits ++ storeUnits
  val l1_pf_req = Wire(Decoupled(new L1PrefetchReq()))
  dcache.io.sms_agt_evict_req.ready := false.B
  val prefetcherOpt: Option[BasePrefecher] = coreParams.prefetcher.map {
    case _: SMSParams =>
      val sms = Module(new SMSPrefetcher())
      sms.io_agt_en := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_agt, 2, Some(false.B))
      sms.io_pht_en := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_enable_pht, 2, Some(false.B))
      sms.io_act_threshold := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_threshold, 2, Some(12.U))
      sms.io_act_stride := RegNextN(io.ooo_to_mem.csrCtrl.l1D_pf_active_stride, 2, Some(30.U))
      sms.io_stride_en := false.B
      sms.io_dcache_evict <> dcache.io.sms_agt_evict_req
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
      for (i <- 0 until exuParameters.LduCnt) {
        val source = loadUnits(i).io.prefetch_train_l1
        l1Prefetcher.stride_train(i).valid := source.valid && source.bits.isFirstIssue && (
          source.bits.miss || isFromStride(source.bits.meta_prefetch)
        )
        l1Prefetcher.stride_train(i).bits := source.bits
        l1Prefetcher.stride_train(i).bits.uop.cf.pc := Mux(loadUnits(i).io.s2_ptr_chasing, io.ooo_to_mem.loadPc(i), RegNext(io.ooo_to_mem.loadPc(i)))
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
  val atomicsUnit = Module(new AtomicsUnit)

  val loadWritebackOverride  = Mux(atomicsUnit.io.out.valid, atomicsUnit.io.out.bits, loadUnits.head.io.ldout.bits)
  val ldout0 = Wire(Decoupled(new ExuOutput))
  ldout0.valid := atomicsUnit.io.out.valid || loadUnits.head.io.ldout.valid
  ldout0.bits  := loadWritebackOverride
  atomicsUnit.io.out.ready := ldout0.ready
  loadUnits.head.io.ldout.ready := ldout0.ready

  val ldExeWbReqs = ldout0 +: loadUnits.tail.map(_.io.ldout)
  io.mem_to_ooo.writeback <> ldExeWbReqs ++ VecInit(storeUnits.map(_.io.stout)) ++ VecInit(stdExeUnits.map(_.io.out))
  io.mem_to_ooo.otherFastWakeup := DontCare
  io.mem_to_ooo.otherFastWakeup.take(2).zip(loadUnits.map(_.io.fast_uop)).foreach{case(a,b)=> a := b}
  val stOut = io.mem_to_ooo.writeback.drop(exuParameters.LduCnt).dropRight(exuParameters.StuCnt)

  // prefetch to l1 req
  // Stream's confidence is always 1
  loadUnits.foreach(load_unit => {
    load_unit.io.prefetch_req.valid <> l1_pf_req.valid
    load_unit.io.prefetch_req.bits <> l1_pf_req.bits
  })
  // NOTE: loadUnits(0) has higher bank conflict and miss queue arb priority than loadUnits(1)
  // when loadUnits(0) stage 0 is busy, hw prefetch will never use that pipeline
  val LowConfPort = 0
  loadUnits(LowConfPort).io.prefetch_req.bits.confidence := 0.U

  l1_pf_req.ready := (0 until exuParameters.LduCnt).map{
    case i => {
      if(i == LowConfPort) {
        loadUnits(i).io.canAcceptLowConfPrefetch
      }else {
        Mux(l1_pf_req.bits.confidence === 1.U, loadUnits(i).io.canAcceptHighConfPrefetch, loadUnits(i).io.canAcceptLowConfPrefetch)
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

    fuzzer.io.req.ready := l1_pf_req.ready
  }

  // TODO: fast load wakeup
  val lsq     = Module(new LsqWrapper)
  val vlsq    = Module(new DummyVectorLsq)
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

  val perfEventsPTW = Wire(Vec(19, new PerfEvent))
  if (!coreParams.softPTW) {
    perfEventsPTW := ptw.getPerf
  } else {
    perfEventsPTW := DontCare
  }

  // dtlb
  val dtlb_ld = VecInit(Seq.fill(1){
    val tlb_ld = Module(new TLBNonBlock(exuParameters.LduCnt + 1, 2, ldtlbParams))
    tlb_ld.io // let the module have name in waveform
  })
  val dtlb_st = VecInit(Seq.fill(1){
    val tlb_st = Module(new TLBNonBlock(exuParameters.StuCnt, 1, sttlbParams))
    tlb_st.io // let the module have name in waveform
  })
  val dtlb_prefetch = VecInit(Seq.fill(1){
    val tlb_prefetch = Module(new TLBNonBlock(1, 2, pftlbParams))
    tlb_prefetch.io // let the module have name in waveform
  })
  val dtlb = dtlb_ld ++ dtlb_st ++ dtlb_prefetch
  val ptwio = Wire(new VectorTlbPtwIO(exuParameters.LduCnt + 1 + exuParameters.StuCnt + 1)) // load + store + hw prefetch
  val dtlb_reqs = dtlb.map(_.requestor).flatten
  val dtlb_pmps = dtlb.map(_.pmp).flatten
  dtlb.map(_.hartId := io.hartId)
  dtlb.map(_.sfence := sfence)
  dtlb.map(_.csr := tlbcsr)
  dtlb.map(_.flushPipe.map(a => a := false.B)) // non-block doesn't need
  if (refillBothTlb) {
    require(ldtlbParams.outReplace == sttlbParams.outReplace)
    require(ldtlbParams.outReplace == pftlbParams.outReplace)
    require(ldtlbParams.outReplace)

    val replace = Module(new TlbReplace(exuParameters.LduCnt + 1 + exuParameters.StuCnt + 1, ldtlbParams))
    replace.io.apply_sep(dtlb_ld.map(_.replace) ++ dtlb_st.map(_.replace) ++ dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.entry.tag)
  } else {
    if (ldtlbParams.outReplace) {
      val replace_ld = Module(new TlbReplace(exuParameters.LduCnt + 1, ldtlbParams))
      replace_ld.io.apply_sep(dtlb_ld.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
    if (sttlbParams.outReplace) {
      val replace_st = Module(new TlbReplace(exuParameters.StuCnt, sttlbParams))
      replace_st.io.apply_sep(dtlb_st.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
    if (pftlbParams.outReplace) {
      val replace_pf = Module(new TlbReplace(1, pftlbParams))
      replace_pf.io.apply_sep(dtlb_prefetch.map(_.replace), ptwio.resp.bits.data.entry.tag)
    }
  }

  val ptw_resp_next = RegEnable(ptwio.resp.bits, ptwio.resp.valid)
  val ptw_resp_v = RegNext(ptwio.resp.valid && !(sfence.valid && tlbcsr.satp.changed), init = false.B)
  ptwio.resp.ready := true.B

  val tlbreplay = WireInit(VecInit(Seq.fill(2)(false.B)))
  dontTouch(tlbreplay)
  for (i <- 0 until 2) {
    tlbreplay(i) := dtlb_ld(0).ptw.req(i).valid && ptw_resp_next.vector(0) && ptw_resp_v &&
      ptw_resp_next.data.hit(dtlb_ld(0).ptw.req(i).bits.vpn, tlbcsr.satp.asid, allType = true, ignoreAsid = true)
  }

  dtlb.flatMap(a => a.ptw.req)
    .zipWithIndex
    .foreach{ case (tlb, i) =>
      tlb.ready := ptwio.req(i).ready
      ptwio.req(i).bits := tlb.bits
    val vector_hit = if (refillBothTlb) Cat(ptw_resp_next.vector).orR
      else if (i < (exuParameters.LduCnt + 1)) Cat(ptw_resp_next.vector.take(exuParameters.LduCnt + 1)).orR
      else if (i < (exuParameters.LduCnt + 1 + exuParameters.StuCnt)) Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt + 1).take(exuParameters.StuCnt)).orR
      else Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt + 1 + exuParameters.StuCnt)).orR
    ptwio.req(i).valid := tlb.valid && !(ptw_resp_v && vector_hit &&
      ptw_resp_next.data.hit(tlb.bits.vpn, tlbcsr.satp.asid, allType = true, ignoreAsid = true))
  }
  dtlb.foreach(_.ptw.resp.bits := ptw_resp_next.data)
  if (refillBothTlb) {
    dtlb.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector).orR)
  } else {
    dtlb_ld.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.take(exuParameters.LduCnt + 1)).orR)
    dtlb_st.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt + 1).take(exuParameters.StuCnt)).orR)
    dtlb_prefetch.foreach(_.ptw.resp.valid := ptw_resp_v && Cat(ptw_resp_next.vector.drop(exuParameters.LduCnt + exuParameters.StuCnt + 1)).orR)
  }

  val dtlbRepeater  = PTWNewFilter(ldtlbParams.fenceDelay, ptwio, ptw.io.tlb(1), sfence, tlbcsr, l2tlbParams.dfilterSize)
  val itlbRepeater2 = PTWRepeaterNB(passReady = false, itlbParams.fenceDelay, io.fetch_to_mem.itlb, ptw.io.tlb(0), sfence, tlbcsr)

  lsq.io.debugTopDown.robHeadMissInDTlb := dtlbRepeater.io.rob_head_miss_in_tlb

  // pmp
  val pmp = Module(new PMP())
  pmp.io.distribute_csr <> csrCtrl.distribute_csr

  val pmp_check = VecInit(Seq.fill(exuParameters.LduCnt + exuParameters.StuCnt + 2)(Module(new PMPChecker(3)).io))
  for ((p,d) <- pmp_check zip dtlb_pmps) {
    p.apply(tlbcsr.priv.dmode, pmp.io.pmp, pmp.io.pma, d)
    require(p.req.bits.size.getWidth == d.bits.size.getWidth)
  }

  for (i <- 0 until exuParameters.LduCnt) {
    io.debug_ls.debugLsInfo(i) := loadUnits(i).io.debug_ls
  }
  for (i <- 0 until exuParameters.StuCnt) {
    io.debug_ls.debugLsInfo(i + exuParameters.LduCnt) := storeUnits(i).io.debug_ls
  }

  io.mem_to_ooo.lsTopdownInfo := loadUnits.map(_.io.lsTopdownInfo)

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

  val fastReplaySel = loadUnits.zipWithIndex.map { case (ldu, i) => {
    val wrapper = Wire(Valid(new BalanceEntry))
    wrapper.valid        := ldu.io.fast_rep_out.valid
    wrapper.bits.req     := ldu.io.fast_rep_out.bits
    wrapper.bits.balance := ldu.io.fast_rep_out.bits.rep_info.bank_conflict
    wrapper.bits.port    := i.U
    wrapper
  }}
  val balanceFastReplaySel = balanceReOrder(fastReplaySel)

  val correctMissTrain = WireInit(Constantin.createRecord("CorrectMissTrain" + p(XSCoreParamsKey).HartId.toString, initValue = 0.U)) === 1.U

  for (i <- 0 until exuParameters.LduCnt) {
    loadUnits(i).io.redirect <> redirect
    loadUnits(i).io.isFirstIssue := true.B

    // get input form dispatch
    loadUnits(i).io.ldin <> io.ooo_to_mem.issue(i)
    loadUnits(i).io.feedback_slow <> io.rsfeedback(i).feedbackSlow
    loadUnits(i).io.feedback_fast <> io.rsfeedback(i).feedbackFast
    loadUnits(i).io.rsIdx := io.rsfeedback(i).rsIdx
    loadUnits(i).io.correctMissTrain := correctMissTrain

    // fast replay
    loadUnits(i).io.fast_rep_in.valid := balanceFastReplaySel(i).valid
    loadUnits(i).io.fast_rep_in.bits := balanceFastReplaySel(i).bits.req

    loadUnits(i).io.fast_rep_out.ready := false.B
    for (j <- 0 until exuParameters.LduCnt) {
      when (balanceFastReplaySel(j).valid && balanceFastReplaySel(j).bits.port === i.U) {
        loadUnits(i).io.fast_rep_out.ready := loadUnits(j).io.fast_rep_in.ready
      }
    }

    // get input form dispatch
    loadUnits(i).io.ldin <> io.ooo_to_mem.issue(i)
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
    loadUnits(i).io.tlb <> dtlb_reqs.take(exuParameters.LduCnt)(i)
    // pmp
    loadUnits(i).io.pmp <> pmp_check(i).resp
    // st-ld violation query
    for (s <- 0 until StorePipelineWidth) {
      loadUnits(i).io.stld_nuke_query(s) := storeUnits(s).io.stld_nuke_query
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
      pf.io.ld_in(i).bits.uop.cf.pc := Mux(loadUnits(i).io.s2_ptr_chasing, io.ooo_to_mem.loadPc(i), RegNext(io.ooo_to_mem.loadPc(i)))
    })
    l1PrefetcherOpt.foreach(pf => {
      // stream will train on all load sources
      val source = loadUnits(i).io.prefetch_train_l1
      pf.io.ld_in(i).valid := source.valid && source.bits.isFirstIssue
      pf.io.ld_in(i).bits := source.bits
      pf.io.st_in(i).valid := false.B
      pf.io.st_in(i).bits := DontCare
    })

    // load to load fast forward: load(i) prefers data(i)
    val fastPriority = (i until exuParameters.LduCnt) ++ (0 until i)
    val fastValidVec = fastPriority.map(j => loadUnits(j).io.l2l_fwd_out.valid)
    val fastDataVec = fastPriority.map(j => loadUnits(j).io.l2l_fwd_out.data)
    val fastErrorVec = fastPriority.map(j => loadUnits(j).io.l2l_fwd_out.dly_ld_err)
    val fastMatchVec = fastPriority.map(j => io.ooo_to_mem.loadFastMatch(i)(j))
    loadUnits(i).io.l2l_fwd_in.valid := VecInit(fastValidVec).asUInt.orR
    loadUnits(i).io.l2l_fwd_in.data := ParallelPriorityMux(fastValidVec, fastDataVec)
    loadUnits(i).io.l2l_fwd_in.dly_ld_err := ParallelPriorityMux(fastValidVec, fastErrorVec)
    val fastMatch = ParallelPriorityMux(fastValidVec, fastMatchVec)
    loadUnits(i).io.ld_fast_match := fastMatch
    loadUnits(i).io.ld_fast_imm := io.ooo_to_mem.loadFastImm(i)
    loadUnits(i).io.ld_fast_fuOpType := io.ooo_to_mem.loadFastFuOpType(i)
    loadUnits(i).io.replay <> lsq.io.replay(i)

    val l2_hint = RegNext(io.l2_hint)
    loadUnits(i).io.l2_hint <> l2_hint
    loadUnits(i).io.tlb_hint.id := dtlbRepeater.io.hint.get.req(i).id
    loadUnits(i).io.tlb_hint.full := dtlbRepeater.io.hint.get.req(i).full ||
      RegNext(tlbreplay(i)) || RegNext(dtlb_ld(0).tlbreplay(i))

    // passdown to lsq (load s2)
    lsq.io.ldu.ldin(i) <> loadUnits(i).io.lsq.ldin
    lsq.io.ldout(i) <> loadUnits(i).io.lsq.uncache
    lsq.io.ld_raw_data(i) <> loadUnits(i).io.lsq.ld_raw_data
    lsq.io.trigger(i) <> loadUnits(i).io.lsq.trigger

    lsq.io.l2_hint.valid := l2_hint.valid
    lsq.io.l2_hint.bits.sourceId := l2_hint.bits.sourceId

    lsq.io.tlb_hint <> dtlbRepeater.io.hint.get

    // alter writeback exception info
    io.s3_delayed_load_error(i) := loadUnits(i).io.s3_dly_ld_err

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
      io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(lTriggerMapping(j)) := hit(j)
//      io.writeback(i).bits.uop.cf.trigger.backendTiming(lTriggerMapping(j)) := tdata(lTriggerMapping(j)).timing
      //      if (lChainMapping.contains(j)) io.writeback(i).bits.uop.cf.trigger.triggerChainVec(lChainMapping(j)) := hit && tdata(j+3).chain
    }
    when(tdata(2).chain) {
      io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(2) := hit(0) && hit(1)
      io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(3) := hit(0) && hit(1)
    }
    when(!io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendEn(1)) {
      io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(5) := false.B
    }

    XSDebug(io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.getHitBackend && io.mem_to_ooo.writeback(i).valid, p"Debug Mode: Load Inst No.${i}" +
    p"has trigger hit vec ${io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit}\n")

  }
  // Prefetcher
  val StreamDTLBPortIndex = exuParameters.LduCnt
  val PrefetcherDTLBPortIndex = exuParameters.LduCnt + exuParameters.StuCnt + 1
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
  for (i <- 0 until exuParameters.StuCnt) {
    val stu = storeUnits(i)

    stdExeUnits(i).io.redirect <> redirect
    stdExeUnits(i).io.fromInt <> io.ooo_to_mem.issue(i + exuParameters.LduCnt + exuParameters.StuCnt)
    stdExeUnits(i).io.fromFp := DontCare
    stdExeUnits(i).io.out := DontCare

    stu.io.redirect      <> redirect
    stu.io.dcache        <> dcache.io.lsu.sta(i)
    stu.io.feedback_slow <> io.rsfeedback(exuParameters.LduCnt + i).feedbackSlow
    stu.io.rsIdx         <> io.rsfeedback(exuParameters.LduCnt + i).rsIdx
    // NOTE: just for dtlb's perf cnt
    stu.io.isFirstIssue <> io.rsfeedback(exuParameters.LduCnt + i).isFirstIssue
    stu.io.stin         <> io.ooo_to_mem.issue(exuParameters.LduCnt + i)
    stu.io.lsq          <> lsq.io.sta.storeAddrIn(i)
    stu.io.lsq_replenish <> lsq.io.sta.storeAddrInRe(i)
    // dtlb
    stu.io.tlb          <> dtlb_reqs.drop(exuParameters.LduCnt + 1)(i)
    stu.io.pmp          <> pmp_check(exuParameters.LduCnt + 1 + i).resp

    // prefetch
    stu.io.prefetch_req <> sbuffer.io.store_prefetch(i)

    // store unit does not need fast feedback
    io.rsfeedback(exuParameters.LduCnt + i).feedbackFast := DontCare

    // Lsq to sta unit
    lsq.io.sta.storeMaskIn(i) <> stu.io.st_mask_out

    // Lsq to std unit's rs
    lsq.io.std.storeDataIn(i) := stData(i)

    // store prefetch train
    prefetcherOpt.foreach(pf => {
      pf.io.st_in(i).valid := Mux(pf_train_on_hit,
        stu.io.prefetch_train.valid,
        stu.io.prefetch_train.valid && stu.io.prefetch_train.bits.isFirstIssue && (
          stu.io.prefetch_train.bits.miss
          )
      )
      pf.io.st_in(i).bits := stu.io.prefetch_train.bits
      pf.io.st_in(i).bits.uop.cf.pc := RegNext(io.ooo_to_mem.storePc(i))
    })

    // 1. sync issue info to store set LFST
    // 2. when store issue, broadcast issued sqPtr to wake up the following insts
    // io.stIn(i).valid := io.issue(exuParameters.LduCnt + i).valid
    // io.stIn(i).bits := io.issue(exuParameters.LduCnt + i).bits
    io.mem_to_ooo.stIn(i).valid := stu.io.issue.valid
    io.mem_to_ooo.stIn(i).bits := stu.io.issue.bits

    stu.io.stout.ready := true.B

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
       stOut(i).bits.uop.cf.trigger.backendHit(sTriggerMapping(j)) := hit(j)
     }

     when(tdata(0).chain) {
       io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(0) := hit(0) && hit(1)
       io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit(1) := hit(0) && hit(1)
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

  when (atomicsUnit.io.out.valid) {
    // when atom inst writeback, surpress normal load trigger
    (0 until exuParameters.LduCnt).map(i => {
      io.mem_to_ooo.writeback(i).bits.uop.cf.trigger.backendHit := VecInit(Seq.fill(6)(false.B))
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

  //  lsq.io.rob            <> io.lsqio.rob
  lsq.io.enq            <> io.ooo_to_mem.enqLsq
  lsq.io.brqRedirect    <> redirect

  //  violation rollback
  def selectOldest[T <: Redirect](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).robIdx, bits(1).robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }
  val rollback = lsq.io.rollback +: loadUnits.map(_.io.rollback)
  val rollbackSel = selectOldest(rollback.map(_.valid), rollback.map(_.bits))
  io.mem_to_ooo.memoryViolation.valid := rollbackSel._1(0)
  io.mem_to_ooo.memoryViolation.bits  := rollbackSel._2(0)
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
  lsq.io.tl_d_channel <> dcache.io.lsu.tl_d_channel

  // LSQ to store buffer
  lsq.io.sbuffer        <> sbuffer.io.in
  lsq.io.sqEmpty        <> sbuffer.io.sqempty
  dcache.io.force_write := lsq.io.force_write
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
    io.ooo_to_mem.issue(atomic_rs(i)).valid && FuType.storeIsAMO((io.ooo_to_mem.issue(atomic_rs(i)).bits.uop.ctrl.fuType))
  )

  val st_data_atomics = Seq.tabulate(exuParameters.StuCnt)(i =>
    stData(i).valid && FuType.storeIsAMO(stData(i).bits.uop.ctrl.fuType)
  )

  for (i <- 0 until exuParameters.StuCnt) when(st_atomics(i)) {
    io.ooo_to_mem.issue(atomic_rs(i)).ready := atomicsUnit.io.in.ready
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
    st_atomics(i) -> io.ooo_to_mem.issue(atomic_rs(i)).bits))
  atomicsUnit.io.storeDataIn.valid := st_data_atomics.reduce(_ || _)
  atomicsUnit.io.storeDataIn.bits  := Mux1H(Seq.tabulate(exuParameters.StuCnt)(i =>
    st_data_atomics(i) -> stData(i).bits))
  atomicsUnit.io.rsIdx    := Mux1H(Seq.tabulate(exuParameters.StuCnt)(i =>
    st_atomics(i) -> io.rsfeedback(atomic_rs(i)).rsIdx))
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

  for (i <- 0 until exuParameters.StuCnt) when (state === s_atomics(i)) {
    atomicsUnit.io.feedbackSlow <> io.rsfeedback(atomic_rs(i)).feedbackSlow

    assert(!storeUnits(i).io.feedback_slow.valid)
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

  io.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
  io.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
  io.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
  io.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
  io.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)
  io.inner_hartId := io.hartId
  io.inner_reset_vector := io.outer_reset_vector
  io.outer_cpu_halt := io.inner_cpu_halt
  io.outer_beu_errors_icache := RegNext(io.inner_beu_errors_icache)
  io.outer_l2_pf_enable := io.inner_l2_pf_enable
  // io.inner_hc_perfEvents <> io.outer_hc_perfEvents

  if (p(DebugOptionsKey).FPGAPlatform) {
    val resetTree = ResetGenNode(
      Seq(
        ResetGenNode(Seq(ResetGenNode(Seq(CellNode(reset_io_frontend))))),
        CellNode(reset_io_backend),
        ModuleNode(itlbRepeater2),
        ModuleNode(dtlbRepeater),
        ModuleNode(ptw),
        ModuleNode(ptw_to_l2_buffer)
      )
    )
    ResetGen(resetTree, reset, !p(DebugOptionsKey).FPGAPlatform)
  } else {
    reset_io_frontend := DontCare
    reset_io_backend := DontCare
  }

  // top-down info
  dcache.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  dtlbRepeater.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  lsq.io.debugTopDown.robHeadVaddr := io.debugTopDown.robHeadVaddr
  io.debugTopDown.toCore.robHeadMissInDCache := dcache.io.debugTopDown.robHeadMissInDCache
  io.debugTopDown.toCore.robHeadTlbReplay := lsq.io.debugTopDown.robHeadTlbReplay
  io.debugTopDown.toCore.robHeadTlbMiss := lsq.io.debugTopDown.robHeadTlbMiss
  io.debugTopDown.toCore.robHeadLoadVio := lsq.io.debugTopDown.robHeadLoadVio
  io.debugTopDown.toCore.robHeadLoadMSHR := lsq.io.debugTopDown.robHeadLoadMSHR
  dcache.io.debugTopDown.robHeadOtherReplay := lsq.io.debugTopDown.robHeadOtherReplay
  dcache.io.debugRolling := io.debugRolling

  val ldDeqCount = PopCount(io.ooo_to_mem.issue.take(exuParameters.LduCnt).map(_.valid))
  val stDeqCount = PopCount(io.ooo_to_mem.issue.drop(exuParameters.LduCnt).map(_.valid))
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

  val perfFromUnits = (loadUnits ++ Seq(sbuffer, lsq, dcache)).flatMap(_.getPerfEvents)
  val perfFromPTW    = perfEventsPTW.map(x => ("perfEventsPTW", x.value))
  val perfBlock     = Seq(("ldDeqCount", ldDeqCount),
                          ("stDeqCount", stDeqCount))
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromPTW ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("MemBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
}
