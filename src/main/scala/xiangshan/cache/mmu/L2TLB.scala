/***************************************************************************************
* Copyright (c) 2021-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
* Copyright (c) 2024-2025 Institute of Information Engineering, Chinese Academy of Sciences
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

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMP, PMPChecker, PMPReqBundle, PMPRespBundle}
import xiangshan.backend.fu.util.HasCSRConst
import difftest._

class L2TLB()(implicit p: Parameters) extends LazyModule with HasPtwConst {
  override def shouldBeInlined: Boolean = false

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw",
      sourceId = IdRange(0, MemReqWidth)
    )),
    requestFields = Seq(ReqSourceField())
  )))

  lazy val module = new L2TLBImp(this)
}

class L2TLBImp(outer: L2TLB)(implicit p: Parameters) extends PtwModule(outer) with HasCSRConst with HasPerfEvents {

  val (mem, edge) = outer.node.out.head

  val io = IO(new L2TLBIO)
  val difftestIO = IO(new Bundle() {
    val ptwResp = Output(Bool())
    val ptwAddr = Output(UInt(64.W))
    val ptwData = Output(Vec(4, UInt(64.W)))
  })

  /* Ptw processes multiple requests
   * Divide Ptw procedure into two stages: cache access ; mem access if cache miss
   *           miss queue itlb       dtlb
   *               |       |         |
   *               ------arbiter------
   *                            |
   *                    l1 - l2 - l3 - sp
   *                            |
   *          -------------------------------------------
   *    miss  |  queue                                  | hit
   *    [][][][][][]                                    |
   *          |                                         |
   *    state machine accessing mem                     |
   *          |                                         |
   *          ---------------arbiter---------------------
   *                 |                    |
   *                itlb                 dtlb
   */

  difftestIO <> DontCare

  val sfence_tmp = DelayN(io.sfence, 1)
  val csr_tmp    = DelayN(io.csr.tlb, 1)
  val sfence_dup = Seq.fill(if (HasBitmapCheck) 11 else 9)(RegNext(sfence_tmp))
  val csr_dup = Seq.fill(if (HasBitmapCheck) 10 else 8)(RegNext(csr_tmp)) // TODO: add csr_modified?
  val satp   = csr_dup(0).satp
  val vsatp  = csr_dup(0).vsatp
  val hgatp  = csr_dup(0).hgatp
  val priv   = csr_dup(0).priv
  val mPBMTE = csr_dup(0).mPBMTE
  val hPBMTE = csr_dup(0).hPBMTE
  val flush  = sfence_dup(0).valid || satp.changed || vsatp.changed || hgatp.changed || priv.virt_changed

  val pmp = Module(new PMP())
  val pmp_check = VecInit(Seq.fill(if (HasBitmapCheck) 5 else 4)(Module(new PMPChecker(lgMaxSize = 3, sameCycle = true)).io))
  pmp.io.distribute_csr := io.csr.distribute_csr
  if (HasBitmapCheck) {
    pmp_check.foreach(_.check_env.apply(csr_dup(0).mbmc.CMODE.asBool, ModeS, pmp.io.pmp, pmp.io.pma))
  } else {
    pmp_check.foreach(_.check_env.apply(ModeS, pmp.io.pmp, pmp.io.pma))
  }

  // add bitmapcheck
  val bitmap = Option.when(HasBitmapCheck)(Module(new Bitmap))
  val bitmapcache = Option.when(HasBitmapCheck)(Module(new BitmapCache))

  if (HasBitmapCheck) {
    bitmap.foreach { Bitmap =>
      Bitmap.io.csr := csr_dup(8)
      Bitmap.io.sfence := sfence_dup(9)
      bitmapcache.foreach { BitmapCache =>
        // connect bitmap and bitmapcache
        BitmapCache.io.req <> Bitmap.io.cache.req
        Bitmap.io.cache.resp <> BitmapCache.io.resp
        BitmapCache.io.refill <> Bitmap.io.refill
        BitmapCache.io.csr := csr_dup(9)
        BitmapCache.io.sfence := sfence_dup(10)
      }
    }
  }

  val missQueue = Module(new L2TlbMissQueue)
  val cache = Module(new PtwCache)
  val ptw = Module(new PTW)
  val hptw = Module(new HPTW)
  val llptw = Module(new LLPTW)
  val blockmq = Module(new BlockHelper(3))
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new L2TlbWithHptwIdBundle, ((if (l2tlbParams.enablePrefetch) 4 else 3) + (if(HasHExtension) 1 else 0))))
  val hptw_req_arb = Module(new Arbiter(new Bundle {
    val id = UInt(log2Up(l2tlbParams.llptwsize).W)
    val source = UInt(bSourceWidth.W)
    val gvpn = UInt(gvpnLen.W)
  }, 2))
  val hptw_resp_arb = Module(new Arbiter(new Bundle {
    val resp = new HptwResp()
    val id = UInt(log2Up(l2tlbParams.llptwsize).W)
  }, 2))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new Bundle {
    val s2xlate = UInt(2.W)
    val s1 = new PtwSectorResp ()
    val s2 = new HptwResp()
  }, 1)).io)
  val mergeArb = (0 until PtwWidth).map(i => Module(new Arbiter(new Bundle {
    val s2xlate = UInt(2.W)
    val s1 = new PtwMergeResp()
    val s2 = new HptwResp()
  }, 3)).io)
  val outArbCachePort = 0
  val outArbFsmPort = 1
  val outArbMqPort = 2

  if (HasBitmapCheck) {
    // connect ptwcache and bitmap sleep-wakeup port
    cache.io.bitmap_wakeup.get <> bitmap.get.io.wakeup
  }

  // hptw arb input port
  val InHptwArbPTWPort = 0
  val InHptwArbLLPTWPort = 1
  hptw_req_arb.io.in(InHptwArbPTWPort).valid := ptw.io.hptw.req.valid
  hptw_req_arb.io.in(InHptwArbPTWPort).bits.gvpn := ptw.io.hptw.req.bits.gvpn
  hptw_req_arb.io.in(InHptwArbPTWPort).bits.id := ptw.io.hptw.req.bits.id
  hptw_req_arb.io.in(InHptwArbPTWPort).bits.source := ptw.io.hptw.req.bits.source
  ptw.io.hptw.req.ready := hptw_req_arb.io.in(InHptwArbPTWPort).ready

  hptw_req_arb.io.in(InHptwArbLLPTWPort).valid := llptw.io.hptw.req.valid
  hptw_req_arb.io.in(InHptwArbLLPTWPort).bits.gvpn := llptw.io.hptw.req.bits.gvpn
  hptw_req_arb.io.in(InHptwArbLLPTWPort).bits.id := llptw.io.hptw.req.bits.id
  hptw_req_arb.io.in(InHptwArbLLPTWPort).bits.source := llptw.io.hptw.req.bits.source
  llptw.io.hptw.req.ready := hptw_req_arb.io.in(InHptwArbLLPTWPort).ready

  // arb2 input port
  val InArbHPTWPort = 0
  val InArbPTWPort = 1
  val InArbMissQueuePort = 2
  val InArbTlbPort = 3
  val InArbPrefetchPort = 4
  // NOTE: when cache out but miss and ptw doesnt accept,
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))

  val tlbCounter = RegInit(0.U(log2Ceil(MissQueueSize + 1).W))
  val reqVec = WireInit(VecInit(Seq.fill(PtwWidth)(false.B)))
  val respVec = WireInit(VecInit(Seq.fill(PtwWidth)(false.B)))

  for (i <- 0 until PtwWidth) {
    when (io.tlb(i).req(0).fire) {
      reqVec(i) := true.B
    }
    when (io.tlb(i).resp.fire) {
      respVec(i) := true.B
    }
  }

  when (flush) {
    tlbCounter := 0.U
  } .otherwise {
    tlbCounter := tlbCounter + PopCount(reqVec) - PopCount(respVec)
  }
  XSError(!(tlbCounter >= 0.U && tlbCounter <= MissQueueSize.U), s"l2tlb full!")

  arb2.io.in(InArbPTWPort).valid := ptw.io.llptw.valid
  arb2.io.in(InArbPTWPort).bits.req_info := ptw.io.llptw.bits.req_info
  arb2.io.in(InArbPTWPort).bits.isHptwReq := false.B
  arb2.io.in(InArbPTWPort).bits.isLLptw := false.B
  arb2.io.in(InArbPTWPort).bits.hptwId := DontCare
  ptw.io.llptw.ready := arb2.io.in(InArbPTWPort).ready
  block_decoupled(missQueue.io.out, arb2.io.in(InArbMissQueuePort), Mux(missQueue.io.out.bits.isLLptw, !llptw.io.in.ready, !ptw.io.req.ready))

  arb2.io.in(InArbTlbPort).valid := arb1.io.out.fire
  arb2.io.in(InArbTlbPort).bits.req_info.vpn := arb1.io.out.bits.vpn
  arb2.io.in(InArbTlbPort).bits.req_info.s2xlate := arb1.io.out.bits.s2xlate
  arb2.io.in(InArbTlbPort).bits.req_info.source := arb1.io.chosen
  arb2.io.in(InArbTlbPort).bits.isHptwReq := false.B
  arb2.io.in(InArbTlbPort).bits.isLLptw := false.B
  arb2.io.in(InArbTlbPort).bits.hptwId := DontCare
  // 1. arb1 and arb2 are both comb logic, so ready can work just the same cycle
  // 2. arb1 can send one req at most in a cycle, so do not need to write
  //    "tlbCounter <= (MissQueueSize - 2).U"
  arb1.io.out.ready := arb2.io.in(InArbTlbPort).ready && tlbCounter < MissQueueSize.U

  arb2.io.in(InArbHPTWPort).valid := hptw_req_arb.io.out.valid
  arb2.io.in(InArbHPTWPort).bits.req_info.vpn := hptw_req_arb.io.out.bits.gvpn
  arb2.io.in(InArbHPTWPort).bits.req_info.s2xlate := onlyStage2
  arb2.io.in(InArbHPTWPort).bits.req_info.source := hptw_req_arb.io.out.bits.source
  arb2.io.in(InArbHPTWPort).bits.isHptwReq := true.B
  arb2.io.in(InArbHPTWPort).bits.isLLptw := false.B
  arb2.io.in(InArbHPTWPort).bits.hptwId := hptw_req_arb.io.out.bits.id
  hptw_req_arb.io.out.ready := arb2.io.in(InArbHPTWPort).ready
  val hartId = p(XSCoreParamsKey).HartId
  if (l2tlbParams.enablePrefetch) {
    val prefetch = Module(new L2TlbPrefetch())
    val recv = cache.io.resp
    // NOTE: 1. prefetch doesn't gen prefetch 2. req from mq doesn't gen prefetch
    // NOTE: 1. miss req gen prefetch 2. hit but prefetched gen prefetch
    prefetch.io.in.valid := recv.fire && !from_pre(recv.bits.req_info.source) && (!recv.bits.hit  ||
      recv.bits.prefetch) && recv.bits.isFirst
    prefetch.io.in.bits.vpn := recv.bits.req_info.vpn
    prefetch.io.sfence := sfence_dup(0)
    prefetch.io.csr := csr_dup(0)
    arb2.io.in(InArbPrefetchPort) <> prefetch.io.out

    val isWriteL2TlbPrefetchTable = Constantin.createRecord(s"isWriteL2TlbPrefetchTable$hartId")
    val L2TlbPrefetchTable = ChiselDB.createTable(s"L2TlbPrefetch_hart$hartId", new L2TlbPrefetchDB)
    val L2TlbPrefetchDB = Wire(new L2TlbPrefetchDB)
    L2TlbPrefetchDB.vpn := prefetch.io.out.bits.req_info.vpn
    L2TlbPrefetchTable.log(L2TlbPrefetchDB, isWriteL2TlbPrefetchTable.orR && prefetch.io.out.fire, "L2TlbPrefetch", clock, reset)
  }
  arb2.io.out.ready := cache.io.req.ready

  // Instructs jmp_bitmap_check requests from cache need go to LLPTW/PTW for processing
  val toFsm_toLLPTW = if (HasBitmapCheck) cache.io.resp.bits.toFsm.bitmapCheck.get.jmp_bitmap_check && cache.io.resp.bits.toFsm.bitmapCheck.get.toLLPTW else false.B
  val toFsm_toPTW = if (HasBitmapCheck) cache.io.resp.bits.toFsm.bitmapCheck.get.jmp_bitmap_check && !cache.io.resp.bits.toFsm.bitmapCheck.get.toLLPTW else false.B

  val mq_arb = Module(new Arbiter(new L2TlbWithHptwIdBundle, 2))
  mq_arb.io.in(0).valid := cache.io.resp.valid && !cache.io.resp.bits.hit &&
    !from_pre(cache.io.resp.bits.req_info.source) && !cache.io.resp.bits.isHptwReq && // hptw reqs are not sent to missqueue
    (cache.io.resp.bits.bypassed || (
      ((((!cache.io.resp.bits.toFsm.l1Hit && !toFsm_toLLPTW) || toFsm_toPTW) || cache.io.resp.bits.toFsm.stage1Hit) && !cache.io.resp.bits.isHptwReq && (cache.io.resp.bits.isFirst || !ptw.io.req.ready)) // send to ptw, is first or ptw is busy;
      || (((cache.io.resp.bits.toFsm.l1Hit && !toFsm_toPTW) || toFsm_toLLPTW) && !llptw.io.in.ready) // send to llptw, llptw is full
    ))

  mq_arb.io.in(0).bits.req_info :=  cache.io.resp.bits.req_info
  mq_arb.io.in(0).bits.isHptwReq := false.B
  mq_arb.io.in(0).bits.hptwId :=  DontCare
  mq_arb.io.in(0).bits.isLLptw := (cache.io.resp.bits.toFsm.l1Hit && !toFsm_toPTW) || toFsm_toLLPTW
  mq_arb.io.in(1).bits.req_info := llptw.io.cache.bits
  mq_arb.io.in(1).bits.isHptwReq := false.B
  mq_arb.io.in(1).bits.hptwId := DontCare
  mq_arb.io.in(1).bits.isLLptw := false.B
  mq_arb.io.in(1).valid := llptw.io.cache.valid
  llptw.io.cache.ready := mq_arb.io.in(1).ready
  missQueue.io.in <> mq_arb.io.out
  missQueue.io.sfence  := sfence_dup(6)
  missQueue.io.csr := csr_dup(5)

  blockmq.io.start := missQueue.io.out.fire
  blockmq.io.enable := ptw.io.req.fire

  llptw.io.in.valid := cache.io.resp.valid &&
    !cache.io.resp.bits.hit &&
    (toFsm_toLLPTW || (cache.io.resp.bits.toFsm.l1Hit && !toFsm_toPTW)) &&
    !cache.io.resp.bits.bypassed &&
    !cache.io.resp.bits.isHptwReq
  llptw.io.in.bits.req_info := cache.io.resp.bits.req_info
  llptw.io.in.bits.ppn := cache.io.resp.bits.toFsm.ppn
  if (HasBitmapCheck) {
    llptw.io.in.bits.bitmapCheck.get.jmp_bitmap_check := cache.io.resp.bits.toFsm.bitmapCheck.get.jmp_bitmap_check
    llptw.io.in.bits.bitmapCheck.get.ptes := cache.io.resp.bits.toFsm.bitmapCheck.get.ptes
    llptw.io.in.bits.bitmapCheck.get.cfs := cache.io.resp.bits.toFsm.bitmapCheck.get.cfs
    llptw.io.in.bits.bitmapCheck.get.hitway := cache.io.resp.bits.toFsm.bitmapCheck.get.hitway
  }
  llptw.io.sfence := sfence_dup(1)
  llptw.io.csr := csr_dup(1)
  val llptw_stage1 = Reg(Vec(l2tlbParams.llptwsize, new PtwMergeResp()))
  when(llptw.io.in.fire){
    llptw_stage1(llptw.io.mem.enq_ptr) := cache.io.resp.bits.stage1
  }

  cache.io.req.valid := arb2.io.out.fire
  cache.io.req.bits.req_info := arb2.io.out.bits.req_info
  cache.io.req.bits.isFirst := (arb2.io.chosen =/= InArbMissQueuePort.U && !arb2.io.out.bits.isHptwReq)
  cache.io.req.bits.isHptwReq := arb2.io.out.bits.isHptwReq
  cache.io.req.bits.hptwId := arb2.io.out.bits.hptwId
  cache.io.req.bits.bypassed.map(_ := false.B)
  cache.io.sfence := sfence_dup(2)
  cache.io.csr := csr_dup(2)
  cache.io.sfence_dup.zip(sfence_dup.drop(2).take(4)).map(s => s._1 := s._2)
  cache.io.csr_dup.zip(csr_dup.drop(2).take(3)).map(c => c._1 := c._2)
  cache.io.resp.ready := MuxCase(mq_arb.io.in(0).ready || ptw.io.req.ready, Seq(
    (!cache.io.resp.bits.hit && cache.io.resp.bits.isHptwReq) -> hptw.io.req.ready,
    (cache.io.resp.bits.hit && cache.io.resp.bits.isHptwReq) -> hptw_resp_arb.io.in(HptwRespArbCachePort).ready,
    cache.io.resp.bits.hit -> outReady(cache.io.resp.bits.req_info.source, outArbCachePort),
    ((toFsm_toLLPTW || (cache.io.resp.bits.toFsm.l1Hit && !toFsm_toPTW)) && !cache.io.resp.bits.bypassed && llptw.io.in.ready) -> llptw.io.in.ready,
    (cache.io.resp.bits.bypassed || cache.io.resp.bits.isFirst) -> mq_arb.io.in(0).ready
  ))

  // NOTE: missQueue req has higher priority
  ptw.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && ((!cache.io.resp.bits.toFsm.l1Hit && !toFsm_toLLPTW) || toFsm_toPTW) &&
    !cache.io.resp.bits.bypassed &&
    !cache.io.resp.bits.isFirst &&
    !cache.io.resp.bits.isHptwReq
  ptw.io.req.bits.req_info := cache.io.resp.bits.req_info
  if (EnableSv48) {
    ptw.io.req.bits.l3Hit.get := cache.io.resp.bits.toFsm.l3Hit.get
  }
  ptw.io.req.bits.l2Hit := cache.io.resp.bits.toFsm.l2Hit
  ptw.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  ptw.io.req.bits.stage1Hit := cache.io.resp.bits.toFsm.stage1Hit
  ptw.io.req.bits.stage1 := cache.io.resp.bits.stage1
  if (HasBitmapCheck) {
    ptw.io.req.bits.bitmapCheck.get.jmp_bitmap_check := cache.io.resp.bits.toFsm.bitmapCheck.get.jmp_bitmap_check
    ptw.io.req.bits.bitmapCheck.get.pte := cache.io.resp.bits.toFsm.bitmapCheck.get.pte
    ptw.io.req.bits.bitmapCheck.get.cfs := cache.io.resp.bits.toFsm.bitmapCheck.get.cfs
    ptw.io.req.bits.bitmapCheck.get.SPlevel := cache.io.resp.bits.toFsm.bitmapCheck.get.SPlevel
  }
  ptw.io.sfence := sfence_dup(7)
  ptw.io.csr := csr_dup(6)
  ptw.io.resp.ready := outReady(ptw.io.resp.bits.source, outArbFsmPort)

  hptw.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.isHptwReq
  hptw.io.req.bits.gvpn := cache.io.resp.bits.req_info.vpn
  hptw.io.req.bits.id := cache.io.resp.bits.toHptw.id
  hptw.io.req.bits.source := cache.io.resp.bits.req_info.source
  if (EnableSv48) {
    hptw.io.req.bits.l3Hit.get := cache.io.resp.bits.toHptw.l3Hit.get
  }
  hptw.io.req.bits.l2Hit := cache.io.resp.bits.toHptw.l2Hit
  hptw.io.req.bits.l1Hit := cache.io.resp.bits.toHptw.l1Hit
  hptw.io.req.bits.ppn := cache.io.resp.bits.toHptw.ppn
  hptw.io.req.bits.bypassed := cache.io.resp.bits.toHptw.bypassed
  if (HasBitmapCheck) {
    hptw.io.req.bits.bitmapCheck.get <> cache.io.resp.bits.toHptw.bitmapCheck.get
  }
  hptw.io.sfence := sfence_dup(8)
  hptw.io.csr := csr_dup(7)
  // mem req
  def blockBytes_align(addr: UInt) = {
    Cat(addr(PAddrBits - 1, log2Up(l2tlbParams.blockBytes)), 0.U(log2Up(l2tlbParams.blockBytes).W))
  }
  def addr_low_from_vpn(vpn: UInt) = {
    vpn(log2Ceil(l2tlbParams.blockBytes)-log2Ceil(XLEN/8)-1, 0)
  }
  def addr_low_from_paddr(paddr: UInt) = {
    paddr(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
  }
  def from_llptw(id: UInt) = {
    id < l2tlbParams.llptwsize.U
  }
  def from_ptw(id: UInt) = {
    id === l2tlbParams.llptwsize.U
  }
  def from_hptw(id: UInt) = {
    id === l2tlbParams.llptwsize.U + 1.U
  }
  def from_bitmap(id: UInt) = {
    (id > l2tlbParams.llptwsize.U + 1.U) && (id < MemReqWidth.U)
  }
  val waiting_resp = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  val flush_latch = RegInit(VecInit(Seq.fill(MemReqWidth)(false.B)))
  val hptw_bypassed = RegInit(false.B)
  for (i <- waiting_resp.indices) {
    assert(!flush_latch(i) || waiting_resp(i)) // when sfence_latch wait for mem resp, waiting_resp should be true
  }

  val wfiReq = DelayN(io.wfi.wfiReq, 1)

  // When no left pending response from L2, can be safe to enter wfi
  io.wfi.wfiSafe := DelayN(wfiReq && !waiting_resp.reduce(_ || _), 1)

  val llptw_out = llptw.io.out
  val llptw_mem = llptw.io.mem
  llptw_mem.flush_latch := flush_latch.take(l2tlbParams.llptwsize)
  llptw_mem.req_mask := waiting_resp.take(l2tlbParams.llptwsize)
  ptw.io.mem.mask := waiting_resp.apply(l2tlbParams.llptwsize)
  hptw.io.mem.mask := waiting_resp.apply(l2tlbParams.llptwsize + 1)
  if (HasBitmapCheck) {
    bitmap.get.io.mem.req_mask := waiting_resp.slice(MemReqWidth - (l2tlbParams.llptwsize + 2), MemReqWidth)
  }
  val mem_arb = Module(new Arbiter(new L2TlbMemReqBundle(), if (HasBitmapCheck) 4 else 3))
  mem_arb.io.in(0) <> ptw.io.mem.req
  mem_arb.io.in(1) <> llptw_mem.req
  mem_arb.io.in(2) <> hptw.io.mem.req
  if (HasBitmapCheck) {
    mem_arb.io.in(3) <> bitmap.get.io.mem.req
  }
  mem_arb.io.out.ready := mem.a.ready && !flush && !wfiReq

  // // assert, should not send mem access at same addr for twice.
  // val last_resp_vpn = RegEnable(cache.io.refill.bits.req_info_dup(0).vpn, cache.io.refill.valid)
  // val last_resp_s2xlate = RegEnable(cache.io.refill.bits.req_info_dup(0).s2xlate, cache.io.refill.valid)
  // val last_resp_level = RegEnable(cache.io.refill.bits.level_dup(0), cache.io.refill.valid)
  // val last_resp_v = RegInit(false.B)
  // val last_has_invalid = !Cat(cache.io.refill.bits.ptes.asTypeOf(Vec(blockBits/XLEN, UInt(XLEN.W))).map(a => a(0))).andR || cache.io.refill.bits.sel_pte_dup(0).asTypeOf(new PteBundle).isAf()
  // when (cache.io.refill.valid) { last_resp_v := !last_has_invalid}
  // when (flush) { last_resp_v := false.B }
  // XSError(last_resp_v && cache.io.refill.valid &&
  //   (cache.io.refill.bits.req_info_dup(0).vpn === last_resp_vpn) &&
  //   (cache.io.refill.bits.level_dup(0) === last_resp_level) &&
  //   (cache.io.refill.bits.req_info_dup(0).s2xlate === last_resp_s2xlate),
  //   "l2tlb should not access mem at same addr for twice")
  // // ATTENTION: this may wrongly assert when: a ptes is l2, last part is valid,
  // // but the current part is invalid, so one more mem access happened
  // // If this happened, remove the assert.

  val req_addr_low = Reg(Vec(MemReqWidth, UInt((log2Up(l2tlbParams.blockBytes)-log2Up(XLEN/8)).W)))

  when (llptw.io.in.fire) {
    // when enq miss queue, set the req_addr_low to receive the mem resp data part
    req_addr_low(llptw_mem.enq_ptr) := addr_low_from_vpn(llptw.io.in.bits.req_info.vpn)
  }
  when (mem_arb.io.out.fire) {
    req_addr_low(mem_arb.io.out.bits.id) := addr_low_from_paddr(mem_arb.io.out.bits.addr)
    waiting_resp(mem_arb.io.out.bits.id) := true.B
    hptw_bypassed := from_hptw(mem_arb.io.out.bits.id) && mem_arb.io.out.bits.hptw_bypassed
  }
  // mem read
  val memRead =  edge.Get(
    fromSource = mem_arb.io.out.bits.id,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = blockBytes_align(mem_arb.io.out.bits.addr),
    lgSize     = log2Up(l2tlbParams.blockBytes).U
  )._2
  mem.a.bits := memRead
  mem.a.valid := mem_arb.io.out.valid && !flush && !wfiReq
  mem.a.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.PTW.id.U)
  mem.d.ready := true.B
  // mem -> data buffer
  val refill_data = RegInit(VecInit.fill(blockBits / l1BusDataWidth)(0.U(l1BusDataWidth.W)))
  val refill_helper = edge.firstlastHelper(mem.d.bits, mem.d.fire)
  val mem_resp_done = refill_helper._3
  val mem_resp_from_llptw = from_llptw(mem.d.bits.source)
  val mem_resp_from_ptw = from_ptw(mem.d.bits.source)
  val mem_resp_from_hptw = from_hptw(mem.d.bits.source)
  val mem_resp_from_bitmap = from_bitmap(mem.d.bits.source)
  when (mem.d.valid) {
    assert(mem.d.bits.source < MemReqWidth.U)
    refill_data(refill_helper._4) := mem.d.bits.data
  }
  // refill_data_tmp is the wire fork of refill_data, but one cycle earlier
  val refill_data_tmp = WireInit(refill_data)
  refill_data_tmp(refill_helper._4) := mem.d.bits.data

  // save only one pte for each id
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte = VecInit((0 until (if (HasBitmapCheck) MemReqWidth / 2 else MemReqWidth)).map(i =>
    if (i == l2tlbParams.llptwsize + 1) {RegEnable(get_part(refill_data_tmp, req_addr_low(i)), 0.U.asTypeOf(get_part(refill_data_tmp, req_addr_low(i))), mem_resp_done && mem_resp_from_hptw) }
    else if (i == l2tlbParams.llptwsize) {RegEnable(get_part(refill_data_tmp, req_addr_low(i)), 0.U.asTypeOf(get_part(refill_data_tmp, req_addr_low(i))), mem_resp_done && mem_resp_from_ptw) }
    else { Mux(llptw_mem.buffer_it(i), get_part(refill_data, req_addr_low(i)), RegEnable(get_part(refill_data, req_addr_low(i)), 0.U.asTypeOf(get_part(refill_data, req_addr_low(i))), llptw_mem.buffer_it(i))) }
    // llptw could not use refill_data_tmp, because enq bypass's result works at next cycle
  ))

  // save eight ptes for each id when sector tlb
  // (miss queue may can't resp to tlb with low latency, it should have highest priority, but diffcult to design cache)
  val resp_pte_sector = VecInit((0 until (if (HasBitmapCheck) MemReqWidth / 2 else MemReqWidth)).map(i =>
    if (i == l2tlbParams.llptwsize + 1) {RegEnable(refill_data_tmp, 0.U.asTypeOf(refill_data_tmp), mem_resp_done && mem_resp_from_hptw) }
    else if (i == l2tlbParams.llptwsize) {RegEnable(refill_data_tmp, 0.U.asTypeOf(refill_data_tmp), mem_resp_done && mem_resp_from_ptw) }
    else { Mux(llptw_mem.buffer_it(i), refill_data, RegEnable(refill_data, 0.U.asTypeOf(refill_data), llptw_mem.buffer_it(i))) }
    // llptw could not use refill_data_tmp, because enq bypass's result works at next cycle
  ))

  if (HasBitmapCheck) {
    // add bitmap arb
    bitmap.foreach { Bitmap =>
      val bitmap_arb = Module(new Arbiter(new bitmapReqBundle(), 3))
      bitmap_arb.io.in(0) <> ptw.io.bitmap.get.req
      bitmap_arb.io.in(1) <> llptw.io.bitmap.get.req
      bitmap_arb.io.in(2) <> hptw.io.bitmap.get.req
      bitmap_arb.io.out.ready := Bitmap.io.req.ready

      Bitmap.io.req <> bitmap_arb.io.out

      // connect bitmap resp to PTW
      val bitmapresp_to_llptw = from_llptw(Bitmap.io.resp.bits.id)
      val bitmapresp_to_hptw = from_hptw(Bitmap.io.resp.bits.id)
      val bitmapresp_to_ptw = from_ptw(Bitmap.io.resp.bits.id)

      Bitmap.io.resp.ready := (llptw.io.bitmap.get.resp.ready && bitmapresp_to_llptw) || (hptw.io.bitmap.get.resp.ready && bitmapresp_to_hptw) || (ptw.io.bitmap.get.resp.ready && bitmapresp_to_ptw)

      // bitmap -> llptw ptw hptw
      llptw.io.bitmap.get.resp.valid := Bitmap.io.resp.valid && bitmapresp_to_llptw
      hptw.io.bitmap.get.resp.valid := Bitmap.io.resp.valid && bitmapresp_to_hptw
      ptw.io.bitmap.get.resp.valid := Bitmap.io.resp.valid && bitmapresp_to_ptw

      // add ptw、hptw、llptw with bitmap resp connect
      ptw.io.bitmap.get.resp.bits := Bitmap.io.resp.bits
      hptw.io.bitmap.get.resp.bits := Bitmap.io.resp.bits
      llptw.io.bitmap.get.resp.bits := Bitmap.io.resp.bits

      // mem -> bitmap
      Bitmap.io.mem.resp.valid := mem_resp_done && mem_resp_from_bitmap
      Bitmap.io.mem.resp.bits.id := DataHoldBypass(mem.d.bits.source, mem.d.valid)
      Bitmap.io.mem.resp.bits.value := DataHoldBypass(refill_data_tmp.asUInt, mem.d.valid)
    }

    // ptwcache -> hptw llptw
    hptw.io.l0_way_info.get := cache.io.l0_way_info.get
    llptw.io.l0_way_info.get := cache.io.l0_way_info.get
  }

  // mem -> llptw
  llptw_mem.resp.valid := mem_resp_done && mem_resp_from_llptw
  llptw_mem.resp.bits.id := DataHoldBypass(mem.d.bits.source, mem.d.valid)
  llptw_mem.resp.bits.value := DataHoldBypass(refill_data_tmp.asUInt, mem.d.valid)
  // mem -> ptw
  ptw.io.mem.resp.valid := mem_resp_done && mem_resp_from_ptw
  ptw.io.mem.resp.bits := resp_pte.apply(l2tlbParams.llptwsize)
  // mem -> hptw
  hptw.io.mem.resp.valid := mem_resp_done && mem_resp_from_hptw
  hptw.io.mem.resp.bits := resp_pte.apply(l2tlbParams.llptwsize + 1)
  // mem -> cache
  val refill_from_llptw = mem_resp_from_llptw
  val refill_from_ptw = mem_resp_from_ptw
  val refill_from_hptw = mem_resp_from_hptw
  val refill_level = Mux(refill_from_llptw, 0.U, Mux(refill_from_ptw, RegEnable(ptw.io.refill.level, 0.U, ptw.io.mem.req.fire), RegEnable(hptw.io.refill.level, 0.U, hptw.io.mem.req.fire)))
  val refill_valid = mem_resp_done && (if (HasBitmapCheck) !mem_resp_from_bitmap else true.B) && !flush && !flush_latch(mem.d.bits.source) && !(from_hptw(mem.d.bits.source) && hptw_bypassed)

  cache.io.refill.valid := GatedValidRegNext(refill_valid, false.B)
  cache.io.refill.bits.ptes := refill_data.asUInt
  cache.io.refill.bits.req_info_dup.map(_ := RegEnable(Mux(refill_from_llptw, llptw_mem.refill, Mux(refill_from_ptw, ptw.io.refill.req_info, hptw.io.refill.req_info)), refill_valid))
  cache.io.refill.bits.level_dup.map(_ := RegEnable(refill_level, refill_valid))
  cache.io.refill.bits.levelOH(refill_level, refill_valid)
  cache.io.refill.bits.sel_pte_dup.map(_ := RegEnable(sel_data(refill_data_tmp.asUInt, req_addr_low(mem.d.bits.source)), refill_valid))

  if (env.EnableDifftest) {
    val difftest_ptw_addr = RegInit(VecInit(Seq.fill(MemReqWidth)(0.U(PAddrBits.W))))
    when (mem.a.valid) {
      difftest_ptw_addr(mem.a.bits.source) := mem.a.bits.address
    }

    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index := 2.U
    difftest.valid := cache.io.refill.valid
    difftest.addr := difftest_ptw_addr(RegEnable(mem.d.bits.source, mem.d.valid))
    difftest.data := refill_data.asTypeOf(difftest.data)
    difftest.idtfr := DontCare
  }

  if (env.EnableDifftest) {
    for (i <- 0 until PtwWidth) {
      val difftest = DifftestModule(new DiffL2TLBEvent)
      difftest.coreid := io.hartId
      difftest.valid := io.tlb(i).resp.fire && !io.tlb(i).resp.bits.s1.af && !io.tlb(i).resp.bits.s2.gaf
      difftest.index := i.U
      difftest.vpn := Cat(io.tlb(i).resp.bits.s1.entry.tag, 0.U(sectortlbwidth.W))
      difftest.pbmt := io.tlb(i).resp.bits.s1.entry.pbmt
      difftest.g_pbmt := io.tlb(i).resp.bits.s2.entry.pbmt
      for (j <- 0 until tlbcontiguous) {
        difftest.ppn(j) := Cat(io.tlb(i).resp.bits.s1.entry.ppn, io.tlb(i).resp.bits.s1.ppn_low(j))
        difftest.valididx(j) := io.tlb(i).resp.bits.s1.valididx(j)
        difftest.pteidx(j) := io.tlb(i).resp.bits.s1.pteidx(j)
      }
      difftest.perm := io.tlb(i).resp.bits.s1.entry.perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      difftest.level := io.tlb(i).resp.bits.s1.entry.level.getOrElse(0.U.asUInt)
      difftest.pf := io.tlb(i).resp.bits.s1.pf
      difftest.satp := Cat(io.csr.tlb.satp.mode, io.csr.tlb.satp.asid, io.csr.tlb.satp.ppn)
      difftest.vsatp := Cat(io.csr.tlb.vsatp.mode, io.csr.tlb.vsatp.asid, io.csr.tlb.vsatp.ppn)
      difftest.hgatp := Cat(io.csr.tlb.hgatp.mode, io.csr.tlb.hgatp.vmid, io.csr.tlb.hgatp.ppn)
      difftest.gvpn := io.tlb(i).resp.bits.s2.entry.tag
      difftest.g_perm := io.tlb(i).resp.bits.s2.entry.perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      difftest.g_level := io.tlb(i).resp.bits.s2.entry.level.getOrElse(0.U.asUInt)
      difftest.s2ppn := io.tlb(i).resp.bits.s2.entry.ppn
      difftest.gpf := io.tlb(i).resp.bits.s2.gpf
      difftest.s2xlate := io.tlb(i).resp.bits.s2xlate
    }
  }

  // pmp
  pmp_check(0).req <> ptw.io.pmp.req
  ptw.io.pmp.resp <> pmp_check(0).resp
  pmp_check(1).req <> llptw.io.pmp(0).req
  llptw.io.pmp(0).resp <> pmp_check(1).resp
  pmp_check(2).req <> llptw.io.pmp(1).req
  llptw.io.pmp(1).resp <> pmp_check(2).resp
  pmp_check(3).req <> hptw.io.pmp.req
  hptw.io.pmp.resp <> pmp_check(3).resp
  if (HasBitmapCheck) {
    pmp_check(4).req <> bitmap.get.io.pmp.req
    bitmap.get.io.pmp.resp <> pmp_check(4).resp
  }

  llptw_out.ready := outReady(llptw_out.bits.req_info.source, outArbMqPort)

  // hptw and page cache -> ptw and llptw
  val HptwRespArbCachePort = 0
  val HptwRespArbHptw = 1
  hptw_resp_arb.io.in(HptwRespArbCachePort).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.isHptwReq
  hptw_resp_arb.io.in(HptwRespArbCachePort).bits.id := cache.io.resp.bits.toHptw.id
  hptw_resp_arb.io.in(HptwRespArbCachePort).bits.resp := cache.io.resp.bits.toHptw.resp
  hptw_resp_arb.io.in(HptwRespArbHptw).valid := hptw.io.resp.valid
  hptw_resp_arb.io.in(HptwRespArbHptw).bits.id := hptw.io.resp.bits.id
  hptw_resp_arb.io.in(HptwRespArbHptw).bits.resp := hptw.io.resp.bits.resp
  hptw.io.resp.ready := hptw_resp_arb.io.in(HptwRespArbHptw).ready

  ptw.io.hptw.resp.valid := hptw_resp_arb.io.out.valid && hptw_resp_arb.io.out.bits.id === FsmReqID.U
  ptw.io.hptw.resp.bits.h_resp := hptw_resp_arb.io.out.bits.resp
  llptw.io.hptw.resp.valid := hptw_resp_arb.io.out.valid && hptw_resp_arb.io.out.bits.id =/= FsmReqID.U
  llptw.io.hptw.resp.bits.id := hptw_resp_arb.io.out.bits.id
  llptw.io.hptw.resp.bits.h_resp := hptw_resp_arb.io.out.bits.resp
  hptw_resp_arb.io.out.ready := true.B

  val cfsValue = Option.when(HasBitmapCheck)(llptw_out.bits.bitmapCheck.get.cfs)

  // Timing: Maybe need to do some optimization or even add one more cycle
  for (i <- 0 until PtwWidth) {
    mergeArb(i).in(outArbCachePort).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.req_info.source===i.U && !cache.io.resp.bits.isHptwReq
    mergeArb(i).in(outArbCachePort).bits.s2xlate := cache.io.resp.bits.req_info.s2xlate
    mergeArb(i).in(outArbCachePort).bits.s1 := cache.io.resp.bits.stage1
    mergeArb(i).in(outArbCachePort).bits.s2 := cache.io.resp.bits.toHptw.resp
    mergeArb(i).in(outArbFsmPort).valid := ptw.io.resp.valid && ptw.io.resp.bits.source===i.U
    mergeArb(i).in(outArbFsmPort).bits.s2xlate := ptw.io.resp.bits.s2xlate
    mergeArb(i).in(outArbFsmPort).bits.s1 := ptw.io.resp.bits.resp
    mergeArb(i).in(outArbFsmPort).bits.s2 := ptw.io.resp.bits.h_resp
    mergeArb(i).in(outArbMqPort).valid := llptw_out.valid && llptw_out.bits.req_info.source===i.U
    mergeArb(i).in(outArbMqPort).bits.s2xlate := llptw_out.bits.req_info.s2xlate
    mergeArb(i).in(outArbMqPort).bits.s1 := Mux(
      llptw_out.bits.first_s2xlate_fault, llptw_stage1(llptw_out.bits.id),
      // When G-stage triggers gpf | gaf, `first_s2xlate_fault` will be true
      // So Here only including pf | af in Stage1, or Stage1Gpf (gpf = llptw_out.bits.h_resp.gpf)
      contiguous_pte_to_merge_ptwResp(
        if (HasBitmapCheck) Mux(llptw_out.bits.bitmapCheck.get.jmp_bitmap_check, llptw_out.bits.bitmapCheck.get.ptes.asUInt, resp_pte_sector(llptw_out.bits.id).asUInt) else resp_pte_sector(llptw_out.bits.id).asUInt, llptw_out.bits.req_info.vpn, llptw_out.bits.af,
        true, s2xlate = llptw_out.bits.req_info.s2xlate, mPBMTE = mPBMTE, hPBMTE = hPBMTE, gpf = llptw_out.bits.h_resp.gpf,
        cfs = cfsValue.getOrElse(VecInit(Seq.fill(tlbcontiguous)(false.B)))
      )
    )
    mergeArb(i).in(outArbMqPort).bits.s2 := llptw_out.bits.h_resp
    mergeArb(i).out.ready := outArb(i).in(0).ready
  }

  for (i <- 0 until PtwWidth) {
    outArb(i).in(0).valid := mergeArb(i).out.valid
    outArb(i).in(0).bits.s2xlate := mergeArb(i).out.bits.s2xlate
    if (HasBitmapCheck) {
      // mbmc:bitmap csr
      val mbmc = csr_dup(0).mbmc
      val bitmap_enable = mbmc.BME === 1.U && mbmc.CMODE === 0.U
      when (!bitmap_enable) {
        outArb(i).in(0).bits.s1 := merge_ptwResp_to_sector_ptwResp(mergeArb(i).out.bits.s1)
        outArb(i).in(0).bits.s2 := mergeArb(i).out.bits.s2
      } .otherwise {
        when (mergeArb(i).out.bits.s2xlate === noS2xlate || mergeArb(i).out.bits.s2xlate === onlyStage1) {
          outArb(i).in(0).bits.s1 := ptwResp_to_sector_4kptwResp(mergeArb(i).out.bits.s1)
          outArb(i).in(0).bits.s2 := mergeArb(i).out.bits.s2
        } .otherwise {
          outArb(i).in(0).bits.s1 := merge_ptwResp_to_sector_ptwResp(mergeArb(i).out.bits.s1)
          outArb(i).in(0).bits.s2 := mergeArb(i).out.bits.s2
          outArb(i).in(0).bits.s2.entry.level.map(_ := 0.U(2.W))
          outArb(i).in(0).bits.s2.entry.ppn := get_4kppn(mergeArb(i).out.bits.s2.entry.ppn, mergeArb(i).out.bits.s2.entry.tag, mergeArb(i).out.bits.s2.entry.level.get, mergeArb(i).out.bits.s2.entry.n.get.asBool)
          outArb(i).in(0).bits.s2.entry.n.map(_ := false.B)
        }
      }
    } else {
      outArb(i).in(0).bits.s1 := merge_ptwResp_to_sector_ptwResp(mergeArb(i).out.bits.s1)
      outArb(i).in(0).bits.s2 := mergeArb(i).out.bits.s2
    }
  }

  // io.tlb.map(_.resp) <> outArb.map(_.out)
  io.tlb.map(_.resp).zip(outArb.map(_.out)).map{
    case (resp, out) => resp <> out
  }

  // sfence
  when (flush) {
    for (i <- 0 until MemReqWidth) {
      when (waiting_resp(i)) {
        flush_latch(i) := true.B
      }
    }
  }
  // mem -> control signal
  // waiting_resp and sfence_latch will be reset when mem_resp_done
  when (mem_resp_done) {
    waiting_resp(mem.d.bits.source) := false.B
    flush_latch(mem.d.bits.source) := false.B
  }

  def get_4kppn(ppn: UInt, vpn: UInt, level: UInt, n: Bool): UInt = {
    MuxLookup(level, 0.U)(Seq(
      3.U -> Cat(ppn(ppn.getWidth - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(ppn(ppn.getWidth - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(ppn(ppn.getWidth - 1, vpnnLen), vpn(vpnnLen - 1, 0)),
      0.U -> Mux(n === 0.U, ppn(ppn.getWidth - 1, 0), Cat(ppn(ppn.getWidth - 1, pteNapotBits), vpn(pteNapotBits - 1, 0)))
    ))
  }

  def get_4kppn_up(ppn_up: UInt, vpn_up: UInt, level: UInt, n: Bool): UInt = {
    MuxLookup(level, 0.U)(Seq(
      3.U -> Cat(ppn_up(ppn_up.getWidth - 1, vpnnLen * 3 - sectortlbwidth), vpn_up(vpnnLen * 3 - 1 - sectortlbwidth, 0)),
      2.U -> Cat(ppn_up(ppn_up.getWidth - 1, vpnnLen * 2 - sectortlbwidth), vpn_up(vpnnLen * 2 - 1 - sectortlbwidth, 0)),
      1.U -> Cat(ppn_up(ppn_up.getWidth - 1, vpnnLen - sectortlbwidth), vpn_up(vpnnLen - 1 - sectortlbwidth, 0)),
      0.U -> Mux(n === 0.U, ppn_up(ppn_up.getWidth - 1, 0), Cat(ppn_up(ppn_up.getWidth - 1, pteNapotBits - sectortlbwidth), vpn_up(pteNapotBits - 1 - sectortlbwidth, 0)))
    ))
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  def get_part(data: Vec[UInt], index: UInt): UInt = {
    val inner_data = data.asTypeOf(Vec(data.getWidth / XLEN, UInt(XLEN.W)))
    inner_data(index)
  }

  // not_super means that this is a normal page
  // valididx(i) will be all true when super page to be convenient for l1 tlb matching
  def contiguous_pte_to_merge_ptwResp(pte: UInt, vpn: UInt, af: Bool, af_first: Boolean, s2xlate: UInt, mPBMTE: Bool, hPBMTE: Bool, not_super: Boolean = true, gpf: Bool, cfs : Vec[Bool]) : PtwMergeResp = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val ptw_merge_resp = Wire(new PtwMergeResp())
    val hasS2xlate = s2xlate =/= noS2xlate
    val pbmte = Mux(s2xlate === onlyStage1 || s2xlate === allStage, hPBMTE, mPBMTE)
    for (i <- 0 until tlbcontiguous) {
      val pte_in = pte(64 * i + 63, 64 * i).asTypeOf(new PteBundle())
      val ptw_resp = Wire(new PtwMergeEntry(tagLen = sectorvpnLen, hasPerm = true, hasLevel = true, hasNapot = true))
      ptw_resp.ppn := pte_in.getPPN()(ptePPNLen - 1, sectortlbwidth)
      ptw_resp.ppn_low := pte_in.getPPN()(sectortlbwidth - 1, 0)
      ptw_resp.level.map(_ := 0.U)
      ptw_resp.pbmt := pte_in.pbmt
      ptw_resp.n.map(_ := pte_in.n === true.B && pte_in.getPPN()(3, 0) === 8.U)
      ptw_resp.perm.map(_ := pte_in.getPerm())
      ptw_resp.tag := vpn(vpnLen - 1, sectortlbwidth)
      // LLPTW will not handle onlyS2 situations
      // noS2xlate. pf: allStagePf; af: af(pmp_af) || pte_in.isAf(ppn_af); gpf: never
      // onlyStage1. pf: allStagePf; af: af(pmp_af) || pte_in.isAf(ppn_af); gpf: never
      // allStage. pf: allStagePf; af: af(pmp_af) && !gpf; gpf: incoming parameter `gpf`
      // priority: af (pmp check) > pf (pte check) > af (pte check)
      val isPf = pte_in.isPf(0.U, pbmte) || !pte_in.isLeaf()
      val isAf = pte_in.isAf() && (s2xlate === noS2xlate || s2xlate === onlyStage1) && !isPf
      ptw_resp.pf := (if (af_first) !af else true.B) && isPf
      ptw_resp.af := (if (af_first) true.B else !isPf) && (af || isAf)
      ptw_resp.cf := cfs(ptw_resp.ppn(sectortlbwidth - 1, 0))
      ptw_resp.v := !ptw_resp.pf
      ptw_resp.prefetch := DontCare
      ptw_resp.asid := Mux(hasS2xlate, vsatp.asid, satp.asid)
      ptw_resp.vmid.map(_ := hgatp.vmid)
      ptw_merge_resp.entry(i) := ptw_resp
    }
    ptw_merge_resp.pteidx := UIntToOH(vpn(sectortlbwidth - 1, 0)).asBools
    val napot = ptw_merge_resp.entry(vpn(sectortlbwidth - 1, 0)).n.getOrElse(0.U)
    ptw_merge_resp.not_super := not_super.B && !napot
    ptw_merge_resp.not_merge := hasS2xlate
    ptw_merge_resp
  }

  def merge_ptwResp_to_sector_ptwResp(pte: PtwMergeResp) : PtwSectorResp = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val ptw_sector_resp = Wire(new PtwSectorResp)
    ptw_sector_resp.entry.tag := pte.entry(OHToUInt(pte.pteidx)).tag
    ptw_sector_resp.entry.asid := pte.entry(OHToUInt(pte.pteidx)).asid
    ptw_sector_resp.entry.vmid.map(_ := pte.entry(OHToUInt(pte.pteidx)).vmid.getOrElse(0.U))
    ptw_sector_resp.entry.ppn := pte.entry(OHToUInt(pte.pteidx)).ppn
    ptw_sector_resp.entry.pbmt := pte.entry(OHToUInt(pte.pteidx)).pbmt
    ptw_sector_resp.entry.n.map(_ := pte.entry(OHToUInt(pte.pteidx)).n.getOrElse(0.U))
    ptw_sector_resp.entry.perm.map(_ := pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)))
    ptw_sector_resp.entry.level.map(_ := pte.entry(OHToUInt(pte.pteidx)).level.getOrElse(0.U(log2Up(Level + 1).W)))
    ptw_sector_resp.entry.prefetch := pte.entry(OHToUInt(pte.pteidx)).prefetch
    ptw_sector_resp.entry.v := pte.entry(OHToUInt(pte.pteidx)).v
    ptw_sector_resp.af := pte.entry(OHToUInt(pte.pteidx)).af
    ptw_sector_resp.pf := pte.entry(OHToUInt(pte.pteidx)).pf
    ptw_sector_resp.addr_low := OHToUInt(pte.pteidx)
    ptw_sector_resp.pteidx := pte.pteidx
    for (i <- 0 until tlbcontiguous) {
      val ppn_equal = pte.entry(i).ppn === pte.entry(OHToUInt(pte.pteidx)).ppn
      val pbmt_equal = pte.entry(i).pbmt === pte.entry(OHToUInt(pte.pteidx)).pbmt
      val perm_equal = pte.entry(i).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt === pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      val v_equal = pte.entry(i).v === pte.entry(OHToUInt(pte.pteidx)).v
      val af_equal = pte.entry(i).af === pte.entry(OHToUInt(pte.pteidx)).af
      val pf_equal = pte.entry(i).pf === pte.entry(OHToUInt(pte.pteidx)).pf
      val cf_equal = if (HasBitmapCheck) pte.entry(i).cf === pte.entry(OHToUInt(pte.pteidx)).cf else true.B
      ptw_sector_resp.valididx(i) := ((ppn_equal && pbmt_equal && perm_equal && v_equal && af_equal && pf_equal && cf_equal) || !pte.not_super) && !pte.not_merge
      ptw_sector_resp.ppn_low(i) := pte.entry(i).ppn_low
    }
    ptw_sector_resp.valididx(OHToUInt(pte.pteidx)) := true.B
    ptw_sector_resp
  }

  def ptwResp_to_sector_4kptwResp(pte: PtwMergeResp) : PtwSectorResp = {
    assert(tlbcontiguous == 8, "Only support tlbcontiguous = 8!")
    val ptw_sector_resp = Wire(new PtwSectorResp)
    ptw_sector_resp.entry.tag := pte.entry(OHToUInt(pte.pteidx)).tag
    ptw_sector_resp.entry.asid := pte.entry(OHToUInt(pte.pteidx)).asid
    ptw_sector_resp.entry.vmid.map(_ := pte.entry(OHToUInt(pte.pteidx)).vmid.getOrElse(0.U))
    ptw_sector_resp.entry.ppn := get_4kppn_up(pte.entry(OHToUInt(pte.pteidx)).ppn, pte.entry(OHToUInt(pte.pteidx)).tag, pte.entry(OHToUInt(pte.pteidx)).level.getOrElse(0.U(log2Up(Level + 1).W)), pte.entry(OHToUInt(pte.pteidx)).n.getOrElse(0.U).asBool)
    ptw_sector_resp.entry.pbmt := pte.entry(OHToUInt(pte.pteidx)).pbmt
    ptw_sector_resp.entry.n.map(_ := false.B)
    ptw_sector_resp.entry.perm.map(_ := pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)))
    ptw_sector_resp.entry.level.map(_ := (0.U(log2Up(Level + 1).W)))
    ptw_sector_resp.entry.prefetch := pte.entry(OHToUInt(pte.pteidx)).prefetch
    ptw_sector_resp.entry.v := pte.entry(OHToUInt(pte.pteidx)).v
    ptw_sector_resp.af := pte.entry(OHToUInt(pte.pteidx)).af
    ptw_sector_resp.pf := pte.entry(OHToUInt(pte.pteidx)).pf
    ptw_sector_resp.addr_low := OHToUInt(pte.pteidx)
    ptw_sector_resp.pteidx := pte.pteidx
    for (i <- 0 until tlbcontiguous) {
      val ppn_equal = pte.entry(i).ppn === pte.entry(OHToUInt(pte.pteidx)).ppn
      val pbmt_equal = pte.entry(i).pbmt === pte.entry(OHToUInt(pte.pteidx)).pbmt
      val perm_equal = pte.entry(i).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt === pte.entry(OHToUInt(pte.pteidx)).perm.getOrElse(0.U.asTypeOf(new PtePermBundle)).asUInt
      val v_equal = pte.entry(i).v === pte.entry(OHToUInt(pte.pteidx)).v
      val af_equal = pte.entry(i).af === pte.entry(OHToUInt(pte.pteidx)).af
      val pf_equal = pte.entry(i).pf === pte.entry(OHToUInt(pte.pteidx)).pf
      val cf_equal = pte.entry(i).cf === pte.entry(OHToUInt(pte.pteidx)).cf
      ptw_sector_resp.valididx(i) := (ppn_equal && pbmt_equal && perm_equal && v_equal && af_equal && pf_equal && cf_equal) && !pte.not_merge
      ptw_sector_resp.ppn_low(i) := pte.entry(i).ppn_low
    }
    when (!pte.not_super) {
      for (i <- 0 until tlbcontiguous) {
        ptw_sector_resp.valididx(i) := false.B
        ptw_sector_resp.ppn_low(i) := OHToUInt(pte.pteidx)
      }
    }
    ptw_sector_resp.valididx(OHToUInt(pte.pteidx)) := true.B
    ptw_sector_resp
  }

  def outReady(source: UInt, port: Int): Bool = {
    MuxLookup(source, true.B)((0 until PtwWidth).map(i => i.U -> mergeArb(i).in(port).ready))
  }

  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[sfence] ${io.sfence}\n")
  XSDebug(p"[io.csr.tlb] ${io.csr.tlb}\n")

  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"req_count${i}", io.tlb(i).req(0).fire)
    XSPerfAccumulate(s"req_blocked_count_${i}", io.tlb(i).req(0).valid && !io.tlb(i).req(0).ready)
  }
  XSPerfAccumulate(s"req_blocked_by_mq", arb1.io.out.valid && missQueue.io.out.valid)
  for (i <- 0 until (MemReqWidth + 1)) {
    XSPerfAccumulate(s"mem_req_util${i}", PopCount(waiting_resp) === i.U)
  }
  XSPerfAccumulate("mem_cycle", PopCount(waiting_resp) =/= 0.U)
  XSPerfAccumulate("mem_count", mem.a.fire)
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"llptw_ppn_af${i}", mergeArb(i).in(outArbMqPort).valid && mergeArb(i).in(outArbMqPort).bits.s1.entry(OHToUInt(mergeArb(i).in(outArbMqPort).bits.s1.pteidx)).af && !llptw_out.bits.af)
    XSPerfAccumulate(s"access_fault${i}", io.tlb(i).resp.fire && io.tlb(i).resp.bits.s1.af)
  }

  // print configs
  println(s"${l2tlbParams.name}: a ptw, a llptw with size ${l2tlbParams.llptwsize}, miss queue size ${MissQueueSize} l2:${l2tlbParams.l2Size} fa l1: nSets ${l2tlbParams.l1nSets} nWays ${l2tlbParams.l1nWays} l0: ${l2tlbParams.l0nSets} nWays ${l2tlbParams.l0nWays} blockBytes:${l2tlbParams.blockBytes}")

  val perfEvents  = Seq(llptw, cache, ptw).flatMap(_.getPerfEvents)
  generatePerfEvent()

  val isWriteL1TlbTable = Constantin.createRecord(s"isWriteL1TlbTable$hartId")
  val L1TlbTable = ChiselDB.createTable(s"L1Tlb_hart$hartId", new L1TlbDB)
  val ITlbReqDB, DTlbReqDB, ITlbRespDB, DTlbRespDB = Wire(new L1TlbDB)
  ITlbReqDB.vpn := io.tlb(0).req(0).bits.vpn
  DTlbReqDB.vpn := io.tlb(1).req(0).bits.vpn
  ITlbRespDB.vpn := Cat(io.tlb(0).resp.bits.s1.entry.tag, OHToUInt(io.tlb(0).resp.bits.s1.pteidx))
  DTlbRespDB.vpn := Cat(io.tlb(1).resp.bits.s1.entry.tag, OHToUInt(io.tlb(1).resp.bits.s1.pteidx))
  L1TlbTable.log(ITlbReqDB, isWriteL1TlbTable.orR && io.tlb(0).req(0).fire, "ITlbReq", clock, reset)
  L1TlbTable.log(DTlbReqDB, isWriteL1TlbTable.orR && io.tlb(1).req(0).fire, "DTlbReq", clock, reset)
  L1TlbTable.log(ITlbRespDB, isWriteL1TlbTable.orR && io.tlb(0).resp.fire, "ITlbResp", clock, reset)
  L1TlbTable.log(DTlbRespDB, isWriteL1TlbTable.orR && io.tlb(1).resp.fire, "DTlbResp", clock, reset)

  val isWritePageCacheTable = Constantin.createRecord(s"isWritePageCacheTable$hartId")
  val PageCacheTable = ChiselDB.createTable(s"PageCache_hart$hartId", new PageCacheDB)
  val PageCacheDB = Wire(new PageCacheDB)
  PageCacheDB.vpn := Cat(cache.io.resp.bits.stage1.entry(0).tag, OHToUInt(cache.io.resp.bits.stage1.pteidx))
  PageCacheDB.source := cache.io.resp.bits.req_info.source
  PageCacheDB.bypassed := cache.io.resp.bits.bypassed
  PageCacheDB.is_first := cache.io.resp.bits.isFirst
  PageCacheDB.prefetched := cache.io.resp.bits.stage1.entry(0).prefetch
  PageCacheDB.prefetch := cache.io.resp.bits.prefetch
  PageCacheDB.l2Hit := cache.io.resp.bits.toFsm.l2Hit
  PageCacheDB.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  PageCacheDB.hit := cache.io.resp.bits.hit
  PageCacheTable.log(PageCacheDB, isWritePageCacheTable.orR && cache.io.resp.fire, "PageCache", clock, reset)

  val isWritePTWTable = Constantin.createRecord(s"isWritePTWTable$hartId")
  val PTWTable = ChiselDB.createTable(s"PTW_hart$hartId", new PTWDB)
  val PTWReqDB, PTWRespDB, LLPTWReqDB, LLPTWRespDB = Wire(new PTWDB)
  PTWReqDB.vpn := ptw.io.req.bits.req_info.vpn
  PTWReqDB.source := ptw.io.req.bits.req_info.source
  PTWRespDB.vpn := ptw.io.refill.req_info.vpn
  PTWRespDB.source := ptw.io.refill.req_info.source
  LLPTWReqDB.vpn := llptw.io.in.bits.req_info.vpn
  LLPTWReqDB.source := llptw.io.in.bits.req_info.source
  LLPTWRespDB.vpn := llptw.io.mem.refill.vpn
  LLPTWRespDB.source := llptw.io.mem.refill.source
  PTWTable.log(PTWReqDB, isWritePTWTable.orR && ptw.io.req.fire, "PTWReq", clock, reset)
  PTWTable.log(PTWRespDB, isWritePTWTable.orR && ptw.io.mem.resp.fire, "PTWResp", clock, reset)
  PTWTable.log(LLPTWReqDB, isWritePTWTable.orR && llptw.io.in.fire, "LLPTWReq", clock, reset)
  PTWTable.log(LLPTWRespDB, isWritePTWTable.orR && llptw.io.mem.resp.fire, "LLPTWResp", clock, reset)

  val isWriteL2TlbMissQueueTable = Constantin.createRecord(s"isWriteL2TlbMissQueueTable$hartId")
  val L2TlbMissQueueTable = ChiselDB.createTable(s"L2TlbMissQueue_hart$hartId", new L2TlbMissQueueDB)
  val L2TlbMissQueueInDB, L2TlbMissQueueOutDB = Wire(new L2TlbMissQueueDB)
  L2TlbMissQueueInDB.vpn := missQueue.io.in.bits.req_info.vpn
  L2TlbMissQueueOutDB.vpn := missQueue.io.out.bits.req_info.vpn
  L2TlbMissQueueTable.log(L2TlbMissQueueInDB, isWriteL2TlbMissQueueTable.orR && missQueue.io.in.fire, "L2TlbMissQueueIn", clock, reset)
  L2TlbMissQueueTable.log(L2TlbMissQueueOutDB, isWriteL2TlbMissQueueTable.orR && missQueue.io.out.fire, "L2TlbMissQueueOut", clock, reset)
}

/** BlockHelper, block missqueue, not to send too many req to cache
 *  Parameter:
 *    enable: enable BlockHelper, mq should not send too many reqs
 *    start: when miss queue out fire and need, block miss queue's out
 *    block: block miss queue's out
 *    latency: last missqueue out's cache access latency
 */
class BlockHelper(latency: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val start = Input(Bool())
    val block = Output(Bool())
  })

  val count = RegInit(0.U(log2Ceil(latency).W))
  val valid = RegInit(false.B)
  val work = RegInit(true.B)

  io.block := valid

  when (io.start && work) { valid := true.B }
  when (valid) { count := count + 1.U }
  when (count === (latency.U) || io.enable) {
    valid := false.B
    work := io.enable
    count := 0.U
  }
}

class PTEHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val satp   = IO(Input(UInt(64.W)))
  val vpn    = IO(Input(UInt(64.W)))
  val pte    = IO(Output(UInt(64.W)))
  val level  = IO(Output(UInt(8.W)))
  val pf     = IO(Output(UInt(8.W)))
}

class PTWDelayN[T <: Data](gen: T, n: Int, flush: Bool) extends Module {
  val io = IO(new Bundle() {
    val in = Input(gen)
    val out = Output(gen)
    val ptwflush = Input(flush.cloneType)
  })
  val out = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  val t = RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(gen))))
  out(0) := io.in
  if (n == 1) {
    io.out := out(0)
  } else {
    when (io.ptwflush) {
      for (i <- 0 until n) {
        t(i) := 0.U.asTypeOf(gen)
        out(i) := 0.U.asTypeOf(gen)
      }
      io.out := 0.U.asTypeOf(gen)
    } .otherwise {
      for (i <- 1 until n) {
        t(i-1) := out(i-1)
        out(i) := t(i-1)
      }
      io.out := out(n-1)
    }
  }
}

object PTWDelayN {
  def apply[T <: Data](in: T, n: Int, flush: Bool): T = {
    val delay = Module(new PTWDelayN(in.cloneType, n, flush))
    delay.io.in := in
    delay.io.ptwflush := flush
    delay.io.out
  }
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new L2TLBIO)
  val flush = VecInit(Seq.fill(PtwWidth)(false.B))
  flush(0) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed || io.csr.tlb.vsatp.changed || io.csr.tlb.hgatp.changed || io.csr.tlb.priv.virt_changed, itlbParams.fenceDelay)
  flush(1) := DelayN(io.sfence.valid || io.csr.tlb.satp.changed || io.csr.tlb.vsatp.changed || io.csr.tlb.hgatp.changed || io.csr.tlb.priv.virt_changed, ldtlbParams.fenceDelay)
  for (i <- 0 until PtwWidth) {
    val helper = Module(new PTEHelper())
    helper.clock := clock
    helper.satp := io.csr.tlb.satp.ppn

    if (coreParams.softPTWDelay == 1) {
      helper.enable := io.tlb(i).req(0).fire
      helper.vpn := io.tlb(i).req(0).bits.vpn
    } else {
      helper.enable := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay - 1, flush(i))
      helper.vpn := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay - 1, flush(i))
    }

    val pte = helper.pte.asTypeOf(new PteBundle)
    val level = helper.level
    val pf = helper.pf
    val empty = RegInit(true.B)
    when (io.tlb(i).req(0).fire) {
      empty := false.B
    } .elsewhen (io.tlb(i).resp.fire || flush(i)) {
      empty := true.B
    }

    io.tlb(i).req(0).ready := empty || io.tlb(i).resp.fire
    io.tlb(i).resp.valid := PTWDelayN(io.tlb(i).req(0).fire, coreParams.softPTWDelay, flush(i))
    assert(!io.tlb(i).resp.valid || io.tlb(i).resp.ready)
    io.tlb(i).resp.bits.s1.entry.tag := PTWDelayN(io.tlb(i).req(0).bits.vpn, coreParams.softPTWDelay, flush(i))
    io.tlb(i).resp.bits.s1.entry.pbmt := pte.pbmt
    io.tlb(i).resp.bits.s1.entry.ppn := pte.ppn
    io.tlb(i).resp.bits.s1.entry.perm.map(_ := pte.getPerm())
    io.tlb(i).resp.bits.s1.entry.level.map(_ := level)
    io.tlb(i).resp.bits.s1.pf := pf
    io.tlb(i).resp.bits.s1.af := DontCare // TODO: implement it
    io.tlb(i).resp.bits.s1.entry.v := !pf
    io.tlb(i).resp.bits.s1.entry.prefetch := DontCare
    io.tlb(i).resp.bits.s1.entry.asid := io.csr.tlb.satp.asid
  }
}

class L2TLBWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  val useSoftPTW = coreParams.softPTW
  val node = if (!useSoftPTW) TLIdentityNode() else null
  val ptw = if (!useSoftPTW) LazyModule(new L2TLB()) else null
  if (!useSoftPTW) {
    node := ptw.node
  }

  class L2TLBWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val io = IO(new L2TLBIO)
    val perfEvents = if (useSoftPTW) {
      val fake_ptw = Module(new FakePTW())
      io <> fake_ptw.io
      Seq()
    }
    else {
        io <> ptw.module.io
        ptw.module.getPerfEvents
    }
    generatePerfEvent()
  }

  lazy val module = new L2TLBWrapperImp(this)
}
