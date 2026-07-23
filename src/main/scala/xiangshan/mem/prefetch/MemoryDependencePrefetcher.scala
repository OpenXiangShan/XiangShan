/***************************************************************************************
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
***************************************************************************************/

package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{MultiPortQueue, ValidPseudoLRU}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.trace._

/** L1 MDP is a first-class prefetcher in XSCoreParams.prefetcher.
  *
  * The table sizes, confidence policy, buffers and chasing depth are kept in
  * one parameter object so configurations can enable/tune MDP like the other
  * L1 prefetchers instead of relying on Wrapper-local constants.
  */
case class MdpParams(
  immBits: Int = 12,
  mdtEntries: Int = 16,
  bstEntries: Int = 8,
  bsmtEntries: Int = 8,
  bsmtDepthBlocks: Int = 1,
  counterBits: Int = 3,
  confInit: Int = 1,
  confThreshold: Int = 4,
  immCntInit: Int = 4,
  immCntThreshold: Int = 2,
  bstConfThreshold: Int = 3,
  trainFilterSize: Int = 12,
  chasingFilterSize: Int = 32,
  prefetchBufferEntries: Int = 8,
  sourceQueueRows: Int = 4,
  chasingDepth: Int = 1
) extends PrefetcherParams {
  override def name: String = "mdp"
  override def tlbPlace = TLBPlace.dtlb_pf
  require(chasingDepth >= 1)
  require(bsmtEntries == 8)
  require(bsmtDepthBlocks >= 1)
}

trait HasMdpParameters extends HasL1PrefetchHelper {
  def mdpParams: MdpParams = p(XSCoreParamsKey).prefetcher.collectFirst {
    case params: MdpParams => params
  }.getOrElse(MdpParams())

  val MDP_IMM_BITS = mdpParams.immBits
  val MDP_MDT_ENTRIES = mdpParams.mdtEntries
  val MDP_BST_ENTRIES = mdpParams.bstEntries
  val MDP_BSMT_ENTRIES = mdpParams.bsmtEntries
  val MDP_COUNTER_BITS = mdpParams.counterBits

  val MDP_CONF_INIT = mdpParams.confInit
  val MDP_CONF_THRESHOLD = mdpParams.confThreshold
  val MDP_IMM_CNT_INIT = mdpParams.immCntInit
  val MDP_IMM_CNT_THRESHOLD = mdpParams.immCntThreshold
  val MDP_BST_CONF_THRESHOLD = mdpParams.bstConfThreshold
  val MDP_CHASING_DEPTH = mdpParams.chasingDepth

  def mdpCounterMax: Int = (1 << MDP_COUNTER_BITS) - 1
}

/** Cause of the next MDP chasing request carried through L1/L2 miss state. */
object MdpPfOrigin {
  val width = 3
  val stride = 0.U(width.W)
  val stream = 1.U(width.W)
  val chain = 2.U(width.W)
  // Reserved while todo item 4 is deferred in this iteration.
  val history = 3.U(width.W)
  // A demand MDT hit produces the original, unclassified chasing request.
  // Only a refill caused by an MDP prefetch receives a detailed origin above.
  val legacy = 4.U(width.W)
}

class MdpTrainReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From each LDU S0 (through MdpTrainFilter) to MDP train0.  The producer PC
  // is the MDT key and pc/imm describe the dependent load.
  val wakedup = Bool()
  val wakedupPC = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)
  val imm = UInt(MDP_IMM_BITS.W)
  val loadSize = UInt(2.W)
  val loadUnsigned = Bool()
  val robIdx = new RobPtr
}

class MdpTriggerReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From every scalar demand load entering an LDU S0 (including replays) to
  // that LDU's parallel MDP trigger port.  The identity and load semantics are
  // registered in the LDU for matching and possible refill-data extraction.
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val lduId = UInt(log2Up(backendParams.LduCnt).W)
  val robIdx = new RobPtr
  val loadSize = UInt(2.W)
  val loadUnsigned = Bool()
}

class MdpL1PfHintBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From MDP back to the matching LDU.  pc/vaddr/lduId/robIdx identify the
  // exact in-flight request; imm then travels through LoadPipe into an MSHR.
  val imm = UInt(MDP_IMM_BITS.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val lduId = UInt(log2Up(backendParams.LduCnt).W)
  val robIdx = new RobPtr
  val mdpChainImm = UInt(MDP_IMM_BITS.W)
  val mdpChainValid = Bool()
  val mdpChainLoadSize = UInt(2.W)
  val mdpChainLoadUnsigned = Bool()
  val mdpOrigin = UInt(MdpPfOrigin.width.W)
}

class MdpChasingPfReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From a completed hinted DCache MSHR refill, through the per-MSHR filter,
  // to MDP.  data is the extracted load value and MDP adds imm to it.
  val data = UInt(XLEN.W)
  val imm = UInt(MDP_IMM_BITS.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)
  val mdpChainImm = UInt(MDP_IMM_BITS.W)
  val mdpChainValid = Bool()
  val mdpChainLoadSize = UInt(2.W)
  val mdpChainLoadUnsigned = Bool()
  val mdpOrigin = UInt(MdpPfOrigin.width.W)
  val mdpLoadSize = UInt(2.W)
  val mdpLoadUnsigned = Bool()
}

/** Internal MDP candidate passed to the translation/filter buffer.
  *
  * Unlike the generic SourcePrefetchReq, this bundle retains the exact target
  * byte address and load semantics needed when a stridePf miss must feed the
  * next chasingPf step.
  */
class MdpSourcePrefetchReq(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // triggerPC/triggerVA identify the load that generated this candidate;
  // prefetchVA is the exact byte address sent to the translation buffer.
  val triggerPC = UInt(VAddrBits.W)
  val triggerVA = UInt(VAddrBits.W)
  val prefetchVA = UInt(VAddrBits.W)
  // pfSource separates stridePf/chasingPf in the common L1 PrefetchMonitor.
  val pfSource = UInt(L1PfSourceBits.W)
  // Hint-carrying base/recursive requests set the fields below. They pass
  // through LDU/LoadPipe/MSHR and are consumed when a refill creates chasingPf.
  val mdpPfHint = Bool()
  val mdpImm = UInt(MDP_IMM_BITS.W)
  val mdpVaddr = UInt(VAddrBits.W)
  val mdpLoadSize = UInt(2.W)
  val mdpLoadUnsigned = Bool()
  val mdpChainImm = UInt(MDP_IMM_BITS.W)
  val mdpChainValid = Bool()
  val mdpChainLoadSize = UInt(2.W)
  val mdpChainLoadUnsigned = Bool()
  val mdpOrigin = UInt(MdpPfOrigin.width.W)
}

class MdpTrainFilter(size: Int)(implicit p: Parameters) extends XSModule with HasMdpParameters {
  private val enqLanes = backendParams.LduCnt
  private val queueRows = (size + enqLanes - 1) / enqLanes

  val io = IO(new Bundle {
    // One input per LDU from PrefetcherWrapper; the queue preserves concurrent
    // train0 events before sending a single Decoupled stream to MDP.
    val in = Flipped(Vec(enqLanes, ValidIO(new MdpTrainReqBundle)))
    val out = DecoupledIO(new MdpTrainReqBundle)
  })

  val queue = Module(new MultiPortQueue(
    new MdpTrainReqBundle,
    enq_lanes = enqLanes,
    deq_lanes = 1,
    lanes = enqLanes,
    rows = queueRows
  ))

  for (i <- 0 until enqLanes) {
    queue.io.enq(i).valid := io.in(i).valid
    queue.io.enq(i).bits := io.in(i).bits
  }
  io.out <> queue.io.deq.head

  // Performance counters are kept at the end of the class.
  for (i <- 0 until enqLanes) {
    XSPerfAccumulate(s"mdp_train0_filter_drop_$i", io.in(i).valid && !queue.io.enq(i).ready)
  }
  XSPerfAccumulate("mdp_train0_filter_enq", PopCount(queue.io.enq.map(_.fire)))
  XSPerfAccumulate("mdp_train0_filter_deq", io.out.fire)
}

/** Absorb simultaneous MSHR refill completions without collapsing them through
  * a single ParallelMux.  Each MSHR owns one enqueue lane and MDP consumes
  * queued chasingPf events in order; overflow is exposed by per-lane counters.
  */
class MdpChasingPfFilter(size: Int)(implicit p: Parameters) extends XSModule with HasMdpParameters {
  private val enqLanes = cfg.nMissEntries
  private val queueRows = (size + enqLanes - 1) / enqLanes

  val io = IO(new Bundle {
    // One input lane per MissEntry from DCache/MemBlock.  The single output is
    // a serialized chasingPf stream; a full queue reports drops below.
    val in = Flipped(Vec(enqLanes, ValidIO(new MdpChasingPfReqBundle)))
    val out = DecoupledIO(new MdpChasingPfReqBundle)
  })

  val queue = Module(new MultiPortQueue(
    new MdpChasingPfReqBundle,
    enq_lanes = enqLanes,
    deq_lanes = 1,
    lanes = enqLanes,
    rows = queueRows
  ))
  queue.io.enq.zip(io.in).foreach { case (enq, in) =>
    enq.valid := in.valid
    enq.bits := in.bits
  }
  io.out <> queue.io.deq.head

  for (i <- 0 until enqLanes) {
    XSPerfAccumulate(s"mdp_chasing_pf_filter_drop_$i", io.in(i).valid && !queue.io.enq(i).ready)
  }
  XSPerfAccumulate("mdp_chasing_pf_filter_enq", PopCount(queue.io.enq.map(_.fire)))
  XSPerfAccumulate("mdp_chasing_pf_filter_deq", io.out.fire)
}

class MdpPrefetchBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasMdpParameters {
  private val indexWidth = log2Up(size)
  private val lineOffsetWidth = DCacheLineOffset
  private val vlineWidth = VAddrBits - lineOffsetWidth
  private val plineWidth = PAddrBits - lineOffsetWidth

  class Entry extends DCacheBundle {
    val vline = UInt(vlineWidth.W)
    val pline = UInt(plineWidth.W)
    val pvalid = Bool()
    // Set after a TLB miss so retries can be distinguished from first attempts.
    val tlbMiss = Bool()
    val triggerPC = UInt(VAddrBits.W)
    val triggerVA = UInt(VAddrBits.W)
    val pfSource = UInt(L1PfSourceBits.W)
    val mdpPfHint = Bool()
    val mdpImm = UInt(MDP_IMM_BITS.W)
    val mdpVaddr = UInt(VAddrBits.W)
    val mdpLoadSize = UInt(2.W)
    val mdpLoadUnsigned = Bool()
    val mdpChainImm = UInt(MDP_IMM_BITS.W)
    val mdpChainValid = Bool()
    val mdpChainLoadSize = UInt(2.W)
    val mdpChainLoadUnsigned = Bool()
    val mdpOrigin = UInt(MdpPfOrigin.width.W)

    def vaddr: UInt = Cat(vline, 0.U(lineOffsetWidth.W))
    def paddr: UInt = Cat(pline, 0.U(lineOffsetWidth.W))
  }

  val io = IO(new Bundle {
    // srcReq comes from MDP's chasingPf/stridePf source queue.
    val srcReq = Flipped(ValidIO(new MdpSourcePrefetchReq))
    // TLB/PMP use MDP's parameter-assigned port in PrefetcherWrapper/MemBlock.
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle)
    // l1Req goes to MDP's input of PrefetcherWrapper's common L1 arbiter.
    val l1Req = DecoupledIO(new L1PrefetchReq)
  })

  // Entries are deduplicated by virtual cache line.  A new request invalidates
  // any old translation so that VA-to-PA changes cannot reuse stale state.
  val entries = Reg(Vec(size, new Entry))
  val valids = RegInit(VecInit(Seq.fill(size)(false.B)))
  val replacer = new ValidPseudoLRU(size)

  val srcLine = io.srcReq.bits.prefetchVA(VAddrBits - 1, lineOffsetWidth)
  val matchVec = VecInit((0 until size).map(i => io.srcReq.valid && valids(i) && entries(i).vline === srcLine))
  val invalidVec = VecInit(valids.map(! _))
  val replaceCandidates = Mux(invalidVec.asUInt.orR, invalidVec, VecInit(Seq.fill(size)(true.B)))
  val replaceIdx = replacer.way(replaceCandidates.reverse)._2
  val srcIdx = Wire(UInt(indexWidth.W))
  val hasMatch = matchVec.asUInt.orR
  srcIdx := Mux(hasMatch, OHToUInt(matchVec), replaceIdx)
  // A same-line base candidate must not erase a recursive chain that has not
  // reached L1 yet.  A new chain candidate may still promote a base entry.
  val preserveExistingChain = io.srcReq.valid && hasMatch &&
    entries(srcIdx).mdpChainValid && !io.srcReq.bits.mdpChainValid
  val preserveExistingHint = io.srcReq.valid && hasMatch &&
    entries(srcIdx).mdpPfHint && !io.srcReq.bits.mdpPfHint
  val preserveExistingContext = preserveExistingChain || preserveExistingHint

  when(io.srcReq.valid && !preserveExistingContext) {
    entries(srcIdx).vline := srcLine
    entries(srcIdx).pline := 0.U
    entries(srcIdx).pvalid := false.B
    entries(srcIdx).tlbMiss := false.B
    entries(srcIdx).triggerPC := io.srcReq.bits.triggerPC
    entries(srcIdx).triggerVA := io.srcReq.bits.triggerVA
    entries(srcIdx).pfSource := io.srcReq.bits.pfSource
    entries(srcIdx).mdpPfHint := io.srcReq.bits.mdpPfHint
    entries(srcIdx).mdpImm := io.srcReq.bits.mdpImm
    entries(srcIdx).mdpVaddr := io.srcReq.bits.mdpVaddr
    entries(srcIdx).mdpLoadSize := io.srcReq.bits.mdpLoadSize
    entries(srcIdx).mdpLoadUnsigned := io.srcReq.bits.mdpLoadUnsigned
    entries(srcIdx).mdpChainImm := io.srcReq.bits.mdpChainImm
    entries(srcIdx).mdpChainValid := io.srcReq.bits.mdpChainValid
    entries(srcIdx).mdpChainLoadSize := io.srcReq.bits.mdpChainLoadSize
    entries(srcIdx).mdpChainLoadUnsigned := io.srcReq.bits.mdpChainLoadUnsigned
    entries(srcIdx).mdpOrigin := io.srcReq.bits.mdpOrigin
    valids(srcIdx) := true.B
    replacer.access(srcIdx)
  }

  // Each untranslated entry competes for one TLB request.  The three-cycle
  // in-flight mask prevents the same entry from issuing again before response.
  val tlbReqArb = Module(new RRArbiterInit(new TlbReq, size))
  val pfIdxArb = Module(new RRArbiterInit(UInt(indexWidth.W), size))
  val s0_tlbFireOH = VecInit(tlbReqArb.io.in.map(_.fire))
  val s0_tlbRetry = VecInit((0 until size).map(i => s0_tlbFireOH(i) && entries(i).tlbMiss)).asUInt.orR
  // Advance a pulse, not a held index.  Clearing each empty stage is required
  // so notInFlight releases the entry after its s3 response; RegEnable would
  // retain the last one-hot indefinitely and could block a replacement entry.
  val s1_tlbFireOH = RegNext(s0_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val s2_tlbFireOH = RegNext(s1_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val s3_tlbFireOH = RegNext(s2_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val notInFlight = VecInit((0 until size).map(i =>
    !s1_tlbFireOH(i) && !s2_tlbFireOH(i) && !s3_tlbFireOH(i)
  ))

  for (i <- 0 until size) {
    tlbReqArb.io.in(i).valid := valids(i) && !entries(i).pvalid && notInFlight(i)
    tlbReqArb.io.in(i).bits.vaddr := entries(i).vaddr
    tlbReqArb.io.in(i).bits.cmd := TlbCmd.read
    tlbReqArb.io.in(i).bits.isPrefetch := true.B
    tlbReqArb.io.in(i).bits.size := 3.U
    tlbReqArb.io.in(i).bits.kill := false.B
    tlbReqArb.io.in(i).bits.no_translate := false.B
    tlbReqArb.io.in(i).bits.fullva := 0.U
    tlbReqArb.io.in(i).bits.checkfullva := false.B
    tlbReqArb.io.in(i).bits.memidx := DontCare
    tlbReqArb.io.in(i).bits.debug := DontCare
    tlbReqArb.io.in(i).bits.hlvx := false.B
    tlbReqArb.io.in(i).bits.hyperinst := false.B
    tlbReqArb.io.in(i).bits.pmp_addr := DontCare
  }
  tlbReqArb.io.out.ready := true.B

  val s1_tlbReqValid = RegNext(tlbReqArb.io.out.valid, false.B)
  val s1_tlbReqBits = RegEnable(tlbReqArb.io.out.bits, tlbReqArb.io.out.valid)
  val s1_vaddr = RegEnable(tlbReqArb.io.out.bits.vaddr, tlbReqArb.io.out.valid)
  val s1_tlbRetry = RegNext(s0_tlbRetry, false.B)
  io.tlbReq.req.valid := s1_tlbReqValid
  io.tlbReq.req.bits := s1_tlbReqBits
  io.tlbReq.req_kill := false.B
  io.tlbReq.resp.ready := true.B

  // A TLB miss retains the virtual entry.  Once the three-cycle in-flight mask
  // clears, RR arbitration retries it just like MutiLevelPrefetchFilter.  Only
  // translation/access faults and stale responses prevent a translated send.
  val s2_tlbRespValid = io.tlbReq.resp.valid
  val s2_tlbRespBits = io.tlbReq.resp.bits
  val s2_vaddr = RegEnable(s1_vaddr, s1_tlbReqValid)
  val s3_tlbRespValid = RegNext(s2_tlbRespValid, false.B)
  val s3_tlbRespBits = RegEnable(s2_tlbRespBits, s2_tlbRespValid)
  val s3_vaddr = RegEnable(s2_vaddr, s2_tlbRespValid)
  val s3_index = OHToUInt(s3_tlbFireOH.asUInt)
  val s3_overwritten = entries(s3_index).vaddr =/= s3_vaddr
  val s3_sameCycleOverwrite = io.srcReq.valid && srcIdx === s3_index &&
    srcLine =/= s3_vaddr(VAddrBits - 1, lineOffsetWidth)
  val s3_stale = s3_overwritten || s3_sameCycleOverwrite
  val s3_responseMatches = s3_tlbRespValid && s3_tlbFireOH.asUInt.orR && !s3_stale
  val s3_tlbMiss = s3_responseMatches && s3_tlbRespBits.miss
  val s3_tlbHit = s3_responseMatches && !s3_tlbRespBits.miss
  val s3_fault = s3_tlbHit && (
    !PmemRanges.map(_.cover(s3_tlbRespBits.paddr.head)).reduce(_ || _) ||
    s3_tlbRespBits.excp.head.pf.ld || s3_tlbRespBits.excp.head.gpf.ld || s3_tlbRespBits.excp.head.af.ld ||
    io.pmpResp.mmio || Pbmt.isUncache(s3_tlbRespBits.pbmt.head) || io.pmpResp.ld
  )

  when(s3_tlbMiss) {
    entries(s3_index).tlbMiss := true.B
  }.elsewhen(s3_fault) {
    valids(s3_index) := false.B
    entries(s3_index).tlbMiss := false.B
  }.elsewhen(s3_tlbHit) {
    entries(s3_index).pline := s3_tlbRespBits.paddr.head(PAddrBits - 1, lineOffsetWidth)
    entries(s3_index).pvalid := true.B
    entries(s3_index).tlbMiss := false.B
  }

  // Translated entries share one L1 request port.  The MDP source tag lets the
  // common L1 prefetch statistics and downstream metadata identify them.
  for (i <- 0 until size) {
    pfIdxArb.io.in(i).valid := valids(i) && entries(i).pvalid
    pfIdxArb.io.in(i).bits := i.U
  }
  val pfIdx = pfIdxArb.io.out.bits
  pfIdxArb.io.out.ready := io.l1Req.ready
  io.l1Req.valid := pfIdxArb.io.out.valid
  io.l1Req.bits.paddr := entries(pfIdx).paddr
  io.l1Req.bits.vaddr := entries(pfIdx).vaddr
  io.l1Req.bits.confidence := 1.U
  io.l1Req.bits.is_store := false.B
  io.l1Req.bits.pf_source.value := entries(pfIdx).pfSource
  io.l1Req.bits.mdpPfHint := entries(pfIdx).mdpPfHint
  io.l1Req.bits.mdpImm := entries(pfIdx).mdpImm
  io.l1Req.bits.mdpVaddr := entries(pfIdx).mdpVaddr
  io.l1Req.bits.mdpPC := entries(pfIdx).triggerPC
  io.l1Req.bits.mdpLoadSize := entries(pfIdx).mdpLoadSize
  io.l1Req.bits.mdpLoadUnsigned := entries(pfIdx).mdpLoadUnsigned
  io.l1Req.bits.mdpChainImm := entries(pfIdx).mdpChainImm
  io.l1Req.bits.mdpChainValid := entries(pfIdx).mdpChainValid
  io.l1Req.bits.mdpChainLoadSize := entries(pfIdx).mdpChainLoadSize
  io.l1Req.bits.mdpChainLoadUnsigned := entries(pfIdx).mdpChainLoadUnsigned
  io.l1Req.bits.mdpOrigin := entries(pfIdx).mdpOrigin

  when(pfIdxArb.io.out.fire &&
    (!(io.srcReq.valid && srcIdx === pfIdx) || preserveExistingContext)) {
    valids(pfIdx) := false.B
  }

  // MDP owns one prefetcherSeq-assigned non-blocking DTLB requestor.  A TLB
  // response in s2 must correspond to exactly one entry selected in s0.
  assert(PopCount(s0_tlbFireOH) <= 1.U, "L1 MDP TLB request selection must be one-hot")
  assert(!io.tlbReq.req.valid || io.tlbReq.req.ready, "L1 MDP non-blocking TLB request must not be blocked")
  assert(!io.tlbReq.resp.valid || s2_tlbFireOH.asUInt.orR, "L1 MDP TLB response has no matching request")
  val s4_tlbMiss = RegNext(s3_tlbMiss, false.B)
  val s4_tlbMissIndex = RegEnable(s3_index, s3_tlbMiss)
  when(s4_tlbMiss) {
    assert(valids(s4_tlbMissIndex), "L1 MDP TLB miss must retain its prefetch entry for retry")
  }
  when(io.tlbReq.req.fire) {
    assert(io.tlbReq.req.bits.vaddr(lineOffsetWidth - 1, 0) === 0.U,
      "L1 MDP TLB request must be line-aligned")
    assert(io.tlbReq.req.bits.cmd === TlbCmd.read, "L1 MDP TLB request must use read command")
    assert(io.tlbReq.req.bits.isPrefetch, "L1 MDP TLB request must be marked as prefetch")
    assert(!io.tlbReq.req.bits.kill && !io.tlbReq.req.bits.no_translate,
      "L1 MDP TLB request must be translated and must not be killed")
  }

  // Performance counters are kept at the end of the class.
  XSPerfAccumulate("mdp_pf_buffer_src", io.srcReq.valid)
  XSPerfAccumulate("mdp_pf_buffer_prevent_downgrade", preserveExistingContext)
  XSPerfAccumulate("mdp_pf_buffer_preserve_hint", preserveExistingHint)
  XSPerfAccumulate("mdp_pf_buffer_preserve_chain", preserveExistingChain)
  Seq(
    "legacy_chasing" -> isFromMdpLegacyChasing(io.srcReq.bits.pfSource),
    "stride" -> isFromMdpStride(io.srcReq.bits.pfSource),
    "stream" -> isFromMdpStream(io.srcReq.bits.pfSource),
    "chasing_stride" -> isFromMdpChasingStride(io.srcReq.bits.pfSource),
    "chasing_stream" -> isFromMdpChasingStream(io.srcReq.bits.pfSource),
    "chasing_chain" -> isFromMdpChasingChain(io.srcReq.bits.pfSource),
    "chasing_history" -> isFromMdpChasingHistory(io.srcReq.bits.pfSource)
  ).foreach { case (name, sourceMatch) =>
    XSPerfAccumulate(s"mdp_pf_buffer_src_$name", io.srcReq.valid && sourceMatch)
    XSPerfAccumulate(s"mdp_pf_buffer_overwrite_$name",
      io.srcReq.valid && hasMatch && !preserveExistingContext && sourceMatch)
  }
  XSPerfAccumulate("mdp_tlb_req", io.tlbReq.req.fire)
  XSPerfAccumulate("mdp_tlb_req_blocked", io.tlbReq.req.valid && !io.tlbReq.req.ready)
  XSPerfAccumulate("mdp_tlb_resp", io.tlbReq.resp.fire)
  XSPerfAccumulate("mdp_tlb_resp_without_req", io.tlbReq.resp.valid && !s2_tlbFireOH.asUInt.orR)
  XSPerfAccumulate("mdp_tlb_req_without_resp", s2_tlbFireOH.asUInt.orR && !io.tlbReq.resp.valid)
  XSPerfAccumulate("mdp_tlb_miss", s3_tlbMiss)
  XSPerfAccumulate("mdp_tlb_miss_retry", io.tlbReq.req.fire && s1_tlbRetry)
  XSPerfAccumulate("mdp_tlb_fault", s3_fault)
  XSPerfAccumulate("mdp_tlb_stale", s3_tlbRespValid && s3_stale)
  XSPerfAccumulate("mdp_l1_pf_fire", io.l1Req.fire)
  Seq(
    "legacy_chasing" -> isFromMdpLegacyChasing(entries(pfIdx).pfSource),
    "stride" -> isFromMdpStride(entries(pfIdx).pfSource),
    "stream" -> isFromMdpStream(entries(pfIdx).pfSource),
    "chasing_stride" -> isFromMdpChasingStride(entries(pfIdx).pfSource),
    "chasing_stream" -> isFromMdpChasingStream(entries(pfIdx).pfSource),
    "chasing_chain" -> isFromMdpChasingChain(entries(pfIdx).pfSource),
    "chasing_history" -> isFromMdpChasingHistory(entries(pfIdx).pfSource)
  ).foreach { case (name, sourceMatch) =>
    XSPerfAccumulate(s"mdp_l1_pf_fire_$name", io.l1Req.fire && sourceMatch)
  }
}

class MemoryDependencePrefetcher(implicit p: Parameters) extends XSModule with HasMdpParameters {
  // MDT learns producer-load PC -> dependent-load immediate relationships.
  class MdtEntry extends XSBundle {
    val hashPC = UInt(HASH_TAG_WIDTH.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val immCnt = UInt(MDP_COUNTER_BITS.W)
    // Once two confident dependent-load PCs are observed in train0, chainImm
    // records the second MDT immediate used after the first chasing request.
    val chainImm = UInt(MDP_IMM_BITS.W)
    val chainValid = Bool()
    val chainLoadSize = UInt(2.W)
    val chainLoadUnsigned = Bool()
    val conf = UInt(MDP_COUNTER_BITS.W)
    val valid = Bool()
  }

  // ChiselDB row schemas.  Table creation and logging are grouped at the end
  // of the class, away from the predictor datapath.
  class MdpTrain0DBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val wakedupPC = UInt(VAddrBits.W)
    val pc = UInt(VAddrBits.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val hit = Bool()
    val mdtIdx = UInt(log2Up(MDP_MDT_ENTRIES).W)
    val oldImm = UInt(MDP_IMM_BITS.W)
    val oldImmCnt = UInt(MDP_COUNTER_BITS.W)
    val oldConf = UInt(MDP_COUNTER_BITS.W)
    val dependentHit = Bool()
    val dependentImm = UInt(MDP_IMM_BITS.W)
    val oldChainImm = UInt(MDP_IMM_BITS.W)
    val oldChainValid = Bool()
    val chainUpdated = Bool()
    val newChainImm = UInt(MDP_IMM_BITS.W)
    val chainLoadSize = UInt(2.W)
    val chainLoadUnsigned = Bool()
  }

  class MdpHintDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val lduId = UInt(log2Up(backendParams.LduCnt).W)
    val pc = UInt(VAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val chainImm = UInt(MDP_IMM_BITS.W)
    val chainValid = Bool()
    val chainLoadSize = UInt(2.W)
    val chainLoadUnsigned = Bool()
    val robIdx = UInt(log2Ceil(RobSize).W)
  }

  class MdpChasingPfDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val mshrId = UInt(log2Up(cfg.nMissEntries).W)
    val pc = UInt(VAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val data = UInt(XLEN.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val chainImm = UInt(MDP_IMM_BITS.W)
    val chainValid = Bool()
    val chainLoadSize = UInt(2.W)
    val chainLoadUnsigned = Bool()
    val origin = UInt(MdpPfOrigin.width.W)
    val pfSource = UInt(L1PfSourceBits.W)
    val prefetchVaddr = UInt(VAddrBits.W)
  }

  class MdpL1PrefetchDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    // PC of the load that generated this chasingPf or stridePf candidate.
    // MdpPrefetchBuffer carries it across TLB translation to the final fire.
    val triggerPC = UInt(VAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val paddr = UInt(PAddrBits.W)
    val pfSource = UInt(L1PfSourceBits.W)
    val origin = UInt(MdpPfOrigin.width.W)
    val chainValid = Bool()
    val mdpPfHint = Bool()
  }

  val io = IO(new Bundle {
    // train0 arrives from awakened IQ-issued loads through LDU and the
    // multi-LDU MdpTrainFilter; it learns producer PC -> consumer immediate.
    val train0 = Flipped(DecoupledIO(new MdpTrainReqBundle))
    // train1: ordinary completed-load training copied from the existing S3
    // prefetch training path; it updates BST only after an MDT hit.
    val train1 = Flipped(DecoupledIO(new TrainReqBundle))
    // trigger arrives from every scalar demand load entering each LDU S0;
    // l1PfHint returns the s1 lookup result on the same per-LDU lane.
    val trigger = Flipped(Vec(backendParams.LduCnt, ValidIO(new MdpTriggerReqBundle)))
    val l1PfHint = Vec(backendParams.LduCnt, ValidIO(new MdpL1PfHintBundle))
    // chasingPf comes from DCache MissQueue refills through MdpChasingPfFilter.
    val chasingPf = Flipped(DecoupledIO(new MdpChasingPfReqBundle))
    // Translation/protection use MDP's parameter-assigned DTLB/PMP lane;
    // accepted requests return to PrefetcherWrapper's common L1 arbiter.
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle)
    val l1PrefetchReq = DecoupledIO(new L1PrefetchReq)
  })

  val mdt = RegInit(VecInit(Seq.fill(MDP_MDT_ENTRIES)(0.U.asTypeOf(new MdtEntry))))
  val mdtPlru = new ValidPseudoLRU(MDP_MDT_ENTRIES)

  // train0 s0: query MDT and select the update/replacement entry.
  // Hold s0 for one cycle while s1 writes MDT. This prevents a back-to-back
  // same-entry lookup from capturing the value before the preceding update;
  // MdpTrainFilter retains the blocked event.
  val s1_train0Valid = RegInit(false.B)
  io.train0.ready := !s1_train0Valid
  val s0_train0Hash = pc_hash_tag(io.train0.bits.wakedupPC)
  val s0_train0MatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === s0_train0Hash))
  val s0_train0Hit = s0_train0MatchVec.asUInt.orR
  val s0_train0DependentHash = pc_hash_tag(io.train0.bits.pc)
  val s0_train0DependentMatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === s0_train0DependentHash))
  val s0_train0DependentHit = s0_train0DependentMatchVec.asUInt.orR
  val s0_train0DependentEntry = Mux1H(s0_train0DependentMatchVec, mdt)
  val mdtMinConf = mdt.map(_.conf).reduce((a, b) => Mux(a < b, a, b))
  val mdtInvalidVec = VecInit(mdt.map(!_.valid))
  val mdtLowConfVec = VecInit(mdt.map(e => e.valid && e.conf === mdtMinConf))
  val mdtReplaceCandidates = Mux(mdtInvalidVec.asUInt.orR, mdtInvalidVec, mdtLowConfVec)
  val mdtReplaceIdx = mdtPlru.way(mdtReplaceCandidates.reverse)._2
  val s0_train0Idx = Wire(UInt(log2Up(MDP_MDT_ENTRIES).W))
  s0_train0Idx := Mux(s0_train0Hit, OHToUInt(s0_train0MatchVec), mdtReplaceIdx)

  val s1_train0Bits = RegEnable(io.train0.bits, io.train0.fire)
  val s1_train0Hash = RegEnable(s0_train0Hash, io.train0.fire)
  val s1_train0Hit = RegEnable(s0_train0Hit, io.train0.fire)
  val s1_train0Idx = RegEnable(s0_train0Idx, io.train0.fire)
  val s1_train0OldEntry = RegEnable(mdt(s0_train0Idx), io.train0.fire)
  val s1_train0DependentHit = RegEnable(s0_train0DependentHit, io.train0.fire)
  val s1_train0DependentEntry = RegEnable(s0_train0DependentEntry, io.train0.fire)
  when(io.train0.fire) {
    s1_train0Valid := true.B
  }.elsewhen(s1_train0Valid) {
    s1_train0Valid := false.B
  }

  // train0 s1: update MDT.  This is the only stage that writes MDT state.
  val s1_train0ChainCanUpdate = s1_train0Valid && s1_train0Bits.wakedup &&
    s1_train0Hit && s1_train0DependentHit &&
    s1_train0OldEntry.conf >= MDP_CONF_THRESHOLD.U &&
    s1_train0DependentEntry.conf >= MDP_CONF_THRESHOLD.U
  val s1_train0OldChainS = s1_train0OldEntry.chainImm.asSInt.pad(MDP_IMM_BITS + 1)
  val s1_train0DependentImmS = s1_train0DependentEntry.imm.asSInt.pad(MDP_IMM_BITS + 1)
  val s1_train0ChainDiff = Mux(
    s1_train0OldChainS >= s1_train0DependentImmS,
    (s1_train0OldChainS - s1_train0DependentImmS).asUInt,
    (s1_train0DependentImmS - s1_train0OldChainS).asUInt
  )
  val s1_train0ChainNear = s1_train0OldEntry.chainValid && s1_train0ChainDiff <= 1.U
  val s1_train0ChainMin = Mux(
    s1_train0OldChainS <= s1_train0DependentImmS,
    s1_train0OldEntry.chainImm,
    s1_train0DependentEntry.imm
  )
  val s1_train0NewChainImm = Mux(
    s1_train0ChainNear,
    s1_train0ChainMin,
    s1_train0DependentEntry.imm
  )
  when(s1_train0Valid && s1_train0Bits.wakedup) {
    val entry = mdt(s1_train0Idx)
    val oldImm = s1_train0OldEntry.imm.asSInt.pad(MDP_IMM_BITS + 1)
    val newImm = s1_train0Bits.imm.asSInt.pad(MDP_IMM_BITS + 1)
    // V0.4 treats only an exactly repeated immediate as confirmation of the
    // current dependence.  A near value is a mismatch and must decay immCnt.
    val immSame = oldImm === newImm
    val immCntDec = Mux(s1_train0OldEntry.immCnt === 0.U, 0.U, s1_train0OldEntry.immCnt - 1.U)

    when(s1_train0Hit) {
      entry.conf := Mux(
        s1_train0OldEntry.conf === mdpCounterMax.U,
        s1_train0OldEntry.conf,
        s1_train0OldEntry.conf + 1.U
      )
      when(immSame) {
        entry.imm := s1_train0OldEntry.imm
        entry.immCnt := Mux(
          s1_train0OldEntry.immCnt === mdpCounterMax.U,
          s1_train0OldEntry.immCnt,
          s1_train0OldEntry.immCnt + 1.U
        )
      }.otherwise {
        entry.immCnt := immCntDec
        when(immCntDec < MDP_IMM_CNT_THRESHOLD.U) {
          entry.imm := s1_train0Bits.imm
          entry.immCnt := MDP_IMM_CNT_INIT.U
        }
      }
      when(s1_train0ChainCanUpdate) {
        entry.chainImm := s1_train0NewChainImm
        entry.chainValid := true.B
        entry.chainLoadSize := s1_train0Bits.loadSize
        entry.chainLoadUnsigned := s1_train0Bits.loadUnsigned
      }
    }.otherwise {
      entry.hashPC := s1_train0Hash
      entry.imm := s1_train0Bits.imm
      entry.immCnt := MDP_IMM_CNT_INIT.U
      entry.conf := MDP_CONF_INIT.U
      entry.chainImm := 0.U
      entry.chainValid := false.B
      entry.chainLoadSize := 0.U
      entry.chainLoadUnsigned := false.B
      entry.valid := true.B
    }
    mdtPlru.access(s1_train0Idx)
  }

  // trigger s0: all LDUs query MDT in parallel.  Only lookup results are
  // captured here; the hint decision is intentionally delayed to s1.
  for (i <- 0 until backendParams.LduCnt) {
    val s0_triggerHash = pc_hash_tag(io.trigger(i).bits.pc)
    val s0_triggerMatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === s0_triggerHash))
    val s0_triggerHit = s0_triggerMatchVec.asUInt.orR
    val s0_triggerEntry = Mux1H(s0_triggerMatchVec, mdt)

    val s1_triggerValid = RegNext(io.trigger(i).valid, false.B)
    val s1_triggerBits = RegEnable(io.trigger(i).bits, io.trigger(i).valid)
    val s1_triggerHit = RegEnable(s0_triggerHit, io.trigger(i).valid)
    val s1_triggerEntry = RegEnable(s0_triggerEntry, io.trigger(i).valid)

    // trigger s1: chasingPf permission depends only on MDT confidence.  The
    // request identity is returned unchanged so LDU can assert exact alignment.
    val s1_triggerConfident = s1_triggerHit && s1_triggerEntry.conf >= MDP_CONF_THRESHOLD.U
    // TODO: pfHintMDP needs an additional register after hint generation.  The
    // LDU and LoadPipe request-identity pipelines must be delayed with it.
    io.l1PfHint(i).valid := s1_triggerValid && s1_triggerConfident
    io.l1PfHint(i).bits.imm := s1_triggerEntry.imm
    io.l1PfHint(i).bits.mdpChainImm := s1_triggerEntry.chainImm
    io.l1PfHint(i).bits.mdpChainValid := s1_triggerEntry.chainValid
    io.l1PfHint(i).bits.mdpChainLoadSize := s1_triggerEntry.chainLoadSize
    io.l1PfHint(i).bits.mdpChainLoadUnsigned := s1_triggerEntry.chainLoadUnsigned
    io.l1PfHint(i).bits.mdpOrigin := MdpPfOrigin.legacy
    io.l1PfHint(i).bits.pc := s1_triggerBits.pc
    io.l1PfHint(i).bits.vaddr := s1_triggerBits.vaddr
    io.l1PfHint(i).bits.lduId := s1_triggerBits.lduId
    io.l1PfHint(i).bits.robIdx := s1_triggerBits.robIdx
  }

  // train1 s0: query MDT once, then atomically fork the training event to BSeT
  // and BSmT. Both child modules use this boundary as their s0 anchor.
  val s0_train1Hash = pc_hash_tag(io.train1.bits.pc)
  val s0_train1MdtMatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === s0_train1Hash))
  val s0_train1MdtHit = s0_train1MdtMatchVec.asUInt.orR
  val s0_train1MdtEntry = Mux1H(s0_train1MdtMatchVec, mdt)
  val s0_train1MdtIdx = OHToUInt(s0_train1MdtMatchVec)
  val train1MdtInfo = Wire(new MdpMdtInfoBundle)
  train1MdtInfo.hit := s0_train1MdtHit
  train1MdtInfo.index := s0_train1MdtIdx
  train1MdtInfo.hashPC := s0_train1Hash
  train1MdtInfo.conf := s0_train1MdtEntry.conf
  train1MdtInfo.imm := s0_train1MdtEntry.imm
  train1MdtInfo.chainImm := s0_train1MdtEntry.chainImm
  train1MdtInfo.chainValid := s0_train1MdtEntry.chainValid
  train1MdtInfo.chainLoadSize := s0_train1MdtEntry.chainLoadSize
  train1MdtInfo.chainLoadUnsigned := s0_train1MdtEntry.chainLoadUnsigned

  val bset = Module(new BaseStrideTable)
  val bsmt = Module(new BaseStreamTable(mdpParams.bsmtEntries, mdpParams.bsmtDepthBlocks))
  val train1TablesReady = bset.io.train.ready && bsmt.io.train.ready
  io.train1.ready := train1TablesReady
  bset.io.train.valid := io.train1.valid && train1TablesReady
  bset.io.train.bits := io.train1.bits
  bset.io.mdtInfo := train1MdtInfo
  bsmt.io.train.valid := io.train1.valid && train1TablesReady
  bsmt.io.train.bits := io.train1.bits
  bsmt.io.mdtInfo := train1MdtInfo
  assert(bset.io.train.fire === bsmt.io.train.fire,
    "BSeT and BSmT must consume every train1 event atomically")

  // chasingPf is asynchronous to train0/train1.  Capture it in s1 and register
  // the data+imm result once more so the prefetch address is produced in s2.
  io.chasingPf.ready := true.B
  val s1_chasingPfValid = RegNext(io.chasingPf.fire, false.B)
  val s1_chasingPfBits = RegEnable(io.chasingPf.bits, io.chasingPf.fire)
  val s1_chasingPfVaddr = (
    s1_chasingPfBits.data + SignExt(s1_chasingPfBits.imm, XLEN)
  )(VAddrBits - 1, 0)
  val s2_chasingPfValid = RegNext(s1_chasingPfValid, false.B)
  val s2_chasingPfBits = RegEnable(s1_chasingPfBits, s1_chasingPfValid)
  val s2_chasingPfVaddr = RegEnable(s1_chasingPfVaddr, s1_chasingPfValid)
  val s2_chasingPfLine = Cat(
    s2_chasingPfVaddr(VAddrBits - 1, DCacheLineOffset),
    0.U(DCacheLineOffset.W)
  )
  val s2_chasingPfSource = MuxLookup(
    s2_chasingPfBits.mdpOrigin,
    L1_HW_PREFETCH_MDP_CHASING
  )(Seq(
    MdpPfOrigin.stride -> L1_HW_PREFETCH_MDP_CHASING_STRIDE,
    MdpPfOrigin.stream -> L1_HW_PREFETCH_MDP_CHASING_STREAM,
    MdpPfOrigin.chain -> L1_HW_PREFETCH_MDP_CHASING_CHAIN,
    MdpPfOrigin.history -> L1_HW_PREFETCH_MDP_CHASING_HISTORY,
    MdpPfOrigin.legacy -> L1_HW_PREFETCH_MDP_CHASING
  ))

  // chasingDepth is the total number of generated requests.  V0.3 sets it to
  // one, so only data + signext(imm) is sent; larger values retain the existing
  // behavior of adding adjacent lines in the learned immediate direction.
  // Only depth zero recursively carries BaseChain metadata; otherwise one
  // refill could fan out into an exponentially growing chain.
  val chasingPfReqs = Seq.tabulate(MDP_CHASING_DEPTH) { depth =>
    val req = Wire(Valid(new MdpSourcePrefetchReq))
    val lineDelta = (depth * blockBytes).U(VAddrBits.W)
    val depthVaddr = if (depth == 0) {
      s2_chasingPfVaddr
    } else {
      Mux(
        s2_chasingPfBits.imm(MDP_IMM_BITS - 1),
        s2_chasingPfLine - lineDelta,
        s2_chasingPfLine + lineDelta
      )
    }
    req.valid := s2_chasingPfValid
    req.bits.triggerPC := s2_chasingPfBits.pc
    req.bits.triggerVA := s2_chasingPfBits.vaddr
    req.bits.prefetchVA := depthVaddr
    val recursive = (depth == 0).B && s2_chasingPfBits.mdpChainValid
    req.bits.pfSource := s2_chasingPfSource
    req.bits.mdpPfHint := recursive
    req.bits.mdpImm := Mux(recursive, s2_chasingPfBits.mdpChainImm, 0.U)
    req.bits.mdpVaddr := depthVaddr
    req.bits.mdpLoadSize := Mux(recursive, s2_chasingPfBits.mdpChainLoadSize, s2_chasingPfBits.mdpLoadSize)
    req.bits.mdpLoadUnsigned := Mux(
      recursive,
      s2_chasingPfBits.mdpChainLoadUnsigned,
      s2_chasingPfBits.mdpLoadUnsigned
    )
    // Consume one trained chain descriptor. The resulting carrier is hinted so
    // its refill emits the next chasing request, but it cannot recursively reuse
    // the same descriptor without another PC-indexed MDT lookup.
    req.bits.mdpChainImm := 0.U
    req.bits.mdpChainValid := false.B
    req.bits.mdpChainLoadSize := 0.U
    req.bits.mdpChainLoadUnsigned := false.B
    req.bits.mdpOrigin := MdpPfOrigin.chain
    req
  }

  // BSeT and BSmT both emit hint carriers. If they miss, their exact address,
  // MDT/chain metadata and load semantics return through the MSHR as chasingPf.
  val stridePfReq = Wire(Valid(new MdpSourcePrefetchReq))
  stridePfReq := bset.io.stridePf
  val rawStreamPfReq = Wire(Valid(new MdpSourcePrefetchReq))
  rawStreamPfReq := bsmt.io.streamPf
  val basePfSameLine = stridePfReq.valid && rawStreamPfReq.valid &&
    stridePfReq.bits.prefetchVA(VAddrBits - 1, DCacheLineOffset) ===
      rawStreamPfReq.bits.prefetchVA(VAddrBits - 1, DCacheLineOffset)
  val streamPfReq = Wire(Valid(new MdpSourcePrefetchReq))
  streamPfReq.bits := rawStreamPfReq.bits
  streamPfReq.valid := rawStreamPfReq.valid && !basePfSameLine

  // All depth candidates and two base prefetches may be generated together; independent
  // enqueue lanes preserve them before the shared translation/filter buffer.
  val sourceEnqLanes = MDP_CHASING_DEPTH + 2
  val sourceQueue = Module(new MultiPortQueue(
    new MdpSourcePrefetchReq,
    enq_lanes = sourceEnqLanes,
    deq_lanes = 1,
    lanes = sourceEnqLanes,
    rows = mdpParams.sourceQueueRows
  ))
  chasingPfReqs.zipWithIndex.foreach { case (req, i) =>
    sourceQueue.io.enq(i).valid := req.valid
    sourceQueue.io.enq(i).bits := req.bits
  }
  sourceQueue.io.enq(MDP_CHASING_DEPTH).valid := stridePfReq.valid
  sourceQueue.io.enq(MDP_CHASING_DEPTH).bits := stridePfReq.bits
  sourceQueue.io.enq(MDP_CHASING_DEPTH + 1).valid := streamPfReq.valid
  sourceQueue.io.enq(MDP_CHASING_DEPTH + 1).bits := streamPfReq.bits

  val pfBuffer = Module(new MdpPrefetchBuffer(mdpParams.prefetchBufferEntries))
  pfBuffer.io.srcReq.valid := sourceQueue.io.deq.head.valid
  pfBuffer.io.srcReq.bits := sourceQueue.io.deq.head.bits
  sourceQueue.io.deq.head.ready := true.B
  io.tlbReq <> pfBuffer.io.tlbReq
  pfBuffer.io.pmpResp := io.pmpResp
  io.l1PrefetchReq <> pfBuffer.io.l1Req

  // ChiselDB instrumentation is grouped at the end of the class.
  val hartId = p(XSCoreParamsKey).HartId
  val train0Table = ChiselDB.createTable(s"mdpTrain0_hart$hartId", new MdpTrain0DBEntry, basicDB = true)
  val hintTable = ChiselDB.createTable(s"mdpHint_hart$hartId", new MdpHintDBEntry, basicDB = true)
  val chasingPfTable = ChiselDB.createTable(s"mdpChasingPf_hart$hartId", new MdpChasingPfDBEntry, basicDB = true)
  val l1PrefetchTable = ChiselDB.createTable(s"mdpL1Prefetch_hart$hartId", new MdpL1PrefetchDBEntry, basicDB = true)

  val train0Log = Wire(new MdpTrain0DBEntry)
  train0Log.timeCnt := GTimer()
  train0Log.wakedupPC := s1_train0Bits.wakedupPC
  train0Log.pc := s1_train0Bits.pc
  train0Log.imm := s1_train0Bits.imm
  train0Log.hit := s1_train0Hit
  train0Log.mdtIdx := s1_train0Idx
  train0Log.oldImm := s1_train0OldEntry.imm
  train0Log.oldImmCnt := s1_train0OldEntry.immCnt
  train0Log.oldConf := s1_train0OldEntry.conf
  train0Log.dependentHit := s1_train0DependentHit
  train0Log.dependentImm := Mux(s1_train0DependentHit, s1_train0DependentEntry.imm, 0.U)
  train0Log.oldChainImm := s1_train0OldEntry.chainImm
  train0Log.oldChainValid := s1_train0OldEntry.chainValid
  train0Log.chainUpdated := s1_train0ChainCanUpdate
  train0Log.newChainImm := Mux(
    s1_train0ChainCanUpdate,
    s1_train0NewChainImm,
    s1_train0OldEntry.chainImm
  )
  train0Log.chainLoadSize := s1_train0Bits.loadSize
  train0Log.chainLoadUnsigned := s1_train0Bits.loadUnsigned
  train0Table.log(train0Log, s1_train0Valid, "mdp", clock, reset)

  for (i <- 0 until backendParams.LduCnt) {
    val hintLog = Wire(new MdpHintDBEntry)
    hintLog.timeCnt := GTimer()
    hintLog.lduId := io.l1PfHint(i).bits.lduId
    hintLog.pc := io.l1PfHint(i).bits.pc
    hintLog.vaddr := io.l1PfHint(i).bits.vaddr
    hintLog.imm := io.l1PfHint(i).bits.imm
    hintLog.chainImm := io.l1PfHint(i).bits.mdpChainImm
    hintLog.chainValid := io.l1PfHint(i).bits.mdpChainValid
    hintLog.chainLoadSize := io.l1PfHint(i).bits.mdpChainLoadSize
    hintLog.chainLoadUnsigned := io.l1PfHint(i).bits.mdpChainLoadUnsigned
    hintLog.robIdx := io.l1PfHint(i).bits.robIdx.value
    hintTable.log(hintLog, io.l1PfHint(i).valid, s"ldu$i", clock, reset)
  }

  val chasingPfLog = Wire(new MdpChasingPfDBEntry)
  chasingPfLog.timeCnt := GTimer()
  chasingPfLog.mshrId := s2_chasingPfBits.mshrId
  chasingPfLog.pc := s2_chasingPfBits.pc
  chasingPfLog.vaddr := s2_chasingPfBits.vaddr
  chasingPfLog.data := s2_chasingPfBits.data
  chasingPfLog.imm := s2_chasingPfBits.imm
  chasingPfLog.chainImm := s2_chasingPfBits.mdpChainImm
  chasingPfLog.chainValid := s2_chasingPfBits.mdpChainValid
  chasingPfLog.chainLoadSize := s2_chasingPfBits.mdpChainLoadSize
  chasingPfLog.chainLoadUnsigned := s2_chasingPfBits.mdpChainLoadUnsigned
  chasingPfLog.origin := s2_chasingPfBits.mdpOrigin
  chasingPfLog.pfSource := s2_chasingPfSource
  chasingPfLog.prefetchVaddr := s2_chasingPfVaddr
  chasingPfTable.log(chasingPfLog, s2_chasingPfValid, "mdp", clock, reset)

  val l1PrefetchLog = Wire(new MdpL1PrefetchDBEntry)
  l1PrefetchLog.timeCnt := GTimer()
  l1PrefetchLog.triggerPC := io.l1PrefetchReq.bits.mdpPC
  l1PrefetchLog.vaddr := io.l1PrefetchReq.bits.vaddr
  l1PrefetchLog.paddr := io.l1PrefetchReq.bits.paddr
  l1PrefetchLog.pfSource := io.l1PrefetchReq.bits.pf_source.value
  l1PrefetchLog.origin := io.l1PrefetchReq.bits.mdpOrigin
  l1PrefetchLog.chainValid := io.l1PrefetchReq.bits.mdpChainValid
  l1PrefetchLog.mdpPfHint := io.l1PrefetchReq.bits.mdpPfHint
  l1PrefetchTable.log(l1PrefetchLog, io.l1PrefetchReq.fire, "mdp", clock, reset)

  // Assertions and performance counters are kept at the end of the class.
  assert(PopCount(s0_train0MatchVec) <= 1.U)
  assert(PopCount(s0_train0DependentMatchVec) <= 1.U)
  assert(PopCount(s0_train1MdtMatchVec) <= 1.U)

  XSPerfAccumulate("mdp_train0", io.train0.fire)
  XSPerfAccumulate("mdp_train0_hit", s1_train0Valid && s1_train0Hit)
  XSPerfAccumulate("mdp_train0_alloc", s1_train0Valid && !s1_train0Hit)
  XSPerfAccumulate("mdp_mdt_imm_same", s1_train0Valid && s1_train0Hit &&
    s1_train0OldEntry.imm === s1_train0Bits.imm)
  XSPerfAccumulate("mdp_mdt_imm_mismatch_decay", s1_train0Valid && s1_train0Hit &&
    s1_train0OldEntry.imm =/= s1_train0Bits.imm &&
    s1_train0OldEntry.immCnt > MDP_IMM_CNT_THRESHOLD.U)
  XSPerfAccumulate("mdp_mdt_imm_mismatch_replace", s1_train0Valid && s1_train0Hit &&
    s1_train0OldEntry.imm =/= s1_train0Bits.imm &&
    Mux(s1_train0OldEntry.immCnt === 0.U, 0.U, s1_train0OldEntry.immCnt - 1.U) <
      MDP_IMM_CNT_THRESHOLD.U)
  XSPerfAccumulate("mdp_mdt_chain_update", s1_train0ChainCanUpdate)
  XSPerfAccumulate("mdp_mdt_chain_update_initial", s1_train0ChainCanUpdate &&
    !s1_train0OldEntry.chainValid)
  XSPerfAccumulate("mdp_mdt_chain_update_near_keep", s1_train0ChainCanUpdate &&
    s1_train0ChainNear && s1_train0OldChainS <= s1_train0DependentImmS)
  XSPerfAccumulate("mdp_mdt_chain_update_near_smaller", s1_train0ChainCanUpdate &&
    s1_train0ChainNear && s1_train0OldChainS > s1_train0DependentImmS)
  XSPerfAccumulate("mdp_mdt_chain_update_far_replace", s1_train0ChainCanUpdate &&
    s1_train0OldEntry.chainValid && !s1_train0ChainNear)
  XSPerfAccumulate("mdp_train1", io.train1.fire)
  XSPerfAccumulate("mdp_train1_mdt_hit", io.train1.fire && s0_train1MdtHit)
  XSPerfAccumulate("mdp_stride_pf_s2", stridePfReq.valid)
  XSPerfAccumulate("mdp_stream_pf_s2", rawStreamPfReq.valid)
  XSPerfAccumulate("mdp_base_pf_same_line", basePfSameLine)
  XSPerfAccumulate("mdp_chasing_pf", io.chasingPf.fire)
  XSPerfAccumulate("mdp_chasing_pf_s2", s2_chasingPfValid)
  chasingPfReqs.zipWithIndex.foreach { case (req, i) =>
    XSPerfAccumulate(s"mdp_chasing_pf_queue_drop_$i", req.valid && !sourceQueue.io.enq(i).ready)
  }
  XSPerfAccumulate(
    "mdp_stride_pf_queue_drop",
    stridePfReq.valid && !sourceQueue.io.enq(MDP_CHASING_DEPTH).ready
  )
  XSPerfAccumulate(
    "mdp_stream_pf_queue_drop",
    streamPfReq.valid && !sourceQueue.io.enq(MDP_CHASING_DEPTH + 1).ready
  )
  XSPerfAccumulate("mdp_chain_recursive_pf", chasingPfReqs.head.valid &&
    chasingPfReqs.head.bits.mdpPfHint)
  for (i <- 0 until backendParams.LduCnt) {
    XSPerfAccumulate(s"mdp_trigger_$i", io.trigger(i).valid)
    XSPerfAccumulate(s"mdp_hint_$i", io.l1PfHint(i).valid)
  }
}
