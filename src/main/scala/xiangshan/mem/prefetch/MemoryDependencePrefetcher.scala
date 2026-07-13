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

trait HasMdpParameters extends HasL1PrefetchHelper {
  // The dependency immediate uses the RISC-V I-type immediate width.  MDT and
  // BST sizes/thresholds are intentionally small so the predictor is fully
  // associative without adding a long lookup path.
  val MDP_IMM_BITS = 12
  val MDP_MDT_ENTRIES = 16
  val MDP_BST_ENTRIES = 8
  val MDP_COUNTER_BITS = 3

  val MDP_CONF_INIT = 1
  val MDP_CONF_THRESHOLD = 4
  val MDP_IMM_CNT_INIT = 4
  val MDP_IMM_CNT_THRESHOLD = 2
  val MDP_BST_CONF_THRESHOLD = 3

  def mdpCounterMax: Int = (1 << MDP_COUNTER_BITS) - 1
}

class MdpTrainReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From each LDU S0 (through MdpTrainFilter) to MDP train0.  The producer PC
  // is the MDT key and pc/imm describe the dependent load.
  val wakedup = Bool()
  val wakedupPC = UInt(VAddrBits.W)
  val pc = UInt(VAddrBits.W)
  val imm = UInt(MDP_IMM_BITS.W)
  val robIdx = new RobPtr
}

class MdpTriggerReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From each LDU's first demand-load issue directly to the corresponding
  // parallel MDP trigger port.
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val lduId = UInt(log2Up(backendParams.LduCnt).W)
}

class MdpL1PfHintBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From MDP back to the matching LDU.  pc/vaddr/lduId identify the exact
  // in-flight request and imm is retained by its DCache MSHR on a miss.
  val imm = UInt(MDP_IMM_BITS.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val lduId = UInt(log2Up(backendParams.LduCnt).W)
}

class MdpPendingReqBundle(implicit p: Parameters) extends XSBundle with HasMdpParameters {
  // From a completed hinted DCache MSHR refill to MDP.  data is the extracted
  // load value; MDP adds imm to it to form the dependent prefetch address.
  val data = UInt(XLEN.W)
  val imm = UInt(MDP_IMM_BITS.W)
  val pc = UInt(VAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)
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

class MdpPrefetchBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasMdpParameters {
  private val indexWidth = log2Up(size)
  private val lineOffsetWidth = DCacheLineOffset
  private val vlineWidth = VAddrBits - lineOffsetWidth
  private val plineWidth = PAddrBits - lineOffsetWidth

  class Entry extends DCacheBundle {
    val vline = UInt(vlineWidth.W)
    val pline = UInt(plineWidth.W)
    val pvalid = Bool()
    val triggerPC = UInt(VAddrBits.W)
    val triggerVA = UInt(VAddrBits.W)

    def vaddr: UInt = Cat(vline, 0.U(lineOffsetWidth.W))
    def paddr: UInt = Cat(pline, 0.U(lineOffsetWidth.W))
  }

  val io = IO(new Bundle {
    // srcReq comes from the pending/stride source queue in MDP.
    val srcReq = Flipped(ValidIO(new SourcePrefetchReq))
    // TLB/PMP connect to the dedicated MDP DTLB port in MemBlock.
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle)
    // l1Req goes to the MDP input of PrefetcherWrapper's L1 arbiter.
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
  srcIdx := Mux(matchVec.asUInt.orR, OHToUInt(matchVec), replaceIdx)

  when(io.srcReq.valid) {
    entries(srcIdx).vline := srcLine
    entries(srcIdx).pline := 0.U
    entries(srcIdx).pvalid := false.B
    entries(srcIdx).triggerPC := io.srcReq.bits.triggerPC
    entries(srcIdx).triggerVA := io.srcReq.bits.triggerVA
    valids(srcIdx) := true.B
    replacer.access(srcIdx)
  }

  // Each untranslated entry competes for one TLB request.  The three-cycle
  // in-flight mask prevents the same entry from issuing again before response.
  val tlbReqArb = Module(new RRArbiterInit(new TlbReq, size))
  val pfIdxArb = Module(new RRArbiterInit(UInt(indexWidth.W), size))
  val s0TlbFireOH = VecInit(tlbReqArb.io.in.map(_.fire))
  val s0TlbFire = s0TlbFireOH.asUInt.orR
  val s1TlbFire = RegNext(s0TlbFire, false.B)
  val s2TlbFire = RegNext(s1TlbFire, false.B)
  val s1TlbFireOH = RegEnable(s0TlbFireOH, 0.U.asTypeOf(s0TlbFireOH), s0TlbFire)
  val s2TlbFireOH = RegEnable(s1TlbFireOH, 0.U.asTypeOf(s0TlbFireOH), s1TlbFire)
  val s3TlbFireOH = RegEnable(s2TlbFireOH, 0.U.asTypeOf(s0TlbFireOH), s2TlbFire)
  val notInFlight = VecInit((0 until size).map(i => !s1TlbFireOH(i) && !s2TlbFireOH(i) && !s3TlbFireOH(i)))

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

  val s1TlbReqValid = RegNext(tlbReqArb.io.out.valid, false.B)
  val s1TlbReqBits = RegEnable(tlbReqArb.io.out.bits, tlbReqArb.io.out.valid)
  val s1Vaddr = RegEnable(tlbReqArb.io.out.bits.vaddr, tlbReqArb.io.out.valid)
  io.tlbReq.req.valid := s1TlbReqValid
  io.tlbReq.req.bits := s1TlbReqBits
  io.tlbReq.req_kill := false.B
  io.tlbReq.resp.ready := true.B

  // Reject misses, access faults, MMIO/uncacheable addresses and stale
  // responses for entries that were replaced while translation was in flight.
  val s2TlbRespValid = io.tlbReq.resp.valid
  val s2TlbRespBits = io.tlbReq.resp.bits
  val s2Vaddr = RegEnable(s1Vaddr, s1TlbReqValid)
  val s3TlbRespValid = RegNext(s2TlbRespValid, false.B)
  val s3TlbRespBits = RegEnable(s2TlbRespBits, s2TlbRespValid)
  val s3Vaddr = RegEnable(s2Vaddr, s2TlbRespValid)
  val s3Index = OHToUInt(s3TlbFireOH.asUInt)
  val s3TlbHit = s3TlbRespValid && !s3TlbRespBits.miss
  val s3Fault = s3TlbHit && (
    !PmemRanges.map(_.cover(s3TlbRespBits.paddr.head)).reduce(_ || _) ||
    s3TlbRespBits.excp.head.pf.ld || s3TlbRespBits.excp.head.gpf.ld || s3TlbRespBits.excp.head.af.ld ||
    io.pmpResp.mmio || Pbmt.isUncache(s3TlbRespBits.pbmt.head) || io.pmpResp.ld
  )
  val s3Overwritten = entries(s3Index).vaddr =/= s3Vaddr
  val s3SameCycleOverwrite = io.srcReq.valid && srcIdx === s3Index &&
    srcLine =/= s3Vaddr(VAddrBits - 1, lineOffsetWidth)
  val s3Stale = s3Overwritten || s3SameCycleOverwrite

  when(s3TlbRespValid && !s3Stale && (s3TlbRespBits.miss || s3Fault)) {
    valids(s3Index) := false.B
  }.elsewhen(s3TlbHit && !s3Fault && !s3Stale) {
    entries(s3Index).pline := s3TlbRespBits.paddr.head(PAddrBits - 1, lineOffsetWidth)
    entries(s3Index).pvalid := true.B
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
  io.l1Req.bits.pf_source.value := L1_HW_PREFETCH_MDP

  when(pfIdxArb.io.out.fire && !(io.srcReq.valid && srcIdx === pfIdx)) {
    valids(pfIdx) := false.B
  }

  // Performance counters are kept at the end of the class.
  XSPerfAccumulate("mdp_pf_buffer_src", io.srcReq.valid)
  XSPerfAccumulate("mdp_tlb_req", io.tlbReq.req.fire)
  XSPerfAccumulate("mdp_tlb_miss", s3TlbRespValid && s3TlbRespBits.miss)
  XSPerfAccumulate("mdp_tlb_fault", s3Fault)
  XSPerfAccumulate("mdp_tlb_stale", s3TlbRespValid && s3Stale)
  XSPerfAccumulate("mdp_l1_pf_fire", io.l1Req.fire)
}

class MemoryDependencePrefetcher(implicit p: Parameters) extends XSModule with HasMdpParameters {
  // MDT learns producer-load PC -> dependent-load immediate relationships.
  class MdtEntry extends XSBundle {
    val hashPC = UInt(HASH_TAG_WIDTH.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val immCnt = UInt(MDP_COUNTER_BITS.W)
    val conf = UInt(MDP_COUNTER_BITS.W)
    val valid = Bool()
  }

  // BST learns address strides only for PCs that are already present in MDT.
  class BstEntry extends XSBundle {
    val hashPC = UInt(HASH_TAG_WIDTH.W)
    val prevVaddr = UInt(VAddrBits.W)
    val stride = UInt(VAddrBits.W)
    val decr = Bool()
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
  }

  class MdpHintDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val lduId = UInt(log2Up(backendParams.LduCnt).W)
    val pc = UInt(VAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val imm = UInt(MDP_IMM_BITS.W)
  }

  class MdpPendingDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val mshrId = UInt(log2Up(cfg.nMissEntries).W)
    val pc = UInt(VAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val data = UInt(XLEN.W)
    val imm = UInt(MDP_IMM_BITS.W)
    val prefetchVaddr = UInt(VAddrBits.W)
  }

  class MdpL1PrefetchDBEntry extends XSBundle {
    val timeCnt = UInt(64.W)
    val vaddr = UInt(VAddrBits.W)
    val paddr = UInt(PAddrBits.W)
  }

  val io = IO(new Bundle {
    // train0: awakened load dependencies from all LDUs after MdpTrainFilter.
    val train0 = Flipped(DecoupledIO(new MdpTrainReqBundle))
    // train1: ordinary completed-load training copied from the existing S3
    // prefetch training path; it updates BST only after an MDT hit.
    val train1 = Flipped(DecoupledIO(new TrainReqBundle))
    // trigger/l1PfHint stay one-to-one with LDUs and are never serialized.
    val trigger = Flipped(Vec(backendParams.LduCnt, ValidIO(new MdpTriggerReqBundle)))
    val l1PfHint = Vec(backendParams.LduCnt, ValidIO(new MdpL1PfHintBundle))
    // pending comes from the DCache MissQueue after a hinted load refills.
    val pending = Flipped(ValidIO(new MdpPendingReqBundle))
    // Translation/protection go to MemBlock's dedicated MDP DTLB/PMP port;
    // accepted L1 requests go to PrefetcherWrapper's common L1 arbiter.
    val tlbReq = new TlbRequestIO(nRespDups = 2)
    val pmpResp = Flipped(new PMPRespBundle)
    val l1PrefetchReq = DecoupledIO(new L1PrefetchReq)
  })

  val mdt = RegInit(VecInit(Seq.fill(MDP_MDT_ENTRIES)(0.U.asTypeOf(new MdtEntry))))
  val mdtPlru = new ValidPseudoLRU(MDP_MDT_ENTRIES)
  val bst = RegInit(VecInit(Seq.fill(MDP_BST_ENTRIES)(0.U.asTypeOf(new BstEntry))))
  val bstPlru = new ValidPseudoLRU(MDP_BST_ENTRIES)

  // Main training uses the producer load PC as the MDT key.  Replacement first
  // selects invalid entries, otherwise the minimum-confidence PLRU candidate.
  io.train0.ready := true.B
  val train0Hash = pc_hash_tag(io.train0.bits.wakedupPC)
  val train0MatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === train0Hash))
  val train0Hit = train0MatchVec.asUInt.orR
  val mdtMinConf = mdt.map(_.conf).reduce((a, b) => Mux(a < b, a, b))
  val mdtInvalidVec = VecInit(mdt.map(!_.valid))
  val mdtLowConfVec = VecInit(mdt.map(e => e.valid && e.conf === mdtMinConf))
  val mdtReplaceCandidates = Mux(mdtInvalidVec.asUInt.orR, mdtInvalidVec, mdtLowConfVec)
  val mdtReplaceIdx = mdtPlru.way(mdtReplaceCandidates.reverse)._2
  val train0Idx = Wire(UInt(log2Up(MDP_MDT_ENTRIES).W))
  train0Idx := Mux(train0Hit, OHToUInt(train0MatchVec), mdtReplaceIdx)

  when(io.train0.fire && io.train0.bits.wakedup) {
    val entry = mdt(train0Idx)
    val oldImm = entry.imm.asSInt.pad(MDP_IMM_BITS + 1)
    val newImm = io.train0.bits.imm.asSInt.pad(MDP_IMM_BITS + 1)
    val immDiff = Mux(oldImm >= newImm, oldImm - newImm, newImm - oldImm)
    val immClose = immDiff <= 1.S
    val immCntDec = Mux(entry.immCnt === 0.U, 0.U, entry.immCnt - 1.U)

    when(train0Hit) {
      entry.conf := Mux(entry.conf === mdpCounterMax.U, entry.conf, entry.conf + 1.U)
      when(immClose) {
        entry.imm := Mux(oldImm <= newImm, entry.imm, io.train0.bits.imm)
        entry.immCnt := Mux(entry.immCnt === mdpCounterMax.U, entry.immCnt, entry.immCnt + 1.U)
      }.otherwise {
        entry.immCnt := immCntDec
        when(immCntDec < MDP_IMM_CNT_THRESHOLD.U) {
          entry.imm := io.train0.bits.imm
          entry.immCnt := MDP_IMM_CNT_INIT.U
        }
      }
    }.otherwise {
      entry.hashPC := train0Hash
      entry.imm := io.train0.bits.imm
      entry.immCnt := MDP_IMM_CNT_INIT.U
      entry.conf := MDP_CONF_INIT.U
      entry.valid := true.B
    }
    mdtPlru.access(train0Idx)
  }

  // Trigger lookup is combinational and parallel for all LDUs so the hint can
  // return to the same load request before its DCache miss is allocated.
  for (i <- 0 until backendParams.LduCnt) {
    val triggerHash = pc_hash_tag(io.trigger(i).bits.pc)
    val triggerMatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === triggerHash))
    val triggerHit = triggerMatchVec.asUInt.orR
    val triggerEntry = Mux1H(triggerMatchVec, mdt)
    val triggerConfident = triggerHit &&
      triggerEntry.conf >= MDP_CONF_THRESHOLD.U &&
      triggerEntry.immCnt >= MDP_IMM_CNT_THRESHOLD.U

    // TODO: Pipeline the generated pfHintMDP before returning it to the LDU.
    io.l1PfHint(i).valid := io.trigger(i).valid && triggerConfident
    io.l1PfHint(i).bits.imm := triggerEntry.imm
    io.l1PfHint(i).bits.pc := io.trigger(i).bits.pc
    io.l1PfHint(i).bits.vaddr := io.trigger(i).bits.vaddr
    io.l1PfHint(i).bits.lduId := io.trigger(i).bits.lduId

  }

  // Secondary training learns a stride only for loads whose PC is recognized
  // by MDT.  A stable, confident stride generates the second prefetch source.
  val train1Hash = pc_hash_tag(io.train1.bits.pc)
  val train1MdtMatchVec = VecInit(mdt.map(e => e.valid && e.hashPC === train1Hash))
  val train1MdtHit = train1MdtMatchVec.asUInt.orR
  val bstMatchVec = VecInit(bst.map(e => e.valid && e.hashPC === train1Hash))
  val bstHit = bstMatchVec.asUInt.orR
  val bstInvalidVec = VecInit(bst.map(!_.valid))
  val bstReplaceCandidates = Mux(bstInvalidVec.asUInt.orR, bstInvalidVec, VecInit(Seq.fill(MDP_BST_ENTRIES)(true.B)))
  val bstReplaceIdx = bstPlru.way(bstReplaceCandidates.reverse)._2
  val bstIdx = Wire(UInt(log2Up(MDP_BST_ENTRIES).W))
  bstIdx := Mux(bstHit, OHToUInt(bstMatchVec), bstReplaceIdx)
  val bstEntry = bst(bstIdx)
  val train1DeltaPlus = io.train1.bits.vaddr -& bstEntry.prevVaddr
  val train1Decr = train1DeltaPlus(VAddrBits)
  val train1Delta = Mux(train1Decr, bstEntry.prevVaddr - io.train1.bits.vaddr, train1DeltaPlus(VAddrBits - 1, 0))
  val train1StrideValid = train1Delta =/= 0.U
  val train1StrideMatch = train1Delta === bstEntry.stride && train1Decr === bstEntry.decr
  val train1CanPrefetch = io.train1.fire && train1MdtHit && bstHit && train1StrideValid &&
    train1StrideMatch && bstEntry.conf >= MDP_BST_CONF_THRESHOLD.U
  val train1PrefetchVaddr = Mux(
    bstEntry.decr,
    io.train1.bits.vaddr - bstEntry.stride,
    io.train1.bits.vaddr + bstEntry.stride
  )(VAddrBits - 1, 0)

  io.train1.ready := true.B
  when(io.train1.fire && train1MdtHit) {
    when(bstHit) {
      when(train1StrideValid) {
        when(train1StrideMatch) {
          bstEntry.conf := Mux(bstEntry.conf === mdpCounterMax.U, bstEntry.conf, bstEntry.conf + 1.U)
        }.otherwise {
          bstEntry.conf := Mux(bstEntry.conf === 0.U, 0.U, bstEntry.conf - 1.U)
          when(bstEntry.conf <= 1.U) {
            bstEntry.stride := train1Delta
            bstEntry.decr := train1Decr
          }
        }
      }
      bstEntry.prevVaddr := io.train1.bits.vaddr
    }.otherwise {
      bstEntry.hashPC := train1Hash
      bstEntry.prevVaddr := io.train1.bits.vaddr
      bstEntry.stride := 0.U
      bstEntry.decr := false.B
      bstEntry.conf := 0.U
      bstEntry.valid := true.B
    }
    bstPlru.access(bstIdx)
  }

  // The primary MDP prefetch address follows load address-generation semantics:
  // refill load data is the base and the learned 12-bit immediate is signed.
  val pendingVaddr = (io.pending.bits.data + SignExt(io.pending.bits.imm, XLEN))(VAddrBits - 1, 0)
  val pendingPfReq = Wire(Valid(new SourcePrefetchReq))
  pendingPfReq.valid := io.pending.valid
  pendingPfReq.bits.triggerPC := io.pending.bits.pc
  pendingPfReq.bits.triggerVA := io.pending.bits.vaddr
  pendingPfReq.bits.prefetchVA := pendingVaddr
  pendingPfReq.bits.prefetchTarget := PrefetchTarget.L1.id.U

  val stridePfReq = Wire(Valid(new SourcePrefetchReq))
  stridePfReq.valid := train1CanPrefetch
  stridePfReq.bits.triggerPC := io.train1.bits.pc
  stridePfReq.bits.triggerVA := io.train1.bits.vaddr
  stridePfReq.bits.prefetchVA := train1PrefetchVaddr
  stridePfReq.bits.prefetchTarget := PrefetchTarget.L1.id.U

  // Pending-data and stride candidates may arrive together, so use two enqueue
  // lanes before the shared translation/filter buffer.
  val sourceQueue = Module(new MultiPortQueue(
    new SourcePrefetchReq,
    enq_lanes = 2,
    deq_lanes = 1,
    lanes = 2,
    rows = 4
  ))
  sourceQueue.io.enq(0).valid := pendingPfReq.valid
  sourceQueue.io.enq(0).bits := pendingPfReq.bits
  sourceQueue.io.enq(1).valid := stridePfReq.valid
  sourceQueue.io.enq(1).bits := stridePfReq.bits

  val pfBuffer = Module(new MdpPrefetchBuffer(8))
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
  val pendingTable = ChiselDB.createTable(s"mdpPending_hart$hartId", new MdpPendingDBEntry, basicDB = true)
  val l1PrefetchTable = ChiselDB.createTable(s"mdpL1Prefetch_hart$hartId", new MdpL1PrefetchDBEntry, basicDB = true)

  val train0Log = Wire(new MdpTrain0DBEntry)
  train0Log.timeCnt := GTimer()
  train0Log.wakedupPC := io.train0.bits.wakedupPC
  train0Log.pc := io.train0.bits.pc
  train0Log.imm := io.train0.bits.imm
  train0Log.hit := train0Hit
  train0Log.mdtIdx := train0Idx
  train0Log.oldImm := mdt(train0Idx).imm
  train0Log.oldImmCnt := mdt(train0Idx).immCnt
  train0Log.oldConf := mdt(train0Idx).conf
  train0Table.log(train0Log, io.train0.fire, "mdp", clock, reset)

  for (i <- 0 until backendParams.LduCnt) {
    val hintLog = Wire(new MdpHintDBEntry)
    hintLog.timeCnt := GTimer()
    hintLog.lduId := io.l1PfHint(i).bits.lduId
    hintLog.pc := io.l1PfHint(i).bits.pc
    hintLog.vaddr := io.l1PfHint(i).bits.vaddr
    hintLog.imm := io.l1PfHint(i).bits.imm
    hintTable.log(hintLog, io.l1PfHint(i).valid, s"ldu$i", clock, reset)
  }

  val pendingLog = Wire(new MdpPendingDBEntry)
  pendingLog.timeCnt := GTimer()
  pendingLog.mshrId := io.pending.bits.mshrId
  pendingLog.pc := io.pending.bits.pc
  pendingLog.vaddr := io.pending.bits.vaddr
  pendingLog.data := io.pending.bits.data
  pendingLog.imm := io.pending.bits.imm
  pendingLog.prefetchVaddr := pendingVaddr
  pendingTable.log(pendingLog, io.pending.valid, "mdp", clock, reset)

  val l1PrefetchLog = Wire(new MdpL1PrefetchDBEntry)
  l1PrefetchLog.timeCnt := GTimer()
  l1PrefetchLog.vaddr := io.l1PrefetchReq.bits.vaddr
  l1PrefetchLog.paddr := io.l1PrefetchReq.bits.paddr
  l1PrefetchTable.log(l1PrefetchLog, io.l1PrefetchReq.fire, "mdp", clock, reset)

  // Assertions and performance counters are kept at the end of the class.
  assert(PopCount(train0MatchVec) <= 1.U)
  assert(PopCount(train1MdtMatchVec) <= 1.U)
  assert(PopCount(bstMatchVec) <= 1.U)

  XSPerfAccumulate("mdp_train0", io.train0.fire)
  XSPerfAccumulate("mdp_train0_hit", io.train0.fire && train0Hit)
  XSPerfAccumulate("mdp_train0_alloc", io.train0.fire && !train0Hit)
  XSPerfAccumulate("mdp_train1", io.train1.fire)
  XSPerfAccumulate("mdp_train1_mdt_hit", io.train1.fire && train1MdtHit)
  XSPerfAccumulate("mdp_train1_bst_hit", io.train1.fire && train1MdtHit && bstHit)
  XSPerfAccumulate("mdp_pending", io.pending.valid)
  XSPerfAccumulate("mdp_pending_queue_drop", pendingPfReq.valid && !sourceQueue.io.enq(0).ready)
  XSPerfAccumulate("mdp_stride_queue_drop", stridePfReq.valid && !sourceQueue.io.enq(1).ready)
  for (i <- 0 until backendParams.LduCnt) {
    XSPerfAccumulate(s"mdp_trigger_$i", io.trigger(i).valid)
    XSPerfAccumulate(s"mdp_hint_$i", io.l1PfHint(i).valid)
  }
}
