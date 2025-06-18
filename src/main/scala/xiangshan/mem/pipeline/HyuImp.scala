/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.datapath.{NewPipelineConnect, NewPipelineConnectPipe}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.FuConfig.{LduCfg, VlduCfg, StaCfg}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR.{MemTrigger, TriggerUtil}
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.mem.Bundles._
import xiangshan.mem.BitUtils._
import xiangshan.cache.{LoadForwardMissQueueReqBundle, LoadForwardMissQueueRespBundle, DcacheToLduForwardIO}
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO}
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.{TlbCmd, TlbRequestIO, TlbReq, TlbResp, TlbHintReq}


class HyuIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO with HasMemBlockParameters {
  // from
  val fromBackend = new Bundle () {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
  }
  val fromVex = new Bundle() {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
  }
  val fromMisalign = new Bundle() {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
    val misalignAllowSpec = Input(Bool())
  }
  val fromLdu = new Bundle() {
    val replay = Flipped(DecoupledIO(new LsPipelineBundle))
  }
  val fromPrefetch = new Bundle() {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
  }
  val fromTlb = new Bundle() {
    val resp = Flipped(DecoupledIO(new TlbResp(2)))
    val hint = Flipped(new TlbHintReq)
  }
  val fromPmp = new Bundle() {
    val resp = Flipped(new PMPRespBundle())
  }
  val fromDCache = new DCacheLoadRespIO
  val fromSta = new Bundle() {
    val nukeQuery = Vec(StorePipelineWidth, Flipped(ValidIO(new StoreNukeQueryBundle)))
  }
  val fromLsq = new Bundle() {
    val forward = Input(new LoadForwardRespBundle)
    val rarQuery = Flipped(ValidIO(new LoadNukeQueryRespBundle))
    val replay = Flipped(DecoupledIO(new LsPipelineBundle))
    val nc = Flipped(DecoupledIO(new LsPipelineBundle))
    val mmio = Flipped(DecoupledIO(new LsPipelineBundle))
    val lqDeqPtr = Input(new LqPtr)
  }
  val fromSbuffer = new Bundle() {
    val forward = Input(new LoadForwardRespBundle)
  }
  val fromUbuffer = new Bundle() {
    val forward = Input(new LoadForwardRespBundle)
  }
  val fromMissQueue = new Bundle() {
    val forward = Input(new LoadForwardMissQueueRespBundle)
  }
  val fromBus = Input(new DcacheToLduForwardIO)

  // to
  val toBackend = new Bundle() {
    val stIssued = Valid(new MemExuInput)
    val wakeup = ValidIO(new DynInst)
    val ldCancel = Output(new LoadCancelIO)
    val iqFeedback = new MemRSFeedbackIO
    val rollback = ValidIO(new Redirect)
    val lduWriteback = DecoupledIO(new LsPipelineBundle)
    val staWriteback = DecoupledIO(new LsPipelineBundle)
  }
  val toVex = new Bundle() {
    val lduWriteback = DecoupledIO(new VecPipelineFeedbackIO())
    val staWriteback = DecoupledIO(new VecPipelineFeedbackIO(isVStore = true))
  }
  val toTlb = new Bundle() {
    val req = DecoupledIO(new TlbReq)
    val req_kill = Output(Bool())
  }
  val toDCache = new DCacheLoadReqIO
  val toLoadMisalign = new MisalignBufferIO
  val toStoreMisalign = new MisalignBufferIO
  val toLsq = new Bundle() {
    val forward = ValidIO(new LoadForwardReqBundle)
    val rarQuery = new LoadNukeQueryReqIO
    val rawQuery = new LoadNukeQueryReqIO
    val nukeQuery = ValidIO(new StoreNukeQueryBundle)
    val addrUpdate = ValidIO(new LsPipelineBundle)
    val excpUpdate = ValidIO(new LsPipelineBundle)
    val maskOut = ValidIO(new StoreMaskBundle)
    val writeback = DecoupledIO(new LsPipelineBundle)
  }
  val toPrefetch = new Bundle() {
    val ifetch = ValidIO(new SoftIfetchPrefetchBundle)
    val train = new LsPrefetchTrainIO
    val trainL1 = new LsPrefetchTrainIO
  }
  val toLdu = new Bundle() {
    val replay = DecoupledIO(new LsPipelineBundle)
    val nukeQuery = ValidIO(new StoreNukeQueryBundle)
  }
  val toSbuffer = new Bundle() {
    val forward = ValidIO(new LoadForwardReqBundle)
  }
  val toUbuffer = new Bundle() {
    val forward = ValidIO(new LoadForwardReqBundle)
  }
  val toMissQueue = new Bundle() {
    val forward = ValidIO(new LoadForwardMissQueueReqBundle)
  }

  //
  val s0_s1_valid = Output(Bool())

  // perf
  val debugLsInfo = Output(new DebugLsInfoBundle)
  val lsTopdownInfo = Output(new LsTopdownInfo)
}

class HyuImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams) extends MemUnitImp(wrapper) {

  private val SelectGroupSize = RollbackGroupSize
  private val lgSelectGroupSize = log2Ceil(SelectGroupSize)
  private val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1
  private val TotalDelayCycles = TotalSelectCycles - 2
  private val LdDataDup = 3
  require(LdDataDup >= 2)

  def nukeDetect(queries: Seq[Valid[StoreNukeQueryBundle]], req: LsPipelineBundle): Bool = {
    /**
      * Nuke query detects potential memory consistency violations where a load
      * may be load from memory while a previous store to the same address is pending.
      * If the store unit performs a 128 bits access, and the load is vectorized or
      * misaligned with 16-byte boundaries, it needs to perform 128 bits matching.
      */
    val match128Bits = queries.map { case query =>
      query.bits.matchLine || (req.isVector || req.misalignWith16Byte) && req.is128Bits
    }
    val nukePAddrMatch = queries.zip(match128Bits).map { case (query, match128Bits) =>
      Mux(
        match128Bits,
        req.paddr(PAddrBits-1, 4) === query.bits.paddr(PAddrBits-1, 4),
        req.paddr(PAddrBits-1, 3) === query.bits.paddr(PAddrBits-1, 3)
      )
    }
    val nukeMaskMatch = queries.map { case query =>
      (req.mask & query.bits.mask).orR
    }
    val nukeRobOrder = queries.map { case query =>
      isAfter(req.uop.robIdx, query.bits.uop.robIdx)
    }
    queries.zip(nukePAddrMatch).zip(nukeMaskMatch).zip(nukeRobOrder).map {
      case (((query, paddrMatch), maskMatch), robOrder) =>
        query.valid && paddrMatch && maskMatch && robOrder
    }.reduce(_|_) && !req.tlbMiss
  }

  /**
    * --------------------------------------------------------------------
    * IOs
    * --------------------------------------------------------------------
    */
  io.suggestName("none")
  override lazy val io = IO(new HyuIO).suggestName("io")

  /**
    * --------------------------------------------------------------------
    * Performance Kits
    * --------------------------------------------------------------------
    */
  PerfCCT.updateInstPos(
    io.fromBackend.issue.bits.uop.debug_seqNum,
    PerfCCT.InstPos.AtFU.id.U,
    io.fromBackend.issue.valid,
    clock,
    reset
  )

  /**
    * --------------------------------------------------------------------
    * Submodules
    * --------------------------------------------------------------------
    */
  val trigger = Module(new MemTrigger(params.triggerType))

  /**
    * --------------------------------------------------------------------
    * Pipeline s0
    * --------------------------------------------------------------------
    * Virtual address generation
    */
  val s0_kill = WireInit(false.B)
  val s0_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))

  /**
    * --------------------------------------------------------------------
    * HyuImp requests priority
    * --------------------------------------------------------------------
    *
    * ------------------------------------------------------
    *      Source                                Priority
    * ------------------------------------------------------
    *   MisalignBuffer                           Highest
    *   Lsq Replay (dcache miss)
    *   Ldu fast replay
    *   MMIO wakeup
    *   Noncacheable
    *   Lsq Replay (other schemes)
    *   High confidence hardware prefetch
    *   Vector from VecExuBlock
    *   Scalar from Backend
    *   Low confidence hardware prefetch         Lowest
    * ------------------------------------------------------
    */
  val s0_srcPriorityMapping = Seq(
    io.fromMisalign.issue.valid -> io.fromMisalign.issue.bits,
    (io.fromLsq.replay.valid && io.fromLsq.replay.bits.forwardTLDchannel) -> io.fromLsq.replay.bits,
    io.fromLdu.replay.valid -> io.fromLdu.replay.bits,
    io.fromLsq.mmio.valid -> io.fromLsq.mmio.bits,
    io.fromLsq.nc.valid -> io.fromLsq.nc.bits,
    (io.fromLsq.replay.valid && !io.fromLsq.replay.bits.forwardTLDchannel) -> io.fromLsq.replay.bits,
    (io.fromPrefetch.issue.valid && io.fromPrefetch.issue.bits.confidence > 0.U) -> io.fromPrefetch.issue.bits,
    io.fromVex.issue.valid -> io.fromVex.issue.bits,
    io.fromBackend.issue.valid -> io.fromBackend.issue.bits,
    (io.fromPrefetch.issue.valid && io.fromPrefetch.issue.bits.confidence === 0.U) -> io.fromPrefetch.issue.bits,
  )
  val Seq(
    mabIdx, superReplayIdx, fastReplayIdx, mmioIdx, ncIdx, replayIdx,
    highPfIdx, vexIdx, scalarIdx, lowPfIdx
  ) = (0 until s0_srcPriorityMapping.length)

  val s0_srcValidVec = s0_srcPriorityMapping.map(_._1)

  // The first element is always true, as it is used to indicate the first source
  val s0_srcReadyVec = true.B +: (0 until s0_srcValidVec.length - 1).map(i => !s0_srcValidVec.take(i+1).reduce(_ || _))
  val s0_srcFiredVec = s0_srcValidVec.zip(s0_srcReadyVec).map { case (vld, rdy) => vld && rdy }
  val s0_srcSelect = ParallelPriorityMux(s0_srcPriorityMapping)

  /**
    * --------------------------------------------------------------------
    * LSQ Replay Ready Special Handling
    * --------------------------------------------------------------------
    * 1. Cache-missed LSQ replay always has the highest priority and is always accepted
    *   if the pipeline and DCache are ready.
    * 2. Other LSQ replay schemes are accepted only if not stalled by younger scalar/vector requests.
    *
    * The stall logic ensures that a replay request will not proceed if there is a younger
    * scalar or vector request in the pipeline, maintaining correct ordering.
    */
  val s0_scalarOlder = io.fromBackend.issue.valid &&
    isAfter(io.fromLsq.replay.bits.uop.robIdx, io.fromBackend.issue.bits.uop.robIdx)
  val s0_vexOrder = io.fromVex.issue.valid &&
    isAfter(io.fromLsq.replay.bits.uop.robIdx, io.fromVex.issue.bits.uop.robIdx)
  val s0_lsqReplayStall = s0_scalarOlder || s0_vexOrder
  io.fromLsq.replay.ready := s0_out.ready && io.toDCache.req.ready &&
    (s0_srcReadyVec(superReplayIdx) || s0_srcReadyVec(replayIdx) && !s0_lsqReplayStall)

  /**
    * Misalign/FastReplay/Vex/Scalar request need query dcache
    */
  io.fromMisalign.issue.ready := s0_out.ready && io.toDCache.req.ready && s0_srcReadyVec(mabIdx)
  io.fromLdu.replay.ready := s0_out.ready && io.toDCache.req.ready && s0_srcReadyVec(fastReplayIdx)
  io.fromVex.issue.ready := s0_out.ready && io.toDCache.req.ready && s0_srcReadyVec(vexIdx)
  io.fromBackend.issue.ready := s0_out.ready && io.toDCache.req.ready && s0_srcReadyVec(scalarIdx)

  /**
    * Mmio/Noncacheable request no need query dcache
    */
  io.fromLsq.mmio.ready := s0_out.ready && s0_srcReadyVec(mmioIdx)
  io.fromLsq.nc.ready := s0_out.ready && s0_srcReadyVec(ncIdx)

  /**
    * Prefetch request always can be accepts
    */
  io.fromPrefetch.issue.ready := true.B

  /**
    * --------------------------------------------------------------------
    * Load Wakeup Logic
    * --------------------------------------------------------------------
    * This logic determines which uop should be used for issuing a wakeup signal
    * to the scheduler (IQ) after a load completes or is replayed.
    *
    * Wakeup sources (priority order):
    *   1. Misaligned load buffer (misalign wakeup)
    *   2. LSQ super replay (cache miss replay)
    *   3. Fast replay
    *   4. MMIO load
    *   5. Non-cacheable load
    *   6. LSQ normal replay
    *   7. Scalar issue (normal load from IQ)
    */
  val s0_wakeupPriorityMapping = Seq(
    (s0_srcValidVec(mabIdx) && io.fromMisalign.issue.bits.misalignNeedWakeUp) -> io.fromMisalign.issue.bits,
    s0_srcValidVec(superReplayIdx) -> io.fromLsq.replay.bits,
    s0_srcValidVec(fastReplayIdx) -> io.fromLdu.replay.bits,
    s0_srcValidVec(mmioIdx) -> io.fromLsq.mmio.bits,
    s0_srcValidVec(ncIdx) -> io.fromLsq.nc.bits,
    s0_srcValidVec(replayIdx) -> io.fromLsq.replay.bits,
    s0_srcValidVec(scalarIdx) -> io.fromBackend.issue.bits,
  )
  val s0_canWakeup = s0_wakeupPriorityMapping.map(_._1).reduce(_|_) && s0_out.fire
  val s0_wakeupUop = ParallelPriorityMux(s0_wakeupPriorityMapping)
  io.toBackend.wakeup.valid := s0_canWakeup && !s0_wakeupUop.isVector && s0_wakeupUop.isLoad
  io.toBackend.wakeup.bits := s0_wakeupUop.uop

  /**
    * --------------------------------------------------------------------
    * Virtual Address Selection for TLB and DCache
    * --------------------------------------------------------------------
    * This section selects the appropriate virtual address for TLB and DCache
    * requests based on the current source priority. The selection is performed
    * using ParallelPriorityMux, which chooses the first valid source in the
    * priority order.
    */
  val s0_scalarVAddr = io.fromBackend.issue.bits.src(0) + SignExt(io.fromBackend.issue.bits.uop.imm(11, 0), VAddrBits)
  val s0_vaddrUsedTlb = ParallelPriorityMux(Seq(
    s0_srcValidVec(mabIdx) -> io.fromMisalign.issue.bits.vaddr,
    (s0_srcValidVec(superReplayIdx) || s0_srcValidVec(replayIdx)) -> io.fromLsq.replay.bits.vaddr,
    s0_srcValidVec(vexIdx) -> io.fromVex.issue.bits.vaddr,
    true.B -> s0_scalarVAddr
  ))

  val s0_maskUsedTlb = ParallelPriorityMux(Seq(
    s0_srcValidVec(mabIdx) -> io.fromMisalign.issue.bits.mask,
    (s0_srcValidVec(superReplayIdx) || s0_srcValidVec(replayIdx)) -> io.fromLsq.replay.bits.mask,
    s0_srcValidVec(vexIdx) -> io.fromVex.issue.bits.mask,
    true.B -> genVWmask(s0_scalarVAddr, io.fromBackend.issue.bits.uop.fuOpType(1, 0))
  ))

  val s0_vaddrUsedCache = ParallelPriorityMux(Seq(
    s0_srcValidVec(fastReplayIdx) -> io.fromLdu.replay.bits.vaddr,
    s0_srcValidVec(highPfIdx) -> io.fromPrefetch.issue.bits.vaddr,
    s0_srcValidVec(ncIdx) -> io.fromLsq.nc.bits.vaddr,
    true.B -> s0_vaddrUsedTlb
  ))

  val s0_scalarFullVAddr = io.fromBackend.issue.bits.src(0) + SignExt(io.fromBackend.issue.bits.uop.imm(11, 0), XLEN)
  val s0_fullVAddrUsedTlb = ParallelPriorityMux(Seq(
    s0_srcValidVec(mabIdx) -> io.fromMisalign.issue.bits.fullva,
    s0_srcValidVec(vexIdx) -> io.fromVex.issue.bits.vaddr,
    s0_srcValidVec(scalarIdx) -> s0_scalarFullVAddr,
    true.B -> s0_vaddrUsedCache
  ))

  val s0_maskUsedCache = ParallelPriorityMux(Seq(
    s0_srcValidVec(fastReplayIdx) -> io.fromLdu.replay.bits.mask,
    s0_srcValidVec(highPfIdx) -> 0.U,
    s0_srcValidVec(ncIdx) -> io.fromLsq.nc.bits.mask,
    true.B -> s0_maskUsedTlb
  ))

  val s0_paddrTranslated = ParallelPriorityMux(Seq(
    s0_srcValidVec(fastReplayIdx) -> io.fromLdu.replay.bits.paddr,
    s0_srcValidVec(ncIdx) -> io.fromLsq.nc.bits.paddr,
    s0_srcValidVec(highPfIdx) -> io.fromPrefetch.issue.bits.paddr,
    (s0_srcValidVec(scalarIdx) && s0_srcSelect.isPrefetchI) -> 0.U,
    s0_srcValidVec(lowPfIdx) -> io.fromPrefetch.issue.bits.paddr
  ))

  val s0_checkfullva = s0_srcFiredVec(vexIdx) || s0_srcFiredVec(scalarIdx)

  /**
    * --------------------------------------------------------------------
    * Memory alignment checking logic
    * --------------------------------------------------------------------
    * 1. Basic alignment based on access size
    * 2. Detection of 16-byte boundary crossing
    * 3. Special handling of misaligned access within 16 bytes
    */
  val s0_isCbo = s0_srcFiredVec(scalarIdx) && LSUOpType.isCboAll(io.fromBackend.issue.bits.uop.fuOpType)
  val s0_alignType = Mux(
    s0_srcSelect.isVector,
    s0_srcSelect.alignType(1, 0),
    LSUOpType.size(s0_srcSelect.uop.fuOpType(1, 0))
  )
  val s0_addrAlign = LookupTree(s0_alignType, List(
    "b00".U -> true.B,// 1 byte align
    "b01".U -> (s0_vaddrUsedCache(0) === 0.U),  // 2 byte align
    "b10".U -> (s0_vaddrUsedCache(1, 0) === 0.U), // 4 byte align
    "b11".U -> (s0_vaddrUsedCache(2, 0) === 0.U),  // 8 byte align
  ))
  val s0_addrAlignOrCbo = s0_addrAlign || s0_isCbo

  XSError(
    s0_srcSelect.isVector && s0_vaddrUsedCache(3, 0) =/= 0.U && s0_srcSelect.alignType(2),
    "unit-stride 128 bit element is not aligned!"
  )

  /**
    * --------------------------------------------------------------------
    * Misalignment and 16-byte boundary crossing detection
    * --------------------------------------------------------------------
    * Check if memory access crosses 16-byte boundary or has
    * special alignment requirements within a 16-byte block.
    *
    * 1) cross 16-byte boundary
    *            upper                 lower
    *              |                     |
    *  31          |         16 15       |             0
    * +------------v----------+ +--------v--------------+
    * |                       | |                       |
    * +-----------------------+ +-----------------------+
    *
    * 2) within 16-byte boundary
    *                                  upper  lower
    *                                    |      |
    *  31                    16 15       |      |       0
    * +-----------------------+ +--------v------v-------+
    * |                       | |                       |
    * +-----------------------+ +-----------------------+
    */
  val s0_checkVAddrLower = s0_vaddrUsedCache(4, 0)
  val s0_checkVAddrUpper = LookupTree(s0_alignType, List(
    "b00".U -> 0.U,
    "b01".U -> 1.U,
    "b10".U -> 3.U,
    "b11".U -> 7.U
  )) + s0_checkVAddrLower
  // Check if access crosses 16-byte boundary by comparing bit[4]
  val s0_cross16Bytes = s0_checkVAddrUpper(4) =/= s0_checkVAddrLower(4)
  // Special case: misaligned but within 16-byte block
  val s0_misalignWith16Byte = !s0_cross16Bytes && !s0_addrAlignOrCbo
  val s0_isFinalSplit = s0_srcSelect.isFinalSplit
  val s0_misalignNeedWakeUp = s0_srcSelect.misalignNeedWakeUp
  val s0_misalign = !s0_addrAlignOrCbo ||
    s0_srcSelect.uop.exceptionVec(loadAddrMisaligned) ||
    s0_srcSelect.uop.exceptionVec(storeAddrMisaligned)

  /**
    * --------------------------------------------------------------------
    * 128-bit access handling:
    * --------------------------------------------------------------------
    * 1. Either originally 128-bit access
    * 2. misaligned access within 16-byte block(read two banks (8bytes/bank))
    */
  val s0_is128Bits = s0_srcSelect.is128Bits || s0_misalignWith16Byte

  /**
    * --------------------------------------------------------------------
    * TLB Request Address Selection and Attributes
    * --------------------------------------------------------------------
    * This section selects the appropriate virtual address (fullva) and
    * TLB attributes (hlv, hlvx) for the TLB request, based on the current
    * selected source. The selection is priority-based and covers all
    * possible request sources.
    */
  val s0_hyperUsedTlb = ParallelPriorityMux(Seq(
    s0_srcValidVec(mabIdx) -> Mux(
      io.fromMisalign.issue.bits.isLoad,
      LSUOpType.isHlv(io.fromMisalign.issue.bits.uop.fuOpType),
      LSUOpType.isHsv(io.fromMisalign.issue.bits.uop.fuOpType)
    ),
    (s0_srcValidVec(superReplayIdx) || s0_srcValidVec(replayIdx)) -> LSUOpType.isHlv(io.fromLsq.replay.bits.uop.fuOpType),
    s0_srcValidVec(scalarIdx) -> Mux(
      io.fromBackend.issue.bits.isLoad,
      LSUOpType.isHlv(io.fromBackend.issue.bits.uop.fuOpType),
      LSUOpType.isHsv(io.fromBackend.issue.bits.uop.fuOpType)
    )
  ))

  val s0_hyperxUsedTlb = ParallelPriorityMux(Seq(
    s0_srcValidVec(mabIdx) -> Mux(
      io.fromMisalign.issue.bits.isLoad,
      LSUOpType.isHlvx(io.fromMisalign.issue.bits.uop.fuOpType),
      false.B // Hsvx always be false
    ),
    (s0_srcValidVec(superReplayIdx) || s0_srcValidVec(replayIdx)) -> LSUOpType.isHlvx(io.fromLsq.replay.bits.uop.fuOpType),
    s0_srcValidVec(scalarIdx) -> Mux(
      io.fromBackend.issue.bits.isLoad,
      LSUOpType.isHlvx(io.fromBackend.issue.bits.uop.fuOpType),
      false.B // Hsvx always be false
    )
  ))

  /**
    * -----------------------------------------------------------------------------
    * TLB Query Control Logic
    * -----------------------------------------------------------------------------
    * Determines whether a TLB query is needed for the current request.
    * 1. Hardware prefetches and instruction prefetches do not require TLB queries.
    * 2. Fast replay and non-cacheable requests have already queried the TLB.
    * 3. MMIO and misaligned buffer requests only need a wakeup, not a TLB query.
    */
  val s0_hwPfFired = s0_srcFiredVec(highPfIdx) || s0_srcFiredVec(lowPfIdx)
  val s0_pfIFired = s0_srcSelect.isPrefetchI
  val s0_tlbAlreadyQueried = s0_srcFiredVec(fastReplayIdx) || s0_srcFiredVec(ncIdx)
  val s0_justWakeup = s0_srcFiredVec(mmioIdx) || s0_srcFiredVec(mabIdx) && s0_misalignNeedWakeUp
  val s0_tlbNoQuery = s0_hwPfFired || s0_pfIFired || s0_tlbAlreadyQueried || s0_justWakeup

  val s0_sizeUsedTlb = Mux(s0_srcSelect.isVector, s0_srcSelect.alignType(2, 0), LSUOpType.size(s0_srcSelect.uop.fuOpType))

  io.toTlb.req.valid := s0_out.valid
  io.toTlb.req.bits.cmd := Mux(
    s0_srcSelect.isPrefetch && s0_srcSelect.isPrefetchWrite || s0_srcSelect.isStore,
    TlbCmd.write,
    TlbCmd.read
  )
  io.toTlb.req.bits.vaddr := s0_vaddrUsedTlb
  io.toTlb.req.bits.fullva := s0_fullVAddrUsedTlb
  io.toTlb.req.bits.checkfullva := s0_checkfullva
  io.toTlb.req.bits.isPrefetch := s0_srcSelect.isPrefetch
  io.toTlb.req.bits.hyperinst := s0_hyperUsedTlb
  io.toTlb.req.bits.hlvx := s0_hyperxUsedTlb
  io.toTlb.req.bits.size := s0_sizeUsedTlb
  io.toTlb.req.bits.kill := false.B
  io.toTlb.req.bits.memidx.is_ld := s0_srcSelect.isLoad
  io.toTlb.req.bits.memidx.is_st := s0_srcSelect.isStore
  io.toTlb.req.bits.memidx.idx := Mux(s0_srcSelect.isLoad, s0_srcSelect.uop.lqIdx.value, s0_srcSelect.uop.sqIdx.value)
  io.toTlb.req.bits.no_translate := s0_tlbNoQuery
  io.toTlb.req.bits.debug.robIdx := s0_srcSelect.uop.robIdx
  io.toTlb.req.bits.debug.pc := s0_srcSelect.uop.pc
  io.toTlb.req.bits.debug.isFirstIssue := s0_srcSelect.isFirstIssue

  /**
    * --------------------------------------------------------------------
    * DCache access here: not **real** dcache for write
    * --------------------------------------------------------------------
    * just read meta and tag in dcache, to find out the store will hit or miss
    *
    * NOTE: The store request does not wait for the dcache to be ready.
    *       If the dcache is not ready at this time, the dcache is not queried.
    *       But, store prefetch request will always wait for dcache to be ready to make progress.
    */
  val s0_cmdUsedCache = ParallelPriorityMux(Seq(
    s0_srcSelect.isPrefetchRead -> MemoryOpConstants.M_PFR,
    s0_srcSelect.isPrefetchWrite -> MemoryOpConstants.M_PFW,
    s0_srcSelect.isStore -> MemoryOpConstants.M_XWR,
    true.B -> MemoryOpConstants.M_XRD
  ))

  val s0_instTypeUsedCache = ParallelPriorityMux(Seq(
    s0_srcSelect.isPrefetch -> DCACHE_PREFETCH_SOURCE.U,
    s0_srcSelect.isLoad -> LOAD_SOURCE.U,
    true.B -> STORE_SOURCE.U
  ))

  val s0_pfSource = Mux(s0_hwPfFired, io.fromPrefetch.issue.bits.pfSource.value, L1_HW_PREFETCH_NULL)

  io.toDCache.req.valid := s0_out.valid
  io.toDCache.req.bits.vaddr := s0_vaddrUsedCache
  io.toDCache.req.bits.vaddr_dup := s0_vaddrUsedCache
  io.toDCache.req.bits.mask := s0_maskUsedCache
  io.toDCache.req.bits.cmd := s0_cmdUsedCache
  io.toDCache.req.bits.isFirstIssue := s0_srcSelect.isFirstIssue
  io.toDCache.req.bits.instrtype := s0_instTypeUsedCache
  io.toDCache.req.bits.replayCarry := s0_srcSelect.replayCarry
  io.toDCache.req.bits.lqIdx := s0_srcSelect.uop.lqIdx
  io.toDCache.req.bits.debug_robIdx := s0_srcSelect.uop.robIdx.value
  io.toDCache.req.bits.data := DontCare
  io.toDCache.req.bits.id := DontCare

  io.toDCache.pf_source := s0_pfSource
  io.toDCache.is128Req := s0_is128Bits
  io.toDCache.replacementUpdated := (s0_srcFiredVec(superReplayIdx) || s0_srcFiredVec(replayIdx)) &&
    io.fromLsq.replay.bits.replacementUpdated

  if (env.FPGAPlatform) {
    io.toDCache.s0_pc := DontCare
  } else {
    io.toDCache.s0_pc := s1_out.bits.uop.pc
  }

  // update StoreQueue mask
  io.toLsq.maskOut.valid := s0_srcFiredVec(scalarIdx) && io.fromBackend.issue.bits.isStore ||
    s0_srcFiredVec(vexIdx) && io.fromVex.issue.bits.isStore
  io.toLsq.maskOut.bits.sqIdx := s0_srcSelect.uop.sqIdx
  io.toLsq.maskOut.bits.mask := s0_srcSelect.mask

  // to Prefetch
  val s0_highConfPrefetch = s0_out.bits.isHWPrefetch && s0_out.bits.confidence > 0.U
  io.toPrefetch.train.canAcceptHighConfPrefetch := s0_out.fire && s0_highConfPrefetch && io.toDCache.req.ready
  val s0_lowConfPrefetch = s0_out.bits.isHWPrefetch && s0_out.bits.confidence === 0.U
  io.toPrefetch.train.canAcceptLowConfPrefetch := s0_out.fire && s0_lowConfPrefetch && io.toDCache.req.ready

  io.toPrefetch.trainL1.canAcceptHighConfPrefetch := io.toPrefetch.train.canAcceptHighConfPrefetch
  io.toPrefetch.trainL1.canAcceptLowConfPrefetch := io.toPrefetch.train.canAcceptLowConfPrefetch

  /**
    * -------------------------------------------------------------------
    * Output Valid Control Logic
    * -------------------------------------------------------------------
    *  The output is valid only if a source (except MMIO) fires and no misalign wakeup.
    */
  val s0_anySrcFiredExceptMmio = s0_srcFiredVec.reduce(_|_) && !s0_srcFiredVec(mmioIdx)
  val s0_misalignWakeup = s0_srcFiredVec(mabIdx) && s0_misalignNeedWakeUp

  s0_out.valid := s0_anySrcFiredExceptMmio && !s0_misalignWakeup && io.toDCache.req.ready
  s0_out.bits := s0_srcSelect
  s0_out.bits.vaddr := s0_vaddrUsedCache
  s0_out.bits.fullva := s0_fullVAddrUsedTlb
  s0_out.bits.paddr := s0_paddrTranslated
  s0_out.bits.mask := s0_maskUsedCache
  s0_out.bits.is128Bits := s0_is128Bits
  s0_out.bits.misalign := s0_misalign
  s0_out.bits.misalignWith16Byte := s0_misalignWith16Byte
  s0_out.bits.isFinalSplit := s0_isFinalSplit
  s0_out.bits.uop.exceptionVec(loadAddrMisaligned) := s0_misalign && s0_srcSelect.isLoad && !s0_misalignWith16Byte
  s0_out.bits.uop.exceptionVec(storeAddrMisaligned) := s0_misalign && s0_srcSelect.isStore

  when (io.toTlb.req.valid && s0_srcSelect.isFirstIssue) {
    s0_out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // Logger
  printPipeLine(s0_out.bits, s0_out.valid, "S0")

  XSPerfAccumulate("s0_inValid", s0_out.valid)
  XSPerfAccumulate("s0_inFire", s0_out.fire)
  XSPerfAccumulate("s0_inBlock", s0_out.valid && !s0_out.ready)
  XSPerfAccumulate("s0_vecInFire", s0_out.fire && s0_out.bits.isVector)
  XSPerfAccumulate("s0_vecInBlock", s0_out.valid && !s0_out.ready && s0_out.bits.isVector)
  XSPerfAccumulate("s0_inFireFirstIssue", s0_out.fire && s0_out.bits.isFirstIssue)
  XSPerfAccumulate("s0_lsqReplayFire", s0_out.fire && s0_out.bits.isLoadReplay)
  XSPerfAccumulate("s0_lsqReplayFire", s0_out.fire && s0_out.bits.isLoadReplay && s0_out.bits.isVector)
  XSPerfAccumulate("s0_scalarFire", s0_out.fire)
  XSPerfAccumulate("s0_fastReplayFire", s0_out.fire && s0_out.bits.isFastReplay)
  XSPerfAccumulate("s0_fastReplayVecFire", s0_out.fire && s0_out.bits.isFastReplay && s0_out.bits.isVector)
  XSPerfAccumulate("s0_vecAddrVLenAlign", s0_out.fire && s0_out.bits.isVector && s0_vaddrUsedCache(3, 0) === 0.U)
  XSPerfAccumulate("s0_vecAddrVLenMisalign", s0_out.fire && s0_out.bits.isVector && s0_vaddrUsedCache(3, 0) =/= 0.U)
  XSPerfAccumulate("s0_forwardFromBus", s0_out.fire && s0_out.bits.forwardTLDchannel)

  /**
    * --------------------------------------------------------------------
    * Pipeline s1
    * --------------------------------------------------------------------
    * TLB resp (send paddr to dcache)
    */
  val s1_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s1_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  NewPipelineConnect.connect(s0_out, s1_in, s1_in.fire, s0_kill)

  val s1_ncWithData = s1_in.bits.nc
  val s1_ldCancel = RegEnable(io.fromLdu.replay.bits.ldCancel, io.fromLdu.replay.valid) && s1_in.bits.isFastReplay

  val s1_redirectReg = Wire(Valid(new Redirect))
  s1_redirectReg.valid := GatedValidRegNext(io.fromCtrl.redirect.valid)
  s1_redirectReg.bits := RegEnable(io.fromCtrl.redirect.bits, io.fromCtrl.redirect.valid)

  // Kill the instruction if its ROB index is flushed by either the current or last redirect
  val s1_kill = s1_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect) || s1_in.bits.uop.robIdx.needFlush(s1_redirectReg)

  // FIXME: need it?
  io.fromTlb.resp.ready := true.B

  /**
    * --------------------------------------------------------------------
    * TLB Response Handling and Address Preparation
    * --------------------------------------------------------------------
    * This section handles the TLB response in pipeline stage s1, prepares
    * physical addresses for LSU and DCache, and generates kill signals.
    *
    * The kill signals are propagated to TLB and DCache, and physical addresses
    * are forwarded from the last stage if need.
    */
  val s1_tlbMemIdx = io.fromTlb.resp.bits.memidx
  val s1_tlbMiss = io.fromTlb.resp.valid && io.fromTlb.resp.bits.miss && s1_in.valid
  val s1_tlbFastMiss = io.fromTlb.resp.valid && io.fromTlb.resp.bits.fastMiss && s1_in.valid
  val s1_tlbHit = io.fromTlb.resp.valid && !io.fromTlb.resp.bits.miss && s1_in.valid
  val s1_pbmt = Mux(s1_tlbHit, io.fromTlb.resp.bits.pbmt.head, 0.U(Pbmt.width.W))
  val s1_paddrDupLsu = Mux(s1_in.bits.tlbNoQuery, s1_in.bits.paddr, io.fromTlb.resp.bits.paddr(0))
  val s1_paddrDupDCache = Mux(s1_in.bits.tlbNoQuery, s1_in.bits.paddr, io.fromTlb.resp.bits.paddr(1))
  val s1_gpaddrDupLsu = Mux(s1_in.bits.tlbNoQuery, s1_in.bits.gpaddr, io.fromTlb.resp.bits.gpaddr(0))
  val s1_hasAnyExceptions = ExceptionNO.selectByFu(s1_out.bits.uop.exceptionVec, LduCfg).asUInt.orR

  io.toTlb.req_kill := s1_kill || s1_ldCancel
  io.toTlb.req.bits.pmp_addr := s1_in.bits.paddr

  io.toDCache.s1_paddr_dup_lsu := s1_paddrDupLsu
  io.toDCache.s1_paddr_dup_dcache := s1_paddrDupDCache
  io.toDCache.s1_kill := s1_kill || s1_ldCancel || s1_tlbMiss || s1_hasAnyExceptions
  io.toDCache.s1_kill_data_read := s1_kill || s1_ldCancel || s1_tlbFastMiss

  if (env.FPGAPlatform) {
    io.toDCache.s1_pc := DontCare
  } else {
    io.toDCache.s1_pc := s1_out.bits.uop.pc
  }

  /**
    * --------------------------------------------------------------------
    * Store to load forwarding
    * --------------------------------------------------------------------
    * Store to load forwarding handles forwarding logic from LSU stages to various queues,
    * including StoreQueue, SBuffer, UBuffer, and MissQueue.
    */
  val s1_sqIdxMask = RegEnable(UIntToMask(s0_out.bits.uop.sqIdx.value, StoreQueueSize), s0_out.fire)
  io.toLsq.forward.valid := s1_in.valid
  io.toLsq.forward.bits.vaddr := s1_in.bits.vaddr
  io.toLsq.forward.bits.paddr := s1_paddrDupLsu
  io.toLsq.forward.bits.uop := s1_in.bits.uop
  io.toLsq.forward.bits.sqIdx := s1_in.bits.uop.sqIdx
  io.toLsq.forward.bits.mask := s1_in.bits.mask
  io.toLsq.forward.bits.sqIdxMask := s1_sqIdxMask
  io.toLsq.forward.bits.nc := false.B

  io.toSbuffer.forward := io.toLsq.forward
  io.toUbuffer.forward := io.toLsq.forward

  io.toMissQueue.forward.valid := s1_in.valid && s1_in.bits.forwardTLDchannel
  io.toMissQueue.forward.bits.mshrId := s1_in.bits.mshrId
  io.toMissQueue.forward.bits.paddr := s1_paddrDupDCache

  // feedback fast
  io.toBackend.iqFeedback.feedbackFast.valid := false.B
  io.toBackend.iqFeedback.feedbackFast.bits := DontCare

  // store issue feedback for MDP
  val s1_isVectorStore = !s1_in.bits.isVector && s1_in.bits.isStore && !s1_in.bits.isHWPrefetch
  val s1_notFromMisalign = !s1_in.bits.isMisAlignBuf
  io.toBackend.stIssued.valid := s1_in.valid && !s1_tlbMiss && s1_isVectorStore && s1_notFromMisalign
  io.toBackend.stIssued.bits := s1_in.bits.toMemExuInputBundle()

  // st-ld violation dectect request
  io.toLdu.nukeQuery.valid := s1_in.valid && !s1_tlbMiss && !s1_in.bits.isHWPrefetch
  io.toLdu.nukeQuery.bits.uop := s1_in.bits.uop
  io.toLdu.nukeQuery.bits.paddr := s1_paddrDupDCache
  io.toLdu.nukeQuery.bits.mask := s1_in.bits.mask
  io.toLdu.nukeQuery.bits.matchLine := (s1_in.bits.isVector || s1_in.bits.misalignWith16Byte) && s1_in.bits.is128Bits

  io.toLsq.nukeQuery := io.toLdu.nukeQuery

  // update StoreQueue address
  val s1_alignOrMisalignWith16Byte = !s1_in.bits.misalign|| s1_in.bits.misalignWith16Byte
  val s1_fromMisalignOrFinalSplit = !s1_in.bits.isMisAlignBuf || s1_in.bits.isFinalSplit
  val s1_updateAddrValid = s1_hasAnyExceptions || s1_alignOrMisalignWith16Byte && s1_fromMisalignOrFinalSplit
  io.toLsq.addrUpdate.valid := s1_in.valid && s1_in.bits.isStore
  io.toLsq.addrUpdate.bits := s1_in.bits
  io.toLsq.addrUpdate.bits.tlbMiss := s1_tlbMiss
  io.toLsq.addrUpdate.bits.updateAddrValid := s1_updateAddrValid

  // StoreMisalignBuffer enqueue
  val s1_isCbo = RegEnable(s0_isCbo, false.B, s0_out.fire)
  val s1_isMisalign = s1_in.bits.misalign && !s1_in.bits.misalignWith16Byte && !s1_in.bits.isMisAlignBuf
  val s1_handleMisalign = !s1_in.bits.isHWPrefetch && !s1_isCbo && s1_isMisalign
  val s1_canMisalignEnq = s1_handleMisalign && !s1_tlbMiss && GatedValidRegNext(io.fromCtrl.csrCtrl.hd_misalign_st_enable)
  io.toStoreMisalign.enq.valid := s1_in.valid && s1_in.bits.isStore && s1_canMisalignEnq
  io.toStoreMisalign.enq.bits := s1_in.bits

  /**
    * --------------------------------------------------------------------
    * Trigger Logic (Hardware Breakpoints / Debug Traps)
    * --------------------------------------------------------------------
    * The interfaces with the trigger module, which checks if memory
    * operations (like loads/stores) should raise debug breakpoints or exceptions
    * based on conditions defined in control/status registers (CSRs).
    */
  trigger.io.fromCsrTrigger.tdataVec := io.fromCtrl.trigger.tdataVec
  trigger.io.fromCsrTrigger.tEnableVec := io.fromCtrl.trigger.tEnableVec
  trigger.io.fromCsrTrigger.triggerCanRaiseBpExp := io.fromCtrl.trigger.triggerCanRaiseBpExp
  trigger.io.fromCsrTrigger.debugMode := io.fromCtrl.trigger.debugMode
  trigger.io.fromLoadStore.vaddr := s1_in.bits.vaddr
  trigger.io.fromLoadStore.isVectorUnitStride := s1_in.bits.isVector && s1_in.bits.is128Bits
  trigger.io.fromLoadStore.mask := s1_in.bits.mask
  trigger.io.isCbo.foreach { case isCbo => isCbo := s1_isCbo }

  val s1_triggerAction = trigger.io.toLoadStore.triggerAction
  val s1_triggerDebugMode = TriggerAction.isDmode(s1_triggerAction)
  val s1_triggerBreakpoint = TriggerAction.isExp(s1_triggerAction)
  val s1_inTriggerMode = s1_triggerDebugMode || s1_triggerBreakpoint

  // to Prefetch
  io.toPrefetch.train.s1PrefetchSpec := s1_out.fire
  io.toPrefetch.trainL1.s1PrefetchSpec := s1_out.fire

  /**
    * --------------------------------------------------------------------
    * Vector Address Offset Calculation for Trigger Mode
    * --------------------------------------------------------------------
    * This logic computes the offset for vector memory accesses in the S1 pipeline stage.
    * 1. If in trigger mode (hardware breakpoint or debug trap), the offset is calculated
    *   as the difference between the triggered virtual address and the base vector address.
    * 2. Otherwise, the offset is the sum of the current vaddr and the first unmasked element offset,
    *   minus the base vector address. This aligns the access to the correct vector element.
    *
    * This offset is used for vector load/store operations to determine which element
    * in the vector register file should be accessed or updated.
    */
  val s1_vexVAddrOffset = Mux(
    s1_inTriggerMode,
    trigger.io.toLoadStore.triggerVaddr - s1_in.bits.vecBaseVaddr,
    s1_in.bits.vaddr + genVFirstUnmask(s1_in.bits.mask).asUInt - s1_in.bits.vecBaseVaddr
  )

  s1_out <> s1_in
  s1_out.bits.uop.trigger := s1_triggerAction
  s1_out.bits.paddr := s1_paddrDupLsu
  s1_out.bits.gpaddr := s1_gpaddrDupLsu
  s1_out.bits.tlbMiss := s1_tlbMiss
  s1_out.bits.fullva := io.fromTlb.resp.bits.fullva
  s1_out.bits.vaNeedExt := io.fromTlb.resp.bits.excp(0).vaNeedExt
  s1_out.bits.isHyper := io.fromTlb.resp.bits.excp(0).isHyper
  s1_out.bits.isForVSnonLeafPTE := io.fromTlb.resp.bits.isForVSnonLeafPTE
  s1_out.bits.ptwBack := io.fromTlb.resp.bits.ptwBack
  s1_out.bits.nc := Pbmt.isNC(s1_pbmt) || s1_in.bits.nc
  s1_out.bits.mmio := Pbmt.isIO(s1_pbmt)
  s1_out.bits.ldCancel := s1_ldCancel
  s1_out.bits.vecVaddrOffset := s1_vexVAddrOffset
  s1_out.bits.vecTriggerMask := Mux(s1_inTriggerMode, trigger.io.toLoadStore.triggerMask, 0.U)
  s1_out.bits.cause(ReplayCauseNO.C_TM) := s1_tlbMiss
  s1_out.bits.cause(ReplayCauseNO.C_NK) := s1_out.bits.isLoad && !s1_out.bits.isSWPrefetch &&
    nukeDetect(io.fromSta.nukeQuery, s1_out.bits)

  /**
    * --------------------------------------------------------------------
    * TLB Response Timing Update Logic
    * --------------------------------------------------------------------
    * This logic updates the debugInfo.tlbRespTime field in the uop when a TLB response
    * is received for the corresponding memory operation. It ensures that the timing
    * information is only updated for the matching load or store queue index.
    *
    * - s1_updateTlbRespTime: True when a valid TLB response is received and the memidx
    *   matches the current uop's lqIdx (for loads) or sqIdx (for stores).
    * - When s1_updateTlbRespTime is true, update the tlbRespTime field with the current global timer.
    */
  val s1_updateTlbRespTime = io.fromTlb.resp.valid &&
    Mux(
      io.fromTlb.resp.bits.memidx.is_ld,
      io.fromTlb.resp.bits.memidx.idx === s1_in.bits.uop.lqIdx.value,
      io.fromTlb.resp.bits.memidx.idx === s1_in.bits.uop.sqIdx.value
    )
  when (s1_updateTlbRespTime) {
    s1_out.bits.uop.debugInfo.tlbRespTime := GTimer()
  }

  /**
    * --------------------------------------------------------------------
    * Update Exceptions
    * --------------------------------------------------------------------
    * This block updates the exception vector for the uop in the s1 pipeline stage.
    * - If s1_ldCancel is asserted, all exceptions are cleared and only HardwareError is set, (HardwareError exception
    *    will be late killed for better timing).
    * - Otherwise, exception bits are set based on the TLB response and trigger logic.
    *   - Page faults, guest page faults, and access faults are set if reported by TLB and the response is valid.
    *   - Breakpoint exception is set if a trigger breakpoint is detected.
    *   - Misalign is cleared by default.
    * - If any TLB-related exception occurs and the original request required a full VA check,
    *   misaligned exceptions are cleared to avoid redundant reporting.
    */
  when (s1_ldCancel) {
    s1_out.bits.uop.exceptionVec := 0.U.asTypeOf(s1_out.bits.uop.exceptionVec)
    s1_out.bits.uop.exceptionVec(hardwareError) := true.B
  } .otherwise {
    val s1_tlbRespValid = s1_tlbHit && !s1_in.bits.tlbNoQuery
    s1_out.bits.uop.exceptionVec(loadPageFault) := io.fromTlb.resp.bits.excp(0).pf.ld && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(storePageFault) := io.fromTlb.resp.bits.excp(0).pf.st && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(loadGuestPageFault) := io.fromTlb.resp.bits.excp(0).gpf.ld && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(storeGuestPageFault) := io.fromTlb.resp.bits.excp(0).gpf.st && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(loadAccessFault) := io.fromTlb.resp.bits.excp(0).af.ld && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(storeAccessFault) := io.fromTlb.resp.bits.excp(0).af.st && s1_tlbRespValid
    s1_out.bits.uop.exceptionVec(breakPoint) := s1_triggerBreakpoint
    s1_out.bits.misalign := false.B

    val s1_tlbRelatedExceptions = s1_out.bits.uop.exceptionVec(loadPageFault) ||
      s1_out.bits.uop.exceptionVec(storePageFault) ||
      s1_out.bits.uop.exceptionVec(loadGuestPageFault) ||
      s1_out.bits.uop.exceptionVec(storeGuestPageFault) ||
      s1_out.bits.uop.exceptionVec(loadAccessFault) ||
      s1_out.bits.uop.exceptionVec(storeAccessFault)
    when (RegEnable(io.toTlb.req.bits.checkfullva, false.B, s0_out.fire) && s1_tlbRelatedExceptions) {
      s1_out.bits.uop.exceptionVec(loadAddrMisaligned) := false.B
      s1_out.bits.uop.exceptionVec(storeAddrMisaligned) := false.B
      s1_out.bits.misalign := false.B
    }
  }

  //
  io.s0_s1_valid := s0_out.valid || s1_out.valid

  XSDebug(s1_out.valid,
    p"S1: pc ${Hexadecimal(s1_out.bits.uop.pc)}, lId ${Hexadecimal(s1_out.bits.uop.lqIdx.asUInt)}, tlb_miss ${s1_out.bits.tlbMiss}, " +
    p"paddr ${Hexadecimal(s1_out.bits.paddr)}, mmio ${s1_out.bits.mmio}\n"
  )

  // Debug
  io.debugLsInfo.s1_robIdx := s1_out.bits.uop.robIdx.value
  io.debugLsInfo.s1_isLoadToLoadForward := false.B // FIXME: load to load not supported right now!
  io.debugLsInfo.s1_isTlbFirstMiss := s1_out.valid && s1_out.bits.tlbMiss && s1_out.bits.isFirstIssue

  // Topdown
  io.lsTopdownInfo.s1.robIdx := s1_out.bits.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid := s1_out.valid && s1_out.bits.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits := s1_out.bits.vaddr

  XSPerfAccumulate("s1_inValid", s1_in.valid)
  XSPerfAccumulate("s1_inFired", s1_in.fire)
  XSPerfAccumulate("s1_inFiredFirstIssue", s1_in.fire && s1_in.bits.isFirstIssue)
  XSPerfAccumulate("s1_tlbMiss", s1_in.fire && s1_tlbMiss)
  XSPerfAccumulate("s1_tlbMissFirstIssue", s1_in.fire && s1_tlbMiss && s1_in.bits.isFirstIssue)
  XSPerfAccumulate("s1_ldCancel", s1_in.valid && s1_ldCancel)

  /**
    * --------------------------------------------------------------------
    * Pipeline s2
    * --------------------------------------------------------------------
    * DCache resp
    */
  val s2_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s2_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  NewPipelineConnect.connect(s1_out, s2_in, s2_in.fire, s1_kill)

  //
  val s2_kill = s2_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)

  //
  io.fromDCache.resp.ready := true.B // FIXME: need it?

  if (env.FPGAPlatform) {
    io.toDCache.s2_pc := DontCare
  } else {
    io.toDCache.s2_pc := s1_in.bits.uop.pc
  }

  /**
    * --------------------------------------------------------------------
    * Data Forwarding from Bus or MSHR
    * --------------------------------------------------------------------
    */
  val (s2_forwardFromBus, s2_forwardDataFromBus, s2_busCorrupt) = io.fromBus.forward(
    s1_out.valid && s1_out.bits.forwardTLDchannel,
    s1_out.bits.mshrId,
    s1_out.bits.paddr
  )
  val (s2_forwardDataValid, s2_forwardFromMshr, s2_forwardDataFromMshr, s2_mshrCorrupt) = io.fromMissQueue.forward.forward()
  val s2_forwardFromBusOrMshr = s2_forwardDataValid && (s2_forwardFromBus || s2_forwardFromMshr)

  /**
    * The response signal of `pmp/pma` is credible only after the physical address is actually generated.
    * Therefore, the response signals of pmp/pma generated after an address translation has produced an `access fault`
    * or a `page fault` are completely unreliable.
    */
  val s2_hasAccessExceptions = s2_in.bits.uop.exceptionVec(loadAccessFault) ||
    s2_in.bits.uop.exceptionVec(loadPageFault) ||
    s2_in.bits.uop.exceptionVec(loadGuestPageFault)

  /**
    * soft prefetch will not trigger any exception (but ecc error interrupt may
    * be triggered)
    */
  val s2_tlbUnrelatedExceptions = s2_in.bits.uop.exceptionVec(loadAddrMisaligned) ||
    s2_in.bits.uop.exceptionVec(storeAddrMisaligned) ||
    s2_in.bits.uop.exceptionVec(breakPoint)
  val s2_triggerDebugMode = RegEnable(s1_triggerDebugMode, s1_out.fire)
  val s2_hasAnyExceptions = (s2_triggerDebugMode || ExceptionNO.selectByFu(s2_out.bits.uop.exceptionVec, VlduCfg).asUInt.orR)

  /**
    * --------------------------------------------------------------------
    * TLB Response and Uncache/MMIO Determination
    * --------------------------------------------------------------------
    * - s2_tlbRespValid: True if there is no TLB miss and no access exceptions in s2 stage.
    *   Indicates that the TLB response is valid and can be trusted for further checks.
    *
    * - s2_pbmt: Physical memory attribute (PBMT) carried from s1 to s2.
    *   Used to determine memory type and access permissions.
    *
    * - s2_mmio: True if the PMP/PMAs indicate MMIO for the current access.
    *   Only valid when TLB response is valid and PBMT indicates PMA, and PMP response signals MMIO.
    *
    * - s2_uncache: True if the access should be treated as uncacheable.
    *   This is the case if PMP/PMAs indicate MMIO, or if the instruction is marked as non-cacheable or MMIO.
    *
    * Note: Uncache include MMIO, Noncacheable(NC)
    */
  val s2_tlbRespValid = !s2_in.bits.tlbMiss && !s2_hasAccessExceptions
  val s2_pbmt = RegEnable(s1_pbmt, s1_out.fire)
  val s2_mmio = s2_tlbRespValid && Mux(Pbmt.isUncache(s2_pbmt), s2_in.bits.mmio, io.fromPmp.resp.mmio)
  val s2_uncache = (s2_mmio || s2_in.bits.nc || s2_in.bits.mmio) && !s2_in.bits.isPrefetch
  val s2_memBackTypeMM = !io.fromPmp.resp.mmio
  val s2_updateAddrValid = WireInit(true.B)

  // update StoreQueue exception
  val s2_isCbo = RegEnable(s1_isCbo, s1_out.fire)
  val s2_isCboNoZero = LSUOpType.isCbo(s2_in.bits.uop.fuOpType)
  val s2_misalignVector = s2_out.bits.isMisAlignBuf && s2_out.bits.isVector

  io.toLsq.excpUpdate.valid := s2_out.valid && s2_out.bits.isStore
  io.toLsq.excpUpdate.bits := s2_out.bits
  io.toLsq.excpUpdate.bits.af := s2_out.bits.af && !s2_kill
  io.toLsq.excpUpdate.bits.mmio := (s2_mmio || s2_isCboNoZero) && !s2_hasAnyExceptions
  io.toLsq.excpUpdate.bits.miss := io.fromDCache.resp.fire && io.fromDCache.resp.bits.miss
  io.toLsq.excpUpdate.bits.updateAddrValid := s2_updateAddrValid
  io.toLsq.excpUpdate.bits.isVector := s2_out.bits.isVector || s2_misalignVector
  io.toLsq.excpUpdate.bits.hasException := (s2_hasAnyExceptions || s2_out.bits.af) && !s2_kill

  //  misalign can enqueue
  val s2_hasTriggerExceptions = s2_out.bits.uop.exceptionVec(breakPoint) || s2_triggerDebugMode
  val s2_misalignCross16Byte = s2_in.bits.misalign && !s2_in.bits.misalignWith16Byte
  val s2_uncacheIlegalMisalign = s2_uncache
  val s2_misalign = GatedValidRegNext(io.fromCtrl.csrCtrl.hd_misalign_ld_enable) &&
     s2_misalignCross16Byte && !s2_hasTriggerExceptions && !s2_uncacheIlegalMisalign
  val s2_misalignCanEnq = !isAfter(s2_out.bits.uop.lqIdx, io.fromLsq.lqDeqPtr) || io.fromMisalign.misalignAllowSpec

  /**
    * --------------------------------------------------------------------
    * Store-to-Load Forwarding Merge Logic
    * --------------------------------------------------------------------
    * This section merges forwarding results from LSQ, SBuffer, and UBuffer for each byte lane.
    * - s2_forwardMask: Indicates for each byte whether forwarding is valid from any source.
    *   Priority: LSQ > UBuffer (for non-cacheable with data) > SBuffer.
    * - s2_forwardData: The actual forwarded data for each byte, selected by priority.
    *   Uses ParallelPriorityMux to select the highest-priority valid data source.
    * - s2_fullForward: True if all requested bytes are forwarded and LSQ data is not invalid.
    */
  val s2_forwardMask = Wire(Vec(VLEN/8, Bool()))
  val s2_forwardData = Wire(Vec(VLEN/8, UInt(8.W)))
  for (i <- 0 until VLEN / 8) {
    s2_forwardMask(i) := io.fromLsq.forward.forwardMask(i) || io.fromSbuffer.forward.forwardMask(i) ||
      io.fromUbuffer.forward.forwardMask(i)

    s2_forwardData(i) := ParallelPriorityMux(Seq(
      io.fromLsq.forward.forwardMask(i) -> io.fromLsq.forward.forwardData(i),
      s2_in.bits.nc -> io.fromUbuffer.forward.forwardData(i),
      true.B -> io.fromSbuffer.forward.forwardData(i)
    ))
  }
  val s2_fullForward = ((~s2_forwardMask.asUInt & s2_in.bits.mask) === 0.U) && !io.fromLsq.forward.dataInvalid
  val s2_vpMatchFail = io.fromLsq.forward.matchInvalid ||
    io.fromSbuffer.forward.matchInvalid ||
    io.fromUbuffer.forward.matchInvalid

  // data merge
  val s2_ncWithData = RegEnable(s1_ncWithData, s1_out.fire)
  val s2_dataFromNc = shiftDataToHigh(s2_in.bits.paddr, s2_in.bits.data)
  val s2_rawDataFromPipe = Wire(new LoadDataFromDcacheBundle)
  s2_rawDataFromPipe.dcacheData := Mux(s2_ncWithData, s2_dataFromNc, io.fromDCache.resp.bits.data)
  s2_rawDataFromPipe.forwardDchan := s2_forwardFromBus && !s2_ncWithData
  s2_rawDataFromPipe.forwardDataDchan := s2_forwardDataFromBus
  s2_rawDataFromPipe.forwardMshr := s2_forwardFromMshr && !s2_ncWithData
  s2_rawDataFromPipe.forwardDataMshr := s2_forwardDataFromMshr
  s2_rawDataFromPipe.forwardResultValid := s2_forwardDataValid
  s2_rawDataFromPipe.forwardMask := s2_forwardMask
  s2_rawDataFromPipe.forwardData := s2_forwardData
  s2_rawDataFromPipe.uop := s2_out.bits.uop
  s2_rawDataFromPipe.addrOffset := s2_out.bits.paddr(3, 0)

  val s2_rawDataFromBus = s2_rawDataFromPipe.mergeTLData()
  val s2_mergedDataFromPipe = s2_rawDataFromPipe.mergeLsqFwdData(s2_rawDataFromBus)

  XSDebug(s2_in.valid, "[FWD LOAD RESP] pc %x fwd %x(%b)\n",
    s2_in.bits.uop.pc,
    s2_forwardData.asUInt, s2_forwardMask.asUInt
  )
  /**
    * --------------------------------------------------------------------
    * Replay Causes Handling
    * --------------------------------------------------------------------
    */
  // if noncacheable or all data from forwarding, it should shield some dcache related causes.
  val s2_useDCache = !s2_forwardFromBusOrMshr && !s2_fullForward && !s2_in.bits.nc

  // Indicates memory ambiguous from Lsq when MDP enable.
  val s2_memAmb = s2_in.bits.uop.storeSetHit &&
    io.fromLsq.forward.addrInvalid && RegEnable(io.toLsq.forward.valid, s1_out.fire)

  // Indicates forward fail from Lsq
  val s2_forwardFail = io.fromLsq.forward.dataInvalid && RegEnable(io.toLsq.forward.valid, s1_out.fire)

  // Indicates dcache miss from DCache (miss) and whether DCache data is needed
  val s2_dcacheMiss = io.fromDCache.resp.bits.miss && s2_useDCache

  // Indicates a enqueue nack from the MissQueue (s2_mq_nack) and whether DCache data is needed.
  val s2_mqNack = io.fromDCache.s2_mq_nack && s2_useDCache

  // Indicates a bank conflict from the DCache (s2_bank_conflict) and whether DCache data is needed.
  val s2_bankConflict = io.fromDCache.s2_bank_conflict && s2_useDCache

  // Indicates wpu predict fail from the DCache and whether DCache data is needed
  val s2_wpuPredFail = io.fromDCache.s2_wpu_pred_fail && s2_useDCache

  // Indicates a enqueue nack from the LoadQueueRAR
  val s2_rarNack = io.toLsq.rarQuery.req.valid && !io.toLsq.rarQuery.req.ready

  // Indicates a enqueue nack from the LoadQueueRAW
  val s2_rawNack = io.toLsq.rawQuery.req.valid && !io.toLsq.rawQuery.req.ready

  // Indicates a nuke
  val s2_hasNuke = nukeDetect(io.fromSta.nukeQuery, s2_in.bits) || ReplayCauseNO.hasNK(s2_in.bits.cause)

  // Indicates whether miss request accepted by MissQueue
  val s2_mshrHandled = io.fromDCache.resp.bits.handled

  //
  val s2_hasHigherPriorityCauses = ReplayCauseNO.hasHigherCausesThan(s2_out.bits.cause, ReplayCauseNO.C_DR)
  val s2_dcacheFastReplay = (s2_mqNack || !s2_dcacheMiss && (s2_bankConflict || s2_wpuPredFail))
  val s2_nukeFastReplay = !s2_mqNack && !s2_dcacheMiss && !s2_bankConflict && !s2_wpuPredFail && s2_hasNuke
  val s2_dcacheOrNukeReplay = !s2_hasHigherPriorityCauses && (s2_dcacheFastReplay || s2_nukeFastReplay)
  val s2_canFastReplay = !s2_in.bits.isFastReplay && s2_dcacheOrNukeReplay

  /**
    * This signal is true if the current memory operation in S2 can safely continue
    * without exceptions, uncache/MMIO handling, prefetch, or cancellation.
    */
  val s2_safeToProceed = !s2_hasAnyExceptions &&
    (!s2_uncache || s2_in.bits.nc) &&
    !s2_in.bits.isPrefetch &&
    !s2_in.bits.ldCancel

  //
  val s2_mmioOrNcWithData = s2_out.bits.mmio || (s2_in.bits.nc && s2_ncWithData)
  val s2_safeWakeup = !s2_out.bits.needReplay && !s2_hasAnyExceptions && !s2_out.bits.misalign && !s2_mmioOrNcWithData
  val s2_safeWriteback = s2_hasAnyExceptions || s2_safeWakeup || s2_vpMatchFail

  /**
    * --------------------------------------------------------------------
    * LSQ RAR/RAW Query Interface Assignment
    * --------------------------------------------------------------------
    * Handles the assignment of the RAR (Load After Load) and RAW (Load After Store)
    * query interfaces to the LSQ.
    */
  val s2_dataFromForward = s2_fullForward || s2_forwardDataValid
  val s2_noNeedQueryDCache = s2_out.bits.nc
  val s2_dcacheDataReady = !s2_dcacheMiss
  val s2_dataValid = s2_dataFromForward || s2_noNeedQueryDCache || s2_dcacheDataReady
  io.toLsq.rarQuery.req.valid := s2_out.valid
  io.toLsq.rarQuery.req.bits.uop := s2_out.bits.uop
  io.toLsq.rarQuery.req.bits.mask := s2_out.bits.mask
  io.toLsq.rarQuery.req.bits.paddr := s2_out.bits.paddr
  io.toLsq.rarQuery.req.bits.dataValid := s2_dataValid
  io.toLsq.rarQuery.req.bits.nc := s2_out.bits.nc

  io.toLsq.rawQuery.req.valid := io.toLsq.rarQuery.req.valid
  io.toLsq.rawQuery.req.bits := io.toLsq.rarQuery.req.bits

  //
  val s2_pmpHasAccessFault = io.fromPmp.resp.ld || io.fromPmp.resp.st
  io.toDCache.s2_kill := s2_pmpHasAccessFault || s2_uncache || s2_kill

  // RegNext prefetch train for better timing
  // ** Now, prefetch train is valid at load s3 **
  val s2_prefetchTrainValid = !s2_uncache && (!s2_in.bits.tlbMiss || s2_in.bits.isHWPrefetch)
  val s2_prefetchTrainL1Valid = !s2_uncache
  val s2_metaPrefetch = io.fromDCache.resp.bits.meta_prefetch
  val s2_metaAccess = io.fromDCache.resp.bits.meta_access

  // store misalign revoke
  io.toStoreMisalign.revoke := s2_hasAnyExceptions && s2_in.bits.isStore
  val s2_misalignBufferNack = !io.toStoreMisalign.revoke &&
    RegEnable(io.toStoreMisalign.enq.valid && !io.toStoreMisalign.enq.ready, false.B, s1_out.fire)

  // feedback
  val s2_canFeedback = GatedValidRegNext(!s1_out.bits.isVector && !s1_out.bits.isMisAlignBuf && s1_out.bits.isStore)
  io.toBackend.iqFeedback.feedbackSlow.valid := s2_out.valid && s2_canFeedback
  io.toBackend.iqFeedback.feedbackSlow.bits.robIdx := s2_out.bits.uop.robIdx
  io.toBackend.iqFeedback.feedbackSlow.bits.hit := !s2_out.bits.tlbMiss
  io.toBackend.iqFeedback.feedbackSlow.bits.flushState := s2_out.bits.ptwBack
  io.toBackend.iqFeedback.feedbackSlow.bits.sourceType := RSFeedbackType.tlbMiss
  io.toBackend.iqFeedback.feedbackSlow.bits.dataInvalidSqIdx := DontCare
  io.toBackend.iqFeedback.feedbackSlow.bits.sqIdx := s2_out.bits.uop.sqIdx
  io.toBackend.iqFeedback.feedbackSlow.bits.lqIdx := s2_out.bits.uop.lqIdx

  XSDebug(io.toBackend.iqFeedback.feedbackSlow.valid,
    "S2 Store: tlbHit: %d robIdx: %d\n",
    io.toBackend.iqFeedback.feedbackSlow.bits.hit,
    io.toBackend.iqFeedback.feedbackSlow.bits.robIdx.value
  )

  /**
    * --------------------------------------------------------------------
    * Store Misalign Writeback
    * --------------------------------------------------------------------
    */
  io.toStoreMisalign.writeback.valid := s2_out.fire && s2_out.bits.isStore && s2_out.bits.isMisAlignBuf
  io.toStoreMisalign.writeback.bits  := s2_out.bits
  io.toStoreMisalign.writeback.bits.cause := 0.U.asTypeOf(s2_out.bits.cause)
  io.toStoreMisalign.writeback.bits.cause(ReplayCauseNO.C_TM) := s2_in.bits.tlbMiss // only need tlb miss

  /**
    * Determines the vstart for vector operations in the S2 pipeline stage.
    * 1. For load replay or fast replay instructions, vstart is directly taken from the uop (micro-op) field, cause
    *  it was first time execute, it was calculated.
    * 2. For vector loads, vstart is calculated by shifting the vector address offset
    *   right by the vector element effective width (veew), which aligns the offset to the element boundary.
    */
  val s2_vstart = Mux(
    s2_in.bits.isLoadReplay || s2_in.bits.isFastReplay,
    s2_in.bits.uop.vpu.vstart,
    s2_in.bits.vecVaddrOffset >> s2_in.bits.uop.vpu.veew
  )

  //
  s2_out <> s2_in
  s2_out.bits.uop.vpu.vstart := s2_vstart
  s2_out.bits.mmio := s2_mmio
  s2_out.bits.atomic := s2_in.bits.atomic || Pbmt.isPMA(s2_pbmt) && io.fromPmp.resp.atomic
  s2_out.bits.memBackTypeMM := s2_memBackTypeMM
  s2_out.bits.misalign := s2_misalign
  s2_out.bits.cause(ReplayCauseNO.C_MA) := s2_memAmb
  s2_out.bits.cause(ReplayCauseNO.C_FF) := s2_forwardFail
  s2_out.bits.cause(ReplayCauseNO.C_DR) := s2_mqNack
  s2_out.bits.cause(ReplayCauseNO.C_DM) := s2_dcacheMiss
  s2_out.bits.cause(ReplayCauseNO.C_BC) := s2_bankConflict
  s2_out.bits.cause(ReplayCauseNO.C_WPF) := s2_wpuPredFail
  s2_out.bits.cause(ReplayCauseNO.C_RARF) := s2_rarNack
  s2_out.bits.cause(ReplayCauseNO.C_RAWF) := s2_rawNack
  s2_out.bits.cause(ReplayCauseNO.C_NK) := s2_hasNuke
  s2_out.bits.tlbId := io.fromTlb.hint.id
  s2_out.bits.tlbHandled := !io.fromTlb.hint.full
  s2_out.bits.mshrId := io.fromDCache.resp.bits.mshr_id
  s2_out.bits.mshrHandled := s2_mshrHandled
  s2_out.bits.lastBeat := s2_in.bits.paddr(log2Up(refillBytes))
  s2_out.bits.fullForward := s2_fullForward
  s2_out.bits.dataInvalidSqIdx := io.fromLsq.forward.dataInvalidSqIdx
  s2_out.bits.addrInvalidSqIdx := io.fromLsq.forward.addrInvalidSqIdx
  s2_out.bits.replayCarry := io.fromDCache.resp.bits.replayCarry

  /**
    * --------------------------------------------------------------------
    *  Update exception logic
    * --------------------------------------------------------------------
    * Handling exception detection and updates for the memory pipeline stage S2.
    * The following types of exceptions are handled:
    * 1. Address misalignment exceptions for uncacheable accesses
    * 2. Access fault exceptions for vector/CBO operations
    * 3. Data corruption exceptions from forwarding paths
    */
  // Detect address misalignment exceptions for uncacheable accesses
  val s2_uncacheNotSupportMisalign = s2_in.bits.misalign && s2_uncache && !s2_in.bits.tlbMiss
  // Check if there are higher priority exceptions that would override misalignment
  val s2_exceptionExceptMisalign = !s2_in.bits.tlbMiss &&
    (s2_triggerDebugMode || ExceptionNO.selectByFuAndUnSelect(s2_out.bits.uop.exceptionVec, StaCfg, Seq(storeAddrMisaligned)).asUInt.orR)
  // Update store/load address misaligned exceptions
  s2_out.bits.uop.exceptionVec(storeAddrMisaligned) := s2_in.bits.isStore && s2_uncacheNotSupportMisalign && !s2_exceptionExceptMisalign
  s2_out.bits.uop.exceptionVec(loadAddrMisaligned) := s2_in.bits.isLoad && s2_uncacheNotSupportMisalign

  /**
   * Access fault handling for vector/CBO operations on uncacheable memory
   * Vector and cache block operations are not supported for uncacheable memory regions,
   * so we need to raise an access fault if attempted
   */
  val s2_vectorOrCboNotSupportUncache = !s2_in.bits.tlbMiss && s2_uncache && (s2_in.bits.isVector || s2_isCbo)
  // Update store access fault exceptions
  s2_out.bits.uop.exceptionVec(storeAccessFault) := io.fromPmp.resp.st ||
    s2_vectorOrCboNotSupportUncache && s2_in.bits.isStore ||
    s2_in.bits.uop.exceptionVec(storeAccessFault)

  /**
   * Handle data corruption detection from forwarded data
   * Data can be corrupted when forwarded from:
   * 1. Bus interface (L2/L3 cache or memory)
   * 2. MSHR (Miss Status Handling Register)
   */
  val s2_hasCorrupt = s2_forwardFromBus && s2_busCorrupt ||
    s2_forwardDataValid && s2_forwardFromMshr && s2_mshrCorrupt

  // Update load access fault exceptions
  s2_out.bits.uop.exceptionVec(loadAccessFault) := io.fromPmp.resp.ld ||
    s2_vectorOrCboNotSupportUncache && s2_in.bits.isLoad ||
    s2_hasCorrupt ||
    s2_in.bits.uop.exceptionVec(loadAccessFault)


  // FIXME: never kill consumers at s2
  io.toBackend.ldCancel.ld1Cancel := false.B

  // to Prefetch
  io.toPrefetch.train.s2PrefetchSpec := s2_out.fire
  io.toPrefetch.trainL1.s2PrefetchSpec := s2_out.fire

  // Debug
  io.debugLsInfo.s2_robIdx := s2_out.bits.uop.robIdx.value
  io.debugLsInfo.s2_isBankConflict := s2_out.fire && s2_bankConflict
  io.debugLsInfo.s2_isDcacheFirstMiss := s2_out.fire && s2_dcacheMiss
  io.debugLsInfo.s2_isForwardFail := s2_out.fire && s2_forwardFail

  // Topdown
  io.lsTopdownInfo.s2.robIdx := s2_in.bits.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid := s2_in.fire && s2_in.bits.hasROBEntry && !s2_in.bits.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits := s2_in.bits.paddr
  io.lsTopdownInfo.s2.first_real_miss := io.fromDCache.resp.bits.real_miss
  io.lsTopdownInfo.s2.cache_miss_en := s2_in.fire && s2_in.bits.hasROBEntry && !s2_in.bits.tlbMiss && !s2_in.bits.missDbUpdated

  /**
    * --------------------------------------------------------------------
    * Pipeline s3
    * --------------------------------------------------------------------
    * Writeback and update load queue
    */
  val s3_in = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))
  val s3_out = WireInit(0.U.asTypeOf(DecoupledIO(new LsPipelineBundle)))

  NewPipelineConnect.connect(s2_out, s3_in, s3_in.fire, s2_kill)

  val s3_kill = s3_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
  val s3_hasHardwareError = Option.when(EnableAccurateLoadError)(
    io.fromDCache.resp.bits.error_delayed && GatedValidRegNext(io.fromCtrl.csrCtrl.cache_error_enable)
  )
  val s3_hasAnyExceptions = RegEnable(s2_hasAnyExceptions, s2_out.fire)
  val s3_vpMatchFail = RegEnable(s2_vpMatchFail, s2_out.fire)

  val s3_mmioWriteback = Wire(ValidIO(new LsPipelineBundle))
  s3_mmioWriteback.valid := GatedValidRegNextN(s0_srcFiredVec(mmioIdx), 3)
  s3_mmioWriteback.bits := GatedRegNextN(io.fromLsq.mmio.bits, 3)

  val s3_misalignWakeup = Wire(ValidIO(new LsPipelineBundle))
  s3_misalignWakeup.valid := GatedValidRegNextN(s0_srcFiredVec(mabIdx), 3)
  s3_misalignWakeup.bits := GatedRegNextN(io.fromMisalign.issue.bits, 3)

  // init
  s3_out.ready := false.B

  s3_out <> s3_in
  s3_out.valid := s3_in.valid || s3_mmioWriteback.valid
  s3_out.bits := Mux(s3_mmioWriteback.valid, s3_mmioWriteback.bits, s3_in.bits)

  // data merge
  val s3_mergedDataFromPipe = RegEnable(s2_mergedDataFromPipe, s2_out.fire)
  val s3_dataFromPipe = (0 until LdDataDup).map { case i =>
    Seq(
      s3_mergedDataFromPipe(63,      0),
      s3_mergedDataFromPipe(71,      8),
      s3_mergedDataFromPipe(79,     16),
      s3_mergedDataFromPipe(87,     24),
      s3_mergedDataFromPipe(95,     32),
      s3_mergedDataFromPipe(103,    40),
      s3_mergedDataFromPipe(111,    48),
      s3_mergedDataFromPipe(119,    56),
      s3_mergedDataFromPipe(127,    64),
      s3_mergedDataFromPipe(127,    72),
      s3_mergedDataFromPipe(127,    80),
      s3_mergedDataFromPipe(127,    88),
      s3_mergedDataFromPipe(127,    96),
      s3_mergedDataFromPipe(127,   104),
      s3_mergedDataFromPipe(127,   112),
      s3_mergedDataFromPipe(127,   120),
    )
  }
  val s3_dataSelectdByOffset = s3_dataFromPipe.map { case data =>
    Mux1H(genDataSelectByOffset(s3_in.bits.paddr(3, 0)), data)
  }
  val s3_signExtData = s3_dataSelectdByOffset.map { case data =>
    newRdataHelper(genRdataOH(s3_in.bits.uop), data, XLEN)
  }

  // to Ldu
  val s3_canFastReplay = RegEnable(s2_canFastReplay, false.B, s2_out.fire)
  io.toLdu.replay.valid := s3_out.valid && s3_canFastReplay
  io.toLdu.replay.bits := s3_out.bits
  io.toLdu.replay.bits.ldCancel := s3_hasHardwareError.getOrElse(false.B)

  val s3_fastReplayCanceled = io.toLdu.replay.valid && io.toLdu.replay.bits.forwardTLDchannel ||
    io.fromMisalign.issue.valid ||
    !io.toDCache.req.ready
  val s3_canWritebackLsq = !s3_canFastReplay || s3_fastReplayCanceled

  // to misalign buffer
  val s3_misalignCanEnq = RegEnable(s2_misalignCanEnq, false.B, s2_out.fire)
  val s3_misalignNeedEnq = !s3_out.bits.isMisAlignBuf && s3_out.bits.misalign && s3_canWritebackLsq
  io.toLoadMisalign.enq.valid := s3_out.valid && s3_out.bits.isLoad && s3_misalignNeedEnq
  io.toLoadMisalign.enq.bits := s3_out.bits
  io.toLoadMisalign.revoke := false.B

  val s3_misalignCanWriteback = s3_canWritebackLsq && s3_out.bits.isMisAlignBuf || s3_misalignWakeup.valid
  io.toLoadMisalign.writeback.valid := s3_out.valid && s3_misalignCanWriteback
  io.toLoadMisalign.writeback.bits := Mux(s3_misalignWakeup.valid, s3_misalignWakeup.bits, s3_out.bits)
  io.toLoadMisalign.writeback.bits.data := s3_signExtData.head
  io.toLoadMisalign.writeback.bits.cause := Mux(
    s3_misalignWakeup.valid,
    0.U.asTypeOf(s3_in.bits.cause.asUInt), // just wakeup
    PriorityEncoderOH(s3_in.bits.cause.asUInt) // no need C_MF cause
  ).asBools

  // update cause
  val s3_writebackCause = Wire(s3_out.bits.cause.cloneType)
  s3_out.bits.cause(ReplayCauseNO.C_MF) := io.toLoadMisalign.enq.valid && !io.toLoadMisalign.enq.ready

  when (s3_hasAnyExceptions || s3_hasHardwareError.getOrElse(false.B) || s3_vpMatchFail || s3_in.bits.isMisAlignBuf) {
    s3_writebackCause := 0.U.asTypeOf(s3_writebackCause.cloneType)
  } .otherwise {
    s3_writebackCause := PriorityEncoderOH(s3_out.bits.cause.asUInt).asBools
  }

  io.toLsq.writeback.valid := s3_out.valid && s3_out.bits.isLoad && s3_canWritebackLsq
  io.toLsq.writeback.bits := s3_out.bits
  io.toLsq.writeback.bits.updateAddrValid := false.B
  io.toLsq.writeback.bits.cause := s3_writebackCause
  io.toLsq.writeback.bits.hasException := false.B

  // LoadQueueRAR and LoadQueueRAW revoke
  val s3_misalignNeedReplay = s3_out.bits.isMisAlignBuf && io.toLoadMisalign.writeback.bits.needReplay
  val s3_needReplay = io.toLsq.writeback.bits.needReplay || s3_misalignNeedReplay

  /**
   * Load Revocation Logic
   * A load needs to be revoked (cancelled) if any of these conditions are true:
   * 1. Has any exceptions detected
   * 2. Needs replay due to dependencies/conflicts
   * 3. Is a misaligned access
   */
  val s3_loadNeedRevoke = (s3_hasAnyExceptions || s3_needReplay || s3_in.bits.misalign) && s3_out.bits.isLoad
  io.toLsq.rarQuery.revoke := s3_loadNeedRevoke
  io.toLsq.rawQuery.revoke := s3_loadNeedRevoke

  /**
    * --------------------------------------------------------------------
    * Handle Rollback
    * --------------------------------------------------------------------
    * Handling rollback scheme if
    * +----------------------------------------------------------+----------------+
    * | VAddr-PAddr match fail when store to load forwarding     | flush itself   |
    * +----------------------------------------------------------+----------------+
    * | RAR nuke (load to load violation)                        | flush after    |
    * +----------------------------------------------------------|----------------+
    * | Misalign request (issued by Misalign Buffer) need replay | flush itself   |
    * +----------------------------------------------------------+----------------+
    */
  val s3_hasRARNuke = io.fromLsq.rarQuery.valid &&
    io.fromLsq.rarQuery.bits.replayInst &&
    GatedValidRegNext(io.fromCtrl.csrCtrl.ldld_vio_check_enable)
  val s3_misalignNeedFlush = s3_in.bits.isMisAlignBuf && (
    ReplayCauseNO.hasFF(io.toLoadMisalign.writeback.bits.cause) ||
    ReplayCauseNO.hasMA(io.toLoadMisalign.writeback.bits.cause) ||
    ReplayCauseNO.hasNK(io.toLoadMisalign.writeback.bits.cause) ||
    ReplayCauseNO.hasRARF(io.toLoadMisalign.writeback.bits.cause)
  )
  val s3_needRollback = (s3_vpMatchFail || s3_hasRARNuke || s3_misalignNeedFlush) && !s3_hasAnyExceptions
  val s3_flushAfter = Mux(s3_vpMatchFail || s3_misalignNeedFlush, RedirectLevel.flush, RedirectLevel.flushAfter)
  io.toBackend.rollback.valid := s3_out.valid && s3_needRollback
  io.toBackend.rollback.bits := s3_out.bits.toRedirectBundle(s3_flushAfter)

  XSPerfAccumulate("s3_rollbackTotal", io.toBackend.rollback.valid)
  XSPerfAccumulate("s3_flushItselfRollback", io.toBackend.rollback.valid && s3_vpMatchFail)
  XSPerfAccumulate("s3_flushAfterRollback", io.toBackend.rollback.valid && s3_hasRARNuke)
  XSPerfAccumulate("s3_misalignRollback", io.toBackend.rollback.valid && s3_in.bits.isMisAlignBuf)

  /**
    * --------------------------------------------------------------------
    * Writeback to Backend
    * --------------------------------------------------------------------
    */
  val s3_safeWriteback = RegEnable(s2_safeWriteback, false.B, s2_out.fire)
  val s3_canWriteback = s3_safeWriteback || s3_hasHardwareError.getOrElse(false.B)
  val s3_scalarCanWriteback = s3_out.bits.isLoad && !s3_out.bits.isVector && s3_canWriteback
  io.toBackend.lduWriteback.valid := s3_out.valid && s3_scalarCanWriteback
  io.toBackend.lduWriteback.bits := s3_out.bits
  io.toBackend.lduWriteback.bits.data := Mux(s3_mmioWriteback.valid, s3_mmioWriteback.bits.data, s3_signExtData.head)

  when (s3_out.bits.isLoad && !s3_out.bits.isVector) {
    io.toBackend.lduWriteback.ready <> s3_out.ready
  }

  /**
    * --------------------------------------------------------------------
    * Writeback to Vector Execution Block
    * --------------------------------------------------------------------
    * Only allow vector load writeback if not misaligned and pipeline is ready.
    * Handles exception, hit, and data selection for vector writeback.
    */
  val s3_vexCanWriteback = s3_canWriteback && !s3_out.bits.misalign && !s3_out.bits.isMisAlignBuf
  io.toVex.lduWriteback.valid := s3_out.valid && s3_out.bits.isLoad && s3_out.bits.isVector && s3_vexCanWriteback
  io.toVex.lduWriteback.bits := s3_out.bits.toVecPipelineFeedbackBundle()
  io.toVex.lduWriteback.bits.hasException := s3_hasAnyExceptions || s3_hasHardwareError.getOrElse(false.B)
  io.toVex.lduWriteback.bits.hit := !io.toLsq.writeback.bits.needReplay || io.toLsq.writeback.ready
  io.toVex.lduWriteback.bits.exceptionVec := ExceptionNO.selectByFu(s3_out.bits.uop.exceptionVec, VlduCfg)
  io.toVex.lduWriteback.bits.nc := DontCare
  io.toVex.lduWriteback.bits.mmio := DontCare
  io.toVex.lduWriteback.bits.sourceType := RSFeedbackType.lrqFull
  io.toVex.lduWriteback.bits.vecdata.get := ParallelPriorityMux(Seq( // FIXME: if vector can access mmio memory space
    s3_out.bits.misalignWith16Byte -> s3_dataSelectdByOffset(1),
    s3_out.bits.is128Bits -> s3_mergedDataFromPipe(1),
    true.B -> rdataVecHelper(s3_out.bits.alignType(1, 0), s3_dataSelectdByOffset(1), VLEN)
  ))

  when (s3_out.bits.isLoad && s3_out.bits.isVector) {
    io.toVex.lduWriteback.ready <> s3_out.ready
  }

  /**
    * Used to cancel of a load and consumers if it cannot be safely written back and is not a vector.
    */
  val s3_ldNeedCancel = !s3_safeWriteback && !s3_out.bits.isVector
  io.toBackend.ldCancel.ld2Cancel := s3_out.valid && s3_ldNeedCancel && s3_out.bits.isLoad

  /**
    * --------------------------------------------------------------------
    * Prefetch Train
    * --------------------------------------------------------------------
    * Handling the assignment of prefetch training signals at the S3 pipeline stage.
    * Ensures that prefetch training is only triggered for valid memory uop
    * that have passed through the pipeline and met the necessary conditions in S2.
    */
  io.toPrefetch.train.req.valid := s3_out.valid && RegEnable(s2_prefetchTrainValid, s2_out.fire)
  io.toPrefetch.train.req.bits.fromLsPipelineBundle(s3_out.bits)
  io.toPrefetch.train.req.bits.metaPrefetch := RegEnable(s2_metaPrefetch, s2_out.fire)
  io.toPrefetch.train.req.bits.metaAccess := RegEnable(s2_metaAccess, s2_out.fire)
  io.toPrefetch.trainL1.req.valid := s3_out.valid && RegEnable(s2_prefetchTrainL1Valid, s2_out.fire)
  io.toPrefetch.trainL1.req.bits := io.toPrefetch.train.req.bits

  // Software prefetch, prefetch.i(Zicbop)
  io.toPrefetch.ifetch.valid := RegNext(io.fromBackend.issue.fire) && s1_in.bits.isPrefetchI
  io.toPrefetch.ifetch.bits.vaddr := s1_in.bits.vaddr

  // debug
  io.debugLsInfo.s3_robIdx := s3_out.bits.uop.robIdx.value
  io.debugLsInfo.s3_isReplayFast := s3_out.valid && s3_out.bits.isLoad && !s3_fastReplayCanceled
  io.debugLsInfo.s3_isReplayRS := false.B // Load never replay from Iq
  io.debugLsInfo.s3_isReplaySlow := io.toLsq.writeback.valid && io.toLsq.writeback.bits.needReplay
  io.debugLsInfo.s3_isReplay := s3_out.valid && io.toLsq.writeback.bits.needReplay
  io.debugLsInfo.replayCause := io.toLsq.writeback.bits.cause
  io.debugLsInfo.replayCnt := 1.U

  XSPerfAccumulate("s3_ncLdWriteback", io.toBackend.lduWriteback.valid && s3_out.bits.nc)
  XSPerfAccumulate("s3_ncLdException", s3_out.valid && s3_out.bits.nc && s3_out.bits.uop.exceptionVec.reduce(_|_))
  XSPerfAccumulate("s3_ncRARNuke", s3_out.valid && s3_out.bits.nc && s3_hasRARNuke)
  XSPerfAccumulate("s3_ncRAWNuke", s3_out.valid && s3_out.bits.nc && ReplayCauseNO.hasNK(s3_in.bits.cause))
  XSPerfAccumulate("s3_ncRARNack", s3_out.valid && s3_out.bits.nc && ReplayCauseNO.hasRARF(s3_in.bits.cause))
  XSPerfAccumulate("s3_ncRAWNack", s3_out.valid && s3_out.bits.nc && ReplayCauseNO.hasRAWF(s3_in.bits.cause))
  XSPerfAccumulate("s3_ncStLdForward", s3_out.valid && s3_out.bits.nc && RegNext(s2_fullForward))
  XSPerfAccumulate("s3_ncStLdForwardNotReady", s3_out.valid && s3_out.bits.nc && RegNext(s2_memAmb || s2_forwardFail))
  XSPerfAccumulate("s3_ncStLdForwardAddrMismatch", s3_out.valid && s3_out.bits.nc && s3_vpMatchFail)

  /**
    * --------------------------------------------------------------------
    * Store Writeback delay
    * --------------------------------------------------------------------
    * Determine whether unit has a store execution (StaExe), this block handles the store writeback
    * and the propagation of data through the pipeline.
    */
  val wbPipelines = Seq.fill(TotalDelayCycles)(Module(new NewPipelineConnectPipe(new LsPipelineBundle)))

  /**
    * --------------------------------------------------------------------
    * Notes:
    * - This logic ensures that store writeback is correctly delayed and flushed
    *   as needed, supporting single-cycle or multi-cycle writeback.
    */
  // init
  io.toBackend.staWriteback.valid := false.B
  io.toBackend.staWriteback.bits := DontCare

  io.toVex.staWriteback.valid := false.B
  io.toVex.staWriteback.bits := DontCare

  // scalar store writeback
  io.toBackend.staWriteback.valid := s3_out.valid && s3_out.bits.isStore && !s3_out.bits.isVector
  io.toBackend.staWriteback.bits := s3_out.bits
  io.toBackend.staWriteback.bits.data := DontCare
  when (s3_out.bits.isStore && !s3_out.bits.isVector) {
    io.toBackend.staWriteback.ready <> s3_out.ready
  }

  // vector store writeback
  io.toVex.staWriteback.valid := s3_out.valid && s3_out.bits.isStore && s3_out.bits.isVector
  io.toVex.staWriteback.bits := s3_out.bits.toVecPipelineFeedbackBundle(isVStore = true)
  io.toVex.staWriteback.bits.vecdata.foreach { _ := DontCare }
  when (s3_out.bits.isStore && s3_out.bits.isVector) {
    io.toVex.staWriteback.ready <> s3_out.ready
  }

  // delay writeback
  if (wbPipelines.length > 0) {
    wbPipelines.head.io.in.valid := s3_out.valid && s3_out.bits.isStore && s3_canWriteback
    wbPipelines.head.io.in.bits := s3_out.bits
    wbPipelines.head.io.isFlush := s3_out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
    wbPipelines.head.io.rightOutFire := wbPipelines.head.io.out.fire
    // override
    when (s3_out.bits.isStore) {
      wbPipelines.head.io.in.ready <> s3_out.ready
    }

    wbPipelines.dropRight(1).zip(wbPipelines.drop(1)).foreach { case (sink, source) =>
      sink.io.in <> source.io.out
      sink.io.isFlush := source.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      sink.io.rightOutFire := sink.io.out.fire
    }

    // override
    when (!wbPipelines.last.io.out.bits.isVector) {
      io.toBackend.staWriteback <> wbPipelines.last.io.out
      io.toBackend.staWriteback.bits.data := DontCare
    } .otherwise {
      io.toVex.staWriteback.valid := wbPipelines.last.io.out.valid
      io.toVex.staWriteback.ready <> wbPipelines.last.io.out.ready
      io.toVex.staWriteback.bits := wbPipelines.last.io.out.bits.toVecPipelineFeedbackBundle(isVStore = true)
      io.toVex.staWriteback.bits.vecdata.foreach { _ := DontCare }
    }
  }

}
