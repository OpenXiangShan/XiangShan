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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.mem.mdp._
import xiangshan.mem.Bundles._
import xiangshan.mem.ReplayCauseNO
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._

class HyuIO()(implicit p: Parameters, val params: MemUnitParams) extends MemUnitIO
  with HasMemBlockParameters {
  // from
  val fromLsq = OptionWrapper(params.hasLdExe, new Bundle() {
    val forward = Input(new LoadForwardRespBundle)
    val rarNuke = Flipped(ValidIO(new LoadNukeQueryRespBundle))
    val rawNuke = Flipped(ValidIO(new LoadNukeQueryRespBundle))
    val mmioLdWriteback = Flipped(ValidIO(new MemExuOutput))
    val mmioLdData = Input(new LoadDataFromLQBundle)
  })
  val fromSta = OptionWrapper(params.hasLdExe, Vec(StorePipelineWidth, Flipped(ValidIO(new StoreNukeQueryBundle))))
  val fromSBuffer = OptionWrapper(params.hasLdExe, Input(new LoadForwardRespBundle))
  val fromUncache = OptionWrapper(params.hasLdExe, Input(new LoadForwardRespBundle))
  val fromMissQueue = OptionWrapper(params.hasLdExe, Flipped(new MissQueueForwardRespBundle))
  val fromTL = OptionWrapper(params.hasLdExe, Input(new DcacheToLduForwardIO))

  // to
  val toLdu = new Bundle() {
    val replay = OptionWrapper(params.hasLdExe, DecoupledIO(new LsPipelineBundle))
    val nukeQuery = OptionWrapper(params.hasStaExe, ValidIO(new StoreNukeQueryBundle))
  }
  val toLsq = new Bundle() {
    val out = OptionWrapper(params.hasLdExe, DecoupledIO(new LsPipelineBundle))
    val forward = OptionWrapper(params.hasLdExe, ValidIO(new LoadForwardReqBundle))
    val rawNuke = OptionWrapper(params.hasLdExe, new Bundle() {
      val req = DecoupledIO(new LoadNukeQueryReqBundle)
      val revoke = Output(Bool())
    })
    val rarNuke = OptionWrapper(params.hasLdExe, new Bundle() {
      val req = DecoupledIO(new LoadNukeQueryReqBundle)
      val revoke = Output(Bool())
    })
    val addrUpdate = OptionWrapper(params.hasStaExe, ValidIO(new LsPipelineBundle))
    val excpUpdate = OptionWrapper(params.hasStaExe, new LsPipelineBundle)
    val maskOut = OptionWrapper(params.hasStaExe, ValidIO(new StoreMaskBundle))
  }
  val toSBuffer = OptionWrapper(params.hasLdExe, ValidIO(new LoadForwardReqBundle))
  val toUncache = OptionWrapper(params.hasLdExe, ValidIO(new LoadForwardReqBundle))
  val toMissQueue = OptionWrapper(params.hasLdExe, ValidIO(new MissQueueForwardReqBundle))
  val toPrefetch = new Bundle() {
    val ifetch = OptionWrapper(params.hasLdExe, ValidIO(new SoftIfetchPrefetchBundle))
    val train = OptionWrapper(params.hasPrefetch, new LsPrefetchTrainIO)
    val trainL1 = OptionWrapper(params.hasLdExe, new LsPrefetchTrainIO)
  }
  val toLoadMisalignBuf = OptionWrapper(params.hasLdExe, new MemUnitToMisalignBufferIO)
  val toStoreMisalignBuf = OptionWrapper(params.hasStaExe, new MemUnitToMisalignBufferIO)

  //
  val correctMissTrain = OptionWrapper(params.hasLdExe, Input(Bool())) // TODO: remove it?

  // perf
  val debugLsInfo = Output(new DebugLsInfoBundle)
  val lsTopdownInfo = Output(new LsTopdownInfo)

  // common
  val commonOut = Output(new Bundle() {
    val storePipeEmpty = Bool()
  })
}

class HyuImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
  with HasDCacheParameters
  with HasCircularQueuePtrHelper {

  io.suggestName("none")
  override lazy val io = IO(new HyuIO).suggestName("io")

  protected val (fromLsq, toLsq) = (io.fromLsq, io.toLsq)
  protected val (fromSBuffer, toSBuffer) = (io.fromSBuffer, io.toSBuffer)
  protected val (fromMissQueue, toMissQueue) = (io.fromMissQueue, io.toMissQueue)
  protected val (fromUncache, toUncache) = (io.fromUncache, io.toUncache)
  protected val fromTL = io.fromTL
  protected val fromSta = io.fromSta
  protected val toPrefetch = io.toPrefetch
  protected val toLdu = io.toLdu
  protected val toStoreMisalignBuf = io.toStoreMisalignBuf
  protected val toLoadMisalignBuf = io.toLoadMisalignBuf
  protected val commonOut = io.commonOut

  // Modules
  val forwardNetwork = OptionWrapper(params.hasLdExe, Module(new ForwardNetwork))
  val exceptionGen = Module(new ExceptionGen())
  val replayCauseGen = Module(new ReplayCauseGen())
  val feedbackGen = Module(new FeedbackGen)

  val forwardNetworkCommonOut = WireInit(0.U.asTypeOf(new ForwardNetworkCommonOutBundle))
  // Pipeline
  // -------------------------------------------------------------------
  // stage 0
  // -------------------------------------------------------------------
  // to Tlb
  val s0_vaddr = WireInit(0.U(VAddrBits.W))
  val s0_fullva = WireInit(0.U(XLEN.W))
  val s0_tlbVAddr = WireInit(0.U(VAddrBits.W))
  val s0_tlbFullVAddr = WireInit(0.U(XLEN.W))
  val s0_dcacheVAddr = WireInit(0.U(VAddrBits.W))
  val s0_paddr = WireInit(0.U(PAddrBits.W))
  val s0_tlbNoQuery = WireInit(false.B)

  // override select logic
  s0_in.zip(s0_chosen).zip(zipIssueParams()).zipWithIndex.foreach {
    case (((in, chosen), (issue, issueParam)), i) =>
      // Generate virtual address
      val vaddr = issueParam.issueType match {
        case MemIssueType.Ld | MemIssueType.Sta  => AGen(issue.bits.src(0), issue.bits.uop.imm, VAddrBits)
        case _ => issue.bits.vaddr
      }

      // Generate full virtual address
      val fullva = issueParam.issueType match {
        case MemIssueType.Ld | MemIssueType.Sta => AGen(issue.bits.src(0), issue.bits.uop.imm, XLEN)
        case _ => issue.bits.fullva
      }

      // Generate physical address
      val paddr = issueParam.issueType match {
        case MemIssueType.Fr => issue.bits.paddr
        case _ => 0.U
      }

      // Generate mask
      val mask = issueParam.issueType match {
        case MemIssueType.Ld | MemIssueType.Sta => genVWmask128(vaddr, issue.bits.uop.fuOpType(2, 0))
        case _ => issue.bits.mask
      }

      // Connect input to issue
      in.bits.vaddr := vaddr
      in.bits.fullva := fullva
      in.bits.paddr := paddr
      in.bits.mask := mask

      when (issue.valid && chosen) {
        // Set s0_vaddr
        if (MemIssueType.needVAddr(issueParam.issueType)) {
          s0_vaddr := vaddr
        }
        // Set s0_tlbVAddr
        if (MemIssueType.needTlbVAddr(issueParam.issueType)) {
          s0_tlbVAddr := vaddr
          s0_tlbFullVAddr := fullva
        }
        // Set s0_dcacheVAddr
        if (MemIssueType.needDCacheVAddr(issueParam.issueType)) {
          s0_dcacheVAddr := vaddr
        }
        // Set s0_paddr
        if (MemIssueType.needPAddr(issueParam.issueType)) {
          s0_paddr := paddr
        }
      }
  }

  /**
   * Determine whether stage 0 (s0) can proceed to the next stage
   * 1. If the instruction is a store uop
   * 2. If the instruction operates on non-cacheable memory
   * 3. If the data cache (DCache) is ready to accept a request
   */
  val s0_canGo = s0_out.bits.isStore || s0_out.bits.isNoncacheable || toDCache.req.ready

  s0_selOut.ready := s0_canGo && s1_in.ready
  s0_out.valid := s0_selOut.valid && !s0_selOut.bits.isMmio

  /**
    * Determine whether the TLB (Translation Lookaside Buffer) query can be cancelled for s0
    * 1. If the instruction is a hardware prefetch
    * 2. If the instruction is a software prefetch
    * 3. If the instruction is marked for fast replay
    * 4. If the instruction operates on non-cacheable memory
    */
  s0_tlbNoQuery := s0_out.bits.isHWPrefetch || s0_out.bits.isPrefetchInst || s0_out.bits.isFastReplay ||
    s0_out.bits.isNoncacheable

  // to Tlb
  toTlb.req.valid := s0_out.valid && s0_canGo
  toTlb.req.bits.vaddr := s0_tlbVAddr

  /**
    * Tlb query command:
    * 1. software prefetch write (prefetch_w) or store prefetch: TlbCmd.write
    * 2. otherwise: TlbCmd.read
    */
  toTlb.req.bits.cmd := Mux(s0_out.bits.isSWPrefetchWrite || s0_out.bits.isStore, TlbCmd.write, TlbCmd.read)

  /**
    * Tlb query size:
    * 1. vector: alignedType(2, 0)
    * 2. otherwise: fuOpType
    */
  toTlb.req.bits.size := Mux(s0_out.bits.isVector, s0_out.bits.alignType(2, 0), s0_out.bits.uop.fuOpType)

  // to DCache
  toDCache.req.valid := s0_out.valid && !s0_out.bits.isPrefetchInst && !s0_out.bits.isNoncacheable
  toDCache.req.bits.vaddr := s0_dcacheVAddr

  /**
    * DCache query command:
    * 1. prefetch read: M_PFR
    * 2. prefetch write or store prefetch: M_PFW
    * 3. load: M_XRD
    */
  toDCache.req.bits.cmd := Mux(
    s0_out.bits.isSWPrefetchRead,
    MemoryOpConstants.M_PFR,
    Mux(
      s0_out.bits.isSWPrefetchWrite || s0_out.bits.isStore,
      MemoryOpConstants.M_PFW,
      MemoryOpConstants.M_XRD
    )
  )

  /**
    * DCache query source:
    * 1. prefetch: DCACHE_PREFETCH_SOURCE
    * 2. store prefetch: STORE_SOURCE
    * 3. load: LOAD_SOURCE
    */
  toDCache.req.bits.instrtype := Mux(
    s0_out.bits.isHWPrefetch,
    DCACHE_PREFETCH_SOURCE.U,
    Mux(
      s0_out.bits.isStore,
      STORE_SOURCE.U,
      LOAD_SOURCE.U
    )
  )

  /**
    * DCache query prefetch source:
    * 1. prefetch: pfSource.value
    * 2. otherwise: L1_HW_PREFETCH_NULL
    */
  toDCache.pf_source := Mux(s0_out.bits.isHWPrefetch, s0_out.bits.pfSource.value, L1_HW_PREFETCH_NULL)

  // DCache query size
  toDCache.is128Req := s0_out.bits.is128bit

  // dcache replacement extra info
  // TODO: should prefetch load update replacement?
  toDCache.replacementUpdated := Mux(s0_out.bits.isLoadReplay, s0_out.bits.replacementUpdated, false.B)

  if (!env.FPGAPlatform) {
    toDCache.s0_pc := s0_out.bits.uop.pc
  }

  // to Prefetch
  // prefetch related ctrl signal
  if (toPrefetch.train.isDefined) {
    // Determine whether the current request is a high-confidence hardware prefetch
    val s0_highConfPrefetch = s0_out.bits.isHWPrefetch && s0_out.bits.confidence > 0.U
    // Accept high-confidence prefetch requests if valid and dcache is ready
    toPrefetch.train.get.canAcceptHighConfPrefetch := s0_out.valid && s0_highConfPrefetch && toDCache.req.ready

    // Determine whether the current request is a low-confidence hardware prefetch
    val s0_lowConfPrefetch = s0_out.bits.isHWPrefetch && s0_out.bits.confidence === 0.U
    // Accept low-confidence prefetch requests if valid and dcache is ready
    toPrefetch.train.get.canAcceptLowConfPrefetch := s0_out.valid && s0_lowConfPrefetch && toDCache.req.ready
  }

  if (toPrefetch.trainL1.isDefined) {
    // no need to train l1
    toPrefetch.trainL1.get.canAcceptHighConfPrefetch := DontCare
    toPrefetch.trainL1.get.canAcceptLowConfPrefetch := DontCare
  }

  // prefetch.i(Zicbop)
  if (toPrefetch.ifetch.isDefined) {
    // Determine whether the current request is a prefetch_i (only issue from `IssueQueue`)
    val ifetchValid = s0_out.bits.isIq && s0_out.bits.isPrefetchInst
    toPrefetch.ifetch.get.valid := RegNext(ifetchValid)
    toPrefetch.ifetch.get.bits.vaddr := RegEnable(s0_out.bits.vaddr, 0.U, ifetchValid)
  }

  // to Backend
  if (params.hasLdExe) {
    toBackend.wakeup <> feedbackGen.io.toBackend.wakeup
  }

  // to Lsq
  if (toLsq.maskOut.isDefined) {
    // store needs to update mask.
    toLsq.maskOut.get.valid := s0_out.valid && s0_out.bits.isStore
    toLsq.maskOut.get.bits.mask := s0_out.bits.mask
    toLsq.maskOut.get.bits.sqIdx := s0_out.bits.uop.sqIdx
  }

  // assign s0_out
  s0_out.bits.vaddr := Mux(s0_out.bits.isNoncacheable, s0_selOut.bits.vaddr, s0_dcacheVAddr)
  s0_out.bits.fullva := s0_fullva
  s0_out.bits.paddr := s0_paddr
  s0_out.bits.tlbNoQuery := s0_tlbNoQuery
  s0_out.bits.is128bit := s0_selOut.bits.is128bit || exceptionGen.io.commonOut.s0_misalignWith16Bytes
  s0_out.bits.misalign := exceptionGen.io.commonOut.s0_misalign
  s0_out.bits.misalignNeedWakeUp := s0_selOut.bits.isMisalignBuf && s0_selOut.bits.misalignNeedWakeUp
  s0_out.bits.misalignWith16Bytes := exceptionGen.io.commonOut.s0_misalignWith16Bytes
  s0_out.bits.isFinalSplit := exceptionGen.io.commonOut.s0_isFinalSplit

  // Debug info
  when (s0_out.valid && s0_out.bits.isFirstIssue) {
    s0_out.bits.uop.debugInfo.tlbFirstReqTime := GTimer()
  }

  // perf cnt
  val perf_dcacheVAddr = toDCache.req.bits.vaddr
  val perf_vaddrHighMatch = perf_dcacheVAddr(VAddrBits - 1, 12) === s0_out.bits.src(0)
  val perf_isNotVector = !s0_out.bits.isVector
  XSPerfAccumulate("s0_stallDCache", s0_out.valid && !toDCache.req.ready)
  XSPerfAccumulate("s0_addrSpecSuccess", s0_out.fire && perf_vaddrHighMatch && perf_isNotVector)
  XSPerfAccumulate("s0_addrSpecFailed", s0_out.fire && !perf_vaddrHighMatch && perf_isNotVector)
  XSPerfAccumulate("s0_addrSpecSuccessOnce", s0_out.fire  && perf_vaddrHighMatch && perf_isNotVector && s0_out.bits.isFirstIssue)
  XSPerfAccumulate("s0_addrSpecFailedOnce", s0_out.fire && !perf_vaddrHighMatch && perf_isNotVector && s0_out.bits.isFirstIssue)
  XSPerfAccumulate("s0_vecAddrVlenAligned", s0_out.fire  && s0_out.bits.isVector && perf_dcacheVAddr(3, 0) === 0.U)
  XSPerfAccumulate("s0_vecAddrVlenUnaligned", s0_out.fire && s0_out.bits.isVector && perf_dcacheVAddr(3, 0) =/= 0.U)
  XSPerfAccumulate("s0_forwardTLDchannel", s0_out.fire && s0_out.bits.forwardTLDchannel)
  XSPerfAccumulate("s0_hardwarePrefetchFire", s0_out.fire && s0_out.bits.isHWPrefetch)
  XSPerfAccumulate("s0_softwarePrefetchFire", s0_out.fire && s0_out.bits.isSWPrefetch)
  XSPerfAccumulate("s0_hardwarePrefetchBlocked", s0_out.valid && !s0_out.ready && s0_out.bits.isHWPrefetch)
  XSPerfAccumulate("s0_hardwarePrefetchTotal", s0_out.fire && s0_out.bits.isPrefetch)

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  // last cycle redirect
  val s1_redirect = Wire(Valid(new Redirect))
  s1_redirect.valid := GatedValidRegNext(fromCtrl.redirect.valid)
  s1_redirect.bits := RegEnable(fromCtrl.redirect.bits, fromCtrl.redirect.valid)

  // Checkif there is an exception in the current uop
  val s1_exceptionVec = exceptionGen.io.commonOut.s1_exceptionVecOut
  val s1_hasException = Cat(ExceptionNO.partialSelect(s1_exceptionVec, params.exceptionOut)).orR

  // Determine whether there is a delayed error that requires a fast replay
  val s1_delayedError = s1_in.bits.delayedError && s1_in.bits.isFastReplay

  s1_kill := s1_delayedError ||
    s1_in.bits.uop.robIdx.needFlush(fromCtrl.redirect) ||
    s1_in.bits.uop.robIdx.needFlush(s1_redirect)

  // from tlb
  val s1_paddrDupLsu = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.paddr,
    fromTlb.resp.bits.paddr(0)
  )
  val s1_paddrDupDCache = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.paddr,
    fromTlb.resp.bits.paddr(1)
  )
  val s1_gpaddrDupLsu = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.gpaddr,
    fromTlb.resp.bits.gpaddr(0)
  )
  val s1_vaNeedExt = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.vaNeedExt,
    fromTlb.resp.bits.excp(0).vaNeedExt
  )
  val s1_isHyper = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.isHyper,
    fromTlb.resp.bits.excp(0).isHyper
  )
  val s1_isForVSnonLeafPTE = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.isForVSnonLeafPTE,
    fromTlb.resp.bits.isForVSnonLeafPTE
  )
  val s1_ptwBack = Mux(
    s1_in.bits.tlbNoQuery,
    s1_in.bits.ptwBack,
    fromTlb.resp.bits.ptwBack
  )

  /**
    * Determine whether the TLB response is valid for s1
    * 1. If TLB query cancelled.
    * 1. If TLB response is valid.
    * 1. If TLB response is miss.
    */
  val s1_tlbMiss = fromTlb.resp.valid && fromTlb.resp.bits.miss
  val s1_tlbRealMiss = !s1_in.bits.tlbNoQuery && s1_tlbMiss

  /**
    * Assign a value to s1_pbmt:
    * 1. If the TLB query is cancelled (`tlbNoQuery`) or a TLB miss (`s1_tlbMiss`),
    *   assign a default value of 0.
    * 2. Otherwise, use the most significant bit of the pbmt field from the TLB response.
    */
  s1_pbmt := Mux(s1_in.bits.tlbNoQuery || s1_tlbMiss, 0.U, fromTlb.resp.bits.pbmt.head)

  // to Tlb
  toTlb.req_kill := s1_kill || s1_in.bits.tlbNoQuery
  toTlb.req.bits.pmp_addr := s1_in.bits.paddr

  /**
   * Determines whether the load/store request should be killed in s1 (`toDCache.s1_kill`).
   * Conditions:
   * 1. A kill signal is triggered in s1 (`s1_kill`).
   * 2. A real TLB miss is detected in s1 (`s1_tlbRealMiss`).
   * 3. An exception is present in s1 (`s1_hasException`).
   */
  toDCache.s1_kill := s1_kill || s1_tlbRealMiss || s1_hasException

  toDCache.s1_paddr_dup_lsu := s1_paddrDupLsu
  toDCache.s1_paddr_dup_dcache := s1_paddrDupDCache

  /**
   * Determines whether the data read request should be killed in s1 (`toDCache.s1_kill_data_read`).
   * Conditions for `toDCache.s1_kill_data_read` to be true:
   * 1. A kill signal is triggered in s1 (`s1_kill`).
   * 2. A fast TLB miss occurs, and the TLB response is valid (`fromTlb.resp.bits.fastMiss && fromTlb.resp.valid`).
   */
  toDCache.s1_kill_data_read := s1_kill || fromTlb.resp.bits.fastMiss && fromTlb.resp.valid

  if (!env.FPGAPlatform) {
    toDCache.s1_pc := s1_in.bits.uop.pc
  }

  /**
    * Send issue response to backend (only for store)
    * If the `stIssue` signal is defined for the backend, proceed with
    * the response logic.
    */
  if (params.hasStaExe) {
    toBackend.stIssue <> feedbackGen.io.toBackend.stIssue
  }

  /**
   * Determines whether there is no need to forward data in s1 (`s1_noNeedForward`).
   * Conditions:
   * 1. A TLB miss is detected (`s1_out.bits.tlbMiss`).
   * 2. The uop is a store (`s1_in.bits.isStore`).
   * 3. The uop is a prefetch (`s1_in.bits.isPrefetch`).
   * 4. An exception is present in stage 1 (`s1_hasException`).
   */
  val s1_noNeedForward = s1_out.bits.tlbMiss || s1_in.bits.isStore || s1_in.bits.isPrefetch || s1_hasException

  // to prefetch
  if (toPrefetch.train.isDefined) {
    toPrefetch.train.get.s1_prefetchSpec := s1_out.fire
  }

  if (toPrefetch.trainL1.isDefined) {
    toPrefetch.trainL1.get.s1_prefetchSpec := s1_out.fire
  }

  // to Ldu st-ld violation query
  if (toLdu.nukeQuery.isDefined) {
    val nukeQuery = toLdu.nukeQuery.get
    /**
     * Determines whether the nuke query in s1 (`s1_nukeQueryCanGo`).
     * Conditions:
     * 1. The TLB response does not indicate a miss (`!fromTlb.resp.bits.miss`).
     * 2. The uop is a store (`s1_in.bits.isStore`).
     * 3. The uop is not a hardware prefetch (`!s1_in.bits.isHWPrefetch`).
     * 4. The uop is not issued by misalignment buffer (`!s1_in.bits.isMisalignBuf`).
     */
    val s1_nukeQueryCanGo = !fromTlb.resp.bits.miss && s1_in.bits.isStore &&
      !s1_in.bits.isHWPrefetch && !s1_in.bits.isMisalignBuf

    nukeQuery.valid := s1_in.valid && s1_nukeQueryCanGo
    nukeQuery.bits.robIdx := s1_in.bits.uop.robIdx
    nukeQuery.bits.paddr := s1_paddrDupLsu
    nukeQuery.bits.mask := s1_in.bits.mask
    nukeQuery.bits.matchLine := (s1_in.bits.isVector || s1_in.bits.misalignWith16Bytes) && s1_in.bits.is128bit
  }

  // to Lsq (only for store)
  if (toLsq.addrUpdate.isDefined) {
    val addrUpdate = toLsq.addrUpdate.get
    val s1_addrUpdateCanGo = !s1_in.bits.isHWPrefetch && !s1_in.bits.isMisalignBuf
    addrUpdate.valid := s1_in.valid && s1_addrUpdateCanGo
    addrUpdate.bits := s1_out.bits
    addrUpdate.bits.miss := s1_tlbRealMiss
    addrUpdate.bits.isVector := s1_in.bits.isVector

    /**
      * Determined whether update address valid:
      * Conditions:
      * 1. The address is not misaligned or the misalignment is with 16 bytes (`!s1_in.bits.misalign || s1_in.bits.misalignWith16Bytes`).
      * 2. The uop is not in the misalignment buffer, or it is the final split (`!s1_in.bits.isMisalignBuf || s1_in.bits.isFinalSplit`).
      * 3. Alternatively, an exception is present (`s1_hasException`).
      */
    addrUpdate.bits.updateAddrValid := (!s1_in.bits.misalign || s1_in.bits.misalignWith16Bytes) &&
      (!s1_in.bits.isMisalignBuf || s1_in.bits.isFinalSplit) || s1_hasException
  }

  // assign s1_out
  s1_out.bits.vaNeedExt := s1_vaNeedExt
  s1_out.bits.isHyper := s1_isHyper
  s1_out.bits.paddr := s1_paddrDupLsu
  s1_out.bits.gpaddr := s1_gpaddrDupLsu
  s1_out.bits.isForVSnonLeafPTE := s1_isForVSnonLeafPTE
  s1_out.bits.tlbMiss := s1_tlbRealMiss
  s1_out.bits.ptwBack := s1_ptwBack
  s1_out.bits.delayedError := s1_delayedError
  s1_out.bits.misalign := exceptionGen.io.commonOut.s1_misalign
  s1_out.bits.isNoncacheable := s1_in.bits.isNoncacheable || Pbmt.isNC(s1_pbmt)
  s1_out.bits.mmio := LSUOpType.isCbo(s1_in.bits.uop.fuOpType) || Pbmt.isIO(s1_pbmt)
  s1_out.bits.atomic := LSUOpType.isCbo(s1_in.bits.uop.fuOpType) || Pbmt.isIO(s1_pbmt)

  commonOut.storePipeEmpty := s0_out.valid || s1_out.valid

  XSError(s1_in.bits.isLoad && LSUOpType.isCbo(s1_in.bits.uop.fuOpType), "Load can not be setted Cbo type!")

  // Debug
  io.debugLsInfo.s1_robIdx := s1_in.bits.uop.robIdx.value
  io.debugLsInfo.s1_isTlbFirstMiss := s1_out.fire && s1_tlbRealMiss && fromTlb.resp.bits.debug.isFirstIssue &&
    !s1_in.bits.isHWPrefetch
  io.debugLsInfo.s1_isLoadToLoadForward := s1_in.bits.isFastPath
  io.lsTopdownInfo.s1.robIdx := s1_out.bits.uop.robIdx.value
  io.lsTopdownInfo.s1.vaddr_valid := s1_out.valid && s1_out.bits.hasROBEntry
  io.lsTopdownInfo.s1.vaddr_bits := s1_out.bits.vaddr

  // perf cnt
  XSPerfAccumulate("s1_delayedError", s1_out.fire && s1_delayedError)

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_noNeedForward = RegEnable(s1_noNeedForward, s1_out.fire)
  val s2_triggerDebugMode = RegEnable(s1_triggerDebugMode, s1_out.fire)
  val s2_exceptionVec = exceptionGen.io.commonOut.s2_exceptionVecOut
  val s2_hasException = s2_triggerDebugMode || Cat(ExceptionNO.partialSelect(s2_exceptionVec, params.exceptionOut)).orR

  // from pmp
  val s2_pbmt = RegEnable(s1_pbmt, s1_out.fire)

  /**
   * Determines whether the uop in s2 is actually uncacheable (`s2_actuallyUncache`).
   * Conditions:
   * 1. The uop is a PMA (Physical Memory Attribute) uop according to the Pbmt (Page-based Memory Translation)
   *    and it is an MMIO uop (`Pbmt.isPMA(s2_pbmt) && fromPmp.mmio`).
   * 2. The uop is marked as non-cacheable (`s2_in.bits.isNoncacheable`).
   * 3. The uop is an MMIO (Memory-Mapped I/O) uop (`s2_in.bits.mmio`).
   */
  val s2_actuallyUncache = Pbmt.isPMA(s2_pbmt) && fromPmp.mmio || s2_in.bits.isNoncacheable || s2_in.bits.mmio

  /**
   * Determines whether the uop in s2 is a real MMIO uop (`s2_realMmio`).
   * Conditions:
   * 1. The uop is an MMIO uop (`s2_in.bits.mmio`).
   * 2. The uop is flagged as MMIO by the PMP (Physical Memory Protection) (`fromPmp.mmio`).
   */
  val s2_realMmio = s2_in.bits.mmio || fromPmp.mmio

  s2_out.bits.mmio := s2_realMmio
  s2_out.bits.atomic := s2_in.bits.atomic || fromPmp.atomic || s2_realMmio
  s2_out.bits.memBackTypeMM := !fromPmp.mmio

  // from tlb
  s2_out.bits.tlbHandled := !fromTlb.hint.full
  s2_out.bits.tlbId := fromTlb.hint.id

  // from dcache
  val s2_dcacheShouldResp = !(s2_in.bits.tlbMiss || s2_hasException || s2_in.bits.delayedError ||
    s2_actuallyUncache || s2_in.bits.isPrefetch)
  XSError((s2_in.valid && s2_dcacheShouldResp && !fromDCache.resp.valid), "DCache response got lost")

  /**
    * Determine whether the cache-related cause should be cleared for s2.
    * The cache-related cause is cleared if:
    * - The uop is a store (`s2_in.bits.isStore`), OR
    * - The uop is a real MMIO (memory-mapped I/O) or is non-cacheable (`s2_realMmio || s2_in.bits.isNoncacheable`).
    */
  val s2_clearCacheRelatedCause = s2_in.bits.isStore || (s2_realMmio || s2_in.bits.isNoncacheable)

  /**
    * Determine whether full forwarding is required in s2.
    * Full forwarding is enabled if the forward network indicates so and forwarding is not deemed unnecessary
    */
  val s2_fullForward = forwardNetworkCommonOut.s2_fullForward && !s2_noNeedForward

  /**
    * Determine whether there is a vaddr-paddr (VP) match failure in s2.
    * A VP match failure occurs if the forward network signals it and forwarding is not unnecessary
    */
  val s2_vpMatchFail = forwardNetworkCommonOut.s2_vpMatchFail && !s2_noNeedForward

  /**
   * Determine whether a forwarding failure in s2.
   * A forwarding failure is indicated if the data is invalid in the forward network and forwarding is not unnecessary
   */
  val s2_forwardFail = forwardNetworkCommonOut.s2_dataInvalid && !s2_noNeedForward

  /**
    * Determine whether the address is invalid in s2
    * The address is considered invalid if:
    * 1. The micro-op (uop) hits a store set,
    * 2. Forwarding is not unnecessary, and
    * 3. The forward network signals an invalid address
    */
  val s2_addrInvalid = s2_in.bits.uop.storeSetHit && !s2_noNeedForward &&
    forwardNetworkCommonOut.s2_addrInvalid

  // from DCache
  val s2_dcacheMiss = WireInit(false.B)
  val s2_mqNack = WireInit(false.B)
  val s2_bankConflict = WireInit(false.B)
  val s2_wayPredictFail = WireInit(false.B)

  if (params.hasDCacheQuery) {
    /**
      * Determine whether DCache data is needed for the current uop in stage 2 (s2)
      * The uop need dcache data if:
      * 1. Data is not being forwarded from the Miss Queue or Transaction Layer (TL)
      * 2. Full data forwarding is not available
      * 3. The uop is cacheable (not marked as non-cacheable)
      */
    val s2_needDCacheData = !s2_clearCacheRelatedCause &&
      !forwardNetworkCommonOut.s2_forwardFromMissQueueOrTL &&
      !forwardNetworkCommonOut.s2_fullForward

    /**
      * Determine whether there is a data cache miss for s2:
      * - A miss occurs when the DCache response indicates a miss (fromDCache.resp.bits.miss)
      *   and data from the DCache is required (s2_needDCacheData).
      */
    s2_dcacheMiss := fromDCache.resp.bits.miss && s2_needDCacheData

    /**
      * Determine whether there is an MQ (Miss Queue) NACK for stage s2:
      * - An MQ NACK occurs when the DCache signals a NACK (fromDCache.s2_mq_nack)
      *   and data from the DCache is required (s2_needDCacheData).
      */
    s2_mqNack := fromDCache.s2_mq_nack && s2_needDCacheData

    /**
      * Determine whether there is a bank conflict in the DCache for stage s2:
      * - A bank conflict occurs when the DCache signals a bank conflict
      *   (fromDCache.s2_bank_conflict) and data from the DCache is required (s2_needDCacheData).
      */
    s2_bankConflict := fromDCache.s2_bank_conflict && s2_needDCacheData

     /**
       * Determine whether the way prediction failed for stage s2:
       * - A way prediction failure occurs when the DCache signals a way prediction
       *   failure (fromDCache.s2_wpu_pred_fail) and data from the DCache is required (s2_needDCacheData).
       */
    s2_wayPredictFail := fromDCache.s2_wpu_pred_fail && s2_needDCacheData

    // miss queue nack
    s2_out.bits.mshrHandled := fromDCache.resp.bits.handled
    s2_out.bits.mshrId := fromDCache.resp.bits.mshr_id
    s2_out.bits.lastBeat := s2_in.bits.paddr(log2Up(refillBytes))

    // dcache miss
    s2_out.bits.miss := s2_dcacheMiss
    s2_out.bits.replayCarry := fromDCache.resp.bits.replayCarry
  }

  if (fromLsq.isDefined) {
    val forwardResp = fromLsq.get.forward
    s2_out.bits.fullForward := s2_fullForward
    s2_out.bits.dataInvalidSqIdx := forwardResp.dataInvalidSqIdx
    s2_out.bits.addrInvalidSqIdx := forwardResp.addrInvalidSqIdx
  }

  // to DCache
  if (!env.FPGAPlatform) {
    toDCache.s2_pc := s2_in.bits.uop.pc
  }
  toDCache.s2_kill := fromPmp.ld || fromPmp.st || s2_actuallyUncache || s2_kill || s2_hasException

  // to Backend
  toBackend.iqFeedback <> feedbackGen.io.toBackend.iqFeedback

  // to lsq
  if (toLsq.rarNuke.isDefined && toLsq.rawNuke.isDefined) {
    val rarNuke = toLsq.rarNuke.get

    /**
      * Determine whether a query can be made in s2
      * The query is allowed if all of the following conditions are true:
      * 1. The address is valid .
      * 2. There is no TLB miss.
      * 3. Forwarding has not failed.
      * 4. The load is not issued by misalign buffer.
      * 5. The uop is load.
      * 6. The uop is not MMIO.
      */
    val s2_canQuery = !s2_addrInvalid && !s2_in.bits.tlbMiss && !s2_forwardFail &&
      !s2_in.bits.isMisalignBuf && s2_in.bits.isLoad && !s2_realMmio

    rarNuke.req.valid := s2_in.valid && s2_canQuery
    rarNuke.req.bits.uop := s2_in.bits.uop
    rarNuke.req.bits.mask := s2_in.bits.mask
    rarNuke.req.bits.paddr := s2_in.bits.paddr
    rarNuke.req.bits.isNoncacheable := s2_in.bits.isNoncacheable

    /**
      * Determine whether the data is valid for the RAR nuke request
      * Data is considered valid if any of the following conditions are true:
      * 1. Full forwarding is enabled.
      * 2. The forward network indicates forwarding from the miss queue or TL.
      * 3. The data cache (DCache) response indicates no miss.
      */
    rarNuke.req.bits.dataValid := s2_fullForward || !fromDCache.resp.bits.miss ||
      forwardNetworkCommonOut.s2_forwardFromMissQueueOrTL

    val rawNuke = toLsq.rawNuke.get
    rawNuke.req.valid := rarNuke.req.valid
    rawNuke.req.bits := rarNuke.req.bits
  }

  if (toLsq.excpUpdate.isDefined) {
    val excpUpdate = toLsq.excpUpdate.get
    excpUpdate := s2_out.bits
    excpUpdate.af := exceptionGen.io.commonOut.s2_exceptionVecOut(storeAccessFault)
    excpUpdate.miss := fromDCache.resp.fire && fromDCache.resp.bits.miss
    excpUpdate.hasException := s2_hasException

    /**
      * The address is valid if:
      * 1. The address is not misaligned.
      * 2. The instruction is not issued by misaligned buffer, or it is the final split of a misaligned uop.
      * 3. An exception is present.
      */
    excpUpdate.updateAddrValid := !s2_in.bits.misalign &&
      (!s2_in.bits.isMisalignBuf || s2_in.bits.isFinalSplit) || s2_hasException
  }

  // to store misalign buffer
  feedbackGen.io.toStoreMisalignBuf.enq.ready := false.B
  feedbackGen.io.toStoreMisalignBuf.out.map(_.ready := false.B)
  if (toStoreMisalignBuf.isDefined) {
    // enq
    toStoreMisalignBuf.get.enq <> feedbackGen.io.toStoreMisalignBuf.enq
    // out
    if (toStoreMisalignBuf.get.out.isDefined) {
      toStoreMisalignBuf.get.out.get <> feedbackGen.io.toStoreMisalignBuf.out.get
      toStoreMisalignBuf.get.out.get.bits.cause(ReplayCauseNO.C_TM) := s2_in.bits.tlbMiss
      toStoreMisalignBuf.get.out.get.bits.uop.exceptionVec := s2_exceptionVec
    }
  }

  // to prefetch
  if (toPrefetch.train.isDefined) {
    /**
      * Determined the prefetch speculation signal (s2_prefetchSpec) for stage 2.
      * Conditions:
      * 1. The input in s2 is valid (`s2_in.valid`).
      * 2. The load is not a real MMIO (Memory-Mapped I/O) load (`!s2_realMmio`).
      * 3. One of the following conditions is true:
      *    - There is no TLB miss (`!s2_in.bits.tlbMiss`).
      *    - The uop is a hardware prefetch (`s2_in.bits.isHWPrefetch`).
      */
    toPrefetch.train.get.s2_prefetchSpec := s2_in.valid && !s2_realMmio &&
      (!s2_in.bits.tlbMiss || s2_in.bits.isHWPrefetch)

    // add performance counter
    XSPerfAccumulate("s2_prefetch", s2_out.fire && s2_out.bits.isPrefetch)
    XSPerfAccumulate("s2_prefetchIgnored", s2_out.fire && s2_out.bits.isPrefetch && fromDCache.s2_mq_nack)
    XSPerfAccumulate("s2_prefetchMiss", s2_out.fire && s2_out.bits.isPrefetch && fromDCache.resp.bits.miss)
    XSPerfAccumulate("s2_prefetchHit",  s2_out.fire && s2_out.bits.isPrefetch && !fromDCache.resp.bits.miss)
    XSPerfAccumulate("s2_prefetchAccept", s2_out.fire && s2_out.bits.isPrefetch && fromDCache.resp.bits.miss && !fromDCache.s2_mq_nack)
  }

  if (toPrefetch.trainL1.isDefined) {
    toPrefetch.trainL1.get.s2_prefetchSpec := s2_in.valid && !s2_realMmio
  }

  // misalign exception check
  val s2_misalignException = WireInit(false.B)

  /**
   * Determines whether there is a misalignment in s2 (`s2_misalign`).
   * Conditions:
   * 1. The uop is not a vector uop (`!s2_in.bits.isVector`).
   * 2. A misalignment exception is detected (`s2_misalignException`).
   * 3. The uop is not aligned to 16 bytes (`!s2_in.bits.misalignWith16Bytes`).
   * 4. There is no breakpoint exception (`!exceptionGen.io.commonOut.s2_exceptionVecOut(breakPoint)`).
   * 5. Debug mode is not triggered by a previous stage 1 trigger (`!RegEnable(s1_triggerDebugMode, false.B, s1_out.fire)`).
   * 6. The uop is not a memory-mapped I/O (MMIO) uop (`!s2_in.bits.mmio`).
   */
  val s2_misalign = !s2_in.bits.isVector && s2_misalignException && !s2_in.bits.misalignWith16Bytes &&
    !exceptionGen.io.commonOut.s2_exceptionVecOut(breakPoint) && !s2_in.bits.mmio
    !RegEnable(s1_triggerDebugMode, false.B, s1_out.fire)

  when (s2_in.bits.isStore) {
    /**
      * Determined misalignment exceptions.
      * Conditions:
      * 1. The exception generator signals a store address misalignment (`storeAddrMisaligned`).
      * 2. The misaligned store enable signal is active (`hd_misalign_st_enable`).
      */
    s2_misalignException := exceptionGen.io.commonOut.s2_exceptionVecOut(storeAddrMisaligned) &&
      GatedValidRegNext(fromCtrl.csr.hd_misalign_st_enable)

    /**
      * Determined  `s2_out.valid` signal in s2:
      * Coniditions:
      * 1. The input is valid (`s2_in.valid`).
      * 2. Either the uop is not a real MMIO uop, or there is no exception (`!s2_realMmio || !s2_hasException`).
      * 3. The uop is not a hardware prefetch (`!s2_in.bits.isHWPrefetch`).
      * 4. The uop is not marked as misaligned in the buffer (`!s2_in.bits.isMisalignBuf`).
      * 5. There is no detected misalignment (`!s2_misalign`).
      */
    s2_out.valid := s2_in.valid && (!s2_realMmio || !s2_hasException) && !s2_in.bits.isHWPrefetch &&
      !s2_in.bits.isMisalignBuf && !s2_misalign
  } .otherwise {
    /**
      * Determined misalignment exceptions.
      * Conditions:
      * 1. The exception generator signals a load address misalignment (`storeAddrMisaligned`).
      * 2. The misaligned load enable signal is active (`hd_misalign_ld_enable`).
      */
    s2_misalignException := exceptionGen.io.commonOut.s2_exceptionVecOut(loadAddrMisaligned) &&
      GatedValidRegNext(fromCtrl.csr.hd_misalign_ld_enable)
    s2_out.valid := s2_in.valid && !s2_in.bits.isHWPrefetch
  }

  val s2_canFastReplay = replayCauseGen.io.commonOut.s2_canFastReplay

  /**
   * Determines whether a safe wakeup condition is met for s2 (`s2_safeWakeup`).
   * The uop can safe wakeup if:
   * 1. There is no need for replay according to the replay cause generator (`!s2_needReplay`).
   * 2. There is an exception present in s2 (`s2_hasException`).
   * 3. The uop is not a real MMIO (Memory-Mapped I/O) load (`!s2_realMmio`).
   * 4. There is no misalignment(`!s2_misalign`).
   */
  val s2_safeWakeup = !replayCauseGen.io.commonOut.s2_needReplay && s2_hasException && !s2_realMmio && !s2_misalign

  /**
   * Determines whether a safe writeback condition is met for s2 (`s2_safeWriteback`).
   * The uop can be safe writebacked if:
   * 1. The safe wakeup condition is set(`s2_safeWakeup`).
   * 2. There is a vaddr-paddr (VP) match failure (`s2_vpMatchFail`).
   * 3. A misalignment requires a wakeup (`s2_in.bits.misalignNeedWakeUp`).
   */
  val s2_safeWriteback = s2_safeWakeup || s2_vpMatchFail || s2_in.bits.misalignNeedWakeUp

  // set out uop
  when (s2_in.bits.isLoadReplay || s2_in.bits.isFastReplay) {
    s2_out.bits.uop.vpu.vstart := s2_in.bits.uop.vpu.vstart
  }

  /**
    * Determine whether a load uop can proceed to s3.
    * A load can proceed if it is not a hardware prefetch (`s2_out.bits.isHWPrefetch`).
    */
  val s2_loadCanGoToS3 = !s2_out.bits.isHWPrefetch

  /**
    * Determine whether a store uop can proceed to s3.
    * A store can go to s3 if:
    * - It is not a hardware prefetch (`!s2_out.bits.isHWPrefetch`),
    * - It is either not a real MMIO (`!s2_realMmio`) or there is an exception (`s2_hasException`),
    * - It is not misalign (`!s2_in.bits.misalign`),
    * - It is not issued by misalign buffer (`!s2_in.bits.isMisalignBuf`).
    */
  val s2_storeCanGoToS3 = !s2_out.bits.isHWPrefetch && (!s2_realMmio || s2_hasException) &&
    !s2_in.bits.misalign && !s2_in.bits.isMisalignBuf

  s2_out.valid := s2_in.valid && (s2_loadCanGoToS3 || s2_storeCanGoToS3)

  // Debug
  io.debugLsInfo.s2_robIdx := s2_out.bits.uop.robIdx.value
  io.debugLsInfo.s2_isBankConflict := s2_out.fire
  io.debugLsInfo.s2_isDcacheFirstMiss := s2_out.fire
  io.debugLsInfo.s2_isForwardFail := s2_out.fire

  // Topdown
  io.lsTopdownInfo.s2.robIdx := s2_out.bits.uop.robIdx.value
  io.lsTopdownInfo.s2.paddr_valid := s2_out.fire && s2_out.bits.hasROBEntry && !s2_out.bits.tlbMiss
  io.lsTopdownInfo.s2.paddr_bits := s2_out.bits.paddr
  io.lsTopdownInfo.s2.first_real_miss := fromDCache.resp.bits.miss
  io.lsTopdownInfo.s2.cache_miss_en := s2_out.fire && s2_out.bits.hasROBEntry && !s2_out.bits.tlbMiss && !s2_out.bits.missDbUpdated

  XSPerfAccumulate("s2_dcacheMiss", s2_out.fire && fromDCache.resp.bits.miss)
  XSPerfAccumulate("s2_dcacheFirstMiss", s2_out.fire && s2_out.bits.miss && s2_in.bits.isFirstIssue)
  XSPerfAccumulate("s2_dcacheRealFirstMiss", s2_out.fire&& fromDCache.resp.bits.miss && s2_in.bits.isFirstIssue)
  XSPerfAccumulate("s2_fullForward", s2_out.fire && s2_fullForward)
  XSPerfAccumulate("s2_fullForwardButDCacheMiss", s2_out.fire && fromDCache.resp.bits.miss && s2_fullForward)

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  // writeback and update
  val s3_hasException = RegEnable(s2_hasException, false.B, s2_out.fire)
  val s3_vpMatchFail = RegEnable(s2_vpMatchFail, false.B, s2_out.fire)
  val s3_canFastReplay = RegEnable(s2_canFastReplay, false.B, s2_out.fire)
  val s3_mmioWbPort = RegNextN(s0_wbPort, 3, Some(0.U.asTypeOf(s0_wbPort)))

  // update replay causes
  val s3_replayCause = ReplayCauseNO.highestSelect(replayCauseGen.io.replayCauseOut)
  val s3_misalignReplayCause = Mux(s3_in.bits.misalignNeedWakeUp, 0.U.asTypeOf(s3_replayCause), s3_replayCause)

  /**
    * Determine whether any replay should be cancelled.
    * - `s3_replayIssues`: A collection of replay issues detected at s3.
    * - `s3_replayCancelled`: Indicates whether any replay uop is valid and forwarded via the TL D channel.
    *   This is determined by checking whether any issue in `s3_replayIssues` has both a valid signal and
    *   the `forwardTLDchannel` flag set.
    */
  val s3_replayIssues = getReplayIssues()
  val s3_replayCancelled = WireInit(false.B)
  if (s3_replayIssues.nonEmpty) {
    s3_replayCancelled := s3_replayIssues.map(iss => iss.valid && iss.bits.forwardTLDchannel).reduce(_||_)
  }

  /**
    * Determine whether any misalign buffer issued should be cancelled.
    * - `s3_misalignIssues`: A collection of misalignment issues detected at s3.
    * - `s3_misalignCancelled`: Indicates whether any misalignment issue is valid.
    *   This is determined by checking whether any issue in `s3_misalignIssues` has a valid signal.
    */
  val s3_misalignIssues = getMisalignIssues()
  val s3_misalignCancelled = WireInit(false.B)
  if (s3_misalignIssues.nonEmpty) {
    s3_misalignCancelled := s3_misalignIssues.map(iss => iss.valid).reduce(_||_)
  }

  /**
    * Determine whether fast replay is cancelled at s3.
    * Conditions:
    *   1. Replay-related issues are cancelled (`s3_replayCancelled`).
    *   2. Misalign-related issues are cancelled (`s3_misalignCancelled`).
    *   3. The DCache request is not ready (`!toDCache.req.ready`), which prevents fast replay.
    */
  val s3_fastReplayCanclled = s3_replayCancelled || s3_misalignCancelled || !toDCache.req.ready

  val s3_flushPipe = WireInit(false.B)
  if (fromLsq.isDefined) {
    val rarNuke = fromLsq.get.rarNuke
    /**
      * Determine whether a RAR (Read After Read) nuke is defined for s3.
      * The uop should flush pipeline if:
      * - The RAR nuke response is valid,
      * - It indicates need replay this uop,
      * - The load violation check is enabled (controlled by `csr.ldld_vio_check_enable`).
      */
    s3_flushPipe := rarNuke.valid && rarNuke.bits.replayInst &&
      GatedValidRegNext(fromCtrl.csr.ldld_vio_check_enable)
  }
  val s3_replayInst = s3_vpMatchFail

  /**
   * s3_fromMisalignFlush is a signal that checks if the current uop(s3_in) is a load
   * and originates from the misalign buffer (isMisalignBuf). This signal is used to detect
   * if we need to handle misaligned load uop.
   * - isMisalignBuf: Indicates whether the uop is issued by misalign buffer.
   * - isLoad: Indicates whether the uop is a load.
   * - C_FF: daa forward fail(C_FF), indicating that the data invalid from forwarding.
   * - C_MA: memory ambiguous(C_MA), indicating that the MDP predict a dependency.
   * - C_NK: nuke (C_NK), indicating a st-ld nuke.
   *
   */
  val s3_fromMisalignFlush = s3_in.bits.isMisalignBuf && s3_in.bits.isLoad
    (s3_misalignReplayCause(ReplayCauseNO.C_FF) ||
     s3_misalignReplayCause(ReplayCauseNO.C_MA) ||
     s3_misalignReplayCause(ReplayCauseNO.C_NK))

  if (toLsq.out.isDefined) {
    /**
      * Determine whether the output cause to LSQ (Load-Store Queue) for s3.
      * The replay cause should be cleared if:
      * - Has exceptions,
      * - It indicates need replay this uop.
      * - It indicates need replay uops, which younger than this uop,
      * - It indicates this is a misalign uop.
      *
      * Otherwise, the cause is set to the replay cause.
      */

    when (s3_hasException || s3_replayInst || s3_flushPipe || s3_in.bits.isMisalignBuf) {
      toLsq.out.get.bits.cause := 0.U.asTypeOf(s3_out.bits.cause)
    } .otherwise {
      toLsq.out.get.bits.cause := s3_replayCause
    }
  }

  // to Backend
  if (params.hasLdExe) {
    toBackend.ldCancel <> feedbackGen.io.toBackend.ldCancel
  }

  // to fast replay
  feedbackGen.io.toLdu.replay.ready := false.B
  if (toLdu.replay.isDefined) {
    toLdu.replay.get <> feedbackGen.io.toLdu.replay
  }

  // to lsq
  feedbackGen.io.toLsq.ready := true.B
  if (toLsq.out.isDefined) {
    toLsq.out.get <> feedbackGen.io.toLsq

    val s3_revoke = (s3_hasException || toLsq.out.get.bits.needReplay && s3_in.bits.misalign) && s3_in.bits.isLoad
    if (toLsq.rarNuke.isDefined) {
      toLsq.rarNuke.get.revoke := s3_revoke
    }
    if (toLsq.rawNuke.isDefined) {
      toLsq.rawNuke.get.revoke := s3_revoke
    }

    io.debugLsInfo.s3_isReplay := toLsq.out.get.fire && toLsq.out.get.bits.needReplay && s3_in.bits.isLoad
    io.debugLsInfo.replayCause := toLsq.out.get.bits.cause
    io.debugLsInfo.replayCnt := 1.U
  } else {
    io.debugLsInfo.s3_isReplay := false.B
    io.debugLsInfo.replayCause := ReplayCauseNO(false.B)
    io.debugLsInfo.replayCnt := 0.U
  }

  // to Backend rollback
  if (params.hasLdExe) {
    toBackend.rollback <> feedbackGen.io.toBackend.rollback
  }

  // to load misalign buffer
  feedbackGen.io.toLoadMisalignBuf.enq.ready := false.B
  feedbackGen.io.toLoadMisalignBuf.out.map(_.ready := false.B)
  if (toLoadMisalignBuf.isDefined) {
    // enq
    toLoadMisalignBuf.get.enq <> feedbackGen.io.toLoadMisalignBuf.enq
    // data out

    if (toLoadMisalignBuf.get.out.isDefined) {
      toLoadMisalignBuf.get.out.get <> feedbackGen.io.toLoadMisalignBuf.out.get
      toLoadMisalignBuf.get.out.get.bits.data := 0.U
      toLoadMisalignBuf.get.out.get.bits.cause := s3_misalignReplayCause
    }
  }

  // to prefetch
  if (toPrefetch.train.isDefined) {
    val toPrfTrain = toPrefetch.train.get
    val s2_pfTrainValid = s2_in.valid && !s2_actuallyUncache && (!s2_in.bits.tlbMiss || s2_in.bits.isHWPrefetch)
    toPrfTrain.req.valid := GatedValidRegNext(s2_pfTrainValid)
    toPrfTrain.req.bits.fromLsPipelineBundle(s2_in.bits, latch = true, enable = s2_pfTrainValid)
    toPrfTrain.req.bits.miss := RegEnable(fromDCache.resp.bits.miss, s2_pfTrainValid)
    toPrfTrain.req.bits.metaPrefetch := RegEnable(fromDCache.resp.bits.meta_prefetch, s2_pfTrainValid)
    toPrfTrain.req.bits.metaAccess := RegEnable(fromDCache.resp.bits.meta_access, s2_pfTrainValid)
  }

  if (toPrefetch.trainL1.isDefined) {
    val toPrfTrainL1 = toPrefetch.trainL1.get
    val s2_pfTrainL1Valid = s2_in.valid && !s2_actuallyUncache
    toPrfTrainL1.req.valid := GatedValidRegNext(s2_pfTrainL1Valid)
    toPrfTrainL1.req.bits.fromLsPipelineBundle(s2_in.bits, latch = true, enable = s2_pfTrainL1Valid)
    toPrfTrainL1.req.bits.miss := RegEnable(fromDCache.resp.bits.miss, s2_pfTrainL1Valid)
    toPrfTrainL1.req.bits.metaPrefetch := RegEnable(fromDCache.resp.bits.meta_prefetch, s2_pfTrainL1Valid)
    toPrfTrainL1.req.bits.metaAccess := RegEnable(fromDCache.resp.bits.meta_access, s2_pfTrainL1Valid)
  }

  // Debug
  io.debugLsInfo.s3_robIdx := s3_out.bits.uop.robIdx.value
  io.debugLsInfo.s3_isReplayFast := s3_out.fire && s3_out.bits.isFastReplay
  io.debugLsInfo.s3_isReplayRS := s3_out.fire && s3_out.bits.isIq
  io.debugLsInfo.s3_isReplaySlow := s3_out.fire && s3_out.bits.isLoadReplay

  // Modules conntion
  // Exception generator
  exceptionGen.io.fromCtrl.csr <> fromCtrl.csr
  exceptionGen.io.fromPmp <> fromPmp
  exceptionGen.io.fromDCache.error_delayed <> fromDCache.resp.bits.error_delayed
  exceptionGen.io.fromTrigger.breakPoint := s1_triggerBreakpoint
  Connection.connect(
    sink        = exceptionGen.io.s0_in,
    source      = s0_selOut,
    connectFn   = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      sink.bits.vaddr := s0_dcacheVAddr
    }),
    connectName = "exceptionGen s0"
  )
  Connection.connect(exceptionGen.io.s1_in, s1_in, None, "exceptionGen s1")
  Connection.connect(exceptionGen.io.s2_in, s2_in, None, "exceptionGen s2")
  Connection.connect(exceptionGen.io.s3_in, s3_in, None, "exceptionGen s3")
  Connection.connect(exceptionGen.io.fromTlb, fromTlb.resp, None, "exceptionGen fromTlb")

  // Feedback generator
  feedbackGen.io.fromCtrl.redirect <> fromCtrl.redirect
  feedbackGen.io.fromCtrl.trigger := DontCare
  feedbackGen.io.fromCtrl.trigger.debugMode := s1_triggerDebugMode
  feedbackGen.io.fromPmp <> fromPmp
  feedbackGen.io.toBackend.writeback <> toBackend.writeback
  Connection.connect(feedbackGen.io.fromTlb, fromTlb.resp, None, "feedbackGen fromTlb")
  Connection.connect(feedbackGen.io.s0_in, s0_out, None,"feedbackGen s0")
  Connection.connect(feedbackGen.io.s1_in, s1_in, None, "feedbackGen s1")
  Connection.connect(
    sink      = feedbackGen.io.s2_in,
    source    = s2_in,
    connectFn = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      sink.bits.misalign := s2_misalign
    }),
    connectName = "feedbackGen s2"
  )
  Connection.connect(
    sink      = feedbackGen.io.s3_in,
    source    = s3_in,
    connectFn = Some((sink: DecoupledIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      sink.bits.data := 0.U
      sink.bits.cause := s3_misalignReplayCause
      sink.bits.uop.exceptionVec := exceptionGen.io.commonOut.s3_exceptionVecOut
    }),
    connectName = "feedbackGen s3"
  )

  // common in ports
  feedbackGen.io.commonIn := DontCare
  feedbackGen.io.commonIn.s3_wbPort := Mux(s3_in.valid, s3_wbPort, s3_mmioWbPort)
  feedbackGen.io.commonIn.s3_canFastReplay := s3_canFastReplay
  feedbackGen.io.commonIn.s3_fastReplayCancelled := s3_fastReplayCanclled
  feedbackGen.io.commonIn.s3_replayInst := s3_replayInst
  feedbackGen.io.commonIn.s3_flushPipe := s3_flushPipe
  feedbackGen.io.commonIn.s3_fromMisalignFlush := s3_fromMisalignFlush
  feedbackGen.io.commonIn.s3_hasException := s3_hasException

  if (fromLsq.isDefined) {
    feedbackGen.io.fromLsq.mmioLdWriteback <> fromLsq.get.mmioLdWriteback
  } else {
    feedbackGen.io.fromLsq.mmioLdWriteback := DontCare
  }

  // Forward network
  forwardNetwork.foreach {
    case mod =>
      Connection.connect(mod.io.s0_in, s0_out, None, "forwardNetwork s0")
      Connection.connect(mod.io.s1_in, s1_in, None, "forwardNetwork s1")
      Connection.connect(mod.io.s2_in, s2_in, None, "forwardNetwork s2")
      Connection.connect(mod.io.s3_in, s3_in, None, "forwardNetwork s3")

      // data from DCache
      mod.io.fromDCache := 0.U.asTypeOf(mod.io.fromDCache.cloneType)
      if (params.hasLdExe) {
        mod.io.fromDCache := fromDCache.resp
      }

      // data from StoreQueue
      mod.io.fromLsq := 0.U.asTypeOf(mod.io.fromLsq.cloneType)
      if (toLsq.forward.isDefined) {
        toLsq.forward.get <> mod.io.toLsq
        mod.io.fromLsq.forward <> fromLsq.get.forward
        mod.io.fromLsq.mmioLdData := fromLsq.get.mmioLdData
      }
      // data from SBuffer
      mod.io.fromSBuffer := 0.U.asTypeOf(mod.io.fromSBuffer.cloneType)
      if (toSBuffer.isDefined) {
        toSBuffer.get <> mod.io.toSBuffer
        mod.io.fromSBuffer <> fromSBuffer.get
      }
      // data from Uncache buffer
      mod.io.fromUncache := 0.U.asTypeOf(mod.io.fromUncache.cloneType)
      if (toUncache.isDefined) {
        toUncache.get <> mod.io.toUncache
        mod.io.fromUncache <> fromUncache.get
      }
      // data from MissQueue
      mod.io.fromMissQueue := 0.U.asTypeOf(mod.io.fromMissQueue.cloneType)
      if (toMissQueue.isDefined) {
        toMissQueue.get <> mod.io.toMissQueue
        mod.io.fromMissQueue <> fromMissQueue.get
      }
      // data from TL D channel
      mod.io.fromTL := 0.U.asTypeOf(mod.io.fromTL.cloneType)
      if (fromTL.isDefined) {
        mod.io.fromTL <> fromTL.get
      }

      forwardNetworkCommonOut := mod.io.commonOut
      feedbackGen.io.commonIn.s3_data := mod.io.dataOut
      feedbackGen.io.commonIn.s3_misalignData := mod.io.misalignDataOut
      feedbackGen.io.commonIn.s3_vecData := mod.io.vecDataOut
  }

  // Replay cause generator
  Connection.connect(
    sink        = replayCauseGen.io.s1_in,
    source      = s1_in,
    connectFn   = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      sink.bits.paddr := s1_paddrDupLsu
    }),
    connectName = "replayCauseGen s1"
  )

  Connection.connect(
    sink        = replayCauseGen.io.s2_in,
    source      = s2_in,
    connectFn   = Some((sink: ValidIO[LsPipelineBundle], source: DecoupledIO[LsPipelineBundle]) => {
      sink.valid := source.valid
      sink.bits := source.bits
      sink.bits.isNoncacheable := s2_actuallyUncache
    }),
    connectName = "replayCauseGen s2"
  )

  Connection.connect(replayCauseGen.io.s3_in, s3_in, None, "replayCauseGen s3")
  Connection.connect(replayCauseGen.io.fromTlb, fromTlb.resp, None, "replayCauseGen fromTlb")
  if (fromSta.isDefined) {
    replayCauseGen.io.fromSta <> fromSta.get
  } else {
    replayCauseGen.io.fromSta := DontCare
  }

  /**
   * Assign Miss Queue NACK signal
   * Indicates a NACK (negative acknowledgment) from the Miss Queue (s2_mq_nack) and whether DCache data is needed.
   */
  replayCauseGen.io.fromDCache.mqNack := s2_mqNack

  /**
   * Assign DCache miss signal
   * Indicates a cache miss in the current uop (fromDCache.resp.miss), and whether DCache data is needed.
   */
  replayCauseGen.io.fromDCache.miss := s2_dcacheMiss

  /**
    * Assign bank conflict signal
    * Indicates a bank conflict in the DCache (s2_bank_conflict) and whether DCache data is needed.
    */
  replayCauseGen.io.fromDCache.bankConflict := s2_bankConflict

  /**
   * Assign way prediction failure signal
   * Indicates a failure in way prediction during DCache access (s2_wpu_pred_fail) and whether DCache data is needed.
   */
  replayCauseGen.io.fromDCache.wayPredictFail := s2_wayPredictFail

  /**
    * Assign address invalid signal
    * Determine whether the address is invalid in s2.
    */
  replayCauseGen.io.fromLsq.addrInvalid := s2_addrInvalid

  /**
    * Assign data invalid signal
    * Determine whether data forwarding resulted in invalid data, considering whether forwarding is required.
    */
  replayCauseGen.io.fromLsq.dataInvalid := forwardNetworkCommonOut.s2_dataInvalid && !s2_noNeedForward

  /**
    * Assign RAR (Read-After-Read) NACK signal
    * Determine whether the RAR nuke enqueue request can be accepted, default is false.
    */
  if (toLsq.rarNuke.isDefined) {
    replayCauseGen.io.fromLsq.rarNack := toLsq.rarNuke.get.req.valid && !toLsq.rarNuke.get.req.ready
  } else {
    replayCauseGen.io.fromLsq.rarNack := false.B
  }

  /**
    * Assign RAW (Read-After-Write) NACK signal
    * Determine whether the RAW enqueue request can be accepted, default is false.
    */
  if (toLsq.rawNuke.isDefined) {
    replayCauseGen.io.fromLsq.rawNack := toLsq.rawNuke.get.req.valid && !toLsq.rawNuke.get.req.ready
  } else {
    replayCauseGen.io.fromLsq.rawNack := false.B
  }

  /**
    * Assign misalignment buffer NACK signal
    * Determine whether the load misalignment buffer enqueue request can be accepted, default is false.
    */
  if (toStoreMisalignBuf.isDefined) {
    replayCauseGen.io.fromLsq.misalignBufNack := toStoreMisalignBuf.get.enq.valid && !toStoreMisalignBuf.get.enq.ready
  } else {
    replayCauseGen.io.fromLsq.misalignBufNack := false.B
  }
}
