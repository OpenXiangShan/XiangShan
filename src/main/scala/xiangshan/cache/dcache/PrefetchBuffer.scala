/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan.L1CacheErrorInfo

class PrefetchBuffer(implicit p: Parameters) extends DCacheModule {
  require(PBEntries > 0)
  require(cfg.blockBytes * 8 % VLEN == 0)
  private val entryCount = PBEntries
  private val lineRows = cfg.blockBytes * 8 / DCacheSRAMRowBits
  private val windowRows = VLEN / DCacheSRAMRowBits

  val io = IO(new Bundle {
    val load = Vec(LoadPipelineWidth, new PBLoadIO)
    val mshr = new PBMSHRIO
    val pipe = new PBPipeIO
    val releaseReq = Decoupled(new WritebackReq)
    val dcache = new PBDCacheIO
  })

  // Meta & Data
  val entryMeta = RegInit(VecInit(Seq.fill(entryCount)(0.U.asTypeOf(new PBMeta))))
  val entryData = Reg(Vec(entryCount, Vec(lineRows, UInt(DCacheSRAMRowBits.W))))

  val entryReleased = VecInit(entryMeta.map(_.state === PBState.released))
  val entryOccupied = VecInit(entryMeta.map(_.state =/= PBState.invalid))
  val entryVisible = VecInit(entryMeta.map(m =>
    m.state =/= PBState.invalid && m.state =/= PBState.released))
  val entryReserved = VecInit(entryMeta.map(_.state === PBState.reserved))
  val entryLocked = VecInit(entryMeta.map(m =>
    m.state === PBState.probeLocked || m.state === PBState.moveLocked))
  val entryReadable = VecInit(entryMeta.map(m =>
    m.state === PBState.resident && !m.dataBad && cleanPermission(m.coh)))

  // functions
  def blockAddr(addr: UInt): UInt = addr(PAddrBits - 1, blockOffBits)
  def blockAlignedAddr(addr: UInt): UInt = Cat(blockAddr(addr), 0.U(blockOffBits.W))
  def cleanPermission(coh: ClientMetadata): Bool =
    coh.state === ClientStates.Branch || coh.state === ClientStates.Trunk
  def selectWindow(data: Vec[UInt], addr: UInt): UInt =
    VecInit(data.grouped(windowRows).map(rows => VecInit(rows).asUInt).toSeq)(
      addr(blockOffBits - 1, log2Up(VLEN / 8)))
  def matchBlock(paddr: UInt): Vec[Bool] = {
    val matchOH = VecInit(entryMeta.indices.map(i =>
      entryVisible(i) && blockAddr(entryMeta(i).paddr) === blockAddr(paddr)))
    assert(PopCount(matchOH) <= 1.U, "PB physical block must match at most one entry")
    matchOH
  }

  // ========================================================================
  // alloc & refill
  // ========================================================================
  val freeEntryOH = VecInit(entryOccupied.map(occupied => !occupied)).asUInt
  val allocId = PriorityEncoder(freeEntryOH)
  val allocReady = freeEntryOH.orR
  val allocFire = io.mshr.allocReq.valid && allocReady
  val allocBlocked = io.mshr.allocReq.valid && !allocReady

  val refillId = io.mshr.refillReq.bits.entryId
  val refillMeta = entryMeta(refillId)
  val refillReady = refillMeta.state === PBState.reserved && refillMeta.missEntryId === io.mshr.refillReq.bits.missEntryId
  val refillFire = io.mshr.refillReq.valid && refillReady
  val refillDenied = io.mshr.refillReq.bits.denied
  val refillCorrupt = io.mshr.refillReq.bits.corrupt
  val refillData = VecInit((0 until lineRows).map { row =>
    io.mshr.refillReq.bits.data((row + 1) * DCacheSRAMRowBits - 1, row * DCacheSRAMRowBits)
  })

  // ========================================================================
  // Load
  // ========================================================================
  val loadS1Valid = Wire(Vec(LoadPipelineWidth, Bool()))
  val loadS1Match = Wire(Vec(LoadPipelineWidth, Bool()))
  val loadS1Hit = Wire(Vec(LoadPipelineWidth, Bool()))
  val loadS1Retry = Wire(Vec(LoadPipelineWidth, Bool()))
  val loadS1EntryId = Wire(Vec(LoadPipelineWidth, UInt(PBIdBits.W)))
  val loadS1Offset = Wire(Vec(LoadPipelineWidth, UInt(blockOffBits.W)))

  for (i <- 0 until LoadPipelineWidth) {
    val port = io.load(i)
    val queryValid = port.s1_paddr.valid && !port.s1_kill
    val matchOH = matchBlock(port.s1_paddr.bits)
    val readable = matchOH.zip(entryReadable).map { case (m, r) => m && r }.reduce(_ || _)
    val hasOwner = matchOH.zip(entryReserved).map { case (m, r) => m && !r }.reduce(_ || _)
    loadS1Valid(i) := queryValid
    loadS1Match(i) := queryValid && matchOH.asUInt.orR
    loadS1Hit(i) := queryValid && readable
    loadS1Retry(i) := queryValid && hasOwner && !readable
    loadS1EntryId(i) := OHToUInt(matchOH)
    loadS1Offset(i) := port.s1_paddr.bits(blockOffBits - 1, 0)
  }

  val loadS2Valid = RegInit(VecInit(Seq.fill(LoadPipelineWidth)(false.B)))
  val loadS2Hit = Reg(Vec(LoadPipelineWidth, Bool()))
  val loadS2Retry = Reg(Vec(LoadPipelineWidth, Bool()))
  val loadS2EntryId = Reg(Vec(LoadPipelineWidth, UInt(PBIdBits.W)))
  val loadS2Offset = Reg(Vec(LoadPipelineWidth, UInt(blockOffBits.W)))
  val loadS2Resp = Wire(Vec(LoadPipelineWidth, new PBLoadResp))

  for (i <- 0 until LoadPipelineWidth) {
    loadS2Valid(i) := loadS1Valid(i)
    when (loadS1Valid(i)) {
      loadS2Hit(i) := loadS1Hit(i)
      loadS2Retry(i) := loadS1Retry(i)
      loadS2EntryId(i) := loadS1EntryId(i)
      loadS2Offset(i) := loadS1Offset(i)
    }
  }

  for (i <- 0 until LoadPipelineWidth) {
    val entryId = loadS2EntryId(i)
    val meta = entryMeta(entryId)
    val data = selectWindow(entryData(entryId), loadS2Offset(i))
    loadS2Resp(i).hit := loadS2Hit(i)
    loadS2Resp(i).retry := loadS2Retry(i)
    loadS2Resp(i).data := Mux(loadS2Hit(i), data, 0.U)
    loadS2Resp(i).prefetchSource := meta.prefetchSource
  }

  val entryUsedNow = Wire(Vec(entryCount, Bool()))
  for (i <- 0 until entryCount) {
    val loadUsesEntry = VecInit((0 until LoadPipelineWidth).map(lane =>
      io.load(lane).s2_use && loadS2Valid(lane) && loadS2Hit(lane) &&
        loadS2EntryId(lane) === i.U)).asUInt.orR
    entryUsedNow(i) := entryVisible(i) && !entryReserved(i) && loadUsesEntry
  }

  // ========================================================================
  // Move/Release
  // ========================================================================
  val moveSelValid = RegInit(false.B)
  val moveSelId = RegInit(0.U(PBIdBits.W))
  val relSelValid = RegInit(false.B)
  val relSelId = RegInit(0.U(PBIdBits.W))

  val entryProbeLock = Wire(Vec(entryCount, Bool()))
  val entryStoreLock = Wire(Vec(entryCount, Bool()))

  val moveRelEnable = !io.dcache.wfi.wfiReq || allocBlocked
  val entryNeedMove = VecInit(entryMeta.indices.map(i =>
    entryReadable(i) && entryMeta(i).movePending
  ))
  val entryNeedRel = VecInit(entryMeta.indices.map { i =>
    val meta = entryMeta(i)
    !entryNeedMove(i) && (meta.state === PBState.poison ||
      meta.state === PBState.resident && allocBlocked)
  })

  val entrySelected = VecInit(entryMeta.indices.map(i =>
    moveSelValid && moveSelId === i.U || relSelValid && relSelId === i.U))
  val entryCanSelect = VecInit(entryMeta.indices.map(i => moveRelEnable &&
    !entrySelected(i) && !entryProbeLock(i) && !entryStoreLock(i)))

  // Move
  val moveArb = Module(new RRArbiter(UInt(PBIdBits.W), entryCount))
  val moveMask = VecInit(entryMeta.indices.map(i => entryCanSelect(i) && entryNeedMove(i)))
  val moveSelReady = Wire(Bool())

  for (i <- entryMeta.indices) {
    moveArb.io.in(i).valid := moveMask(i)
    moveArb.io.in(i).bits := i.U
  }
  moveArb.io.out.ready := moveSelReady

  val moveS0Valid = moveSelValid && entryNeedMove(moveSelId) && moveRelEnable
  val moveS0Fire = moveS0Valid && io.pipe.s0_moveReq.ready
  val moveTaken = entryProbeLock(moveSelId) || entryStoreLock(moveSelId)
  val moveSelFire = moveSelReady && moveArb.io.out.valid
  val entryMoveLock = VecInit(entryMeta.indices.map(i => moveS0Fire && moveSelId === i.U))

  moveSelReady := !moveSelValid || !moveS0Valid || moveS0Fire || moveTaken
  
  when (moveSelReady) { moveSelValid := moveArb.io.out.valid }
  when (moveSelFire) { moveSelId := moveArb.io.out.bits }

  // Release
  val relArb = Module(new RRArbiter(UInt(PBIdBits.W), entryCount))
  val relMask = VecInit(entryMeta.indices.map(i => entryCanSelect(i) && entryNeedRel(i)))
  val relSelReady = Wire(Bool())

  for (i <- entryMeta.indices) {
    relArb.io.in(i).valid := relMask(i)
    relArb.io.in(i).bits := i.U
  }
  relArb.io.out.ready := relSelReady

  val relValid = relSelValid && entryNeedRel(relSelId) && moveRelEnable
  val relFire = relValid && io.releaseReq.ready
  val relTaken = entryProbeLock(relSelId) || entryStoreLock(relSelId)
  val relSelFire = relSelReady && relArb.io.out.valid

  relSelReady := !relSelValid || !relValid || relFire || relTaken

  when (relSelReady) { relSelValid := relArb.io.out.valid }
  when (relSelFire) { relSelId := relArb.io.out.bits }

  val relData = entryData(relSelId)
  val relDataBad = entryMeta(relSelId).dataBad
  val relReq = WireInit(0.U.asTypeOf(new WritebackReq))
  relReq.addr := entryMeta(relSelId).paddr
  relReq.param := Mux(entryMeta(relSelId).coh.state === ClientStates.Trunk,
    TLPermissions.TtoN, TLPermissions.BtoN)
  relReq.voluntary := true.B
  relReq.hasData := dcacheParameters.alwaysReleaseData.B
  relReq.data := (if (dcacheParameters.alwaysReleaseData) relData.asUInt else 0.U)
  relReq.corrupt := dcacheParameters.alwaysReleaseData.B && relDataBad

  // ========================================================================
  // MainPipe
  // ========================================================================
  val pipeS1IsProbe = RegInit(false.B)
  val pipeS1IsMove = RegInit(false.B)
  val pipeS1MoveId = RegInit(0.U(PBIdBits.W))
  val probeS1Resp = RegInit(0.U.asTypeOf(new PBProbeResp))
  val pipeS2Valid = RegInit(false.B)
  val storeS2Valid = RegInit(false.B)
  val storeS2Locked = RegInit(false.B)
  val pipeS2Resp = RegInit(0.U.asTypeOf(new PBPipeDataResp))
  val pipeS1Fire = Wire(Bool())

  // S0
  val probeS0Fire = io.pipe.s0_probeReq.valid
  val probeS0MatchOH = VecInit(matchBlock(io.pipe.s0_probeReq.bits).zip(entryReserved).map {
    case (hit, reserved) => hit && !reserved
  })
  val probeS0EntryId = OHToUInt(probeS0MatchOH)
  val probeS0AlreadyLocked = (probeS0MatchOH.asUInt & entryLocked.asUInt).orR
  val probeS0Lock = probeS0Fire && probeS0MatchOH.asUInt.orR &&
    !probeS0AlreadyLocked && !(relFire && probeS0MatchOH(relSelId))

  entryProbeLock := VecInit(entryMeta.indices.map(i => probeS0Lock && probeS0MatchOH(i)))

  when (moveS0Fire) {
    pipeS1IsMove := true.B
    pipeS1MoveId := moveSelId
  }.elsewhen (pipeS1Fire) {
    pipeS1IsMove := false.B
  }

  when (probeS0Fire) {
    pipeS1IsProbe := true.B
    probeS1Resp.locked := probeS0Lock
    probeS1Resp.entryId := probeS0EntryId
    probeS1Resp.coh := Mux(probeS0Lock, entryMeta(probeS0EntryId).coh, 0.U.asTypeOf(new ClientMetadata))
  }.elsewhen (pipeS1Fire) {
    pipeS1IsProbe := false.B
  }

  // S1
  val pipeS1Query = io.pipe.s1_paddr.valid
  val pipeS1MatchOH = matchBlock(io.pipe.s1_paddr.bits)
  val pipeS1MatchId = OHToUInt(pipeS1MatchOH)
  val pipeS1Hit = pipeS1Query && !pipeS1IsProbe && !pipeS1IsMove &&
    pipeS1MatchOH.zip(entryReadable).map { case (m, r) => m && r }.reduce(_ || _)
  val pipeS1HasOwner = pipeS1MatchOH.zip(entryReserved).map { case (m, r) => m && !r }.reduce(_ || _)
  val pipeS1Ready = !pipeS2Valid || io.pipe.s2_dataResp.ready
  
  pipeS1Fire := pipeS1Query && pipeS1Ready
  
  val storeS1Fire = pipeS1Fire && !pipeS1IsProbe && !pipeS1IsMove && io.pipe.s1_storeReq
  val storeS1AliasMatch = if (blockOffBits + idxBits > pgIdxBits) {
    io.pipe.s1_alias === get_alias(entryMeta(pipeS1MatchId).vaddr)
  } else {
    true.B
  }
  val storeS1Lock = storeS1Fire && pipeS1Hit && storeS1AliasMatch &&
    !(relFire && pipeS1MatchOH(relSelId))
  val storeS1AliasMismatch = storeS1Fire && pipeS1Hit && !storeS1AliasMatch &&
    !(relFire && pipeS1MatchOH(relSelId))
  
  entryStoreLock := VecInit(entryMeta.indices.map(i => storeS1Lock && pipeS1MatchOH(i)))

  val pipeS1ReadId = Mux(pipeS1IsMove, pipeS1MoveId, pipeS1MatchId)
  val pipeS1ReadMeta = entryMeta(pipeS1ReadId)
  val pipeS1LineOwned = pipeS1IsMove || storeS1Lock
  val pipeS1OwnerBlocked = pipeS1HasOwner && !pipeS1Hit
  val storeS1LockFailed = storeS1Fire && pipeS1Hit && !storeS1Lock
  val pipeS1Resp = Wire(new PBPipeDataResp)
  pipeS1Resp := 0.U.asTypeOf(new PBPipeDataResp)
  pipeS1Resp.retry := !pipeS1LineOwned && (pipeS1OwnerBlocked || storeS1LockFailed)
  pipeS1Resp.entryId := pipeS1ReadId
  pipeS1Resp.coh := pipeS1ReadMeta.coh
  pipeS1Resp.prefetchSource := pipeS1ReadMeta.prefetchSource
  pipeS1Resp.used := pipeS1ReadMeta.used
  pipeS1Resp.dataBad := pipeS1LineOwned && pipeS1ReadMeta.dataBad

  // s2
  val pipeS2Fire = pipeS2Valid && io.pipe.s2_dataResp.ready
  val storeS2RespValid = pipeS2Valid && storeS2Valid
  val moveS2Abort = io.pipe.s2_moveAbort.valid

  when (pipeS2Fire) { pipeS2Valid := false.B }
  when (pipeS1Fire) {
    when (!pipeS1IsProbe) {
      pipeS2Valid := true.B
      storeS2Valid := io.pipe.s1_storeReq && !pipeS1IsMove
      storeS2Locked := storeS1Lock
      pipeS2Resp := pipeS1Resp
    }
  }

  // s3
  val moveS3Done = io.pipe.s3_moveDone.valid
  val probeS3Done = io.pipe.s3_probeDone.valid

  // ========================================================================
  // Update Meta and Data
  // ========================================================================
  val entryAllocate = VecInit(entryMeta.indices.map(i => allocFire && allocId === i.U))
  val entryCancel = VecInit(entryMeta.indices.map(i => io.mshr.cancelReq.map(c => c.valid && c.bits === i.U).reduce(_ || _)))
  val entryRefill = VecInit(entryMeta.indices.map(i => refillFire && refillId === i.U))
  val entryMoveAbort = VecInit(entryMeta.indices.map(i => moveS2Abort && io.pipe.s2_moveAbort.bits.entryId === i.U))
  val entryMoveDone = VecInit(entryMeta.indices.map(i => moveS3Done && io.pipe.s3_moveDone.bits === i.U))
  val entryProbeDone = VecInit(entryMeta.indices.map(i => probeS3Done && io.pipe.s3_probeDone.bits === i.U))
  val entryRel = VecInit(entryMeta.indices.map(i => relFire && relSelId === i.U))
  val entryReleasedDone = VecInit(entryMeta.map(_.state === PBState.released))

  val nextMeta = WireInit(entryMeta)
  for (i <- entryMeta.indices) {
    val meta = entryMeta(i)
    val next = nextMeta(i)
    when (entryUsedNow(i)) {
      next.used := true.B
      when (meta.state === PBState.resident) { next.movePending := true.B }
    }
    when (storeS1AliasMismatch && pipeS1MatchOH(i)) { next.movePending := true.B }
    when (entryAllocate(i)) {
      next := 0.U.asTypeOf(new PBMeta)
      next.state := PBState.reserved
      next.paddr := blockAlignedAddr(io.mshr.allocReq.bits.addr)
      next.missEntryId := io.mshr.allocReq.bits.missEntryId
      next.vaddr := io.mshr.allocReq.bits.vaddr
      next.prefetchSource := io.mshr.allocReq.bits.prefetchSource
    }.elsewhen (entryCancel(i)) {
      next.state := PBState.invalid
    }.elsewhen (entryRefill(i)) {
      next.state := Mux(refillDenied, PBState.invalid, Mux(refillCorrupt, PBState.poison, PBState.resident))
      next.coh := io.mshr.refillReq.bits.coh
      next.dataBad := refillCorrupt && !refillDenied
    }.elsewhen (entryRel(i)) {
      next.state := PBState.released
    }.elsewhen (entryReleasedDone(i)) {
      next.state := PBState.invalid
    }.elsewhen (entryProbeDone(i) || entryMoveDone(i)) {
      next.state := PBState.invalid
    }.elsewhen (entryMoveAbort(i)) {
      val bad = meta.dataBad || io.pipe.s2_moveAbort.bits.dataBad
      next.state := Mux(bad, PBState.poison, PBState.resident)
      next.dataBad := bad
      next.movePending := true.B
    }.elsewhen (entryProbeLock(i)) {
      next.state := PBState.probeLocked
    }.elsewhen (entryStoreLock(i) || entryMoveLock(i)) {
      next.state := PBState.moveLocked
    }
  }

  for (i <- entryMeta.indices) {
    entryMeta(i) := nextMeta(i)
    when (entryRefill(i) && !refillDenied) {
      entryData(i) := refillData
    }
  }

  // ========================================================================
  // Error & WFI & Status
  // ========================================================================
  val pipeBad = pipeS1Fire && !pipeS1IsProbe && pipeS1LineOwned && pipeS1Resp.dataBad
  val relBad = relFire && dcacheParameters.alwaysReleaseData.B && relDataBad
  val refillBad = refillFire && (refillDenied || refillCorrupt)
  val errorReport = WireInit(0.U.asTypeOf(Valid(new L1CacheErrorInfo)))
  val pbError = pipeBad || relBad || refillBad
  errorReport.valid := pbError
  errorReport.bits.paddr := Mux(pipeBad, pipeS1ReadMeta.paddr,
    Mux(relBad, relReq.addr, refillMeta.paddr))
  errorReport.bits.source.tag := false.B
  errorReport.bits.source.data := pipeBad || relBad
  errorReport.bits.source.l2 := refillBad
  errorReport.bits.opType.load := false.B
  errorReport.bits.opType.release := pipeBad || relBad
  errorReport.bits.report_to_beu := pbError

  val drainingReservation = entryMeta.map(m => m.state === PBState.reserved &&
    !io.mshr.preAcquire(m.missEntryId)).reduce(_ || _)
  val wfiSafe = !entryLocked.asUInt.orR && !drainingReservation &&
    !entryReleased.asUInt.orR &&
    !moveSelValid && !relSelValid && !pipeS1IsProbe && !pipeS1IsMove &&
    !pipeS1Query && !pipeS2Valid && !refillFire && !probeS0Fire && !moveS0Fire &&
    !loadS1Valid.asUInt.orR && !loadS2Valid.asUInt.orR

  val entryOwned = VecInit(entryVisible.zip(entryReserved).map { case (visible, reserved) => visible && !reserved })

  // ========================================================================
  // IO
  // ========================================================================
  io.mshr.allocReq.ready := allocReady
  io.mshr.allocEntryId := allocId
  io.mshr.refillReq.ready := refillReady
  for (i <- entryMeta.indices) {
    io.mshr.status(i).valid := entryOwned(i)
    io.mshr.status(i).bits := entryMeta(i).paddr
  }
  io.pipe.s0_moveReq.valid := moveS0Valid
  io.pipe.s0_moveReq.bits.entryId := moveSelId
  io.pipe.s0_moveReq.bits.paddr := entryMeta(moveSelId).paddr
  io.pipe.s0_moveReq.bits.vaddr := entryMeta(moveSelId).vaddr
  io.pipe.s1_hit := pipeS1Hit
  io.pipe.s1_paddr.ready := pipeS1Ready
  io.pipe.s1_probeResp.valid := pipeS1IsProbe
  io.pipe.s1_probeResp.bits := probeS1Resp
  io.pipe.s2_storeResp.valid := storeS2RespValid
  io.pipe.s2_storeResp.bits := storeS2Locked
  io.pipe.s2_dataResp.valid := pipeS2Valid
  io.pipe.s2_dataResp.bits := pipeS2Resp
  io.pipe.s2_dataResp.bits.data := entryData(pipeS2Resp.entryId).asUInt
  io.pipe.s2_dataResp.bits.used := entryMeta(pipeS2Resp.entryId).used
  for (i <- 0 until LoadPipelineWidth) {
    io.load(i).s1_hit := loadS1Hit(i)
    io.load(i).s1_retry := loadS1Retry(i)
    io.load(i).s2_dataResp.valid := loadS2Valid(i)
    io.load(i).s2_dataResp.bits := loadS2Resp(i)
  }
  io.releaseReq.valid := relValid
  io.releaseReq.bits := relReq
  io.dcache.error.report := errorReport
  io.dcache.error.fatal := false.B
  io.dcache.wfi.safe := wfiSafe

  // ========================================================================
  // perf
  // ========================================================================
  // Performance-only state. These registers never participate in PB matching,
  // arbitration, state transitions, or ready/valid generation.
  val perfCycle = RegInit(0.U(64.W))
  val perfReserveCycle = RegInit(VecInit(Seq.fill(entryCount)(0.U(64.W))))
  val perfFillCycle = RegInit(VecInit(Seq.fill(entryCount)(0.U(64.W))))
  val perfFirstUseCycle = RegInit(VecInit(Seq.fill(entryCount)(0.U(64.W))))
  val perfLockCycle = RegInit(VecInit(Seq.fill(entryCount)(0.U(64.W))))
  val perfAliasMove = RegInit(VecInit(Seq.fill(entryCount)(false.B)))
  val perfPrevMoveFire = RegInit(false.B)

  val perfUnusedQueueDepth = entryCount + 2
  val perfUnusedQueue = RegInit(VecInit(
    Seq.fill(perfUnusedQueueDepth)(0.U(L1PfSourceBits.W))))
  val perfUnusedCount = RegInit(0.U(log2Ceil(perfUnusedQueueDepth + 1).W))

  val refillGood = refillFire && !refillDenied && !refillCorrupt
  val refillAccepted = refillFire && !refillDenied
  val entryFirstUse = VecInit(entryMeta.indices.map(i =>
    entryUsedNow(i) && !entryMeta(i).used && !entryMeta(i).dataBad))
  val entryFinalExit = VecInit(entryMeta.indices.map(i =>
    entryProbeDone(i) || entryReleasedDone(i)))
  val entryFinalExitUsed = VecInit(entryMeta.indices.map(i =>
    entryFinalExit(i) && (entryMeta(i).used || entryUsedNow(i))))
  val entryFinalExitBad = VecInit(entryMeta.indices.map(i =>
    entryFinalExit(i) && (entryMeta(i).dataBad ||
      entryRel(i) && dcacheParameters.alwaysReleaseData.B && relDataBad)))
  val entryUnusedExit = VecInit(entryMeta.indices.map(i =>
    entryFinalExit(i) && !entryFinalExitUsed(i) && !entryFinalExitBad(i)))

  val perfUnusedPop = perfUnusedCount =/= 0.U
  val perfUnusedBase = perfUnusedCount - perfUnusedPop.asUInt
  val perfUnusedPushCount = PopCount(entryUnusedExit)
  val perfUnusedNextCount = perfUnusedBase +& perfUnusedPushCount

  val perfLoadRetryLocked = VecInit((0 until LoadPipelineWidth).map(lane =>
    loadS1Retry(lane) && entryLocked(loadS1EntryId(lane))))
  val perfLoadRetryError = VecInit((0 until LoadPipelineWidth).map(lane =>
    loadS1Retry(lane) && entryMeta(loadS1EntryId(lane)).dataBad))
  // The current Load protocol has no separate stale-snapshot retry path.
  val perfLoadRetrySnapshot = VecInit(Seq.fill(LoadPipelineWidth)(false.B))
  // Loads are denied while an entry is locked, so this legacy event remains
  // structurally observable but is expected to stay zero.
  val perfLoadWithLock = VecInit((0 until LoadPipelineWidth).map(lane =>
    loadS1Hit(lane) && entryLocked(loadS1EntryId(lane))))
  val perfEntryLock = VecInit(entryMeta.indices.map(i =>
    entryProbeLock(i) || entryStoreLock(i) || entryMoveLock(i)))

  for (i <- entryMeta.indices) {
    when (entryAllocate(i)) {
      perfReserveCycle(i) := perfCycle
      perfAliasMove(i) := false.B
    }
    when (refillAccepted && refillId === i.U) {
      perfFillCycle(i) := perfCycle
    }
    when (entryFirstUse(i)) {
      perfFirstUseCycle(i) := perfCycle
    }
    when (perfEntryLock(i)) {
      perfLockCycle(i) := perfCycle
    }
    when (storeS1AliasMismatch && pipeS1MatchOH(i)) {
      perfAliasMove(i) := true.B
    }
  }

  perfCycle := perfCycle + 1.U
  perfPrevMoveFire := moveS0Fire

  when (perfUnusedPop) {
    for (i <- 0 until perfUnusedQueueDepth - 1) {
      perfUnusedQueue(i) := perfUnusedQueue(i + 1)
    }
  }
  for (i <- entryMeta.indices) {
    when (entryUnusedExit(i)) {
      val writeId = (perfUnusedBase +& PopCount(entryUnusedExit.take(i)))(
        log2Ceil(perfUnusedQueueDepth) - 1, 0)
      perfUnusedQueue(writeId) := entryMeta(i).prefetchSource
    }
  }
  perfUnusedCount := perfUnusedNextCount

  for (i <- entryMeta.indices) {
    io.dcache.perf.firstUse(i).valid := entryFirstUse(i)
    io.dcache.perf.firstUse(i).bits := entryMeta(i).prefetchSource
  }
  io.dcache.perf.unusedExit.valid := perfUnusedPop
  io.dcache.perf.unusedExit.bits := perfUnusedQueue(0)

  XSPerfAccumulate("reservation", allocFire)
  XSPerfAccumulate("reservation_cancel", PopCount(entryCancel))
  XSPerfAccumulate("fill", refillGood)
  XSPerfAccumulate("fill_denied", refillFire && refillDenied)
  XSPerfAccumulate("fill_corrupt", refillFire && !refillDenied && refillCorrupt)
  XSPerfAccumulate("fill_backpressure_cycles", io.mshr.refillReq.valid && !refillReady)
  XSPerfAccumulate("query_hit", PopCount(loadS1Match))
  XSPerfAccumulate("hit", PopCount((0 until LoadPipelineWidth).map(lane =>
    loadS2Valid(lane) && loadS2Hit(lane))))
  XSPerfAccumulate("retry", PopCount((0 until LoadPipelineWidth).map(lane =>
    loadS2Valid(lane) && loadS2Retry(lane))))
  XSPerfAccumulate("use", PopCount(entryUsedNow))
  XSPerfAccumulate("lane_use", PopCount(io.load.map(_.s2_use)))
  XSPerfAccumulate("first_use", PopCount(entryFirstUse))
  XSPerfAccumulate("promote", moveS3Done)
  XSPerfAccumulate("probe", probeS3Done)
  XSPerfAccumulate("evict", relFire)
  XSPerfAccumulate("full", allocBlocked)
  XSPerfAccumulate("capacity_full_cycles", !freeEntryOH.orR)
  XSPerfAccumulate("maint_entry_wait_cycles", moveS0Valid && !io.pipe.s0_moveReq.ready)
  XSPerfAccumulate("release_wait_cycles", relValid && !io.releaseReq.ready)
  XSPerfAccumulate("line_response_wait_cycles", pipeS2Valid && !io.pipe.s2_dataResp.ready)
  XSPerfAccumulate("promotion_abort", moveS2Abort)
  XSPerfAccumulate("move_request", moveS0Fire)
  XSPerfAccumulate("move_request_back_to_back", moveS0Fire && perfPrevMoveFire)
  XSPerfAccumulate("move_release_parallel", moveS0Fire && relFire)
  XSPerfAccumulate("probe_release_overlap", probeS0Fire && relFire && probeS0MatchOH(relSelId))

  for (lane <- 0 until LoadPipelineWidth) {
    XSPerfAccumulate(s"lane_${lane}_query", loadS1Valid(lane))
    XSPerfAccumulate(s"lane_${lane}_retry_claimed", perfLoadRetryLocked(lane))
    XSPerfAccumulate(s"lane_${lane}_retry_snapshot", perfLoadRetrySnapshot(lane))
    XSPerfAccumulate(s"lane_${lane}_retry_error", perfLoadRetryError(lane))
    XSPerfAccumulate(s"lane_${lane}_authorize_with_claim", perfLoadWithLock(lane))
  }

  for ((mask, name) <- Seq(
    entryReserved -> "reserved",
    VecInit(entryMeta.map(_.state === PBState.resident)) -> "resident",
    entryLocked -> "claimed",
    VecInit(entryMeta.map(_.state === PBState.poison)) -> "poison",
    entryReleased -> "released")) {
    val occupancy = PopCount(mask)
    PrefetchPerfBoundary(s"${name}_occupancy", occupancy)
    XSPerfAccumulate(s"${name}_entry_cycles", occupancy)
    XSPerfHistogram(s"${name}_occupancy", occupancy, true.B, 0, entryCount + 1, 1)
  }
  PrefetchPerfBoundary("unused_occupancy", PopCount(entryMeta.indices.map(i =>
    entryVisible(i) && !entryReserved(i) && !entryMeta(i).used && !entryMeta(i).dataBad)))

  for (i <- entryMeta.indices) {
    val storeMove = entryStoreLock(i)
    val aliasMismatch = storeS1AliasMismatch && pipeS1MatchOH(i)
    val activeMove = entryMoveLock(i)
    val moveAfterUse = (storeMove || activeMove) &&
      (entryMeta(i).used || entryFirstUse(i))
    val useToMoveAge = Mux(entryFirstUse(i), 0.U, perfCycle - perfFirstUseCycle(i))
    val capacityExit = entryReleasedDone(i)
    val probeExit = entryProbeDone(i)

    XSPerfHistogram(s"entry_${i}_fill_to_use", perfCycle - perfFillCycle(i),
      entryFirstUse(i), 0, 4096, 128)
    XSPerfHistogram(s"entry_${i}_reservation_lifetime", perfCycle - perfReserveCycle(i),
      entryFinalExit(i) || entryMoveDone(i) || entryCancel(i) ||
        entryRefill(i) && refillDenied, 0, 8192, 256)
    XSPerfHistogram(s"entry_${i}_use_to_claim", useToMoveAge,
      moveAfterUse, 0, 4096, 128)
    XSPerfAccumulate(s"entry_${i}_first_use_after_claim",
      entryFirstUse(i) && entryLocked(i))
    XSPerfAccumulate(s"entry_${i}_store_promote_claim", storeMove)
    XSPerfAccumulate(s"entry_${i}_alias_promote_claim", aliasMismatch)
    XSPerfAccumulate(s"entry_${i}_load_promote_claim",
      activeMove && !storeMove && !aliasMismatch)
    XSPerfHistogram(s"entry_${i}_claim_to_promote", perfCycle - perfLockCycle(i),
      entryMoveDone(i), 0, 4096, 128)
    XSPerfHistogram(s"entry_${i}_claim_to_abort", perfCycle - perfLockCycle(i),
      entryMoveAbort(i), 0, 4096, 128)
    XSPerfAccumulate(s"entry_${i}_probe_used_exit",
      probeExit && !entryFinalExitBad(i) && entryFinalExitUsed(i))
    XSPerfAccumulate(s"entry_${i}_probe_unused_exit",
      probeExit && !entryFinalExitBad(i) && !entryFinalExitUsed(i))
    XSPerfAccumulate(s"entry_${i}_capacity_used_exit",
      capacityExit && !entryFinalExitBad(i) && entryFinalExitUsed(i))
    XSPerfAccumulate(s"entry_${i}_capacity_unused_exit",
      capacityExit && !entryFinalExitBad(i) && !entryFinalExitUsed(i))
    XSPerfHistogram(s"entry_${i}_fill_to_unused_probe", perfCycle - perfFillCycle(i),
      probeExit && !entryFinalExitBad(i) && !entryFinalExitUsed(i), 0, 8192, 256)
    XSPerfHistogram(s"entry_${i}_fill_to_unused_capacity", perfCycle - perfFillCycle(i),
      capacityExit && !entryFinalExitBad(i) && !entryFinalExitUsed(i), 0, 8192, 256)
    XSPerfAccumulate(s"entry_${i}_error_exit", entryFinalExitBad(i))
    XSPerfAccumulate(s"entry_${i}_unused_promotion",
      entryMoveDone(i) && !entryMeta(i).used && !entryUsedNow(i))
  }

}
