// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import difftest.DiffRefillEvent
import difftest.DifftestModule
import oceanus.compactchi._
import org.chipsalliance.cde.config.Parameters
import xiangshan.cache.DCacheCCHI
import xiangshan.cache.ICacheCCHI
import utility.ChiselDB
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.WfiReqBundle
import xiangshan.XSCoreParamsKey

class ICacheMissUnit(implicit p: Parameters) extends ICacheModule with ICacheAddrHelper {
  class ICacheMissUnitIO(implicit p: Parameters) extends ICacheBundle {
    // difftest
    val hartId: Bool = Input(Bool())
    // control
    val fencei: Bool         = Input(Bool())
    val flush:  Bool         = Input(Bool())
    val wfi:    WfiReqBundle = Flipped(new WfiReqBundle)
    // request from mainPipe
    val fetchReq: DecoupledIO[MissReqBundle] = Flipped(DecoupledIO(new MissReqBundle))
    // request from prefetchPipe
    val prefetchReq: DecoupledIO[MissReqBundle] = Flipped(DecoupledIO(new MissReqBundle))
    // response to mainPipe / prefetchPipe / waylookup
    val resp: Valid[MissRespBundle] = ValidIO(new MissRespBundle)
    // SRAM Write
    val metaWrite: MetaWriteBundle = new MetaWriteBundle
    val dataWrite: DataWriteBundle = new DataWriteBundle
    // get victim from replacer
    val victim: ReplacerVictimBundle = new ReplacerVictimBundle
    // Compact CHI
    val txreq: DecoupledIO[FlitREQ] = DecoupledIO(new FlitREQ)
    val rxdat: DecoupledIO[FlitDnDAT] = Flipped(DecoupledIO(new FlitDnDAT))
  }

  val io: ICacheMissUnitIO = IO(new ICacheMissUnitIO)

  /* *****************************************************************************
   * fetch have higher priority
   * fetch MSHR: lower index have a higher priority
   * prefetch MSHR: the prefetchMSHRs earlier have a higher priority
   *                 ---------       --------------       -----------
   * ---fetch reg--->| Demux |-----> | fetch MSHR |------>| Arbiter |---txreq--->
   *                 ---------       --------------       -----------
   *                                 | fetch MSHR |            ^
   *                                 --------------            |
   *                                                           |
   *                                -----------------          |
   *                                | prefetch MSHR |          |
   *                 ---------      -----------------     -----------
   * ---fetch reg--->| Demux |----> | prefetch MSHR |---->| Arbiter |
   *                 ---------      -----------------     -----------
   *                                |    .......    |
   *                                -----------------
   * ***************************************************************************** */

  private val fetchDemux    = Module(new DeMultiplexer(new MissReqBundle, NumFetchMshr))
  private val prefetchDemux = Module(new DeMultiplexer(new MissReqBundle, NumPrefetchMshr))
  private val prefetchArb   = Module(new MuxBundle(new MshrReqBundle, NumPrefetchMshr))
  private val acquireArb    = Module(new Arbiter(new MshrReqBundle, NumFetchMshr + 1))

  // To avoid duplicate request reception.
  private val fetchHit    = Wire(Bool())
  private val prefetchHit = Wire(Bool())
  fetchDemux.io.in <> io.fetchReq
  fetchDemux.io.in.valid := io.fetchReq.valid && !fetchHit
  io.fetchReq.ready      := fetchDemux.io.in.ready || fetchHit
  prefetchDemux.io.in <> io.prefetchReq
  prefetchDemux.io.in.valid := io.prefetchReq.valid && !prefetchHit
  io.prefetchReq.ready      := prefetchDemux.io.in.ready || prefetchHit
  acquireArb.io.in.last <> prefetchArb.io.out

  // resolve aliasing, refer to comments on AliasTagBits in trait HasICacheParameters
  // vSetIdx is vAddr(untagBits, blockOffBits); TagAlias = vSetIdx high AliasTagBits -> FlitREQ.TagAlias
  private def aliasFromVSetIdx(vSetIdx: UInt): UInt =
    AliasTagBits.map(w => vSetIdx.head(w)).getOrElse(0.U(DCacheCCHI.tagAliasWidth.W))

  // TXREQ (was mem_acquire on TileLink)
  io.txreq.valid := acquireArb.io.out.valid
  acquireArb.io.out.ready := io.txreq.ready
  io.txreq.bits := 0.U.asTypeOf(io.txreq.bits)
  when(io.txreq.fire) {
    val req = acquireArb.io.out.bits
    ICacheCCHI.Tx.missReq(
      io.txreq.bits,
      txnId = req.mshrId,
      addr = Cat(req.blkPAddr, 0.U(blockOffBits.W)),
      alias = aliasFromVSetIdx(req.vSetIdx),
      srcId = cchiIcacheSrcId
    )
  }

  private val allMshr = (0 until NumAllMshr).map { i =>
    val isFetch = i < NumFetchMshr
    val mshr    = Module(new ICacheMshr(isFetch, i))
    mshr.io.fencei               := io.fencei
    mshr.io.wfi.wfiReq           := io.wfi.wfiReq
    mshr.io.lookUps(0).req.valid := io.fetchReq.valid
    mshr.io.lookUps(0).req.bits  := io.fetchReq.bits
    mshr.io.lookUps(1).req.valid := io.prefetchReq.valid
    mshr.io.lookUps(1).req.bits  := io.prefetchReq.bits
    mshr.io.victimWay            := io.victim.resp.way
    if (isFetch) {
      mshr.io.flush := false.B
      mshr.io.req <> fetchDemux.io.out(i)
      acquireArb.io.in(i) <> mshr.io.acquire
    } else {
      mshr.io.flush := io.flush
      mshr.io.req <> prefetchDemux.io.out(i - NumFetchMshr)
      prefetchArb.io.in(i - NumFetchMshr) <> mshr.io.acquire
    }
    mshr
  }

  /**
    ******************************************************************************
    * MSHR look up
    * - look up all mshr
    ******************************************************************************
    */
  private val prefetchHitFetchReq =
    (io.prefetchReq.bits.blkPAddr === io.fetchReq.bits.blkPAddr) &&
      (io.prefetchReq.bits.vSetIdx === io.fetchReq.bits.vSetIdx) &&
      io.fetchReq.valid
  fetchHit    := allMshr.map(_.io.lookUps(0).resp.hit).reduce(_ || _)
  prefetchHit := allMshr.map(_.io.lookUps(1).resp.hit).reduce(_ || _) || prefetchHitFetchReq

  /**
    ******************************************************************************
    * prefetchMSHRs priority
    * - The requests that enter the prefetchMSHRs earlier have a higher priority in issuing.
    * - The order of enqueuing is recorded in FIFO when request enters MSHRs.
    * - The requests are dispatched in the order they are recorded in FIFO.
    ******************************************************************************
    */
  private val priorityFIFO = Module(new FIFOReg(UInt(log2Ceil(NumPrefetchMshr).W), NumPrefetchMshr, hasFlush = true))
  priorityFIFO.io.flush.get := io.flush || io.fencei
  priorityFIFO.io.enq.valid := prefetchDemux.io.in.fire
  priorityFIFO.io.enq.bits  := prefetchDemux.io.chosen
  priorityFIFO.io.deq.ready := prefetchArb.io.out.fire
  prefetchArb.io.sel        := priorityFIFO.io.deq.bits
  assert(
    !(priorityFIFO.io.enq.fire ^ prefetchDemux.io.in.fire),
    "priorityFIFO.io.enq and io.prefetchReq must fire at the same cycle"
  )
  assert(
    !(priorityFIFO.io.deq.fire ^ prefetchArb.io.out.fire),
    "priorityFIFO.io.deq and prefetchArb.io.out must fire at the same cycle"
  )

  /**
    ******************************************************************************
    * RXDAT CompData
    * - A CompData line is 2 beats (DataID 0/1), which may arrive out of order.
    * - Beats of different TxnIDs may interleave arbitrarily (TxnID is the MSHR id),
    *   so each MSHR owns its receive state (gotData / beatHalf / err flags) instead
    *   of a single shared pair collector.
    * - At most one line completes per cycle (only 1 beat per cycle), so the single
    *   assembly registers below are reloaded at most once per cycle. io.resp pulses
    *   may be back-to-back on consecutive completions; consumers snoop by address
    *   and tolerate that (mainPipe holds per bank, wayLookup composes per cycle).
    * - io.rxdat.ready is always true: the SRAM write port accepts 1 full line per
    *   cycle and the sustained completion rate is at most 1 line per 2 beats.
    ******************************************************************************
    */
  require(refillCycles == 2, "CompData refill uses DataID 0/1")
  // assembled cacheline registers, loaded at complete
  private val respDataReg = Reg(Vec(refillCycles, UInt(beatBits.W)))
  private val corruptReg  = RegInit(false.B)
  private val deniedReg   = RegInit(false.B)

  // per-MSHR receive state
  private val gotData    = RegInit(VecInit(Seq.fill(NumAllMshr)(VecInit(Seq.fill(refillCycles)(false.B)))))
  private val beatHalf   = Reg(Vec(NumAllMshr, UInt(beatBits.W))) // the first-arrived half beat
  private val errCorrupt = RegInit(VecInit(Seq.fill(NumAllMshr)(false.B)))
  private val errDenied  = RegInit(VecInit(Seq.fill(NumAllMshr)(false.B)))

  private val txnId  = io.rxdat.bits.TxnID(log2Ceil(NumAllMshr) - 1, 0)
  private val dataId = io.rxdat.bits.DataID(log2Ceil(refillCycles) - 1, 0)

  private val compDataValid =
    io.rxdat.valid && CCHIOpcode.CompData.is(io.rxdat.bits.Opcode, io.rxdat.valid)
  private val compDataFire = io.rxdat.fire && compDataValid

  // this beat completes the pair: the other half has already arrived
  private val complete   = compDataFire && gotData(txnId)(dataId ^ 1.U)
  private val completeId = txnId

  (0 until NumAllMshr).foreach { i =>
    when(compDataFire && txnId === i.U) {
      gotData(i)(dataId) := true.B
      beatHalf(i)        := io.rxdat.bits.Data
      errCorrupt(i)      := errCorrupt(i) || ICacheCCHI.Rx.corrupt(io.rxdat.bits.RespErr)
      errDenied(i)       := errDenied(i) || ICacheCCHI.Rx.denied(io.rxdat.bits.RespErr)
    }
  }

  io.rxdat.ready := true.B

  private val completeNext   = RegNext(complete)
  private val completeIdNext = RegEnable(completeId, complete)

  // Load the assembly registers from the completing entry: beatHalf still holds the
  // first-arrived half (updated at end of this cycle), the arriving beat is the other.
  when(complete) {
    respDataReg(dataId)       := io.rxdat.bits.Data
    respDataReg(dataId ^ 1.U) := beatHalf(completeId)
    corruptReg := errCorrupt(completeId) || ICacheCCHI.Rx.corrupt(io.rxdat.bits.RespErr)
    deniedReg  := errDenied(completeId) || ICacheCCHI.Rx.denied(io.rxdat.bits.RespErr)
  }

  // Per-entry state clears 1 cycle after complete (when the entry is invalidated),
  // unconditionally (not gated by mshrValid): a flush/fencei-killed MSHR must not
  // leak its error flags into the next allocation. Since corruptReg/deniedReg are
  // fully reloaded at every complete, no cross-transaction leak is possible.
  when(completeNext) {
    (0 until refillCycles).foreach(j => gotData(completeIdNext)(j) := false.B)
    errCorrupt(completeIdNext) := false.B
    errDenied(completeIdNext)  := false.B
  }
  // defensive: clear on allocation so a reallocated entry never inherits stale state
  (0 until NumAllMshr).foreach { i =>
    when(allMshr(i).io.req.fire) {
      (0 until refillCycles).foreach(j => gotData(i)(j) := false.B)
      errCorrupt(i) := false.B
      errDenied(i)  := false.B
    }
  }

  assert(!compDataFire || io.rxdat.bits.TxnID < NumAllMshr.U, "DnDAT CompData TxnID must be a MSHR id")
  assert(!compDataFire || io.rxdat.bits.DataID < refillCycles.U, "DnDAT CompData DataID out of range")
  assert(
    !compDataFire || !VecInit(allMshr.map(_.io.wfi.wfiSafe))(txnId),
    "DnDAT CompData beat for MSHR with no outstanding txn"
  )
  // L2 must never resend a beat: assert the contract instead of masking it in hardware
  assert(
    !(compDataFire && gotData(txnId)(dataId)),
    "DnDAT CompData duplicate beat (DataID already received for this TxnID)"
  )

  /**
    ******************************************************************************
    * invalid mshr when finish transition
    ******************************************************************************
    */
  (0 until NumAllMshr).foreach(i => allMshr(i).io.invalid := completeNext && (completeIdNext === i.U))

  /* *****************************************************************************
   * respond to fetch and write SRAM
   * ***************************************************************************** */
  // get request information from MSHRs
  private val allMshrInfo = VecInit(allMshr.map(_.io.info))
  // select MSHR info 1 cycle before sending response to mainPipe/prefetchPipe for better timing
  private val mshrInfo =
    RegEnable(allMshrInfo(completeId).bits, 0.U.asTypeOf(allMshrInfo(0).bits), complete)
  // we can latch mshr.io.info.bits since they are set on req.fire or acquire.fire, and keeps unchanged during response
  // however, we should not latch mshr.io.info.valid, since io.flush/fencei may clear it at any time
  private val mshrValid = allMshrInfo(completeIdNext).valid

  // get waymask from replacer when acquire fire
  io.victim.req.valid        := acquireArb.io.out.fire
  io.victim.req.bits.vSetIdx := acquireArb.io.out.bits.vSetIdx
  private val waymask = UIntToOH(mshrInfo.way)
  // maybeRvcMap: whether lower 2 bits of each 2 bytes is not 0b11
  private val maybeRvcMap =
    VecInit(respDataReg.asTypeOf(Vec(MaxInstNumPerBlock, UInt((instBytes * 8).W))).map(_(1, 0) =/= 3.U)).asUInt
  // NOTE: when flush/fencei, missUnit will still send response to mainPipe/prefetchPipe
  //       this is intentional to fix timing (io.flush -> mainPipe/prefetchPipe s2_miss -> s2_ready -> ftq ready)
  //       unnecessary response will be dropped by mainPipe/prefetchPipe/wayLookup since their sx_valid is set to false
  private val respValid = mshrValid && completeNext
  // NOTE: but we should not write meta/dataArray when flush/fencei
  private val writeSramValid = respValid && !corruptReg && !io.flush && !io.fencei

  // write SRAM
  io.metaWrite.req.bits.generate(
    phyTag = getPTagFromBlk(mshrInfo.blkPAddr),
    maybeRvcMap = maybeRvcMap,
    vSetIdx = mshrInfo.vSetIdx,
    waymask = waymask,
    poison = false.B
  )
  io.dataWrite.req.bits.generate(
    data = respDataReg.asUInt,
    vSetIdx = mshrInfo.vSetIdx,
    waymask = waymask,
    poison = false.B
  )

  io.metaWrite.req.valid := writeSramValid
  io.dataWrite.req.valid := writeSramValid

  // response fetch
  io.resp.valid            := respValid
  io.resp.bits.blkPAddr    := mshrInfo.blkPAddr
  io.resp.bits.vSetIdx     := mshrInfo.vSetIdx
  io.resp.bits.waymask     := waymask
  io.resp.bits.data        := respDataReg.asUInt
  io.resp.bits.maybeRvcMap := maybeRvcMap
  io.resp.bits.corrupt     := corruptReg
  io.resp.bits.denied      := deniedReg

  // we are safe to enter wfi if all entries have no pending response from L2
  io.wfi.wfiSafe := allMshr.map(_.io.wfi.wfiSafe).reduce(_ && _)

  /* *** perf *** */
  // Total requests, duplicate requests will be excluded.
  XSPerfAccumulate("enqFetchReq", fetchDemux.io.in.fire)
  XSPerfAccumulate("enqPrefetchReq", prefetchDemux.io.in.fire)

  // Duplicate requests
  XSPerfAccumulate("duplicateFetchReq", fetchHit)
  XSPerfAccumulate("duplicatePrefetchReq", prefetchHit) // includes prefetchHitFetchReq
  XSPerfAccumulate("prefetchHitFetchReq", prefetchHitFetchReq)

  // Mshr occupancy
  XSPerfHistogram(
    "fetchMshrEmptyCnt",
    PopCount(fetchDemux.io.out.map(_.ready)),
    true.B,
    0,
    NumFetchMshr
  )
  XSPerfHistogram(
    "prefetchMshrEmptyCnt",
    PopCount(prefetchDemux.io.out.map(_.ready)),
    true.B,
    0,
    NumPrefetchMshr
  )

  // missTrace
  private class FetchTrace extends Bundle {
    val blkPAddr: UInt = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx:  UInt = UInt(idxBits.W)
    val mshr:     UInt = UInt(log2Ceil(NumAllMshr).W)
    val hitMshr:  Bool = Bool()
  }

  private class PrefetchTrace extends FetchTrace {
    val hitFetch: Bool = Bool()
  }

  private class RespTrace extends Bundle {
    val mshr:     UInt = UInt(log2Ceil(NumAllMshr).W)
    val victim:   UInt = UInt(wayBits.W)
    val latency:  UInt = UInt(16.W) // magic number: latency should less than 65536 cycles
    val corrupt:  Bool = Bool()
    val denied:   Bool = Bool()
    val canceled: Bool = Bool()
  }

  private val fetchTable    = ChiselDB.createTable("ICacheFetchMissTrace", new FetchTrace, EnableTrace)
  private val prefetchTable = ChiselDB.createTable("ICachePrefetchMissTrace", new PrefetchTrace, EnableTrace)
  private val respTable     = ChiselDB.createTable("ICacheMissRespTrace", new RespTrace, EnableTrace)

  private val fetchTrace = Wire(new FetchTrace)
  fetchTrace.blkPAddr := io.fetchReq.bits.blkPAddr
  fetchTrace.vSetIdx  := io.fetchReq.bits.vSetIdx
  fetchTrace.mshr := Mux(
    fetchHit,
    PriorityEncoder(allMshr.map(_.io.lookUps(0).resp.hit)),
    fetchDemux.io.chosen
  )
  fetchTrace.hitMshr := fetchHit
  fetchTable.log(data = fetchTrace, en = io.fetchReq.fire, clock = clock, reset = reset)

  private val prefetchTrace = Wire(new PrefetchTrace)
  prefetchTrace.blkPAddr := io.prefetchReq.bits.blkPAddr
  prefetchTrace.vSetIdx  := io.prefetchReq.bits.vSetIdx
  prefetchTrace.mshr := Mux(
    prefetchHit,
    PriorityEncoder(allMshr.map(_.io.lookUps(1).resp.hit)),
    prefetchDemux.io.chosen
  )
  prefetchTrace.hitMshr  := prefetchHit && !prefetchHitFetchReq
  prefetchTrace.hitFetch := prefetchHitFetchReq
  prefetchTable.log(data = prefetchTrace, en = io.prefetchReq.fire, clock = clock, reset = reset)

  private val respTrace = Wire(new RespTrace)
  respTrace.mshr     := completeIdNext
  respTrace.victim   := mshrInfo.way
  respTrace.latency  := VecInit(allMshr.map(_.io.perf_latency))(completeIdNext)
  respTrace.corrupt  := corruptReg
  respTrace.denied   := deniedReg
  respTrace.canceled := !mshrValid // fence.i or flushed
  respTable.log(data = respTrace, en = completeNext, clock = clock, reset = reset)

  /**
    ******************************************************************************
    * Difftest
    ******************************************************************************
    */
  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffRefillEvent, dontCare = true)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := writeSramValid
    difftest.addr   := Cat(mshrInfo.blkPAddr, 0.U(blockOffBits.W))
    difftest.data   := respDataReg.asTypeOf(difftest.data)
    difftest.mask   := VecInit.fill(difftest.mask.getWidth)(true.B).asUInt
  }
}
