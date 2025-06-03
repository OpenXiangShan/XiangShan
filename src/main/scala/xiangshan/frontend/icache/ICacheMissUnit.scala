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
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.tilelink.TLBundleD
import freechips.rocketchip.tilelink.TLEdgeOut
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.Constantin
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.WfiReqBundle
import xiangshan.XSCoreParamsKey

class ICacheMissUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule with ICacheAddrHelper {
  class ICacheMissUnitIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
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
    // Tilelink
    val memAcquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
    val memGrant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
  }

  val io: ICacheMissUnitIO = IO(new ICacheMissUnitIO(edge))

  /* *****************************************************************************
   * fetch have higher priority
   * fetch MSHR: lower index have a higher priority
   * prefetch MSHR: the prefetchMSHRs earlier have a higher priority
   *                 ---------       --------------       -----------
   * ---fetch reg--->| Demux |-----> | fetch MSHR |------>| Arbiter |---acquire--->
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
  private val prefetchArb   = Module(new MuxBundle(new MshrAcquireBundle(edge), NumPrefetchMshr))
  private val acquireArb    = Module(new Arbiter(new MshrAcquireBundle(edge), NumFetchMshr + 1))

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

  // mem_acquire connect
  io.memAcquire.valid     := acquireArb.io.out.valid
  io.memAcquire.bits      := acquireArb.io.out.bits.acquire
  acquireArb.io.out.ready := io.memAcquire.ready

  private val allMshr = (0 until NumFetchMshr + NumPrefetchMshr).map { i =>
    val isFetch = i < NumFetchMshr
    val mshr    = Module(new ICacheMshr(edge, isFetch, i))
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
  fetchHit    := allMshr.map(mshr => mshr.io.lookUps(0).resp.hit).reduce(_ || _)
  prefetchHit := allMshr.map(mshr => mshr.io.lookUps(1).resp.hit).reduce(_ || _) || prefetchHitFetchReq

  /**
    ******************************************************************************
    * prefetchMSHRs priority
    * - The requests that enter the prefetchMSHRs earlier have a higher priority in issuing.
    * - The order of enqueuing is recorded in FIFO when request enters MSHRs.
    * - The requests are dispatched in the order they are recorded in FIFO.
    ******************************************************************************
    */
  // When the FIFO is full, enqueue and dequeue operations do not occur at the same cycle.
  // So the depth of the FIFO is set to match the number of MSHRs.
  // val priorityFIFO = Module(new Queue(UInt(log2Ceil(nPrefetchMshr).W), nPrefetchMshr, hasFlush=true))
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
    * Tilelink D channel (grant)
    ******************************************************************************
    */
  // cacheline register
  private val readBeatCnt = RegInit(UInt(log2Up(refillCycles).W), 0.U)
  private val respDataReg = RegInit(VecInit(Seq.fill(refillCycles)(0.U(beatBits.W))))

  private val waitLast = readBeatCnt === (refillCycles - 1).U
  when(io.memGrant.fire && edge.hasData(io.memGrant.bits)) {
    respDataReg(readBeatCnt) := io.memGrant.bits.data
    readBeatCnt              := Mux(waitLast, 0.U, readBeatCnt + 1.U)
  }

  // last transition finish or corrupt
  private val lastFire = io.memGrant.fire && edge.hasData(io.memGrant.bits) && waitLast

  private val (_, _, refillDone, _) = edge.addr_inc(io.memGrant)
  assert(!(refillDone ^ lastFire), "refill not done!")
  io.memGrant.ready := true.B

  private val lastFireNext = RegNext(lastFire)
  private val idNext       = RegNext(io.memGrant.bits.source)

  private val corruptReg = RegInit(false.B)
  private val deniedReg  = RegInit(false.B)
  when(io.memGrant.fire && edge.hasData(io.memGrant.bits)) {
    // Set corruptReg / deniedReg when any beat is corrupt / denied
    corruptReg := corruptReg || io.memGrant.bits.corrupt
    deniedReg  := deniedReg || io.memGrant.bits.denied
  }.elsewhen(lastFireNext) {
    // Clear corruptReg / deniedReg when response it sent to mainPipe
    // This used to be io.resp.valid (lastFireNext && mshrValid) but when mshr is flushed by io.flush/fencei,
    // mshrValid is false.B and corruptReg will never be cleared, that's not correct
    // so we remove mshrValid here, and the condition leftover is lastFireNext
    // or, actually, io.resp.valid || (lastFireNext && !mshrValid)
    corruptReg := false.B
    deniedReg  := false.B
  }

  /**
    ******************************************************************************
    * invalid mshr when finish transition
    ******************************************************************************
    */
  (0 until (NumFetchMshr + NumPrefetchMshr)).foreach(i => allMshr(i).io.invalid := lastFireNext && (idNext === i.U))

  /* *****************************************************************************
   * response fetch and write SRAM
   * ***************************************************************************** */
  // get request information from MSHRs
  private val allMshrResp = VecInit(allMshr.map(mshr => mshr.io.info))
  // select MSHR response 1 cycle before sending response to mainPipe/prefetchPipe for better timing
  private val mshrResp =
    RegEnable(allMshrResp(io.memGrant.bits.source).bits, 0.U.asTypeOf(allMshrResp(0).bits), lastFire)
  // we can latch mshr.io.resp.bits since they are set on req.fire or acquire.fire, and keeps unchanged during response
  // however, we should not latch mshr.io.resp.valid, since io.flush/fencei may clear it at any time
  private val mshrValid = allMshrResp(idNext).valid

  // get waymask from replacer when acquire fire
  io.victim.req.valid        := acquireArb.io.out.fire
  io.victim.req.bits.vSetIdx := acquireArb.io.out.bits.vSetIdx
  private val waymask = UIntToOH(mshrResp.way)
  // NOTE: when flush/fencei, missUnit will still send response to mainPipe/prefetchPipe
  //       this is intentional to fix timing (io.flush -> mainPipe/prefetchPipe s2_miss -> s2_ready -> ftq ready)
  //       unnecessary response will be dropped by mainPipe/prefetchPipe/wayLookup since their sx_valid is set to false
  private val respValid = mshrValid && lastFireNext
  // NOTE: but we should not write meta/dataArray when flush/fencei
  // NOTE: tilelink spec asks corrupt to be set when denied is set, so we don't need to check deniedReg here
  private val writeSramValid = respValid && !corruptReg && !io.flush && !io.fencei

  // write SRAM
  io.metaWrite.req.bits.generate(
    phyTag = getPTagFromBlk(mshrResp.blkPAddr),
    vSetIdx = mshrResp.vSetIdx,
    waymask = waymask,
    bankIdx = mshrResp.vSetIdx(0),
    poison = false.B
  )
  io.dataWrite.req.bits.generate(
    data = respDataReg.asUInt,
    vSetIdx = mshrResp.vSetIdx,
    waymask = waymask,
    bankIdx = mshrResp.vSetIdx(0),
    poison = false.B
  )

  io.metaWrite.req.valid := writeSramValid
  io.dataWrite.req.valid := writeSramValid

  // response fetch
  io.resp.valid         := respValid
  io.resp.bits.blkPAddr := mshrResp.blkPAddr
  io.resp.bits.vSetIdx  := mshrResp.vSetIdx
  io.resp.bits.waymask  := waymask
  io.resp.bits.data     := respDataReg.asUInt
  io.resp.bits.corrupt  := corruptReg
  io.resp.bits.denied   := deniedReg

  // we are safe to enter wfi if all entries have no pending response from L2
  io.wfi.wfiSafe := allMshr.map(_.io.wfi.wfiSafe).reduce(_ && _)

  /* *****************************************************************************
   * perf
   * ***************************************************************************** */
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

  /**
    ******************************************************************************
    * ChiselDB: record ICache SRAM write log
    ******************************************************************************
    */
  private class ICacheSramDb(implicit p: Parameters) extends ICacheBundle {
    val blkPAddr: UInt = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx:  UInt = UInt(idxBits.W)
    val waymask:  UInt = UInt(wayBits.W)
  }

  private val isWriteICacheSRAMTable =
    WireInit(Constantin.createRecord("isWriteICacheSRAMTable" + p(XSCoreParamsKey).HartId.toString))
  private val iCacheSramTable =
    ChiselDB.createTable("ICacheSRAMTable" + p(XSCoreParamsKey).HartId.toString, new ICacheSramDb)

  private val iCacheSramDbDumpData = Wire(new ICacheSramDb)
  iCacheSramDbDumpData.blkPAddr := mshrResp.blkPAddr
  iCacheSramDbDumpData.vSetIdx  := mshrResp.vSetIdx
  iCacheSramDbDumpData.waymask  := OHToUInt(waymask)
  iCacheSramTable.log(
    data = iCacheSramDbDumpData,
    en = writeSramValid,
    clock = clock,
    reset = reset
  )

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
    difftest.addr   := Cat(mshrResp.blkPAddr, 0.U(blockOffBits.W))
    difftest.data   := respDataReg.asTypeOf(difftest.data)
    difftest.idtfr  := DontCare
  }
}
