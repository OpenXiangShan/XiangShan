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
//
//
// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] Glenn Reinman, Brad Calder, and Todd Austin. "[Fetch directed instruction prefetching.]
// (https://doi.org/10.1109/MICRO.1999.809439)" 32nd Annual ACM/IEEE International Symposium on Microarchitecture
// (MICRO). 1999.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModuleImp
import org.chipsalliance.cde.config.Parameters
import utility.BoolStopWatch
import utility.HasPerfEvents
import utility.XSPerfAccumulate
import xiangshan.L1CacheErrorInfo
import xiangshan.SoftIfetchPrefetchBundle
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.FtqToPrefetchIO

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters with HasPerfEvents {

  class ICacheIO(implicit p: Parameters) extends ICacheBundle {
    val hartId: UInt = Input(UInt(hartIdLen.W))
    // FTQ
    val fetch:       ICacheMainPipeBundle = new ICacheMainPipeBundle
    val ftqPrefetch: FtqToPrefetchIO      = Flipped(new FtqToPrefetchIO)
    // memblock
    val softPrefetch: Vec[Valid[SoftIfetchPrefetchBundle]] =
      Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
    // IFU
    val stop:  Bool = Input(Bool())
    val toIFU: Bool = Output(Bool())
    // PMP: mainPipe & prefetchPipe need PortNumber each
    val pmp: Vec[ICachePMPBundle] = Vec(2 * PortNumber, new ICachePMPBundle)
    // iTLB
    val itlb:          Vec[TlbRequestIO] = Vec(PortNumber, new TlbRequestIO)
    val itlbFlushPipe: Bool              = Bool()
    // backend/BEU
    val error: Valid[L1CacheErrorInfo] = ValidIO(new L1CacheErrorInfo)
    // backend/CSR
    val csr_pf_enable: Bool = Input(Bool())
    // flush
    val fencei: Bool = Input(Bool())
    val flush:  Bool = Input(Bool())

    // perf
    val perfInfo: ICachePerfInfo = Output(new ICachePerfInfo)
  }
  val io: ICacheIO = IO(new ICacheIO)

  println("ICache:")
  println("  TagECC: " + cacheParams.tagECC)
  println("  DataECC: " + cacheParams.dataECC)
  println("  ICacheSets: " + cacheParams.nSets)
  println("  ICacheWays: " + cacheParams.nWays)
  println("  PortNumber: " + cacheParams.PortNumber)
  println("  nFetchMshr: " + cacheParams.nFetchMshr)
  println("  nPrefetchMshr: " + cacheParams.nPrefetchMshr)
  println("  nWayLookupSize: " + cacheParams.nWayLookupSize)
  println("  DataCodeUnit: " + cacheParams.DataCodeUnit)
  println("  ICacheDataBanks: " + cacheParams.ICacheDataBanks)
  println("  ICacheDataSRAMWidth: " + cacheParams.ICacheDataSRAMWidth)

  val (bus, edge) = outer.clientNode.out.head

  private val metaArray  = Module(new ICacheMetaArray)
  private val dataArray  = Module(new ICacheDataArray)
  private val mainPipe   = Module(new ICacheMainPipe)
  private val missUnit   = Module(new ICacheMissUnit(edge))
  private val replacer   = Module(new ICacheReplacer)
  private val prefetcher = Module(new IPrefetchPipe)
  private val wayLookup  = Module(new WayLookup)

  private val ecc_enable = if (outer.ctrlUnitOpt.nonEmpty) outer.ctrlUnitOpt.get.module.io.ecc_enable else true.B

  // dataArray io
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    when(ctrlUnit.io.injecting) {
      dataArray.io.write <> ctrlUnit.io.dataWrite
      missUnit.io.data_write.ready := false.B
    }.otherwise {
      ctrlUnit.io.dataWrite.ready := false.B
      dataArray.io.write <> missUnit.io.data_write
    }
  } else {
    dataArray.io.write <> missUnit.io.data_write
  }
  dataArray.io.read <> mainPipe.io.dataArray.toIData
  mainPipe.io.dataArray.fromIData := dataArray.io.readResp

  // metaArray io
  metaArray.io.flushAll := io.fencei
  metaArray.io.flush <> mainPipe.io.metaArrayFlush
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    when(ctrlUnit.io.injecting) {
      metaArray.io.write <> ctrlUnit.io.metaWrite
      metaArray.io.read <> ctrlUnit.io.metaRead
      missUnit.io.meta_write.ready         := false.B
      prefetcher.io.metaRead.toIMeta.ready := false.B
    }.otherwise {
      ctrlUnit.io.metaWrite.ready := false.B
      ctrlUnit.io.metaRead.ready  := false.B
      metaArray.io.write <> missUnit.io.meta_write
      metaArray.io.read <> prefetcher.io.metaRead.toIMeta
    }
    ctrlUnit.io.metaReadResp := metaArray.io.readResp
  } else {
    metaArray.io.write <> missUnit.io.meta_write
    metaArray.io.read <> prefetcher.io.metaRead.toIMeta
  }
  prefetcher.io.metaRead.fromIMeta := metaArray.io.readResp

  prefetcher.io.flush         := io.flush
  prefetcher.io.csr_pf_enable := io.csr_pf_enable
  prefetcher.io.ecc_enable    := ecc_enable
  prefetcher.io.MSHRResp      := missUnit.io.fetch_resp
  prefetcher.io.flushFromBpu  := io.ftqPrefetch.flushFromBpu
  // cache softPrefetch
  private val softPrefetchValid = RegInit(false.B)
  private val softPrefetch      = RegInit(0.U.asTypeOf(new IPrefetchReq))
  /* FIXME:
   * If there is already a pending softPrefetch request, it will be overwritten.
   * Also, if there are multiple softPrefetch requests in the same cycle, only the first one will be accepted.
   * We should implement a softPrefetchQueue (like ibuffer, multi-in, single-out) to solve this.
   * However, the impact on performance still needs to be assessed.
   * Considering that the frequency of prefetch.i may not be high, let's start with a temporary dummy solution.
   */
  when(io.softPrefetch.map(_.valid).reduce(_ || _)) {
    softPrefetchValid := true.B
    softPrefetch.fromSoftPrefetch(MuxCase(
      0.U.asTypeOf(new SoftIfetchPrefetchBundle),
      io.softPrefetch.map(req => req.valid -> req.bits)
    ))
  }.elsewhen(prefetcher.io.req.fire) {
    softPrefetchValid := false.B
  }
  // pass ftqPrefetch
  private val ftqPrefetch = WireInit(0.U.asTypeOf(new IPrefetchReq))
  ftqPrefetch.fromFtqICacheInfo(io.ftqPrefetch.req.bits)
  // software prefetch has higher priority
  prefetcher.io.req.valid                 := softPrefetchValid || io.ftqPrefetch.req.valid
  prefetcher.io.req.bits                  := Mux(softPrefetchValid, softPrefetch, ftqPrefetch)
  prefetcher.io.req.bits.backendException := io.ftqPrefetch.backendException
  io.ftqPrefetch.req.ready                := prefetcher.io.req.ready && !softPrefetchValid

  missUnit.io.hartId := io.hartId
  missUnit.io.fencei := io.fencei
  missUnit.io.flush  := io.flush
  missUnit.io.fetch_req <> mainPipe.io.mshr.req
  missUnit.io.prefetch_req <> prefetcher.io.MSHRReq
  missUnit.io.mem_grant.valid := false.B
  missUnit.io.mem_grant.bits  := DontCare
  missUnit.io.mem_grant <> bus.d

  mainPipe.io.flush      := io.flush
  mainPipe.io.respStall  := io.stop
  mainPipe.io.ecc_enable := ecc_enable
  mainPipe.io.hartId     := io.hartId
  mainPipe.io.mshr.resp  := missUnit.io.fetch_resp
  mainPipe.io.fetch.req <> io.fetch.req
  mainPipe.io.wayLookupRead <> wayLookup.io.read

  wayLookup.io.flush := io.flush
  wayLookup.io.write <> prefetcher.io.wayLookupWrite
  wayLookup.io.update := missUnit.io.fetch_resp

  replacer.io.touch <> mainPipe.io.touch
  replacer.io.victim <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)
  io.itlbFlushPipe := prefetcher.io.itlbFlushPipe

  // notify IFU that Icache pipeline is available
  io.toIFU    := mainPipe.io.fetch.req.ready
  io.perfInfo := mainPipe.io.perfInfo

  io.fetch.resp <> mainPipe.io.fetch.resp
  io.fetch.topdownIcacheMiss := mainPipe.io.fetch.topdownIcacheMiss
  io.fetch.topdownItlbMiss   := mainPipe.io.fetch.topdownItlbMiss

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.mem_acquire

  // Parity error port
  private val errors       = mainPipe.io.errors
  private val errors_valid = errors.map(e => e.valid).reduce(_ | _)
  io.error.bits <> RegEnable(
    PriorityMux(errors.map(e => e.valid -> e.bits)),
    0.U.asTypeOf(errors(0).bits),
    errors_valid
  )
  io.error.valid := RegNext(errors_valid, false.B)

  XSPerfAccumulate(
    "softPrefetch_drop_not_ready",
    io.softPrefetch.map(_.valid).reduce(_ || _) && softPrefetchValid && !prefetcher.io.req.fire
  )
  XSPerfAccumulate("softPrefetch_drop_multi_req", PopCount(io.softPrefetch.map(_.valid)) > 1.U)
  XSPerfAccumulate("softPrefetch_block_ftq", softPrefetchValid && io.ftqPrefetch.req.valid)

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true))
  )
  generatePerfEvent()
}
