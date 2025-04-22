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
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToICacheIO

class ICacheImp(outer: ICache) extends LazyModuleImp(outer) with HasICacheParameters with HasPerfEvents {
  class ICacheIO(implicit p: Parameters) extends ICacheBundle {
    val hartId: UInt = Input(UInt(hartIdLen.W))
    // FTQ
    val fromFtq: FtqToICacheIO = Flipped(new FtqToICacheIO)
    // memblock
    val softPrefetchReq: Vec[Valid[SoftIfetchPrefetchBundle]] =
      Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
    // IFU
    val toIfu:   ICacheToIfuIO = new ICacheToIfuIO
    val fromIfu: IfuToICacheIO = Flipped(new IfuToICacheIO)
    // PMP: mainPipe & prefetchPipe need PortNumber each
    val pmp: Vec[PmpCheckBundle] = Vec(2 * PortNumber, new PmpCheckBundle)
    // iTLB
    val itlb:          Vec[TlbRequestIO] = Vec(PortNumber, new TlbRequestIO)
    val itlbFlushPipe: Bool              = Bool()
    // backend/BEU
    val error: Valid[L1CacheErrorInfo] = ValidIO(new L1CacheErrorInfo)
    // backend/CSR
    val csrPfEnable: Bool = Input(Bool())
    // flush
    val fencei: Bool = Input(Bool())
    val flush:  Bool = Input(Bool())
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
  private val prefetcher = Module(new ICachePrefetchPipe)
  private val wayLookup  = Module(new ICacheWayLookup)

  private val eccEnable = if (outer.ctrlUnitOpt.nonEmpty) outer.ctrlUnitOpt.get.module.io.eccEnable else true.B

  // dataArray io
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    when(ctrlUnit.io.injecting) {
      dataArray.io.write <> ctrlUnit.io.dataWrite
      missUnit.io.dataWrite.req.ready := false.B
    }.otherwise {
      ctrlUnit.io.dataWrite.req.ready := false.B
      dataArray.io.write <> missUnit.io.dataWrite
    }
  } else {
    dataArray.io.write <> missUnit.io.dataWrite
  }
  dataArray.io.read <> mainPipe.io.dataRead

  // metaArray io
  metaArray.io.flushAll := io.fencei
  metaArray.io.flush <> mainPipe.io.metaFlush
  if (outer.ctrlUnitOpt.nonEmpty) {
    val ctrlUnit = outer.ctrlUnitOpt.get.module
    when(ctrlUnit.io.injecting) {
      metaArray.io.write <> ctrlUnit.io.metaWrite
      metaArray.io.read <> ctrlUnit.io.metaRead
      missUnit.io.metaWrite.req.ready  := false.B
      prefetcher.io.metaRead.req.ready := false.B
      prefetcher.io.metaRead.resp      := DontCare
    }.otherwise {
      ctrlUnit.io.metaWrite.req.ready := false.B
      ctrlUnit.io.metaRead.req.ready  := false.B
      ctrlUnit.io.metaRead.resp       := DontCare
      metaArray.io.write <> missUnit.io.metaWrite
      metaArray.io.read <> prefetcher.io.metaRead
    }
  } else {
    metaArray.io.write <> missUnit.io.metaWrite
    metaArray.io.read <> prefetcher.io.metaRead
  }

  prefetcher.io.flush        := io.flush
  prefetcher.io.csrPfEnable  := io.csrPfEnable
  prefetcher.io.eccEnable    := eccEnable
  prefetcher.io.missResp     := missUnit.io.resp
  prefetcher.io.flushFromBpu := io.fromFtq.flushFromBpu
  // cache softPrefetch
  private val softPrefetchValid = RegInit(false.B)
  private val softPrefetch      = RegInit(0.U.asTypeOf(new PrefetchReqBundle))
  /* FIXME:
   * If there is already a pending softPrefetch request, it will be overwritten.
   * Also, if there are multiple softPrefetch requests in the same cycle, only the first one will be accepted.
   * We should implement a softPrefetchQueue (like ibuffer, multi-in, single-out) to solve this.
   * However, the impact on performance still needs to be assessed.
   * Considering that the frequency of prefetch.i may not be high, let's start with a temporary dummy solution.
   */
  when(io.softPrefetchReq.map(_.valid).reduce(_ || _)) {
    softPrefetchValid := true.B
    softPrefetch.fromSoftPrefetch(MuxCase(
      0.U.asTypeOf(new SoftIfetchPrefetchBundle),
      io.softPrefetchReq.map(req => req.valid -> req.bits)
    ))
  }.elsewhen(prefetcher.io.req.fire) {
    softPrefetchValid := false.B
  }
  // pass ftqPrefetch
  private val ftqPrefetch = WireInit(0.U.asTypeOf(new PrefetchReqBundle))
  ftqPrefetch.fromFtqICacheInfo(io.fromFtq.prefetchReq.bits.req)
  // software prefetch has higher priority
  prefetcher.io.req.valid                 := softPrefetchValid || io.fromFtq.prefetchReq.valid
  prefetcher.io.req.bits                  := Mux(softPrefetchValid, softPrefetch, ftqPrefetch)
  prefetcher.io.req.bits.backendException := io.fromFtq.prefetchReq.bits.backendException
  io.fromFtq.prefetchReq.ready            := prefetcher.io.req.ready && !softPrefetchValid

  missUnit.io.hartId := io.hartId
  missUnit.io.fencei := io.fencei
  missUnit.io.flush  := io.flush
  missUnit.io.fetchReq <> mainPipe.io.missReq
  missUnit.io.prefetchReq <> prefetcher.io.missReq
  missUnit.io.memGrant.valid := false.B
  missUnit.io.memGrant.bits  := DontCare
  missUnit.io.memGrant <> bus.d

  mainPipe.io.flush     := io.flush
  mainPipe.io.respStall := io.fromIfu.stall
  mainPipe.io.eccEnable := eccEnable
  mainPipe.io.hartId    := io.hartId
  mainPipe.io.missResp  := missUnit.io.resp
  mainPipe.io.req <> io.fromFtq.fetchReq
  mainPipe.io.wayLookupRead <> wayLookup.io.read

  wayLookup.io.flush := io.flush
  wayLookup.io.write <> prefetcher.io.wayLookupWrite
  wayLookup.io.update := missUnit.io.resp

  replacer.io.touch <> mainPipe.io.replacerTouch
  replacer.io.victim <> missUnit.io.victim

  io.pmp(0) <> mainPipe.io.pmp(0)
  io.pmp(1) <> mainPipe.io.pmp(1)
  io.pmp(2) <> prefetcher.io.pmp(0)
  io.pmp(3) <> prefetcher.io.pmp(1)

  io.itlb(0) <> prefetcher.io.itlb(0)
  io.itlb(1) <> prefetcher.io.itlb(1)
  io.itlbFlushPipe := prefetcher.io.itlbFlushPipe

  // notify IFU that Icache pipeline is available
  io.toIfu.fetchReady := mainPipe.io.req.ready

  // send resp
  io.toIfu.s1Resp <> mainPipe.io.s1Resp
  io.toIfu.s2Resp <> mainPipe.io.s2Resp

  // perf
  io.toIfu.perf    := mainPipe.io.perf
  io.toIfu.topdown := mainPipe.io.topdown

  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  bus.a <> missUnit.io.memAcquire

  // Parity error port
  private val errors      = mainPipe.io.errors
  private val errorsValid = errors.map(e => e.valid).reduce(_ | _)
  io.error.bits <> RegEnable(
    PriorityMux(errors.map(e => e.valid -> e.bits)),
    0.U.asTypeOf(errors(0).bits),
    errorsValid
  )
  io.error.valid := RegNext(errorsValid, false.B)

  XSPerfAccumulate(
    "softPrefetch_drop_not_ready",
    io.softPrefetchReq.map(_.valid).reduce(_ || _) && softPrefetchValid && !prefetcher.io.req.fire
  )
  XSPerfAccumulate("softPrefetch_drop_multi_req", PopCount(io.softPrefetchReq.map(_.valid)) > 1.U)
  XSPerfAccumulate("softPrefetch_block_ftq", softPrefetchValid && io.fromFtq.prefetchReq.valid)

  val perfEvents: Seq[(String, Bool)] = Seq(
    ("icache_miss_cnt  ", false.B),
    ("icache_miss_penalty", BoolStopWatch(start = false.B, stop = false.B || false.B, startHighPriority = true))
  )
  generatePerfEvent()
}
