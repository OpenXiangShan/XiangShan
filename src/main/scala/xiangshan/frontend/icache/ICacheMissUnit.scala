/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

class DeMultiplexerIO[T <: Data](gen: T, n: Int) extends Bundle {
  val in:     DecoupledIO[T]      = Flipped(DecoupledIO(gen))
  val out:    Vec[DecoupledIO[T]] = Vec(n, DecoupledIO(gen))
  val chosen: UInt                = Output(UInt(log2Ceil(n).W))
}

/** Hardware module that is used to sequence 1 producer into n consumer.
 * Priority is given to lower producer.
 */
class DeMultiplexer[T <: Data](val gen: T, val n: Int) extends Module {
  require(n >= 2)
  val io: DeMultiplexerIO[T] = IO(new DeMultiplexerIO(gen, n))

  private val grant = false.B +: (1 until n).map(i => (0 until i).map(io.out(_).ready).reduce(_ || _))
  (0 until n).foreach { i =>
    io.out(i).bits  := io.in.bits
    io.out(i).valid := !grant(i) && io.in.valid
  }

  io.in.ready := grant.last || io.out.last.ready
  io.chosen   := PriorityEncoder(VecInit(io.out.map(_.ready)))
}

class MuxBundleIO[T <: Data](gen: T, n: Int) extends Bundle {
  val sel: UInt                = Input(UInt(log2Ceil(n).W))
  val in:  Vec[DecoupledIO[T]] = Flipped(Vec(n, DecoupledIO(gen)))
  val out: DecoupledIO[T]      = DecoupledIO(gen)
}

class MuxBundle[T <: Data](val gen: T, val n: Int) extends Module {
  require(n >= 2)
  val io: MuxBundleIO[T] = IO(new MuxBundleIO[T](gen, n))

  io.in <> DontCare
  io.out <> DontCare
  (0 until n).foreach { i =>
    when(io.sel === i.U) {
      io.out <> io.in(i)
    }
    io.in(i).ready := (io.sel === i.U) && io.out.ready
  }
}

class ICacheMissReq(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
}

class ICacheMissResp(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val waymask:  UInt = UInt(nWays.W)
  val data:     UInt = UInt(blockBits.W)
  val corrupt:  Bool = Bool()
}

class LookUpMSHR(implicit p: Parameters) extends ICacheBundle {
  val info: Valid[ICacheMissReq] = ValidIO(new ICacheMissReq)
  val hit:  Bool                 = Input(Bool())
}

class MSHRResp(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val way:      UInt = UInt(wayBits.W)
}

class MSHRAcquire(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  val acquire: TLBundleA = new TLBundleA(edge.bundle)
  val vSetIdx: UInt      = UInt(idxBits.W)
}

class ICacheMSHRIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  val fencei:    Bool                       = Input(Bool())
  val flush:     Bool                       = Input(Bool())
  val invalid:   Bool                       = Input(Bool())
  val req:       DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq))
  val acquire:   DecoupledIO[MSHRAcquire]   = DecoupledIO(new MSHRAcquire(edge))
  val lookUps:   Vec[LookUpMSHR]            = Flipped(Vec(2, new LookUpMSHR))
  val resp:      Valid[MSHRResp]            = ValidIO(new MSHRResp)
  val victimWay: UInt                       = Input(UInt(wayBits.W))
}

class ICacheMSHR(edge: TLEdgeOut, isFetch: Boolean, ID: Int)(implicit p: Parameters) extends ICacheModule {
  val io: ICacheMSHRIO = IO(new ICacheMSHRIO(edge))

  private val valid = RegInit(Bool(), false.B)
  // this MSHR doesn't respond to fetch and sram
  private val flush  = RegInit(Bool(), false.B)
  private val fencei = RegInit(Bool(), false.B)
  // this MSHR has been issued
  private val issue = RegInit(Bool(), false.B)

  private val blkPaddr = RegInit(UInt((PAddrBits - blockOffBits).W), 0.U)
  private val vSetIdx  = RegInit(UInt(idxBits.W), 0.U)
  private val way      = RegInit(UInt(wayBits.W), 0.U)

  // look up and return result at the same cycle
  private val hits = io.lookUps.map { lookup =>
    valid && !fencei && !flush && (lookup.info.bits.vSetIdx === vSetIdx) &&
    (lookup.info.bits.blkPaddr === blkPaddr)
  }
  // Decoupling valid and bits
  (0 until 2).foreach(i => io.lookUps(i).hit := hits(i))

  // disable wake up when hit MSHR (fencei is low)
  // when(hit) {
  //   flush := false.B
  // }

  // invalid when the req hasn't been issued
  when(io.fencei || io.flush) {
    fencei := true.B
    flush  := true.B
    when(!issue) {
      valid := false.B
    }
  }

  // receive request and register
  io.req.ready := !valid && !io.flush && !io.fencei
  when(io.req.fire) {
    valid    := true.B
    flush    := false.B
    issue    := false.B
    fencei   := false.B
    blkPaddr := io.req.bits.blkPaddr
    vSetIdx  := io.req.bits.vSetIdx
  }

  // send request to L2
  io.acquire.valid := valid && !issue && !io.flush && !io.fencei
  private val getBlock = edge.Get(
    fromSource = ID.U,
    toAddress = Cat(blkPaddr, 0.U(blockOffBits.W)),
    lgSize = log2Up(cacheParams.blockBytes).U
  )._2
  io.acquire.bits.acquire := getBlock
  io.acquire.bits.acquire.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPUInst.id.U)
  io.acquire.bits.vSetIdx := vSetIdx

  // get victim way when acquire fire
  when(io.acquire.fire) {
    issue := true.B
    way   := io.victimWay
  }

  // invalid request when grant finish
  when(io.invalid) {
    valid := false.B
  }

  // offer the information other than data for write sram and response fetch
  io.resp.valid         := valid && (!flush && !fencei)
  io.resp.bits.blkPaddr := blkPaddr
  io.resp.bits.vSetIdx  := vSetIdx
  io.resp.bits.way      := way
}

class ICacheMissUnitIO(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  // difftest
  val hartId: Bool = Input(Bool())
  // control
  val fencei: Bool = Input(Bool())
  val flush:  Bool = Input(Bool())
  // fetch
  val fetch_req:  DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq))
  val fetch_resp: Valid[ICacheMissResp]      = ValidIO(new ICacheMissResp)
  // prefetch
  val prefetch_req: DecoupledIO[ICacheMissReq] = Flipped(DecoupledIO(new ICacheMissReq))
  // SRAM Write Req
  val meta_write: DecoupledIO[ICacheMetaWriteBundle] = DecoupledIO(new ICacheMetaWriteBundle)
  val data_write: DecoupledIO[ICacheDataWriteBundle] = DecoupledIO(new ICacheDataWriteBundle)
  // get victim from replacer
  val victim: ReplacerVictim = new ReplacerVictim
  // Tilelink
  val mem_acquire: DecoupledIO[TLBundleA] = DecoupledIO(new TLBundleA(edge.bundle))
  val mem_grant:   DecoupledIO[TLBundleD] = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
}

class ICacheMissUnit(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheModule {
  val io: ICacheMissUnitIO = IO(new ICacheMissUnitIO(edge))

  /**
    ******************************************************************************
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
    ******************************************************************************
    */

  private val fetchDemux    = Module(new DeMultiplexer(new ICacheMissReq, nFetchMshr))
  private val prefetchDemux = Module(new DeMultiplexer(new ICacheMissReq, nPrefetchMshr))
  private val prefetchArb   = Module(new MuxBundle(new MSHRAcquire(edge), nPrefetchMshr))
  private val acquireArb    = Module(new Arbiter(new MSHRAcquire(edge), nFetchMshr + 1))

  // To avoid duplicate request reception.
  private val fetchHit    = Wire(Bool())
  private val prefetchHit = Wire(Bool())
  fetchDemux.io.in <> io.fetch_req
  fetchDemux.io.in.valid := io.fetch_req.valid && !fetchHit
  io.fetch_req.ready     := fetchDemux.io.in.ready || fetchHit
  prefetchDemux.io.in <> io.prefetch_req
  prefetchDemux.io.in.valid := io.prefetch_req.valid && !prefetchHit
  io.prefetch_req.ready     := prefetchDemux.io.in.ready || prefetchHit
  acquireArb.io.in.last <> prefetchArb.io.out

  // mem_acquire connect
  io.mem_acquire.valid    := acquireArb.io.out.valid
  io.mem_acquire.bits     := acquireArb.io.out.bits.acquire
  acquireArb.io.out.ready := io.mem_acquire.ready

  private val fetchMSHRs = (0 until nFetchMshr).map { i =>
    val mshr = Module(new ICacheMSHR(edge, true, i))
    mshr.io.flush  := false.B
    mshr.io.fencei := io.fencei
    mshr.io.req <> fetchDemux.io.out(i)
    mshr.io.lookUps(0).info.valid := io.fetch_req.valid
    mshr.io.lookUps(0).info.bits  := io.fetch_req.bits
    mshr.io.lookUps(1).info.valid := io.prefetch_req.valid
    mshr.io.lookUps(1).info.bits  := io.prefetch_req.bits
    mshr.io.victimWay             := io.victim.way
    acquireArb.io.in(i) <> mshr.io.acquire
    mshr
  }

  private val prefetchMSHRs = (0 until nPrefetchMshr).map { i =>
    val mshr = Module(new ICacheMSHR(edge, false, nFetchMshr + i))
    mshr.io.flush  := io.flush
    mshr.io.fencei := io.fencei
    mshr.io.req <> prefetchDemux.io.out(i)
    mshr.io.lookUps(0).info.valid := io.fetch_req.valid
    mshr.io.lookUps(0).info.bits  := io.fetch_req.bits
    mshr.io.lookUps(1).info.valid := io.prefetch_req.valid
    mshr.io.lookUps(1).info.bits  := io.prefetch_req.bits
    mshr.io.victimWay             := io.victim.way
    prefetchArb.io.in(i) <> mshr.io.acquire
    mshr
  }

  /**
    ******************************************************************************
    * MSHR look up
    * - look up all mshr
    ******************************************************************************
    */
  private val allMSHRs = fetchMSHRs ++ prefetchMSHRs
  private val prefetchHitFetchReq = (io.prefetch_req.bits.blkPaddr === io.fetch_req.bits.blkPaddr) &&
    (io.prefetch_req.bits.vSetIdx === io.fetch_req.bits.vSetIdx) &&
    io.fetch_req.valid
  fetchHit    := allMSHRs.map(mshr => mshr.io.lookUps(0).hit).reduce(_ || _)
  prefetchHit := allMSHRs.map(mshr => mshr.io.lookUps(1).hit).reduce(_ || _) || prefetchHitFetchReq

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
  private val priorityFIFO = Module(new FIFOReg(UInt(log2Ceil(nPrefetchMshr).W), nPrefetchMshr, hasFlush = true))
  priorityFIFO.io.flush.get := io.flush || io.fencei
  priorityFIFO.io.enq.valid := prefetchDemux.io.in.fire
  priorityFIFO.io.enq.bits  := prefetchDemux.io.chosen
  priorityFIFO.io.deq.ready := prefetchArb.io.out.fire
  prefetchArb.io.sel        := priorityFIFO.io.deq.bits
  assert(
    !(priorityFIFO.io.enq.fire ^ prefetchDemux.io.in.fire),
    "priorityFIFO.io.enq and io.prefetch_req must fire at the same cycle"
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

  private val wait_last = readBeatCnt === (refillCycles - 1).U
  when(io.mem_grant.fire && edge.hasData(io.mem_grant.bits)) {
    respDataReg(readBeatCnt) := io.mem_grant.bits.data
    readBeatCnt              := Mux(wait_last, 0.U, readBeatCnt + 1.U)
  }

  // last transition finish or corrupt
  private val last_fire = io.mem_grant.fire && edge.hasData(io.mem_grant.bits) && wait_last

  private val (_, _, refill_done, _) = edge.addr_inc(io.mem_grant)
  assert(!(refill_done ^ last_fire), "refill not done!")
  io.mem_grant.ready := true.B

  private val last_fire_r = RegNext(last_fire)
  private val id_r        = RegNext(io.mem_grant.bits.source)

  // if any beat is corrupt, the whole response (to mainPipe/metaArray/dataArray) is corrupt
  private val corrupt_r = RegInit(false.B)
  when(io.mem_grant.fire && edge.hasData(io.mem_grant.bits) && io.mem_grant.bits.corrupt) {
    // Set corrupt_r when any beat is corrupt
    // This is actually when(xxx.fire && xxx.hasData) { corrupt_r := corrupt_r || io.mem_grant.bits.corrupt }
    corrupt_r := true.B
  }.elsewhen(last_fire_r) {
    // Clear corrupt_r when response it sent to mainPipe
    // This used to be io.fetch_resp.valid (last_fire_r && mshr_valid) but when mshr is flushed by io.flush/fencei,
    // mshr_valid is false.B and corrupt_r will never be cleared, that's not correct
    // so we remove mshr_valid here, and the condition leftover is last_fire_r
    // or, actually, io.fetch_resp.valid || (last_fire_r && !mshr_valid)
    corrupt_r := false.B
  }

  /**
    ******************************************************************************
    * invalid mshr when finish transition
    ******************************************************************************
    */
  (0 until (nFetchMshr + nPrefetchMshr)).foreach(i => allMSHRs(i).io.invalid := last_fire_r && (id_r === i.U))

  /**
    ******************************************************************************
    * response fetch and write SRAM
    ******************************************************************************
    */
  // get request information from MSHRs
  private val allMSHRs_resp = VecInit(allMSHRs.map(mshr => mshr.io.resp))
  // select MSHR response 1 cycle before sending response to mainPipe/prefetchPipe for better timing
  private val mshr_resp =
    RegEnable(allMSHRs_resp(io.mem_grant.bits.source).bits, 0.U.asTypeOf(allMSHRs_resp(0).bits), last_fire)
  // we can latch mshr.io.resp.bits since they are set on req.fire or acquire.fire, and keeps unchanged during response
  // however, we should not latch mshr.io.resp.valid, since io.flush/fencei may clear it at any time
  private val mshr_valid = allMSHRs_resp(id_r).valid

  // get waymask from replacer when acquire fire
  io.victim.vSetIdx.valid := acquireArb.io.out.fire
  io.victim.vSetIdx.bits  := acquireArb.io.out.bits.vSetIdx
  private val waymask = UIntToOH(mshr_resp.way)
  // NOTE: when flush/fencei, missUnit will still send response to mainPipe/prefetchPipe
  //       this is intentional to fix timing (io.flush -> mainPipe/prefetchPipe s2_miss -> s2_ready -> ftq ready)
  //       unnecessary response will be dropped by mainPipe/prefetchPipe/wayLookup since their sx_valid is set to false
  private val fetch_resp_valid = mshr_valid && last_fire_r
  // NOTE: but we should not write meta/dataArray when flush/fencei
  private val write_sram_valid = fetch_resp_valid && !corrupt_r && !io.flush && !io.fencei

  // write SRAM
  io.meta_write.bits.generate(
    tag = getPhyTagFromBlk(mshr_resp.blkPaddr),
    idx = mshr_resp.vSetIdx,
    waymask = waymask,
    bankIdx = mshr_resp.vSetIdx(0),
    poison = false.B
  )
  io.data_write.bits.generate(
    data = respDataReg.asUInt,
    idx = mshr_resp.vSetIdx,
    waymask = waymask,
    bankIdx = mshr_resp.vSetIdx(0),
    poison = false.B
  )

  io.meta_write.valid := write_sram_valid
  io.data_write.valid := write_sram_valid

  // response fetch
  io.fetch_resp.valid         := fetch_resp_valid
  io.fetch_resp.bits.blkPaddr := mshr_resp.blkPaddr
  io.fetch_resp.bits.vSetIdx  := mshr_resp.vSetIdx
  io.fetch_resp.bits.waymask  := waymask
  io.fetch_resp.bits.data     := respDataReg.asUInt
  io.fetch_resp.bits.corrupt  := corrupt_r

  /**
    ******************************************************************************
    * performance counter
    ******************************************************************************
    */
  // Duplicate requests will be excluded.
  XSPerfAccumulate("enq_fetch_req", fetchDemux.io.in.fire)
  XSPerfAccumulate("enq_prefetch_req", prefetchDemux.io.in.fire)

  /**
    ******************************************************************************
    * ChiselDB: record ICache SRAM write log
    ******************************************************************************
    */
  private class ICacheSRAMDB(implicit p: Parameters) extends ICacheBundle {
    val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
    val vSetIdx:  UInt = UInt(idxBits.W)
    val waymask:  UInt = UInt(wayBits.W)
  }

  private val isWriteICacheSRAMTable =
    WireInit(Constantin.createRecord("isWriteICacheSRAMTable" + p(XSCoreParamsKey).HartId.toString))
  private val ICacheSRAMTable =
    ChiselDB.createTable("ICacheSRAMTable" + p(XSCoreParamsKey).HartId.toString, new ICacheSRAMDB)

  private val ICacheSRAMDBDumpData = Wire(new ICacheSRAMDB)
  ICacheSRAMDBDumpData.blkPaddr := mshr_resp.blkPaddr
  ICacheSRAMDBDumpData.vSetIdx  := mshr_resp.vSetIdx
  ICacheSRAMDBDumpData.waymask  := OHToUInt(waymask)
  ICacheSRAMTable.log(
    data = ICacheSRAMDBDumpData,
    en = write_sram_valid,
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
    difftest.valid  := write_sram_valid
    difftest.addr   := Cat(mshr_resp.blkPaddr, 0.U(blockOffBits.W))
    difftest.data   := respDataReg.asTypeOf(difftest.data)
    difftest.idtfr  := DontCare
  }
}
