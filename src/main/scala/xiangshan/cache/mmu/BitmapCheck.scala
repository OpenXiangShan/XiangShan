/***************************************************************************************
* Copyright (c) 2024-2025 Institute of Information Engineering, Chinese Academy of Sciences
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

package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import utility.mbist.MbistPipeline
import coupledL2.utils.SplittedSRAM
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

class bitmapReqBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
    val bmppn = UInt(ppnLen.W)
    val id = UInt(log2Up(l2tlbParams.llptwsize+2).W)
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
    val way_info = UInt(l2tlbParams.l0nWays.W)
    val hptw_bypassed = Bool()
    val s2xlate = UInt(2.W)
    val n = Bool() // Napot
}

class bitmapRespBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
    val cf = Bool()
    val cfs = Vec(tlbcontiguous,Bool())
    val id = UInt(log2Up(l2tlbParams.llptwsize+2).W)
}

class BitmapWakeup(implicit p: Parameters) extends PtwBundle {
  val setIndex = Input(UInt(PtwL0SetIdxLen.W))
  val tag = Input(UInt(SPTagLen.W))
  val way_info = UInt(l2tlbParams.l0nWays.W)
  val pte_index = UInt(sectortlbwidth.W)
  val check_success = Bool()
  val s2xlate = UInt(2.W)
}

class bitmapEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val ppn = UInt(ppnLen.W)
  val vpn = UInt(vpnLen.W)
  val s2xlate = UInt(2.W)
  val id = UInt(bMemID.W)
  val wait_id = UInt(log2Up(l2tlbParams.llptwsize+2).W)
  // bitmap check faild? : 0 success, 1 faild
  val cf = Bool()
  val hit = Bool()
  val cfs = Vec(tlbcontiguous,Bool())
  val level = UInt(log2Up(Level).W)
  val way_info = UInt(l2tlbParams.l0nWays.W)
  val hptw_bypassed = Bool()
  val n = Bool() // Napot
  val data = UInt(XLEN.W)
}

class bitmapIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(DecoupledIO(new Bundle {
      val id = Output(UInt(bMemID.W))
      val value = Output(UInt(blockBits.W))
    }))
    val req_mask = Input(Vec(l2tlbParams.llptwsize+2, Bool()))
  }
  val req = Flipped(DecoupledIO(new bitmapReqBundle()))
  val resp = DecoupledIO(new bitmapRespBundle())

  val pmp = new Bundle {
    val req = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }

  val wakeup = DecoupledIO(new BitmapWakeup())

  // bitmap cache req/resp and refill port
  val cache = new Bundle {
    val req = DecoupledIO(new bitmapCacheReqBundle())
    val resp = Flipped(DecoupledIO(new bitmapCacheRespBundle()))
  }
  val refill = Output(ValidIO(new Bundle {
    val tag = UInt(ppnLen.W)
    val data = UInt(XLEN.W)
  }))
}

class Bitmap(implicit p: Parameters) extends XSModule with HasPtwConst {
  def getRealPPN(ppn: UInt, vpn: UInt, level: UInt, n: Bool): UInt = {
    val nokeyid_ppn = Cat(0.U(KeyIDBits.W), ppn(ppnLen-KeyIDBits-1, 0))
    val check_ppn = Mux(csr.mbmc.KEYIDEN.asBool, nokeyid_ppn, ppn)
    val effective_ppn = MuxLookup(level, 0.U)(Seq(
      3.U -> Cat(check_ppn(ppnLen - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(check_ppn(ppnLen - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(check_ppn(ppnLen - 1, vpnnLen), vpn(vpnnLen - 1, 0)),
      0.U -> Mux(n === 0.U, check_ppn(ppnLen - 1, 0), Cat(check_ppn(ppnLen - 1, pteNapotBits), vpn(pteNapotBits - 1, 0)))
    ))
    effective_ppn
  }

  def getBitmapAddr(effective_ppn: UInt): UInt = {
    bitmap_base + (effective_ppn >> log2Ceil(XLEN) << log2Ceil(8))
  }

  val io = IO(new bitmapIO)

  val csr = io.csr
  val sfence = io.sfence
  val flush = sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed || csr.priv.virt_changed
  val bitmap_base = csr.mbmc.BMA << 6

  val entries = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize+2)(0.U.asTypeOf(new bitmapEntry()))))
  // add pmp check
  val state_idle :: state_addr_check :: state_cache_req :: state_cache_resp  ::state_mem_req :: state_mem_waiting :: state_mem_out :: Nil = Enum(7)
  val state = RegInit(VecInit(Seq.fill(l2tlbParams.llptwsize+2)(state_idle)))

  val is_emptys = state.map(_ === state_idle)
  val is_cache_req = state.map (_ === state_cache_req)
  val is_cache_resp = state.map (_ === state_cache_resp)
  val is_mems = state.map(_ === state_mem_req)
  val is_waiting = state.map(_ === state_mem_waiting)
  val is_having = state.map(_ === state_mem_out)

  val full = !ParallelOR(is_emptys).asBool
  val waiting = ParallelOR(is_waiting).asBool
  val enq_ptr = ParallelPriorityEncoder(is_emptys)

  val mem_ptr = ParallelPriorityEncoder(is_having)
  val mem_arb = Module(new RRArbiterInit(new bitmapEntry(), l2tlbParams.llptwsize+2))

  val bitmapdata = Wire(Vec(blockBits / XLEN, UInt(XLEN.W)))
  if (HasBitmapCheckDefault) {
    for (i <- 0 until blockBits / XLEN) {
      bitmapdata(i) := 0.U
    }
  } else {
    bitmapdata := io.mem.resp.bits.value.asTypeOf(Vec(blockBits / XLEN, UInt(XLEN.W)))
  }

  for (i <- 0 until l2tlbParams.llptwsize+2) {
    mem_arb.io.in(i).bits := entries(i)
    mem_arb.io.in(i).valid := is_mems(i) && !io.mem.req_mask(i)
  }

  val cache_req_arb = Module(new Arbiter(new bitmapCacheReqBundle(),l2tlbParams.llptwsize + 2))
  for (i <- 0 until l2tlbParams.llptwsize+2) {
    cache_req_arb.io.in(i).valid := is_cache_req(i)
    cache_req_arb.io.in(i).bits.tag := entries(i).ppn
    cache_req_arb.io.in(i).bits.order := i.U;
  }

  val req_real_ppn = getRealPPN(io.req.bits.bmppn, io.req.bits.vpn, io.req.bits.level, io.req.bits.n)

  val dup_vec = state.indices.map(i =>
    dupBitmapPPN(req_real_ppn, entries(i).ppn)
  )
  val dup_req_fire = mem_arb.io.out.fire && dupBitmapPPN(req_real_ppn, mem_arb.io.out.bits.ppn)
  val dup_vec_wait = dup_vec.zip(is_waiting).map{case (d, w) => d && w}
  val dup_wait_resp = io.mem.resp.fire && VecInit(dup_vec_wait)(io.mem.resp.bits.id - (l2tlbParams.llptwsize + 2).U)
  val wait_id = Mux(dup_req_fire, mem_arb.io.chosen, ParallelMux(dup_vec_wait zip entries.map(_.wait_id)))

  val to_wait = Cat(dup_vec_wait).orR || dup_req_fire
  val to_mem_out = dup_wait_resp

  val enq_state_normal = MuxCase(state_addr_check, Seq(
    to_mem_out -> state_mem_out,
    to_wait -> state_mem_waiting
  ))
  val enq_state =  enq_state_normal
  val enq_ptr_reg = RegNext(enq_ptr)

  val need_addr_check = RegNext(enq_state === state_addr_check && io.req.fire && !flush)

  io.pmp.req.valid := need_addr_check
  io.pmp.req.bits.addr := RegEnable(getBitmapAddr(req_real_ppn),io.req.fire)
  io.pmp.req.bits.cmd := TlbCmd.read
  io.pmp.req.bits.size := 3.U
  val pmp_resp_valid = io.pmp.req.valid

  when (io.req.fire) {
    state(enq_ptr) := enq_state
    entries(enq_ptr).ppn := req_real_ppn
    entries(enq_ptr).vpn := io.req.bits.vpn
    entries(enq_ptr).id := io.req.bits.id
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
    entries(enq_ptr).cf := false.B
    for (i <- 0 until tlbcontiguous) {
      entries(enq_ptr).cfs(i) := false.B
    }
    when (to_mem_out) {
      val index = getBitmapAddr(req_real_ppn)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
      entries(enq_ptr).cf := bitmapdata(index)(req_real_ppn(log2Up(XLEN)-1, 0))
      val ppnPart = req_real_ppn(log2Up(XLEN)-1, log2Up(8))
      val selectedBits = bitmapdata(index).asTypeOf(Vec(XLEN/8, UInt(8.W)))(ppnPart)
      for (j <- 0 until tlbcontiguous) {
        entries(enq_ptr).cfs(j) := selectedBits(j)
      }
    }
    entries(enq_ptr).hit := to_wait || to_mem_out
    entries(enq_ptr).level := io.req.bits.level
    entries(enq_ptr).way_info := io.req.bits.way_info
    entries(enq_ptr).hptw_bypassed := io.req.bits.hptw_bypassed
    entries(enq_ptr).n := io.req.bits.n
    entries(enq_ptr).s2xlate := io.req.bits.s2xlate
  }

  // when pmp check failed, use cf bit represent
  when (pmp_resp_valid) {
    val ptr = enq_ptr_reg
    val accessFault = io.pmp.resp.ld || io.pmp.resp.mmio
    entries(ptr).cf := accessFault
    for (i <- 0 until tlbcontiguous) {
      entries(ptr).cfs(i) := accessFault
    }
    // firstly req bitmap cache
    state(ptr) := Mux(accessFault, state_mem_out, state_cache_req)
  }

  val cache_wait = ParallelOR(is_cache_resp).asBool
  io.cache.resp.ready := !flush && cache_wait

  val hit = WireInit(false.B)
  io.cache.req.valid := cache_req_arb.io.out.valid && !flush
  io.cache.req.bits.tag := cache_req_arb.io.out.bits.tag
  io.cache.req.bits.order := cache_req_arb.io.out.bits.order
  cache_req_arb.io.out.ready := io.cache.req.ready


  when (cache_req_arb.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) === state_cache_req && cache_req_arb.io.chosen === i.U) {
        state(i) := state_cache_resp
      }
    }
  }

  when (io.cache.resp.fire) {
    for (i <- state.indices) {
      val cm_dup_vec = state.indices.map(j =>
        dupBitmapPPN(entries(i).ppn, entries(j).ppn)
      )
      val cm_dup_req_fire = mem_arb.io.out.fire && dupBitmapPPN(entries(i).ppn, mem_arb.io.out.bits.ppn)
      val cm_dup_vec_wait = cm_dup_vec.zip(is_waiting).map{case (d, w) => d && w}
      val cm_dup_wait_resp = io.mem.resp.fire && VecInit(cm_dup_vec_wait)(io.mem.resp.bits.id - (l2tlbParams.llptwsize + 2).U)
      val cm_wait_id = Mux(cm_dup_req_fire, mem_arb.io.chosen, ParallelMux(cm_dup_vec_wait zip entries.map(_.wait_id)))
      val cm_to_wait = Cat(cm_dup_vec_wait).orR || cm_dup_req_fire
      val cm_to_mem_out = cm_dup_wait_resp
      val cm_next_state_normal = MuxCase(state_mem_req, Seq(
        cm_to_mem_out -> state_mem_out,
        cm_to_wait -> state_mem_waiting
      ))
      when (state(i) === state_cache_resp && io.cache.resp.bits.order === i.U) {
          hit := io.cache.resp.bits.hit
          when (hit) {
            entries(i).cf := io.cache.resp.bits.cfs(entries(i).ppn(2,0))
            entries(i).hit := true.B
            entries(i).cfs := io.cache.resp.bits.cfs
            state(i) := state_mem_out
          } .otherwise {
            state(i) := cm_next_state_normal
            entries(i).wait_id := Mux(cm_to_wait, cm_wait_id, entries(i).wait_id)
            entries(i).hit := cm_to_wait || cm_to_mem_out
            when (cm_to_mem_out) {
              val index = getBitmapAddr(entries(i).ppn)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
              entries(i).cf := bitmapdata(index)(entries(i).ppn(log2Up(XLEN)-1,0))
              val ppnPart = entries(i).ppn(log2Up(XLEN)-1, log2Up(8))
              val selectedBits = bitmapdata(index).asTypeOf(Vec(XLEN/8, UInt(8.W)))(ppnPart)
              for (j <- 0 until tlbcontiguous) {
                entries(i).cfs(j) := selectedBits(j)
              }
            }
          }
      }
    }
  }

  when (mem_arb.io.out.fire) {
    for (i <- state.indices) {
      when (state(i) === state_mem_req && dupBitmapPPN(entries(i).ppn, mem_arb.io.out.bits.ppn)) {
        state(i) := state_mem_waiting
        entries(i).wait_id := mem_arb.io.chosen
      }
    }
  }

  when (io.mem.resp.fire) {
    state.indices.map{i =>
      when (state(i) === state_mem_waiting && io.mem.resp.bits.id === entries(i).wait_id + (l2tlbParams.llptwsize + 2).U) {
        state(i) := state_mem_out
        val index = getBitmapAddr(entries(i).ppn)(log2Up(l2tlbParams.blockBytes)-1, log2Up(XLEN/8))
        entries(i).data := bitmapdata(index)
        entries(i).cf := bitmapdata(index)(entries(i).ppn(log2Up(XLEN)-1, 0))
        val ppnPart = entries(i).ppn(log2Up(XLEN)-1, log2Up(8))
        val selectedBits = bitmapdata(index).asTypeOf(Vec(XLEN/8, UInt(8.W)))(ppnPart)
        for (j <- 0 until tlbcontiguous) {
          entries(i).cfs(j) := selectedBits(j)
        }
      }
    }
  }

  when (io.resp.fire) {
    state(mem_ptr) := state_idle
  }

  when (flush) {
    state.map(_ := state_idle)
  }

  io.req.ready := !full

  // io.resp.ready always ture
  val wakeup_valid_1cycle = io.resp.valid && !entries(mem_ptr).hptw_bypassed && entries(mem_ptr).level === 0.U && entries(mem_ptr).n === 0.U
  // when wakeup is stall, block resp valid too
  val wakeup_stall = {
    val valid = RegInit(false.B)
    when (wakeup_valid_1cycle) { valid := true.B }
    when (io.wakeup.fire) { valid := false.B }
    valid
  }

  io.resp.valid := ParallelOR(is_having).asBool && !wakeup_stall
  // if cache hit, resp the cache's resp
  io.resp.bits.cf := entries(mem_ptr).cf
  io.resp.bits.cfs := entries(mem_ptr).cfs
  io.resp.bits.id := entries(mem_ptr).id

  io.mem.req.valid := mem_arb.io.out.valid && !flush
  io.mem.req.bits.addr := getBitmapAddr(mem_arb.io.out.bits.ppn)
  io.mem.req.bits.id := mem_arb.io.chosen + (l2tlbParams.llptwsize + 2).U
  mem_arb.io.out.ready := io.mem.req.ready

  io.mem.resp.ready := waiting

  io.mem.req.bits.hptw_bypassed := false.B

  io.wakeup.valid := ValidHoldBypass(wakeup_valid_1cycle, io.wakeup.ready)
  io.wakeup.bits.setIndex := DataHoldBypass(genPtwL0SetIdx(entries(mem_ptr).vpn), wakeup_valid_1cycle)
  io.wakeup.bits.tag := DataHoldBypass(entries(mem_ptr).vpn(vpnLen - 1, vpnLen - SPTagLen), wakeup_valid_1cycle)
  io.wakeup.bits.way_info := DataHoldBypass(entries(mem_ptr).way_info, wakeup_valid_1cycle)
  io.wakeup.bits.pte_index := DataHoldBypass(entries(mem_ptr).vpn(sectortlbwidth - 1, 0), wakeup_valid_1cycle)
  io.wakeup.bits.check_success := DataHoldBypass(!entries(mem_ptr).cf, wakeup_valid_1cycle)
  io.wakeup.bits.s2xlate := DataHoldBypass(entries(mem_ptr).s2xlate, wakeup_valid_1cycle)

  // when don't hit, refill the data to bitmap cache
  io.refill.valid := io.resp.valid && !entries(mem_ptr).hit
  io.refill.bits.tag := entries(mem_ptr).ppn
  io.refill.bits.data := entries(mem_ptr).data

  XSPerfAccumulate("bitmap_req", io.req.fire)
  XSPerfAccumulate("bitmap_mem_req", io.mem.req.fire)
}

// add bitmap cache
class bitmapCacheReqBundle(implicit p: Parameters) extends PtwBundle{
  val order = UInt((l2tlbParams.llptwsize + 2).W)
  val tag = UInt(ppnLen.W)
}
class bitmapCacheRespBundle(implicit p: Parameters) extends PtwBundle{
  val hit = Bool()
  val cfs = Vec(tlbcontiguous,Bool())
  val order = UInt((l2tlbParams.llptwsize + 2).W)
  def apply(hit : Bool, cfs : Vec[Bool], order : UInt) = {
    this.hit := hit
    this.cfs := cfs
    this.order := order
  }
}
class bitmapCacheIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new bitmapCacheReqBundle()))
  val resp = DecoupledIO(new bitmapCacheRespBundle())
  val refill = Flipped(ValidIO(new Bundle {
    val tag = UInt(ppnLen.W)
    val data = UInt(XLEN.W)
  }))
}
class bitmapCacheEntry(ReservedBits: Int)(implicit p: Parameters) extends PtwBundle{
  val tag = UInt((ppnLen-log2Ceil(l2tlbParams.bcnSets)-log2Ceil(XLEN)).W)
  val data = UInt(XLEN.W) // store 64bits in one entry
  val reservedBits = if(ReservedBits > 0) Some(UInt(ReservedBits.W)) else None
  def hit(ppn : UInt) = {
    (this.tag === ppn(ppnLen-1,log2Ceil(l2tlbParams.bcnSets)+log2Ceil(XLEN)))
  }
  def refill(ppn : UInt,data : UInt) = {
    this.tag := ppn(ppnLen-1,log2Ceil(l2tlbParams.bcnSets)+log2Ceil(XLEN))
    this.data := data
    this.reservedBits.map(_ := true.B)
  }
}

class BitmapCache(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new bitmapCacheIO)

  val csr = io.csr
  val sfence = io.sfence
  val flush = sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed || csr.priv.virt_changed
  val bitmap_cache_clear = csr.mbmc.BCLEAR

  val bitmapCacheEntryType = new bitmapCacheEntry(ReservedBits = l2tlbParams.bcReservedBits)
  val bitmapcache = Module(new SplittedSRAM(
    bitmapCacheEntryType,
    set = l2tlbParams.bcnSets,
    way = l2tlbParams.bcnWays,
    waySplit = 2,
    dataSplit = 2,
    singlePort = sramSinglePort,
    readMCP2 = false,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))
  val mbistBC = MbistPipeline.PlaceMbistPipeline(1, s"MbistBitmapCache", hasMbist)

  val bitmapReplace = ReplacementPolicy.fromString(l2tlbParams.bcReplacer,l2tlbParams.bcnWays,l2tlbParams.bcnSets)

  val bcv = RegInit(0.U((l2tlbParams.bcnSets * l2tlbParams.bcnWays).W))
  def genPtwBCSetIdx(ppn: UInt) = {
    ppn(log2Ceil(l2tlbParams.bcnSets)+log2Ceil(XLEN)-1,log2Ceil(XLEN))
  }
  def getbcvSet(ppn: UInt) = {
    require(log2Up(l2tlbParams.bcnWays) == log2Down(l2tlbParams.bcnWays))
    val set = genPtwBCSetIdx(ppn)
    val bcvVec = bcv.asTypeOf(Vec(l2tlbParams.bcnSets, UInt(l2tlbParams.bcnWays.W)))
    bcvVec(set)
  }

  // when refill, refuce to accept new req
  val rwHarzad = io.refill.valid

  val stageReq   = Wire(Decoupled(new bitmapCacheReqBundle())) // enq stage & read cache valid
  val stageDelay = Wire(Decoupled(new bitmapCacheReqBundle())) // cache resp
  val stageResp  = Wire(Decoupled(new bitmapCacheReqBundle())) // check hit & deq stage

  val stageDelay_valid_1cycle = OneCycleValid(stageReq.fire, flush)
  val stageResp_valid_1cycle = OneCycleValid(stageResp.fire, flush)

  stageReq <> io.req
  PipelineConnect(stageReq, stageDelay, stageDelay.ready, flush, rwHarzad)
  PipelineConnect(stageDelay, stageResp, stageResp.ready, flush)
  stageResp.ready := !stageResp.valid || io.resp.ready

  if (EnableClockGate) {
    val te = ClockGate.genTeSink
    val bc_masked_clock = ClockGate(te.cgen, stageReq.fire | (!flush && io.refill.valid) | mbistBC.map(_.mbist.req).getOrElse(false.B), clock)
    bitmapcache.clock := bc_masked_clock
  }

  val ppn_search = stageReq.bits.tag
  val ridx = genPtwBCSetIdx(ppn_search)
  bitmapcache.io.r.req.valid := stageReq.fire
  bitmapcache.io.r.req.bits.apply(setIdx = ridx)
  val vVec_req = getbcvSet(ppn_search)

  // delay one cycle after sram read
  val delay_ppn = stageDelay.bits.tag
  val data_resp = DataHoldBypass(bitmapcache.io.r.resp.data, stageDelay_valid_1cycle)
  val vVec_delay = RegEnable(vVec_req, stageReq.fire)
  val hitVec_delay = VecInit(data_resp.zip(vVec_delay.asBools).map { case (wayData, v) => wayData.hit(delay_ppn) && v})

  // check hit and deq stage
  val resp_ppn = stageResp.bits.tag
  val ramDatas = RegEnable(data_resp, stageDelay.fire)
  val hitVec = RegEnable(hitVec_delay, stageDelay.fire)
  val hit = ParallelOR(hitVec)

  val hitWayEntry = ParallelPriorityMux(hitVec zip ramDatas)
  val hitWay = ParallelPriorityMux(hitVec zip (0 until l2tlbParams.bcnWays).map(_.U(log2Up(l2tlbParams.bcnWays).W)))

  when (hit && stageResp_valid_1cycle) { bitmapReplace.access(genPtwBCSetIdx(resp_ppn), hitWay) }

  val cfs = Wire(Vec(tlbcontiguous, Bool()))

  val cfsdata = hitWayEntry.data.asTypeOf(Vec(XLEN/8, UInt(8.W)))(resp_ppn(log2Up(XLEN)-1, log2Up(8)))
  for (i <- 0 until tlbcontiguous) {
    cfs(i) := cfsdata(i)
  }

  val resp_res = Wire(new bitmapCacheRespBundle())
  resp_res.apply(hit, cfs, stageResp.bits.order)

  io.resp.valid := stageResp.valid
  io.resp.bits := resp_res

  // refill
  bitmapcache.io.w.req <> DontCare
  bitmapcache.io.w.req.valid := false.B

  val bcRefillIdx = genPtwBCSetIdx(io.refill.bits.tag)
  val bcWdata = Wire(bitmapCacheEntryType)
  bcWdata.refill(io.refill.bits.tag, io.refill.bits.data)

  val bcVictimWay = replaceWrapper(getbcvSet(io.refill.bits.tag), bitmapReplace.way(bcRefillIdx)).suggestName(s"bc_victimWay")
  val bcVictimWayOH = UIntToOH(bcVictimWay).asUInt.suggestName(s"bc_victimWayOH")
  val bcRfvOH = UIntToOH(Cat(bcRefillIdx, bcVictimWay)).suggestName(s"bc_rfvOH")

  when (io.refill.valid) {
    bitmapcache.io.w.apply(
      valid = true.B,
      setIdx = bcRefillIdx,
      data = bcWdata,
      waymask = bcVictimWayOH
    )
    bitmapReplace.access(bcRefillIdx, bcVictimWay)
    bcv := bcv | bcRfvOH
  }
  when (bitmap_cache_clear === 1.U) {
    bcv := 0.U
  }

  XSPerfAccumulate("bitmap_cache_resp", io.resp.fire)
  XSPerfAccumulate("bitmap_cache_resp_miss", io.resp.fire && !io.resp.bits.hit)
}
