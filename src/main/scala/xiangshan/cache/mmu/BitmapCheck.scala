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
}

class bitmapRespBundle(implicit p: Parameters) extends XSBundle with HasPtwConst {
    val cf = Bool()
    val cfs = Vec(tlbcontiguous,Bool())
    val id = UInt(log2Up(l2tlbParams.llptwsize+2).W)
}

class bitmapEntry(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val ppn = UInt(ppnLen.W)
  val vpn = UInt(vpnLen.W)
  val id = UInt(bMemID.W)
  val wait_id = UInt(log2Up(l2tlbParams.llptwsize+2).W)
  // bitmap check faild? : 0 success, 1 faild
  val cf = Bool()
  val hit = Bool()
  val cfs = Vec(tlbcontiguous,Bool())
  val level = UInt(log2Up(Level).W)
  val way_info = UInt(l2tlbParams.l0nWays.W)
  val hptw_bypassed = Bool()
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

  val wakeup = ValidIO(new Bundle {
    val setIndex = UInt(PtwL0SetIdxLen.W)
    val tag = UInt(SPTagLen.W)
    val isSp = Bool()
    val way_info = UInt(l2tlbParams.l0nWays.W)
    val pte_index = UInt(sectortlbwidth.W)
    val check_success = Bool()
  })

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
  def getBitmapAddr(ppn: UInt): UInt = {
    val effective_ppn = ppn(ppnLen-KeyIDBits-1, 0)
    bitmap_base + (effective_ppn >> log2Ceil(XLEN) << log2Ceil(8))
  }

  val io = IO(new bitmapIO)

  val csr = io.csr
  val sfence = io.sfence
  val flush = sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed
  val bitmap_base = csr.mbmc.BMA << 6

  val entries = Reg(Vec(l2tlbParams.llptwsize+2, new bitmapEntry()))
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
  val mem_arb = Module(new RRArbiter(new bitmapEntry(), l2tlbParams.llptwsize+2))

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

  val dup_vec = state.indices.map(i =>
    dupBitmapPPN(io.req.bits.bmppn, entries(i).ppn)
  )
  val dup_req_fire = mem_arb.io.out.fire && dupBitmapPPN(io.req.bits.bmppn, mem_arb.io.out.bits.ppn)
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
  io.pmp.req.bits.addr := RegEnable(getBitmapAddr(io.req.bits.bmppn),io.req.fire)
  io.pmp.req.bits.cmd := TlbCmd.read
  io.pmp.req.bits.size := 3.U 
  val pmp_resp_valid = io.pmp.req.valid 

  when (io.req.fire) {
    state(enq_ptr) := enq_state
    entries(enq_ptr).ppn := io.req.bits.bmppn
    entries(enq_ptr).vpn := io.req.bits.vpn
    entries(enq_ptr).id := io.req.bits.id
    entries(enq_ptr).wait_id := Mux(to_wait, wait_id, enq_ptr)
    entries(enq_ptr).cf := false.B
    for (i <- 0 until tlbcontiguous) {
      entries(enq_ptr).cfs(i) := false.B
    }
    entries(enq_ptr).hit := to_wait
    entries(enq_ptr).level := io.req.bits.level
    entries(enq_ptr).way_info := io.req.bits.way_info
    entries(enq_ptr).hptw_bypassed := io.req.bits.hptw_bypassed
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
            entries(i).cf := io.cache.resp.bits.cfs(entries(i).ppn(5,0))
            entries(i).hit := true.B
            entries(i).cfs := io.cache.resp.bits.cfs
            state(i) := state_mem_out
          } .otherwise {
            state(i) := cm_next_state_normal
            entries(i).wait_id := Mux(cm_to_wait, cm_wait_id, entries(i).wait_id)
            entries(i).hit := cm_to_wait
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
        entries(i).cf := bitmapdata(index)(entries(i).ppn(5,0))
        val ppnPart = entries(i).ppn(5,3)
        val start = (ppnPart << 3.U)
        val end = start + 7.U
        val mask = (1.U << 8) - 1.U
        val selectedBits = (bitmapdata(index) >> start) & mask
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

  io.resp.valid := ParallelOR(is_having).asBool
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

  io.wakeup.valid := io.resp.valid && !entries(mem_ptr).hptw_bypassed
  io.wakeup.bits.setIndex := genPtwL0SetIdx(entries(mem_ptr).vpn)
  io.wakeup.bits.tag := entries(mem_ptr).vpn(vpnLen - 1, vpnLen - SPTagLen)
  io.wakeup.bits.isSp := entries(mem_ptr).level =/= 0.U
  io.wakeup.bits.way_info := entries(mem_ptr).way_info
  io.wakeup.bits.pte_index := entries(mem_ptr).vpn(sectortlbwidth - 1, 0)
  io.wakeup.bits.check_success := !entries(mem_ptr).cf

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
class bitmapCacheEntry(implicit p: Parameters) extends PtwBundle{
  val tag = UInt((ppnLen-log2Ceil(XLEN)).W)
  val data = UInt(XLEN.W) // store 64bits in one entry
  val valid = Bool()
  def hit(tag : UInt) = {
    (this.tag === tag(ppnLen-1,log2Ceil(XLEN))) && this.valid === 1.B
  }
  def refill(tag : UInt,data : UInt,valid : Bool) = {
    this.tag := tag(ppnLen-1,log2Ceil(XLEN))
    this.data := data
    this.valid := valid
  }
}

class BitmapCache(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new bitmapCacheIO)

  val csr = io.csr
  val sfence = io.sfence
  val flush = sfence.valid || csr.satp.changed || csr.vsatp.changed || csr.hgatp.changed
  val bitmap_cache_clear = csr.mbmc.BCLEAR

  val bitmapCachesize = 16
  val bitmapcache = Reg(Vec(bitmapCachesize,new bitmapCacheEntry()))
  val bitmapReplace = ReplacementPolicy.fromString(l2tlbParams.l3Replacer, bitmapCachesize)

  // -----
  // -S0--
  // -----
  val addr_search = io.req.bits.tag
  val hitVecT = bitmapcache.map(_.hit(addr_search))

  // -----
  // -S1--
  // -----
  val index = RegEnable(addr_search(log2Up(XLEN)-1,0), io.req.fire)
  val order = RegEnable(io.req.bits.order, io.req.fire)
  val hitVec = RegEnable(VecInit(hitVecT), io.req.fire)
  val CacheData = RegEnable(ParallelPriorityMux(hitVecT zip bitmapcache.map(_.data)), io.req.fire)
  val cfs = Wire(Vec(tlbcontiguous, Bool()))

  val start = (index(5, 3) << 3.U)
  val end = start + 7.U
  val mask = (1.U << 8) - 1.U
  val cfsdata = (CacheData >> start) & mask
  for (i <- 0 until tlbcontiguous) {
    cfs(i) := cfsdata(i)
  }
  val hit = ParallelOR(hitVec)

  val resp_res = Wire(new bitmapCacheRespBundle())
  resp_res.apply(hit,cfs,order)

  val resp_valid_reg = RegInit(false.B)
  when (flush) {
    resp_valid_reg := false.B
  } .elsewhen(io.req.fire) {
    resp_valid_reg := true.B
  } .elsewhen(io.resp.fire) {
    resp_valid_reg := false.B
  } .otherwise {
    resp_valid_reg := resp_valid_reg
  }

  io.req.ready := !resp_valid_reg || io.resp.fire
  io.resp.valid := resp_valid_reg
  io.resp.bits := resp_res

  when (!flush && hit && io.resp.fire) {
    bitmapReplace.access(OHToUInt(hitVec))
  }

  // -----
  // refill
  // -----
  val rf_addr = io.refill.bits.tag
  val rf_data = io.refill.bits.data
  val rf_vd = io.refill.valid
  when (!flush && rf_vd) {
    val refillindex = bitmapReplace.way
    dontTouch(refillindex)
    bitmapcache(refillindex).refill(rf_addr,rf_data,true.B)
    bitmapReplace.access(refillindex)
  }
  when (bitmap_cache_clear === 1.U) {
    bitmapcache.foreach(_.valid := false.B) 
  }

  XSPerfAccumulate("bitmap_cache_resp", io.resp.fire)
  XSPerfAccumulate("bitmap_cache_resp_miss", io.resp.fire && !io.resp.bits.hit)
}
