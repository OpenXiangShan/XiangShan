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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache._

trait HasStorePrefetchHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
  val PAGEOFFSET = 12 // page offset 4096 Bytes
  val BLOCKOFFSET = log2Up(dcacheParameters.blockBytes) // cache block offset 64 Bytes

  val STRIDE_PC_BITS = 10
  val STRIDE_ENTRY = 16
  val MAX_STRIDE = 1024
  val STRIDE_BLK_ADDR_BITS = log2Up(MAX_STRIDE)
  val BLOCK_ADDR_PAGE_BIT = log2Up(dcacheParameters.pageSize / dcacheParameters.blockBytes)

  val SATURATE_COUNTER_BITS = 7

  val ENABLE_STRIDE = false
  val ENABLE_SPB = true

  def block_addr(x: UInt): UInt = {
    val offset = log2Up(dcacheParameters.blockBytes)
    x(x.getWidth - 1, offset)
  }

  // filter logic (granularity: a page)
  def same_page_addr(addr0: UInt, addr1: UInt): Bool = {
    addr0(addr0.getWidth - 1, PAGEOFFSET) === addr1(addr1.getWidth - 1, PAGEOFFSET)
  }

  def filter_by_page_addr(valid_vec: Vec[Bool], data_vec: Vec[UInt], incoming_vaddr: UInt) : Bool = {
    val match_vec = (valid_vec zip data_vec).map{
      case(v, e_vaddr) => v && same_page_addr(e_vaddr, incoming_vaddr)
    }
    VecInit(match_vec).asUInt.orR
  }

  def cache_block_addr_difference(req_addr: UInt, last_addr: UInt): UInt = {
    (block_addr(req_addr).asSInt - block_addr(last_addr).asSInt)(SATURATE_COUNTER_BITS - 1, 0)
  }

  def get_store_count_divided_by_8(st_count: UInt): UInt = {
    st_count(st_count.getWidth - 1, 3)
  }

  def trigger_check(st_count: UInt, N: UInt): Bool = {
    st_count > N
  }

  def can_burst(st_count: UInt, N: UInt, sa_count: SInt): Bool = {
    // 1.counter overflows
    // 2.counter / 8 == saturate counter
    // 3.saturate counter is not negtive
    trigger_check(st_count, N) && get_store_count_divided_by_8(st_count) === sa_count.asUInt && sa_count(sa_count.getWidth - 1) === false.B
  }

  def pc_hash(x: UInt): UInt = {
    val width = STRIDE_PC_BITS
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }
}

// L1 Store prefetch component

// an prefetch request generator used by spb or lpb to burst some prefetch request to L1 Dcache
// class PrefetchBurstGenerator(is_store: Boolean)(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
//   val io = IO(new DCacheBundle {
//     val alloc = Input(Bool())
//     val vaddr = Input(UInt(VAddrBits.W))
//     val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
//   })

//   val SIZE = 2 // Buffer size

//   val valids = RegInit(VecInit(List.tabulate(SIZE){_ => false.B}))
//   val datas  = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U.asTypeOf(io.vaddr)}))
//   val pagebits = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U(1.W)}))

//   // enq
//   val enq_valids = ~(valids.asUInt)
//   val full = !(enq_valids.orR)
//   val enq_idx = PriorityEncoder(enq_valids)
//   val enq_filter = filter_by_page_addr(valids, datas, io.vaddr)

//   when(io.alloc && !full && !enq_filter) {
//     valids(enq_idx) := true.B
//     datas(enq_idx) := io.vaddr
//     pagebits(enq_idx) := io.vaddr(PAGEOFFSET)
//   }

//   XSPerfAccumulate("burst_generator_alloc_success", io.alloc && !full && !enq_filter)
//   XSPerfAccumulate("burst_generator_alloc_fail", io.alloc && full && !enq_filter)
//   XSPerfAccumulate("burst_generator_full", full)

//   // next prefetch address
//   val datas_next = Wire(Vec(SIZE, chiselTypeOf(datas(0))))
//   datas_next := datas.map(_ + Cat(1.U(1.W), 0.U(BLOCKOFFSET.W)))
//   // double next prefetch address
//   val datas_next_next = Wire(Vec(SIZE, chiselTypeOf(datas(0))))
//   datas_next_next := datas.map(_ + Cat(2.U(2.W), 0.U(BLOCKOFFSET.W)))

//   // deq
//   // val deq_valids = (valids zip datas zip pagebits).map{case (v, vaddr, pgbit) => v && vaddr(PAGEOFFSET) === pagebits}
//   val deq_valids = valids
//   val deq_decoupled = Wire(Vec(SIZE, Vec(StorePipelineWidth, Decoupled(new StorePrefetchReq))))

//   (deq_valids zip deq_decoupled zip datas zip datas_next zip datas_next_next zip pagebits zip valids).foreach{case ((((((deq_valid, out_decouple), data), data_next), data_next_next), pg_bit), v) => {
//     out_decouple(0).valid := deq_valid
//     out_decouple(0).bits := DontCare
//     out_decouple(0).bits.vaddr := data
//     out_decouple(1).valid := deq_valid && data_next(PAGEOFFSET) === pg_bit && out_decouple(0).fire
//     out_decouple(1).bits := DontCare
//     out_decouple(1).bits.vaddr := data_next
//     when(out_decouple(1).fire) {
//       data := data_next_next
//       when(data_next_next(PAGEOFFSET) =/= pg_bit) {
//         v := false.B
//       }
//     }.elsewhen(out_decouple(0).fire) {
//       data := data_next
//       when(data_next(PAGEOFFSET) =/= pg_bit) {
//         v := false.B
//       }
//     }
//   }}
//   for (i <- 0 until StorePipelineWidth) {
//     arbiter(deq_decoupled.map(_(i)), io.prefetch_req(i), Some(s"spb_deq_arb${i}"))
//   }

//   XSPerfAccumulate("burst_valid_num", PopCount(valids))
//   XSPerfAccumulate("prefetch_req_fire_by_generator", PopCount(VecInit(io.prefetch_req.map(_.fire))))
// }

// // spb(store prefetch burst)
// class StorePrefetchBurstsIO(implicit p: Parameters) extends DCacheBundle with HasStorePrefetchHelper {
//   val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddr)))
//   val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
// }

// // Virtual Address Prefetch
// class StorePrefetchBursts(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
//   val io = IO(new StorePrefetchBurstsIO)
//   require(EnsbufferWidth == 2)

//   // meta for SPB 
//   val N = BigInt(48)
//   val last_st_block_addr = RegInit(0.U(VAddrBits.W))
//   val saturate_counter = RegInit(0.U(SATURATE_COUNTER_BITS.W))
//   val store_count = RegInit(0.U((log2Up(N) + 1).W))
//   val burst_engine = Module(new PrefetchBurstGenerator(is_store = true))

//   val sbuffer_fire_vec  = VecInit(io.sbuffer_enq.map(_.valid))
//   val sbuffer_vaddr_vec = VecInit(io.sbuffer_enq.map(_.bits.vaddr))
//   val last_st_block_addr_vec = VecInit(last_st_block_addr) ++ sbuffer_vaddr_vec

//   val temp_store_count_vec = WireInit(VecInit(Seq.tabulate(EnsbufferWidth){case i => store_count + PopCount(sbuffer_fire_vec.take(i + 1))}))
//   val temp_saturate_count_vec = WireInit(VecInit(Seq.tabulate(EnsbufferWidth){case i => saturate_counter + ((last_st_block_addr_vec.take(i + 2) zipWithIndex) zip (VecInit(false.B) ++ sbuffer_fire_vec.take(i + 1))).map{
//     case ((vaddr, index), fire) => {
//       if(index == 0) {
//         0.U
//       }else {
//         Mux(fire, cache_block_addr_difference(vaddr, last_st_block_addr_vec(index - 1)), 0.U)
//       }
//     }
//   }.reduce(_ + _)}))

//   for(i <- 0 until EnsbufferWidth) {
//     when(sbuffer_fire_vec(i)) {
//       last_st_block_addr := sbuffer_vaddr_vec(i)
//     }
//   }
//   val check_vec = temp_store_count_vec.map{
//     case st_count => trigger_check(st_count, N.U)
//   }
//   val burst_vec = (temp_store_count_vec zip temp_saturate_count_vec).map{
//     case (st_count, sa_count) => {
//       can_burst(st_count, N.U, sa_count)
//     }
//   }
//   val do_check = VecInit(check_vec).asUInt.orR
//   val trigger_burst = VecInit(burst_vec).asUInt.orR
//   val burst_idx = PriorityEncoder(burst_vec)

//   store_count := Mux(trigger_burst || do_check, 0.U, temp_store_count_vec.last)
//   saturate_counter := Mux(trigger_burst || do_check, 0.U, temp_saturate_count_vec.last)

//   burst_engine.io.alloc := trigger_burst
//   burst_engine.io.vaddr := io.sbuffer_enq(burst_idx).bits.vaddr
//   burst_engine.io.prefetch_req <> io.prefetch_req

//   // perf 
//   XSPerfAccumulate("trigger_burst", trigger_burst)
//   XSPerfAccumulate("trigger_check", do_check)
// }


// // an simple lpb (load prefetch burst)
// class LoadPrefetchBurstsIO(implicit p: Parameters) extends DCacheBundle with HasStorePrefetchHelper {
//     val lq_deq  = Flipped(Valid(new DCacheWordReqWithVaddr))
//     val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
// }

// // Virtual Address Prefetch
// class LoadPrefetchBursts(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
//   val io = IO(new LoadPrefetchBurstsIO)

//   // meta for SPB 
//   val N = BigInt(15)
//   val last_ld_block_addr = RegInit(0.U(VAddrBits.W))
//   val saturate_counter = RegInit(0.U(SATURATE_COUNTER_BITS.W))
//   val load_count = RegInit(0.U(6.W))
//   val burst_engine = Module(new PrefetchBurstGenerator(is_store = false))

//   val lq_deq_fire = io.lq_deq.fire
//   val lq_vaddr = io.lq_deq.bits.vaddr

//   val next_load_count = load_count + Mux(lq_deq_fire, 1.U, 0.U)
//   val next_saturate_count = saturate_counter + Mux(lq_deq_fire, cache_block_addr_difference(lq_vaddr, last_ld_block_addr), 0.U)

//   when(lq_deq_fire) {
//     last_ld_block_addr := lq_vaddr
//   }

//   val check = trigger_check(next_load_count, N.U)
//   val burst = can_burst(next_load_count, N.U, next_saturate_count)

//   load_count := Mux(burst || check, 0.U, next_load_count)
//   saturate_counter := Mux(burst || check, 0.U, next_saturate_count)

//   burst_engine.io.alloc := burst
//   burst_engine.io.vaddr := io.lq_deq.bits.vaddr
//   burst_engine.io.prefetch_req <> io.prefetch_req

//   // perf 
//   XSPerfAccumulate("trigger_burst", burst)
//   XSPerfAccumulate("trigger_check", check)
// }


// L2 Store prefetch component

// an prefetch request generator used by spb to burst some prefetch request to L2 Cache
class L2PrefetchBurstGenerator(is_store: Boolean)(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val alloc = Input(Bool())
    val paddr = Input(UInt(PAddrBits.W))
    val prefetch_req = DecoupledIO(new StorePrefetchReq)
  })

  val SIZE = 2 // Buffer size

  val valids = RegInit(VecInit(List.tabulate(SIZE){_ => false.B}))
  val datas  = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U.asTypeOf(io.paddr)}))
  val pagebits = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U(1.W)}))

  // enq
  val enq_valids = ~(valids.asUInt)
  val full = !(enq_valids.orR)
  val enq_idx = PriorityEncoder(enq_valids)
  val enq_filter = filter_by_page_addr(valids, datas, io.paddr)

  when(io.alloc && !full && !enq_filter) {
    valids(enq_idx) := true.B
    datas(enq_idx) := io.paddr
    pagebits(enq_idx) := io.paddr(PAGEOFFSET)
  }

  XSPerfAccumulate("burst_generator_alloc_success", io.alloc && !full && !enq_filter)
  XSPerfAccumulate("burst_generator_alloc_fail", io.alloc && full && !enq_filter)
  XSPerfAccumulate("burst_generator_full", full)

  // next prefetch address
  val datas_next = Wire(Vec(SIZE, chiselTypeOf(datas(0))))
  datas_next := datas.map(_ + Cat(1.U(1.W), 0.U(BLOCKOFFSET.W)))

  // deq
  // val deq_valids = (valids zip datas zip pagebits).map{case (v, vaddr, pgbit) => v && vaddr(PAGEOFFSET) === pagebits}
  val deq_valids = valids
  val deq_decoupled = Wire(Vec(SIZE, DecoupledIO(new StorePrefetchReq)))

  (deq_valids zip deq_decoupled zip datas zip datas_next zip pagebits zip valids).foreach{case (((((deq_valid, out_decouple), data), data_next), pg_bit), v) => {
    out_decouple.valid := deq_valid
    out_decouple.bits := DontCare
    out_decouple.bits.paddr := data
    when(out_decouple.fire) {
      data := data_next
      when(data_next(PAGEOFFSET) =/= pg_bit) {
        v := false.B
      }
    }
  }}

  arbiter(deq_decoupled, io.prefetch_req, Some("l2_store_prefetch_arb"))

  XSPerfAccumulate("burst_valid_num", PopCount(valids))
  XSPerfAccumulate("prefetch_req_fire_by_generator", PopCount(VecInit(io.prefetch_req.fire)))
}

// Physical Address Prefetch
class L2StorePrefetchBursts(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val enable = Input(Bool())
    val sbuffer_enq  = Flipped(Valid(new DCacheWordReqWithVaddr))
    val prefetch_req = DecoupledIO(new StorePrefetchReq)
  })
  require(EnsbufferWidth == 2)

  // meta for SPB 
  val N = BigInt(48)
  val last_st_block_addr = RegInit(0.U(PAddrBits.W))
  val saturate_counter = RegInit(0.S(SATURATE_COUNTER_BITS.W))
  val store_count = RegInit(0.U((log2Up(N) + 1).W))
  val burst_engine = Module(new L2PrefetchBurstGenerator(is_store = true))

  val sbuffer_fire = io.sbuffer_enq.valid
  val sbuffer_paddr = io.sbuffer_enq.bits.addr

  val next_store_count = store_count + Mux(sbuffer_fire, 1.U, 0.U)
  val next_saturate_count = (saturate_counter + Mux(sbuffer_fire, cache_block_addr_difference(sbuffer_paddr, last_st_block_addr).asSInt, 0.S)).asSInt

  when(sbuffer_fire) {
    last_st_block_addr := sbuffer_paddr
  }

  val check = trigger_check(next_store_count, N.U)
  val burst = can_burst(next_store_count, N.U, next_saturate_count)

  store_count := Mux(burst || check, 0.U, next_store_count)
  saturate_counter := Mux(burst || check, 0.S, next_saturate_count)

  burst_engine.io.alloc := burst && io.enable
  burst_engine.io.paddr := io.sbuffer_enq.bits.addr
  burst_engine.io.prefetch_req <> io.prefetch_req

  // perf 
  XSPerfAccumulate("trigger_burst", burst && io.enable)
  XSPerfAccumulate("trigger_check", check && io.enable)
}

// Store Stride Prefetch

// Serializer: FIFO queue, recieve EnsbufferWidth requests sent from sq to sbuffer
//             save them to a FIFO queue, pop them in order
class Serializer(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddrAndPc)))
    val prefetch_train = DecoupledIO(new DCacheWordReqWithVaddrAndPc)
  })
  val QueueSize = 12

  class SerializerPtr(implicit p: Parameters) extends CircularQueuePtr[SerializerPtr](
    p => QueueSize
  ){
  }

  object SerializerPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): SerializerPtr = {
      val ptr = Wire(new SerializerPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val enqPtrExt = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new SerializerPtr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new SerializerPtr))

  val deqPtr = deqPtrExt.value

  val reqs = RegInit(VecInit((0 until QueueSize).map(_.U.asTypeOf(Valid(new DCacheWordReqWithVaddrAndPc)))))

  // deq
  io.prefetch_train.valid := reqs(deqPtr).valid
  io.prefetch_train.bits  := reqs(deqPtr).bits

  when(io.prefetch_train.fire) {
    deqPtrExt := deqPtrExt + 1.U
    reqs(deqPtr).valid := false.B
  }

  // enq
  val count_vsreq = PopCount(io.sbuffer_enq.map(_.valid))
  val canEnqueue = (distanceBetween(enqPtrExt(0), deqPtrExt) + count_vsreq) <= QueueSize.U

  when(canEnqueue) {
    for(i <- 0 until EnsbufferWidth) {
      when(io.sbuffer_enq(i).valid) {
        reqs(enqPtrExt(i).value) := io.sbuffer_enq(i)
      }
    }
    enqPtrExt.map(ptr => ptr := ptr + count_vsreq)
  }

  XSPerfAccumulate("canNotEnqueue", !canEnqueue)
  XSPerfAccumulate("prefetch_train_fire", io.prefetch_train.fire)
  XSPerfAccumulate("full", PopCount(reqs.map(_.valid)) === QueueSize.U)
}

class StoreStridePF()(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new Bundle() {
    val stride_en = Input(Bool())
    val s0_lookup = Flipped(new DecoupledIO(new Bundle() {
      val pc = UInt(STRIDE_PC_BITS.W)
      val vaddr = UInt(VAddrBits.W)
      val paddr = UInt(PAddrBits.W)
    }))
    val s2_gen_req = ValidIO(new StorePrefetchReq())
  })

  val prev_valid = RegNext(io.s0_lookup.fire, false.B)
  val prev_pc = RegEnable(io.s0_lookup.bits.pc, io.s0_lookup.fire)

  val s0_valid = io.s0_lookup.fire

  io.s0_lookup.ready := !(prev_valid && prev_pc === io.s0_lookup.bits.pc)

  def entry_map[T](fn: Int => T) = (0 until STRIDE_ENTRY).map(fn)

  val replacement = ReplacementPolicy.fromString("plru", STRIDE_ENTRY)
  val valids = entry_map(_ => RegInit(false.B))
  val entries_pc = entry_map(_ => Reg(UInt(STRIDE_PC_BITS.W)))
  val entries_conf = entry_map(_ => RegInit(1.U(2.W)))
  val entries_last_addr = entry_map(_ => Reg(UInt(STRIDE_BLK_ADDR_BITS.W)))
  val entries_stride = entry_map(_ => Reg(SInt((STRIDE_BLK_ADDR_BITS+1).W)))


  val s0_match_vec = valids.zip(entries_pc).map({
    case (v, pc) => v && pc === io.s0_lookup.bits.pc
  })

  val s0_hit = s0_valid && Cat(s0_match_vec).orR
  val s0_miss = s0_valid && !s0_hit
  val s0_matched_conf = Mux1H(s0_match_vec, entries_conf)
  val s0_matched_last_addr = Mux1H(s0_match_vec, entries_last_addr)
  val s0_matched_last_stride = Mux1H(s0_match_vec, entries_stride)


  val s1_vaddr = RegEnable(io.s0_lookup.bits.vaddr, s0_valid)
  val s1_paddr = RegEnable(io.s0_lookup.bits.paddr, s0_valid)
  val s1_hit = RegNext(s0_hit)
  val s1_alloc = RegNext(s0_miss)
  val s1_conf = RegNext(s0_matched_conf)
  val s1_last_addr = RegNext(s0_matched_last_addr)
  val s1_last_stride = RegNext(s0_matched_last_stride)
  val s1_match_vec = RegNext(VecInit(s0_match_vec))

  val s1_new_stride_vaddr = s1_vaddr(BLOCKOFFSET + STRIDE_BLK_ADDR_BITS - 1, BLOCKOFFSET)
  val s1_new_stride = (0.U(1.W) ## s1_new_stride_vaddr).asSInt - (0.U(1.W) ## s1_last_addr).asSInt
  val s1_stride_non_zero = s1_last_stride =/= 0.S
  val s1_new_stride_non_zero = s1_new_stride =/= 0.S
  val s1_stride_match = s1_new_stride === s1_last_stride && s1_stride_non_zero
  val s1_replace_idx = replacement.way

  for(i <- 0 until STRIDE_ENTRY){
    val alloc = s1_alloc && i.U === s1_replace_idx
    val update = s1_hit && s1_match_vec(i)
    when(update){
      assert(valids(i))
      when(s1_new_stride_non_zero) {
        entries_conf(i) := Mux(s1_stride_match,
          Mux(s1_conf === 3.U, 3.U, s1_conf + 1.U),
          Mux(s1_conf === 0.U, 0.U, s1_conf - 1.U)
        )
      }
      entries_last_addr(i) := s1_new_stride_vaddr
      // if new stride is 0 which indicates that 
      // the new store is in the same cache block of last store
      // so dont update stride, otherwise the stride will be 0
      when(!s1_conf(1) && s1_new_stride_non_zero){
        entries_stride(i) := s1_new_stride
      }
    }
    when(alloc){
      valids(i) := true.B
      entries_pc(i) := prev_pc
      entries_conf(i) := 0.U
      entries_last_addr(i) := s1_new_stride_vaddr
      entries_stride(i) := 0.S
    }
    assert(!(update && alloc))
  }
  when(s1_hit){
    replacement.access(OHToUInt(s1_match_vec.asUInt))
  }.elsewhen(s1_alloc){
    replacement.access(s1_replace_idx)
  }

  val s1_block_vaddr = block_addr(s1_vaddr)
  val s1_pf_block_vaddr = (s1_block_vaddr.asSInt + s1_last_stride).asUInt
  val s1_pf_cross_page = s1_pf_block_vaddr(BLOCK_ADDR_PAGE_BIT) =/= s1_block_vaddr(BLOCK_ADDR_PAGE_BIT)

  val s2_pf_gen_valid = RegNext(s1_hit && s1_stride_match, false.B)
  val s2_pf_gen_paddr_valid = RegEnable(!s1_pf_cross_page, s1_hit && s1_stride_match)
  val s2_pf_block_vaddr = RegEnable(s1_pf_block_vaddr, s1_hit && s1_stride_match)
  val s2_block_paddr = RegEnable(block_addr(s1_paddr), s1_hit && s1_stride_match)

  val s2_pf_block_addr = Mux(s2_pf_gen_paddr_valid,
    Cat(
      s2_block_paddr(PAddrBits - BLOCKOFFSET - 1, BLOCK_ADDR_PAGE_BIT),
      s2_pf_block_vaddr(BLOCK_ADDR_PAGE_BIT - 1, 0)
    ),
    s2_pf_block_vaddr
  )
  val s2_pf_full_addr = Wire(UInt(PAddrBits.W))
  s2_pf_full_addr := s2_pf_block_addr ## 0.U(BLOCKOFFSET.W)

  val s2_full_vaddr = Wire(UInt(VAddrBits.W))
  s2_full_vaddr := s2_pf_block_vaddr ## 0.U(BLOCKOFFSET.W)

  io.s2_gen_req.valid := s2_pf_gen_valid && io.stride_en && s2_pf_gen_paddr_valid
  io.s2_gen_req.bits.paddr := s2_pf_full_addr
  io.s2_gen_req.bits.vaddr := s2_full_vaddr

  XSPerfAccumulate("s2_gen_req_valid", s2_pf_gen_valid && io.stride_en)
  XSPerfAccumulate("s1_stride_match", s1_stride_match)
}

class StorePfWrapper()(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddrAndPc)))
    val prefetch_req = DecoupledIO(new StorePrefetchReq)
  })

  val serializer_of_stride = Module(new Serializer())
  val serializer_of_spb = Module(new Serializer())
  val storeStridePF = Module(new StoreStridePF())
  val spb = Module(new L2StorePrefetchBursts())

  // give mutiple reqs to serializer, serializer will give out one req per cycle
  serializer_of_stride.io.sbuffer_enq <> io.sbuffer_enq
  serializer_of_spb.io.sbuffer_enq <> io.sbuffer_enq

  // train stride
  storeStridePF.io.stride_en := ENABLE_STRIDE.B
  storeStridePF.io.s0_lookup.valid := serializer_of_stride.io.prefetch_train.valid
  storeStridePF.io.s0_lookup.bits.pc := pc_hash(serializer_of_stride.io.prefetch_train.bits.pc)
  storeStridePF.io.s0_lookup.bits.vaddr := serializer_of_stride.io.prefetch_train.bits.vaddr
  storeStridePF.io.s0_lookup.bits.paddr := serializer_of_stride.io.prefetch_train.bits.addr
  // stride may reject some specific train req, so connect the ready signal
  serializer_of_stride.io.prefetch_train.ready := storeStridePF.io.s0_lookup.ready

  // train spb
  spb.io.enable := ENABLE_SPB.B
  spb.io.sbuffer_enq.valid := serializer_of_spb.io.prefetch_train.valid
  spb.io.sbuffer_enq.bits  := serializer_of_spb.io.prefetch_train.bits.toDCacheWordReqWithVaddr()
  // spb will always recieve train req
  serializer_of_spb.io.prefetch_train.ready := true.B

  // fire a prefetch req: stride has higher priority than spb
  io.prefetch_req.valid := storeStridePF.io.s2_gen_req.valid || spb.io.prefetch_req.valid
  io.prefetch_req.bits := Mux(storeStridePF.io.s2_gen_req.valid, storeStridePF.io.s2_gen_req.bits, spb.io.prefetch_req.bits)

  // if no stride prefetch, and prefetch channel is free, give ready to spb to help it deq
  spb.io.prefetch_req.ready := !storeStridePF.io.s2_gen_req.valid && io.prefetch_req.ready
}

class EastLakeLikeStorePfWrapper()(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddrAndPc)))
    val prefetch_req = DecoupledIO(new StorePrefetchReq)
  })

  val serializer_of_stride = Module(new Serializer())
  val storeStridePF = Module(new StoreStridePF())

  // give mutiple reqs to serializer, serializer will give out one req per cycle
  serializer_of_stride.io.sbuffer_enq <> io.sbuffer_enq

  // train stride
  storeStridePF.io.stride_en := ENABLE_STRIDE.B
  storeStridePF.io.s0_lookup.valid := serializer_of_stride.io.prefetch_train.valid
  storeStridePF.io.s0_lookup.bits.pc := pc_hash(serializer_of_stride.io.prefetch_train.bits.pc)
  storeStridePF.io.s0_lookup.bits.vaddr := serializer_of_stride.io.prefetch_train.bits.vaddr
  storeStridePF.io.s0_lookup.bits.paddr := serializer_of_stride.io.prefetch_train.bits.addr
  // stride may reject some specific train req, so connect the ready signal
  serializer_of_stride.io.prefetch_train.ready := storeStridePF.io.s0_lookup.ready

  // fire a prefetch req: stride has higher priority than spb
  io.prefetch_req.valid := storeStridePF.io.s2_gen_req.valid
  io.prefetch_req.bits := storeStridePF.io.s2_gen_req.bits
}