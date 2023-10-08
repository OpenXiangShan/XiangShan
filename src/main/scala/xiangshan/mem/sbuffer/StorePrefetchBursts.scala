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
import xiangshan._
import utils._
import utility._
import xiangshan.cache._

trait HasStorePrefetchHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
  // common
  val PAGEOFFSET = 12 // page offset 4096 Bytes
  val BLOCKOFFSET = log2Up(dcacheParameters.blockBytes) // cache block offset 64 Bytes

  // spb parameters
  val ENABLE_SPB = EnableStorePrefetchSPB
  val ONLY_ON_MEMSET = false
  val SATURATE_COUNTER_BITS = 7
  val BURST_ENGINE_SIZE = 2
  val SPB_N = 48

  // serializer parameters
  val SERIALIZER_SIZE = 12

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
}

// L1 Store prefetch component

// an prefetch request generator used by spb to burst some prefetch request to L1 Dcache
class PrefetchBurstGenerator(is_store: Boolean)(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val alloc = Input(Bool())
    val vaddr = Input(UInt(VAddrBits.W))
    val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
  })

  require(StorePipelineWidth == 2)

  val SIZE = BURST_ENGINE_SIZE

  val valids = RegInit(VecInit(List.tabulate(SIZE){_ => false.B}))
  val datas  = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U.asTypeOf(io.vaddr)}))
  val pagebits = RegInit(VecInit(List.tabulate(SIZE){_ => 0.U(1.W)}))

  // enq
  val enq_valids = ~(valids.asUInt)
  val full = !(enq_valids.orR)
  val enq_idx = PriorityEncoder(enq_valids)
  val enq_filter = filter_by_page_addr(valids, datas, io.vaddr)

  when(io.alloc && !full && !enq_filter) {
    valids(enq_idx) := true.B
    datas(enq_idx) := io.vaddr
    pagebits(enq_idx) := io.vaddr(PAGEOFFSET)
  }

  XSPerfAccumulate("burst_generator_alloc_success", io.alloc && !full && !enq_filter)
  XSPerfAccumulate("burst_generator_alloc_fail", io.alloc && full && !enq_filter)
  XSPerfAccumulate("burst_generator_full", full)

  // next prefetch address
  val datas_next = Wire(Vec(SIZE, chiselTypeOf(datas(0))))
  datas_next := datas.map(_ + Cat(1.U(1.W), 0.U(BLOCKOFFSET.W)))
  // double next prefetch address
  val datas_next_next = Wire(Vec(SIZE, chiselTypeOf(datas(0))))
  datas_next_next := datas.map(_ + Cat(2.U(2.W), 0.U(BLOCKOFFSET.W)))

  // deq
  // val deq_valids = (valids zip datas zip pagebits).map{case (v, vaddr, pgbit) => v && vaddr(PAGEOFFSET) === pagebits}
  val deq_valids = valids
  val deq_decoupled = Wire(Vec(SIZE, Vec(StorePipelineWidth, Decoupled(new StorePrefetchReq))))

  (deq_valids zip deq_decoupled zip datas zip datas_next zip datas_next_next zip pagebits zip valids).foreach{case ((((((deq_valid, out_decouple), data), data_next), data_next_next), pg_bit), v) => {
    out_decouple(0).valid := deq_valid
    out_decouple(0).bits := DontCare
    out_decouple(0).bits.vaddr := data
    out_decouple(1).valid := deq_valid && data_next(PAGEOFFSET) === pg_bit && out_decouple(0).fire
    out_decouple(1).bits := DontCare
    out_decouple(1).bits.vaddr := data_next
    when(out_decouple(1).fire) {
      // fired 2 prefetch reqs
      data := data_next_next
      when(data_next_next(PAGEOFFSET) =/= pg_bit) {
        // cross page, invalid this entry
        v := false.B
      }
    }.elsewhen(out_decouple(0).fire) {
      // fired 1 prefetch req
      data := data_next
      when(data_next(PAGEOFFSET) =/= pg_bit) {
        // cross page, invalid this entry
        v := false.B
      }
    }
  }}
  for (i <- 0 until StorePipelineWidth) {
    arbiter(deq_decoupled.map(_(i)), io.prefetch_req(i), Some(s"spb_deq_arb${i}"))
  }

  XSPerfAccumulate("burst_valid_num", PopCount(valids))
  XSPerfAccumulate("prefetch_req_fire_by_generator", PopCount(VecInit(io.prefetch_req.map(_.fire))))
}

class StorePrefetchBursts(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val enable = Input(Bool())
    val memSetPattenDetected = Input(Bool())
    val sbuffer_enq  = Flipped(Valid(new DCacheWordReqWithVaddr))
    val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
  })
  require(EnsbufferWidth == 2)

  // meta for SPB
  val N = SPB_N
  val last_st_block_addr = RegInit(0.U(VAddrBits.W))
  val saturate_counter = RegInit(0.S(SATURATE_COUNTER_BITS.W))
  val store_count = RegInit(0.U((log2Up(N) + 1).W))
  val burst_engine = Module(new PrefetchBurstGenerator(is_store = true))

  val sbuffer_fire = io.sbuffer_enq.valid
  val sbuffer_vaddr = io.sbuffer_enq.bits.vaddr

  val next_store_count = store_count + Mux(sbuffer_fire, 1.U, 0.U)
  val next_saturate_count = (saturate_counter + Mux(sbuffer_fire, cache_block_addr_difference(sbuffer_vaddr, last_st_block_addr).asSInt, 0.S)).asSInt

  when(sbuffer_fire) {
    last_st_block_addr := sbuffer_vaddr
  }

  val check = trigger_check(next_store_count, N.U)
  val burst = can_burst(next_store_count, N.U, next_saturate_count)

  store_count := Mux(burst || check, 0.U, next_store_count)
  saturate_counter := Mux(burst || check, 0.S, next_saturate_count)

  if(ONLY_ON_MEMSET) {
    // very strict: only burst on memset
    burst_engine.io.alloc := burst && io.enable && io.memSetPattenDetected
  }else {
    burst_engine.io.alloc := burst && io.enable
  }
  burst_engine.io.vaddr := get_block_addr(io.sbuffer_enq.bits.vaddr)
  burst_engine.io.prefetch_req <> io.prefetch_req

  // perf
  XSPerfAccumulate("trigger_burst", burst && io.enable)
  XSPerfAccumulate("trigger_check", check && io.enable)
}

// L2 Store prefetch component

// Serializer: FIFO queue, recieve EnsbufferWidth requests sent from sq to sbuffer
//             save them to a FIFO queue, pop them in order
class Serializer(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddr)))
    val prefetch_train = DecoupledIO(new DCacheWordReqWithVaddr)
  })
  val QueueSize = SERIALIZER_SIZE

  class SerializerPtr(implicit p: Parameters) extends CircularQueuePtr[SerializerPtr](p => QueueSize){}

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

  val reqs = RegInit(VecInit((0 until QueueSize).map(_.U.asTypeOf(Valid(new DCacheWordReqWithVaddr)))))

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

class StorePfWrapper()(implicit p: Parameters) extends DCacheModule with HasStorePrefetchHelper {
  val io = IO(new DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddr)))
    val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
    val memSetPattenDetected = Input(Bool())
  })

  // TODO: remove serializer, use a ptr in sq
  val serializer = Module(new Serializer())
  val spb = Module(new StorePrefetchBursts())

  // give mutiple reqs to serializer, serializer will give out one req per cycle
  for(i <- 0 until EnsbufferWidth) {
    serializer.io.sbuffer_enq(i).valid := io.sbuffer_enq(i).valid && ENABLE_SPB.B
    serializer.io.sbuffer_enq(i).bits := io.sbuffer_enq(i).bits
  }

  // train spb
  spb.io.enable := ENABLE_SPB.B
  spb.io.memSetPattenDetected := io.memSetPattenDetected
  spb.io.sbuffer_enq.valid := serializer.io.prefetch_train.valid
  spb.io.sbuffer_enq.bits  := serializer.io.prefetch_train.bits
  // spb will always recieve train req
  serializer.io.prefetch_train.ready := true.B

  // fire a prefetch req
  io.prefetch_req <> spb.io.prefetch_req
}