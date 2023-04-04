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

class StorePrefetchBurstsIO(implicit p: Parameters) extends DCacheBundle {
    val sbuffer_enq  = Vec(EnsbufferWidth, Flipped(Valid(new DCacheWordReqWithVaddr)))
    val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
}

class PrefetchBurstGenerator(implicit p: Parameters) extends DCacheModule {
    val io = IO(new DCacheBundle {
        val alloc = Input(Bool())
        val vaddr = Input(UInt(VAddrBits.W))
        val prefetch_req = Vec(StorePipelineWidth, DecoupledIO(new StorePrefetchReq))
    })

    val SIZE = 2 // Buffer size
    val PAGEOFFSET = 12 // page offset 4096 Bytes
    val BLOCKOFFSET = 5 // cache block offset 64 Bytes

    // filter logic (granularity: a page)
    def same_page_addr(paddr0: UInt, paddr1: UInt): Bool = {
        require(paddr0.getWidth == VAddrBits && paddr1.getWidth == VAddrBits)
        paddr0(paddr0.getWidth - 1, PAGEOFFSET) === paddr1(paddr1.getWidth - 1, PAGEOFFSET)
    }
    def filter_by_page_addr(valid_vec: Vec[Bool], data_vec: Vec[UInt], incoming_vaddr: UInt) : Bool = {
        val match_vec = (valid_vec zip data_vec).map{
            case(v, e_vaddr) => v && same_page_addr(e_vaddr, incoming_vaddr)
        }
        VecInit(match_vec).asUInt.orR
    }

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
    val deq_decoupled = Wire(Vec(StorePipelineWidth, Vec(SIZE, Decoupled(new StorePrefetchReq))))

    (deq_valids zip deq_decoupled zip datas zip datas_next zip datas_next_next zip pagebits zip valids).foreach{case ((((((deq_valid, out_decouple), data), data_next), data_next_next), pg_bit), v) => {
        out_decouple(0).valid := deq_valid
        out_decouple(0).bits := DontCare
        out_decouple(0).bits.vaddr := data
        out_decouple(1).valid := deq_valid && data_next(PAGEOFFSET) === pg_bit && out_decouple(0).fire
        out_decouple(1).bits := DontCare
        out_decouple(1).bits.vaddr := data_next
        when(out_decouple(1).fire) {
            data := data_next_next
            when(data_next_next(PAGEOFFSET) =/= pg_bit) {
                v := false.B
            }
        }.elsewhen(out_decouple(0).fire) {
            data := data_next
            when(data_next(PAGEOFFSET) =/= pg_bit) {
                v := false.B
            }
        }
    }}
    for (i <- 0 until StorePipelineWidth) {
        arbiter(deq_decoupled(i), io.prefetch_req(i), Some(s"spb_deq_art${i}"))
    }

    XSPerfAccumulate("burst_valid_num", PopCount(valids))
    XSPerfAccumulate("prefetch_req_fire_by_generator", PopCount(VecInit(io.prefetch_req.map(_.fire))))
}

// Virtual Address Prefetch
class StorePrefetchBursts(implicit p: Parameters) extends DCacheModule {
    val io = IO(new StorePrefetchBurstsIO)
    require(EnsbufferWidth == 2)

    def cache_block_addr_difference(req_vaddr: UInt, last_vaddr: UInt): UInt = {
        (get_block_addr(req_vaddr) - get_block_addr(last_vaddr)) >> blockOffBits
    }
    def get_store_count_divided_by_8(st_count: UInt): UInt = {
        st_count(st_count.getWidth - 1, 3)
    }
    def trigger_check(st_count: UInt, N: UInt): Bool = {
        st_count > N
    }
    def can_burst(st_count: UInt, N: UInt, sa_count: UInt): Bool = {
        trigger_check(st_count, N) && get_store_count_divided_by_8(st_count) === sa_count
    }

    // meta for SPB 
    val N = BigInt(48)
    val last_st_block_addr = RegInit(0.U(VAddrBits.W))
    val saturate_counter = RegInit(0.U(4.W))
    val store_count = RegInit(0.U(log2Up(N).W))
    val burst_engine = Module(new PrefetchBurstGenerator())

    val sbuffer_fire_vec  = VecInit(io.sbuffer_enq.map(_.valid))
    val sbuffer_vaddr_vec = VecInit(io.sbuffer_enq.map(_.bits.vaddr))
    val last_st_block_addr_vec = VecInit(last_st_block_addr) ++ sbuffer_vaddr_vec

    val temp_store_count_vec = WireInit(VecInit(Seq.tabulate(EnsbufferWidth){case i => store_count + PopCount(sbuffer_fire_vec.take(i + 1))}))
    val temp_saturate_count_vec = WireInit(VecInit(Seq.tabulate(EnsbufferWidth){case i => saturate_counter + ((last_st_block_addr_vec.take(i + 2) zipWithIndex) zip (VecInit(false.B) ++ sbuffer_fire_vec.take(i + 1))).map{
        case ((vaddr, index), fire) => {
            if(index == 0) {
                0.U
            }else {
                Mux(fire, cache_block_addr_difference(vaddr, last_st_block_addr_vec(index - 1)), 0.U)
            }
        }
    }.reduce(_ + _)}))

    for(i <- 0 until EnsbufferWidth) {
        when(sbuffer_fire_vec(i)) {
            last_st_block_addr := sbuffer_vaddr_vec(i)
        }
    }
    val check_vec = temp_store_count_vec.map{
        case st_count => trigger_check(st_count, N.U)
    }
    val burst_vec = (temp_store_count_vec zip temp_saturate_count_vec).map{
        case (st_count, sa_count) => {
            can_burst(st_count, N.U, sa_count)
        }
    }
    val do_check = VecInit(check_vec).asUInt.orR
    val trigger_burst = VecInit(burst_vec).asUInt.orR
    val burst_idx = PriorityEncoder(burst_vec)

    store_count := Mux(trigger_burst || do_check, 0.U, temp_store_count_vec.last)
    saturate_counter := Mux(trigger_burst || do_check, 0.U, temp_saturate_count_vec.last)

    burst_engine.io.alloc := trigger_burst
    burst_engine.io.vaddr := io.sbuffer_enq(burst_idx).bits.vaddr
    burst_engine.io.prefetch_req <> io.prefetch_req

    // perf 
    XSPerfAccumulate("trigger_burst", trigger_burst)
    XSPerfAccumulate("trigger_check", do_check)
}