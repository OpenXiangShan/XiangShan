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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{HasPerfEvents, XSDebug, XSPerfAccumulate, QueuePerf}
import utility.Constantin

// TODO: distinguish load and store
class StorePrefetchReq(implicit p: Parameters) extends DCacheBundle {
    val paddr = UInt(PAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
}

class StorePrefetchMissQueue(implicit p: Parameters) extends DCacheModule{
    val io = IO(new DCacheBundle {
        // enq0: odd
        // enq1: even
        val enq = Vec(StorePipelineWidth, Flipped(DecoupledIO(new StorePrefetchReq)))
        val mshr_release_info = Flipped(Valid(new MissEntryReleaseInfo))
        // prefetch to L2 cache
        val deq = DecoupledIO(new StorePrefetchReq)
    })
    require(StorePipelineWidth == 2)
    val QueueSize = 6
    val Interval = RegInit(0.U(10.W))
    val valids = RegInit(VecInit(List.tabulate(QueueSize){_ => false.B}))
    val datas = RegInit(VecInit(List.tabulate(QueueSize){_ => 0.U.asTypeOf(new StorePrefetchReq)}))
    val counters = RegInit(VecInit(List.tabulate(QueueSize){_ => 0.U(10.W)}))
    val cancel_mask = Wire(Vec(QueueSize, Bool()))

    Interval := Constantin.createRecord("staInterval")

    def get_enq_mask(odd: Boolean): Vec[Bool] = {
        WireInit(VecInit(List.tabulate(QueueSize){i => ((i % 2) == (if (odd) 1 else 0)).B}))
    }
    def get_deq_mask(odd: Boolean): Vec[Bool] = {
        WireInit(VecInit(List.tabulate(QueueSize){i => ((i % 2) == (if (odd) 1 else 0)).B}))
    }
    def same_cache_line_addr(paddr0: UInt, paddr1: UInt): Bool = {
        require(paddr0.getWidth == PAddrBits)
        get_block_addr(paddr0) === get_block_addr(paddr1)
    }
    def filter_by_cache_line_addr(valid_vec: Vec[Bool], data_vec: Vec[StorePrefetchReq], incoming_paddr: UInt) : Bool = {
        val match_vec = (valid_vec zip (data_vec.map(_.paddr))).map{
            case(v, e_paddr) => v && same_cache_line_addr(e_paddr, incoming_paddr)
        }
        VecInit(match_vec).asUInt.orR
    }
    
    counters.foreach{case counter => {
        counter := Mux(counter === 0.U, counter, counter - 1.U)
    }}
    // enq
    for (i <- 0 until StorePipelineWidth) {
        val odd = i == 0
        val enq_mask = get_enq_mask(odd).asUInt
        val enq_valids = (~(valids.asUInt) & enq_mask)
        val full = !(enq_valids.orR)
        val enq_idx = PriorityEncoder(enq_valids)

        val enq_req = io.enq(i)
        val enq_cancel = io.mshr_release_info.valid && same_cache_line_addr(io.mshr_release_info.bits.paddr, enq_req.bits.paddr)
        val enq_filter = filter_by_cache_line_addr(valids, datas, enq_req.bits.paddr)
        when(i.U === 0.U) {
            enq_req.ready := !full && !enq_cancel && !enq_filter
        }.otherwise {
            val req1_same_as_req0 = same_cache_line_addr(io.enq(0).bits.paddr, io.enq(1).bits.paddr) && io.enq(0).valid
            enq_req.ready := !full && !enq_cancel && !enq_filter && !req1_same_as_req0
        }

        when(enq_req.fire) {
            valids(enq_idx) := true.B
            datas(enq_idx) := enq_req.bits
            counters(enq_idx) := Interval
        }
    }

    val counter_mask = VecInit(counters.map(_ === 0.U))
    val deq_valids = (valids.asUInt & ~cancel_mask.asUInt & counter_mask.asUInt)
    val deq_idx = PriorityEncoder(deq_valids)

    val deq_req = io.deq
    deq_req.valid := deq_valids.orR
    deq_req.bits := datas(deq_idx)

    when(deq_req.fire) {
        valids(deq_idx) := false.B
    }

    // cancel
    val cancel_vec = (valids zip datas).map{case (v, data) => {
        v && io.mshr_release_info.valid && same_cache_line_addr(io.mshr_release_info.bits.paddr, data.paddr)
    }}
    cancel_mask := cancel_vec
    (valids zip cancel_vec).foreach{case (v, cancel) => {
        when(cancel) {
            v := false.B
        }
    }}

    (0 until StorePipelineWidth).map{case i => XSPerfAccumulate(s"enq${i}_fire", io.enq(i).fire)}
    XSPerfAccumulate("deq_fire", io.deq.fire)
    XSPerfAccumulate("mshr_release_cancel", PopCount(VecInit(cancel_vec)))

    XSPerfAccumulate("has_req", (VecInit((0 until StorePipelineWidth).map(i => io.enq(i).valid))).asUInt.orR)
    XSPerfAccumulate("muti_req", (VecInit((0 until StorePipelineWidth).map(i => io.enq(i).valid))).asUInt.andR)
    XSPerfAccumulate("has_enq", (VecInit((0 until StorePipelineWidth).map(i => io.enq(i).fire))).asUInt.orR)
    XSPerfAccumulate("muti_enq", (VecInit((0 until StorePipelineWidth).map(i => io.enq(i).fire))).asUInt.andR)
    XSPerfAccumulate("same_cache_line", same_cache_line_addr(io.enq(0).bits.paddr, io.enq(1).bits.paddr) && io.enq(0).valid && io.enq(1).valid)

    val perfValidCount = RegNext(PopCount(valids))

    QueuePerf(QueueSize, perfValidCount, perfValidCount === QueueSize.U)
}

