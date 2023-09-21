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

package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}
import utils._
import utility._
import xiangshan.{L1CacheErrorInfo, XSCoreParamsKey}
import xiangshan.mem.HasL1PrefetchSourceParameter
import utility.{CircularQueuePtr}
import xiangshan.cache._
import xiangshan.{XSBundle, XSModule}

//----------------------------------------
// Feedback Direct Prefetching
class CounterFilterDataBundle(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way = UInt(wayBits.W)
}

class CounterFilterQueryBundle(implicit p: Parameters) extends DCacheBundle {
  val req = ValidIO(new CounterFilterDataBundle())
  val resp = Input(Bool())
}

// no Set Blocking in LoadPipe, so when counting useful prefetch counter, duplicate result occurs
// s0    s1     s2     s3
// r                   w
// if 3 load insts is accessing the same cache line(set0, way0) in s0, s1, s2
// they think they all prefetch hit, increment useful prefetch counter 3 times
// so when load arrives at s3, save it's set&way to an FIFO, all loads will search this FIFO to avoid this case
class CounterFilter()(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    // input, only from load for now
    val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new CounterFilterDataBundle())))
    val query = Flipped(Vec(exuParameters.LduCnt, new CounterFilterQueryBundle()))
  })

  val LduStages = 4
  val SIZE = (LduStages) * exuParameters.LduCnt
  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => SIZE ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(SIZE){ (0.U.asTypeOf(new CounterFilterDataBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(SIZE){ (false.B) }))

  // enq
  val enqLen = exuParameters.LduCnt
  val deqLen = exuParameters.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(VecInit((0 until deqLen).map(_.U.asTypeOf(new Ptr))))
  
  val deqPtr = WireInit(deqPtrExt(0).value)

  val reqs_l = io.ld_in.map(_.bits)
  val reqs_vl = io.ld_in.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))
  val last3CycleAlloc = RegInit(0.U(log2Ceil(exuParameters.LduCnt + 1).W))

  for(i <- (0 until enqLen)) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)

    needAlloc(i) := req_v
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt(0)

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }

    assert(!needAlloc(i) || canAlloc(i), s"port${i} can not accept CounterFilter enq request, check if SIZE >= (Ldu stages - 2) * LduCnt")
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => x := x + allocNum}
  last3CycleAlloc := RegNext(RegNext(allocNum))

  // deq
  for(i <- (0 until deqLen)) {
    when(i.U < last3CycleAlloc) {
      valids(deqPtrExt(i).value) := false.B
    }
  }
  
  deqPtrExt.foreach{case x => x := x + last3CycleAlloc}

  // query
  val querys_l = io.query.map(_.req.bits)
  val querys_vl = io.query.map(_.req.valid)
  for(i <- (0 until exuParameters.LduCnt)) {
    val q = querys_l(i)
    val q_v = querys_vl(i)

    val entry_match = Cat(entries.zip(valids).map {
      case(e, v) => v && (q.idx === e.idx) && (q.way === e.way)
    }).orR

    io.query(i).resp := q_v && entry_match
  }

  XSPerfAccumulate("req_nums", PopCount(io.query.map(_.req.valid)))
  XSPerfAccumulate("req_set_way_match", PopCount(io.query.map(_.resp)))
}

class BloomQueryBundle(n: Int)(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(BLOOMADDRWIDTH.W)

  def BLOOMADDRWIDTH = log2Ceil(n)

  def get_addr(paddr: UInt): UInt = {
    assert(paddr.getWidth == PAddrBits)
    assert(paddr.getWidth >= (blockOffBits + 2 * BLOOMADDRWIDTH))
    val block_paddr = paddr(paddr.getWidth - 1, blockOffBits)
    val low_part = block_paddr(BLOOMADDRWIDTH - 1, 0)
    val high_part = block_paddr(2 * BLOOMADDRWIDTH - 1, BLOOMADDRWIDTH)
    low_part ^ high_part
  }
}

class BloomRespBundle(implicit p: Parameters) extends DCacheBundle {
  val res = Bool()
}
class BloomFilter(n: Int, bypass: Boolean = true)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new DCacheBundle {
    val set = Flipped(ValidIO(new BloomQueryBundle(n)))
    val clr = Flipped(ValidIO(new BloomQueryBundle(n)))
    val query = Vec(LoadPipelineWidth, Flipped(ValidIO(new BloomQueryBundle(n))))
    val resp = Vec(LoadPipelineWidth, ValidIO(new BloomRespBundle))
  })

  val data = RegInit(0.U(n.W))
  val data_next = Wire(Vec(n, Bool()))

  for (i <- 0 until n) {
    when(io.clr.valid && i.U === io.clr.bits.addr) {
      data_next(i) := false.B
    }.elsewhen(io.set.valid && i.U === io.set.bits.addr) {
      data_next(i) := true.B
    }.otherwise {
      data_next(i) := data(i).asBool
    }
  }

  // resp will valid in next cycle
  for(i <- 0 until LoadPipelineWidth) {
    io.resp(i).valid := RegNext(io.query(i).valid)
    if(bypass) {
      io.resp(i).bits.res := RegEnable(data_next(io.query(i).bits.addr), io.query(i).valid)
    }else {
      io.resp(i).bits.res := RegEnable(data(io.query(i).bits.addr), io.query(i).valid)
    }
  }

  data := data_next.asUInt

  assert(PopCount(data ^ data_next.asUInt) <= 2.U)

  XSPerfHistogram("valid_nums", PopCount(data), true.B, 0, n + 1, 20)
}

class FDPrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle {
  val refill = Input(Bool()) // from refill pipe, fire
  val accuracy = new XSBundle {
    val total_prefetch = Input(Bool()) // from mshr enq, fire, alloc, prefetch
    val useful_prefetch = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, prefetch hit
  }
    
  val timely = new XSBundle {
    val late_prefetch = Input(Bool()) // from mshr enq, a load matches a mshr caused by prefetch
  }

  val pollution = new XSBundle {
    val demand_miss = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, fisrt miss
    val cache_pollution = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, fisrt miss and pollution caused
  }

  val pf_ctrl = Output(new PrefetchControlBundle)
}

class FDPrefetcherMonitor()(implicit p: Parameters) extends XSModule {
  val io = IO(new FDPrefetcherMonitorBundle)

  val INTERVAL = 8192
  val CNTWIDTH = log2Up(INTERVAL) + 1

  io.pf_ctrl := DontCare

  val refill_cnt = RegInit(0.U(CNTWIDTH.W))

  val total_prefetch_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val useful_prefetch_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val late_prefetch_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val demand_miss_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val pollution_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val prev_cnts = Seq(total_prefetch_prev_cnt, useful_prefetch_prev_cnt, late_prefetch_prev_cnt, demand_miss_prev_cnt, pollution_prev_cnt)

  val total_prefetch_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val useful_prefetch_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val late_prefetch_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val demand_miss_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val pollution_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val interval_cnts = Seq(total_prefetch_interval_cnt, useful_prefetch_interval_cnt, late_prefetch_interval_cnt, demand_miss_interval_cnt, pollution_interval_cnt)

  val interval_trigger = refill_cnt === INTERVAL.U

  val io_ens = Seq(io.accuracy.total_prefetch, io.accuracy.useful_prefetch, io.timely.late_prefetch, io.pollution.demand_miss, io.pollution.cache_pollution)

  for((interval, en) <- interval_cnts.zip(io_ens)) {
    interval := interval + PopCount(en.asUInt)
  }

  when(io.refill) {
    refill_cnt := refill_cnt + 1.U
  }

  when(interval_trigger) {
    refill_cnt := 0.U
    for((prev, interval) <- prev_cnts.zip(interval_cnts)) {
      prev := Cat(0.U(1.W), prev(prev.getWidth - 1, 1)) + Cat(0.U(1.W), interval(interval.getWidth - 1, 1))
      interval := 0.U
    }
  }

  XSPerfAccumulate("io_refill", io.refill)
  XSPerfAccumulate("total_prefetch_en", io.accuracy.total_prefetch)
  XSPerfAccumulate("useful_prefetch_en", PopCount(io.accuracy.useful_prefetch) + io.timely.late_prefetch)
  XSPerfAccumulate("late_prefetch_en", io.timely.late_prefetch)
  XSPerfAccumulate("demand_miss_en", PopCount(io.pollution.demand_miss))
  XSPerfAccumulate("cache_pollution_en", PopCount(io.pollution.cache_pollution))
}