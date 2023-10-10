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
import xiangshan.backend.rob.RobDebugRollingIO
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

class CoverageInfo()(implicit p: Parameters) extends XSBundle {
  val demand_access = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, all prefetch hited load
  val prefetch_hit = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, all load
}

class AccuracyInfo()(implicit p: Parameters) extends XSBundle {
  val total_prefetch = Input(Bool()) // from mshr enq, fire, alloc, prefetch
  val useful_prefetch = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, prefetch hit
}

class TimelyInfo()(implicit p: Parameters) extends XSBundle {
  val late_prefetch = Input(Bool()) // from mshr enq, a load matches a mshr caused by prefetch
}

class PollutionInfo()(implicit p: Parameters) extends XSBundle {
  val demand_miss = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, fisrt miss
  val cache_pollution = Vec(LoadPipelineWidth, Input(Bool())) // from load pipeline, fisrt miss and pollution caused
}

class FDPrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle {
  val refill = Input(Bool()) // from refill pipe, fire

  val coverage = new CoverageInfo
  val accuracy = new XSBundle {
    val stream = new AccuracyInfo
    val stride = new AccuracyInfo
  }
  val timely = new XSBundle {
    val stream = new TimelyInfo
    val stride = new TimelyInfo
  }
  val pollution = new PollutionInfo

  val pf_ctrl_stride = Output(new PrefetchControlBundle)
  val pf_ctrl_stream = Output(new PrefetchControlBundle)
  val debugRolling = Flipped(new RobDebugRollingIO)
}

class FDPrefetcherMonitor()(implicit p: Parameters) extends XSModule {
  val io = IO(new FDPrefetcherMonitorBundle)

  val INTERVAL = 8192
  val CNTWIDTH = log2Up(INTERVAL) + 1

  io.pf_ctrl_stream := DontCare

  // counters
  val refill_cnt = RegInit(0.U(CNTWIDTH.W))

  val demand_access_prev_cnt = RegInit(0.U(32.W))
  val prefetch_hit_prev_cnt = RegInit(0.U(32.W))
  val total_prefetch_prev_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val useful_prefetch_prev_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val late_prefetch_prev_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val demand_miss_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val pollution_prev_cnt = RegInit(0.U(CNTWIDTH.W))
  val prev_cnts_stream = Seq( demand_access_prev_cnt, prefetch_hit_prev_cnt,
                              total_prefetch_prev_cnt(0), useful_prefetch_prev_cnt(0),
                              late_prefetch_prev_cnt(0),
                              demand_miss_prev_cnt, pollution_prev_cnt )
  val prev_cnts_stride = Seq( total_prefetch_prev_cnt(1), useful_prefetch_prev_cnt(1),
                              late_prefetch_prev_cnt(1) )

  val demand_access_interval_cnt = RegInit(0.U(32.W))
  val prefetch_hit_interval_cnt = RegInit(0.U(32.W))
  val total_prefetch_interval_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val useful_prefetch_interval_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val late_prefetch_interval_cnt = RegInit(VecInit(Seq.fill(2)(0.U(CNTWIDTH.W))))
  val demand_miss_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val pollution_interval_cnt = RegInit(0.U(CNTWIDTH.W))
  val interval_cnts_stream = Seq( demand_access_interval_cnt, prefetch_hit_interval_cnt,
                                  total_prefetch_interval_cnt(0), useful_prefetch_interval_cnt(0),
                                  late_prefetch_interval_cnt(0),
                                  demand_miss_interval_cnt, pollution_interval_cnt )
  val interval_cnts_stride = Seq( total_prefetch_interval_cnt(1), useful_prefetch_interval_cnt(1),
                                  late_prefetch_interval_cnt(1) )

  val interval_trigger = refill_cnt === INTERVAL.U

  val io_ens_stream = Seq( io.coverage.demand_access, io.coverage.prefetch_hit,
                           io.accuracy.stream.total_prefetch, io.accuracy.stream.useful_prefetch,
                           io.timely.stream.late_prefetch,
                           io.pollution.demand_miss, io.pollution.cache_pollution )
  val io_ens_stride = Seq( io.accuracy.stride.total_prefetch, io.accuracy.stride.useful_prefetch,
                           io.timely.stride.late_prefetch )

  // update interval counters
  for((interval, en) <- interval_cnts_stream.zip(io_ens_stream)) {
    interval := interval + PopCount(en.asUInt)
  }
  for((interval, en) <- interval_cnts_stride.zip(io_ens_stride)) {
    interval := interval + PopCount(en.asUInt)
  }

  when(io.refill) {
    refill_cnt := refill_cnt + 1.U
  }

  // after an interval, update the prev counters
  // prev = prev / 2 + interval / 2
  when(interval_trigger) {
    refill_cnt := 0.U
    for((prev, interval) <- prev_cnts_stream.zip(interval_cnts_stream)) {
      prev := Cat(0.U(1.W), prev(prev.getWidth - 1, 1)) + Cat(0.U(1.W), interval(interval.getWidth - 1, 1))
      interval := 0.U
    }
    for((prev, interval) <- prev_cnts_stride.zip(interval_cnts_stride)) {
      prev := Cat(0.U(1.W), prev(prev.getWidth - 1, 1)) + Cat(0.U(1.W), interval(interval.getWidth - 1, 1))
      interval := 0.U
    }
  }

  require(prev_cnts_stream.size == interval_cnts_stream.size)
  require(prev_cnts_stride.size == interval_cnts_stride.size)
  require(interval_cnts_stream.size == io_ens_stream.size)
  require(interval_cnts_stride.size == io_ens_stride.size)

  // dynamic stride control logic
  val STRIDECNTBITS = 4
  val SATURATEVAL = (1 << STRIDECNTBITS) - 1
  val INITSAVAL = 7
  val INITDPVAL = 2
  val MAXDP = 10
  
  val dec_const = WireInit(Constantin.createRecord("dec_const" + p(XSCoreParamsKey).HartId.toString, initValue = 3.U))
  val stride_sa_cnt = RegInit(INITSAVAL.U(STRIDECNTBITS.W))
  // use this saturate counter to slow down stride dec
  val depth_dec_sa_cnt = Reg(UInt(32.W))
  val stride_depth_ratio = RegInit(INITDPVAL.U((log2Up(MAXDP) + 1).W))
  val stride_trigger_depth_inc = WireInit(false.B)
  val stride_trigger_depth_dec = WireInit(false.B)

  // stride pf hit trigger depth dec
  when(Cat(io.accuracy.stride.useful_prefetch).orR) {
    val next_sa_cnt = stride_sa_cnt - 1.U
    val saturate = next_sa_cnt === 0.U
    when(saturate) {
      stride_sa_cnt := INITSAVAL.U
      val dec_saturate = (depth_dec_sa_cnt - 1.U) === 0.U
      depth_dec_sa_cnt := Mux(dec_saturate, dec_const, depth_dec_sa_cnt - 1.U)
      when(dec_saturate) {
        stride_trigger_depth_dec := true.B
      }
    }.otherwise {
      stride_sa_cnt := next_sa_cnt
    }
  }

  // late trigger depth inc
  when(io.timely.stride.late_prefetch) {
    val next_sa_cnt = stride_sa_cnt + 3.U
    val saturate = next_sa_cnt >= SATURATEVAL.U
    when(saturate) {
      stride_sa_cnt := INITSAVAL.U
      stride_trigger_depth_inc := true.B
    }.otherwise {
      stride_sa_cnt := next_sa_cnt
    }
  }

  // TODO: time vary depth decr
  stride_depth_ratio := Mux(
    stride_trigger_depth_dec,
    Mux(stride_depth_ratio === 0.U, stride_depth_ratio, stride_depth_ratio - 1.U),
    Mux(
      stride_trigger_depth_inc,
      Mux(stride_depth_ratio === MAXDP.U, stride_depth_ratio, stride_depth_ratio + 1.U),
      stride_depth_ratio
    )
  )

  io.pf_ctrl_stride.dynamic_depth := stride_depth_ratio
  io.pf_ctrl_stride.flush := false.B
  io.pf_ctrl_stride.enable := true.B
  io.pf_ctrl_stride.confidence := 1.U

  when(reset.asBool) {
    depth_dec_sa_cnt := dec_const
  }

  // rolling by instr
  // stream
  XSPerfRolling(
    "L1StreamPrefetchAccuracyIns",
    PopCount(io.accuracy.stream.useful_prefetch), PopCount(io.accuracy.stream.total_prefetch),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "L1StreamPrefetchLatenessIns",
    PopCount(io.timely.stream.late_prefetch), PopCount(io.accuracy.stream.total_prefetch),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  // stride
  XSPerfRolling(
    "L1StridePrefetchAccuracyIns",
    PopCount(io.accuracy.stride.useful_prefetch), PopCount(io.accuracy.stride.total_prefetch),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "L1StridePrefetchLatenessIns",
    PopCount(io.timely.stride.late_prefetch), PopCount(io.accuracy.stride.total_prefetch),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  // total
  XSPerfRolling(
    "L1PrefetchCoverageIns",
    PopCount(io.coverage.prefetch_hit), PopCount(io.coverage.demand_access),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "L1PrefetchPollutionIns",
    PopCount(io.pollution.cache_pollution), PopCount(io.pollution.demand_miss),
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "IPCIns",
    io.debugRolling.robTrueCommit, 1.U,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfAccumulate("io_refill", io.refill)
  XSPerfAccumulate("demand_access_en", PopCount(io.coverage.demand_access))
  XSPerfAccumulate("prefetch_hit_en", PopCount(io.coverage.prefetch_hit))
  XSPerfAccumulate("total_stream_prefetch_en", io.accuracy.stream.total_prefetch)
  XSPerfAccumulate("total_stride_prefetch_en", io.accuracy.stride.total_prefetch)
  XSPerfAccumulate("useful_stream_prefetch_en", PopCount(io.accuracy.stream.useful_prefetch) + io.timely.stream.late_prefetch)
  XSPerfAccumulate("useful_stride_prefetch_en", PopCount(io.accuracy.stride.useful_prefetch) + io.timely.stride.late_prefetch)
  XSPerfAccumulate("late_stream_prefetch_en", io.timely.stream.late_prefetch)
  XSPerfAccumulate("late_stride_prefetch_en", io.timely.stride.late_prefetch)
  XSPerfAccumulate("demand_miss_en", PopCount(io.pollution.demand_miss))
  XSPerfAccumulate("cache_pollution_en", PopCount(io.pollution.cache_pollution))

  XSPerfAccumulate("stride_trigger_depth_inc", stride_trigger_depth_inc)
  XSPerfAccumulate("stride_trigger_depth_dec", stride_trigger_depth_dec)
  XSPerfHistogram("stride_depth_ratio", stride_depth_ratio, true.B, 0, MAXDP, 1)
}