package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import xiangshan._
import xiangshan.mem.{LdPrefetchTrainBundle, StPrefetchTrainBundle, L1PrefetchReq}
import utils._
import utility._

trait HasPrefetcherMonitorHelper {
  val TIMELY_CHECK_INTERVAL = 1000
  val VALIDITY_CHECK_INTERVAL = 1000

  val BAD_THRESHOLD = 400
  val DISABLE_THRESHOLD = 900
  val LATE_HIT_THRESHOLD = 900
  val LATE_MISS_THRESHOLD = 200

  val BACK_OFF_INTERVAL = 100000
  val LOW_CONF_INTERVAL = 200000

  // val enableDynamicPrefetcher = false
}

class PrefetchControlBundle()(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val dynamic_depth = UInt(DEPTH_BITS.W)
  val flush = Bool()
  val enable = Bool()
  val confidence = UInt(1.W)
}

class PrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle {
  val timely = new XSBundle {
    val total_prefetch = Input(Bool())
    val late_hit_prefetch = Input(Bool())
    val late_miss_prefetch = Input(Bool())
    val prefetch_hit = Input(UInt(2.W))
  }
    
  val validity = new XSBundle {
    val good_prefetch = Input(Bool())
    val bad_prefetch = Input(Bool())
  }

  val pf_ctrl = Output(new PrefetchControlBundle)
}

class PrefetcherMonitor()(implicit p: Parameters) extends XSModule with HasPrefetcherMonitorHelper with HasStreamPrefetchHelper {
  val io = IO(new PrefetcherMonitorBundle)

  val depth = Reg(UInt(DEPTH_BITS.W))
  val flush = RegInit(false.B)
  val enable = RegInit(true.B)
  val confidence = RegInit(1.U(1.W))
  
  // TODO: mshr number
  // mshr full && load miss && load send mshr req && !load match,  -> decr nmax prefetch
  // mshr free 

  io.pf_ctrl.dynamic_depth := depth
  io.pf_ctrl.flush := flush
  io.pf_ctrl.enable := enable
  io.pf_ctrl.confidence := confidence

  val depth_const = Wire(UInt(DEPTH_BITS.W))
  depth_const := Constantin.createRecord("depth" + p(XSCoreParamsKey).HartId.toString, initValue = 32.U)

  val total_prefetch_cnt = RegInit(0.U((log2Up(TIMELY_CHECK_INTERVAL) + 1).W))
  val late_hit_prefetch_cnt = RegInit(0.U((log2Up(TIMELY_CHECK_INTERVAL) + 1).W))
  val late_miss_prefetch_cnt = RegInit(0.U((log2Up(TIMELY_CHECK_INTERVAL) + 1).W))
  val prefetch_hit_cnt = RegInit(0.U(32.W))

  val good_prefetch_cnt = RegInit(0.U((log2Up(VALIDITY_CHECK_INTERVAL) + 1).W))
  val bad_prefetch_cnt = RegInit(0.U((log2Up(VALIDITY_CHECK_INTERVAL) + 1).W))

  val back_off_cnt = RegInit(0.U((log2Up(BACK_OFF_INTERVAL) + 1).W))
  val low_conf_cnt = RegInit(0.U((log2Up(LOW_CONF_INTERVAL) + 1).W))

  val timely_reset = total_prefetch_cnt === TIMELY_CHECK_INTERVAL.U
  val validity_reset = (good_prefetch_cnt + bad_prefetch_cnt) === VALIDITY_CHECK_INTERVAL.U
  val back_off_reset = back_off_cnt === BACK_OFF_INTERVAL.U
  val conf_reset = low_conf_cnt === LOW_CONF_INTERVAL.U

  total_prefetch_cnt := Mux(timely_reset, 0.U, total_prefetch_cnt + io.timely.total_prefetch)
  late_hit_prefetch_cnt := Mux(timely_reset, 0.U, late_hit_prefetch_cnt + io.timely.late_hit_prefetch)
  late_miss_prefetch_cnt := Mux(timely_reset, 0.U, late_miss_prefetch_cnt + io.timely.late_miss_prefetch)
  prefetch_hit_cnt := Mux(timely_reset, 0.U, prefetch_hit_cnt + io.timely.prefetch_hit)

  good_prefetch_cnt := Mux(validity_reset, 0.U, good_prefetch_cnt + io.validity.good_prefetch)
  bad_prefetch_cnt := Mux(validity_reset, 0.U, bad_prefetch_cnt + io.validity.bad_prefetch)

  back_off_cnt := Mux(back_off_reset, 0.U, back_off_cnt + !enable)
  low_conf_cnt := Mux(conf_reset, 0.U, low_conf_cnt + !confidence.asBool)

  val trigger_late_hit = timely_reset && (late_hit_prefetch_cnt >= LATE_HIT_THRESHOLD.U)
  val trigger_late_miss = timely_reset && (late_miss_prefetch_cnt >= LATE_MISS_THRESHOLD.U)
  val trigger_bad_prefetch = validity_reset && (bad_prefetch_cnt >= BAD_THRESHOLD.U)
  val trigger_disable = validity_reset && (bad_prefetch_cnt >= DISABLE_THRESHOLD.U)

  flush := Mux(flush, false.B, flush)
  enable := Mux(back_off_reset, true.B, enable)
  confidence := Mux(conf_reset, 1.U(1.W), confidence)

  when(trigger_bad_prefetch) {
    depth := Mux(depth === 1.U, depth, depth >> 1)
  }
  when(trigger_disable) {
    confidence := 0.U(1.W)
    enable := false.B
    flush := true.B
  }

  when(trigger_late_miss) {
    depth := Mux(depth === (1 << (DEPTH_BITS - 1)).U, depth, depth << 1)
  }.elsewhen(trigger_late_hit) {
    // for now, late hit will disable the prefether
    confidence := 0.U(1.W)
    enable := false.B
  }

  val enableDynamicPrefetcher_const = WireInit(Constantin.createRecord("enableDynamicPrefetcher" + p(XSCoreParamsKey).HartId.toString, initValue = 1.U))
  val enableDynamicPrefetcher = enableDynamicPrefetcher_const === 1.U

  when(!enableDynamicPrefetcher) {
    depth := depth_const
    flush := false.B
    enable := true.B
    confidence := 1.U
  }.otherwise {
    // for now, only dynamically disable prefetcher, without depth and flush
    depth := depth_const
    flush := false.B
  }

  when(reset.asBool) {
    depth := depth_const
  }

  XSPerfAccumulate("total_prefetch", io.timely.total_prefetch)
  XSPerfAccumulate("late_hit_prefetch", io.timely.late_hit_prefetch)
  XSPerfAccumulate("late_miss_prefetch", io.timely.late_miss_prefetch)
  XSPerfAccumulate("good_prefetch", io.validity.good_prefetch)
  XSPerfAccumulate("bad_prefetch", io.validity.bad_prefetch)
  for(i <- (0 until DEPTH_BITS)) {
    val t = (1 << i)
    XSPerfAccumulate(s"depth${t}", depth === t.U)
  }
  XSPerfAccumulate("trigger_disable", trigger_disable)
  XSPerfAccumulate("prefetch_hit", io.timely.prefetch_hit)

  assert(depth =/= 0.U, "depth should not be zero")
}