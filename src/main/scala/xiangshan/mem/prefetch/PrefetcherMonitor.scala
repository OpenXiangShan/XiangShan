package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.backend.rob.RobDebugRollingIO

class PrefetchControlBundle()(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val dynamic_depth = UInt(DEPTH_BITS.W)
  val flush = Bool()
  val enable = Bool()
  val confidence = UInt(1.W)
}

class LoadPrefetchInfoBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val total_prefetch = Bool() // from loadpipe s2, pf req sent
  val late_hit_prefetch = Bool() // from loadpipe s2, pf req sent but hit
  val nack_prefetch = Bool() // from loadpipe s2, pf req miss but nack
  val pf_source = UInt(L1PfSourceBits.W)

  val prefetch_hit = Bool() // from loadpipe s3, pf block hit by demand, clear pf flag
  val hit_source = UInt(L1PfSourceBits.W)

  val demand_miss = Bool() // from loadpipe s2, demand req miss
  val pollution = Bool() // from loadpipe s2, bloom filter speculate pollution
}

class MainPrefetchInfoBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val bad_prefetch = Bool() // from mainpipe replace, prefetch block but not accessed
  val pf_source = UInt(L1PfSourceBits.W)
}

class MissPrefetchInfoBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val late_miss_prefetch = Bool() // from missqueue, pf req match a existing mshr
  val prefetch_refill = Bool() // from missqueue, pf req allocate a new mshr
  val pf_source = UInt(L1PfSourceBits.W)

  val demand_match_pfmshr = Bool() // from missqueue, demand miss match a existing pf mshr, then clear pf flag
  val load_refill = Bool() // from missqueue, load demand miss allocate a new mshr
}

class PrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val loadinfo = Input(Vec(LoadPipelineWidth, new LoadPrefetchInfoBundle))
  val missinfo = Input(new MissPrefetchInfoBundle)
  val maininfo = Input(new MainPrefetchInfoBundle)

  val clear_flag = Input(Vec(LoadPipelineWidth, Bool()))

  val pf_ctrl = Output(Vec(L1PrefetcherNum, new PrefetchControlBundle))

  val debugRolling = Flipped(new RobDebugRollingIO)
}

class PrefetcherMonitor()(implicit p: Parameters) extends XSModule with HasStreamPrefetchHelper {
  val io = IO(new PrefetcherMonitorBundle)

  val prefetch_info = Wire(new L1PrefetchStatisticBundle)
  prefetch_info.loadinfo := io.loadinfo 
  prefetch_info.missinfo := io.missinfo
  prefetch_info.maininfo := io.maininfo

  for (i <- 0 until LoadPipelineWidth) {
    when(io.clear_flag(i)) {
      prefetch_info.loadinfo(i).prefetch_hit := false.B
    }
  }

  val StreamMonitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("stream")))
  val StrideMonitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("stride")))

  StreamMonitor.io.prefetch_info:= prefetch_info
  StrideMonitor.io.prefetch_info := prefetch_info
  
  // stream 0, stride 1
  io.pf_ctrl(0) := StreamMonitor.io.pf_ctrl
  io.pf_ctrl(1) := StrideMonitor.io.pf_ctrl

  // ldu 0, 1, 2 can only have one prefetch request at a time
  val total_prefetch = io.loadinfo.map(t => t.total_prefetch).reduce(_ || _)
  val late_hit_prefetch = io.loadinfo.map(t => t.late_hit_prefetch).reduce(_ || _)
  val nack_prefetch = io.loadinfo.map(t => t.nack_prefetch).reduce(_ || _)
  val late_miss_prefetch = io.missinfo.late_miss_prefetch
  // demand accesses from different ldu may hit different prefetch blocks
  val good_prefetch = PopCount(prefetch_info.loadinfo.map(t => t.prefetch_hit))
  val bad_prefetch = io.maininfo.bad_prefetch
  val prefetch_refill = io.missinfo.prefetch_refill
  val load_refill = io.missinfo.load_refill
  val demand_match_pfmshr = io.missinfo.demand_match_pfmshr
  // ldu 0, 1, 2 can have multiple demand accesses at a time
  val demand_miss = PopCount(io.loadinfo.map(t => t.demand_miss))
  val pollution = PopCount(io.loadinfo.map(t => t.pollution))
  
  XSPerfAccumulate("total_prefetch", total_prefetch)
  XSPerfAccumulate("late_hit_prefetch", late_hit_prefetch)
  XSPerfAccumulate("nack_prefetch", nack_prefetch)
  XSPerfAccumulate("late_miss_prefetch", late_miss_prefetch)
  XSPerfAccumulate("good_prefetch", good_prefetch)
  XSPerfAccumulate("bad_prefetch", bad_prefetch)
  XSPerfAccumulate("prefetch_refill", prefetch_refill)
  XSPerfAccumulate("load_refill", load_refill)
  XSPerfAccumulate("demand_match_pfmshr", demand_match_pfmshr)
  XSPerfAccumulate("demand_miss", demand_miss)
  XSPerfAccumulate("cache_pollution", pollution)

  // rolling by instr
  XSPerfRolling(
    "L1PrefetchAccuracyIns",
    good_prefetch, total_prefetch,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )
  
  XSPerfRolling(
    "L1PrefetchLatenessIns",
    demand_match_pfmshr, prefetch_refill,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "L1PrefetchPollutionIns",
    pollution, demand_miss,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "IPCIns",
    io.debugRolling.robTrueCommit, 1.U,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )
}

class L1PrefetchStatisticBundle()(implicit p: Parameters) extends XSBundle {
  val loadinfo = Vec(LoadPipelineWidth, new LoadPrefetchInfoBundle)
  val missinfo = new MissPrefetchInfoBundle
  val maininfo = new MainPrefetchInfoBundle
}

class L1PrefetchMonitorBundle()(implicit p: Parameters) extends XSBundle {
  val prefetch_info = Input(new L1PrefetchStatisticBundle)

  val pf_ctrl = Output(new PrefetchControlBundle)
}

class L1PrefetchMonitor(param : PrefetcherMonitorParam)(implicit p: Parameters) extends XSModule with HasStreamPrefetchHelper {
  val io = IO(new L1PrefetchMonitorBundle)

  val depth = Reg(UInt(DEPTH_BITS.W))
  val flush = RegInit(false.B)
  val enable = RegInit(true.B)
  val confidence = RegInit(param.confidence.U(1.W))

  // TODO: mshr number
  // mshr full && load miss && load send mshr req && !load match,  -> decr nmax prefetch
  // mshr free

  io.pf_ctrl.dynamic_depth := depth
  io.pf_ctrl.flush := flush
  io.pf_ctrl.enable := enable
  io.pf_ctrl.confidence := confidence

  val depth_const = Wire(UInt(DEPTH_BITS.W))
  depth_const := Constantin.createRecord(s"${param.name}_depth${p(XSCoreParamsKey).HartId}", initValue = 32)

  val total_prefetch_cnt = RegInit(0.U((log2Up(param.TIMELY_CHECK_INTERVAL) + 1).W))
  val late_hit_prefetch_cnt = RegInit(0.U((log2Up(param.TIMELY_CHECK_INTERVAL) + 1).W))
  val late_miss_prefetch_cnt = RegInit(0.U((log2Up(param.TIMELY_CHECK_INTERVAL) + 1).W))

  val good_prefetch_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))
  val bad_prefetch_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))

  val back_off_cnt = RegInit(0.U((log2Up(param.BACK_OFF_INTERVAL) + 1).W))
  val low_conf_cnt = RegInit(0.U((log2Up(param.LOW_CONF_INTERVAL) + 1).W))

  val timely_reset = (total_prefetch_cnt === param.TIMELY_CHECK_INTERVAL.U) || (late_hit_prefetch_cnt >= param.TIMELY_CHECK_INTERVAL.U)
  val validity_reset = (good_prefetch_cnt + bad_prefetch_cnt) === param.VALIDITY_CHECK_INTERVAL.U
  val back_off_reset = back_off_cnt === param.BACK_OFF_INTERVAL.U
  val conf_reset = low_conf_cnt === param.LOW_CONF_INTERVAL.U

  val total_prefetch = io.prefetch_info.loadinfo.map(t => t.total_prefetch && param.isMyType(t.pf_source)).reduce(_ || _)
  val late_hit_prefetch = io.prefetch_info.loadinfo.map(t => t.late_hit_prefetch && param.isMyType(t.pf_source)).reduce(_ || _)
  val nack_prefetch = io.prefetch_info.loadinfo.map(t => t.nack_prefetch && param.isMyType(t.pf_source)).reduce(_ || _)
  val late_miss_prefetch = io.prefetch_info.missinfo.late_miss_prefetch && param.isMyType(io.prefetch_info.missinfo.pf_source)
  val good_prefetch = PopCount(io.prefetch_info.loadinfo.map(t => t.prefetch_hit && param.isMyType(t.hit_source)))
  val bad_prefetch = io.prefetch_info.maininfo.bad_prefetch && param.isMyType(io.prefetch_info.maininfo.pf_source)

  total_prefetch_cnt := Mux(timely_reset, 0.U, total_prefetch_cnt + total_prefetch)
  late_hit_prefetch_cnt := Mux(timely_reset, 0.U, late_hit_prefetch_cnt + late_hit_prefetch)
  late_miss_prefetch_cnt := Mux(timely_reset, 0.U, late_miss_prefetch_cnt + late_miss_prefetch)
  good_prefetch_cnt := Mux(validity_reset, 0.U, good_prefetch_cnt + good_prefetch)
  bad_prefetch_cnt := Mux(validity_reset, 0.U, bad_prefetch_cnt + bad_prefetch)

  back_off_cnt := Mux(back_off_reset, 0.U, back_off_cnt + !enable)
  low_conf_cnt := Mux(conf_reset, 0.U, low_conf_cnt + !confidence.asBool)

  val trigger_late_hit = timely_reset && (late_hit_prefetch_cnt >= param.LATE_HIT_THRESHOLD.U)
  val trigger_late_miss = timely_reset && (late_miss_prefetch_cnt >= param.LATE_MISS_THRESHOLD.U)
  val trigger_bad_prefetch = validity_reset && (bad_prefetch_cnt >= param.BAD_THRESHOLD.U)
  val trigger_disable = validity_reset && (bad_prefetch_cnt >= param.DISABLE_THRESHOLD.U)

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

  val enableDynamicPrefetcher_const = Constantin.createRecord(s"${param.name}_enableDynamicPrefetcher${p(XSCoreParamsKey).HartId}", initValue = 1)
  val enableDynamicPrefetcher = (enableDynamicPrefetcher_const === 1.U)

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

  XSPerfAccumulate(s"${param.name}_total_prefetch", total_prefetch)
  XSPerfAccumulate(s"${param.name}_late_hit_prefetch", late_hit_prefetch)
  XSPerfAccumulate(s"${param.name}_nack_prefetch", nack_prefetch)
  XSPerfAccumulate(s"${param.name}_late_miss_prefetch", late_miss_prefetch)
  XSPerfAccumulate(s"${param.name}_good_prefetch", good_prefetch)
  XSPerfAccumulate(s"${param.name}_bad_prefetch", bad_prefetch)
  for(i <- (0 until DEPTH_BITS)) {
    val t = (1 << i)
    XSPerfAccumulate(s"${param.name}_depth${t}", depth === t.U)
  }
  XSPerfAccumulate(s"${param.name}_trigger_disable", trigger_disable)
  XSPerfAccumulate(s"${param.name}_trigger_late_hit", trigger_late_hit)
  XSPerfAccumulate(s"${param.name}_trigger_late_miss", trigger_late_miss)
  XSPerfAccumulate(s"${param.name}_trigger_bad_prefetch", trigger_bad_prefetch)
  XSPerfAccumulate(s"${param.name}_disable_time", !enable)

  assert(depth =/= 0.U, s"${param.name}_depth should not be zero")
}

abstract class PrefetcherMonitorParam {
  val name: String
  def isMyType(value: UInt): Bool

  val TIMELY_CHECK_INTERVAL = 1000
  val VALIDITY_CHECK_INTERVAL = 1000

  val BAD_THRESHOLD = 400
  val DISABLE_THRESHOLD = 900
  val LATE_HIT_THRESHOLD = 900
  val LATE_MISS_THRESHOLD = 200

  val BACK_OFF_INTERVAL = 100000
  val LOW_CONF_INTERVAL = 200000

  val confidence = 1
}

object PrefetcherMonitorParam {
  def fromString(s: String): PrefetcherMonitorParam = s.toLowerCase match {
    case "stream" => new StreamMonitorParam()
    case "stride" => new StrideMonitorParam()
    case t => throw new IllegalArgumentException(s"unknown Prefetcher type $t")
  }
}

class StreamMonitorParam extends PrefetcherMonitorParam with HasL1PrefetchSourceParameter {
  override val name: String = "Stream"
  override def isMyType(value: UInt) = value === L1_HW_PREFETCH_STREAM
}

class StrideMonitorParam extends PrefetcherMonitorParam with HasL1PrefetchSourceParameter {
  override val name: String = "Stride"
  override def isMyType(value: UInt) = value === L1_HW_PREFETCH_STRIDE

  override val VALIDITY_CHECK_INTERVAL = 800
  override val DISABLE_THRESHOLD = 700
}