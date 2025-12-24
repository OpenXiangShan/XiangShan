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

class LoadPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val total_prefetch = Bool() // from loadpipe s2, pf req sent
  val pf_late_in_cache = Bool() // from loadpipe s2, pf req sent but hit
  val pf_late_in_cache_source = UInt(L1PfSourceBits.W) // from loadpipe s2, pf req sent but hit, hit's source
  val nack_prefetch = Bool() // from loadpipe s2, pf req miss but nack
  val pf_source = UInt(L1PfSourceBits.W)

  val hit_pf_in_cache = Bool() // from loadpipe s3, pf block hit by demand, clear pf flag
  val hit_source = UInt(L1PfSourceBits.W)

  val demand_miss = Bool() // from loadpipe s2, demand req miss
  val pollution = Bool() // from loadpipe s2, bloom filter speculate pollution
}

class MainPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val pf_useless = Bool() // from mainpipe replace, prefetch block but not accessed
  val pf_source_useless = UInt(L1PfSourceBits.W)

  val hit_pf_in_cache = Bool() // from mainpipe, refill accessed pf block | store req hit pf block
  val hit_pf_source_in_cache = UInt(L1PfSourceBits.W)
}

class MissPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val pf_late_in_mshr = Bool() // from missqueue, pf req match a existing mshr
  val pf_late_in_mshr_source = UInt(L1PfSourceBits.W) // from missqueue, pf req match a existing mshr, it's source type
  val prefetch_miss = Bool() // from missqueue, pf req allocate a new mshr
  val pf_source = UInt(L1PfSourceBits.W)

  val hit_pf_in_mshr = Bool() // from missqueue, demand miss match a existing pf mshr, then clear pf flag
  val hit_pf_in_mshr_source = UInt(L1PfSourceBits.W) // from missqueue, the pf source of demand miss matched
  val load_miss = Bool() // from missqueue, load demand miss allocate a new mshr
}

class PrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val loadinfo = Input(Vec(LoadPipelineWidth, new LoadPrefetchStatBundle))
  val missinfo = Input(new MissPrefetchStatBundle)
  val maininfo = Input(new MainPrefetchStatBundle)

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
      prefetch_info.loadinfo(i).hit_pf_in_cache := false.B
    }
  }

  val StreamMonitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("stream")))
  val StrideMonitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("stride")))
  val BertiMonitor = Module(new L1PrefetchMonitor(PrefetcherMonitorParam.fromString("berti")))

  StreamMonitor.io.prefetch_info:= prefetch_info
  StrideMonitor.io.prefetch_info := prefetch_info
  BertiMonitor.io.prefetch_info := prefetch_info
  
  // stream 0, stride 1
  io.pf_ctrl(0) := StreamMonitor.io.pf_ctrl
  io.pf_ctrl(1) := StrideMonitor.io.pf_ctrl

  // ldu 0, 1, 2 can only have one prefetch request at a time
  val total_prefetch = io.loadinfo.map(t => t.total_prefetch).reduce(_ || _)
  val nack_prefetch = io.loadinfo.map(t => t.nack_prefetch).reduce(_ || _)
  val pf_late_in_cache = io.loadinfo.map(t => t.pf_late_in_cache).reduce(_ || _)
  val pf_late_in_mshr = io.missinfo.pf_late_in_mshr
  val pf_late = pf_late_in_cache.asUInt + pf_late_in_mshr.asUInt
  // demand accesses from different ldu may hit different prefetch blocks
  val hit_pf_in_cache = PopCount(prefetch_info.loadinfo.map(t => t.hit_pf_in_cache) ++ Seq(prefetch_info.maininfo.hit_pf_in_cache))
  val hit_pf_in_mshr = io.missinfo.hit_pf_in_mshr
  val hit_pf = hit_pf_in_cache + hit_pf_in_mshr.asUInt
  val pf_useless = io.maininfo.pf_useless
  val prefetch_miss = io.missinfo.prefetch_miss
  val load_miss_to_mshr = io.missinfo.load_miss
  // ldu 0, 1, 2 can have multiple demand accesses at a time
  val demand_miss_in_ldu = PopCount(io.loadinfo.map(t => t.demand_miss))
  val pollution = PopCount(io.loadinfo.map(t => t.pollution))
  
  XSPerfAccumulate("l1DemandMiss", demand_miss_in_ldu)
  XSPerfAccumulate("l1prefetchSent", total_prefetch)
  XSPerfAccumulate("l1prefetchHit", hit_pf)
  XSPerfAccumulate("l1prefetchHitInCache", hit_pf_in_cache)
  XSPerfAccumulate("l1prefetchHitInMSHR", hit_pf_in_mshr)
  XSPerfAccumulate("l1prefetchLate", pf_late)
  XSPerfAccumulate("l1prefetchLateInCache", pf_late_in_cache)
  XSPerfAccumulate("l1prefetchLateInMSHR", pf_late_in_mshr)
  XSPerfAccumulate("l1prefetchUseless", pf_useless)
  XSPerfAccumulate("l1prefetchDropByNack", nack_prefetch)
  XSPerfAccumulate("mshr_count_Prefetch", prefetch_miss)
  XSPerfAccumulate("mshr_count_CPU", load_miss_to_mshr)
  XSPerfAccumulate("cache_pollution", pollution)

  // rolling by instr
  XSPerfRolling(
    "L1PrefetchAccuracyIns",
    hit_pf_in_cache, total_prefetch,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )
  
  XSPerfRolling(
    "L1PrefetchLatenessIns",
    hit_pf_in_mshr, prefetch_miss,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "L1PrefetchPollutionIns",
    pollution, demand_miss_in_ldu,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )

  XSPerfRolling(
    "IPCIns",
    io.debugRolling.robTrueCommit, 1.U,
    1000, io.debugRolling.robTrueCommit, clock, reset
  )
}

class L1PrefetchStatisticBundle()(implicit p: Parameters) extends XSBundle {
  val loadinfo = Vec(LoadPipelineWidth, new LoadPrefetchStatBundle)
  val missinfo = new MissPrefetchStatBundle
  val maininfo = new MainPrefetchStatBundle
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
  val pf_late_in_cache_cnt = RegInit(0.U((log2Up(param.TIMELY_CHECK_INTERVAL) + 1).W))
  val pf_late_in_mshr_cnt = RegInit(0.U((log2Up(param.TIMELY_CHECK_INTERVAL) + 1).W))

  val hit_pf_in_cache_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))
  val pf_useless_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))

  val back_off_cnt = RegInit(0.U((log2Up(param.BACK_OFF_INTERVAL) + 1).W))
  val low_conf_cnt = RegInit(0.U((log2Up(param.LOW_CONF_INTERVAL) + 1).W))

  val timely_reset = (total_prefetch_cnt === param.TIMELY_CHECK_INTERVAL.U) || (pf_late_in_cache_cnt >= param.TIMELY_CHECK_INTERVAL.U)
  val validity_reset = (hit_pf_in_cache_cnt + pf_useless_cnt) === param.VALIDITY_CHECK_INTERVAL.U
  val back_off_reset = back_off_cnt === param.BACK_OFF_INTERVAL.U
  val conf_reset = low_conf_cnt === param.LOW_CONF_INTERVAL.U

  val total_prefetch = io.prefetch_info.loadinfo.map(t => t.total_prefetch && param.isMyType(t.pf_source)).reduce(_ || _)
  val pf_late_in_cache = io.prefetch_info.loadinfo.map(t => t.pf_late_in_cache && param.isMyType(t.pf_source)).reduce(_ || _)
  val nack_prefetch = io.prefetch_info.loadinfo.map(t => t.nack_prefetch && param.isMyType(t.pf_source)).reduce(_ || _)
  val pf_late_in_mshr = io.prefetch_info.missinfo.pf_late_in_mshr && param.isMyType(io.prefetch_info.missinfo.pf_source)
  val hit_pf_in_cache = PopCount(io.prefetch_info.loadinfo.map(t => t.hit_pf_in_cache && param.isMyType(t.hit_source)) ++ Seq(io.prefetch_info.maininfo.hit_pf_in_cache && param.isMyType(io.prefetch_info.maininfo.hit_pf_source_in_cache)))
  val pf_useless = io.prefetch_info.maininfo.pf_useless && param.isMyType(io.prefetch_info.maininfo.pf_source_useless)
  val prefetch_miss = io.prefetch_info.missinfo.prefetch_miss && param.isMyType(io.prefetch_info.missinfo.pf_source)
  val hit_pf_in_mshr = io.prefetch_info.missinfo.hit_pf_in_mshr && param.isMyType(io.prefetch_info.missinfo.hit_pf_in_mshr_source)
  val hit_pf = hit_pf_in_cache + hit_pf_in_mshr.asUInt
  val pf_late = pf_late_in_cache.asUInt + pf_late_in_mshr.asUInt

  total_prefetch_cnt := Mux(timely_reset, 0.U, total_prefetch_cnt + total_prefetch)
  pf_late_in_cache_cnt := Mux(timely_reset, 0.U, pf_late_in_cache_cnt + pf_late_in_cache)
  pf_late_in_mshr_cnt := Mux(timely_reset, 0.U, pf_late_in_mshr_cnt + pf_late_in_mshr)
  hit_pf_in_cache_cnt := Mux(validity_reset, 0.U, hit_pf_in_cache_cnt + hit_pf_in_cache)
  pf_useless_cnt := Mux(validity_reset, 0.U, pf_useless_cnt + pf_useless)

  back_off_cnt := Mux(back_off_reset, 0.U, back_off_cnt + !enable)
  low_conf_cnt := Mux(conf_reset, 0.U, low_conf_cnt + !confidence.asBool)

  val trigger_late_hit = timely_reset && (pf_late_in_cache_cnt >= param.LATE_HIT_THRESHOLD.U)
  val trigger_late_miss = timely_reset && (pf_late_in_mshr_cnt >= param.LATE_MISS_THRESHOLD.U)
  val trigger_pf_useless = validity_reset && (pf_useless_cnt >= param.BAD_THRESHOLD.U)
  val trigger_disable = validity_reset && (pf_useless_cnt >= param.DISABLE_THRESHOLD.U)

  flush := Mux(flush, false.B, flush)
  enable := Mux(back_off_reset, true.B, enable)
  confidence := Mux(conf_reset, 1.U(1.W), confidence)

  when(trigger_pf_useless) {
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

  val pfTypes: Seq[(String, UInt => Bool)] = Seq(
    // (name, PfSource)
    ("Demand", isDemand),
    ("Stream", isFromStream),
    ("Stride", isFromStride),
    ("Berti", isFromBerti)
  )
  XSPerfAccumulate(s"l1prefetchSent${param.name}", total_prefetch)
  XSPerfAccumulate(s"l1prefetchHit${param.name}", hit_pf)
  XSPerfAccumulate(s"l1prefetchHitInCache${param.name}", hit_pf_in_cache)
  XSPerfAccumulate(s"l1prefetchHitInMSHR${param.name}", hit_pf_in_mshr)
  XSPerfAccumulate(s"l1prefetchLate${param.name}", pf_late)
  XSPerfAccumulate(s"l1prefetchLateInCache${param.name}", pf_late_in_cache)
  XSPerfAccumulate(s"l1prefetchLateInMSHR${param.name}", pf_late_in_mshr)
  for ((x, isLateHitX) <- pfTypes) {
    XSPerfAccumulate(s"l1prefetchLateInCache${param.name}_Hit${x}", PopCount(
      io.prefetch_info.loadinfo.map(y =>
        y.pf_late_in_cache && param.isMyType(y.pf_source) && isLateHitX(y.pf_late_in_cache_source)
    )))
    XSPerfAccumulate(s"l1prefetchLateInMSHR${param.name}_Hit${x}",
      io.prefetch_info.missinfo.pf_late_in_mshr && param.isMyType(io.prefetch_info.missinfo.pf_source)
        && isLateHitX(io.prefetch_info.missinfo.pf_late_in_mshr_source)
    )
  }
  XSPerfAccumulate(s"l1prefetchUseless${param.name}", pf_useless)
  XSPerfAccumulate(s"l1prefetchDropByNack${param.name}", nack_prefetch)
  XSPerfAccumulate(s"mshr_count_Prefetch${param.name}", prefetch_miss)
  for(i <- (0 until DEPTH_BITS)) {
    val t = (1 << i)
    XSPerfAccumulate(s"${param.name}_depth${t}", depth === t.U)
  }
  XSPerfAccumulate(s"${param.name}_trigger_disable", trigger_disable)
  XSPerfAccumulate(s"${param.name}_trigger_late_hit", trigger_late_hit)
  XSPerfAccumulate(s"${param.name}_trigger_late_miss", trigger_late_miss)
  XSPerfAccumulate(s"${param.name}_trigger_pf_useless", trigger_pf_useless)
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
    case "berti" => new BertiMonitorParam()
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

class BertiMonitorParam extends PrefetcherMonitorParam with HasL1PrefetchSourceParameter {
  override val name: String = "Berti"
  override def isMyType(value: UInt) = value === L1_HW_PREFETCH_BERTI
}
