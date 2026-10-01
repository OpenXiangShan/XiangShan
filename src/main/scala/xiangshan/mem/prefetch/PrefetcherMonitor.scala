package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.backend.rob.RobDebugRollingIO

class PrefetchControlBundle()(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val dynamic_depth = UInt(DEPTH_BITS.W)
  val flush = Bool()
  val enable = Bool()
  val confidence = UInt(1.W)
}

class PipePrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
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

class ReplPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val pf_useless = Bool() // from mainpipe replace, prefetch block but not accessed
  val pf_source_useless = UInt(L1PfSourceBits.W)

  val hit_pf_in_cache = Bool() // from mainpipe, refill accessed pf block | store req hit pf block
  val hit_pf_source_in_cache = UInt(L1PfSourceBits.W)
}

class BufferPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val first_use = Vec(LoadPipelineWidth, Valid(UInt(L1PfSourceBits.W)))
  val unused_exit = Valid(UInt(L1PfSourceBits.W))
}

class MissPrefetchStatBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val pf_late_in_mshr = Bool() // from missqueue, pf req match a existing mshr
  val pf_late_in_mshr_source = UInt(L1PfSourceBits.W) // from missqueue, pf req match a existing mshr, it's source type
  val prefetch_miss = Bool() // from missqueue, pf req allocate a new mshr
  val pf_source = UInt(L1PfSourceBits.W)

  val hit_pf_in_mshr = UInt(MissReqPortCount.W) // from missqueue, demand miss match a existing pf mshr, then clear pf flag
  val hit_pf_in_mshr_source = Vec(MissReqPortCount, UInt(L1PfSourceBits.W)) // from missqueue, the pf source of demand miss matched
  val load_miss = UInt(MissReqPortCount.W) // from missqueue, load demand miss allocate a new mshr
}

class PrefetcherMonitorBundle()(implicit p: Parameters) extends XSBundle with HasL1PrefetchSourceParameter {
  val loadinfo = Input(Vec(LoadPipelineWidth, new PipePrefetchStatBundle))
  val maininfo = Input(new PipePrefetchStatBundle)
  val missinfo = Input(new MissPrefetchStatBundle)
  val replinfo = Input(new ReplPrefetchStatBundle)
  val bufferinfo = Input(new BufferPrefetchStatBundle)

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
  prefetch_info.replinfo := io.replinfo
  prefetch_info.bufferinfo := io.bufferinfo

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
  val total_prefetch = io.loadinfo.map(t => t.total_prefetch).reduce(_ || _) || io.maininfo.total_prefetch
  val nack_prefetch_raw = io.loadinfo.map(t => t.nack_prefetch).reduce(_ || _) || io.maininfo.nack_prefetch
  val pf_late_in_cache = io.loadinfo.map(t => t.pf_late_in_cache).reduce(_ || _) || io.maininfo.pf_late_in_cache
  val pf_late_in_mshr = io.missinfo.pf_late_in_mshr
  val pf_late = pf_late_in_cache.asUInt + pf_late_in_mshr.asUInt
  val nack_prefetch = nack_prefetch_raw && !pf_late_in_mshr

  // demand accesses from different ldu may hit different prefetch blocks
  val hit_pf_in_cache = PopCount(prefetch_info.loadinfo.map(t => t.hit_pf_in_cache) ++
    Seq(prefetch_info.maininfo.hit_pf_in_cache) ++ io.bufferinfo.first_use.map(_.valid))
  val hit_pf_in_mshr = PopCount(io.missinfo.hit_pf_in_mshr)
  val hit_pf = hit_pf_in_cache +& hit_pf_in_mshr
  val pf_useless = PopCount(Seq(io.replinfo.pf_useless, io.bufferinfo.unused_exit.valid))
  val prefetch_miss = io.missinfo.prefetch_miss
  val load_miss_to_mshr = PopCount(io.missinfo.load_miss)
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
  val loadinfo = Vec(LoadPipelineWidth, new PipePrefetchStatBundle)
  val maininfo = new PipePrefetchStatBundle
  val missinfo = new MissPrefetchStatBundle
  val replinfo = new ReplPrefetchStatBundle
  val bufferinfo = new BufferPrefetchStatBundle
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

  io.pf_ctrl.dynamic_depth := depth
  io.pf_ctrl.flush := flush
  io.pf_ctrl.enable := enable
  io.pf_ctrl.confidence := confidence

  val back_off_cnt = RegInit(0.U((log2Up(param.BACK_OFF_INTERVAL) + 1).W))
  val low_conf_cnt = RegInit(0.U((log2Up(param.LOW_CONF_INTERVAL) + 1).W))
  val back_off_reset = back_off_cnt === param.BACK_OFF_INTERVAL.U
  val conf_reset = low_conf_cnt === param.LOW_CONF_INTERVAL.U
  back_off_cnt := Mux(back_off_reset, 0.U, back_off_cnt + !enable)
  low_conf_cnt := Mux(confidence.asBool, 0.U, low_conf_cnt + 1.U)

  enable := Mux(back_off_reset, true.B, enable)
  confidence := Mux(conf_reset, 1.U(1.W), confidence)
  flush := Mux(flush, false.B, flush)

  val total_prefetch = io.prefetch_info.loadinfo.map(t => t.total_prefetch && param.isMyType(t.pf_source)).reduce(_ || _) || (io.prefetch_info.maininfo.total_prefetch && param.isMyType(io.prefetch_info.maininfo.pf_source))
  val pf_late_in_cache = io.prefetch_info.loadinfo.map(t => t.pf_late_in_cache && param.isMyType(t.pf_source)).reduce(_ || _) || (io.prefetch_info.maininfo.pf_late_in_cache && param.isMyType(io.prefetch_info.maininfo.pf_source))
  val nack_prefetch_raw = io.prefetch_info.loadinfo.map(t => t.nack_prefetch && param.isMyType(t.pf_source)).reduce(_ || _) || (io.prefetch_info.maininfo.nack_prefetch && param.isMyType(io.prefetch_info.maininfo.pf_source))
  val pf_late_in_mshr = io.prefetch_info.missinfo.pf_late_in_mshr && param.isMyType(io.prefetch_info.missinfo.pf_source)
  val nack_prefetch = nack_prefetch_raw && !pf_late_in_mshr
  val hit_pf_in_cache = PopCount(io.prefetch_info.loadinfo.map(t => t.hit_pf_in_cache && param.isMyType(t.hit_source)) ++
    Seq(io.prefetch_info.maininfo.hit_pf_in_cache && param.isMyType(io.prefetch_info.maininfo.hit_source)) ++
    io.prefetch_info.bufferinfo.first_use.map(t => t.valid && param.isMyType(t.bits)))
  val pf_useless = PopCount(Seq(
    io.prefetch_info.replinfo.pf_useless && param.isMyType(io.prefetch_info.replinfo.pf_source_useless),
    io.prefetch_info.bufferinfo.unused_exit.valid && param.isMyType(io.prefetch_info.bufferinfo.unused_exit.bits)))
  val prefetch_miss = io.prefetch_info.missinfo.prefetch_miss && param.isMyType(io.prefetch_info.missinfo.pf_source)
  val hit_pf_in_mshr = PopCount((0 until MissReqPortCount).map(i => io.prefetch_info.missinfo.hit_pf_in_mshr(i) && param.isMyType(io.prefetch_info.missinfo.hit_pf_in_mshr_source(i))))
  val hit_pf = hit_pf_in_cache + hit_pf_in_mshr
  val pf_late = pf_late_in_cache.asUInt + pf_late_in_mshr

  // DynamicPrefetcher
  val base_depth = Constantin.createRecord(s"${param.name}_depth${p(XSCoreParamsKey).HartId}", initValue = 4)
  val MAX_BITS = 7
  val max_depth = (1 << (MAX_BITS-1)).U(DEPTH_BITS.W)
  val at_base_depth = depth === base_depth
  val at_max_depth = depth === max_depth

  // stat
  val sent_cnt = RegInit(0.U((log2Up(2*param.WINDOW_SIZE)).W))
  val cur_late = RegInit(0.U((log2Up(4*param.WINDOW_SIZE)).W))
  val cur_hit_pf_in_cache = RegInit(0.U((log2Up(2*param.WINDOW_SIZE)).W))
  val cur_useless = RegInit(0.U((log2Up(2*param.WINDOW_SIZE)).W))
  val prev_late = RegInit(0.U((log2Up(4*param.WINDOW_SIZE)).W))
  val prev_hit_pf_in_cache = RegInit(0.U((log2Up(2*param.WINDOW_SIZE)).W))
  val prev_useless = RegInit(0.U((log2Up(2*param.WINDOW_SIZE)).W))

  val window_end = sent_cnt === param.WINDOW_SIZE.U
  val window_fire = window_end && enable

  sent_cnt := Mux(window_end, 0.U, sent_cnt + total_prefetch)
  cur_late := Mux(window_end, 0.U, cur_late + pf_late + hit_pf_in_mshr)
  cur_hit_pf_in_cache := Mux(window_end, 0.U, cur_hit_pf_in_cache + hit_pf_in_cache)
  cur_useless := Mux(window_end, 0.U, cur_useless + pf_useless)

  val window_fire_interval = RegInit(0.U(32.W))
  val window_fire_seen = RegInit(false.B)
  val window_fire_interval_sample = window_fire && window_fire_seen
  val window_fire_interval_value = window_fire_interval
  when(window_fire) {
    window_fire_interval := 1.U
    window_fire_seen := true.B
  }.elsewhen(window_fire_interval =/= "hffffffff".U(32.W)) {
    window_fire_interval := window_fire_interval + 1.U
  }

  // disable
  val disable_request = RegInit(false.B)
  val down_request = RegInit(false.B)
  val validity_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))
  val useless_cnt = RegInit(0.U((log2Up(param.VALIDITY_CHECK_INTERVAL) + 1).W))
  val validity_check = validity_cnt >= param.VALIDITY_CHECK_INTERVAL.U
  val trigger_disable = validity_check && useless_cnt >= param.DISABLE_THRESHOLD.U
  val trigger_down = validity_check && useless_cnt >= param.BAD_THRESHOLD.U
  val old_depth = RegNext(depth, init = base_depth)
  val depth_change_fire = old_depth =/= depth

  when(depth_change_fire) {
    validity_cnt := 0.U
    useless_cnt := 0.U
    disable_request := false.B
    down_request := false.B
  }.otherwise {
    validity_cnt := Mux(validity_check, 0.U, validity_cnt + hit_pf + pf_useless)
    useless_cnt := Mux(validity_check, 0.U, useless_cnt + pf_useless)
    disable_request := Mux(validity_check, trigger_disable, disable_request)
    down_request := Mux(validity_check, trigger_down, down_request)
  }

  // block
  val up_blocked_depth = RegInit(0.U(DEPTH_BITS.W))
  val up_back_off_cnt = RegInit(0.U(5.W))
  val up_target = depth << 1
  val up_blocked = up_back_off_cnt =/= 0.U && up_target === up_blocked_depth
  val high_depth_backoff = Mux(depth >= 64.U(DEPTH_BITS.W), 16.U(5.W),
    Mux(depth >= 32.U(DEPTH_BITS.W), 8.U(5.W), 2.U(5.W))
  )

  // State
  val s_idle :: s_buffer :: s_decision :: s_skip1 :: s_skip2 :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val cur_late_high = cur_late >= param.LATE_HIT_THRESHOLD.U
  val bad_return_by_hit = cur_hit_pf_in_cache + param.HIT_MARGIN.U <= prev_hit_pf_in_cache
  val bad_return_by_useless = cur_useless >= prev_useless + param.HIT_MARGIN.U
  val bad_return = bad_return_by_hit || bad_return_by_useless
  val pending_disable_request = disable_request || trigger_disable
  val pending_down_request = down_request || trigger_down
  val up = cur_late_high && !up_blocked && !pending_disable_request && !pending_down_request && !at_max_depth
  val block = cur_late_high && up_blocked && !pending_disable_request && !pending_down_request && !at_max_depth
  val bad_down = pending_down_request && !pending_disable_request && !at_base_depth
  val disable_down = pending_disable_request && !at_base_depth
  val disable = pending_disable_request && at_base_depth

  val in_idle = state === s_idle
  val in_buffer = state === s_buffer
  val in_decision = state === s_decision
  val up_fire = window_fire && in_idle && up
  val block_fire = window_fire && in_idle && block
  val bad_down_fire = window_fire && in_idle && bad_down
  val disable_down_fire = window_fire && in_idle && disable_down
  val disable_fire = window_fire && in_idle && disable
  val decision_accept_fire = window_fire && in_decision && !bad_return
  val bad_return_fire = window_fire && in_decision && bad_return
  val bad_return_by_hit_fire = window_fire && in_decision && bad_return_by_hit
  val bad_return_by_useless_fire = window_fire && in_decision && bad_return_by_useless
  val bad_return_by_both_fire = bad_return_by_hit_fire && bad_return_by_useless
  val bad_return_by_hit_only_fire = bad_return_fire && bad_return_by_hit && !bad_return_by_useless
  val bad_return_by_useless_only_fire = bad_return_fire && !bad_return_by_hit && bad_return_by_useless

  val idle_late_high_fire = window_fire && in_idle && cur_late_high
  val idle_no_action_fire = window_fire && in_idle && !(up || block || disable_down || bad_down || disable)
  val up_suppressed_by_at_max_fire = idle_late_high_fire && at_max_depth
  val up_suppressed_by_pending_disable_fire = idle_late_high_fire && !at_max_depth && pending_disable_request
  val up_suppressed_by_pending_down_fire = idle_late_high_fire && !at_max_depth && !pending_disable_request && pending_down_request
  val up_suppressed_by_backoff_fire = idle_late_high_fire && !at_max_depth && !pending_disable_request && !pending_down_request && up_blocked
  val pending_down_at_base_fire = window_fire && in_idle && at_base_depth && pending_down_request && !pending_disable_request
  val pending_disable_at_base_fire = window_fire && in_idle && at_base_depth && pending_disable_request
  val late_high_pending_down_at_base_fire = idle_late_high_fire && at_base_depth && pending_down_request && !pending_disable_request
  val late_high_pending_disable_at_base_fire = idle_late_high_fire && at_base_depth && pending_disable_request

  val validity_check_fire = validity_check && enable && !depth_change_fire
  val trigger_down_fire = trigger_down && validity_check_fire
  val trigger_disable_fire = trigger_disable && validity_check_fire
  val bad_down_by_reg_fire = bad_down_fire && down_request
  val bad_down_by_trigger_fire = bad_down_fire && trigger_down
  val disable_down_by_reg_fire = disable_down_fire && disable_request
  val disable_down_by_trigger_fire = disable_down_fire && trigger_disable
  val disable_by_reg_fire = disable_fire && disable_request
  val disable_by_trigger_fire = disable_fire && trigger_disable
  val trigger_down_and_disable_fire = trigger_down_fire && trigger_disable_fire
  val hit_loss = Mux(prev_hit_pf_in_cache > cur_hit_pf_in_cache, prev_hit_pf_in_cache - cur_hit_pf_in_cache, 0.U)
  val hit_gain = Mux(cur_hit_pf_in_cache > prev_hit_pf_in_cache, cur_hit_pf_in_cache - prev_hit_pf_in_cache, 0.U)
  val late_reduction = Mux(prev_late > cur_late, prev_late - cur_late, 0.U)
  val late_increase = Mux(cur_late > prev_late, cur_late - prev_late, 0.U)
  val useless_increase = Mux(cur_useless > prev_useless, cur_useless - prev_useless, 0.U)
  val useless_decrease = Mux(prev_useless > cur_useless, prev_useless - cur_useless, 0.U)

  when(window_fire) {
    switch(state) {
      is(s_idle) {
        when (up) {
          depth := depth << 1
          state := s_buffer
          prev_late := cur_late
          prev_hit_pf_in_cache := cur_hit_pf_in_cache
          prev_useless := cur_useless
        }.elsewhen(block) {
          up_back_off_cnt := up_back_off_cnt - 1.U
        }.elsewhen(disable_down) {
          depth := base_depth
          state := s_skip1
        }.elsewhen(bad_down) {
          depth := depth >> 1
          state := s_skip1
          up_blocked_depth := depth
          up_back_off_cnt := high_depth_backoff
        }.elsewhen(disable) {
          enable := false.B
          flush := true.B
          confidence := 0.U(1.W)
          up_back_off_cnt := 0.U
        }
      }
      is(s_buffer) {
        state := s_decision
      }
      is(s_decision) {
        when (bad_return) {
          up_back_off_cnt := high_depth_backoff
          up_blocked_depth := depth
          depth := Mux(at_base_depth, base_depth, depth >> 1)
        }
        state := s_idle
      }
      is(s_skip1) {
        state := s_skip2
      }
      is(s_skip2) {
        state := s_idle
      }
    }
  }
  when(reset.asBool) {
    depth := base_depth
    up_blocked_depth := base_depth
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
  XSPerfAccumulate(s"${param.name}_window_fire", window_fire)
  XSPerfAccumulate(s"${param.name}_state_idle_window", window_fire && in_idle)
  XSPerfAccumulate(s"${param.name}_state_buffer_window", window_fire && in_buffer)
  XSPerfAccumulate(s"${param.name}_state_decision_window", window_fire && in_decision)
  XSPerfAccumulate(s"${param.name}_state_skip1_window", window_fire && state === s_skip1)
  XSPerfAccumulate(s"${param.name}_state_skip2_window", window_fire && state === s_skip2)
  XSPerfAccumulate(s"${param.name}_late_high_window", window_fire && cur_late_high)
  XSPerfAccumulate(s"${param.name}_up_fire", up_fire)
  XSPerfAccumulate(s"${param.name}_up_blocked_fire", block_fire)
  XSPerfAccumulate(s"${param.name}_bad_down_fire", bad_down_fire)
  XSPerfAccumulate(s"${param.name}_down_to_base_fire", disable_down_fire)
  XSPerfAccumulate(s"${param.name}_disable_request_fire", window_fire && in_idle && disable_request)
  XSPerfAccumulate(s"${param.name}_disable_at_base_fire", disable_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_fire", bad_return_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_by_hit_fire", bad_return_by_hit_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_by_useless_fire", bad_return_by_useless_fire)
  XSPerfAccumulate(s"${param.name}_decision_accept_fire", decision_accept_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_by_hit_only_fire", bad_return_by_hit_only_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_by_useless_only_fire", bad_return_by_useless_only_fire)
  XSPerfAccumulate(s"${param.name}_bad_return_by_both_fire", bad_return_by_both_fire)
  XSPerfAccumulate(s"${param.name}_idle_no_action_fire", idle_no_action_fire)
  XSPerfAccumulate(s"${param.name}_up_suppressed_by_at_max_fire", up_suppressed_by_at_max_fire)
  XSPerfAccumulate(s"${param.name}_up_suppressed_by_pending_disable_fire", up_suppressed_by_pending_disable_fire)
  XSPerfAccumulate(s"${param.name}_up_suppressed_by_pending_down_fire", up_suppressed_by_pending_down_fire)
  XSPerfAccumulate(s"${param.name}_up_suppressed_by_backoff_fire", up_suppressed_by_backoff_fire)
  XSPerfAccumulate(s"${param.name}_pending_down_at_base_fire", pending_down_at_base_fire)
  XSPerfAccumulate(s"${param.name}_pending_disable_at_base_fire", pending_disable_at_base_fire)
  XSPerfAccumulate(s"${param.name}_late_high_pending_down_at_base_fire", late_high_pending_down_at_base_fire)
  XSPerfAccumulate(s"${param.name}_late_high_pending_disable_at_base_fire", late_high_pending_disable_at_base_fire)
  XSPerfAccumulate(s"${param.name}_validity_check_fire", validity_check_fire)
  XSPerfAccumulate(s"${param.name}_trigger_down_fire", trigger_down_fire)
  XSPerfAccumulate(s"${param.name}_trigger_disable_fire", trigger_disable_fire)
  XSPerfAccumulate(s"${param.name}_trigger_down_and_disable_fire", trigger_down_and_disable_fire)
  XSPerfAccumulate(s"${param.name}_bad_down_by_reg_fire", bad_down_by_reg_fire)
  XSPerfAccumulate(s"${param.name}_bad_down_by_trigger_fire", bad_down_by_trigger_fire)
  XSPerfAccumulate(s"${param.name}_down_to_base_by_reg_fire", disable_down_by_reg_fire)
  XSPerfAccumulate(s"${param.name}_down_to_base_by_trigger_fire", disable_down_by_trigger_fire)
  XSPerfAccumulate(s"${param.name}_disable_by_reg_fire", disable_by_reg_fire)
  XSPerfAccumulate(s"${param.name}_disable_by_trigger_fire", disable_by_trigger_fire)
  XSPerfAccumulate(s"${param.name}_window_fire_interval_sample", window_fire_interval_sample)
  XSPerfAccumulate(s"${param.name}_window_fire_interval_sum", Mux(window_fire_interval_sample, window_fire_interval_value, 0.U))
  XSPerfAccumulate(s"${param.name}_window_late_sum", Mux(window_fire, cur_late, 0.U))
  XSPerfAccumulate(s"${param.name}_window_hit_cache_sum", Mux(window_fire, cur_hit_pf_in_cache, 0.U))
  XSPerfAccumulate(s"${param.name}_window_useless_sum", Mux(window_fire, cur_useless, 0.U))
  XSPerfAccumulate(s"${param.name}_up_prev_late_sum", Mux(up_fire, cur_late, 0.U))
  XSPerfAccumulate(s"${param.name}_up_prev_hit_cache_sum", Mux(up_fire, cur_hit_pf_in_cache, 0.U))
  XSPerfAccumulate(s"${param.name}_up_prev_useless_sum", Mux(up_fire, cur_useless, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_prev_late_sum", Mux(window_fire && in_decision, prev_late, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_prev_hit_cache_sum", Mux(window_fire && in_decision, prev_hit_pf_in_cache, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_prev_useless_sum", Mux(window_fire && in_decision, prev_useless, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_cur_late_sum", Mux(window_fire && in_decision, cur_late, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_cur_hit_cache_sum", Mux(window_fire && in_decision, cur_hit_pf_in_cache, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_cur_useless_sum", Mux(window_fire && in_decision, cur_useless, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_late_reduction_sum", Mux(window_fire && in_decision, late_reduction, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_late_increase_sum", Mux(window_fire && in_decision, late_increase, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_hit_loss_sum", Mux(window_fire && in_decision, hit_loss, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_hit_gain_sum", Mux(window_fire && in_decision, hit_gain, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_useless_increase_sum", Mux(window_fire && in_decision, useless_increase, 0.U))
  XSPerfAccumulate(s"${param.name}_decision_useless_decrease_sum", Mux(window_fire && in_decision, useless_decrease, 0.U))
  XSPerfAccumulate(s"${param.name}_validity_useless_sum", Mux(validity_check_fire, useless_cnt, 0.U))
  XSPerfAccumulate(s"${param.name}_validity_total_sum", Mux(validity_check_fire, validity_cnt, 0.U))
  XSPerfAccumulate(s"${param.name}_bad_down_useless_cnt_sum", Mux(bad_down_fire, useless_cnt, 0.U))
  XSPerfAccumulate(s"${param.name}_bad_down_validity_cnt_sum", Mux(bad_down_fire, validity_cnt, 0.U))

  def perfCountHistogram(name: String, value: UInt, sample: Bool): Unit = {
    XSPerfHistogram(s"${param.name}_${name}_0_64", value, sample, 0, 64, 4, true, true)
    XSPerfHistogram(s"${param.name}_${name}_64_256", value, sample, 64, 256, 16, true, true)
    XSPerfHistogram(s"${param.name}_${name}_256_1024", value, sample, 256, 1024, 64, true, false)
    XSPerfMax(s"${param.name}_${name}", value, sample)
  }

  def perfIntervalHistogram(name: String, value: UInt, sample: Bool): Unit = {
    XSPerfHistogram(s"${param.name}_${name}_0_1024", value, sample, 0, 1024, 64, true, true)
    XSPerfHistogram(s"${param.name}_${name}_1024_8192", value, sample, 1024, 8192, 512, true, true)
    XSPerfHistogram(s"${param.name}_${name}_8192_65536", value, sample, 8192, 65536, 4096, true, false)
    XSPerfMax(s"${param.name}_${name}", value, sample)
  }

  perfIntervalHistogram("window_fire_interval_cycle", window_fire_interval_value, window_fire_interval_sample)
  perfCountHistogram("window_cur_late", cur_late, window_fire)
  perfCountHistogram("window_cur_hit_cache", cur_hit_pf_in_cache, window_fire)
  perfCountHistogram("window_cur_useless", cur_useless, window_fire)
  perfCountHistogram("idle_cur_useless", cur_useless, window_fire && in_idle)
  perfCountHistogram("decision_cur_useless", cur_useless, window_fire && in_decision)
  perfCountHistogram("up_prev_late", cur_late, up_fire)
  perfCountHistogram("up_prev_hit_cache", cur_hit_pf_in_cache, up_fire)
  perfCountHistogram("up_prev_useless", cur_useless, up_fire)
  perfCountHistogram("decision_prev_late", prev_late, window_fire && in_decision)
  perfCountHistogram("decision_cur_late", cur_late, window_fire && in_decision)
  perfCountHistogram("decision_late_reduction", late_reduction, window_fire && in_decision)
  perfCountHistogram("decision_late_increase", late_increase, window_fire && in_decision)
  perfCountHistogram("decision_hit_loss", hit_loss, window_fire && in_decision)
  perfCountHistogram("decision_hit_gain", hit_gain, window_fire && in_decision)
  perfCountHistogram("decision_useless_increase", useless_increase, window_fire && in_decision)
  perfCountHistogram("decision_useless_decrease", useless_decrease, window_fire && in_decision)
  perfCountHistogram("bad_return_hit_loss", hit_loss, bad_return_fire)
  perfCountHistogram("bad_return_useless_increase", useless_increase, bad_return_fire)
  perfCountHistogram("validity_useless_cnt", useless_cnt, validity_check_fire)
  perfCountHistogram("validity_total_cnt", validity_cnt, validity_check_fire)
  perfCountHistogram("bad_down_useless_cnt", useless_cnt, bad_down_fire)
  perfCountHistogram("bad_down_validity_cnt", validity_cnt, bad_down_fire)
  for(i <- (0 until MAX_BITS)) {
    val t = (1 << i)
    val at_depth = depth === t.U
    XSPerfAccumulate(s"${param.name}_depth${t}", at_depth && total_prefetch)
    XSPerfAccumulate(s"${param.name}_late_high_at_depth${t}", window_fire && at_depth && cur_late_high)
    XSPerfAccumulate(s"${param.name}_disable_request_at_depth${t}", window_fire && at_depth && disable_request)
    XSPerfAccumulate(s"${param.name}_up_from_depth${t}", up_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_up_blocked_from_depth${t}", block_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_bad_down_from_depth${t}", bad_down_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_down_to_base_from_depth${t}", disable_down_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_bad_return_at_depth${t}", bad_return_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_bad_return_by_hit_at_depth${t}", bad_return_by_hit_fire && at_depth)
    XSPerfAccumulate(s"${param.name}_bad_return_by_useless_at_depth${t}", bad_return_by_useless_fire && at_depth)
  }
  XSPerfAccumulate(s"${param.name}_trigger_disable", RegNext(enable) && !enable)
  XSPerfAccumulate(s"${param.name}_disable_time", !enable)
  XSPerfAccumulate(s"${param.name}_trigger_depth_up", old_depth < depth)
  XSPerfAccumulate(s"${param.name}_trigger_depth_down", old_depth > depth)

  assert(depth =/= 0.U, s"${param.name}_depth should not be zero")
}

abstract class PrefetcherMonitorParam {
  val name: String
  def isMyType(value: UInt): Bool

  val VALIDITY_CHECK_INTERVAL = 1024
  val DISABLE_THRESHOLD = 900
  val BAD_THRESHOLD = 256

  val WINDOW_SIZE = 512
  val LATE_HIT_THRESHOLD = 24
  val HIT_MARGIN = 24

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
