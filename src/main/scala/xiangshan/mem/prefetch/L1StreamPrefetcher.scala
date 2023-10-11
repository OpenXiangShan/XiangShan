package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import xiangshan.cache.HasDCacheParameters
import xiangshan.cache.mmu._
import xiangshan.mem.{L1PrefetchReq, LdPrefetchTrainBundle}
import xiangshan.mem.trace._
import xiangshan.mem.L1PrefetchSource

trait HasStreamPrefetchHelper extends HasL1PrefetchHelper {
  // capacity related
  val STREAM_FILTER_SIZE = 4
  val BIT_VEC_ARRAY_SIZE = 16
  val ACTIVE_THRESHOLD = BIT_VEC_WITDH - 4
  val INIT_DEC_MODE = false

  // bit_vector [StreamBitVectorBundle]:
  // `X`: valid; `.`: invalid; `H`: hit
  // [X X X X X X X X X . . H . X X X]                                                         [. . X X X X . . . . . . . . . .]
  //                    hit in 12th slot & active           --------------------->             prefetch bit_vector [StreamPrefetchReqBundle]
  //                        |  <---------------------------- depth ---------------------------->
  //                                                                                           | <-- width -- >
  val DEPTH_BYTES = 1024
  val DEPTH_CACHE_BLOCKS = DEPTH_BYTES / dcacheParameters.blockBytes
  val WIDTH_BYTES = 128
  val WIDTH_CACHE_BLOCKS = WIDTH_BYTES / dcacheParameters.blockBytes

  val L2_DEPTH_RATIO = 2
  val L2_WIDTH_BYTES = WIDTH_BYTES * 2
  val L2_WIDTH_CACHE_BLOCKS = L2_WIDTH_BYTES / dcacheParameters.blockBytes

  val L3_DEPTH_RATIO = 3
  val L3_WIDTH_BYTES = WIDTH_BYTES * 2 * 2
  val L3_WIDTH_CACHE_BLOCKS = L3_WIDTH_BYTES / dcacheParameters.blockBytes

  val DEPTH_LOOKAHEAD = 6
  val DEPTH_BITS = log2Up(DEPTH_CACHE_BLOCKS) + DEPTH_LOOKAHEAD

  val ENABLE_DECR_MODE = false
  val ENABLE_STRICT_ACTIVE_DETECTION = true

  // constraints
  require((DEPTH_BYTES >= REGION_SIZE) && ((DEPTH_BYTES % REGION_SIZE) == 0) && ((DEPTH_BYTES / REGION_SIZE) > 0))
  require(((VADDR_HASH_WIDTH * 3) + BLK_ADDR_RAW_WIDTH) <= REGION_TAG_BITS)
  require(WIDTH_BYTES >= dcacheParameters.blockBytes)
}

class StreamBitVectorBundle(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val tag = UInt(REGION_TAG_BITS.W)
  val bit_vec = UInt(BIT_VEC_WITDH.W)
  val active = Bool()
  // cnt can be optimized
  val cnt = UInt((log2Up(BIT_VEC_WITDH) + 1).W)
  val decr_mode = Bool()

  def reset(index: Int) = {
    tag := index.U
    bit_vec := 0.U
    active := false.B
    cnt := 0.U
    decr_mode := INIT_DEC_MODE.B
  }

  def tag_match(new_tag: UInt): Bool = {
    region_hash_tag(tag) === region_hash_tag(new_tag)
  }

  def alloc(alloc_tag: UInt, alloc_bit_vec: UInt, alloc_active: Bool, alloc_decr_mode: Bool) = {
    tag := alloc_tag
    bit_vec := alloc_bit_vec
    active := alloc_active
    cnt := 1.U
    if(ENABLE_DECR_MODE) {
      decr_mode := alloc_decr_mode
    }else {
      decr_mode := INIT_DEC_MODE.B
    }

    assert(PopCount(alloc_bit_vec) === 1.U, "alloc vector should be one hot")
  }

  def update(update_bit_vec: UInt, update_active: Bool) = {
    // if the slot is 0 before, increment cnt
    val cnt_en = !((bit_vec & update_bit_vec).orR)
    val cnt_next = Mux(cnt_en, cnt + 1.U, cnt)

    bit_vec := bit_vec | update_bit_vec
    cnt := cnt_next
    when(cnt_next >= ACTIVE_THRESHOLD.U) {
      active := true.B
    }
    when(update_active) {
      active := true.B
    }

    assert(PopCount(update_bit_vec) === 1.U, "update vector should be one hot")
    assert(cnt <= BIT_VEC_WITDH.U, "cnt should always less than bit vector size")
  }
}

class StreamPrefetchReqBundle(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val region = UInt(REGION_TAG_BITS.W)
  val bit_vec = UInt(BIT_VEC_WITDH.W)
  val sink = UInt(SINK_BITS.W)
  val source = new L1PrefetchSource()

  // align prefetch vaddr and width to region
  def getStreamPrefetchReqBundle(vaddr: UInt, width: Int, decr_mode: Bool, sink: UInt, source: UInt): StreamPrefetchReqBundle = {
    val res = Wire(new StreamPrefetchReqBundle)
    res.region := get_region_tag(vaddr)
    res.sink := sink
    res.source.value := source

    val region_bits = get_region_bits(vaddr)
    val region_bit_vec = UIntToOH(region_bits)
    res.bit_vec := Mux(
      decr_mode,
      (0 until width).map{ case i => region_bit_vec >> i}.reduce(_ | _),
      (0 until width).map{ case i => region_bit_vec << i}.reduce(_ | _)
    )

    assert(PopCount(res.bit_vec) <= width.U, "actual prefetch block number should less than or equals to WIDTH_CACHE_BLOCKS")
    assert(PopCount(res.bit_vec) >= 1.U, "at least one block should be included")
    assert(sink <= SINK_L3, "invalid sink")
    for(i <- 0 until BIT_VEC_WITDH) {
      when(decr_mode) {
        when(i.U > region_bits) {
          assert(res.bit_vec(i) === 0.U, s"res.bit_vec(${i}) is not zero in decr_mode, prefetch vector is wrong!")
        }.elsewhen(i.U === region_bits) {
          assert(res.bit_vec(i) === 1.U, s"res.bit_vec(${i}) is zero in decr_mode, prefetch vector is wrong!")
        }
      }.otherwise {
        when(i.U < region_bits) {
          assert(res.bit_vec(i) === 0.U, s"res.bit_vec(${i}) is not zero in incr_mode, prefetch vector is wrong!")
        }.elsewhen(i.U === region_bits) {
          assert(res.bit_vec(i) === 1.U, s"res.bit_vec(${i}) is zero in decr_mode, prefetch vector is wrong!")
        }
      }
    }

    res
  }
}

class StreamBitVectorArray(implicit p: Parameters) extends XSModule with HasStreamPrefetchHelper {
  val io = IO(new XSBundle {
    val enable = Input(Bool())
    // TODO: flush all entry when process changing happens, or disable stream prefetch for a while
    val flush = Input(Bool())
    val dynamic_depth = Input(UInt(DEPTH_BITS.W))
    val train_req = Flipped(DecoupledIO(new PrefetchReqBundle))
    val prefetch_req = ValidIO(new StreamPrefetchReqBundle)

    // Stride send lookup req here
    val stream_lookup_req  = Flipped(ValidIO(new PrefetchReqBundle))
    val stream_lookup_resp = Output(Bool())
  })

  val array = Reg(Vec(BIT_VEC_ARRAY_SIZE, new StreamBitVectorBundle))
  val replacement = ReplacementPolicy.fromString("plru", BIT_VEC_ARRAY_SIZE)

  // s0: generate region tag, parallel match
  val s0_can_accept = Wire(Bool())
  val s0_valid = io.train_req.fire
  val s0_vaddr = io.train_req.bits.vaddr
  val s0_region_bits = get_region_bits(s0_vaddr)
  val s0_region_tag = get_region_tag(s0_vaddr)
  val s0_region_tag_plus_one = get_region_tag(s0_vaddr) + 1.U
  val s0_region_tag_minus_one = get_region_tag(s0_vaddr) - 1.U
  val s0_region_tag_match_vec = array.map(_.tag_match(s0_region_tag))
  val s0_region_tag_plus_one_match_vec = array.map(_.tag_match(s0_region_tag_plus_one))
  val s0_region_tag_minus_one_match_vec = array.map(_.tag_match(s0_region_tag_minus_one))
  val s0_hit = Cat(s0_region_tag_match_vec).orR
  val s0_plus_one_hit = Cat(s0_region_tag_plus_one_match_vec).orR
  val s0_minus_one_hit = Cat(s0_region_tag_minus_one_match_vec).orR
  val s0_hit_vec = VecInit(s0_region_tag_match_vec).asUInt
  val s0_index = Mux(s0_hit, OHToUInt(s0_hit_vec), replacement.way)
  val s0_plus_one_index = OHToUInt(VecInit(s0_region_tag_plus_one_match_vec).asUInt)
  val s0_minus_one_index = OHToUInt(VecInit(s0_region_tag_minus_one_match_vec).asUInt)
  io.train_req.ready := s0_can_accept

  when(s0_valid) {
    replacement.access(s0_index)
  }

  assert(!s0_valid || PopCount(VecInit(s0_region_tag_match_vec)) <= 1.U, "req region should match no more than 1 entry")
  assert(!s0_valid || PopCount(VecInit(s0_region_tag_plus_one_match_vec)) <= 1.U, "req region plus 1 should match no more than 1 entry")
  assert(!s0_valid || PopCount(VecInit(s0_region_tag_minus_one_match_vec)) <= 1.U, "req region minus 1 should match no more than 1 entry")
  assert(!s0_valid || !(s0_hit && s0_plus_one_hit && (s0_index === s0_plus_one_index)), "region and region plus 1 index match failed")
  assert(!s0_valid || !(s0_hit && s0_minus_one_hit && (s0_index === s0_minus_one_index)), "region and region minus 1 index match failed")
  assert(!s0_valid || !(s0_plus_one_hit && s0_minus_one_hit && (s0_minus_one_index === s0_plus_one_index)), "region plus 1 and region minus 1 index match failed")
  assert(!(s0_valid && RegNext(s0_valid) && !s0_hit && !RegNext(s0_hit) && replacement.way === RegNext(replacement.way)), "replacement error")

  XSPerfAccumulate("s0_valid_train_req", s0_valid)
  val s0_hit_pattern_vec = Seq(s0_hit, s0_plus_one_hit, s0_minus_one_hit)
  for(i <- 0 until (1 << s0_hit_pattern_vec.size)) {
    XSPerfAccumulate(s"s0_hit_pattern_${toBinary(i)}", (VecInit(s0_hit_pattern_vec).asUInt === i.U) && s0_valid)
  }
  XSPerfAccumulate("s0_replace_the_neighbor", s0_valid && !s0_hit && ((s0_plus_one_hit && (s0_index === s0_plus_one_index)) || (s0_minus_one_hit && (s0_index === s0_minus_one_index))))
  XSPerfAccumulate("s0_req_valid", io.train_req.valid)
  XSPerfAccumulate("s0_req_cannot_accept", io.train_req.valid && !io.train_req.ready)

  val ratio_const = WireInit(Constantin.createRecord("l2DepthRatio" + p(XSCoreParamsKey).HartId.toString, initValue = L2_DEPTH_RATIO.U))
  val ratio = ratio_const(3, 0)

  val l3_ratio_const = WireInit(Constantin.createRecord("l3DepthRatio" + p(XSCoreParamsKey).HartId.toString, initValue = L3_DEPTH_RATIO.U))
  val l3_ratio = l3_ratio_const(3, 0)

  // s1: alloc or update
  val s1_valid = RegNext(s0_valid)
  val s1_index = RegEnable(s0_index, s0_valid)
  val s1_plus_one_index = RegEnable(s0_plus_one_index, s0_valid)
  val s1_minus_one_index = RegEnable(s0_minus_one_index, s0_valid)
  val s1_hit = RegEnable(s0_hit, s0_valid)
  val s1_plus_one_hit = if(ENABLE_STRICT_ACTIVE_DETECTION)
                            RegEnable(s0_plus_one_hit, s0_valid) && array(s1_plus_one_index).active && (array(s1_plus_one_index).cnt >= ACTIVE_THRESHOLD.U)
                        else
                            RegEnable(s0_plus_one_hit, s0_valid) && array(s1_plus_one_index).active
  val s1_minus_one_hit = if(ENABLE_STRICT_ACTIVE_DETECTION)
                            RegEnable(s0_minus_one_hit, s0_valid) && array(s1_minus_one_index).active && (array(s1_minus_one_index).cnt >= ACTIVE_THRESHOLD.U)
                        else
                            RegEnable(s0_minus_one_hit, s0_valid) && array(s1_minus_one_index).active
  val s1_region_tag = RegEnable(s0_region_tag, s0_valid)
  val s1_region_bits = RegEnable(s0_region_bits, s0_valid)
  val s1_alloc = s1_valid && !s1_hit
  val s1_update = s1_valid && s1_hit
  val s1_pf_l1_incr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) + io.dynamic_depth, 0.U(BLOCK_OFFSET.W))
  val s1_pf_l1_decr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) - io.dynamic_depth, 0.U(BLOCK_OFFSET.W))
  val s1_pf_l2_incr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) + (io.dynamic_depth << ratio), 0.U(BLOCK_OFFSET.W))
  val s1_pf_l2_decr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) - (io.dynamic_depth << ratio), 0.U(BLOCK_OFFSET.W))
  val s1_pf_l3_incr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) + (io.dynamic_depth << l3_ratio), 0.U(BLOCK_OFFSET.W))
  val s1_pf_l3_decr_vaddr = Cat(region_to_block_addr(s1_region_tag, s1_region_bits) - (io.dynamic_depth << l3_ratio), 0.U(BLOCK_OFFSET.W))
  // TODO: remove this
  val s1_can_send_pf = Mux(s1_update, !((array(s1_index).bit_vec & UIntToOH(s1_region_bits)).orR), true.B)
  s0_can_accept := !(s1_valid && (region_hash_tag(s1_region_tag) === region_hash_tag(s0_region_tag)))

  when(s1_alloc) {
    // alloc a new entry
    array(s1_index).alloc(
      alloc_tag = s1_region_tag,
      alloc_bit_vec = UIntToOH(s1_region_bits),
      alloc_active = s1_plus_one_hit || s1_minus_one_hit,
      alloc_decr_mode = RegEnable(s0_plus_one_hit, s0_valid))

  }.elsewhen(s1_update) {
    // update a existing entry
    assert(array(s1_index).cnt =/= 0.U || array(s1_index).tag === s1_index, "entry should have been allocated before")
    array(s1_index).update(
      update_bit_vec = UIntToOH(s1_region_bits),
      update_active = s1_plus_one_hit || s1_minus_one_hit)
  }

  XSPerfAccumulate("s1_alloc", s1_alloc)
  XSPerfAccumulate("s1_update", s1_update)
  XSPerfAccumulate("s1_active_plus_one_hit", s1_valid && s1_plus_one_hit)
  XSPerfAccumulate("s1_active_minus_one_hit", s1_valid && s1_minus_one_hit)

  // s2: trigger prefetch if hit active bit vector, compute meta of prefetch req
  val s2_valid = RegNext(s1_valid)
  val s2_index = RegEnable(s1_index, s1_valid)
  val s2_region_bits = RegEnable(s1_region_bits, s1_valid)
  val s2_region_tag = RegEnable(s1_region_tag, s1_valid)
  val s2_pf_l1_incr_vaddr = RegEnable(s1_pf_l1_incr_vaddr, s1_valid)
  val s2_pf_l1_decr_vaddr = RegEnable(s1_pf_l1_decr_vaddr, s1_valid)
  val s2_pf_l2_incr_vaddr = RegEnable(s1_pf_l2_incr_vaddr, s1_valid)
  val s2_pf_l2_decr_vaddr = RegEnable(s1_pf_l2_decr_vaddr, s1_valid)
  val s2_pf_l3_incr_vaddr = RegEnable(s1_pf_l3_incr_vaddr, s1_valid)
  val s2_pf_l3_decr_vaddr = RegEnable(s1_pf_l3_decr_vaddr, s1_valid)
  val s2_can_send_pf = RegEnable(s1_can_send_pf, s1_valid)
  val s2_active = array(s2_index).active
  val s2_decr_mode = array(s2_index).decr_mode
  val s2_l1_vaddr = Mux(s2_decr_mode, s2_pf_l1_decr_vaddr, s2_pf_l1_incr_vaddr)
  val s2_l2_vaddr = Mux(s2_decr_mode, s2_pf_l2_decr_vaddr, s2_pf_l2_incr_vaddr)
  val s2_l3_vaddr = Mux(s2_decr_mode, s2_pf_l3_decr_vaddr, s2_pf_l3_incr_vaddr)
  val s2_will_send_pf = s2_valid && s2_active && s2_can_send_pf
  val s2_pf_req_valid = s2_will_send_pf && io.enable
  val s2_pf_l1_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
    vaddr = s2_l1_vaddr,
    width = WIDTH_CACHE_BLOCKS,
    decr_mode = s2_decr_mode,
    sink = SINK_L1,
    source = L1_HW_PREFETCH_STREAM)
  val s2_pf_l2_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
    vaddr = s2_l2_vaddr,
    width = L2_WIDTH_CACHE_BLOCKS,
    decr_mode = s2_decr_mode,
    sink = SINK_L2,
    source = L1_HW_PREFETCH_STREAM)
  val s2_pf_l3_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
    vaddr = s2_l3_vaddr,
    width = L3_WIDTH_CACHE_BLOCKS,
    decr_mode = s2_decr_mode,
    sink = SINK_L3,
    source = L1_HW_PREFETCH_STREAM)

  XSPerfAccumulate("s2_valid", s2_valid)
  XSPerfAccumulate("s2_will_not_send_pf", s2_valid && !s2_will_send_pf)
  XSPerfAccumulate("s2_will_send_decr_pf", s2_valid && s2_will_send_pf && s2_decr_mode)
  XSPerfAccumulate("s2_will_send_incr_pf", s2_valid && s2_will_send_pf && !s2_decr_mode)

  // s3: send the l1 prefetch req out
  val s3_pf_l1_valid = RegNext(s2_pf_req_valid)
  val s3_pf_l1_bits = RegEnable(s2_pf_l1_req_bits, s2_pf_req_valid)
  val s3_pf_l2_valid = RegNext(s2_pf_req_valid)
  val s3_pf_l2_bits = RegEnable(s2_pf_l2_req_bits, s2_pf_req_valid)
  val s3_pf_l3_bits = RegEnable(s2_pf_l3_req_bits, s2_pf_req_valid)

  XSPerfAccumulate("s3_pf_sent", s3_pf_l1_valid)

  // s4: send the l2 prefetch req out
  val s4_pf_l2_valid = RegNext(s3_pf_l2_valid)
  val s4_pf_l2_bits = RegEnable(s3_pf_l2_bits, s3_pf_l2_valid)
  val s4_pf_l3_bits = RegEnable(s3_pf_l3_bits, s3_pf_l2_valid)

  val enable_l3_pf = WireInit(Constantin.createRecord("enableL3StreamPrefetch" + p(XSCoreParamsKey).HartId.toString, initValue = 0.U)) =/= 0.U
  // s5: send the l3 prefetch req out
  val s5_pf_l3_valid = RegNext(s4_pf_l2_valid) && enable_l3_pf
  val s5_pf_l3_bits = RegEnable(s4_pf_l3_bits, s4_pf_l2_valid)

  io.prefetch_req.valid := s3_pf_l1_valid || s4_pf_l2_valid || s5_pf_l3_valid
  io.prefetch_req.bits := Mux(s3_pf_l1_valid, s3_pf_l1_bits, Mux(s4_pf_l2_valid, s4_pf_l2_bits, s5_pf_l3_bits))

  XSPerfAccumulate("s4_pf_sent", !s3_pf_l1_valid && s4_pf_l2_valid)
  XSPerfAccumulate("s4_pf_blocked", s3_pf_l1_valid && s4_pf_l2_valid)
  XSPerfAccumulate("pf_sent", io.prefetch_req.valid)

  // Stride lookup starts here
  // S0: Stride send req
  val s0_lookup_valid = io.stream_lookup_req.valid
  val s0_lookup_vaddr = io.stream_lookup_req.bits.vaddr
  val s0_lookup_tag = get_region_tag(s0_lookup_vaddr)
  // S1: match
  val s1_lookup_valid = RegNext(s0_lookup_valid)
  val s1_lookup_tag = RegEnable(s0_lookup_tag, s0_lookup_valid)
  val s1_lookup_tag_match_vec = array.map(_.tag_match(s1_lookup_tag))
  val s1_lookup_hit = VecInit(s1_lookup_tag_match_vec).asUInt.orR
  val s1_lookup_index = OHToUInt(VecInit(s1_lookup_tag_match_vec))
  // S2: read active out
  val s2_lookup_valid = RegNext(s1_lookup_valid)
  val s2_lookup_hit = RegEnable(s1_lookup_hit, s1_lookup_valid)
  val s2_lookup_index = RegEnable(s1_lookup_index, s1_lookup_valid)
  val s2_lookup_active = array(s2_lookup_index).active
  // S3: send back to Stride
  val s3_lookup_valid = RegNext(s2_lookup_valid)
  val s3_lookup_hit = RegEnable(s2_lookup_hit, s2_lookup_valid)
  val s3_lookup_active = RegEnable(s2_lookup_active, s2_lookup_valid)
  io.stream_lookup_resp := s3_lookup_valid && s3_lookup_hit && s3_lookup_active

  // reset meta to avoid muti-hit problem
  for(i <- 0 until BIT_VEC_ARRAY_SIZE) {
    when(reset.asBool || RegNext(io.flush)) {
      array(i).reset(i)
    }
  }

  XSPerfHistogram("bit_vector_active", PopCount(VecInit(array.map(_.active)).asUInt), true.B, 0, BIT_VEC_ARRAY_SIZE, 1)
  XSPerfHistogram("bit_vector_decr_mode", PopCount(VecInit(array.map(_.decr_mode)).asUInt), true.B, 0, BIT_VEC_ARRAY_SIZE, 1)
  XSPerfAccumulate("hash_conflict", s0_valid && s2_valid && (s0_region_tag =/= s2_region_tag) && (region_hash_tag(s0_region_tag) === region_hash_tag(s2_region_tag)))
}