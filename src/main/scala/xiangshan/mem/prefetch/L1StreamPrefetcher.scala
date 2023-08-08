package xiangshan.mem.prefetch

import chipsalliance.rocketchip.config.Parameters
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

trait HasStreamPrefetchHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
  // region related
  val REGION_SIZE = 1024
  val PAGE_OFFSET = 12
  val BLOCK_OFFSET = log2Up(dcacheParameters.blockBytes)
  val BIT_VEC_WITDH = REGION_SIZE / dcacheParameters.blockBytes
  val REGION_BITS = log2Up(BIT_VEC_WITDH)
  val REGION_TAG_OFFSET = BLOCK_OFFSET + REGION_BITS
  val REGION_TAG_BITS = VAddrBits - BLOCK_OFFSET - REGION_BITS

  // hash related
  val VADDR_HASH_WIDTH = 5
  val BLK_ADDR_RAW_WIDTH = 10
  val HASH_TAG_WIDTH = VADDR_HASH_WIDTH + BLK_ADDR_RAW_WIDTH

  // capacity related
  val TRAIN_FILTER_SIZE = 4
  val STREAM_FILTER_SIZE = 16
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

  val DEPTH_LOOKAHEAD = 6
  val DEPTH_BITS = log2Up(DEPTH_CACHE_BLOCKS) + DEPTH_LOOKAHEAD

  val ENABLE_DECR_MODE = false
  val ENABLE_STRICT_ACTIVE_DETECTION = true

  // prefetch sink related
  val SINK_BITS = 2
  def SINK_L1 = "b00".U
  def SINK_L2 = "b01".U
  def SINK_L3 = "b10".U

  // vaddr: |       region tag        |  region bits  | block offset |
  def get_region_tag(vaddr: UInt) = {
    require(vaddr.getWidth == VAddrBits)
    vaddr(vaddr.getWidth - 1, REGION_TAG_OFFSET)
  }

  def get_region_bits(vaddr: UInt) = {
    require(vaddr.getWidth == VAddrBits)
    vaddr(REGION_TAG_OFFSET - 1, BLOCK_OFFSET)
  }

  def block_addr(x: UInt): UInt = {
    x(x.getWidth - 1, BLOCK_OFFSET)
  }

  def vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }

  def pc_hash_tag(x: UInt): UInt = {
    val low = x(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = x(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  def block_hash_tag(x: UInt): UInt = {
    val blk_addr = block_addr(x)
    val low = blk_addr(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = blk_addr(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_hash_tag(region_tag: UInt): UInt = {
    val low = region_tag(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = region_tag(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_to_block_addr(region_tag: UInt, region_bits: UInt): UInt = {
    Cat(region_tag, region_bits)
  }

  def get_candidate_oh(x: UInt): UInt = {
    require(x.getWidth == PAddrBits)
    UIntToOH(x(REGION_BITS + BLOCK_OFFSET - 1, BLOCK_OFFSET))
  }

  def toBinary(n: Int): String = n match {
    case 0|1 => s"$n"
    case _   => s"${toBinary(n/2)}${n%2}"
  }

  // constraints
  require((DEPTH_BYTES >= REGION_SIZE) && ((DEPTH_BYTES % REGION_SIZE) == 0) && ((DEPTH_BYTES / REGION_SIZE) > 0))
  require(((VADDR_HASH_WIDTH * 3) + BLK_ADDR_RAW_WIDTH) <= REGION_TAG_BITS)
  require(WIDTH_BYTES >= dcacheParameters.blockBytes)
}

// get prefetch train reqs from `exuParameters.LduCnt` load pipelines (up to `exuParameters.LduCnt`/cycle)
// filter by cache line address, send out train req to stream (up to 1 req/cycle)
class StreamTrainFilter()(implicit p: Parameters) extends XSModule with HasStreamPrefetchHelper{
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val flush = Input(Bool())
    // train input, only from load for now
    val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
    // filter out
    val train_req = DecoupledIO(new PrefetchReqBundle())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => TRAIN_FILTER_SIZE ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(TRAIN_FILTER_SIZE){ (0.U.asTypeOf(new PrefetchReqBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(TRAIN_FILTER_SIZE){ (false.B) }))

  // enq
  val enqLen = exuParameters.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))
  
  val deqPtr = WireInit(deqPtrExt.value)

  require(TRAIN_FILTER_SIZE >= enqLen)

  val reqs_l = io.ld_in.map(_.bits.asPrefetchReqBundle())
  val reqs_vl = io.ld_in.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  for(i <- (0 until enqLen)) {
    val req = reqs_l(i)
    val req_v = reqs_vl(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)
    val entry_match = Cat(entries.zip(valids).map {
      case(e, v) => v && block_hash_tag(e.vaddr) === block_hash_tag(req.vaddr)
    }).orR
    val prev_enq_match = if(i == 0) false.B else Cat(reqs_l.zip(reqs_vl).take(i).map {
      case(pre, pre_v) => pre_v && block_hash_tag(pre.vaddr) === block_hash_tag(req.vaddr)
    }).orR

    needAlloc(i) := req_v && !entry_match && !prev_enq_match
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt && io.enable

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => x := x + allocNum}

  // deq
  io.train_req.valid := false.B
  io.train_req.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) => {
      when(deqPtr === i.U) {
        io.train_req.valid := valid && io.enable
        io.train_req.bits := entry
      }
    }
  }

  when(io.train_req.fire) {
    valids(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }

  when(RegNext(io.flush)) {
    valids.foreach {case valid => valid := false.B}
    (0 until enqLen).map {case i => enqPtrExt(i) := i.U.asTypeOf(new Ptr)}
    deqPtrExt := 0.U.asTypeOf(new Ptr)
  }

  XSPerfAccumulate("stream_train_filter_full", PopCount(valids) === TRAIN_FILTER_SIZE.U)
  XSPerfAccumulate("stream_train_filter_half", PopCount(valids) >= (TRAIN_FILTER_SIZE / 2).U)
  XSPerfAccumulate("stream_train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqs_vl)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate("stream_train_filter_enq", allocNum > 0.U)
  XSPerfAccumulate("stream_train_filter_deq", io.train_req.fire)
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"stream_train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"stream_train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"stream_train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
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
  val s2_can_send_pf = RegEnable(s1_can_send_pf, s1_valid)
  val s2_active = array(s2_index).active
  val s2_decr_mode = array(s2_index).decr_mode
  val s2_l1_vaddr = Mux(s2_decr_mode, s2_pf_l1_decr_vaddr, s2_pf_l1_incr_vaddr)
  val s2_l2_vaddr = Mux(s2_decr_mode, s2_pf_l2_decr_vaddr, s2_pf_l2_incr_vaddr)
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
  
  XSPerfAccumulate("s2_valid", s2_valid)
  XSPerfAccumulate("s2_will_not_send_pf", s2_valid && !s2_will_send_pf)
  XSPerfAccumulate("s2_will_send_decr_pf", s2_valid && s2_will_send_pf && s2_decr_mode)
  XSPerfAccumulate("s2_will_send_incr_pf", s2_valid && s2_will_send_pf && !s2_decr_mode)

  // s3: send the l1 prefetch req out
  val s3_pf_l1_valid = RegNext(s2_pf_req_valid)
  val s3_pf_l1_bits = RegEnable(s2_pf_l1_req_bits, s2_pf_req_valid)
  val s3_pf_l2_valid = RegNext(s2_pf_req_valid)
  val s3_pf_l2_bits = RegEnable(s2_pf_l2_req_bits, s2_pf_req_valid)

  XSPerfAccumulate("s3_pf_sent", s3_pf_l1_valid)

  // s4: send the l2 prefetch req out
  val s4_pf_l2_valid = RegNext(s3_pf_l2_valid)
  val s4_pf_l2_bits = RegEnable(s3_pf_l2_bits, s3_pf_l2_valid)

  io.prefetch_req.valid := s3_pf_l1_valid || s4_pf_l2_valid
  io.prefetch_req.bits := Mux(s3_pf_l1_valid, s3_pf_l1_bits, s4_pf_l2_bits)

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

class StreamFilterBundle(implicit p: Parameters) extends XSBundle with HasStreamPrefetchHelper {
  val tag = UInt(HASH_TAG_WIDTH.W)
  val region = UInt(REGION_TAG_BITS.W)
  val bit_vec = UInt(BIT_VEC_WITDH.W)
  // NOTE: l1 will not use sent_vec, for making more prefetch reqs to l1 dcache
  val sent_vec = UInt(BIT_VEC_WITDH.W)
  val sink = UInt(SINK_BITS.W)
  val alias = UInt(2.W)
  val is_vaddr = Bool()
  val source = new L1PrefetchSource()

  def reset(index: Int) = {
    tag := region_hash_tag(index.U)
    region := index.U
    bit_vec := 0.U
    sent_vec := 0.U
    sink := SINK_L1
    alias := 0.U
    is_vaddr := false.B
    source.value := L1_HW_PREFETCH_NULL
  }

  def tag_match(new_tag: UInt): Bool = {
    require(new_tag.getWidth == HASH_TAG_WIDTH)
    tag === new_tag
  }

  def update(update_bit_vec: UInt, update_sink: UInt) = {
    bit_vec := bit_vec | update_bit_vec
    when(update_sink < sink) {
      bit_vec := (bit_vec & ~sent_vec) | update_bit_vec
      sink := update_sink
    }

    assert(PopCount(update_bit_vec) >= 1.U, "valid bits in update vector should greater than one")
  }

  def can_send_pf(): Bool = {
    Mux(
      sink === SINK_L1,
      !is_vaddr && bit_vec.orR,
      !is_vaddr && (bit_vec & ~sent_vec).orR
    )
  }

  def get_pf_addr(): UInt = {
    require(PAddrBits <= VAddrBits)
    require((region.getWidth + REGION_BITS + BLOCK_OFFSET) == VAddrBits)

    val candidate = Mux(
      sink === SINK_L1,
      PriorityEncoder(bit_vec).asTypeOf(UInt(REGION_BITS.W)),
      PriorityEncoder(bit_vec & ~sent_vec).asTypeOf(UInt(REGION_BITS.W))
    )
    Cat(region, candidate, 0.U(BLOCK_OFFSET.W))
  }

  def get_tlb_va(): UInt = {
    require((region.getWidth + REGION_TAG_OFFSET) == VAddrBits)
    Cat(region, 0.U(REGION_TAG_OFFSET.W))
  }

  def fromStreamPrefetchReqBundle(x : StreamPrefetchReqBundle): StreamFilterBundle = {
    require(PAGE_OFFSET >= REGION_TAG_OFFSET, "region is greater than 4k, alias bit may be incorrect")

    val res = Wire(new StreamFilterBundle)
    res.tag := region_hash_tag(x.region)
    res.region := x.region
    res.bit_vec := x.bit_vec
    res.sent_vec := 0.U
    res.sink := x.sink
    res.is_vaddr := true.B
    res.source := x.source
    res.alias := x.region(PAGE_OFFSET - REGION_TAG_OFFSET + 1, PAGE_OFFSET - REGION_TAG_OFFSET)

    res
  }
  
  def invalidate() = {
    // disable sending pf req
    when(sink === SINK_L1) {
      bit_vec := 0.U(BIT_VEC_WITDH.W)
    }.otherwise {
      sent_vec := ~(0.U(BIT_VEC_WITDH.W))
    }
    // disable sending tlb req
    is_vaddr := false.B
  }
}

// there are 4 independent pipelines inside
// 1. prefetch enqueue
// 2. tlb request
// 3. actual l1 prefetch
// 4. actual l2 prefetch
class L1L2prefetchFilter(implicit p: Parameters) extends XSModule with HasStreamPrefetchHelper {
  val io = IO(new XSBundle {
    val enable = Input(Bool())
    val flush = Input(Bool())
    val prefetch_req = Flipped(ValidIO(new StreamPrefetchReqBundle))
    val tlb_req = new TlbRequestIO(nRespDups = 2)
    val l1_req = DecoupledIO(new L1PrefetchReq())
    val pf_addr = ValidIO(UInt(PAddrBits.W))
    val confidence = Input(UInt(1.W))
    val l2PfqBusy = Input(Bool())
  })

  val array = Reg(Vec(STREAM_FILTER_SIZE, new StreamFilterBundle))
  val replacement = ReplacementPolicy.fromString("plru", STREAM_FILTER_SIZE)
  val tlb_req_arb = Module(new RRArbiterInit(new TlbReq, STREAM_FILTER_SIZE))
  val l1_pf_req_arb = Module(new RRArbiterInit(new L1PrefetchReq, STREAM_FILTER_SIZE))
  val l2_pf_req_arb = Module(new RRArbiterInit(UInt(PAddrBits.W), STREAM_FILTER_SIZE))

  // enq
  // s0: hash tag match
  val s0_can_accept = Wire(Bool())
  val s0_valid = io.prefetch_req.valid && s0_can_accept
  val s0_region = io.prefetch_req.bits.region
  val s0_region_hash = region_hash_tag(s0_region)
  val s0_match_vec = array.map(_.tag_match(s0_region_hash))
  val s0_hit = VecInit(s0_match_vec).asUInt.orR
  val s0_index = Mux(s0_hit, OHToUInt(VecInit(s0_match_vec).asUInt), replacement.way)
  val s0_prefetch_req = (new StreamFilterBundle).fromStreamPrefetchReqBundle(io.prefetch_req.bits)

  when(s0_valid) {
    replacement.access(s0_index)
  }

  assert(!s0_valid || PopCount(VecInit(s0_match_vec)) <= 1.U, "req region should match no more than 1 entry")
  assert(!(s0_valid && RegNext(s0_valid) && !s0_hit && !RegNext(s0_hit) && replacement.way === RegNext(replacement.way)), "replacement error")

  XSPerfAccumulate("s0_enq_fire", s0_valid)
  XSPerfAccumulate("s0_enq_valid", io.prefetch_req.valid)
  XSPerfAccumulate("s0_cannot_enq", io.prefetch_req.valid && !s0_can_accept)

  // s1: alloc or update
  val s1_valid = RegNext(s0_valid)
  val s1_region = RegEnable(s0_region, s0_valid)
  val s1_region_hash = RegEnable(s0_region_hash, s0_valid)
  val s1_hit = RegEnable(s0_hit, s0_valid)
  val s1_index = RegEnable(s0_index, s0_valid)
  val s1_prefetch_req = RegEnable(s0_prefetch_req, s0_valid)
  val s1_alloc = s1_valid && !s1_hit
  val s1_update = s1_valid && s1_hit
  s0_can_accept := !(s1_valid && s1_alloc && (s0_region_hash === s1_region_hash))

  when(s1_alloc) {
    array(s1_index) := s1_prefetch_req
  }.elsewhen(s1_update) {
    array(s1_index).update(
      update_bit_vec = s1_prefetch_req.bit_vec,
      update_sink = s1_prefetch_req.sink
    )
  }

  // TODO: set this constraint looser to enable more kinds of depth
  // assert(!(s0_valid && s1_valid && s0_region === s1_region), "s0 and s1 must have different region")

  XSPerfAccumulate("s1_enq_valid", s1_valid)
  XSPerfAccumulate("s1_enq_alloc", s1_alloc)
  XSPerfAccumulate("s1_enq_update", s1_update)
  XSPerfAccumulate("hash_conflict", s0_valid && RegNext(s1_valid) && (s0_region =/= RegNext(s1_region)) && (s0_region_hash === RegNext(s1_region_hash)))

  // tlb req
  // s0: arb all tlb reqs
  val s0_tlb_fire_vec = VecInit((0 until STREAM_FILTER_SIZE).map{case i => tlb_req_arb.io.in(i).fire})
  val s1_tlb_fire_vec = RegNext(s0_tlb_fire_vec)
  val s2_tlb_fire_vec = RegNext(s1_tlb_fire_vec)

  for(i <- 0 until STREAM_FILTER_SIZE) {
    val evict = s1_alloc && (s1_index === i.U)
    tlb_req_arb.io.in(i).valid := array(i).is_vaddr && !s1_tlb_fire_vec(i) && !s2_tlb_fire_vec(i) && !evict
    tlb_req_arb.io.in(i).bits.vaddr := array(i).get_tlb_va()
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B
    tlb_req_arb.io.in(i).bits.memidx := DontCare
    tlb_req_arb.io.in(i).bits.debug := DontCare
  }

  assert(PopCount(s0_tlb_fire_vec) <= 1.U, "s0_tlb_fire_vec should be one-hot or empty")

  // s1: send out the req
  val s1_tlb_req_valid = RegNext(tlb_req_arb.io.out.valid)
  val s1_tlb_req_bits = RegEnable(tlb_req_arb.io.out.bits, tlb_req_arb.io.out.valid)
  val s1_tlb_req_index = RegEnable(OHToUInt(s0_tlb_fire_vec.asUInt), tlb_req_arb.io.out.valid)
  val s1_tlb_evict = s1_alloc && (s1_index === s1_tlb_req_index)
  io.tlb_req.req.valid := s1_tlb_req_valid && !s1_tlb_evict
  io.tlb_req.req.bits := s1_tlb_req_bits
  io.tlb_req.req_kill := false.B
  tlb_req_arb.io.out.ready := true.B

  XSPerfAccumulate("s1_tlb_req_sent", io.tlb_req.req.valid)
  XSPerfAccumulate("s1_tlb_req_evict", s1_tlb_req_valid && s1_tlb_evict)

  // s2: get response from tlb
  val s2_tlb_resp = io.tlb_req.resp
  val s2_tlb_update_index = RegEnable(s1_tlb_req_index, s1_tlb_req_valid)
  val s2_tlb_evict = s1_alloc && (s1_index === s2_tlb_update_index)
  when(s2_tlb_resp.valid && !s2_tlb_evict) {
    array(s2_tlb_update_index).is_vaddr := s2_tlb_resp.bits.miss

    when(!s2_tlb_resp.bits.miss) {
      array(s2_tlb_update_index).region := Cat(0.U((VAddrBits - PAddrBits).W), s2_tlb_resp.bits.paddr.head(s2_tlb_resp.bits.paddr.head.getWidth - 1, REGION_TAG_OFFSET))
      when(s2_tlb_resp.bits.excp.head.pf.ld || s2_tlb_resp.bits.excp.head.af.ld) {
        array(s2_tlb_update_index).invalidate()
      }
    }
  }
  s2_tlb_resp.ready := true.B

  XSPerfAccumulate("s2_tlb_resp_valid", s2_tlb_resp.valid)
  XSPerfAccumulate("s2_tlb_resp_evict", s2_tlb_resp.valid && s2_tlb_evict)
  XSPerfAccumulate("s2_tlb_resp_miss", s2_tlb_resp.valid && !s2_tlb_evict && s2_tlb_resp.bits.miss)
  XSPerfAccumulate("s2_tlb_resp_updated", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss)
  XSPerfAccumulate("s2_tlb_resp_page_fault", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss && s2_tlb_resp.bits.excp.head.pf.ld)
  XSPerfAccumulate("s2_tlb_resp_access_fault", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss && s2_tlb_resp.bits.excp.head.af.ld)

  // l1 pf
  // s0: generate prefetch req paddr per entry, arb them
  val s0_pf_fire_vec = VecInit((0 until STREAM_FILTER_SIZE).map{case i => l1_pf_req_arb.io.in(i).fire})
  val s1_pf_fire_vec = RegNext(s0_pf_fire_vec)

  val s0_pf_fire = l1_pf_req_arb.io.out.fire
  val s0_pf_index = OHToUInt(s0_pf_fire_vec.asUInt)
  val s0_pf_candidate_oh = get_candidate_oh(l1_pf_req_arb.io.out.bits.paddr)

  for(i <- 0 until STREAM_FILTER_SIZE) {
    val evict = s1_alloc && (s1_index === i.U)
    l1_pf_req_arb.io.in(i).valid := array(i).can_send_pf() && (array(i).sink === SINK_L1) && !evict
    l1_pf_req_arb.io.in(i).bits.paddr := array(i).get_pf_addr()
    l1_pf_req_arb.io.in(i).bits.alias := array(i).alias
    l1_pf_req_arb.io.in(i).bits.confidence := io.confidence
    l1_pf_req_arb.io.in(i).bits.is_store := false.B
    l1_pf_req_arb.io.in(i).bits.pf_source := array(i).source
  }

  when(s0_pf_fire) {
    array(s0_pf_index).sent_vec := array(s0_pf_index).sent_vec | s0_pf_candidate_oh
  }

  assert(PopCount(s0_pf_fire_vec) <= 1.U, "s0_pf_fire_vec should be one-hot or empty")
  
  // s1: send out to dcache
  val s1_pf_valid = Reg(Bool())
  val s1_pf_bits = RegEnable(l1_pf_req_arb.io.out.bits, l1_pf_req_arb.io.out.fire)
  val s1_pf_index = RegEnable(s0_pf_index, l1_pf_req_arb.io.out.fire)
  val s1_pf_candidate_oh = RegEnable(s0_pf_candidate_oh, l1_pf_req_arb.io.out.fire)
  val s1_pf_evict = s1_alloc && (s1_index === s1_pf_index)
  val s1_pf_update = s1_update && (s1_index === s1_pf_index)
  val s1_pf_can_go = io.l1_req.ready && !s1_pf_evict && !s1_pf_update
  val s1_pf_fire = s1_pf_valid && s1_pf_can_go

  when(s1_pf_can_go) {
    s1_pf_valid := false.B
  }

  when(l1_pf_req_arb.io.out.fire) {
    s1_pf_valid := true.B
  }

  when(s1_pf_fire) {
    array(s1_pf_index).bit_vec := array(s1_pf_index).bit_vec & ~s1_pf_candidate_oh
  }

  // FIXME: the logic is to long, add an extra pf pipe stage
  io.l1_req.valid := s1_pf_valid && !s1_pf_evict && !s1_pf_update && (s1_pf_bits.paddr >= 0x80000000L.U) && io.enable
  io.l1_req.bits := s1_pf_bits

  l1_pf_req_arb.io.out.ready := s1_pf_can_go || !s1_pf_valid

  assert(!((s1_alloc || s1_update) && s1_pf_fire && (s1_index === s1_pf_index)), "pf pipeline & enq pipeline bit_vec harzard!")

  XSPerfAccumulate("s1_pf_valid", s1_pf_valid)
  XSPerfAccumulate("s1_pf_block_by_pipe_unready", s1_pf_valid && !io.l1_req.ready)
  XSPerfAccumulate("s1_pf_block_by_enq_alloc_harzard", s1_pf_valid && s1_pf_evict)
  XSPerfAccumulate("s1_pf_block_by_enq_update_harzard", s1_pf_valid && s1_pf_update)
  XSPerfAccumulate("s1_pf_fire", s1_pf_fire)

  // l2 pf
  // s0: generate prefetch req paddr per entry, arb them, sent out
  io.pf_addr.valid := l2_pf_req_arb.io.out.valid
  io.pf_addr.bits := l2_pf_req_arb.io.out.bits

  l2_pf_req_arb.io.out.ready := true.B

  for(i <- 0 until STREAM_FILTER_SIZE) {
    val evict = s1_alloc && (s1_index === i.U)
    l2_pf_req_arb.io.in(i).valid := array(i).can_send_pf() && (array(i).sink === SINK_L2) && !evict
    l2_pf_req_arb.io.in(i).bits := array(i).get_pf_addr()
  }

  when(l2_pf_req_arb.io.out.valid) {
    array(l2_pf_req_arb.io.chosen).sent_vec := array(l2_pf_req_arb.io.chosen).sent_vec | get_candidate_oh(l2_pf_req_arb.io.out.bits)
  }

  // reset meta to avoid muti-hit problem
  for(i <- 0 until STREAM_FILTER_SIZE) {
    when(reset.asBool || RegNext(io.flush)) {
      array(i).reset(i)
    }
  }

  XSPerfAccumulate("l2_prefetche_queue_busby", io.l2PfqBusy)
  XSPerfHistogram("filter_active", PopCount(VecInit(array.map(_.can_send_pf())).asUInt), true.B, 0, STREAM_FILTER_SIZE, 1)
  XSPerfHistogram("l1_filter_active", PopCount(VecInit(array.map(x => x.can_send_pf() && (x.sink === SINK_L1))).asUInt), true.B, 0, STREAM_FILTER_SIZE, 1)
  XSPerfHistogram("l2_filter_active", PopCount(VecInit(array.map(x => x.can_send_pf() && (x.sink === SINK_L2))).asUInt), true.B, 0, STREAM_FILTER_SIZE, 1)
}

class L1Prefetcher(implicit p: Parameters) extends BasePrefecher with HasStreamPrefetchHelper {
  val pf_ctrl = IO(Input(new PrefetchControlBundle))
  val stride_train = IO(Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle()))))
  val l2PfqBusy = IO(Input(Bool()))

  val stride_train_filter = Module(new StrideTrainFilter)
  val stride_meta_array = Module(new StrideMetaArray)
  val stream_train_filter = Module(new StreamTrainFilter)
  val stream_bit_vec_array = Module(new StreamBitVectorArray)
  val pf_queue_filter = Module(new L1L2prefetchFilter)

  // for now, if the stream is disabled, train and prefetch process will continue, without sending out and reqs
  val enable = io.enable
  val flush = pf_ctrl.flush

  stream_train_filter.io.ld_in.zipWithIndex.foreach {
    case (ld_in, i) => {
      ld_in.valid := io.ld_in(i).valid && enable
      ld_in.bits := io.ld_in(i).bits
    }
  }
  stream_train_filter.io.enable := enable
  stream_train_filter.io.flush := flush

  stride_train_filter.io.ld_in.zipWithIndex.foreach {
    case (ld_in, i) => {
      ld_in.valid := stride_train(i).valid && enable
      ld_in.bits := stride_train(i).bits
    }
  }
  stride_train_filter.io.enable := enable
  stride_train_filter.io.flush := flush

  stream_bit_vec_array.io.enable := enable
  stream_bit_vec_array.io.flush := flush
  stream_bit_vec_array.io.dynamic_depth := pf_ctrl.dynamic_depth
  stream_bit_vec_array.io.train_req <> stream_train_filter.io.train_req

  stride_meta_array.io.enable := enable
  stride_meta_array.io.flush := flush
  stride_meta_array.io.dynamic_depth := 0.U
  stride_meta_array.io.train_req <> stride_train_filter.io.train_req
  stride_meta_array.io.stream_lookup_req <> stream_bit_vec_array.io.stream_lookup_req
  stride_meta_array.io.stream_lookup_resp <> stream_bit_vec_array.io.stream_lookup_resp

  pf_queue_filter.io.prefetch_req.valid := stream_bit_vec_array.io.prefetch_req.valid || stride_meta_array.io.prefetch_req.valid
  pf_queue_filter.io.prefetch_req.bits := Mux(stream_bit_vec_array.io.prefetch_req.valid, stream_bit_vec_array.io.prefetch_req.bits, stride_meta_array.io.prefetch_req.bits)
  // io.l1_req <> pf_queue_filter.io.l1_req
  io.l1_req.valid := pf_queue_filter.io.l1_req.valid && enable && pf_ctrl.enable
  io.l1_req.bits := pf_queue_filter.io.l1_req.bits
  pf_queue_filter.io.l1_req.ready := Mux(pf_ctrl.enable, io.l1_req.ready, true.B)
  io.tlb_req <> pf_queue_filter.io.tlb_req
  pf_queue_filter.io.enable := enable
  pf_queue_filter.io.flush := flush
  pf_queue_filter.io.confidence := pf_ctrl.confidence
  pf_queue_filter.io.l2PfqBusy := l2PfqBusy

  io.pf_addr.valid := pf_queue_filter.io.pf_addr.valid && pf_queue_filter.io.pf_addr.bits > 0x80000000L.U && enable && pf_ctrl.enable
  io.pf_addr.bits := pf_queue_filter.io.pf_addr.bits
}