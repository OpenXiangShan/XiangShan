package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
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

trait HasL1PrefetchHelper extends HasCircularQueuePtrHelper with HasDCacheParameters {
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
  val MLP_SIZE = 32
  val MLP_L1_SIZE = 16
  val MLP_L2L3_SIZE = MLP_SIZE - MLP_L1_SIZE

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
}

trait HasTrainFilterHelper extends HasCircularQueuePtrHelper {
  def reorder[T <: LdPrefetchTrainBundle](source: Vec[ValidIO[T]]): Vec[ValidIO[T]] = {
    if(source.length == 1) {
      source
    }else if(source.length == 2) {
      val source_v = source.map(_.valid)
      val res = Wire(source.cloneType)
      // source 1 is older than source 0 (only when source0/1 are both valid)
      val source_1_older = Mux(Cat(source_v).andR,
        isBefore(source(1).bits.uop.robIdx, source(0).bits.uop.robIdx),
        false.B
      )
      when(source_1_older) {
        res(0) := source(1)
        res(1) := source(0)
      }.otherwise {
        res := source
      }

      res
    }else if(source.length == 3) {
      // TODO: generalize
      val res_0_1 = Reg(source.cloneType)
      val res_1_2 = Reg(source.cloneType)
      val res = Reg(source.cloneType)

      val tmp = reorder(VecInit(source.slice(0, 2)))
      res_0_1(0) := tmp(0)
      res_0_1(1) := tmp(1)
      res_0_1(2) := source(2)
      val tmp_1 = reorder(VecInit(res_0_1.slice(1, 3)))
      res_1_2(0) := res_0_1(0)
      res_1_2(1) := tmp_1(0)
      res_1_2(2) := tmp_1(1)
      val tmp_2 = reorder(VecInit(res_1_2.slice(0, 2)))
      res(0) := tmp_2(0)
      res(1) := tmp_2(1)
      res(2) := res_1_2(2)

      res
    }else {
      require(false, "for now, 4 or more sources are invalid")
      source
    }
  }
}

// get prefetch train reqs from `exuParameters.LduCnt` load pipelines (up to `exuParameters.LduCnt`/cycle)
// filter by cache line address, send out train req to stride (up to 1 req/cycle)
class TrainFilter(size: Int, name: String)(implicit p: Parameters) extends XSModule with HasL1PrefetchHelper with HasTrainFilterHelper {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val flush = Input(Bool())
    // train input, only from load for now
    val ld_in = Flipped(Vec(backendParams.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
    // filter out
    val train_req = DecoupledIO(new PrefetchReqBundle())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => size ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = Reg(Vec(size, new PrefetchReqBundle))
  val valids = RegInit(VecInit(Seq.fill(size){ (false.B) }))

  // enq
  val enqLen = backendParams.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))

  val deqPtr = WireInit(deqPtrExt.value)

  require(size >= enqLen)

  val ld_in_reordered = reorder(io.ld_in)
  val reqs_l = ld_in_reordered.map(_.bits.asPrefetchReqBundle())
  val reqs_vl = ld_in_reordered.map(_.valid)
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

  enqPtrExt.foreach{case x => when(canAlloc.asUInt.orR) {x := x + allocNum} }

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

  XSPerfAccumulate(s"${name}_train_filter_full", PopCount(valids) === size.U)
  XSPerfAccumulate(s"${name}_train_filter_half", PopCount(valids) >= (size / 2).U)
  XSPerfAccumulate(s"${name}_train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqs_vl)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate(s"${name}_train_filter_enq", allocNum > 0.U)
  XSPerfAccumulate(s"${name}_train_filter_deq", io.train_req.fire)
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"${name}_train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"${name}_train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"${name}_train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
}

class MLPReqFilterBundle(implicit p: Parameters) extends XSBundle with HasL1PrefetchHelper {
  val tag = UInt(HASH_TAG_WIDTH.W)
  val region = UInt(REGION_TAG_BITS.W)
  val bit_vec = UInt(BIT_VEC_WITDH.W)
  // NOTE: l1 will not use sent_vec, for making more prefetch reqs to l1 dcache
  val sent_vec = UInt(BIT_VEC_WITDH.W)
  val sink = UInt(SINK_BITS.W)
  val alias = UInt(2.W)
  val is_vaddr = Bool()
  val source = new L1PrefetchSource()
  val debug_va_region = UInt(REGION_TAG_BITS.W)

  def reset(index: Int) = {
    tag := region_hash_tag(index.U)
    region := index.U
    bit_vec := 0.U
    sent_vec := 0.U
    sink := SINK_L1
    alias := 0.U
    is_vaddr := false.B
    source.value := L1_HW_PREFETCH_NULL
    debug_va_region := 0.U
  }

  def tag_match(valid1: Bool, valid2: Bool, new_tag: UInt): Bool = {
    require(new_tag.getWidth == HASH_TAG_WIDTH)
    (tag === new_tag) && valid1 && valid2
  }

  def update(update_bit_vec: UInt, update_sink: UInt) = {
    bit_vec := bit_vec | update_bit_vec
    when(update_sink < sink) {
      bit_vec := (bit_vec & ~sent_vec) | update_bit_vec
      sink := update_sink
    }

    assert(PopCount(update_bit_vec) >= 1.U, "valid bits in update vector should greater than one")
  }

  def can_send_pf(valid: Bool): Bool = {
    Mux(
      sink === SINK_L1,
      !is_vaddr && bit_vec.orR,
      !is_vaddr && (bit_vec & ~sent_vec).orR
    ) && valid
  }

  def may_be_replace(valid: Bool): Bool = {
    // either invalid or has sent out all reqs out
    !valid || RegNext(PopCount(sent_vec) === BIT_VEC_WITDH.U)
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

  def get_pf_debug_vaddr(): UInt = {
    val candidate = Mux(
      sink === SINK_L1,
      PriorityEncoder(bit_vec).asTypeOf(UInt(REGION_BITS.W)),
      PriorityEncoder(bit_vec & ~sent_vec).asTypeOf(UInt(REGION_BITS.W))
    )
    Cat(debug_va_region, candidate, 0.U(BLOCK_OFFSET.W))
  }

  def get_tlb_va(): UInt = {
    require((region.getWidth + REGION_TAG_OFFSET) == VAddrBits)
    Cat(region, 0.U(REGION_TAG_OFFSET.W))
  }

  def fromStreamPrefetchReqBundle(x : StreamPrefetchReqBundle): MLPReqFilterBundle = {
    require(PAGE_OFFSET >= REGION_TAG_OFFSET, "region is greater than 4k, alias bit may be incorrect")

    val res = Wire(new MLPReqFilterBundle)
    res.tag := region_hash_tag(x.region)
    res.region := x.region
    res.bit_vec := x.bit_vec
    res.sent_vec := 0.U
    res.sink := x.sink
    res.is_vaddr := true.B
    res.source := x.source
    res.alias := x.region(PAGE_OFFSET - REGION_TAG_OFFSET + 1, PAGE_OFFSET - REGION_TAG_OFFSET)
    res.debug_va_region := x.region

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

// there are 5 independent pipelines inside
// 1. prefetch enqueue
// 2. tlb request
// 3. actual l1 prefetch
// 4. actual l2 prefetch
// 5. actual l3 prefetch
class MutiLevelPrefetchFilter(implicit p: Parameters) extends XSModule with HasL1PrefetchHelper {
  val io = IO(new XSBundle {
    val enable = Input(Bool())
    val flush = Input(Bool())
    val l1_prefetch_req = Flipped(ValidIO(new StreamPrefetchReqBundle))
    val l2_l3_prefetch_req = Flipped(ValidIO(new StreamPrefetchReqBundle))
    val tlb_req = new TlbRequestIO(nRespDups = 2)
    val l1_req = DecoupledIO(new L1PrefetchReq())
    val l2_pf_addr = ValidIO(new L2PrefetchReq())
    val l3_pf_addr = ValidIO(UInt(PAddrBits.W)) // TODO: l3 pf source
    val confidence = Input(UInt(1.W))
    val l2PfqBusy = Input(Bool())
  })

  val l1_array = Reg(Vec(MLP_L1_SIZE, new MLPReqFilterBundle))
  val l2_array = Reg(Vec(MLP_L2L3_SIZE, new MLPReqFilterBundle))
  val l1_valids = RegInit(VecInit(Seq.fill(MLP_L1_SIZE)(false.B)))
  val l2_valids = RegInit(VecInit(Seq.fill(MLP_L2L3_SIZE)(false.B)))

  def _invalid(e: MLPReqFilterBundle, v: Bool): Unit = {
    v := false.B
    e.invalidate()
  }

  def invalid_array(i: UInt, isL2: Boolean): Unit = {
    if (isL2) {
      _invalid(l2_array(i), l2_valids(i))
    } else {
      _invalid(l1_array(i), l1_valids(i))
    }
  }

  def _reset(e: MLPReqFilterBundle, v: Bool, idx: Int): Unit = {
    v := false.B
    //only need to reset control signals for firendly area
    // e.reset(idx)
  }


  def reset_array(i: Int, isL2: Boolean): Unit = {
    if(isL2){
      _reset(l2_array(i), l2_valids(i), i)
    }else{
      _reset(l1_array(i), l1_valids(i), i)
    }
  }

  val l1_replacement = new ValidPseudoLRU(MLP_L1_SIZE)
  val l2_replacement = new ValidPseudoLRU(MLP_L2L3_SIZE)
  val tlb_req_arb = Module(new RRArbiterInit(new TlbReq, MLP_SIZE))
  val l1_pf_req_arb = Module(new RRArbiterInit(new Bundle {
    val req = new L1PrefetchReq
    val debug_vaddr = UInt(VAddrBits.W)
  }, MLP_L1_SIZE))
  val l2_pf_req_arb = Module(new RRArbiterInit(new Bundle {
    val req = new L2PrefetchReq
    val debug_vaddr = UInt(VAddrBits.W)
  }, MLP_L2L3_SIZE))
  val l3_pf_req_arb = Module(new RRArbiterInit(UInt(PAddrBits.W), MLP_L2L3_SIZE))

  val l1_opt_replace_vec = VecInit(l1_array.zip(l1_valids).map{case (e, v) => e.may_be_replace(v)})
  val l2_opt_replace_vec = VecInit(l2_array.zip(l2_valids).map{case (e, v) => e.may_be_replace(v)})
  // if we have something to replace, then choose it, otherwise follow the plru manner
  val l1_real_replace_vec = Mux(Cat(l1_opt_replace_vec).orR, l1_opt_replace_vec, VecInit(Seq.fill(MLP_L1_SIZE)(true.B)))
  val l2_real_replace_vec = Mux(Cat(l2_opt_replace_vec).orR, l2_opt_replace_vec, VecInit(Seq.fill(MLP_L2L3_SIZE)(true.B)))

  // l1 pf req enq
  // s0: hash tag match
  val s0_l1_can_accept = Wire(Bool())
  val s0_l1_valid = io.l1_prefetch_req.valid && s0_l1_can_accept
  val s0_l1_region = io.l1_prefetch_req.bits.region
  val s0_l1_region_hash = region_hash_tag(s0_l1_region)
  val s0_l1_match_vec = l1_array.zip(l1_valids).map{ case (e, v) => e.tag_match(v, s0_l1_valid, s0_l1_region_hash)}
  val s0_l1_hit = VecInit(s0_l1_match_vec).asUInt.orR
  val s0_l1_index = Wire(UInt(log2Up(MLP_L1_SIZE).W))
  val s0_l1_prefetch_req = (new MLPReqFilterBundle).fromStreamPrefetchReqBundle(io.l1_prefetch_req.bits)

  s0_l1_index := Mux(s0_l1_hit, OHToUInt(VecInit(s0_l1_match_vec).asUInt), l1_replacement.way(l1_real_replace_vec.reverse)._2)

  when(s0_l1_valid) {
    l1_replacement.access(s0_l1_index)
  }

  assert(!s0_l1_valid || PopCount(VecInit(s0_l1_match_vec)) <= 1.U, "req region should match no more than 1 entry")

  XSPerfAccumulate("s0_l1_enq_fire", s0_l1_valid)
  XSPerfAccumulate("s0_l1_enq_valid", io.l1_prefetch_req.valid)
  XSPerfAccumulate("s0_l1_cannot_enq", io.l1_prefetch_req.valid && !s0_l1_can_accept)

  // s1: alloc or update
  val s1_l1_valid = RegNext(s0_l1_valid)
  val s1_l1_region = RegEnable(s0_l1_region, s0_l1_valid)
  val s1_l1_region_hash = RegEnable(s0_l1_region_hash, s0_l1_valid)
  val s1_l1_hit = RegEnable(s0_l1_hit, s0_l1_valid)
  val s1_l1_index = RegEnable(s0_l1_index, s0_l1_valid)
  val s1_l1_prefetch_req = RegEnable(s0_l1_prefetch_req, s0_l1_valid)
  val s1_l1_alloc = s1_l1_valid && !s1_l1_hit
  val s1_l1_update = s1_l1_valid && s1_l1_hit
  s0_l1_can_accept := !(s1_l1_valid && s1_l1_alloc && (s0_l1_region_hash === s1_l1_region_hash))

  when(s1_l1_alloc) {
    l1_valids(s1_l1_index) := true.B
    l1_array(s1_l1_index) := s1_l1_prefetch_req
  }.elsewhen(s1_l1_update) {
    l1_array(s1_l1_index).update(
      update_bit_vec = s1_l1_prefetch_req.bit_vec,
      update_sink = s1_l1_prefetch_req.sink
    )
  }

  XSPerfAccumulate("s1_l1_enq_valid", s1_l1_valid)
  XSPerfAccumulate("s1_l1_enq_alloc", s1_l1_alloc)
  XSPerfAccumulate("s1_l1_enq_update", s1_l1_update)
  XSPerfAccumulate("l1_hash_conflict", s0_l1_valid && RegNext(s1_l1_valid) && (s0_l1_region =/= RegNext(s1_l1_region)) && (s0_l1_region_hash === RegNext(s1_l1_region_hash)))
  XSPerfAccumulate("s1_l1_enq_evict_useful_entry", s1_l1_alloc && l1_array(s1_l1_index).can_send_pf(l1_valids(s1_l1_index)))

  // l2 l3 pf req enq
  // s0: hash tag match
  val s0_l2_can_accept = Wire(Bool())
  val s0_l2_valid = io.l2_l3_prefetch_req.valid && s0_l2_can_accept
  val s0_l2_region = io.l2_l3_prefetch_req.bits.region
  val s0_l2_region_hash = region_hash_tag(s0_l2_region)
  val s0_l2_match_vec = l2_array.zip(l2_valids).map{ case (e, v) => e.tag_match(v, s0_l2_valid, s0_l2_region_hash) }
  val s0_l2_hit = VecInit(s0_l2_match_vec).asUInt.orR
  val s0_l2_index = Wire(UInt(log2Up(MLP_L2L3_SIZE).W))
  val s0_l2_prefetch_req = (new MLPReqFilterBundle).fromStreamPrefetchReqBundle(io.l2_l3_prefetch_req.bits)

  s0_l2_index := Mux(s0_l2_hit, OHToUInt(VecInit(s0_l2_match_vec).asUInt), l2_replacement.way(l2_real_replace_vec.reverse)._2)

  when(s0_l2_valid) {
    l2_replacement.access(s0_l2_index)
  }

  assert(!s0_l2_valid || PopCount(VecInit(s0_l2_match_vec)) <= 1.U, "req region should match no more than 1 entry")

  XSPerfAccumulate("s0_l2_enq_fire", s0_l2_valid)
  XSPerfAccumulate("s0_l2_enq_valid", io.l2_l3_prefetch_req.valid)
  XSPerfAccumulate("s0_l2_cannot_enq", io.l2_l3_prefetch_req.valid && !s0_l2_can_accept)

  // s1: alloc or update
  val s1_l2_valid = RegNext(s0_l2_valid)
  val s1_l2_region = RegEnable(s0_l2_region, s0_l2_valid)
  val s1_l2_region_hash = RegEnable(s0_l2_region_hash, s0_l2_valid)
  val s1_l2_hit = RegEnable(s0_l2_hit, s0_l2_valid)
  val s1_l2_index = RegEnable(s0_l2_index, s0_l2_valid)
  val s1_l2_prefetch_req = RegEnable(s0_l2_prefetch_req, s0_l2_valid)
  val s1_l2_alloc = s1_l2_valid && !s1_l2_hit
  val s1_l2_update = s1_l2_valid && s1_l2_hit
  s0_l2_can_accept := !(s1_l2_valid && s1_l2_alloc && (s0_l2_region_hash === s1_l2_region_hash))

  when(s1_l2_alloc) {
    l2_valids(s1_l2_index) := true.B
    l2_array(s1_l2_index) := s1_l2_prefetch_req
  }.elsewhen(s1_l2_update) {
    l2_array(s1_l2_index).update(
      update_bit_vec = s1_l2_prefetch_req.bit_vec,
      update_sink = s1_l2_prefetch_req.sink
    )
  }

  XSPerfAccumulate("s1_l2_enq_valid", s1_l2_valid)
  XSPerfAccumulate("s1_l2_enq_alloc", s1_l2_alloc)
  XSPerfAccumulate("s1_l2_enq_update", s1_l2_update)
  XSPerfAccumulate("l2_hash_conflict", s0_l2_valid && RegNext(s1_l2_valid) && (s0_l2_region =/= RegNext(s1_l2_region)) && (s0_l2_region_hash === RegNext(s1_l2_region_hash)))
  XSPerfAccumulate("s1_l2_enq_evict_useful_entry", s1_l2_alloc && l2_array(s1_l2_index).can_send_pf(l2_valids(s1_l2_index)))

  // stream pf debug db here
  // Hit:
  // now seens only pending = (region_bits & ~filter_bits) are the peeding request
  // if a PfGen comes, new added request can be new_req = PfGen.region_bits & ~(pending)
  // Alloc:
  // new_req = PfGen.region_bits
  val stream_pf_trace_debug_table = ChiselDB.createTable("StreamPFTrace" + p(XSCoreParamsKey).HartId.toString, new StreamPFTraceInEntry, basicDB = false)
  for (i <- 0 until BIT_VEC_WITDH) {
    // l1 enq log
    val hit_entry = l1_array(s0_l1_index)
    val new_req = Mux(
      s0_l1_hit,
      io.l1_prefetch_req.bits.bit_vec & ~(hit_entry.bit_vec),
      io.l1_prefetch_req.bits.bit_vec
    )
    val log_enable = s0_l1_valid && new_req(i) && (io.l1_prefetch_req.bits.source.value === L1_HW_PREFETCH_STREAM)
    val log_data = Wire(new StreamPFTraceInEntry)

    log_data.TriggerPC := io.l1_prefetch_req.bits.trigger_pc
    log_data.TriggerVaddr := io.l1_prefetch_req.bits.trigger_va
    log_data.PFVaddr := Cat(s0_l1_region, i.U(REGION_BITS.W), 0.U(log2Up(dcacheParameters.blockBytes).W))
    log_data.PFSink := s0_l1_prefetch_req.sink

    stream_pf_trace_debug_table.log(
      data = log_data,
      en = log_enable,
      site = "StreamPFTrace",
      clock = clock,
      reset = reset
    )
  }
  for (i <- 0 until BIT_VEC_WITDH) {
    // l2 l3 enq log
    val hit_entry = l2_array(s0_l2_index)
    val new_req = Mux(
      s0_l2_hit,
      io.l2_l3_prefetch_req.bits.bit_vec & ~(hit_entry.bit_vec),
      io.l2_l3_prefetch_req.bits.bit_vec
    )
    val log_enable = s0_l2_valid && new_req(i) && (io.l2_l3_prefetch_req.bits.source.value === L1_HW_PREFETCH_STREAM)
    val log_data = Wire(new StreamPFTraceInEntry)

    log_data.TriggerPC := io.l2_l3_prefetch_req.bits.trigger_pc
    log_data.TriggerVaddr := io.l2_l3_prefetch_req.bits.trigger_va
    log_data.PFVaddr := Cat(s0_l2_region, i.U(REGION_BITS.W), 0.U(log2Up(dcacheParameters.blockBytes).W))
    log_data.PFSink := s0_l2_prefetch_req.sink

    stream_pf_trace_debug_table.log(
      data = log_data,
      en = log_enable,
      site = "StreamPFTrace",
      clock = clock,
      reset = reset
    )
  }

  // tlb req
  // s0: arb all tlb reqs
  val s0_tlb_fire_vec = VecInit((0 until MLP_SIZE).map{case i => tlb_req_arb.io.in(i).fire})
  val s1_tlb_fire_vec = GatedValidRegNext(s0_tlb_fire_vec)
  val s2_tlb_fire_vec = GatedValidRegNext(s1_tlb_fire_vec)

  for(i <- 0 until MLP_SIZE) {
    val l1_evict = s1_l1_alloc && (s1_l1_index === i.U)
    val l2_evict = s1_l2_alloc && ((s1_l2_index + MLP_L1_SIZE.U) === i.U)
    if(i < MLP_L1_SIZE) {
      tlb_req_arb.io.in(i).valid := l1_valids(i) && l1_array(i).is_vaddr && !s1_tlb_fire_vec(i) && !s2_tlb_fire_vec(i) && !l1_evict
      tlb_req_arb.io.in(i).bits.vaddr := l1_array(i).get_tlb_va()
    }else {
      tlb_req_arb.io.in(i).valid := l2_valids(i - MLP_L1_SIZE) && l2_array(i - MLP_L1_SIZE).is_vaddr && !s1_tlb_fire_vec(i) && !s2_tlb_fire_vec(i) && !l2_evict
      tlb_req_arb.io.in(i).bits.vaddr := l2_array(i - MLP_L1_SIZE).get_tlb_va()
    }
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.isPrefetch := true.B
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.kill := false.B
    tlb_req_arb.io.in(i).bits.no_translate := false.B
    tlb_req_arb.io.in(i).bits.fullva := 0.U
    tlb_req_arb.io.in(i).bits.checkfullva := false.B
    tlb_req_arb.io.in(i).bits.memidx := DontCare
    tlb_req_arb.io.in(i).bits.debug := DontCare
    tlb_req_arb.io.in(i).bits.hlvx := DontCare
    tlb_req_arb.io.in(i).bits.hyperinst := DontCare
    tlb_req_arb.io.in(i).bits.pmp_addr  := DontCare
  }

  assert(PopCount(s0_tlb_fire_vec) <= 1.U, "s0_tlb_fire_vec should be one-hot or empty")

  // s1: send out the req
  val s1_tlb_req_valid = GatedValidRegNext(tlb_req_arb.io.out.valid)
  val s1_tlb_req_bits = RegEnable(tlb_req_arb.io.out.bits, tlb_req_arb.io.out.valid)
  val s1_tlb_req_index = RegEnable(OHToUInt(s0_tlb_fire_vec.asUInt), tlb_req_arb.io.out.valid)
  val s1_l1_tlb_evict = s1_l1_alloc && (s1_l1_index === s1_tlb_req_index)
  val s1_l2_tlb_evict = s1_l2_alloc && ((s1_l2_index + MLP_L1_SIZE.U) === s1_tlb_req_index)
  val s1_tlb_evict = s1_l1_tlb_evict || s1_l2_tlb_evict
  io.tlb_req.req.valid := s1_tlb_req_valid && !s1_tlb_evict
  io.tlb_req.req.bits := s1_tlb_req_bits
  io.tlb_req.req_kill := false.B
  tlb_req_arb.io.out.ready := true.B

  XSPerfAccumulate("s1_tlb_req_sent", io.tlb_req.req.valid)
  XSPerfAccumulate("s1_tlb_req_evict", s1_tlb_req_valid && s1_tlb_evict)

  // s2: get response from tlb
  val s2_tlb_resp = io.tlb_req.resp
  val s2_tlb_update_index = RegEnable(s1_tlb_req_index, s1_tlb_req_valid)
  val s2_l1_tlb_evict = s1_l1_alloc && (s1_l1_index === s2_tlb_update_index)
  val s2_l2_tlb_evict = s1_l2_alloc && ((s1_l2_index + MLP_L1_SIZE.U) === s2_tlb_update_index)
  val s2_tlb_evict = s2_l1_tlb_evict || s2_l2_tlb_evict
  when(s2_tlb_resp.valid && !s2_tlb_evict) {
    when(s2_tlb_update_index < MLP_L1_SIZE.U) {
      l1_array(s2_tlb_update_index).is_vaddr := s2_tlb_resp.bits.miss

      when(!s2_tlb_resp.bits.miss) {
        l1_array(s2_tlb_update_index).region := Cat(0.U((VAddrBits - PAddrBits).W), s2_tlb_resp.bits.paddr.head(s2_tlb_resp.bits.paddr.head.getWidth - 1, REGION_TAG_OFFSET))
        when(s2_tlb_resp.bits.excp.head.pf.ld || s2_tlb_resp.bits.excp.head.gpf.ld || s2_tlb_resp.bits.excp.head.af.ld) {
          invalid_array(s2_tlb_update_index, false)
        }
      }
    }.otherwise {
      val inner_index = s2_tlb_update_index - MLP_L1_SIZE.U
      l2_array(inner_index).is_vaddr := s2_tlb_resp.bits.miss
  
      when(!s2_tlb_resp.bits.miss) {
        l2_array(inner_index).region := Cat(0.U((VAddrBits - PAddrBits).W), s2_tlb_resp.bits.paddr.head(s2_tlb_resp.bits.paddr.head.getWidth - 1, REGION_TAG_OFFSET))
        when(s2_tlb_resp.bits.excp.head.pf.ld || s2_tlb_resp.bits.excp.head.gpf.ld || s2_tlb_resp.bits.excp.head.af.ld) {
          invalid_array(inner_index, true)
        }
      }
    }
  }
  s2_tlb_resp.ready := true.B

  XSPerfAccumulate("s2_tlb_resp_valid", s2_tlb_resp.valid)
  XSPerfAccumulate("s2_tlb_resp_evict", s2_tlb_resp.valid && s2_tlb_evict)
  XSPerfAccumulate("s2_tlb_resp_miss", s2_tlb_resp.valid && !s2_tlb_evict && s2_tlb_resp.bits.miss)
  XSPerfAccumulate("s2_tlb_resp_updated", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss)
  XSPerfAccumulate("s2_tlb_resp_page_fault", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss && s2_tlb_resp.bits.excp.head.pf.ld)
  XSPerfAccumulate("s2_tlb_resp_guestpage_fault", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss && s2_tlb_resp.bits.excp.head.gpf.ld)
  XSPerfAccumulate("s2_tlb_resp_access_fault", s2_tlb_resp.valid && !s2_tlb_evict && !s2_tlb_resp.bits.miss && s2_tlb_resp.bits.excp.head.af.ld)

  // l1 pf
  // s0: generate prefetch req paddr per entry, arb them
  val s0_pf_fire_vec = VecInit((0 until MLP_L1_SIZE).map{case i => l1_pf_req_arb.io.in(i).fire})
  val s1_pf_fire_vec = GatedValidRegNext(s0_pf_fire_vec)

  val s0_pf_fire = l1_pf_req_arb.io.out.fire
  val s0_pf_index = l1_pf_req_arb.io.chosen
  val s0_pf_candidate_oh = get_candidate_oh(l1_pf_req_arb.io.out.bits.req.paddr)

  for(i <- 0 until MLP_L1_SIZE) {
    val evict = s1_l1_alloc && (s1_l1_index === i.U)
    l1_pf_req_arb.io.in(i).valid := l1_array(i).can_send_pf(l1_valids(i)) && !evict
    l1_pf_req_arb.io.in(i).bits.req.paddr := l1_array(i).get_pf_addr()
    l1_pf_req_arb.io.in(i).bits.req.alias := l1_array(i).alias
    l1_pf_req_arb.io.in(i).bits.req.confidence := io.confidence
    l1_pf_req_arb.io.in(i).bits.req.is_store := false.B
    l1_pf_req_arb.io.in(i).bits.req.pf_source := l1_array(i).source
    l1_pf_req_arb.io.in(i).bits.debug_vaddr := l1_array(i).get_pf_debug_vaddr()
  }

  when(s0_pf_fire) {
    l1_array(s0_pf_index).sent_vec := l1_array(s0_pf_index).sent_vec | s0_pf_candidate_oh
  }

  assert(PopCount(s0_pf_fire_vec) <= 1.U, "s0_pf_fire_vec should be one-hot or empty")

  // s1: send out to dcache
  val s1_pf_valid = Reg(Bool())
  val s1_pf_bits = RegEnable(l1_pf_req_arb.io.out.bits, l1_pf_req_arb.io.out.fire)
  val s1_pf_index = RegEnable(s0_pf_index, l1_pf_req_arb.io.out.fire)
  val s1_pf_candidate_oh = RegEnable(s0_pf_candidate_oh, l1_pf_req_arb.io.out.fire)
  val s1_pf_evict = s1_l1_alloc && (s1_l1_index === s1_pf_index)
  val s1_pf_update = s1_l1_update && (s1_l1_index === s1_pf_index)
  val s1_pf_can_go = io.l1_req.ready && !s1_pf_evict && !s1_pf_update
  val s1_pf_fire = s1_pf_valid && s1_pf_can_go

  when(s1_pf_can_go) {
    s1_pf_valid := false.B
  }

  when(l1_pf_req_arb.io.out.fire) {
    s1_pf_valid := true.B
  }

  when(s1_pf_fire) {
    l1_array(s1_pf_index).bit_vec := l1_array(s1_pf_index).bit_vec & ~s1_pf_candidate_oh
  }

  val in_pmem = PmemRanges.map(range => s1_pf_bits.req.paddr.inRange(range._1.U, range._2.U)).reduce(_ || _)
  io.l1_req.valid := s1_pf_valid && !s1_pf_evict && !s1_pf_update && in_pmem && io.enable
  io.l1_req.bits := s1_pf_bits.req

  l1_pf_req_arb.io.out.ready := s1_pf_can_go || !s1_pf_valid

  assert(!((s1_l1_alloc || s1_l1_update) && s1_pf_fire && (s1_l1_index === s1_pf_index)), "pf pipeline & enq pipeline bit_vec harzard!")

  XSPerfAccumulate("s1_pf_valid", s1_pf_valid)
  XSPerfAccumulate("s1_pf_block_by_pipe_unready", s1_pf_valid && !io.l1_req.ready)
  XSPerfAccumulate("s1_pf_block_by_enq_alloc_harzard", s1_pf_valid && s1_pf_evict)
  XSPerfAccumulate("s1_pf_block_by_enq_update_harzard", s1_pf_valid && s1_pf_update)
  XSPerfAccumulate("s1_pf_fire", s1_pf_fire)

  // l2 pf
  // s0: generate prefetch req paddr per entry, arb them, sent out
  io.l2_pf_addr.valid := l2_pf_req_arb.io.out.valid
  io.l2_pf_addr.bits := l2_pf_req_arb.io.out.bits.req

  l2_pf_req_arb.io.out.ready := true.B

  for(i <- 0 until MLP_L2L3_SIZE) {
    val evict = s1_l2_alloc && (s1_l2_index === i.U)
    l2_pf_req_arb.io.in(i).valid := l2_array(i).can_send_pf(l2_valids(i)) && (l2_array(i).sink === SINK_L2) && !evict
    l2_pf_req_arb.io.in(i).bits.req.addr := l2_array(i).get_pf_addr()
    l2_pf_req_arb.io.in(i).bits.req.source := MuxLookup(l2_array(i).source.value, MemReqSource.Prefetch2L2Unknown.id.U)(Seq(
      L1_HW_PREFETCH_STRIDE -> MemReqSource.Prefetch2L2Stride.id.U,
      L1_HW_PREFETCH_STREAM -> MemReqSource.Prefetch2L2Stream.id.U
    ))
    l2_pf_req_arb.io.in(i).bits.debug_vaddr := l2_array(i).get_pf_debug_vaddr()
  }

  when(l2_pf_req_arb.io.out.valid) {
    l2_array(l2_pf_req_arb.io.chosen).sent_vec := l2_array(l2_pf_req_arb.io.chosen).sent_vec | get_candidate_oh(l2_pf_req_arb.io.out.bits.req.addr)
  }

  val stream_out_debug_table = ChiselDB.createTable("StreamPFTraceOut" + p(XSCoreParamsKey).HartId.toString, new StreamPFTraceOutEntry, basicDB = false)
  val l1_debug_data = Wire(new StreamPFTraceOutEntry)
  val l2_debug_data = Wire(new StreamPFTraceOutEntry)
  l1_debug_data.PFVaddr := l1_pf_req_arb.io.out.bits.debug_vaddr
  l1_debug_data.PFSink := SINK_L1
  l2_debug_data.PFVaddr := l2_pf_req_arb.io.out.bits.debug_vaddr
  l2_debug_data.PFSink := SINK_L2

  stream_out_debug_table.log(
    data = l1_debug_data,
    en = l1_pf_req_arb.io.out.fire && (l1_pf_req_arb.io.out.bits.req.pf_source.value === L1_HW_PREFETCH_STREAM),
    site = "StreamPFTraceOut",
    clock = clock,
    reset = reset
  )
  stream_out_debug_table.log(
    data = l2_debug_data,
    en = l2_pf_req_arb.io.out.fire && (l2_pf_req_arb.io.out.bits.req.source === MemReqSource.Prefetch2L2Stream.id.U),
    site = "StreamPFTraceOut",
    clock = clock,
    reset = reset
  )

  // last level cache pf
  // s0: generate prefetch req paddr per entry, arb them, sent out
  io.l3_pf_addr.valid := l3_pf_req_arb.io.out.valid
  io.l3_pf_addr.bits := l3_pf_req_arb.io.out.bits

  l3_pf_req_arb.io.out.ready := true.B

  for(i <- 0 until MLP_L2L3_SIZE) {
    val evict = s1_l2_alloc && (s1_l2_index === i.U)
    l3_pf_req_arb.io.in(i).valid := l2_array(i).can_send_pf(l2_valids(i)) && (l2_array(i).sink === SINK_L3) && !evict
    l3_pf_req_arb.io.in(i).bits := l2_array(i).get_pf_addr()
  }

  when(l3_pf_req_arb.io.out.valid) {
    l2_array(l3_pf_req_arb.io.chosen).sent_vec := l2_array(l3_pf_req_arb.io.chosen).sent_vec | get_candidate_oh(l3_pf_req_arb.io.out.bits)
  }

  // reset meta to avoid muti-hit problem
  for(i <- 0 until MLP_SIZE) {
    if(i < MLP_L1_SIZE) {
      when(RegNext(io.flush)) {
        reset_array(i, false)
      }
    }else {
      when(RegNext(io.flush)) {
        reset_array(i - MLP_L1_SIZE, true)
      }
    }
  }

  XSPerfAccumulate("l2_prefetche_queue_busby", io.l2PfqBusy)
  XSPerfHistogram("filter_active", PopCount(VecInit(
    l1_array.zip(l1_valids).map{ case (e, v) => e.can_send_pf(v) } ++ 
    l2_array.zip(l2_valids).map{ case (e, v) => e.can_send_pf(v) }
    ).asUInt), true.B, 0, MLP_SIZE, 1)
  XSPerfHistogram("l1_filter_active", PopCount(VecInit(l1_array.zip(l1_valids).map{ case (e, v) => e.can_send_pf(v)}).asUInt), true.B, 0, MLP_L1_SIZE, 1)
  XSPerfHistogram("l2_filter_active", PopCount(VecInit(l2_array.zip(l2_valids).map{ case (e, v) => e.can_send_pf(v) && (e.sink === SINK_L2)}).asUInt), true.B, 0, MLP_L2L3_SIZE, 1)
  XSPerfHistogram("l3_filter_active", PopCount(VecInit(l2_array.zip(l2_valids).map{ case (e, v) => e.can_send_pf(v) && (e.sink === SINK_L3)}).asUInt), true.B, 0, MLP_L2L3_SIZE, 1)
}

class L1Prefetcher(implicit p: Parameters) extends BasePrefecher with HasStreamPrefetchHelper with HasStridePrefetchHelper {
  val pf_ctrl = IO(Input(new PrefetchControlBundle))
  val stride_train = IO(Flipped(Vec(backendParams.LduCnt + backendParams.HyuCnt, ValidIO(new LdPrefetchTrainBundle()))))
  val l2PfqBusy = IO(Input(Bool()))

  val stride_train_filter = Module(new TrainFilter(STRIDE_FILTER_SIZE, "stride"))
  val stride_meta_array = Module(new StrideMetaArray)
  val stream_train_filter = Module(new TrainFilter(STREAM_FILTER_SIZE, "stream"))
  val stream_bit_vec_array = Module(new StreamBitVectorArray)
  val pf_queue_filter = Module(new MutiLevelPrefetchFilter)

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

  // stream has higher priority than stride
  pf_queue_filter.io.l1_prefetch_req.valid := stream_bit_vec_array.io.l1_prefetch_req.valid || stride_meta_array.io.l1_prefetch_req.valid
  pf_queue_filter.io.l1_prefetch_req.bits := Mux(
    stream_bit_vec_array.io.l1_prefetch_req.valid,
    stream_bit_vec_array.io.l1_prefetch_req.bits,
    stride_meta_array.io.l1_prefetch_req.bits
  )

  pf_queue_filter.io.l2_l3_prefetch_req.valid := stream_bit_vec_array.io.l2_l3_prefetch_req.valid || stride_meta_array.io.l2_l3_prefetch_req.valid
  pf_queue_filter.io.l2_l3_prefetch_req.bits := Mux(
    stream_bit_vec_array.io.l2_l3_prefetch_req.valid,
    stream_bit_vec_array.io.l2_l3_prefetch_req.bits,
    stride_meta_array.io.l2_l3_prefetch_req.bits
  )

  io.l1_req.valid := pf_queue_filter.io.l1_req.valid && enable && pf_ctrl.enable
  io.l1_req.bits := pf_queue_filter.io.l1_req.bits

  pf_queue_filter.io.l1_req.ready := Mux(pf_ctrl.enable, io.l1_req.ready, true.B)
  pf_queue_filter.io.tlb_req <> io.tlb_req
  pf_queue_filter.io.enable := enable
  pf_queue_filter.io.flush := flush
  pf_queue_filter.io.confidence := pf_ctrl.confidence
  pf_queue_filter.io.l2PfqBusy := l2PfqBusy

  val l2_in_pmem = PmemRanges.map(range => pf_queue_filter.io.l2_pf_addr.bits.addr.inRange(range._1.U, range._2.U)).reduce(_ || _)
  io.l2_req.valid := pf_queue_filter.io.l2_pf_addr.valid && l2_in_pmem && enable && pf_ctrl.enable
  io.l2_req.bits := pf_queue_filter.io.l2_pf_addr.bits

  val l3_in_pmem = PmemRanges.map(range => pf_queue_filter.io.l3_pf_addr.bits.inRange(range._1.U, range._2.U)).reduce(_ || _)
  io.l3_req.valid := pf_queue_filter.io.l3_pf_addr.valid && l3_in_pmem && enable && pf_ctrl.enable
  io.l3_req.bits := pf_queue_filter.io.l3_pf_addr.bits
}
