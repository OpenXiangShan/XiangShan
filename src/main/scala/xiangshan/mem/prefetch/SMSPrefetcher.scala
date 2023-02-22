package xiangshan.mem.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import huancun.utils.ValidIODelay
import xiangshan._
import utils._
import xiangshan.cache.mmu._

case class SMSParams
(
  region_size: Int = 1024,
  vaddr_hash_width: Int = 5,
  block_addr_raw_width: Int = 10,
  filter_table_size: Int = 16,
  active_gen_table_size: Int = 16,
  pht_size: Int = 64,
  pht_ways: Int = 2,
  pht_hist_bits: Int = 2,
  pht_tag_bits: Int = 13,
  pht_lookup_queue_size: Int = 4,
  pf_filter_size: Int = 16
) extends PrefetcherParams

trait HasSMSModuleHelper extends HasCircularQueuePtrHelper
{ this: HasXSParameter =>
  val smsParams = coreParams.prefetcher.get.asInstanceOf[SMSParams]
  val BLK_ADDR_WIDTH = VAddrBits - log2Up(dcacheParameters.blockBytes)
  val REGION_SIZE = smsParams.region_size
  val REGION_BLKS = smsParams.region_size / dcacheParameters.blockBytes
  val REGION_ADDR_BITS = VAddrBits - log2Up(REGION_SIZE)
  val REGION_OFFSET = log2Up(REGION_BLKS)
  val VADDR_HASH_WIDTH = smsParams.vaddr_hash_width
  val BLK_ADDR_RAW_WIDTH = smsParams.block_addr_raw_width
  val REGION_ADDR_RAW_WIDTH = BLK_ADDR_RAW_WIDTH - REGION_OFFSET
  val BLK_TAG_WIDTH = BLK_ADDR_RAW_WIDTH + VADDR_HASH_WIDTH
  val REGION_TAG_WIDTH = REGION_ADDR_RAW_WIDTH + VADDR_HASH_WIDTH
  val PHT_INDEX_BITS = log2Up(smsParams.pht_size / smsParams.pht_ways)
  val PHT_TAG_BITS = smsParams.pht_tag_bits
  val PHT_HIST_BITS = smsParams.pht_hist_bits

  def block_addr(x: UInt): UInt = {
    val offset = log2Up(dcacheParameters.blockBytes)
    x(x.getWidth - 1, offset)
  }

  def region_addr(x: UInt): UInt = {
    val offset = log2Up(REGION_SIZE)
    x(x.getWidth - 1, offset)
  }

  def region_offset_to_bits(off: UInt): UInt = {
    (1.U << off).asUInt
  }

  def region_hash_tag(rg_addr: UInt): UInt = {
    val low = rg_addr(REGION_ADDR_RAW_WIDTH - 1, 0)
    val high = rg_addr(REGION_ADDR_RAW_WIDTH + 3 * VADDR_HASH_WIDTH - 1, REGION_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  def page_bit(region_addr: UInt): UInt = {
    region_addr(log2Up(dcacheParameters.pageSize/REGION_SIZE))
  }

  def block_hash_tag(x: UInt): UInt = {
    val blk_addr = block_addr(x)
    val low = blk_addr(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = blk_addr(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = vaddr_hash(high)
    Cat(high_hash, low)
  }

  def vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }

  def pht_index(pc: UInt): UInt = {
    val low_bits = pc(PHT_INDEX_BITS, 2)
    val hi_bit = pc(1) ^ pc(PHT_INDEX_BITS+1)
    Cat(hi_bit, low_bits)
  }

  def pht_tag(pc: UInt): UInt = {
    pc(PHT_INDEX_BITS + 2 + PHT_TAG_BITS - 1, PHT_INDEX_BITS + 2)
  }
}

class FilterTable()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val s0_lookup = Flipped(ValidIO(new FilterEntry()))
    val s1_result = ValidIO(new FilterEntry())
    val s1_update = Input(Bool())
  })

  val s0_lookup_entry = io.s0_lookup.bits
  val s0_lookup_valid = io.s0_lookup.valid

  val entries = Seq.fill(smsParams.filter_table_size){ Reg(new FilterEntry()) }
  val valids = Seq.fill(smsParams.filter_table_size){ RegInit(false.B) }
  val w_ptr = RegInit(0.U(log2Up(smsParams.filter_table_size).W))

  val prev_entry = RegEnable(s0_lookup_entry, s0_lookup_valid)
  val prev_lookup_valid = RegNext(s0_lookup_valid, false.B)

  val s0_entry_match_vec = entries.zip(valids).map({
    case (ent, v) => v && ent.region_tag === s0_lookup_entry.region_tag && ent.offset =/= s0_lookup_entry.offset
  })
  val s0_any_entry_match = Cat(s0_entry_match_vec).orR
  val s0_matched_entry = Mux1H(s0_entry_match_vec, entries)
  val s0_match_s1 = prev_lookup_valid && prev_entry.region_tag === s0_lookup_entry.region_tag

  val s0_hit = s0_lookup_valid && (s0_any_entry_match || s0_match_s1)

  val s0_lookup_result = Wire(new FilterEntry())
  s0_lookup_result := Mux(s0_match_s1, prev_entry, s0_matched_entry)
  io.s1_result.valid := RegNext(s0_hit, false.B)
  io.s1_result.bits := RegEnable(s0_lookup_result, s0_hit)

  val s0_invalid_mask = valids.map(!_)
  val s0_has_invalid_entry = Cat(s0_invalid_mask).orR
  val s0_invalid_index = PriorityEncoder(s0_invalid_mask)
  // if match, invalidte entry
  for((v, i) <- valids.zipWithIndex){
    when(s0_lookup_valid && s0_entry_match_vec(i)){
      v := false.B
    }
  }

  // stage1
  val s1_has_invalid_entry = RegEnable(s0_has_invalid_entry, s0_lookup_valid)
  val s1_invalid_index = RegEnable(s0_invalid_index, s0_lookup_valid)
  // alloc entry if (s0 miss && s1_update)
  val s1_do_update = io.s1_update && prev_lookup_valid && !io.s1_result.valid
  val update_ptr = Mux(s1_has_invalid_entry, s1_invalid_index, w_ptr)
  when(s1_do_update && !s1_has_invalid_entry){ w_ptr := w_ptr + 1.U }
  for((ent, i) <- entries.zipWithIndex){
    val wen = s1_do_update && update_ptr === i.U
    when(wen){
      valids(i) := true.B
      ent := prev_entry
    }
  }

  XSPerfAccumulate("sms_filter_table_hit", io.s1_result.valid)
  XSPerfAccumulate("sms_filter_table_update", s1_do_update)
  for(i <- 0 until smsParams.filter_table_size){
    XSPerfAccumulate(s"sms_filter_table_access_$i",
      s1_do_update && update_ptr === i.U
    )
  }
}

class FilterEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val pht_index = UInt(PHT_INDEX_BITS.W)
  val pht_tag = UInt(PHT_TAG_BITS.W)
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val offset = UInt(REGION_OFFSET.W)
}

class AGTEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val pht_index = UInt(PHT_INDEX_BITS.W)
  val pht_tag = UInt(PHT_TAG_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val access_cnt = UInt((REGION_BLKS-1).U.getWidth.W)
  val decr_mode = Bool()
}

class PfGenReq()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_addr = UInt(REGION_ADDR_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val paddr_valid = Bool()
  val decr_mode = Bool()
}

class ActiveGenerationTable()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val s0_lookup = Flipped(ValidIO(new Bundle() {
      val region_tag = UInt(REGION_TAG_WIDTH.W)
      val region_p1_tag = UInt(REGION_TAG_WIDTH.W)
      val region_m1_tag = UInt(REGION_TAG_WIDTH.W)
      val region_offset = UInt(REGION_OFFSET.W)
      val pht_index = UInt(PHT_INDEX_BITS.W)
      val pht_tag = UInt(PHT_TAG_BITS.W)
      val allow_cross_region_p1 = Bool()
      val allow_cross_region_m1 = Bool()
      val region_p1_cross_page = Bool()
      val region_m1_cross_page = Bool()
      val region_paddr = UInt(REGION_ADDR_BITS.W)
      val region_vaddr = UInt(REGION_ADDR_BITS.W)
    }))
    // do not alloc entry in filter table if agt hit
    val s1_match_or_alloc = Output(Bool())
    // if agt missed, try lookup pht
    val s2_pht_lookup = ValidIO(new PhtLookup())
    // receive second hit from filter table
    val s1_recv_entry = Flipped(ValidIO(new AGTEntry()))
    // evict entry to pht
    val s2_evict = ValidIO(new AGTEntry())
    val s2_pf_gen_req = ValidIO(new PfGenReq())
  })

  val entries = Seq.fill(smsParams.active_gen_table_size){ Reg(new AGTEntry()) }
  val valids = Seq.fill(smsParams.active_gen_table_size){ RegInit(false.B) }
  val replacement = ReplacementPolicy.fromString("plru", smsParams.active_gen_table_size)

  val s0_lookup = io.s0_lookup.bits
  val s0_lookup_valid = io.s0_lookup.valid

  val prev_lookup = RegEnable(s0_lookup, s0_lookup_valid)
  val prev_lookup_valid = RegNext(s0_lookup_valid, false.B)

  val s0_match_prev = prev_lookup_valid && s0_lookup.region_tag === prev_lookup.region_tag

  def gen_match_vec(region_tag: UInt): Seq[Bool] = {
    entries.zip(valids).map({
      case (ent, v) => v && ent.region_tag === region_tag
    })
  }

  val region_match_vec_s0 = gen_match_vec(s0_lookup.region_tag)
  val region_p1_match_vec_s0 = gen_match_vec(s0_lookup.region_p1_tag)
  val region_m1_match_vec_s0 = gen_match_vec(s0_lookup.region_m1_tag)

  val any_region_match = Cat(region_match_vec_s0).orR
  val any_region_p1_match = Cat(region_p1_match_vec_s0).orR && s0_lookup.allow_cross_region_p1
  val any_region_m1_match = Cat(region_m1_match_vec_s0).orR && s0_lookup.allow_cross_region_m1

  val s0_region_hit = any_region_match
  // region miss, but cross region match
  val s0_alloc = !s0_region_hit && (any_region_p1_match || any_region_m1_match) && !s0_match_prev
  val s0_match_or_alloc = any_region_match || any_region_p1_match || any_region_m1_match
  val s0_pf_gen_match_vec = valids.indices.map(i => {
    Mux(any_region_match,
      region_match_vec_s0(i),
      Mux(any_region_m1_match,
        region_m1_match_vec_s0(i), region_p1_match_vec_s0(i)
      )
    )
  })
  val s0_agt_entry = Wire(new AGTEntry())

  s0_agt_entry.pht_index := s0_lookup.pht_index
  s0_agt_entry.pht_tag := s0_lookup.pht_tag
  s0_agt_entry.region_bits := region_offset_to_bits(s0_lookup.region_offset)
  s0_agt_entry.region_tag := s0_lookup.region_tag
  s0_agt_entry.access_cnt := 1.U
  // lookup_region + 1 == entry_region
  // lookup_region = entry_region - 1 => decr mode
  s0_agt_entry.decr_mode := !s0_region_hit && !any_region_m1_match && any_region_p1_match
  val s0_replace_mask = UIntToOH(replacement.way)
  // s0 hit a entry that may be replaced in s1
  val s0_update_conflict = Cat(VecInit(region_match_vec_s0).asUInt & s0_replace_mask).orR

  // stage1: update/alloc
  val s1_agt_entry = RegEnable(s0_agt_entry, s0_lookup_valid)
  val s1_recv_entry = io.s1_recv_entry
  // cross region match or filter table second hit
  val s1_cross_region_match = RegNext(s0_lookup_valid && s0_alloc, false.B)
  val s1_alloc = s1_cross_region_match || s1_recv_entry.valid
  val s1_alloc_entry = Mux(s1_recv_entry.valid, s1_recv_entry.bits, s1_agt_entry)
  val s1_replace_mask = RegEnable(s0_replace_mask, s0_lookup_valid)
  val s1_evict_entry = Mux1H(s1_replace_mask, entries)
  val s1_evict_valid = Mux1H(s1_replace_mask, valids)
  // region hit, update entry
  val s1_update_conflict = RegEnable(s0_update_conflict, s0_lookup_valid && s0_region_hit)
  val s1_update = RegNext(s0_lookup_valid && s0_region_hit, false.B) && !s1_update_conflict
  val s1_update_mask = RegEnable(
    VecInit(region_match_vec_s0),
    VecInit(Seq.fill(smsParams.active_gen_table_size){ false.B }),
    s0_lookup_valid
  )
  val s1_allow_cross_region_p1 = RegEnable(s0_lookup.allow_cross_region_p1, s0_lookup_valid)
  val s1_allow_cross_region_m1 = RegEnable(s0_lookup.allow_cross_region_m1, s0_lookup_valid)
  val s1_region_p1_cross_page = RegEnable(s0_lookup.region_p1_cross_page, s0_lookup_valid)
  val s1_region_m1_cross_page = RegEnable(s0_lookup.region_m1_cross_page, s0_lookup_valid)
  val s1_pf_gen_match_vec = RegEnable(VecInit(s0_pf_gen_match_vec), s0_lookup_valid)
  val s1_region_paddr = RegEnable(s0_lookup.region_paddr, s0_lookup_valid)
  val s1_region_vaddr = RegEnable(s0_lookup.region_vaddr, s0_lookup_valid)
  val s1_region_offset = RegEnable(s0_lookup.region_offset, s0_lookup_valid)
  for(i <- entries.indices){
    val alloc = s1_replace_mask(i) && s1_alloc
    val update = s1_update_mask(i) && s1_update
    val update_entry = WireInit(entries(i))
    update_entry.region_bits := entries(i).region_bits | s1_agt_entry.region_bits
    update_entry.access_cnt := Mux(entries(i).access_cnt === (REGION_BLKS - 1).U,
      entries(i).access_cnt,
      entries(i).access_cnt + (s1_agt_entry.region_bits & (~entries(i).region_bits).asUInt).orR
    )
    valids(i) := valids(i) || alloc
    entries(i) := Mux(alloc, s1_alloc_entry, Mux(update, update_entry, entries(i)))
  }
  when(s1_update) {
    replacement.access(OHToUInt(s1_update_mask))
  }.elsewhen(s1_alloc){
    replacement.access(OHToUInt(s1_replace_mask))
  }

  io.s1_match_or_alloc := s1_update || s1_alloc


  // pf_addr
  // 1.hit => pf_addr = lookup_addr + (decr ? -1 : 1)
  // 2.lookup region - 1 hit => lookup_addr + 1 (incr mode)
  // 3.lookup region + 1 hit => lookup_addr - 1 (decr mode)
  val s1_hited_entry_decr = Mux1H(s1_update_mask, entries.map(_.decr_mode))
  val s1_pf_gen_decr_mode = Mux(s1_update,
    s1_hited_entry_decr,
    s1_agt_entry.decr_mode
  )
  val s1_pf_gen_paddr_valid = Mux(s1_pf_gen_decr_mode,
    !s1_region_m1_cross_page,
    !s1_region_p1_cross_page
  )

  val s1_pf_gen_vaddr_inc = Cat(0.U, s1_region_vaddr, s1_region_offset) + 30.U
  val s1_pf_gen_vaddr_dec = Cat(0.U, s1_region_vaddr, s1_region_offset) - 30.U
  val PAGE_BIT = log2Up(dcacheParameters.pageSize / dcacheParameters.blockBytes)
  val s1_vaddr_inc_cross_page = s1_pf_gen_vaddr_inc(PAGE_BIT) =/= s1_region_vaddr(PAGE_BIT - REGION_OFFSET)
  val s1_vaddr_dec_cross_page = s1_pf_gen_vaddr_dec(PAGE_BIT) =/= s1_region_vaddr(PAGE_BIT - REGION_OFFSET)
  val s1_vaddr_inc_cross_max_lim = s1_pf_gen_vaddr_inc.head(1).asBool
  val s1_vaddr_dec_cross_max_lim = s1_pf_gen_vaddr_dec.head(1).asBool

  //val s1_pf_gen_vaddr_p1 = s1_region_vaddr(REGION_TAG_WIDTH - 1, 0) + 1.U
  //val s1_pf_gen_vaddr_m1 = s1_region_vaddr(REGION_TAG_WIDTH - 1, 0) - 1.U
  val s1_pf_gen_vaddr = Cat(
    s1_region_vaddr(REGION_ADDR_BITS - 1, REGION_TAG_WIDTH),
    Mux(s1_pf_gen_decr_mode,
      s1_pf_gen_vaddr_dec.tail(1).head(REGION_TAG_WIDTH),
      s1_pf_gen_vaddr_inc.tail(1).head(REGION_TAG_WIDTH)
    )
  )
  val s1_pf_gen_offset = Mux(s1_pf_gen_decr_mode,
    s1_pf_gen_vaddr_dec(REGION_OFFSET - 1, 0),
    s1_pf_gen_vaddr_inc(REGION_OFFSET - 1, 0)
  )
  val s1_pf_gen_offset_mask = UIntToOH(s1_pf_gen_offset)
  val s1_pf_gen_access_cnt = Mux1H(s1_pf_gen_match_vec, entries.map(_.access_cnt))
  val s1_pf_gen_valid = prev_lookup_valid && io.s1_match_or_alloc && Mux(s1_pf_gen_decr_mode,
    !s1_vaddr_dec_cross_page || !s1_vaddr_dec_cross_max_lim,
    !s1_vaddr_inc_cross_page || !s1_vaddr_inc_cross_max_lim
  ) && (s1_pf_gen_access_cnt > (REGION_BLKS * 3 / 4).U)
  val s1_pf_gen_region_tag = s1_agt_entry.region_tag
  val s1_pf_gen_region_addr = Mux(s1_pf_gen_paddr_valid,
    s1_region_paddr, s1_pf_gen_vaddr
  )
  val s1_pf_gen_incr_region_bits = VecInit((0 until REGION_BLKS).map(i => {
    if(i == 0) true.B else !s1_pf_gen_offset_mask(i - 1, 0).orR
  })).asUInt
  val s1_pf_gen_decr_region_bits = VecInit((0 until REGION_BLKS).map(i => {
    if(i == REGION_BLKS - 1) true.B
    else !s1_pf_gen_offset_mask(REGION_BLKS - 1, i + 1).orR
  })).asUInt
  val s1_pf_gen_region_bits = Mux(s1_pf_gen_decr_mode,
    s1_pf_gen_decr_region_bits,
    s1_pf_gen_incr_region_bits
  )
  val s1_pht_lookup_valid = Wire(Bool())
  val s1_pht_lookup = Wire(new PhtLookup())

  s1_pht_lookup_valid := !s1_pf_gen_valid && prev_lookup_valid
  s1_pht_lookup.pht_index := s1_agt_entry.pht_index
  s1_pht_lookup.pht_tag := s1_agt_entry.pht_tag
  s1_pht_lookup.region_tag := s1_agt_entry.region_tag
  s1_pht_lookup.region_addr := s1_region_paddr
  s1_pht_lookup.paddr_valid := true.B
  s1_pht_lookup.region_offset := s1_region_offset

  // stage2: gen pf reg / evict entry to pht
  val s2_evict_entry = RegEnable(s1_evict_entry, s1_alloc)
  val s2_evict_valid = RegNext(s1_alloc && s1_evict_valid, false.B)
  val s2_paddr_valid = RegEnable(s1_pf_gen_paddr_valid, s1_pf_gen_valid)
  val s2_pf_gen_region_tag = RegEnable(s1_pf_gen_region_tag, s1_pf_gen_valid)
  val s2_pf_gen_decr_mode = RegEnable(s1_pf_gen_decr_mode, s1_pf_gen_valid)
  val s2_pf_gen_region_paddr = RegEnable(s1_pf_gen_region_addr, s1_pf_gen_valid)
  val s2_pf_gen_region_bits = RegEnable(s1_pf_gen_region_bits, s1_pf_gen_valid)
  val s2_pf_gen_valid = RegNext(s1_pf_gen_valid, false.B)
  val s2_pht_lookup_valid = RegNext(s1_pht_lookup_valid, false.B)
  val s2_pht_lookup = RegEnable(s1_pht_lookup, s1_pht_lookup_valid)

  io.s2_evict.valid := s2_evict_valid
  io.s2_evict.bits := s2_evict_entry

  io.s2_pf_gen_req.bits.region_tag := s2_pf_gen_region_tag
  io.s2_pf_gen_req.bits.region_addr := s2_pf_gen_region_paddr
  io.s2_pf_gen_req.bits.region_bits := s2_pf_gen_region_bits
  io.s2_pf_gen_req.bits.paddr_valid := s2_paddr_valid
  io.s2_pf_gen_req.bits.decr_mode := s2_pf_gen_decr_mode
  io.s2_pf_gen_req.valid := s2_pf_gen_valid

  io.s2_pht_lookup.valid := s2_pht_lookup_valid
  io.s2_pht_lookup.bits := s2_pht_lookup

  XSPerfAccumulate("sms_agt_alloc", s1_alloc) // cross region match or filter evict
  XSPerfAccumulate("sms_agt_update", s1_update) // entry hit
  XSPerfAccumulate("sms_agt_pf_gen", io.s2_pf_gen_req.valid)
  XSPerfAccumulate("sms_agt_pf_gen_paddr_valid",
    io.s2_pf_gen_req.valid && io.s2_pf_gen_req.bits.paddr_valid
  )
  XSPerfAccumulate("sms_agt_pf_gen_decr_mode",
    io.s2_pf_gen_req.valid && io.s2_pf_gen_req.bits.decr_mode
  )
  for(i <- 0 until smsParams.active_gen_table_size){
    XSPerfAccumulate(s"sms_agt_access_entry_$i",
      s1_alloc && s1_replace_mask(i) || s1_update && s1_update_mask(i)
    )
  }

}

class PhtLookup()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val pht_index = UInt(PHT_INDEX_BITS.W)
  val pht_tag = UInt(PHT_TAG_BITS.W)
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_addr = UInt(REGION_ADDR_BITS.W)
  val paddr_valid = Bool()
  val region_offset = UInt(REGION_OFFSET.W)
}

class PhtEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val hist = Vec(REGION_BLKS, UInt(PHT_HIST_BITS.W))
  val tag = UInt(PHT_TAG_BITS.W)
  val decr_mode = Bool()
}

class PatternHistoryTable()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    // receive agt evicted entry
    val agt_update = Flipped(ValidIO(new AGTEntry()))
    // at stage2, if we know agt missed, lookup pht
    val s2_agt_lookup = Flipped(ValidIO(new PhtLookup()))
    // pht-generated prefetch req
    val pf_gen_req = ValidIO(new PfGenReq())
  })

  val pht_ram = Module(new SRAMTemplate[PhtEntry](new PhtEntry,
    set = smsParams.pht_size / smsParams.pht_ways,
    way =smsParams.pht_ways,
    singlePort = true
  ))
  def PHT_SETS = smsParams.pht_size / smsParams.pht_ways
  val pht_valids = Seq.fill(smsParams.pht_ways){
    RegInit(VecInit(Seq.fill(PHT_SETS){false.B}))
  }
  val replacement = Seq.fill(PHT_SETS) { ReplacementPolicy.fromString("plru", smsParams.pht_ways) }

  val lookup_queue = Module(new OverrideableQueue(new PhtLookup, smsParams.pht_lookup_queue_size))
  lookup_queue.io.in := io.s2_agt_lookup
  val lookup = lookup_queue.io.out

  val evict_queue = Module(new OverrideableQueue(new AGTEntry, smsParams.pht_lookup_queue_size))
  evict_queue.io.in := io.agt_update
  val evict = evict_queue.io.out

  val s3_ram_en = Wire(Bool())
  val s1_valid = Wire(Bool())
  // pipe s0: select an op from [lookup, update], generate ram read addr
  val s0_valid = lookup.valid || evict.valid
  val s0_ready = !s1_valid || s1_valid && !s3_ram_en

  evict.ready := !s1_valid || !s3_ram_en
  lookup.ready := evict.ready && !evict.valid

  val s0_ram_raddr = Mux(evict.valid,
    evict.bits.pht_index,
    lookup.bits.pht_index
  )
  val s0_tag = Mux(evict.valid, evict.bits.pht_tag, lookup.bits.pht_tag)
  val s0_region_addr = lookup.bits.region_addr
  val s0_region_tag = lookup.bits.region_tag
  val s0_region_offset = lookup.bits.region_offset
  val s0_paddr_valid = lookup.bits.paddr_valid
  val s0_region_bits = evict.bits.region_bits
  val s0_decr_mode = evict.bits.decr_mode
  val s0_evict = evict.valid

  // pipe s1: send addr to ram
  val s1_valid_r = RegInit(false.B)
  s1_valid_r := Mux(s3_ram_en, s1_valid, s0_valid)
  s1_valid := s1_valid_r
  val s1_reg_en = s0_valid && (!s3_ram_en || !s1_valid)
  val s1_ram_raddr = RegEnable(s0_ram_raddr, s1_reg_en)
  val s1_tag = RegEnable(s0_tag, s1_reg_en)
  val s1_region_bits = RegEnable(s0_region_bits, s1_reg_en)
  val s1_decr_mode = RegEnable(s0_decr_mode, s1_reg_en)
  val s1_region_addr = RegEnable(s0_region_addr, s1_reg_en)
  val s1_region_tag = RegEnable(s0_region_tag, s1_reg_en)
  val s1_region_offset = RegEnable(s0_region_offset, s1_reg_en)
  val s1_paddr_valid = RegEnable(s0_paddr_valid, s1_reg_en)
  val s1_pht_valids = pht_valids.map(way => Mux1H(
    (0 until PHT_SETS).map(i => i.U === s1_ram_raddr),
    way
  ))
  val s1_evict = RegEnable(s0_evict, s1_reg_en)
  val s1_replace_way = Mux1H(
    (0 until PHT_SETS).map(i => i.U === s1_ram_raddr),
    replacement.map(_.way)
  )

  // pipe s2: generate ram write addr/data
  val s2_valid = RegNext(s1_valid && !s3_ram_en, false.B)
  val s2_reg_en = s1_valid && !s3_ram_en
  val s2_tag = RegEnable(s1_tag, s2_reg_en)
  val s2_region_bits = RegEnable(s1_region_bits, s2_reg_en)
  val s2_decr_mode = RegEnable(s1_decr_mode, s2_reg_en)
  val s2_region_addr = RegEnable(s1_region_addr, s2_reg_en)
  val s2_region_tag = RegEnable(s1_region_tag, s2_reg_en)
  val s2_region_offset = RegEnable(s1_region_offset, s2_reg_en)
  val s2_region_offset_mask = region_offset_to_bits(s2_region_offset)
  val s2_paddr_valid = RegEnable(s1_paddr_valid, s2_reg_en)
  val s2_evict = RegEnable(s1_evict, s2_reg_en)
  val s2_pht_valids = s1_pht_valids.map(v => RegEnable(v, s2_reg_en))
  val s2_replace_way = RegEnable(s1_replace_way, s2_reg_en)
  val s2_ram_waddr = RegEnable(s1_ram_raddr, s2_reg_en)
  val s2_ram_rdata = pht_ram.io.r.resp.data
  val s2_ram_rtags = s2_ram_rdata.map(_.tag)
  val s2_tag_match_vec = s2_ram_rtags.map(t => t === s2_tag)
  val s2_hit_vec = s2_tag_match_vec.zip(s2_pht_valids).map({
    case (tag_match, v) => v && tag_match
  })
  val s2_hist_update = s2_ram_rdata.map(way => VecInit(way.hist.zipWithIndex.map({
    case (h, i) => Mux(s2_region_bits(i),
      Mux(h === ((1 << PHT_HIST_BITS) - 1).U, h, h + 1.U),
      Mux(h === 0.U, 0.U, h - 1.U)
    )
  })))
  val s2_hits_pf_gen = s2_ram_rdata.map(way => VecInit(way.hist.map(_.head(1))).asUInt)
  val s2_new_hist = VecInit((0 until REGION_BLKS).map(i => {
    Cat(0.U((PHT_HIST_BITS - 1).W), s2_region_bits(i))
  }))
  val s2_pht_hit = Cat(s2_hit_vec).orR
  val s2_hist = Mux(s2_pht_hit, Mux1H(s2_hit_vec, s2_hist_update), s2_new_hist)
  val s2_repl_way_mask = UIntToOH(s2_replace_way)
  val s2_pf_gen_region_bits = Mux1H(s2_hit_vec, s2_hits_pf_gen)

  // pipe s3: send addr/data to ram, gen pf_req
  val s3_valid = RegNext(s2_valid, false.B)
  val s3_evict = RegEnable(s2_evict, s2_valid)
  val s3_hist = RegEnable(s2_hist, s2_valid)
  val s3_pf_gen_region_bits = RegEnable(s2_pf_gen_region_bits, s2_valid)
  val s3_decr_mode = RegEnable(s2_decr_mode, s2_valid)
  val s3_region_addr = RegEnable(s2_region_addr, s2_valid)
  val s3_region_tag = RegEnable(s2_region_tag, s2_valid)
  val s3_region_offset_mask = RegEnable(s2_region_offset_mask, s2_valid)
  val s3_paddr_valid = RegEnable(s2_paddr_valid, s2_valid)
  val s3_pht_tag = RegEnable(s2_tag, s2_valid)
  val s3_hit_vec = s2_hit_vec.map(h => RegEnable(h, s2_valid))
  val s3_hit = Cat(s3_hit_vec).orR
  val s3_hit_way = OHToUInt(s3_hit_vec)
  val s3_repl_way = RegEnable(s2_replace_way, s2_valid)
  val s3_repl_way_mask = RegEnable(s2_repl_way_mask, s2_valid)
  val s3_repl_update_mask = RegEnable(VecInit((0 until PHT_SETS).map(i => i.U === s2_ram_waddr)), s2_valid)
  val s3_ram_waddr = RegEnable(s2_ram_waddr, s2_valid)
  s3_ram_en := s3_valid && s3_evict
  val s3_ram_wdata = Wire(new PhtEntry())
  s3_ram_wdata.hist := s3_hist
  s3_ram_wdata.tag := s3_pht_tag
  s3_ram_wdata.decr_mode := s3_decr_mode

  for((valids, way_idx) <- pht_valids.zipWithIndex){
    val update_way = s3_repl_way_mask(way_idx)
    for((v, set_idx) <- valids.zipWithIndex){
      val update_set = s3_repl_update_mask(set_idx)
      when(s3_valid && s3_evict && !s3_hit && update_set && update_way){
        v := true.B
      }
    }
  }
  for((r, i) <- replacement.zipWithIndex){
    when(s3_valid && s3_repl_update_mask(i)){
      when(s3_hit){
        r.access(s3_hit_way)
      }.elsewhen(s3_evict){
        r.access(s3_repl_way)
      }
    }
  }

  val s3_way_mask = Mux(s3_hit,
    VecInit(s3_hit_vec).asUInt,
    s3_repl_way_mask,
  ).asUInt

  pht_ram.io.r(
    s1_valid, s1_ram_raddr
  )
  pht_ram.io.w(
    s3_ram_en, s3_ram_wdata, s3_ram_waddr, s3_way_mask
  )
  // generate pf req if hit
  val s3_pf_gen_region_masked = s3_pf_gen_region_bits & (~s3_region_offset_mask).asUInt
  io.pf_gen_req.valid := s3_valid && !s3_evict && s3_hit && s3_pf_gen_region_masked.orR
  io.pf_gen_req.bits.region_addr := s3_region_addr
  io.pf_gen_req.bits.region_tag := s3_region_tag
  // mask out current req
  io.pf_gen_req.bits.region_bits := s3_pf_gen_region_masked
  io.pf_gen_req.bits.paddr_valid := s3_paddr_valid
  io.pf_gen_req.bits.decr_mode := s3_decr_mode

  XSPerfAccumulate("sms_pht_update", io.agt_update.valid)
  XSPerfAccumulate("sms_pht_update_hit", s2_valid && s2_evict && s2_pht_hit)
  XSPerfAccumulate("sms_pht_lookup", io.s2_agt_lookup.valid)
  XSPerfAccumulate("sms_pht_lookup_hit", s2_valid && !s2_evict && s2_pht_hit)
  for(i <- 0 until smsParams.pht_ways){
    XSPerfAccumulate(s"sms_pht_write_way_$i", pht_ram.io.w.req.fire && pht_ram.io.w.req.bits.waymask.get(i))
  }
  for(i <- 0 until PHT_SETS){
    XSPerfAccumulate(s"sms_pht_write_set_$i", pht_ram.io.w.req.fire && pht_ram.io.w.req.bits.setIdx === i.U)
  }
  XSPerfAccumulate(s"sms_pht_hit_but_no_region", s3_valid && !s3_evict && s3_hit && !s3_pf_gen_region_masked.orR)
  XSPerfAccumulate(s"sms_pht_pf_gen", io.pf_gen_req.valid)
}

class PrefetchFilterEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_addr = UInt(REGION_ADDR_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val filter_bits = UInt(REGION_BLKS.W)
  val paddr_valid = Bool()
  val decr_mode = Bool()
}

class PrefetchFilter()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val gen_req = Flipped(ValidIO(new PfGenReq()))
    val tlb_req = new TlbRequestIO(2)
    val l2_pf_addr = ValidIO(UInt(PAddrBits.W))
  })
  val entries = Seq.fill(smsParams.pf_filter_size){ Reg(new PrefetchFilterEntry()) }
  val valids = Seq.fill(smsParams.pf_filter_size){ RegInit(false.B) }
  val replacement = ReplacementPolicy.fromString("plru", smsParams.pf_filter_size)

  val prev_valid = RegNext(io.gen_req.valid, false.B)
  val prev_gen_req = RegEnable(io.gen_req.bits, io.gen_req.valid)

  val tlb_req_arb = Module(new RRArbiter(new TlbReq, smsParams.pf_filter_size))
  val pf_req_arb = Module(new RRArbiter(UInt(PAddrBits.W), smsParams.pf_filter_size))

  io.tlb_req.req <> tlb_req_arb.io.out
  io.tlb_req.req_kill := false.B
  io.tlb_req.resp.ready := true.B
  io.l2_pf_addr.valid := pf_req_arb.io.out.valid
  io.l2_pf_addr.bits := pf_req_arb.io.out.bits
  pf_req_arb.io.out.ready := true.B

  val s1_valid = Wire(Bool())
  val s1_replace_vec = Wire(UInt(smsParams.pf_filter_size.W))
  val s1_tlb_fire_vec = Wire(UInt(smsParams.pf_filter_size.W))

  // s0: entries lookup
  val s0_gen_req = io.gen_req.bits
  val s0_match_prev = prev_valid && (s0_gen_req.region_tag === prev_gen_req.region_tag)
  val s0_gen_req_valid = io.gen_req.valid && !s0_match_prev
  val s0_match_vec = valids.indices.map(i => {
    valids(i) && entries(i).region_tag === s0_gen_req.region_tag && !(s1_valid && s1_replace_vec(i))
  })
  val s0_any_matched = Cat(s0_match_vec).orR
  val s0_replace_vec = UIntToOH(replacement.way)
  val s0_hit = s0_gen_req_valid && s0_any_matched

  for(((v, ent), i) <- valids.zip(entries).zipWithIndex){
    val is_evicted = s1_valid && s1_replace_vec(i)
    tlb_req_arb.io.in(i).valid := v && !s1_tlb_fire_vec(i) && !ent.paddr_valid && !is_evicted
    tlb_req_arb.io.in(i).bits.vaddr := Cat(ent.region_addr, 0.U(log2Up(REGION_SIZE).W))
    tlb_req_arb.io.in(i).bits.cmd := TlbCmd.read
    tlb_req_arb.io.in(i).bits.size := 3.U
    tlb_req_arb.io.in(i).bits.robIdx := DontCare
    tlb_req_arb.io.in(i).bits.debug := DontCare

    val pending_req_vec = ent.region_bits & (~ent.filter_bits).asUInt
    val first_one_offset = PriorityMux(
      pending_req_vec.asBools,
      (0 until smsParams.filter_table_size).map(_.U(REGION_OFFSET.W))
    )
    val last_one_offset = PriorityMux(
      pending_req_vec.asBools.reverse,
      (0 until smsParams.filter_table_size).reverse.map(_.U(REGION_OFFSET.W))
    )
    val pf_addr = Cat(
      ent.region_addr,
      Mux(ent.decr_mode, last_one_offset, first_one_offset),
      0.U(log2Up(dcacheParameters.blockBytes).W)
    )
    pf_req_arb.io.in(i).valid := v && Cat(pending_req_vec).orR && ent.paddr_valid && !is_evicted
    pf_req_arb.io.in(i).bits := pf_addr
  }

  val s0_tlb_fire_vec = VecInit(tlb_req_arb.io.in.map(_.fire))
  val s0_pf_fire_vec = VecInit(pf_req_arb.io.in.map(_.fire))

  // s1: update or alloc
  val s1_valid_r = RegNext(s0_gen_req_valid, false.B)
  val s1_hit = RegEnable(s0_hit, s0_gen_req_valid)
  val s1_gen_req = RegEnable(s0_gen_req, s0_gen_req_valid)
  val s1_replace_vec_r = RegEnable(s0_replace_vec, s0_gen_req_valid && !s0_hit)
  val s1_update_vec = RegEnable(VecInit(s0_match_vec).asUInt, s0_gen_req_valid && s0_hit)
  val s1_tlb_fire_vec_r = RegNext(s0_tlb_fire_vec, 0.U.asTypeOf(s0_tlb_fire_vec))
  val s1_alloc_entry = Wire(new PrefetchFilterEntry())
  s1_valid := s1_valid_r
  s1_replace_vec := s1_replace_vec_r
  s1_tlb_fire_vec := s1_tlb_fire_vec_r.asUInt
  s1_alloc_entry.region_tag := s1_gen_req.region_tag
  s1_alloc_entry.region_addr := s1_gen_req.region_addr
  s1_alloc_entry.region_bits := s1_gen_req.region_bits
  s1_alloc_entry.paddr_valid := s1_gen_req.paddr_valid
  s1_alloc_entry.decr_mode := s1_gen_req.decr_mode
  s1_alloc_entry.filter_bits := 0.U
  for(((v, ent), i) <- valids.zip(entries).zipWithIndex){
    val alloc = s1_valid && !s1_hit && s1_replace_vec(i)
    val update = s1_valid && s1_hit && s1_update_vec(i)
    // for pf: use s0 data
    val pf_fired = s0_pf_fire_vec(i)
    val tlb_fired = s1_tlb_fire_vec(i) && !io.tlb_req.resp.bits.miss
    when(tlb_fired){
      ent.paddr_valid := !io.tlb_req.resp.bits.miss
      ent.region_addr := region_addr(io.tlb_req.resp.bits.paddr.head)
    }
    when(update){
      ent.region_bits := ent.region_bits | s1_gen_req.region_bits
    }
    when(pf_fired){
      val curr_bit = UIntToOH(block_addr(pf_req_arb.io.in(i).bits)(REGION_OFFSET - 1, 0))
      ent.filter_bits := ent.filter_bits | curr_bit
    }
    when(alloc){
      ent := s1_alloc_entry
      v := true.B
    }
  }
  val s1_access_mask = Mux(s1_hit, s1_update_vec, s1_replace_vec)
  val s1_access_way = OHToUInt(s1_access_mask.asUInt)
  when(s1_valid){
    replacement.access(s1_access_way)
  }

  XSPerfAccumulate("sms_pf_filter_recv_req", io.gen_req.valid)
  XSPerfAccumulate("sms_pf_filter_hit", s1_valid && s1_hit)
  XSPerfAccumulate("sms_pf_filter_tlb_req", io.tlb_req.req.fire)
  XSPerfAccumulate("sms_pf_filter_tlb_resp_miss", io.tlb_req.resp.fire && io.tlb_req.resp.bits.miss)
  for(i <- 0 until smsParams.pf_filter_size){
    XSPerfAccumulate(s"sms_pf_filter_access_way_$i", s1_valid && s1_access_way === i.U)
  }
  XSPerfAccumulate("sms_pf_filter_l2_req", io.l2_pf_addr.valid)
}

class SMSPrefetcher()(implicit p: Parameters) extends BasePrefecher with HasSMSModuleHelper {

  require(exuParameters.LduCnt == 2)

  val io_agt_en = IO(Input(Bool()))
  val io_pht_en = IO(Input(Bool()))

  val ld_curr = io.ld_in.map(_.bits)
  val ld_curr_block_tag = ld_curr.map(x => block_hash_tag(x.vaddr))

  // block filter
  val ld_prev = io.ld_in.map(ld => RegEnable(ld.bits, ld.valid))
  val ld_prev_block_tag = ld_curr_block_tag.zip(io.ld_in.map(_.valid)).map({
    case (tag, v) => RegEnable(tag, v)
  })
  val ld_prev_vld = io.ld_in.map(ld => RegNext(ld.valid, false.B))

  val ld_curr_match_prev = ld_curr_block_tag.map(cur_tag =>
    Cat(ld_prev_block_tag.zip(ld_prev_vld).map({
      case (prev_tag, prev_vld) => prev_vld && prev_tag === cur_tag
    })).orR
  )
  val ld0_match_ld1 = io.ld_in.head.valid && io.ld_in.last.valid && ld_curr_block_tag.head === ld_curr_block_tag.last
  val ld_curr_vld = Seq(
    io.ld_in.head.valid && !ld_curr_match_prev.head,
    io.ld_in.last.valid && !ld_curr_match_prev.last && !ld0_match_ld1
  )
  val ld0_older_than_ld1 = Cat(ld_curr_vld).andR && isBefore(ld_curr.head.uop.robIdx, ld_curr.last.uop.robIdx)
  val pending_vld = RegNext(Cat(ld_curr_vld).andR, false.B)
  val pending_sel_ld0 = RegNext(Mux(pending_vld, ld0_older_than_ld1, !ld0_older_than_ld1))
  val pending_ld = Mux(pending_sel_ld0, ld_prev.head, ld_prev.last)
  val pending_ld_block_tag = Mux(pending_sel_ld0, ld_prev_block_tag.head, ld_prev_block_tag.last)

  // prepare training data
  val train_ld = RegEnable(
    Mux(pending_vld, pending_ld, Mux(ld0_older_than_ld1 || !ld_curr_vld.last, ld_curr.head, ld_curr.last)),
    pending_vld || Cat(ld_curr_vld).orR
  )

  val train_block_tag = RegEnable(
    Mux(pending_vld, pending_ld_block_tag,
      Mux(ld0_older_than_ld1 || !ld_curr_vld.last, ld_curr_block_tag.head, ld_curr_block_tag.last)
    ),
    pending_vld || Cat(ld_curr_vld).orR
  )
  val train_region_tag = train_block_tag.head(REGION_TAG_WIDTH)

  val train_region_addr_raw = region_addr(train_ld.vaddr)(REGION_TAG_WIDTH + 2 * VADDR_HASH_WIDTH - 1, 0)
  val train_region_addr_p1 = Cat(0.U(1.W), train_region_addr_raw) + 1.U
  val train_region_addr_m1 = Cat(0.U(1.W), train_region_addr_raw) - 1.U
  // addr_p1 or addr_m1 is valid?
  val train_allow_cross_region_p1 = !train_region_addr_p1.head(1).asBool
  val train_allow_cross_region_m1 = !train_region_addr_m1.head(1).asBool

  val train_region_p1_tag = region_hash_tag(train_region_addr_p1.tail(1))
  val train_region_m1_tag = region_hash_tag(train_region_addr_m1.tail(1))

  val train_region_p1_cross_page = page_bit(train_region_addr_p1) ^ page_bit(train_region_addr_raw)
  val train_region_m1_cross_page = page_bit(train_region_addr_m1) ^ page_bit(train_region_addr_raw)

  val train_region_paddr = region_addr(train_ld.paddr)
  val train_region_vaddr = region_addr(train_ld.vaddr)
  val train_region_offset = train_block_tag(REGION_OFFSET - 1, 0)
  val train_vld = RegNext(pending_vld || Cat(ld_curr_vld).orR, false.B)


  // prefetch stage0
  val filter_table = Module(new FilterTable())
  val active_gen_table = Module(new ActiveGenerationTable())
  val pht = Module(new PatternHistoryTable())
  val pf_filter = Module(new PrefetchFilter())

  val train_vld_s0 = RegNext(train_vld, false.B)
  val train_s0 = RegEnable(train_ld, train_vld)
  val train_region_tag_s0 = RegEnable(train_region_tag, train_vld)
  val train_region_p1_tag_s0 = RegEnable(train_region_p1_tag, train_vld)
  val train_region_m1_tag_s0 = RegEnable(train_region_m1_tag, train_vld)
  val train_allow_cross_region_p1_s0 = RegEnable(train_allow_cross_region_p1, train_vld)
  val train_allow_cross_region_m1_s0 = RegEnable(train_allow_cross_region_m1, train_vld)
  val train_pht_tag_s0 = RegEnable(pht_tag(train_ld.uop.cf.pc), train_vld)
  val train_pht_index_s0 = RegEnable(pht_index(train_ld.uop.cf.pc), train_vld)
  val train_region_offset_s0 = RegEnable(train_region_offset, train_vld)
  val train_region_p1_cross_page_s0 = RegEnable(train_region_p1_cross_page, train_vld)
  val train_region_m1_cross_page_s0 = RegEnable(train_region_m1_cross_page, train_vld)
  val train_region_paddr_s0 = RegEnable(train_region_paddr, train_vld)
  val train_region_vaddr_s0 = RegEnable(train_region_vaddr, train_vld)

  filter_table.io.s0_lookup.valid := train_vld_s0
  filter_table.io.s0_lookup.bits.pht_tag := train_pht_tag_s0
  filter_table.io.s0_lookup.bits.pht_index := train_pht_index_s0
  filter_table.io.s0_lookup.bits.region_tag := train_region_tag_s0
  filter_table.io.s0_lookup.bits.offset := train_region_offset_s0
  filter_table.io.s1_update := !active_gen_table.io.s1_match_or_alloc

  active_gen_table.io.s0_lookup.valid := train_vld_s0
  active_gen_table.io.s0_lookup.bits.region_tag := train_region_tag_s0
  active_gen_table.io.s0_lookup.bits.region_p1_tag := train_region_p1_tag_s0
  active_gen_table.io.s0_lookup.bits.region_m1_tag := train_region_m1_tag_s0
  active_gen_table.io.s0_lookup.bits.region_offset := train_region_offset_s0
  active_gen_table.io.s0_lookup.bits.pht_index := train_pht_index_s0
  active_gen_table.io.s0_lookup.bits.pht_tag := train_pht_tag_s0
  active_gen_table.io.s0_lookup.bits.allow_cross_region_p1 := train_allow_cross_region_p1_s0
  active_gen_table.io.s0_lookup.bits.allow_cross_region_m1 := train_allow_cross_region_m1_s0
  active_gen_table.io.s0_lookup.bits.region_p1_cross_page := train_region_p1_cross_page_s0
  active_gen_table.io.s0_lookup.bits.region_m1_cross_page := train_region_m1_cross_page_s0
  active_gen_table.io.s0_lookup.bits.region_paddr := train_region_paddr_s0
  active_gen_table.io.s0_lookup.bits.region_vaddr := train_region_vaddr_s0

  val train_region_offset_s1 = RegEnable(train_region_offset_s0, train_vld_s0)
  val agt_region_bits_s1 = region_offset_to_bits(train_region_offset_s1) |
    region_offset_to_bits(filter_table.io.s1_result.bits.offset)

  active_gen_table.io.s1_recv_entry.valid := filter_table.io.s1_result.valid
  active_gen_table.io.s1_recv_entry.bits.pht_index := filter_table.io.s1_result.bits.pht_index
  active_gen_table.io.s1_recv_entry.bits.pht_tag := filter_table.io.s1_result.bits.pht_tag
  active_gen_table.io.s1_recv_entry.bits.region_bits := agt_region_bits_s1
  active_gen_table.io.s1_recv_entry.bits.region_tag := filter_table.io.s1_result.bits.region_tag
  active_gen_table.io.s1_recv_entry.bits.access_cnt := 2.U
  active_gen_table.io.s1_recv_entry.bits.decr_mode := false.B

  pht.io.s2_agt_lookup := active_gen_table.io.s2_pht_lookup
  pht.io.agt_update := active_gen_table.io.s2_evict

  val pht_gen_valid = pht.io.pf_gen_req.valid && io_pht_en
  val agt_gen_valid = active_gen_table.io.s2_pf_gen_req.valid && io_agt_en
  val pf_gen_req = Mux(pht_gen_valid,
    pht.io.pf_gen_req.bits,
    active_gen_table.io.s2_pf_gen_req.bits
  )
  pf_filter.io.gen_req.valid := pht_gen_valid || agt_gen_valid
  pf_filter.io.gen_req.bits := pf_gen_req
  io.tlb_req <> pf_filter.io.tlb_req
  io.pf_addr.valid := pf_filter.io.l2_pf_addr.valid && io.enable
  io.pf_addr.bits := pf_filter.io.l2_pf_addr.bits

  XSPerfAccumulate("sms_pf_gen_conflict",
    pht.io.pf_gen_req.valid && active_gen_table.io.s2_pf_gen_req.valid
  )
  XSPerfAccumulate("sms_pht_disabled", pht.io.pf_gen_req.valid && !io_pht_en)
  XSPerfAccumulate("sms_agt_disabled", active_gen_table.io.s2_pf_gen_req.valid && !io_agt_en)
  XSPerfAccumulate("sms_pf_real_issued", io.pf_addr.valid)
}
