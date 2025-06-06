/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Stephen Somogyi, Thomas F. Wenisch, Anastassia Ailamaki, Babak Falsafi and Andreas Moshovos. "[Spatial memory
* streaming.](https://doi.org/10.1109/ISCA.2006.38)" 33rd International Symposium on Computer Architecture (ISCA).
* 2006.
***************************************************************************************/

package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.mem.L1PrefetchReq
import xiangshan.mem.Bundles.LsPrefetchTrainBundle
import xiangshan.mem.trace._
import xiangshan.mem.HasL1PrefetchSourceParameter
import xiangshan.cache.HasDCacheParameters
import xiangshan.cache.mmu._

case class SMSParams
(
  region_size: Int = 1024,
  vaddr_hash_width: Int = 5,
  block_addr_raw_width: Int = 10,
  stride_pc_bits: Int = 10,
  max_stride: Int = 1024,
  stride_entries: Int = 16,
  active_gen_table_size: Int = 16,
  pht_size: Int = 64,
  pht_ways: Int = 2,
  pht_hist_bits: Int = 2,
  pht_tag_bits: Int = 13,
  pht_lookup_queue_size: Int = 4,
  pf_filter_size: Int = 16,
  train_filter_size: Int = 8
) extends PrefetcherParams

trait HasSMSModuleHelper extends HasCircularQueuePtrHelper with HasDCacheParameters
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
  // page bit index in block addr
  val BLOCK_ADDR_PAGE_BIT = log2Up(dcacheParameters.pageSize / dcacheParameters.blockBytes)
  val REGION_ADDR_PAGE_BIT = log2Up(dcacheParameters.pageSize / smsParams.region_size)
  val STRIDE_PC_BITS = smsParams.stride_pc_bits
  val STRIDE_BLK_ADDR_BITS = log2Up(smsParams.max_stride)

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

  def get_alias_bits(region_vaddr: UInt): UInt = {
    val offset = log2Up(REGION_SIZE)
    get_alias(Cat(region_vaddr, 0.U(offset.W)))
  }
}

class StridePF()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val stride_en = Input(Bool())
    val s0_lookup = Flipped(new ValidIO(new Bundle() {
      val pc = UInt(STRIDE_PC_BITS.W)
      val vaddr = UInt(VAddrBits.W)
      val paddr = UInt(PAddrBits.W)
    }))
    val s1_valid = Input(Bool())
    val s2_gen_req = ValidIO(new PfGenReq())
  })

  val prev_valid = GatedValidRegNext(io.s0_lookup.valid, false.B)
  val prev_pc = RegEnable(io.s0_lookup.bits.pc, io.s0_lookup.valid)

  val s0_valid = io.s0_lookup.valid && !(prev_valid && prev_pc === io.s0_lookup.bits.pc)

  def entry_map[T](fn: Int => T) = (0 until smsParams.stride_entries).map(fn)

  val replacement = ReplacementPolicy.fromString("plru", smsParams.stride_entries)
  val valids = entry_map(_ => RegInit(false.B))
  val entries_pc = entry_map(_ => Reg(UInt(STRIDE_PC_BITS.W)) )
  val entries_conf = entry_map(_ => RegInit(1.U(2.W)))
  val entries_last_addr = entry_map(_ => Reg(UInt(STRIDE_BLK_ADDR_BITS.W)) )
  val entries_stride = entry_map(_ => Reg(SInt((STRIDE_BLK_ADDR_BITS+1).W)))


  val s0_match_vec = valids.zip(entries_pc).map({
    case (v, pc) => v && pc === io.s0_lookup.bits.pc
  })

  val s0_hit = s0_valid && Cat(s0_match_vec).orR
  val s0_miss = s0_valid && !s0_hit
  val s0_matched_conf = Mux1H(s0_match_vec, entries_conf)
  val s0_matched_last_addr = Mux1H(s0_match_vec, entries_last_addr)
  val s0_matched_last_stride = Mux1H(s0_match_vec, entries_stride)

  val s1_hit = GatedValidRegNext(s0_hit) && io.s1_valid
  val s1_alloc = GatedValidRegNext(s0_miss) && io.s1_valid
  val s1_vaddr = RegEnable(io.s0_lookup.bits.vaddr, s0_valid)
  val s1_paddr = RegEnable(io.s0_lookup.bits.paddr, s0_valid)
  val s1_conf = RegEnable(s0_matched_conf, s0_valid)
  val s1_last_addr = RegEnable(s0_matched_last_addr, s0_valid)
  val s1_last_stride = RegEnable(s0_matched_last_stride, s0_valid)
  val s1_match_vec = RegEnable(VecInit(s0_match_vec), s0_valid)

  val BLOCK_OFFSET = log2Up(dcacheParameters.blockBytes)
  val s1_new_stride_vaddr = s1_vaddr(BLOCK_OFFSET + STRIDE_BLK_ADDR_BITS - 1, BLOCK_OFFSET)
  val s1_new_stride = (0.U(1.W) ## s1_new_stride_vaddr).asSInt - (0.U(1.W) ## s1_last_addr).asSInt
  val s1_stride_non_zero = s1_last_stride =/= 0.S
  val s1_stride_match = s1_new_stride === s1_last_stride && s1_stride_non_zero
  val s1_replace_idx = replacement.way

  for(i <- 0 until smsParams.stride_entries){
    val alloc = s1_alloc && i.U === s1_replace_idx
    val update = s1_hit && s1_match_vec(i)
    when(update){
      assert(valids(i))
      entries_conf(i) := Mux(s1_stride_match,
        Mux(s1_conf === 3.U, 3.U, s1_conf + 1.U),
        Mux(s1_conf === 0.U, 0.U, s1_conf - 1.U)
      )
      entries_last_addr(i) := s1_new_stride_vaddr
      when(!s1_conf(1)){
        entries_stride(i) := s1_new_stride
      }
    }
    when(alloc){
      valids(i) := true.B
      entries_pc(i) := prev_pc
      entries_conf(i) := 0.U
      entries_last_addr(i) := s1_new_stride_vaddr
      entries_stride(i) := 0.S
    }
    assert(!(update && alloc))
  }
  when(s1_hit){
    replacement.access(OHToUInt(s1_match_vec.asUInt))
  }.elsewhen(s1_alloc){
    replacement.access(s1_replace_idx)
  }

  val s1_block_vaddr = block_addr(s1_vaddr)
  val s1_pf_block_vaddr = (s1_block_vaddr.asSInt + s1_last_stride).asUInt
  val s1_pf_cross_page = s1_pf_block_vaddr(BLOCK_ADDR_PAGE_BIT) =/= s1_block_vaddr(BLOCK_ADDR_PAGE_BIT)

  val s2_pf_gen_valid = GatedValidRegNext(s1_hit && s1_stride_match, false.B)
  val s2_pf_gen_paddr_valid = RegEnable(!s1_pf_cross_page, s1_hit && s1_stride_match)
  val s2_pf_block_vaddr = RegEnable(s1_pf_block_vaddr, s1_hit && s1_stride_match)
  val s2_block_paddr = RegEnable(block_addr(s1_paddr), s1_hit && s1_stride_match)

  val s2_pf_block_addr = Mux(s2_pf_gen_paddr_valid,
    Cat(
      s2_block_paddr(PAddrBits - BLOCK_OFFSET - 1, BLOCK_ADDR_PAGE_BIT),
      s2_pf_block_vaddr(BLOCK_ADDR_PAGE_BIT - 1, 0)
    ),
    s2_pf_block_vaddr
  )
  val s2_pf_full_addr = Wire(UInt(VAddrBits.W))
  s2_pf_full_addr := s2_pf_block_addr ## 0.U(BLOCK_OFFSET.W)

  val s2_pf_region_addr = region_addr(s2_pf_full_addr)
  val s2_pf_region_offset = s2_pf_block_addr(REGION_OFFSET - 1, 0)

  val s2_full_vaddr = Wire(UInt(VAddrBits.W))
  s2_full_vaddr := s2_pf_block_vaddr ## 0.U(BLOCK_OFFSET.W)

  val s2_region_tag = region_hash_tag(region_addr(s2_full_vaddr))

  io.s2_gen_req.valid := s2_pf_gen_valid && io.stride_en
  io.s2_gen_req.bits.region_tag := s2_region_tag
  io.s2_gen_req.bits.region_addr := s2_pf_region_addr
  io.s2_gen_req.bits.alias_bits := get_alias_bits(region_addr(s2_full_vaddr))
  io.s2_gen_req.bits.region_bits := region_offset_to_bits(s2_pf_region_offset)
  io.s2_gen_req.bits.paddr_valid := s2_pf_gen_paddr_valid
  io.s2_gen_req.bits.decr_mode := false.B
  io.s2_gen_req.bits.debug_source_type := HW_PREFETCH_STRIDE.U

}

class AGTEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val pht_index = UInt(PHT_INDEX_BITS.W)
  val pht_tag = UInt(PHT_TAG_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val region_bit_single = UInt(REGION_BLKS.W)
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_offset = UInt(REGION_OFFSET.W)
  val access_cnt = UInt((REGION_BLKS-1).U.getWidth.W)
  val decr_mode = Bool()
  val single_update = Bool()//this is a signal update request
  val has_been_signal_updated = Bool()
}

class PfGenReq()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_addr = UInt(REGION_ADDR_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val paddr_valid = Bool()
  val decr_mode = Bool()
  val alias_bits = UInt(2.W)
  val debug_source_type = UInt(log2Up(nSourceType).W)
}

class AGTEvictReq()(implicit p: Parameters) extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
}

class ActiveGenerationTable()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val agt_en = Input(Bool())
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
    // dcache has released a block, evict it from agt
    val s0_dcache_evict = Flipped(DecoupledIO(new AGTEvictReq))
    val s1_sel_stride = Output(Bool())
    val s2_stride_hit = Input(Bool())
    // if agt/stride missed, try lookup pht
    val s2_pht_lookup = ValidIO(new PhtLookup())
    // evict entry to pht
    val s2_evict = ValidIO(new AGTEntry())
    val s2_pf_gen_req = ValidIO(new PfGenReq())
    val act_threshold = Input(UInt(REGION_OFFSET.W))
    val act_stride = Input(UInt(6.W))
  })

  val entries = Seq.fill(smsParams.active_gen_table_size){ Reg(new AGTEntry()) }
  val valids = Seq.fill(smsParams.active_gen_table_size){ RegInit(false.B) }
  val replacement = ReplacementPolicy.fromString("plru", smsParams.active_gen_table_size)

  val s1_replace_mask_w = Wire(UInt(smsParams.active_gen_table_size.W))

  val s0_lookup = io.s0_lookup.bits
  val s0_lookup_valid = io.s0_lookup.valid

  val s0_dcache_evict = io.s0_dcache_evict.bits
  val s0_dcache_evict_valid = io.s0_dcache_evict.valid
  val s0_dcache_evict_tag = block_hash_tag(s0_dcache_evict.vaddr).head(REGION_TAG_WIDTH)

  val prev_lookup = RegEnable(s0_lookup, s0_lookup_valid)
  val prev_lookup_valid = GatedValidRegNext(s0_lookup_valid, false.B)

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

  val region_match_vec_dcache_evict_s0 = gen_match_vec(s0_dcache_evict_tag)
  val any_region_dcache_evict_match = Cat(region_match_vec_dcache_evict_s0).orR
  // s0 dcache evict a entry that may be replaced in s1
  val s0_dcache_evict_conflict = Cat(VecInit(region_match_vec_dcache_evict_s0).asUInt & s1_replace_mask_w).orR
  val s0_do_dcache_evict = io.s0_dcache_evict.fire && any_region_dcache_evict_match

  io.s0_dcache_evict.ready := !s0_lookup_valid && !s0_dcache_evict_conflict

  val s0_region_hit = any_region_match
  val s0_cross_region_hit = any_region_m1_match || any_region_p1_match
  val s0_alloc = s0_lookup_valid && !s0_region_hit && !s0_match_prev
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
  // update bits this time
  s0_agt_entry.region_bit_single := region_offset_to_bits(s0_lookup.region_offset)
  s0_agt_entry.region_tag := s0_lookup.region_tag
  s0_agt_entry.region_offset := s0_lookup.region_offset
  s0_agt_entry.access_cnt := 1.U

  s0_agt_entry.has_been_signal_updated := false.B
  // lookup_region + 1 == entry_region
  // lookup_region = entry_region - 1 => decr mode
  s0_agt_entry.decr_mode := !s0_region_hit && !any_region_m1_match && any_region_p1_match
  val s0_replace_way = replacement.way
  val s0_replace_mask = UIntToOH(s0_replace_way)
  // s0 hit a entry that may be replaced in s1
  val s0_update_conflict = Cat(VecInit(region_match_vec_s0).asUInt & s1_replace_mask_w).orR
  val s0_update = s0_lookup_valid && s0_region_hit && !s0_update_conflict
  s0_agt_entry.single_update := s0_update

  val s0_access_way = Mux1H(
    Seq(s0_update, s0_alloc),
    Seq(OHToUInt(region_match_vec_s0), s0_replace_way)
  )
  when(s0_update || s0_alloc) {
    replacement.access(s0_access_way)
  }

  // stage1: update/alloc
  // region hit, update entry
  val s1_update = GatedValidRegNext(s0_update, false.B)
  val s1_update_mask = RegEnable(VecInit(region_match_vec_s0), s0_lookup_valid)
  val s1_agt_entry = RegEnable(s0_agt_entry, s0_lookup_valid)
  val s1_cross_region_match = RegEnable(s0_cross_region_hit, s0_lookup_valid)
  val s1_alloc = GatedValidRegNext(s0_alloc, false.B)
  val s1_alloc_entry = s1_agt_entry
  val s1_do_dcache_evict = GatedValidRegNext(s0_do_dcache_evict, false.B)
  val s1_replace_mask = Mux(
    s1_do_dcache_evict,
    RegEnable(VecInit(region_match_vec_dcache_evict_s0).asUInt, s0_do_dcache_evict),
    RegEnable(s0_replace_mask, s0_lookup_valid)
  )
  s1_replace_mask_w := s1_replace_mask & Fill(smsParams.active_gen_table_size, s1_alloc || s1_do_dcache_evict)
  val s1_evict_entry = Mux1H(s1_replace_mask, entries)
  val s1_evict_valid = Mux1H(s1_replace_mask, valids)
  // pf gen
  val s1_pf_gen_match_vec = RegEnable(VecInit(s0_pf_gen_match_vec), s0_lookup_valid)
  val s1_region_paddr = RegEnable(s0_lookup.region_paddr, s0_lookup_valid)
  val s1_region_vaddr = RegEnable(s0_lookup.region_vaddr, s0_lookup_valid)
  val s1_region_offset = RegEnable(s0_lookup.region_offset, s0_lookup_valid)
  val s1_bit_region_signal = RegEnable(region_offset_to_bits(s0_lookup.region_offset), s0_lookup_valid)

  for(i <- entries.indices){
    val alloc = s1_replace_mask(i) && s1_alloc
    val update = s1_update_mask(i) && s1_update
    val update_entry = WireInit(entries(i))
    update_entry.region_bits := entries(i).region_bits | s1_agt_entry.region_bits
    update_entry.access_cnt := Mux(entries(i).access_cnt === (REGION_BLKS - 1).U,
      entries(i).access_cnt,
      entries(i).access_cnt + (s1_agt_entry.region_bits & (~entries(i).region_bits).asUInt).orR
    )
    update_entry.region_bit_single := s1_agt_entry.region_bit_single
    update_entry.has_been_signal_updated := entries(i).has_been_signal_updated || (!((s1_alloc || s1_do_dcache_evict) && s1_evict_valid)) && s1_update
    valids(i) := valids(i) || alloc
    entries(i) := Mux(alloc, s1_alloc_entry, Mux(update, update_entry, entries(i)))
  }

  val s1_update_entry = Mux1H(s1_update_mask, entries)
  val s1_update_valid = Mux1H(s1_update_mask, valids)


  when(s1_update){
    assert(PopCount(s1_update_mask) === 1.U, "multi-agt-update")
  }
  when(s1_alloc){
    assert(PopCount(s1_replace_mask) === 1.U, "multi-agt-alloc")
  }

  // pf_addr
  // 1.hit => pf_addr = lookup_addr + (decr ? -1 : 1)
  // 2.lookup region - 1 hit => lookup_addr + 1 (incr mode)
  // 3.lookup region + 1 hit => lookup_addr - 1 (decr mode)
  val s1_hited_entry_decr = Mux1H(s1_update_mask, entries.map(_.decr_mode))
  val s1_pf_gen_decr_mode = Mux(s1_update,
    s1_hited_entry_decr,
    s1_agt_entry.decr_mode
  )

  val s1_pf_gen_vaddr_inc = Cat(0.U, s1_region_vaddr(REGION_TAG_WIDTH - 1, 0), s1_region_offset) + io.act_stride
  val s1_pf_gen_vaddr_dec = Cat(0.U, s1_region_vaddr(REGION_TAG_WIDTH - 1, 0), s1_region_offset) - io.act_stride
  val s1_vaddr_inc_cross_page = s1_pf_gen_vaddr_inc(BLOCK_ADDR_PAGE_BIT) =/= s1_region_vaddr(REGION_ADDR_PAGE_BIT)
  val s1_vaddr_dec_cross_page = s1_pf_gen_vaddr_dec(BLOCK_ADDR_PAGE_BIT) =/= s1_region_vaddr(REGION_ADDR_PAGE_BIT)
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
  val s1_in_active_page = s1_pf_gen_access_cnt > io.act_threshold
  val s1_pf_gen_valid = prev_lookup_valid && (s1_alloc && s1_cross_region_match || s1_update) && Mux(s1_pf_gen_decr_mode,
    !s1_vaddr_dec_cross_max_lim,
    !s1_vaddr_inc_cross_max_lim
  ) && s1_in_active_page && io.agt_en
  val s1_pf_gen_paddr_valid = Mux(s1_pf_gen_decr_mode, !s1_vaddr_dec_cross_page, !s1_vaddr_inc_cross_page)
  val s1_pf_gen_region_addr = Mux(s1_pf_gen_paddr_valid,
    Cat(s1_region_paddr(REGION_ADDR_BITS - 1, REGION_ADDR_PAGE_BIT), s1_pf_gen_vaddr(REGION_ADDR_PAGE_BIT - 1, 0)),
    s1_pf_gen_vaddr
  )
  val s1_pf_gen_region_tag = region_hash_tag(s1_pf_gen_vaddr)
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
  s1_pht_lookup.region_vaddr := s1_region_vaddr
  s1_pht_lookup.region_paddr := s1_region_paddr
  s1_pht_lookup.region_offset := s1_region_offset
  s1_pht_lookup.region_bit_single := s1_bit_region_signal

  io.s1_sel_stride := prev_lookup_valid && (s1_alloc && s1_cross_region_match || s1_update) && !s1_in_active_page

  // stage2: gen pf reg / evict entry to pht
  // if no evict, update this time region bits to pht
  val s2_do_dcache_evict = GatedValidRegNext(s1_do_dcache_evict, false.B)
  val s1_send_update_entry = Mux((s1_alloc || s1_do_dcache_evict) && s1_evict_valid, s1_evict_entry, s1_update_entry)
  val s2_evict_entry = RegEnable(s1_send_update_entry, s1_alloc || s1_do_dcache_evict || s1_update)
  val s2_evict_valid = GatedValidRegNext(((s1_alloc || s1_do_dcache_evict) && s1_evict_valid) || s1_update, false.B)
  val s2_update = RegNext(s1_update, false.B)
  val s2_real_update = RegNext(((s1_alloc || s1_do_dcache_evict) && s1_evict_valid), false.B)
  val s2_paddr_valid = RegEnable(s1_pf_gen_paddr_valid, s1_pf_gen_valid)
  val s2_pf_gen_region_tag = RegEnable(s1_pf_gen_region_tag, s1_pf_gen_valid)
  val s2_pf_gen_decr_mode = RegEnable(s1_pf_gen_decr_mode, s1_pf_gen_valid)
  val s2_pf_gen_region_paddr = RegEnable(s1_pf_gen_region_addr, s1_pf_gen_valid)
  val s2_pf_gen_alias_bits = RegEnable(get_alias_bits(s1_pf_gen_vaddr), s1_pf_gen_valid)
  val s2_pf_gen_region_bits = RegEnable(s1_pf_gen_region_bits, s1_pf_gen_valid)
  val s2_pf_gen_valid = GatedValidRegNext(s1_pf_gen_valid, false.B)
  val s2_pht_lookup_valid = GatedValidRegNext(s1_pht_lookup_valid, false.B) && !io.s2_stride_hit
  val s2_pht_lookup = RegEnable(s1_pht_lookup, s1_pht_lookup_valid)

  io.s2_evict.valid := Mux(s2_real_update, s2_evict_valid && (s2_evict_entry.access_cnt > 1.U), s2_evict_valid)
  io.s2_evict.bits := s2_evict_entry
  io.s2_evict.bits.single_update := s2_update && (!s2_real_update)

  io.s2_pf_gen_req.bits.region_tag := s2_pf_gen_region_tag
  io.s2_pf_gen_req.bits.region_addr := s2_pf_gen_region_paddr
  io.s2_pf_gen_req.bits.alias_bits := s2_pf_gen_alias_bits
  io.s2_pf_gen_req.bits.region_bits := s2_pf_gen_region_bits
  io.s2_pf_gen_req.bits.paddr_valid := s2_paddr_valid
  io.s2_pf_gen_req.bits.decr_mode := s2_pf_gen_decr_mode
  io.s2_pf_gen_req.valid := false.B
  io.s2_pf_gen_req.bits.debug_source_type := HW_PREFETCH_AGT.U

  io.s2_pht_lookup.valid := s2_pht_lookup_valid
  io.s2_pht_lookup.bits := s2_pht_lookup

  XSPerfAccumulate("sms_agt_in", io.s0_lookup.valid)
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
  XSPerfAccumulate("sms_agt_evict", s2_evict_valid)
  XSPerfAccumulate("sms_agt_evict_by_plru", s2_evict_valid && !s2_do_dcache_evict)
  XSPerfAccumulate("sms_agt_evict_by_dcache", s2_evict_valid && s2_do_dcache_evict)
  XSPerfAccumulate("sms_agt_evict_one_hot_pattern", s2_evict_valid && (s2_evict_entry.access_cnt === 1.U))
}

class PhtLookup()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val pht_index = UInt(PHT_INDEX_BITS.W)
  val pht_tag = UInt(PHT_TAG_BITS.W)
  val region_paddr = UInt(REGION_ADDR_BITS.W)
  val region_vaddr = UInt(REGION_ADDR_BITS.W)
  val region_offset = UInt(REGION_OFFSET.W)
  val region_bit_single = UInt(REGION_BLKS.W)
}

class PhtEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val hist = Vec(2 * (REGION_BLKS - 1), UInt(PHT_HIST_BITS.W))
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
    singlePort = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))
  def PHT_SETS = smsParams.pht_size / smsParams.pht_ways
  // clockgated on pht_valids
  val pht_valids_reg = RegInit(VecInit(Seq.fill(smsParams.pht_ways){
    VecInit(Seq.fill(PHT_SETS){false.B})
  }))
  val pht_valids_enable = WireInit(VecInit(Seq.fill(PHT_SETS) {false.B}))
  val pht_valids_next = WireInit(pht_valids_reg)
  for(j <- 0 until PHT_SETS){
    when(pht_valids_enable(j)){
      (0 until smsParams.pht_ways).foreach(i => pht_valids_reg(i)(j) := pht_valids_next(i)(j))
    }
  }

  val replacement = Seq.fill(PHT_SETS) { ReplacementPolicy.fromString("plru", smsParams.pht_ways) }

  val lookup_queue = Module(new OverrideableQueue(new PhtLookup, smsParams.pht_lookup_queue_size))
  lookup_queue.io.in := io.s2_agt_lookup
  val lookup = lookup_queue.io.out

  val evict_queue = Module(new OverrideableQueue(new AGTEntry, smsParams.pht_lookup_queue_size))
  evict_queue.io.in := io.agt_update
  val evict = evict_queue.io.out

  XSPerfAccumulate("sms_pht_lookup_in", lookup_queue.io.in.fire)
  XSPerfAccumulate("sms_pht_lookup_out", lookup_queue.io.out.fire)
  XSPerfAccumulate("sms_pht_evict_in", evict_queue.io.in.fire)
  XSPerfAccumulate("sms_pht_evict_out", evict_queue.io.out.fire)

  val s3_ram_en = Wire(Bool())
  val s1_valid = Wire(Bool())
  // if s1.raddr == s2.waddr or s3 is using ram port, block s1
  val s1_wait = Wire(Bool())
  // pipe s0: select an op from [lookup, update], generate ram read addr
  val s0_valid = lookup.valid || evict.valid

  evict.ready := !s1_valid || !s1_wait
  lookup.ready := evict.ready && !evict.valid

  val s0_ram_raddr = Mux(evict.valid,
    evict.bits.pht_index,
    lookup.bits.pht_index
  )
  val s0_tag = Mux(evict.valid, evict.bits.pht_tag, lookup.bits.pht_tag)
  val s0_region_offset = Mux(evict.valid, evict.bits.region_offset, lookup.bits.region_offset)
  val s0_region_paddr = lookup.bits.region_paddr
  val s0_region_vaddr = lookup.bits.region_vaddr
  val s0_region_bits = evict.bits.region_bits
  val s0_decr_mode = evict.bits.decr_mode
  val s0_evict = evict.valid
  val s0_access_cnt_signal = evict.bits.access_cnt
  val s0_single_update = evict.bits.single_update
  val s0_has_been_single_update = evict.bits.has_been_signal_updated
  val s0_region_bit_single = evict.bits.region_bit_single

  // pipe s1: send addr to ram
  val s1_valid_r = RegInit(false.B)
  s1_valid_r := Mux(s1_valid && s1_wait, true.B, s0_valid)
  s1_valid := s1_valid_r
  val s1_reg_en = s0_valid && (!s1_wait || !s1_valid)
  val s1_ram_raddr = RegEnable(s0_ram_raddr, s1_reg_en)
  val s1_tag = RegEnable(s0_tag, s1_reg_en)
  val s1_access_cnt_signal = RegEnable(s0_access_cnt_signal, s1_reg_en)
  val s1_region_bits = RegEnable(s0_region_bits, s1_reg_en)
  val s1_decr_mode = RegEnable(s0_decr_mode, s1_reg_en)
  val s1_region_paddr = RegEnable(s0_region_paddr, s1_reg_en)
  val s1_region_vaddr = RegEnable(s0_region_vaddr, s1_reg_en)
  val s1_region_offset = RegEnable(s0_region_offset, s1_reg_en)
  val s1_single_update = RegEnable(s0_single_update, s1_reg_en)
  val s1_has_been_single_update = RegEnable(s0_has_been_single_update, s1_reg_en)
  val s1_region_bit_single = RegEnable(s0_region_bit_single, s1_reg_en)
  val s1_pht_valids = pht_valids_reg.map(way => Mux1H(
    (0 until PHT_SETS).map(i => i.U === s1_ram_raddr),
    way
  ))
  val s1_evict = RegEnable(s0_evict, s1_reg_en)
  val s1_replace_way = Mux1H(
    (0 until PHT_SETS).map(i => i.U === s1_ram_raddr),
    replacement.map(_.way)
  )
  val s1_hist_update_mask = Cat(
    Fill(REGION_BLKS - 1, true.B), 0.U((REGION_BLKS - 1).W)
  ) >> s1_region_offset
  val s1_hist_bits = Cat(
    s1_region_bits.head(REGION_BLKS - 1) >> s1_region_offset,
    (Cat(
      s1_region_bits.tail(1), 0.U((REGION_BLKS - 1).W)
    ) >> s1_region_offset)(REGION_BLKS - 2, 0)
  )
  val s1_hist_single_bit = Cat(
    s1_region_bit_single.head(REGION_BLKS - 1) >> s1_region_offset,
    (Cat(
      s1_region_bit_single.tail(1), 0.U((REGION_BLKS - 1).W)
    ) >> s1_region_offset)(REGION_BLKS - 2, 0)
  )

  // pipe s2: generate ram write addr/data
  val s2_valid = GatedValidRegNext(s1_valid && !s1_wait, false.B)
  val s2_reg_en = s1_valid && !s1_wait
  val s2_hist_update_mask = RegEnable(s1_hist_update_mask, s2_reg_en)
  val s2_single_update = RegEnable(s1_single_update, s2_reg_en)
  val s2_has_been_single_update = RegEnable(s1_has_been_single_update, s2_reg_en)
  val s2_hist_bits = RegEnable(s1_hist_bits, s2_reg_en)
  val s2_hist_bit_single = RegEnable(s1_hist_single_bit, s2_reg_en)
  val s2_tag = RegEnable(s1_tag, s2_reg_en)
  val s2_region_bits = RegEnable(s1_region_bits, s2_reg_en)
  val s2_decr_mode = RegEnable(s1_decr_mode, s2_reg_en)
  val s2_region_paddr = RegEnable(s1_region_paddr, s2_reg_en)
  val s2_region_vaddr = RegEnable(s1_region_vaddr, s2_reg_en)
  val s2_region_offset = RegEnable(s1_region_offset, s2_reg_en)
  val s2_region_offset_mask = region_offset_to_bits(s2_region_offset)
  val s2_evict = RegEnable(s1_evict, s2_reg_en)
  val s2_pht_valids = s1_pht_valids.map(v => RegEnable(v, s2_reg_en))
  val s2_replace_way = RegEnable(s1_replace_way, s2_reg_en)
  val s2_ram_waddr = RegEnable(s1_ram_raddr, s2_reg_en)
  val s2_ram_rdata = pht_ram.io.r.resp.data
  val s2_ram_rtags = s2_ram_rdata.map(_.tag)
  val s2_tag_match_vec = s2_ram_rtags.map(t => t === s2_tag)
  val s2_access_cnt_signal = RegEnable(s1_access_cnt_signal, s2_reg_en)
  val s2_hit_vec = s2_tag_match_vec.zip(s2_pht_valids).map({
    case (tag_match, v) => v && tag_match
  })

  //distinguish single update and evict update
  val s2_hist_update = s2_ram_rdata.map(way => VecInit(way.hist.zipWithIndex.map({
    case (h, i) =>
      val do_update = s2_hist_update_mask(i)
      val hist_updated = Mux(!s2_single_update,
                            Mux(s2_has_been_single_update,
                              Mux(s2_hist_bits(i), h, Mux(h === 0.U, 0.U, h - 1.U)), Mux(s2_hist_bits(i),Mux(h.andR, h, h + 1.U), Mux(h === 0.U, 0.U, h - 1.U))),
                                Mux(s2_hist_bit_single(i), Mux(h.andR, h, Mux(h===0.U, h+2.U, h+1.U)), h)
                             )
      Mux(do_update, hist_updated, h)
  })))


  val s2_hist_pf_gen = Mux1H(s2_hit_vec, s2_ram_rdata.map(way => VecInit(way.hist.map(_.head(1))).asUInt))
  val s2_new_hist = VecInit(s2_hist_bits.asBools.map(b => Cat(0.U((PHT_HIST_BITS - 1).W), b)))
  val s2_new_hist_single = VecInit(s2_hist_bit_single.asBools.map(b => Cat(0.U((PHT_HIST_BITS - 1).W), b)))
  val s2_new_hist_real = Mux(s2_single_update,s2_new_hist_single,s2_new_hist)
  val s2_pht_hit = Cat(s2_hit_vec).orR
  // update when valid bits over 4
  val signal_update_write = Mux(!s2_single_update, true.B, s2_pht_hit || s2_single_update && (s2_access_cnt_signal >4.U) )
  val s2_hist = Mux(s2_pht_hit, Mux1H(s2_hit_vec, s2_hist_update), s2_new_hist_real)
  val s2_repl_way_mask = UIntToOH(s2_replace_way)
  val s2_incr_region_vaddr = s2_region_vaddr + 1.U
  val s2_decr_region_vaddr = s2_region_vaddr - 1.U



  // pipe s3: send addr/data to ram, gen pf_req
  val s3_valid = GatedValidRegNext(s2_valid && signal_update_write, false.B)
  val s3_evict = RegEnable(s2_evict, s2_valid)
  val s3_hist = RegEnable(s2_hist, s2_valid)
  val s3_hist_pf_gen = RegEnable(s2_hist_pf_gen, s2_valid)

  val s3_hist_update_mask = RegEnable(s2_hist_update_mask.asUInt, s2_valid)

  val s3_region_offset = RegEnable(s2_region_offset, s2_valid)
  val s3_region_offset_mask = RegEnable(s2_region_offset_mask, s2_valid)
  val s3_decr_mode = RegEnable(s2_decr_mode, s2_valid)
  val s3_region_paddr = RegEnable(s2_region_paddr, s2_valid)
  val s3_region_vaddr = RegEnable(s2_region_vaddr, s2_valid)
  val s3_pht_tag = RegEnable(s2_tag, s2_valid)
  val s3_hit_vec = s2_hit_vec.map(h => RegEnable(h, s2_valid))
  val s3_hit = Cat(s3_hit_vec).orR
  val s3_hit_way = OHToUInt(s3_hit_vec)
  val s3_repl_way = RegEnable(s2_replace_way, s2_valid)
  val s3_repl_way_mask = RegEnable(s2_repl_way_mask, s2_valid)
  val s3_repl_update_mask = RegEnable(VecInit((0 until PHT_SETS).map(i => i.U === s2_ram_waddr)), s2_valid)
  val s3_ram_waddr = RegEnable(s2_ram_waddr, s2_valid)
  val s3_incr_region_vaddr = RegEnable(s2_incr_region_vaddr, s2_valid)
  val s3_decr_region_vaddr = RegEnable(s2_decr_region_vaddr, s2_valid)
  s3_ram_en := s3_valid && s3_evict
  val s3_ram_wdata = Wire(new PhtEntry())
  s3_ram_wdata.hist := s3_hist
  s3_ram_wdata.tag := s3_pht_tag
  s3_ram_wdata.decr_mode := s3_decr_mode

  s1_wait := (s2_valid && s2_evict && s2_ram_waddr === s1_ram_raddr) || s3_ram_en

  for((valids, way_idx) <- pht_valids_next.zipWithIndex){
    val update_way = s3_repl_way_mask(way_idx)
    for((v, set_idx) <- valids.zipWithIndex){
      val update_set = s3_repl_update_mask(set_idx)
      when(s3_valid && s3_evict && !s3_hit && update_set && update_way){
        pht_valids_enable(set_idx) := true.B
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
  when(s3_valid && s3_hit){
    assert(!Cat(s3_hit_vec).andR, "sms_pht: multi-hit!")
  }

  // generate pf req if hit
  val s3_hist_hi = s3_hist_pf_gen.head(REGION_BLKS - 1)
  val s3_hist_lo = s3_hist_pf_gen.tail(REGION_BLKS - 1)
  val s3_hist_hi_shifted = (Cat(0.U((REGION_BLKS - 1).W), s3_hist_hi) << s3_region_offset)(2 * (REGION_BLKS - 1) - 1, 0)
  val s3_hist_lo_shifted = (Cat(0.U((REGION_BLKS - 1).W), s3_hist_lo) << s3_region_offset)(2 * (REGION_BLKS - 1) - 1, 0)
  val s3_cur_region_bits = Cat(s3_hist_hi_shifted.tail(REGION_BLKS - 1), 0.U(1.W)) |
    Cat(0.U(1.W), s3_hist_lo_shifted.head(REGION_BLKS - 1))
  val s3_incr_region_bits = Cat(0.U(1.W), s3_hist_hi_shifted.head(REGION_BLKS - 1))
  val s3_decr_region_bits = Cat(s3_hist_lo_shifted.tail(REGION_BLKS - 1), 0.U(1.W))
  val s3_pf_gen_valid = s3_valid && s3_hit && !s3_evict
  val s3_cur_region_valid =  s3_pf_gen_valid && (s3_hist_pf_gen & s3_hist_update_mask).orR
  val s3_incr_region_valid = s3_pf_gen_valid && (s3_hist_hi & (~s3_hist_update_mask.head(REGION_BLKS - 1)).asUInt).orR
  val s3_decr_region_valid = s3_pf_gen_valid && (s3_hist_lo & (~s3_hist_update_mask.tail(REGION_BLKS - 1)).asUInt).orR
  val s3_incr_alias_bits = get_alias_bits(s3_incr_region_vaddr)
  val s3_decr_alias_bits = get_alias_bits(s3_decr_region_vaddr)
  val s3_incr_region_paddr = Cat(
    s3_region_paddr(REGION_ADDR_BITS - 1, REGION_ADDR_PAGE_BIT),
    s3_incr_region_vaddr(REGION_ADDR_PAGE_BIT - 1, 0)
  )
  val s3_decr_region_paddr = Cat(
    s3_region_paddr(REGION_ADDR_BITS - 1, REGION_ADDR_PAGE_BIT),
    s3_decr_region_vaddr(REGION_ADDR_PAGE_BIT - 1, 0)
  )
  val s3_incr_crosspage = s3_incr_region_vaddr(REGION_ADDR_PAGE_BIT) =/= s3_region_vaddr(REGION_ADDR_PAGE_BIT)
  val s3_decr_crosspage = s3_decr_region_vaddr(REGION_ADDR_PAGE_BIT) =/= s3_region_vaddr(REGION_ADDR_PAGE_BIT)
  val s3_cur_region_tag = region_hash_tag(s3_region_vaddr)
  val s3_incr_region_tag = region_hash_tag(s3_incr_region_vaddr)
  val s3_decr_region_tag = region_hash_tag(s3_decr_region_vaddr)

  val pf_gen_req_arb = Module(new Arbiter(new PfGenReq, 3))
  val s4_pf_gen_cur_region_valid = RegInit(false.B)
  val s4_pf_gen_cur_region = Reg(new PfGenReq)
  val s4_pf_gen_incr_region_valid = RegInit(false.B)
  val s4_pf_gen_incr_region = Reg(new PfGenReq)
  val s4_pf_gen_decr_region_valid = RegInit(false.B)
  val s4_pf_gen_decr_region = Reg(new PfGenReq)

  s4_pf_gen_cur_region_valid := s3_cur_region_valid
  when(s3_cur_region_valid){
    s4_pf_gen_cur_region.region_addr := s3_region_paddr
    s4_pf_gen_cur_region.alias_bits := get_alias_bits(s3_region_vaddr)
    s4_pf_gen_cur_region.region_tag := s3_cur_region_tag
    s4_pf_gen_cur_region.region_bits := s3_cur_region_bits
    s4_pf_gen_cur_region.paddr_valid := true.B
    s4_pf_gen_cur_region.decr_mode := false.B
  }
  s4_pf_gen_incr_region_valid := s3_incr_region_valid ||
    (!pf_gen_req_arb.io.in(1).ready && s4_pf_gen_incr_region_valid)
  when(s3_incr_region_valid){
    s4_pf_gen_incr_region.region_addr := Mux(s3_incr_crosspage, s3_incr_region_vaddr, s3_incr_region_paddr)
    s4_pf_gen_incr_region.alias_bits := s3_incr_alias_bits
    s4_pf_gen_incr_region.region_tag := s3_incr_region_tag
    s4_pf_gen_incr_region.region_bits := s3_incr_region_bits
    s4_pf_gen_incr_region.paddr_valid := !s3_incr_crosspage
    s4_pf_gen_incr_region.decr_mode := false.B
  }
  s4_pf_gen_decr_region_valid := s3_decr_region_valid ||
    (!pf_gen_req_arb.io.in(2).ready && s4_pf_gen_decr_region_valid)
  when(s3_decr_region_valid){
    s4_pf_gen_decr_region.region_addr := Mux(s3_decr_crosspage, s3_decr_region_vaddr, s3_decr_region_paddr)
    s4_pf_gen_decr_region.alias_bits := s3_decr_alias_bits
    s4_pf_gen_decr_region.region_tag := s3_decr_region_tag
    s4_pf_gen_decr_region.region_bits := s3_decr_region_bits
    s4_pf_gen_decr_region.paddr_valid := !s3_decr_crosspage
    s4_pf_gen_decr_region.decr_mode := true.B
  }

  pf_gen_req_arb.io.in.head.valid := s4_pf_gen_cur_region_valid
  pf_gen_req_arb.io.in.head.bits := s4_pf_gen_cur_region
  pf_gen_req_arb.io.in.head.bits.debug_source_type := HW_PREFETCH_PHT_CUR.U
  pf_gen_req_arb.io.in(1).valid := s4_pf_gen_incr_region_valid
  pf_gen_req_arb.io.in(1).bits := s4_pf_gen_incr_region
  pf_gen_req_arb.io.in(1).bits.debug_source_type := HW_PREFETCH_PHT_INC.U
  pf_gen_req_arb.io.in(2).valid := s4_pf_gen_decr_region_valid
  pf_gen_req_arb.io.in(2).bits := s4_pf_gen_decr_region
  pf_gen_req_arb.io.in(2).bits.debug_source_type := HW_PREFETCH_PHT_DEC.U
  pf_gen_req_arb.io.out.ready := true.B

  io.pf_gen_req.valid := pf_gen_req_arb.io.out.valid
  io.pf_gen_req.bits := pf_gen_req_arb.io.out.bits

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
  XSPerfAccumulate(s"sms_pht_pf_gen", io.pf_gen_req.valid)
}

class PrefetchFilterEntry()(implicit p: Parameters) extends XSBundle with HasSMSModuleHelper {
  val region_tag = UInt(REGION_TAG_WIDTH.W)
  val region_addr = UInt(REGION_ADDR_BITS.W)
  val region_bits = UInt(REGION_BLKS.W)
  val filter_bits = UInt(REGION_BLKS.W)
  val alias_bits = UInt(2.W)
  val paddr_valid = Bool()
  val decr_mode = Bool()
  val debug_source_type = UInt(log2Up(nSourceType).W)
}

class PrefetchFilter()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper {
  val io = IO(new Bundle() {
    val gen_req = Flipped(ValidIO(new PfGenReq()))
    val tlb_req = new TlbRequestIO(2)
    val pmp_resp = Flipped(new PMPRespBundle())
    val l2_pf_addr = ValidIO(UInt(PAddrBits.W))
    val pf_alias_bits = Output(UInt(2.W))
    val debug_source_type = Output(UInt(log2Up(nSourceType).W))
  })
  val entries = Seq.fill(smsParams.pf_filter_size){ Reg(new PrefetchFilterEntry()) }
  val valids = Seq.fill(smsParams.pf_filter_size){ RegInit(false.B) }
  val replacement = ReplacementPolicy.fromString("plru", smsParams.pf_filter_size)

  val prev_valid = GatedValidRegNext(io.gen_req.valid, false.B)
  val prev_gen_req = RegEnable(io.gen_req.bits, io.gen_req.valid)

  val tlb_req_arb = Module(new RRArbiterInit(new TlbReq, smsParams.pf_filter_size))
  val pf_req_arb = Module(new RRArbiterInit(UInt(PAddrBits.W), smsParams.pf_filter_size))

  io.l2_pf_addr.valid := pf_req_arb.io.out.valid
  io.l2_pf_addr.bits := pf_req_arb.io.out.bits
  io.pf_alias_bits := Mux1H(entries.zipWithIndex.map({
    case (entry, i) => (i.U === pf_req_arb.io.chosen) -> entry.alias_bits
  }))
  pf_req_arb.io.out.ready := true.B

  io.debug_source_type := VecInit(entries.map(_.debug_source_type))(pf_req_arb.io.chosen)

  val s1_valid = Wire(Bool())
  val s1_hit = Wire(Bool())
  val s1_replace_vec = Wire(UInt(smsParams.pf_filter_size.W))
  val s1_tlb_fire_vec = Wire(UInt(smsParams.pf_filter_size.W))
  val s2_tlb_fire_vec = Wire(UInt(smsParams.pf_filter_size.W))
  val s3_tlb_fire_vec = Wire(UInt(smsParams.pf_filter_size.W))
  val not_tlbing_vec = VecInit((0 until smsParams.pf_filter_size).map{case i =>
    !s1_tlb_fire_vec(i) && !s2_tlb_fire_vec(i) && !s3_tlb_fire_vec(i)
  })

  // s0: entries lookup
  val s0_gen_req = io.gen_req.bits
  val s0_match_prev = prev_valid && (s0_gen_req.region_tag === prev_gen_req.region_tag)
  val s0_gen_req_valid = io.gen_req.valid && !s0_match_prev
  val s0_match_vec = valids.indices.map(i => {
    valids(i) && entries(i).region_tag === s0_gen_req.region_tag && !(s1_valid && !s1_hit && s1_replace_vec(i))
  })
  val s0_any_matched = Cat(s0_match_vec).orR
  val s0_replace_vec = UIntToOH(replacement.way)
  val s0_hit = s0_gen_req_valid && s0_any_matched

  for(((v, ent), i) <- valids.zip(entries).zipWithIndex){
    val is_evicted = s1_valid && s1_replace_vec(i)
    tlb_req_arb.io.in(i).valid := v && not_tlbing_vec(i) && !ent.paddr_valid && !is_evicted
    tlb_req_arb.io.in(i).bits.vaddr := Cat(ent.region_addr, 0.U(log2Up(REGION_SIZE).W))
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
    tlb_req_arb.io.in(i).bits.pmp_addr := DontCare

    val pending_req_vec = ent.region_bits & (~ent.filter_bits).asUInt
    val first_one_offset = PriorityMux(
      pending_req_vec.asBools,
      (0 until smsParams.pf_filter_size).map(_.U(REGION_OFFSET.W))
    )
    val last_one_offset = PriorityMux(
      pending_req_vec.asBools.reverse,
      (0 until smsParams.pf_filter_size).reverse.map(_.U(REGION_OFFSET.W))
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

  val s0_update_way = OHToUInt(s0_match_vec)
  val s0_replace_way = replacement.way
  val s0_access_way = Mux(s0_any_matched, s0_update_way, s0_replace_way)
  when(s0_gen_req_valid){
    replacement.access(s0_access_way)
  }

  // s1: update or alloc
  val s1_valid_r = GatedValidRegNext(s0_gen_req_valid, false.B)
  val s1_hit_r = RegEnable(s0_hit, false.B, s0_gen_req_valid)
  val s1_gen_req = RegEnable(s0_gen_req, s0_gen_req_valid)
  val s1_replace_vec_r = RegEnable(s0_replace_vec, s0_gen_req_valid && !s0_hit)
  val s1_update_vec = RegEnable(VecInit(s0_match_vec).asUInt, s0_gen_req_valid && s0_hit)
  val s1_tlb_fire_vec_r = GatedValidRegNext(s0_tlb_fire_vec)
  // tlb req will latch one cycle after tlb_arb
  val s1_tlb_req_valid = GatedValidRegNext(tlb_req_arb.io.out.fire)
  val s1_tlb_req_bits  = RegEnable(tlb_req_arb.io.out.bits, tlb_req_arb.io.out.fire)
  val s1_alloc_entry = Wire(new PrefetchFilterEntry())
  s1_valid := s1_valid_r
  s1_hit := s1_hit_r
  s1_replace_vec := s1_replace_vec_r
  s1_tlb_fire_vec := s1_tlb_fire_vec_r.asUInt
  s1_alloc_entry.region_tag := s1_gen_req.region_tag
  s1_alloc_entry.region_addr := s1_gen_req.region_addr
  s1_alloc_entry.region_bits := s1_gen_req.region_bits
  s1_alloc_entry.paddr_valid := s1_gen_req.paddr_valid
  s1_alloc_entry.decr_mode := s1_gen_req.decr_mode
  s1_alloc_entry.filter_bits := 0.U
  s1_alloc_entry.alias_bits := s1_gen_req.alias_bits
  s1_alloc_entry.debug_source_type := s1_gen_req.debug_source_type
  io.tlb_req.req.valid := s1_tlb_req_valid && !((s1_tlb_fire_vec & s1_replace_vec).orR && s1_valid && !s1_hit)
  io.tlb_req.req.bits := s1_tlb_req_bits
  io.tlb_req.resp.ready := true.B
  io.tlb_req.req_kill := false.B
  tlb_req_arb.io.out.ready := true.B

  // s2: get response from tlb
  val s2_tlb_fire_vec_r = GatedValidRegNext(s1_tlb_fire_vec_r)
  s2_tlb_fire_vec := s2_tlb_fire_vec_r.asUInt

  // s3: get pmp response form PMPChecker
  val s3_tlb_fire_vec_r = GatedValidRegNext(s2_tlb_fire_vec_r)
  val s3_tlb_resp_fire = RegNext(io.tlb_req.resp.fire)
  val s3_tlb_resp = RegEnable(io.tlb_req.resp.bits, io.tlb_req.resp.valid)
  val s3_pmp_resp = io.pmp_resp
  val s3_update_valid = s3_tlb_resp_fire && !s3_tlb_resp.miss
  val s3_drop = s3_update_valid && (
    // page/access fault
    s3_tlb_resp.excp.head.pf.ld || s3_tlb_resp.excp.head.gpf.ld || s3_tlb_resp.excp.head.af.ld ||
    // uncache
    s3_pmp_resp.mmio || Pbmt.isUncache(s3_tlb_resp.pbmt.head) ||
    // pmp access fault
    s3_pmp_resp.ld
  )
  s3_tlb_fire_vec := s3_tlb_fire_vec_r.asUInt

  for(((v, ent), i) <- valids.zip(entries).zipWithIndex){
    val alloc = s1_valid && !s1_hit && s1_replace_vec(i)
    val update = s1_valid && s1_hit && s1_update_vec(i)
    // for pf: use s0 data
    val pf_fired = s0_pf_fire_vec(i)
    val tlb_fired = s3_tlb_fire_vec(i) && s3_update_valid
    when(tlb_fired){
      when(s3_drop){
        v := false.B
      }.otherwise{
        ent.paddr_valid := !s3_tlb_resp.miss
        ent.region_addr := region_addr(s3_tlb_resp.paddr.head)
      }
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
  when(s1_valid && s1_hit){
    assert(PopCount(s1_update_vec) === 1.U, "sms_pf_filter: multi-hit")
  }
  assert(!io.tlb_req.resp.fire || Cat(s2_tlb_fire_vec).orR, "sms_pf_filter: tlb resp fires, but no tlb req from tlb_req_arb 2 cycles ago")

  XSPerfAccumulate("sms_pf_filter_recv_req", io.gen_req.valid)
  XSPerfAccumulate("sms_pf_filter_hit", s1_valid && s1_hit)
  XSPerfAccumulate("sms_pf_filter_tlb_req", io.tlb_req.req.fire)
  XSPerfAccumulate("sms_pf_filter_tlb_resp_miss", io.tlb_req.resp.fire && io.tlb_req.resp.bits.miss)
  XSPerfAccumulate("sms_pf_filter_tlb_resp_drop", s3_drop)
  XSPerfAccumulate("sms_pf_filter_tlb_resp_drop_by_pf_or_af",
    s3_update_valid && (s3_tlb_resp.excp.head.pf.ld || s3_tlb_resp.excp.head.gpf.ld || s3_tlb_resp.excp.head.af.ld)
  )
  XSPerfAccumulate("sms_pf_filter_tlb_resp_drop_by_uncache",
    s3_update_valid && (s3_pmp_resp.mmio || Pbmt.isUncache(s3_tlb_resp.pbmt.head))
  )
  XSPerfAccumulate("sms_pf_filter_tlb_resp_drop_by_pmp_af",
    s3_update_valid && (s3_pmp_resp.ld)
  )
  for(i <- 0 until smsParams.pf_filter_size){
    XSPerfAccumulate(s"sms_pf_filter_access_way_$i", s0_gen_req_valid && s0_access_way === i.U)
  }
  XSPerfAccumulate("sms_pf_filter_l2_req", io.l2_pf_addr.valid)
}

class SMSTrainFilter()(implicit p: Parameters) extends XSModule with HasSMSModuleHelper with HasTrainFilterHelper {
  val io = IO(new Bundle() {
    // train input
    // hybrid load store
    val ld_in = Flipped(Vec(backendParams.LdExuCnt, ValidIO(new LsPrefetchTrainBundle())))
    val st_in = Flipped(Vec(backendParams.StaExuCnt, ValidIO(new LsPrefetchTrainBundle())))
    // filter out
    val train_req = ValidIO(new PrefetchReqBundle())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr](
    p => smsParams.train_filter_size
  ){
  }

  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(smsParams.train_filter_size){ (0.U.asTypeOf(new PrefetchReqBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(smsParams.train_filter_size){ (false.B) }))

  val enqLen = backendParams.LduCnt + backendParams.StaCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))

  val deqPtr = WireInit(deqPtrExt.value)

  require(smsParams.train_filter_size >= enqLen)

  val ld_reorder = reorder(io.ld_in)
  val st_reorder = reorder(io.st_in)
  val reqs_ls = ld_reorder.map(_.bits.toPrefetchReqBundle()) ++ st_reorder.map(_.bits.toPrefetchReqBundle())
  val reqs_vls = ld_reorder.map(_.valid) ++ st_reorder.map(_.valid)
  val needAlloc = Wire(Vec(enqLen, Bool()))
  val canAlloc = Wire(Vec(enqLen, Bool()))

  for(i <- (0 until enqLen)) {
    val req = reqs_ls(i)
    val req_v = reqs_vls(i)
    val index = PopCount(needAlloc.take(i))
    val allocPtr = enqPtrExt(index)
    val entry_match = Cat(entries.zip(valids).map {
      case(e, v) => v && block_hash_tag(e.vaddr) === block_hash_tag(req.vaddr)
    }).orR
    val prev_enq_match = if(i == 0) false.B else Cat(reqs_ls.zip(reqs_vls).take(i).map {
      case(pre, pre_v) => pre_v && block_hash_tag(pre.vaddr) === block_hash_tag(req.vaddr)
    }).orR

    needAlloc(i) := req_v && !entry_match && !prev_enq_match
    canAlloc(i) := needAlloc(i) && allocPtr >= deqPtrExt

    when(canAlloc(i)) {
      valids(allocPtr.value) := true.B
      entries(allocPtr.value) := req
    }
  }
  val allocNum = PopCount(canAlloc)

  enqPtrExt.foreach{case x => when(canAlloc.asUInt.orR) {x := x + allocNum} }

  io.train_req.valid := false.B
  io.train_req.bits := DontCare
  valids.zip(entries).zipWithIndex.foreach {
    case((valid, entry), i) => {
      when(deqPtr === i.U) {
        io.train_req.valid := valid
        io.train_req.bits := entry
      }
    }
  }

  when(io.train_req.valid) {
    valids(deqPtr) := false.B
    deqPtrExt := deqPtrExt + 1.U
  }

  XSPerfAccumulate("sms_train_filter_full", PopCount(valids) === (smsParams.train_filter_size).U)
  XSPerfAccumulate("sms_train_filter_half", PopCount(valids) >= (smsParams.train_filter_size / 2).U)
  XSPerfAccumulate("sms_train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqs_vls)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate("sms_train_filter_enq", allocNum > 0.U)
  XSPerfAccumulate("sms_train_filter_deq", io.train_req.fire)
  def toBinary(n: Int): String = n match {
    case 0|1 => s"$n"
    case _   => s"${toBinary(n/2)}${n%2}"
  }
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"sms_train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"sms_train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"sms_train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
}

class SMSPrefetcher()(implicit p: Parameters) extends BasePrefecher with HasSMSModuleHelper with HasL1PrefetchSourceParameter {
  import freechips.rocketchip.util._

  val io_agt_en = IO(Input(Bool()))
  val io_stride_en = IO(Input(Bool()))
  val io_pht_en = IO(Input(Bool()))
  val io_act_threshold = IO(Input(UInt(REGION_OFFSET.W)))
  val io_act_stride = IO(Input(UInt(6.W)))
  val io_dcache_evict = IO(Flipped(DecoupledIO(new AGTEvictReq)))

  val train_filter = Module(new SMSTrainFilter)

  train_filter.io.ld_in <> io.ld_in
  train_filter.io.st_in <> io.st_in

  val train_ld = train_filter.io.train_req.bits

  val train_block_tag = block_hash_tag(train_ld.vaddr)
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
  val train_vld = train_filter.io.train_req.valid


  // prefetch stage0
  val active_gen_table = Module(new ActiveGenerationTable())
  val stride = Module(new StridePF())
  val pht = Module(new PatternHistoryTable())
  val pf_filter = Module(new PrefetchFilter())

  val train_vld_s0 = GatedValidRegNext(train_vld, false.B)
  val train_s0 = RegEnable(train_ld, train_vld)
  val train_region_tag_s0 = RegEnable(train_region_tag, train_vld)
  val train_region_p1_tag_s0 = RegEnable(train_region_p1_tag, train_vld)
  val train_region_m1_tag_s0 = RegEnable(train_region_m1_tag, train_vld)
  val train_allow_cross_region_p1_s0 = RegEnable(train_allow_cross_region_p1, train_vld)
  val train_allow_cross_region_m1_s0 = RegEnable(train_allow_cross_region_m1, train_vld)
  val train_pht_tag_s0 = RegEnable(pht_tag(train_ld.pc), train_vld)
  val train_pht_index_s0 = RegEnable(pht_index(train_ld.pc), train_vld)
  val train_region_offset_s0 = RegEnable(train_region_offset, train_vld)
  val train_region_p1_cross_page_s0 = RegEnable(train_region_p1_cross_page, train_vld)
  val train_region_m1_cross_page_s0 = RegEnable(train_region_m1_cross_page, train_vld)
  val train_region_paddr_s0 = RegEnable(train_region_paddr, train_vld)
  val train_region_vaddr_s0 = RegEnable(train_region_vaddr, train_vld)

  active_gen_table.io.agt_en := io_agt_en
  active_gen_table.io.act_threshold := io_act_threshold
  active_gen_table.io.act_stride := io_act_stride
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
  active_gen_table.io.s2_stride_hit := stride.io.s2_gen_req.valid
  active_gen_table.io.s0_dcache_evict <> io_dcache_evict

  stride.io.stride_en := io_stride_en
  stride.io.s0_lookup.valid := train_vld_s0
  stride.io.s0_lookup.bits.pc := train_s0.pc(STRIDE_PC_BITS - 1, 0)
  stride.io.s0_lookup.bits.vaddr := Cat(
    train_region_vaddr_s0, train_region_offset_s0, 0.U(log2Up(dcacheParameters.blockBytes).W)
  )
  stride.io.s0_lookup.bits.paddr := Cat(
    train_region_paddr_s0, train_region_offset_s0, 0.U(log2Up(dcacheParameters.blockBytes).W)
  )
  stride.io.s1_valid := active_gen_table.io.s1_sel_stride

  pht.io.s2_agt_lookup := active_gen_table.io.s2_pht_lookup
  pht.io.agt_update := active_gen_table.io.s2_evict

  val pht_gen_valid = pht.io.pf_gen_req.valid && io_pht_en
  val agt_gen_valid = active_gen_table.io.s2_pf_gen_req.valid
  val stride_gen_valid = stride.io.s2_gen_req.valid
  val pf_gen_req = Mux(agt_gen_valid || stride_gen_valid,
    Mux1H(Seq(
      agt_gen_valid -> active_gen_table.io.s2_pf_gen_req.bits,
      stride_gen_valid -> stride.io.s2_gen_req.bits
    )),
    pht.io.pf_gen_req.bits
  )
  assert(!(agt_gen_valid && stride_gen_valid))
  pf_filter.io.gen_req.valid := pht_gen_valid || agt_gen_valid || stride_gen_valid
  pf_filter.io.gen_req.bits := pf_gen_req
  io.tlb_req <> pf_filter.io.tlb_req
  pf_filter.io.pmp_resp := io.pmp_resp
  val is_valid_address = PmemRanges.map(_.cover(pf_filter.io.l2_pf_addr.bits)).reduce(_ || _)

  io.l2_req.valid := pf_filter.io.l2_pf_addr.valid && io.enable && is_valid_address
  io.l2_req.bits.addr := pf_filter.io.l2_pf_addr.bits
  io.l2_req.bits.source := MemReqSource.Prefetch2L2SMS.id.U

  // for now, sms will not send l1 prefetch requests
  io.l1_req.bits.paddr := pf_filter.io.l2_pf_addr.bits
  io.l1_req.bits.alias := pf_filter.io.pf_alias_bits
  io.l1_req.bits.is_store := true.B
  io.l1_req.bits.confidence := 1.U
  io.l1_req.bits.pf_source.value := L1_HW_PREFETCH_NULL
  io.l1_req.valid := false.B

  for((train, i) <- io.ld_in.zipWithIndex){
    XSPerfAccumulate(s"pf_train_miss_${i}", train.valid && train.bits.miss)
    XSPerfAccumulate(s"pf_train_prefetched_${i}", train.valid && isFromL1Prefetch(train.bits.meta_prefetch))
  }
  val trace = Wire(new L1MissTrace)
  trace.vaddr := 0.U
  trace.pc := 0.U
  trace.paddr := io.l2_req.bits.addr
  trace.source := pf_filter.io.debug_source_type
  val table = ChiselDB.createTable("L1SMSMissTrace_hart"+ p(XSCoreParamsKey).HartId.toString, new L1MissTrace)
  table.log(trace, io.l2_req.fire, "SMSPrefetcher", clock, reset)

  XSPerfAccumulate("sms_pf_gen_conflict",
    pht_gen_valid && agt_gen_valid
  )
  XSPerfAccumulate("sms_pht_disabled", pht.io.pf_gen_req.valid && !io_pht_en)
  XSPerfAccumulate("sms_agt_disabled", active_gen_table.io.s2_pf_gen_req.valid && !io_agt_en)
  XSPerfAccumulate("sms_pf_real_issued", io.l2_req.valid)
  XSPerfAccumulate("sms_l1_req_valid", io.l1_req.valid)
  XSPerfAccumulate("sms_l1_req_fire", io.l1_req.fire)
}
