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
import scala.collection.SeqLike

trait HasStridePrefetchHelper extends HasL1PrefetchHelper {
  val STRIDE_FILTER_SIZE = 6
  val STRIDE_ENTRY_NUM = 10
  val STRIDE_BITS = 10 + BLOCK_OFFSET
  val STRIDE_VADDR_BITS = 10 + BLOCK_OFFSET
  val STRIDE_CONF_BITS = 2

  // detail control
  val ALWAYS_UPDATE_PRE_VADDR = 1 // 1 for true, 0 for false
  val AGGRESIVE_POLICY = false // if true, prefetch degree is greater than 1, 1 otherwise
  val STRIDE_LOOK_AHEAD_BLOCKS = 2 // aggressive degree
  val LOOK_UP_STREAM = false // if true, avoid collision with stream

  val STRIDE_WIDTH_BLOCKS = if(AGGRESIVE_POLICY) STRIDE_LOOK_AHEAD_BLOCKS else 1

  def MAX_CONF = (1 << STRIDE_CONF_BITS) - 1
}

class StrideMetaBundle(implicit p: Parameters) extends XSBundle with HasStridePrefetchHelper {
  val pre_vaddr = UInt(STRIDE_VADDR_BITS.W)
  val stride = UInt(STRIDE_BITS.W)
  val confidence = UInt(STRIDE_CONF_BITS.W)
  val hash_pc = UInt(HASH_TAG_WIDTH.W)

  def reset(index: Int) = {
    pre_vaddr := 0.U
    stride := 0.U
    confidence := 0.U
    hash_pc := index.U
  }

  def alloc(vaddr: UInt, alloc_hash_pc: UInt) = {
    pre_vaddr := vaddr(STRIDE_VADDR_BITS - 1, 0)
    stride := 0.U
    confidence := 0.U
    hash_pc := alloc_hash_pc
  }

  def update(vaddr: UInt, always_update_pre_vaddr: Bool) = {
    val new_vaddr = vaddr(STRIDE_VADDR_BITS - 1, 0)
    val new_stride = new_vaddr - pre_vaddr
    val new_stride_blk = block_addr(new_stride)
    // NOTE: for now, disable negtive stride
    val stride_valid = new_stride_blk =/= 0.U && new_stride_blk =/= 1.U && new_stride(STRIDE_VADDR_BITS - 1) === 0.U
    val stride_match = new_stride === stride
    val low_confidence = confidence <= 1.U
    val can_send_pf = stride_valid && stride_match && confidence === MAX_CONF.U

    when(stride_valid) {
      when(stride_match) {
        confidence := Mux(confidence === MAX_CONF.U, confidence, confidence + 1.U)
      }.otherwise {
        confidence := Mux(confidence === 0.U, confidence, confidence - 1.U)
        when(low_confidence) {
          stride := new_stride
        }
      }
      pre_vaddr := new_vaddr
    }
    when(always_update_pre_vaddr) {
      pre_vaddr := new_vaddr
    }

    (can_send_pf, new_stride)
  }

}

class StrideMetaArray(implicit p: Parameters) extends XSModule with HasStridePrefetchHelper {
  val io = IO(new XSBundle {
    val enable = Input(Bool())
    // TODO: flush all entry when process changing happens, or disable stream prefetch for a while
    val flush = Input(Bool())
    val dynamic_depth = Input(UInt(32.W)) // TODO: enable dynamic stride depth
    val train_req = Flipped(DecoupledIO(new PrefetchReqBundle))
    val prefetch_req = ValidIO(new StreamPrefetchReqBundle)
    // query Stream component to see if a stream pattern has already been detected
    val stream_lookup_req  = ValidIO(new PrefetchReqBundle)
    val stream_lookup_resp = Input(Bool())
  })

  val array = Reg(Vec(STRIDE_ENTRY_NUM, new StrideMetaBundle))
  val replacement = ReplacementPolicy.fromString("plru", STRIDE_ENTRY_NUM)

  // s0: hash pc -> cam all entries
  val s0_can_accept = Wire(Bool())
  val s0_valid = io.train_req.fire
  val s0_vaddr = io.train_req.bits.vaddr
  val s0_pc = io.train_req.bits.pc
  val s0_pc_hash = pc_hash_tag(s0_pc)
  val s0_pc_match_vec = VecInit(array.map(_.hash_pc === s0_pc_hash)).asUInt
  val s0_hit = s0_pc_match_vec.orR
  val s0_index = Mux(s0_hit, OHToUInt(s0_pc_match_vec), replacement.way)
  io.train_req.ready := s0_can_accept
  io.stream_lookup_req.valid := s0_valid
  io.stream_lookup_req.bits  := io.train_req.bits

  when(s0_valid) {
    replacement.access(s0_index)
  }

  assert(PopCount(s0_pc_match_vec) <= 1.U)
  XSPerfAccumulate("s0_valid", s0_valid)
  XSPerfAccumulate("s0_hit", s0_valid && s0_hit)
  XSPerfAccumulate("s0_miss", s0_valid && !s0_hit)

  // s1: alloc or update
  val s1_valid = RegNext(s0_valid)
  val s1_index = RegEnable(s0_index, s0_valid)
  val s1_pc_hash = RegEnable(s0_pc_hash, s0_valid)
  val s1_vaddr = RegEnable(s0_vaddr, s0_valid)
  val s1_hit = RegEnable(s0_hit, s0_valid)
  val s1_alloc = s1_valid && !s1_hit
  val s1_update = s1_valid && s1_hit
  val s1_stride = array(s1_index).stride
  val s1_new_stride = WireInit(0.U(STRIDE_BITS.W))
  val s1_can_send_pf = WireInit(false.B)
  s0_can_accept := !(s1_valid && s1_pc_hash === s0_pc_hash)

  val always_update = WireInit(Constantin.createRecord("always_update" + p(XSCoreParamsKey).HartId.toString, initValue = ALWAYS_UPDATE_PRE_VADDR.U)) === 1.U

  when(s1_alloc) {
    array(s1_index).alloc(
      vaddr = s1_vaddr,
      alloc_hash_pc = s1_pc_hash
    )
  }.elsewhen(s1_update) {
    val res = array(s1_index).update(s1_vaddr, always_update)
    s1_can_send_pf := res._1
    s1_new_stride := res._2
  }

  val l1_stride_ratio_const = WireInit(Constantin.createRecord("l1_stride_ratio" + p(XSCoreParamsKey).HartId.toString, initValue = 2.U))
  val l1_stride_ratio = l1_stride_ratio_const(3, 0)
  val l2_stride_ratio_const = WireInit(Constantin.createRecord("l2_stride_ratio" + p(XSCoreParamsKey).HartId.toString, initValue = 5.U))
  val l2_stride_ratio = l2_stride_ratio_const(3, 0)
  // s2: calculate L1 & L2 pf addr
  val s2_valid = RegNext(s1_valid && s1_can_send_pf)
  val s2_vaddr = RegEnable(s1_vaddr, s1_valid && s1_can_send_pf)
  val s2_stride = RegEnable(s1_stride, s1_valid && s1_can_send_pf)
  val s2_l1_depth = s2_stride << l1_stride_ratio
  val s2_l1_pf_vaddr = (s2_vaddr + s2_l1_depth)(VAddrBits - 1, 0)
  val s2_l2_depth = s2_stride << l2_stride_ratio
  val s2_l2_pf_vaddr = (s2_vaddr + s2_l2_depth)(VAddrBits - 1, 0)
  val s2_l1_pf_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
    vaddr = s2_l1_pf_vaddr,
    width = STRIDE_WIDTH_BLOCKS,
    decr_mode = false.B,
    sink = SINK_L1,
    source = L1_HW_PREFETCH_STRIDE)
  val s2_l2_pf_req_bits = (new StreamPrefetchReqBundle).getStreamPrefetchReqBundle(
    vaddr = s2_l2_pf_vaddr,
    width = STRIDE_WIDTH_BLOCKS,
    decr_mode = false.B,
    sink = SINK_L2,
    source = L1_HW_PREFETCH_STRIDE)

  // s3: send l1 pf out
  val s3_valid = if (LOOK_UP_STREAM) RegNext(s2_valid) && !io.stream_lookup_resp else RegNext(s2_valid)
  val s3_l1_pf_req_bits = RegEnable(s2_l1_pf_req_bits, s2_valid)
  val s3_l2_pf_req_bits = RegEnable(s2_l2_pf_req_bits, s2_valid)

  // s4: send l2 pf out
  val s4_valid = RegNext(s3_valid)
  val s4_l2_pf_req_bits = RegEnable(s3_l2_pf_req_bits, s3_valid)

  // l2 has higher priority than l1 ?
  io.prefetch_req.valid := s3_valid || s4_valid
  io.prefetch_req.bits := Mux(s4_valid, s4_l2_pf_req_bits, s3_l1_pf_req_bits)

  XSPerfAccumulate("pf_valid", io.prefetch_req.valid)
  XSPerfAccumulate("l1_pf_valid", s3_valid && !s4_valid)
  XSPerfAccumulate("l1_pf_block", s3_valid && s4_valid)
  XSPerfAccumulate("l2_pf_valid", s4_valid)
  XSPerfAccumulate("detect_stream", io.stream_lookup_resp)
  XSPerfHistogram("high_conf_num", PopCount(VecInit(array.map(_.confidence === MAX_CONF.U))).asUInt, true.B, 0, STRIDE_ENTRY_NUM, 1)
  for(i <- 0 until STRIDE_ENTRY_NUM) {
    XSPerfAccumulate(s"entry_${i}_update", i.U === s1_index && s1_update)
    for(j <- 0 until 4) {
      XSPerfAccumulate(s"entry_${i}_disturb_${j}", i.U === s1_index && s1_update &&
                                                   j.U === s1_new_stride &&
                                                   array(s1_index).confidence === MAX_CONF.U &&
                                                   array(s1_index).stride =/= s1_new_stride
      )
    }
  }

  for(i <- 0 until STRIDE_ENTRY_NUM) {
    when(reset.asBool || RegNext(io.flush)) {
      array(i).reset(i)
    }
  }
}