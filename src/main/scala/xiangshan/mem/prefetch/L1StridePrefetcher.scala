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
import scala.collection.SeqLike

trait HasStridePrefetchHelper extends HasStreamPrefetchHelper {
  val STRIDE_FILTER_SIZE = 6
  val STRIDE_ENTRY_NUM = 10
  val STRIDE_BITS = 10
  val STRIDE_VADDR_BITS = 10
  val STRIDE_CONF_BITS = 2

  // detail control
  val ALWAYS_UPDATE_PRE_VADDR = 1 // 1 for true, 0 for false
  val AGGRESIVE_POLICY = false // if true, prefetch degree is greater than 1, 1 otherwise
  val STRIDE_LOOK_AHEAD_BLOCKS = 2 // aggressive degree
  val LOOK_UP_STREAM = false // if true, avoid collision with stream

  val STRIDE_WIDTH_BLOCKS = if(AGGRESIVE_POLICY) STRIDE_LOOK_AHEAD_BLOCKS else 1
  
  def MAX_CONF = (1 << STRIDE_CONF_BITS) - 1
}

// get prefetch train reqs from `exuParameters.LduCnt` load pipelines (up to `exuParameters.LduCnt`/cycle)
// filter by cache line address, send out train req to stride (up to 1 req/cycle)
class StrideTrainFilter()(implicit p: Parameters) extends XSModule with HasStridePrefetchHelper {
  val io = IO(new Bundle() {
    val enable = Input(Bool())
    val flush = Input(Bool())
    // train input, only from load for now
    val ld_in = Flipped(Vec(exuParameters.LduCnt, ValidIO(new LdPrefetchTrainBundle())))
    // filter out
    val train_req = DecoupledIO(new PrefetchReqBundle())
  })

  class Ptr(implicit p: Parameters) extends CircularQueuePtr[Ptr]( p => STRIDE_FILTER_SIZE ){}
  object Ptr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): Ptr = {
      val ptr = Wire(new Ptr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val entries = RegInit(VecInit(Seq.fill(STRIDE_FILTER_SIZE){ (0.U.asTypeOf(new PrefetchReqBundle())) }))
  val valids = RegInit(VecInit(Seq.fill(STRIDE_FILTER_SIZE){ (false.B) }))

  // enq
  val enqLen = exuParameters.LduCnt
  val enqPtrExt = RegInit(VecInit((0 until enqLen).map(_.U.asTypeOf(new Ptr))))
  val deqPtrExt = RegInit(0.U.asTypeOf(new Ptr))
  
  val deqPtr = WireInit(deqPtrExt.value)

  require(STRIDE_FILTER_SIZE >= enqLen)

  def reorder(source: Vec[ValidIO[LdPrefetchTrainBundle]]): Vec[ValidIO[LdPrefetchTrainBundle]] = {
    if(source.length == 1) {
      source
    }else if(source.length == 2) {
      val source_v = source.map(_.valid)
      val res = Wire(source.cloneType)
      when(PopCount(source_v) === 2.U) {
        // source 1 is older than source 0
        val source_1_older = isBefore(source(1).bits.uop.lqIdx, source(0).bits.uop.lqIdx)
        when(source_1_older) {
          res(0) := source(1)
          res(1) := source(0)
        }.otherwise {
          res := source
        }
      }.otherwise {
        res := source
      }

      res
    }else if(source.length == 3){
      val res_0_1 = Wire(source.cloneType)
      val res_1_2 = Wire(source.cloneType)
      val res = Wire(source.cloneType)

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

  XSPerfAccumulate("stride_train_filter_full", PopCount(valids) === STRIDE_FILTER_SIZE.U)
  XSPerfAccumulate("stride_train_filter_half", PopCount(valids) >= (STRIDE_FILTER_SIZE / 2).U)
  XSPerfAccumulate("stride_train_filter_empty", PopCount(valids) === 0.U)

  val raw_enq_pattern = Cat(reqs_vl)
  val filtered_enq_pattern = Cat(needAlloc)
  val actual_enq_pattern = Cat(canAlloc)
  XSPerfAccumulate("stride_train_filter_enq", allocNum > 0.U)
  XSPerfAccumulate("stride_train_filter_deq", io.train_req.fire)
  for(i <- 0 until (1 << enqLen)) {
    XSPerfAccumulate(s"stride_train_filter_raw_enq_pattern_${toBinary(i)}", raw_enq_pattern === i.U)
    XSPerfAccumulate(s"stride_train_filter_filtered_enq_pattern_${toBinary(i)}", filtered_enq_pattern === i.U)
    XSPerfAccumulate(s"stride_train_filter_actual_enq_pattern_${toBinary(i)}", actual_enq_pattern === i.U)
  }
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
    pre_vaddr := (block_addr(vaddr))(STRIDE_VADDR_BITS - 1, 0)
    stride := 0.U
    confidence := 0.U
    hash_pc := alloc_hash_pc
  }

  def update(vaddr: UInt, always_update_pre_vaddr: Bool) = {
    val new_vaddr = (block_addr(vaddr))(STRIDE_VADDR_BITS - 1, 0)
    val new_stride = new_vaddr - pre_vaddr
    // NOTE: for now, disable negtive stride
    val stride_valid = new_stride =/= 0.U && new_stride =/= 1.U && new_stride(STRIDE_VADDR_BITS - 1) === 0.U
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
    val dynamic_depth = Input(UInt(DEPTH_BITS.W))
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
  val s2_l1_depth = Cat(s2_stride << l1_stride_ratio, 0.U(BLOCK_OFFSET.W))
  val s2_l1_pf_vaddr = (s2_vaddr + s2_l1_depth)(VAddrBits - 1, 0)
  val s2_l2_depth = Cat(s2_stride << l2_stride_ratio, 0.U(BLOCK_OFFSET.W))
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