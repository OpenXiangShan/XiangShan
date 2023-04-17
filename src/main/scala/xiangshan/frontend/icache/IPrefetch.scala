/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
  ***************************************************************************************/

package xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import difftest.DifftestRefillEvent
import freechips.rocketchip.tilelink._
import utils._
import xiangshan.cache.mmu._
import xiangshan.frontend._
import utility._
import xiangshan.XSCoreParamsKey


abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

//TODO: remove this
object DebugFlags {
  val fdip = false
}

class PIQReq(implicit p: Parameters) extends IPrefetchBundle {
  val paddr      = UInt(PAddrBits.W)
  val vSetIdx   = UInt(idxBits.W)
}

class PIQData(implicit p: Parameters) extends IPrefetchBundle {
  val ptage = UInt(tagBits.W)
  val vSetIdx = UInt(idxBits.W)
  val cacheline = UInt(blockBits.W)
  val writeBack = Bool()
}

class PIQToMainPipe(implicit  p: Parameters) extends IPrefetchBundle{
  val info = DecoupledIO(new PIQData)
}
/* need change name */
class MainPipeToPrefetchPipe(implicit p: Parameters) extends IPrefetchBundle {
  val ptage = UInt(tagBits.W)
  val vSetIdx = UInt(idxBits.W)
}

class MainPipeMissInfo(implicit p: Parameters) extends IPrefetchBundle {
  val s1_already_check_ipf = Output(Bool())
  val s2_miss_info = Vec(PortNumber, ValidIO(new MainPipeToPrefetchPipe))
}

class IPrefetchToMissUnit(implicit  p: Parameters) extends IPrefetchBundle{
  val enqReq  = DecoupledIO(new PIQReq)
}

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  val fromFtq         = Flipped(new FtqPrefechBundle)
  val iTLBInter       = new TlbRequestIO
  val pmp             =   new ICachePMPBundle
  val toIMeta         = Decoupled(new ICacheMetaReadReqBundle)
  val fromIMeta       = Input(new ICacheMetaReadRespBundle)
  val toMissUnit      = new IPrefetchToMissUnit
  val freePIQEntry    = Input(UInt(log2Ceil(nPrefetchEntries).W))
  val fromMSHR        = Flipped(Vec(totalMSHRNum,ValidIO(UInt(PAddrBits.W))))
  val IPFBufferRead   = Flipped(new IPFBufferFilterRead)
  /** icache main pipe to prefetch pipe*/
  val mainPipeMissSlotInfo = Flipped(Vec(PortNumber,ValidIO(new MainPipeToPrefetchPipe)))

  val prefetchEnable = Input(Bool())
  val prefetchDisable = Input(Bool())
  val fencei         = Input(Bool())
}

/** Prefetch Buffer **/

class IPFWritePtrQueue(implicit p: Parameters) extends IPrefetchModule with HasCircularQueuePtrHelper
{
  val io = IO(new Bundle{
    val free_ptr = DecoupledIO(UInt(log2Ceil(nIPFBufferSize).W))
    val release_ptr = Flipped(ValidIO(UInt(log2Ceil(nIPFBufferSize).W)))
    val flush = Input(Bool())
  })
  /* define ptr */
  class IPFPtr(implicit p: Parameters) extends CircularQueuePtr[IPFPtr](
    p => p(XSCoreParamsKey).icacheParameters.nPrefBufferEntries
  ){
  }

  object IPFPtr {
    def apply(f: Bool, v: UInt)(implicit p: Parameters): IPFPtr = {
      val ptr = Wire(new IPFPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }

  val queue = RegInit(VecInit((0 until nIPFBufferSize).map(i => i.U(log2Ceil(nIPFBufferSize).W))))
  val enq_ptr = RegInit(IPFPtr(true.B, 0.U))
  val deq_ptr = RegInit(IPFPtr(false.B, 0.U))

  io.free_ptr.valid := !isEmpty(enq_ptr, deq_ptr)
  io.free_ptr.bits := queue(deq_ptr.value)
  deq_ptr := deq_ptr + io.free_ptr.fire

  when (io.release_ptr.valid) {
    queue(enq_ptr.value) := io.release_ptr.bits
    enq_ptr := enq_ptr + 1.U
  }

  when (io.flush) {
    queue := RegInit(VecInit((0 until nIPFBufferSize).map(i => i.U(log2Ceil(nIPFBufferSize).W))))
    enq_ptr := RegInit(IPFPtr(true.B, 0.U))
    deq_ptr := RegInit(IPFPtr(false.B, 0.U))
  }

  XSError(isBefore(enq_ptr, deq_ptr) && !isFull(enq_ptr, deq_ptr), "enq_ptr should not before deq_ptr\n")
}


class PrefetchBuffer(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new Bundle{
    val read  = new IPFBufferRead
    val filter_read = Vec(prefetchPipeNum, new IPFBufferFilterRead)
    val write = Flipped(ValidIO(new IPFBufferWrite))
    /** to ICache replacer */
    val replace = new IPFBufferMove
    /** move & move filter port */
    val mainpipe_missinfo = Flipped(new MainPipeMissInfo)
    val meta_filter_read_req = Decoupled(new ICacheMetaReadReqBundle)
    val meta_filter_read_resp = Input(new ICacheMetaReadRespBundle)
    val move  = new Bundle() {
      val meta_write = DecoupledIO(new ICacheMetaWriteBundle)
      val data_write = DecoupledIO(new ICacheDataWriteBundle)
    }
    val fencei = Input(Bool())
  })

  class IPFBufferEntryMeta(implicit p: Parameters) extends IPrefetchBundle
  {
    val tag = UInt(tagBits.W)
    val index = UInt(idxBits.W)
    val paddr = UInt(PAddrBits.W)
    val valid = Bool()
    val confidence = UInt(log2Ceil(maxIPFMoveConf + 1).W)
    val move = Bool()
    val has_been_hit = Bool()
  }

  class IPFBufferEntryData(implicit p: Parameters) extends IPrefetchBundle
  {
    val cachline = UInt(blockBits.W)
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] ={
    return RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))
  }

  val meta_buffer = InitQueue(new IPFBufferEntryMeta, size = nIPFBufferSize)
  val data_buffer = InitQueue(new IPFBufferEntryData, size = nIPFBufferSize)

  val ipf_write_ptr_queue = Module(new IPFWritePtrQueue())
  ipf_write_ptr_queue.io.flush := io.fencei

  val meta_buffer_empty_oh = WireInit(VecInit(Seq.fill(nIPFBufferSize)(false.B)))
  (0 until nIPFBufferSize).foreach { i =>
    meta_buffer_empty_oh(i) := !meta_buffer(i).valid
  }
  XSPerfAccumulate("ipfbuffer_empty_entry_multi_cycle", PopCount(meta_buffer_empty_oh))

  /** filter read logic */
  val fr_vidx = (0 until prefetchPipeNum).map (i => io.filter_read(i).req.vSetIdx)
  val fr_ptag = (0 until prefetchPipeNum).map (i => get_phy_tag(io.filter_read(i).req.paddr))

  val fr_hit_in_buffer = (0 until prefetchPipeNum).map (i => meta_buffer.map(e => e.valid && (e.tag === fr_ptag(i)) && (e.index === fr_vidx(i))).reduce(_||_))
  val fr_hit_in_s1, fr_hit_in_s2, fr_hit_in_s3 = Wire(Vec(prefetchPipeNum, Bool()))

  (0 until prefetchPipeNum).foreach(i => io.filter_read(i).resp.ipf_hit := fr_hit_in_buffer(i) || fr_hit_in_s1(i) || fr_hit_in_s2(i) || fr_hit_in_s3(i))

  /** read logic */
  (0 until PortNumber).foreach(i => io.read.req(i).ready := true.B)
  val r_valid = VecInit((0 until PortNumber).map( i => io.read.req(i).valid)).reduce(_||_)
  val r_vidx = VecInit((0 until PortNumber).map(i => get_idx(io.read.req(i).bits.vaddr)))
  val r_ptag = VecInit((0 until PortNumber).map(i => get_phy_tag(io.read.req(i).bits.paddr)))
  val r_hit_oh = VecInit((0 until PortNumber).map(i =>
    VecInit(meta_buffer.map(entry =>
      io.read.req(i).valid && // need this condition
        entry.valid &&
        entry.tag === r_ptag(i) &&
        entry.index === r_vidx(i)
    ))))
  val r_buffer_hit = VecInit(r_hit_oh.map(_.reduce(_||_)))
  val r_buffer_hit_idx = VecInit(r_hit_oh.map(PriorityEncoder(_)))
  val r_buffer_hit_data = VecInit((0 until PortNumber).map(i => Mux1H(r_hit_oh(i), data_buffer.map(_.cachline)))) // TODO : be careful of Mux1H

  /** "read" also check data in move pipeline */
  val r_moves1pipe_hit_s1, r_moves1pipe_hit_s2, r_moves1pipe_hit_s3 = WireInit(VecInit(Seq.fill(PortNumber)(false.B)))
  val s1_move_data_cacheline, s2_move_data_cacheline, s3_move_data_cacheline = Wire(UInt(blockBits.W))

  (0 until PortNumber).foreach{ i =>
    io.read.resp(i).valid := io.read.req(i).valid
    io.read.resp(i).bits.ipf_hit := r_buffer_hit(i) || r_moves1pipe_hit_s1(i) || r_moves1pipe_hit_s2(i) || r_moves1pipe_hit_s3(i)
    io.read.resp(i).bits.cacheline := Mux(r_buffer_hit(i), r_buffer_hit_data(i),
      Mux(r_moves1pipe_hit_s1(i), s1_move_data_cacheline,
        Mux(r_moves1pipe_hit_s2(i), s2_move_data_cacheline, s3_move_data_cacheline)))
  }

  (0 until PortNumber).foreach { i =>
    when(io.read.req(i).valid && r_hit_oh(i).reduce(_ || _)) {
      meta_buffer(r_buffer_hit_idx(i)).has_been_hit := true.B
    }
    XSPerfAccumulate("ipf_entry_first_hit_by_port_" + i, io.read.req(i).valid && r_hit_oh(i).reduce(_ || _) &&
      meta_buffer(r_buffer_hit_idx(i)).has_been_hit === false.B)
  }


  /** move logic */
  val r_buffer_hit_s2     = RegNext(r_buffer_hit, init=0.U.asTypeOf(r_buffer_hit.cloneType))
  val r_buffer_hit_idx_s2 = RegNext(r_buffer_hit_idx)
  val r_rvalid_s2         = RegNext(r_valid, init=false.B)

  val s2_move_valid_0 = r_rvalid_s2 && r_buffer_hit_s2(0)
  val s2_move_valid_1 = r_rvalid_s2 && r_buffer_hit_s2(1)

  XSPerfAccumulate("prefetch_hit_bank_0", r_rvalid_s2 && r_buffer_hit_s2(0))
  XSPerfAccumulate("prefetch_hit_bank_1", r_rvalid_s2 && r_buffer_hit_s2(1))

  val move_queue    = RegInit(VecInit(Seq.fill(nIPFBufferSize)(0.U.asTypeOf(r_buffer_hit_idx_s2(0)))))

  val curr_move_ptr = RegInit(0.U(log2Ceil(nIPFBufferSize).W))
  val curr_hit_ptr  = RegInit(0.U(log2Ceil(nIPFBufferSize).W))

  val s2_move_conf_full_0 = meta_buffer(r_buffer_hit_idx_s2(0)).confidence === (maxIPFMoveConf).U
  val s2_move_conf_full_1 = meta_buffer(r_buffer_hit_idx_s2(1)).confidence === (maxIPFMoveConf).U

  val move_repeat_0 = meta_buffer(r_buffer_hit_idx_s2(0)).move
  val move_repeat_1 = meta_buffer(r_buffer_hit_idx_s2(1)).move || (r_buffer_hit_idx_s2(0) === r_buffer_hit_idx_s2(1))

  val s2_move_0 = s2_move_valid_0 && !move_repeat_0
  val s2_move_1 = s2_move_valid_1 && !move_repeat_1

  val s2_move_enqueue_0 = s2_move_0 && s2_move_conf_full_0
  val s2_move_enqueue_1 = s2_move_1 && s2_move_conf_full_1

  when(s2_move_0) {
    when(s2_move_conf_full_0) {
      meta_buffer(r_buffer_hit_idx_s2(0)).move := true.B
    }.otherwise {
      meta_buffer(r_buffer_hit_idx_s2(0)).confidence := meta_buffer(r_buffer_hit_idx_s2(0)).confidence + 1.U
    }
  }
  when(s2_move_1) {
    when(s2_move_conf_full_1) {
      meta_buffer(r_buffer_hit_idx_s2(1)).move := true.B
    }.otherwise {
      meta_buffer(r_buffer_hit_idx_s2(1)).confidence := meta_buffer(r_buffer_hit_idx_s2(1)).confidence + 1.U
    }
  }

  when(s2_move_enqueue_0 && !s2_move_enqueue_1) {
    move_queue(curr_hit_ptr) := r_buffer_hit_idx_s2(0)

    when((curr_hit_ptr + 1.U) =/= curr_move_ptr){
      curr_hit_ptr := curr_hit_ptr + 1.U
    }
  }.elsewhen(!s2_move_enqueue_0 && s2_move_enqueue_1) {
    move_queue(curr_hit_ptr) := r_buffer_hit_idx_s2(1)

    when((curr_hit_ptr + 1.U) =/= curr_move_ptr){
      curr_hit_ptr := curr_hit_ptr + 1.U
    }
  }.elsewhen(s2_move_enqueue_0 && s2_move_enqueue_1) {
    move_queue(curr_hit_ptr) := r_buffer_hit_idx_s2(0)
    move_queue(curr_hit_ptr + 1.U) := r_buffer_hit_idx_s2(1)
    when((curr_hit_ptr + 2.U) =/= curr_move_ptr){
      curr_hit_ptr := curr_hit_ptr + 2.U
    }.otherwise{
      curr_hit_ptr := curr_hit_ptr + 1.U
    }
  }

  val move_queue_empty = curr_move_ptr === curr_hit_ptr
  /** pipeline control signal */
  val s1_ready, s2_ready, s3_ready = Wire(Bool())
  val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())

  /** stage 0 */
  val s0_valid        = !move_queue_empty && meta_buffer(move_queue(curr_move_ptr)).move

  val s0_move_idx     = move_queue(curr_move_ptr)
  val s0_move_meta    = meta_buffer(s0_move_idx)
  val s0_move_data    = data_buffer(s0_move_idx)
  io.replace.vsetIdx  := meta_buffer(s0_move_idx).index
  val s0_waymask      = io.replace.waymask

  s0_fire             := s0_valid && s1_ready

  /** curr_move_ptr control logic */
  val s0_move_jump = !move_queue_empty && !meta_buffer(move_queue(curr_move_ptr)).move
  when (s0_fire) {
    curr_move_ptr := curr_move_ptr + 1.U
    meta_buffer(s0_move_idx).valid := false.B // TODO : maybe should not invalid
    meta_buffer(s0_move_idx).move  := false.B
    meta_buffer(s0_move_idx).confidence := 0.U
  }.elsewhen(s0_move_jump) {
    curr_move_ptr := curr_move_ptr + 1.U
  }

  /** stage 1 : send req to metaArray */
  val s1_valid        = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = io.fencei, lastFlush = false.B)

  val s1_move_idx     = RegEnable(s0_move_idx, s0_fire)
  val s1_move_meta    = RegEnable(s0_move_meta, s0_fire)
  val s1_move_data    = RegEnable(s0_move_data, s0_fire)
  val s1_waymask      = RegEnable(s0_waymask, s0_fire)

  io.meta_filter_read_req.valid := s1_valid
  io.meta_filter_read_req.bits.idx := s1_move_meta.index

  s1_ready            := !s1_valid || s1_fire
  s1_fire             := s1_valid && io.meta_filter_read_req.ready && s2_ready

  (0 until prefetchPipeNum).foreach(i => fr_hit_in_s1(i) := s1_valid && s1_move_meta.index === fr_vidx(i) && s1_move_meta.tag === fr_ptag(i))
  r_moves1pipe_hit_s1 := VecInit((0 until PortNumber).map(i => s1_valid && r_ptag(i) === s1_move_meta.tag && r_vidx(i) === s1_move_meta.index))
  s1_move_data_cacheline := s1_move_data.cachline

  /** stage 2 : collect message from metaArray and mainPipe to filter */
  val s2_valid        = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = io.fencei, lastFlush = false.B)

  val s2_move_idx     = RegEnable(s1_move_idx, s1_fire)
  val s2_move_meta    = RegEnable(s1_move_meta, s1_fire)
  val s2_move_data    = RegEnable(s1_move_data, s1_fire)
  val s2_waymask      = RegEnable(s1_waymask, s1_fire)

  val s2_meta_ptags   = ResultHoldBypass(data = io.meta_filter_read_resp.tags, valid = RegNext(s1_fire))
  val s2_meta_valids  = ResultHoldBypass(data = io.meta_filter_read_resp.entryValid, valid = RegNext(s1_fire))

  val s2_tag_eq_vec = VecInit((0 until nWays).map(w => s2_meta_ptags(w) === s2_move_meta.tag)) // just use port 0
  val s2_tag_match_vec = VecInit(s2_tag_eq_vec.zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s2_meta_valids(w)})
  val s2_hit_in_meta_array = ParallelOR(s2_tag_match_vec)

  val main_s2_missinfo = io.mainpipe_missinfo.s2_miss_info
  val s2_hit_main_s2_missreq = VecInit((0 until PortNumber).map(i =>
    main_s2_missinfo(i).valid && s2_move_meta.index === main_s2_missinfo(i).bits.vSetIdx
      && s2_move_meta.tag === main_s2_missinfo(i).bits.ptage)).reduce(_||_)

  val s2_discard        = s2_hit_in_meta_array || s2_hit_main_s2_missreq // || s2_hit_main_s1_missreq
  val s2_discard_latch  = holdReleaseLatch(valid = s2_discard, release = s2_fire, flush = io.fencei)
  if(DebugFlags.fdip){
    when (s2_fire && s2_discard_latch) {
      printf("<%d> IPrefetchBuffer: s2_discard : hit_in_meta_array=%d,hit_in_main_s2=%d, ptag=0x%x\n",
        GTimer(), s2_hit_in_meta_array, s2_hit_main_s2_missreq, s2_move_meta.tag)
    }
  }

  s2_ready := !s2_valid || s2_fire
  s2_fire := s2_valid && s3_ready && io.mainpipe_missinfo.s1_already_check_ipf

  (0 until prefetchPipeNum).foreach(i => fr_hit_in_s2(i) := s2_valid && s2_move_meta.index === fr_vidx(i) && s2_move_meta.tag === fr_ptag(i))
  r_moves1pipe_hit_s2 := VecInit((0 until PortNumber).map(i => s2_valid && r_ptag(i) === s2_move_meta.tag && r_vidx(i) === s2_move_meta.index))
  s2_move_data_cacheline := s2_move_data.cachline

  /** stage 3 : move data to metaArray and dataArray */
  val s3_valid = generatePipeControl(lastFire = s2_fire, thisFire = s3_fire, thisFlush = io.fencei, lastFlush = false.B)

  val s3_move_idx = RegEnable(s2_move_idx, s2_fire)
  val s3_move_meta = RegEnable(s2_move_meta, s2_fire)
  val s3_move_data = RegEnable(s2_move_data, s2_fire)
  val s3_waymask = RegEnable(s2_waymask, s2_fire)
  val s3_discard = RegEnable(s2_discard_latch, s2_fire)

  io.move.meta_write.valid := s3_valid && !s3_discard && !io.fencei
  io.move.data_write.valid := s3_valid && !s3_discard && !io.fencei
  io.move.meta_write.bits.generate(
    tag = s3_move_meta.tag,
    idx = s3_move_meta.index,
    waymask = s3_waymask,
    bankIdx = s3_move_meta.index(0))
  io.move.data_write.bits.generate(
    data = s3_move_data.cachline,
    idx = s3_move_meta.index,
    waymask = s3_waymask,
    bankIdx = s3_move_meta.index(0),
    paddr = s3_move_meta.paddr)

  s3_ready := !s3_valid || s3_fire
  s3_fire := io.move.meta_write.fire && io.move.data_write.fire || s3_discard || io.fencei
  assert((io.move.meta_write.fire && io.move.data_write.fire) || (!io.move.meta_write.fire && !io.move.data_write.fire),
    "meta and data array need fire at same time")

  (0 until prefetchPipeNum).foreach(i => fr_hit_in_s3(i) := s3_valid && s3_move_meta.index === fr_vidx(i) && s3_move_meta.tag === fr_ptag(i))
  r_moves1pipe_hit_s3 := VecInit((0 until PortNumber).map(i => s3_valid && r_ptag(i) === s3_move_meta.tag && r_vidx(i) === s3_move_meta.index))
  s3_move_data_cacheline := s3_move_data.cachline

  if (DebugFlags.fdip) {
    when(io.move.meta_write.fire) {
      printf("<%d> IPrefetchBuffer: move data to meta sram:ptag=0x%x,vidx=0x%x,waymask=0x%x\n",
        GTimer(), s3_move_meta.tag,s3_move_meta.index,s3_waymask )
    }
  }

  if (env.EnableDifftest) {
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock := clock
    difftest.io.coreid := 0.U
    difftest.io.cacheid := 6.U
    difftest.io.valid := io.move.meta_write.fire
    difftest.io.addr := s3_move_meta.paddr
    difftest.io.data := s3_move_data.cachline.asTypeOf(difftest.io.data)
  }

  /** write logic */
  val replacer = ReplacementPolicy.fromString(Some("random"), nIPFBufferSize)
  val curr_write_ptr = Wire(UInt(log2Ceil(nIPFBufferSize).W))
  when (ipf_write_ptr_queue.io.free_ptr.valid) {
    curr_write_ptr := ipf_write_ptr_queue.io.free_ptr.bits
  }.otherwise {
    curr_write_ptr := replacer.way
    when (io.write.valid) {
      replacer.miss
    }
  }

  ipf_write_ptr_queue.io.release_ptr.valid := s0_fire
  ipf_write_ptr_queue.io.release_ptr.bits := s0_move_idx

  ipf_write_ptr_queue.io.free_ptr.ready := io.write.valid
  when(io.write.valid) {
    meta_buffer(curr_write_ptr).tag := io.write.bits.meta.tag
    meta_buffer(curr_write_ptr).index := io.write.bits.meta.index
    meta_buffer(curr_write_ptr).paddr := io.write.bits.meta.paddr
    meta_buffer(curr_write_ptr).valid := true.B
    meta_buffer(curr_write_ptr).move  := false.B
    meta_buffer(curr_write_ptr).confidence := 0.U
    meta_buffer(curr_write_ptr).has_been_hit := false.B

    data_buffer(curr_write_ptr).cachline := io.write.bits.data

  }

  /** fencei: invalid all entries */
  when(io.fencei) {
    meta_buffer.foreach { b =>
      b.valid := false.B
      b.move := false.B
      b.confidence := 0.U
      b.has_been_hit := false.B
    }
    r_buffer_hit_s2 := 0.U
    r_rvalid_s2 := 0.U
    curr_move_ptr := 0.U
    curr_hit_ptr := 0.U
  }

}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

  val enableBit = RegInit(false.B)
  val maxPrefetchCounter = RegInit(0.U(log2Ceil(nPrefetchEntries + 1).W))

  val reachMaxSize = maxPrefetchCounter === nPrefetchEntries.U

  // when(io.prefetchEnable){
  //   enableBit := true.B
  // }.elsewhen((enableBit && io.prefetchDisable) || (enableBit && reachMaxSize)){
  //   enableBit := false.B
  // }
  // ignore prefetchEnable from ICacheMainPipe
  enableBit := true.B

  class PrefetchDir(implicit  p: Parameters) extends IPrefetchBundle
  {
    val valid = Bool()
    val paddr = UInt(PAddrBits.W)
  }

  val prefetch_dir = RegInit(VecInit(Seq.fill(nPrefetchEntries)(0.U.asTypeOf(new PrefetchDir))))

  val fromFtq = io.fromFtq
  val mainPipeMissSlotInfo = io.mainPipeMissSlotInfo
  val (toITLB,  fromITLB) = (io.iTLBInter.req, io.iTLBInter.resp)
  io.iTLBInter.req_kill := false.B
  val (toIMeta, fromIMeta, fromIMetaValid) = (io.toIMeta, io.fromIMeta.metaData, io.fromIMeta.entryValid)
  val (toIPFBuffer, fromIPFBuffer) = (io.IPFBufferRead.req, io.IPFBufferRead.resp)
  val (toPMP,  fromPMP)   = (io.pmp.req, io.pmp.resp)
  val toMissUnit = io.toMissUnit

  val p0_fire, p1_fire, p2_fire, p3_fire =  WireInit(false.B)
  val p0_discard, p1_discard, p2_discard, p3_discard = WireInit(false.B)
  val p0_ready, p1_ready, p2_ready, p3_ready = WireInit(false.B)

  /** Prefetch Stage 0: req from Ftq */
  val p0_valid  =   fromFtq.req.valid
  val p0_vaddr  =   addrAlign(fromFtq.req.bits.target, blockBytes, VAddrBits)
  val p0_vaddr_reg = RegEnable(p0_vaddr, fromFtq.req.fire())

  /* Cancel request when prefetch not enable
   * or the request from FTQ is same as last time */
  val p0_req_cancel = !enableBit || (p0_vaddr === p0_vaddr_reg) || io.fencei
  p0_fire   :=   p0_valid && p1_ready && toITLB.fire() && !fromITLB.bits.miss && toIMeta.ready && enableBit && !p0_req_cancel
  p0_discard := p0_valid && p0_req_cancel

  toIMeta.valid     := p0_valid && !p0_discard
  toIMeta.bits.idx  := get_idx(p0_vaddr)

  toITLB.valid         := p0_valid && !p0_discard
  toITLB.bits.size     := 3.U // TODO: fix the size
  toITLB.bits.vaddr    := p0_vaddr
  toITLB.bits.debug.pc := p0_vaddr

  toITLB.bits.kill                := DontCare
  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.debug.robIdx        := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare
  toITLB.bits.memidx              := DontCare
  toITLB.bits.no_translate        := false.B

  fromITLB.ready := true.B

  fromFtq.req.ready := p0_req_cancel || p1_ready && toITLB.ready && !fromITLB.bits.miss && toIMeta.ready

  /** Prefetch Stage 1: check in cache & ICacheMainPipeMSHR */
  val p1_valid =  generatePipeControl(lastFire = p0_fire, thisFire = p1_fire || p1_discard, thisFlush = false.B, lastFlush = false.B)

  val p1_vaddr   =  RegEnable(p0_vaddr,    p0_fire)
  // TODO: tlb is none blocked ,when tlb miss, p1 req need cancle. Now there seemes has bug
  //tlb resp
  val tlb_resp_valid = RegInit(false.B)
  when(p0_fire) {tlb_resp_valid := true.B}
    .elsewhen(tlb_resp_valid && (p1_fire || p1_discard)) {tlb_resp_valid := false.B}

  val tlb_resp_paddr = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.paddr(0))
  val tlb_resp_pf    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).pf.instr && tlb_resp_valid)
  val tlb_resp_af    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).af.instr && tlb_resp_valid)

  val p1_exception  = VecInit(Seq(tlb_resp_pf, tlb_resp_af))
  val p1_has_except =  p1_exception.reduce(_ || _)
  val p1_paddr = tlb_resp_paddr

  val p1_ptag = get_phy_tag(p1_paddr)

  val p1_meta_ptags       = ResultHoldBypass(data = VecInit(fromIMeta.map(way => way.tag)),valid = RegNext(p0_fire))
  val p1_meta_valids      = ResultHoldBypass(data = fromIMetaValid,valid = RegNext(p0_fire))

  val p1_tag_eq_vec       =  VecInit(p1_meta_ptags.map(_  ===  p1_ptag ))
  val p1_tag_match_vec    =  VecInit(p1_tag_eq_vec.zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && p1_meta_valids(w)})
  val p1_tag_match        =  ParallelOR(p1_tag_match_vec)
  // check ICacheMissEntry
  val p1_check_in_mshr = VecInit(io.fromMSHR.map(mshr => mshr.valid && mshr.bits === addrAlign(p1_paddr, blockBytes, PAddrBits))).reduce(_||_)

  val (p1_hit, p1_miss)   =  (p1_valid && (p1_tag_match || p1_check_in_mshr) && !p1_has_except , p1_valid && !p1_tag_match && !p1_has_except && !p1_check_in_mshr)


  //overriding the invalid req
  val p1_req_cancle = (p1_hit || (tlb_resp_valid && p1_exception.reduce(_ || _)) || io.fencei) && p1_valid
  val p1_req_accept   = p1_valid && tlb_resp_valid && p1_miss

  p1_ready    :=   p1_fire || p1_req_cancle || !p1_valid
  p1_fire     :=   p1_valid && p1_req_accept && p2_ready && enableBit
  p1_discard  :=   p1_valid && p1_req_cancle

  /** Prefetch Stage 2: check PMP & send check req to ICacheMainPipeMSHR */
  val p2_valid =  generatePipeControl(lastFire = p1_fire, thisFire = p2_fire || p2_discard, thisFlush = false.B, lastFlush = false.B)
  val p2_pmp_fire = p2_valid
  val pmpExcpAF = fromPMP.instr

  val p2_paddr     = RegEnable(p1_paddr,  p1_fire)
  val p2_except_pf = RegEnable(tlb_resp_pf, p1_fire)
  val p2_except_af = DataHoldBypass(pmpExcpAF, p2_pmp_fire) || RegEnable(tlb_resp_af, p1_fire)
  val p2_mmio      = DataHoldBypass(io.pmp.resp.mmio && !p2_except_af && !p2_except_pf, p2_pmp_fire)
  val p2_vaddr   =  RegEnable(p1_vaddr,    p1_fire)


  /*when a prefetch req meet with a miss req in MSHR cancle the prefetch req */
  val p2_check_in_mshr = VecInit(io.fromMSHR.map(mshr => mshr.valid && mshr.bits === addrAlign(p2_paddr, blockBytes, PAddrBits))).reduce(_||_)

  //TODO wait PMP logic
  val p2_exception  = VecInit(Seq(pmpExcpAF, p2_mmio)).reduce(_||_)

  io.pmp.req.valid      := p2_pmp_fire
  io.pmp.req.bits.addr  := p2_paddr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  p2_ready :=   p2_fire || p2_discard || !p2_valid
  p2_fire  :=   p2_valid && !p2_exception && p3_ready && p2_pmp_fire
  p2_discard := p2_valid && (p2_exception && p2_pmp_fire || io.fencei || p2_check_in_mshr)

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p3_valid =  generatePipeControl(lastFire = p2_fire, thisFire = p3_fire || p3_discard, thisFlush = false.B, lastFlush = false.B)

  val p3_paddr = RegEnable(p2_paddr,  p2_fire)
  val p3_check_in_mshr = VecInit(io.fromMSHR.map(mshr => mshr.valid && mshr.bits === addrAlign(p3_paddr, blockBytes, PAddrBits))).reduce(_||_)
  val p3_vaddr   =  RegEnable(p2_vaddr,    p2_fire)
  val p3_vidx = get_idx(p3_vaddr)
  // check in prefetch buffer
  toIPFBuffer.vSetIdx := p3_vidx
  toIPFBuffer.paddr := p3_paddr
  val p3_buffer_hit = fromIPFBuffer.ipf_hit

  val p3_hit_dir = VecInit((0 until nPrefetchEntries).map(i => prefetch_dir(i).valid && prefetch_dir(i).paddr === p3_paddr )).reduce(_||_)
  //Cache miss handling by main pipe, info from mainpipe missslot
  val p3_hit_mp_miss = VecInit((0 until PortNumber).map(i =>
    mainPipeMissSlotInfo(i).valid && (mainPipeMissSlotInfo(i).bits.ptage === get_phy_tag(p3_paddr) &&
    (mainPipeMissSlotInfo(i).bits.vSetIdx === p3_vidx)))).reduce(_||_)
  val p3_req_cancel = /*p3_hit_dir ||*/ p3_check_in_mshr || !enableBit || p3_hit_mp_miss || p3_buffer_hit || io.fencei
  p3_discard := p3_valid && p3_req_cancel

  toMissUnit.enqReq.valid := p3_valid && !p3_req_cancel
  toMissUnit.enqReq.bits.paddr := p3_paddr
  toMissUnit.enqReq.bits.vSetIdx := p3_vidx

  when(io.fencei){
    maxPrefetchCounter := 0.U

    prefetch_dir.foreach(_.valid := false.B)
  }.elsewhen(toMissUnit.enqReq.fire()){
//    when(reachMaxSize){
//      prefetch_dir(io.freePIQEntry).paddr := p3_paddr
//    }.otherwise {
//      maxPrefetchCounter := maxPrefetchCounter + 1.U
//
//      prefetch_dir(maxPrefetchCounter).valid := true.B
//      prefetch_dir(maxPrefetchCounter).paddr := p3_paddr
//    }
    // now prefetch_dir hold status for all PIQ
    prefetch_dir(io.freePIQEntry).paddr := p3_paddr
    prefetch_dir(io.freePIQEntry).valid := true.B
  }

  p3_ready := toMissUnit.enqReq.ready || !enableBit
  p3_fire  := toMissUnit.enqReq.fire()

}

class PIQEntry(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new Bundle{
    val id          = Input(UInt((log2Ceil(nPrefetchEntries + PortNumber)).W))

    val req         = Flipped(DecoupledIO(new PIQReq))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    //write back to Prefetch Buffer
    val piq_write_ipbuffer = DecoupledIO(new IPFBufferWrite)

    val fencei      = Input(Bool())

    val prefetch_entry_data = DecoupledIO(new PIQData)

    val ongoing_req    = ValidIO(UInt(PAddrBits.W))
  })

  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_finish:: Nil = Enum(5)
  val state = RegInit(s_idle)

  //req register
  val req = Reg(new PIQReq)
  val req_idx = req.vSetIdx                     //virtual index
  val req_tag = get_phy_tag(req.paddr)           //physical tag

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  //8 for 64 bits bus and 2 for 256 bits
  val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
  val respDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

  //to main pipe s1
  io.prefetch_entry_data.valid := state =/= s_idle
  io.prefetch_entry_data.bits.vSetIdx := req_idx
  io.prefetch_entry_data.bits.ptage := req_tag
  io.prefetch_entry_data.bits.cacheline := respDataReg.asUInt
  io.prefetch_entry_data.bits.writeBack := state === s_write_back

  //initial
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.piq_write_ipbuffer.bits:= DontCare

  io.req.ready := state === s_idle
  io.mem_acquire.valid := state === s_memReadReq

  val needflush_r = RegInit(false.B)
  when (state === s_idle) { needflush_r := false.B }
  when (state =/= s_idle && io.fencei) { needflush_r := true.B }
  val needflush = needflush_r | io.fencei

  //state change
  switch(state){
    is(s_idle){
      when(io.req.fire()){
        readBeatCnt := 0.U
        state := s_memReadReq
        req := io.req.bits
      }
    }

    // memory request
    is(s_memReadReq){
      when(io.mem_acquire.fire()){
        state := s_memReadResp
      }
    }

    is(s_memReadResp){
      when (edge.hasData(io.mem_grant.bits)) {
        when (io.mem_grant.fire()) {
          readBeatCnt := readBeatCnt + 1.U
          respDataReg(readBeatCnt) := io.mem_grant.bits.data
          when (readBeatCnt === (refillCycles - 1).U) {
            assert(refill_done, "refill not done!")
            state := s_write_back
          }
        }
      }
    }

    is(s_write_back){
      state := Mux(io.piq_write_ipbuffer.fire() || needflush, s_finish, s_write_back)
    }

    is(s_finish){
      state := s_idle
    }
  }

  //refill write and meta write
  //WARNING: Maybe could not finish refill in 1 cycle
  io.piq_write_ipbuffer.valid := (state === s_write_back) && !needflush
  io.piq_write_ipbuffer.bits.meta.tag := req_tag
  io.piq_write_ipbuffer.bits.meta.index := req_idx
  io.piq_write_ipbuffer.bits.meta.paddr := req.paddr
  io.piq_write_ipbuffer.bits.data := respDataReg.asUInt
  io.piq_write_ipbuffer.bits.buffIdx := io.id - PortNumber.U

  io.ongoing_req.valid := state =/= s_idle
  io.ongoing_req.bits := addrAlign(req.paddr, blockBytes, PAddrBits)

  XSPerfAccumulate("PrefetchEntryReq" + Integer.toString(id, 10), io.req.fire())

  //mem request
  io.mem_acquire.bits  := edge.Get(
    fromSource      = io.id,
    toAddress       = Cat(req.paddr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W)),
    lgSize          = (log2Up(cacheParams.blockBytes)).U)._2



  XSError(blockCounter(io.req.fire, io.piq_write_ipbuffer.fire, 10000), "PIQEntry"+ io.id +"_block_10000_cycle,may_has_error\n")
}
