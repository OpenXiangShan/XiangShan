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
import freechips.rocketchip.tilelink._
import utils._
import xiangshan.cache.mmu._
import xiangshan.frontend._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import huancun.{PreferCacheKey}


abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

//TODO: remove this
object DebugFlags {
  val fdip = true
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

class IPrefetchToMissUnit(implicit  p: Parameters) extends IPrefetchBundle{
  val enqReq  = DecoupledIO(new PIQReq)
}

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  val fromFtq         = Flipped(new FtqPrefechBundle)
  val iTLBInter       = new TlbRequestIO
  val pmp             =   new ICachePMPBundle
  val toIMeta         = Decoupled(new ICacheReadBundle)
  val fromIMeta       = Input(new ICacheMetaRespBundle)
  val toMissUnit      = new IPrefetchToMissUnit
  val fromMSHR        = Flipped(Vec(PortNumber,ValidIO(UInt(PAddrBits.W))))

  val prefetchEnable = Input(Bool())
  val prefetchDisable = Input(Bool())
}

/** Prefetch Buffer **/


class PrefetchBuffer(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new Bundle{
    val read  = new IPFBufferRead
    val write = Flipped(ValidIO(new IPFBufferWrite))
    val move  = new Bundle() {
//      /** input */
//      val mp_ip_cache_hit = Input(Bool())
//      val mp_ip_buffer_idx = Input(Bool())
      /** output */
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
    val confidence = UInt(log2Ceil(maxIPFMoveConf).W)
    val move = Bool()
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

  /** read logic */
  // latch read req address and response in next cycle
  val r_vidx_s0   = VecInit(io.read.req.bits.vaddr.map(get_idx(_)))
  val r_ren_s0    = io.read.req.bits.rvalid
  val r_rvalid_s0 = io.read.req.fire()

  val r_vidx_s1 = RegNext(r_vidx_s0, init = 0.U.asTypeOf(r_vidx_s0.cloneType))
  val r_ren_s1 = RegNext(r_ren_s0, init=0.U.asTypeOf(r_ren_s0.cloneType))
  val r_rvalid_s1 = RegNext(r_rvalid_s0, init=false.B)

  val r_ptag_s1 = VecInit(io.read.req.bits.paddr.map(get_phy_tag(_)))

  val r_hit_oh_s1 = VecInit((0 until PortNumber).map(i =>
                                 VecInit(meta_buffer.map(entry => r_ren_s1(i) &&
                                                                  entry.valid &&
                                                                  entry.tag === r_ptag_s1(i) &&
                                                                  entry.index === r_vidx_s1(i)))))

  val r_buffer_hit_s1  = VecInit(r_hit_oh_s1.map(_.reduce(_||_)))
  val r_buffer_hit_idx_s1 = VecInit(r_hit_oh_s1.map(PriorityEncoder(_)))
  val r_buffer_hit_data_s1 = VecInit((0 until PortNumber).map(i => Mux1H(r_hit_oh_s1(i), data_buffer.map(_.cachline)) ))


  io.read.req.ready := true.B//TODO: read-wite bypass
  io.read.resp.valid := RegNext(io.read.req.fire(), init = false.B)
  io.read.resp.bits.ipf_hit := r_buffer_hit_s1
  io.read.resp.bits.cacheline := r_buffer_hit_data_s1


  /** write logic */
  val replacer = ReplacementPolicy.fromString(Some("random"), nIPFBufferSize)
  val curr_write_ptr = RegInit(0.U(log2Ceil(nIPFBufferSize).W))
  val victim_way = curr_write_ptr + 1.U//replacer.way

  when(io.write.valid) {
    meta_buffer(curr_write_ptr).tag := io.write.bits.meta.tag
    meta_buffer(curr_write_ptr).index := io.write.bits.meta.index
    meta_buffer(curr_write_ptr).paddr := io.write.bits.meta.paddr
    meta_buffer(curr_write_ptr).valid := true.B
    meta_buffer(curr_write_ptr).move  := false.B
    meta_buffer(curr_write_ptr).confidence := 0.U

    data_buffer(curr_write_ptr).cachline := io.write.bits.data

    //update replacer
    replacer.access(curr_write_ptr)
    curr_write_ptr := victim_way

    if(DebugFlags.fdip){
      printf("(%d) IPrefetchBuffer: write into buffer, curr_write_ptr: %d, addr: 0x%x\n",GTimer(), curr_write_ptr,io.write.bits.meta.paddr)
    }
  }


  /** move logic */
  //now we only move port 0 for simplicity
  //TODO: move 2 port
  
  //TODO: blocking move
  val r_buffer_hit_s2     = RegNext(r_buffer_hit_s1, init=0.U.asTypeOf(r_buffer_hit_s1.cloneType))
  val r_buffer_hit_idx_s2 = RegNext(r_buffer_hit_idx_s1)
  val r_rvalid_s2         = RegNext(r_rvalid_s1, init=false.B)

  val s2_move_valid = r_rvalid_s2 && r_buffer_hit_s2(0)

  

  XSPerfAccumulate("prefetch_hit_bank_0", r_rvalid_s2 && r_buffer_hit_s2(0))
  XSPerfAccumulate("prefetch_hit_bank_1", r_rvalid_s2 && r_buffer_hit_s2(1))

  val move_queue    = RegInit(VecInit(Seq.fill(nIPFBufferSize)(0.U.asTypeOf(r_buffer_hit_idx_s2(0)))))

  val curr_move_ptr = RegInit(0.U(log2Ceil(nIPFBufferSize).W))
  val curr_hit_ptr  = RegInit(0.U(log2Ceil(nIPFBufferSize).W))

  val move_repeat = meta_buffer(r_buffer_hit_idx_s2(0)).move === true.B

  when(s2_move_valid && !move_repeat) {
    when(meta_buffer(r_buffer_hit_idx_s2(0)).confidence === (maxIPFMoveConf-1).U) {
      meta_buffer(r_buffer_hit_idx_s2(0)).move := true.B
    }.otherwise {
      meta_buffer(r_buffer_hit_idx_s2(0)).confidence := meta_buffer(r_buffer_hit_idx_s2(0)).confidence + 1.U
    }

    move_queue(curr_hit_ptr) := r_buffer_hit_idx_s2(0)
    
    when((curr_hit_ptr + 1.U) =/= curr_move_ptr){
      curr_hit_ptr := curr_hit_ptr + 1.U
    }
  }
  
  val move_queue_empty = curr_move_ptr === curr_hit_ptr
  val move_valid = !move_queue_empty && meta_buffer(move_queue(curr_move_ptr)).move
  val move_jump  = !move_queue_empty && !meta_buffer(move_queue(curr_move_ptr)).move
  io.move.meta_write.valid := move_valid
  io.move.data_write.valid := move_valid
  io.move.meta_write.bits  := DontCare
  io.move.data_write.bits  := DontCare

  when(io.move.meta_write.fire()) {
    //latch read data
    val move_idx = move_queue(curr_move_ptr)
    val moveEntryMeta = meta_buffer(move_idx)
    val moveEntryData = data_buffer(move_idx)


    io.move.meta_write.bits.generate(tag = moveEntryMeta.tag,
      coh = ClientMetadata(ClientStates.Branch),
      idx = moveEntryMeta.index,
      waymask = 0.U,
      bankIdx = moveEntryMeta.index(0))

    io.move.data_write.bits.generate(data = moveEntryData.cachline,
      idx = moveEntryMeta.index,
      waymask = 0.U,
      bankIdx = moveEntryMeta.index(0),
      paddr = moveEntryMeta.paddr)

      curr_move_ptr := curr_move_ptr + 1.U
      meta_buffer(move_idx).valid := false.B
      meta_buffer(move_idx).move  := false.B
      meta_buffer(move_idx).confidence := 0.U
  }.elsewhen(move_jump){
    curr_move_ptr := curr_move_ptr + 1.U
  }

  /** fencei: invalid all entries */
  when(io.fencei) {
    meta_buffer.foreach{
      case b =>
        b.valid := false.B
        b.move := false.B
        b.confidence := 0.U
    }
  }

}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

  val enableBit = RegInit(false.B)
  val maxPrefetchCoutner = RegInit(0.U(log2Ceil(nPrefetchEntries + 1).W))

  val reachMaxSize = maxPrefetchCoutner === nPrefetchEntries.U

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
  val (toITLB,  fromITLB) = (io.iTLBInter.req, io.iTLBInter.resp)
  io.iTLBInter.req_kill := false.B
  val (toIMeta, fromIMeta) = (io.toIMeta, io.fromIMeta.metaData(0))
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
  val p0_req_cancel = !enableBit || (p0_vaddr === p0_vaddr_reg)
  p0_fire   :=   p0_valid && p1_ready && toITLB.fire() && !fromITLB.bits.miss && toIMeta.ready && enableBit && !p0_req_cancel
  p0_discard := p0_valid && p0_req_cancel

  toIMeta.valid     := p0_valid && !p0_discard
  toIMeta.bits.vSetIdx(0) := get_idx(p0_vaddr)

  toIMeta.bits.vSetIdx(1) := DontCare
  toIMeta.bits.isDoubleLine := false.B

  toITLB.valid         := p0_valid && !p0_discard
  toITLB.bits.size     := 3.U // TODO: fix the size
  toITLB.bits.vaddr    := p0_vaddr
  toITLB.bits.debug.pc := p0_vaddr

  toITLB.bits.kill                := DontCare
  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.debug.robIdx        := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare


  fromITLB.ready := true.B

  fromFtq.req.ready :=  !p0_valid || p0_fire || p0_discard

  /** Prefetch Stage 1: cache probe filter */
  val p1_valid =  generatePipeControl(lastFire = p0_fire, thisFire = p1_fire || p1_discard, thisFlush = false.B, lastFlush = false.B)

  val p1_vaddr   =  RegEnable(p0_vaddr,    p0_fire)

  //tlb resp
  val tlb_resp_valid = RegInit(false.B)
  when(p0_fire) {tlb_resp_valid := true.B}
  .elsewhen(tlb_resp_valid && (p1_fire || p1_discard)) {tlb_resp_valid := false.B}

  val tlb_resp_paddr = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.paddr(0))
  val tlb_resp_pf    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).pf.instr && tlb_resp_valid)
  val tlb_resp_af    = ResultHoldBypass(valid = RegNext(p0_fire), data = fromITLB.bits.excp(0).af.instr && tlb_resp_valid)

  val p1_exception  = VecInit(Seq(tlb_resp_pf, tlb_resp_af))
  val p1_has_except =  p1_exception.reduce(_ || _)

  val p1_ptag = get_phy_tag(tlb_resp_paddr)

  val p1_meta_ptags       = ResultHoldBypass(data = VecInit(fromIMeta.map(way => way.tag)),valid = RegNext(p0_fire))
  val p1_meta_cohs        = ResultHoldBypass(data = VecInit(fromIMeta.map(way => way.coh)),valid = RegNext(p0_fire))

  val p1_tag_eq_vec       =  VecInit(p1_meta_ptags.map(_  ===  p1_ptag ))
  val p1_tag_match_vec    =  VecInit(p1_tag_eq_vec.zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && p1_meta_cohs(w).isValid()})
  val p1_tag_match        =  ParallelOR(p1_tag_match_vec)
  val (p1_hit, p1_miss)   =  (p1_valid && p1_tag_match && !p1_has_except, p1_valid && !p1_tag_match && !p1_has_except)

  //overriding the invalid req
  val p1_req_cancle = (p1_hit || (tlb_resp_valid && p1_exception.reduce(_ || _))) && p1_valid
  val p1_req_accept   = p1_valid && tlb_resp_valid && p1_miss

  p1_ready    :=   p1_fire || p1_req_cancle || !p1_valid
  p1_fire     :=   p1_valid && p1_req_accept && p2_ready && enableBit
  p1_discard  :=   p1_valid && p1_req_cancle

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p2_valid =  generatePipeControl(lastFire = p1_fire, thisFire = p2_fire || p2_discard, thisFlush = false.B, lastFlush = false.B)
  val p2_pmp_fire = p2_valid
  val pmpExcpAF = fromPMP.instr

  val p2_paddr     = RegEnable(tlb_resp_paddr,  p1_fire)
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
  p2_discard := p2_valid && (p2_exception && p2_pmp_fire)

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p3_valid =  generatePipeControl(lastFire = p2_fire, thisFire = p3_fire || p3_discard, thisFlush = false.B, lastFlush = false.B)

  val p3_paddr = RegEnable(p2_paddr,  p2_fire)
  val p3_check_in_mshr = RegEnable(p2_check_in_mshr,  p2_fire)
  val p3_vaddr   =  RegEnable(p2_vaddr,    p2_fire)


  val p3_hit_dir = VecInit((0 until nPrefetchEntries).map(i => prefetch_dir(i).valid && prefetch_dir(i).paddr === p3_paddr )).reduce(_||_)

  val p3_req_cancel = p3_hit_dir || p3_check_in_mshr || !enableBit
  p3_discard := p3_valid && p3_req_cancel

  class p3QueueEntry(implicit p: Parameters) extends IPrefetchBundle{
    val paddr = UInt(PAddrBits.W)
    val vSetIdx = UInt(idxBits.W)
  }
  //Is the buffer size suitable?
  val p3_wait_buffer  = RegInit(VecInit(Seq.fill(nPrefetchEntries)(0.U.asTypeOf(new p3QueueEntry))))
  val p3_enqueue_ptr  = RegInit(0.U(log2Ceil(nPrefetchEntries).W))
  val p3_write_ptr    = RegInit(0.U(log2Ceil(nPrefetchEntries).W))
  val p3_issue        = p3_valid && !p3_req_cancel
  val p3_buffer_empty = p3_enqueue_ptr === p3_write_ptr
  val p3_buffer_full  = (p3_write_ptr + 1.U) === p3_enqueue_ptr

  //When wait buffer is not empty or PIQ is not ready to enqueue, write into p3 buffer.
  val p3_buffer_fire = WireInit(false.B)
  when(p3_issue && (!p3_buffer_empty || !toMissUnit.enqReq.ready) && !p3_buffer_full) {
    p3_buffer_fire := true.B

    p3_write_ptr := p3_write_ptr + 1.U
    p3_wait_buffer(p3_write_ptr).paddr   := p3_paddr
    p3_wait_buffer(p3_write_ptr).vSetIdx := get_idx(p3_vaddr)
  }
  when(toMissUnit.enqReq.fire() && !p3_buffer_empty) {
    p3_enqueue_ptr := p3_enqueue_ptr + 1.U
  }

  val enq_paddr   = Mux(p3_buffer_empty, p3_paddr, p3_wait_buffer(p3_enqueue_ptr).paddr)
  val enq_vSetIdx = Mux(p3_buffer_empty, get_idx(p3_vaddr), p3_wait_buffer(p3_enqueue_ptr).vSetIdx)
  toMissUnit.enqReq.valid               := p3_issue || !p3_buffer_empty
  toMissUnit.enqReq.bits.paddr          := enq_paddr
  toMissUnit.enqReq.bits.vSetIdx        := enq_vSetIdx


  when(reachMaxSize){
    maxPrefetchCoutner := 0.U

    prefetch_dir.foreach(_.valid := false.B)
  }.elsewhen(toMissUnit.enqReq.fire()){
    maxPrefetchCoutner := maxPrefetchCoutner + 1.U

    prefetch_dir(maxPrefetchCoutner).valid := true.B
    prefetch_dir(maxPrefetchCoutner).paddr := enq_paddr
  }

  p3_ready := !p3_buffer_full || !enableBit
  p3_fire  := toMissUnit.enqReq.fire() || p3_buffer_fire

  if (DebugFlags.fdip) {
    when(toMissUnit.enqReq.fire()){
      printf("(%d) PIQ enqueue, vaddr: 0x%x, paddr: 0x%x\n",GTimer(), p3_vaddr, p3_paddr)
    }
    when(p0_discard) {
      printf("[%d] discard in p0, aligned vaddr: 0x%x, vaddr: 0x%x\n", GTimer(), p0_vaddr, fromFtq.req.bits.target)
    }
    when(p1_discard) {
      printf("[%d] discard in p1, aligned vaddr: 0x%x\n", GTimer(), p1_vaddr)
    }
    when(p2_discard) {
      printf("[%d] discard in p2, aligned vaddr: 0x%x\n", GTimer(), p2_vaddr)
    }
    when(p3_discard) {
      printf("[%d] discard in p3, aligned vaddr: 0x%x\n", GTimer(), p3_vaddr)
    }

  }

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

    //TODO: fencei flush instructions
    val fencei      = Input(Bool())

    val prefetch_entry_data = DecoupledIO(new PIQData)
//    //hit in prefetch buffer
//    val hitFree     = Input(Bool())
//    //free as a victim
//    val replaceFree = Input(Bool())
  })

  val s_idle :: s_memReadReq :: s_memReadResp :: s_write_back :: s_finish:: Nil = Enum(5)
  val state = RegInit(s_idle)

  //req register
  val req = Reg(new PIQReq)
  val req_idx = req.vSetIdx                     //virtual index
  val req_tag = get_phy_tag(req.paddr)           //physical tag

  if(DebugFlags.fdip){
    when(io.mem_acquire.fire()) {
      printf("(%d) PIQEntry: acquire_fire, source id: %d, paddr: 0x%x\n", GTimer(), id.U, req.paddr)
    }
    when(RegNext(state === s_memReadResp) && (state === s_write_back)){
      printf("(%d) PIQEntry: grant_done, source id: %d, paddr: 0x%x\n", GTimer(), id.U,  req.paddr)
    }
  }

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

  val needFlushReg = RegInit(false.B)
  when(state === s_idle || state === s_finish){
    needFlushReg := false.B
  }
  when((state === s_memReadReq || state === s_memReadResp || state === s_write_back) && io.fencei){
    needFlushReg := true.B
  }
  val needFlush = needFlushReg || io.fencei
  //flush register
//  val needFlush = generateState(enable = io.fencei && (state =/= s_idle) && (state =/= s_finish), release = (state=== s_wait_free))
//
//  val freeEntry   = io.fencei
//  val needFree    = generateState(enable = freeEntry && (state === s_finish),  release = (state === s_finish))

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
      state := Mux(io.piq_write_ipbuffer.fire(), s_finish, s_write_back)
    }

    is(s_finish){
      state := s_idle
    }
  }

  //refill write and meta write
  //WARNING: Maybe could not finish refill in 1 cycle
  io.piq_write_ipbuffer.valid := (state === s_write_back) && !needFlush
  io.piq_write_ipbuffer.bits.meta.tag := req_tag
  io.piq_write_ipbuffer.bits.meta.index := req_idx
  io.piq_write_ipbuffer.bits.meta.paddr := req.paddr
  io.piq_write_ipbuffer.bits.data := respDataReg.asUInt
  io.piq_write_ipbuffer.bits.buffIdx := io.id - PortNumber.U

  XSPerfAccumulate("PrefetchEntryReq" + Integer.toString(id, 10), io.req.fire())

  //mem request
  io.mem_acquire.bits  := edge.Get(
    fromSource      = io.id,
    toAddress       = Cat(req.paddr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W)),
    lgSize          = (log2Up(cacheParams.blockBytes)).U)._2

}
