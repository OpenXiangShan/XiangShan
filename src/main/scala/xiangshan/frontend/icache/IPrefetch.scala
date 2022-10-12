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

class PIQReq(implicit p: Parameters) extends IPrefetchBundle {
  val paddr      = UInt(PAddrBits.W)
  val vSetIdx   = UInt(idxBits.W)
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
  }

  class IPFBufferEntryData(implicit p: Parameters) extends IPrefetchBundle
  {
    val cachline = UInt(blockBits.W)
  }

  def InitQueue[T <: Data](entry: T, size: Int): Vec[T] ={
    return RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(entry.cloneType))))
  }

  val meta_buffer = InitQueue(new IPFBufferEntryMeta, size = nPrefetchEntries)
  val data_buffer = InitQueue(new IPFBufferEntryData, size = nPrefetchEntries)
  val free_list   = RegInit(VecInit(Seq.fill(nPrefetchEntries)(true.B)))

  /** read logic */
  // latch read req address and response in next cycle
  val r_vidx_s0 = VecInit(io.read.req.bits.vaddr.map(get_idx(_)))
  val r_vidx_s1 = RegNext(r_vidx_s0, init = 0.U.asTypeOf(r_vidx_s0.cloneType))

  val r_ptag_s1 = VecInit(io.read.req.bits.paddr.map(get_phy_tag(_)))

  val r_hit_oh_s1 = VecInit((0 until PortNumber).map(i =>
                                 VecInit(meta_buffer.map(entry => entry.valid &&
                                                                  entry.tag === r_ptag_s1(i) &&
                                                                  entry.index === r_vidx_s1(i)))))

  val r_buffer_hit_s1  = VecInit(r_hit_oh_s1.map(_.reduce(_||_)))
  val r_buffer_hit_idx_s1 = VecInit(r_hit_oh_s1.map(PriorityEncoder(_)))
  val r_buffer_hit_data_s1 = VecInit((0 until PortNumber).map(i => Mux1H(r_hit_oh_s1(i), data_buffer.map(_.cachline)) ))


  io.read.req.ready := !io.write.valid
  io.read.resp.valid := RegNext(io.read.req.fire(), init = false.B)
  io.read.resp.bits.ipf_hit := r_buffer_hit_s1
  io.read.resp.bits.cacheline := r_buffer_hit_data_s1


  /** write logic */
  when(io.write.valid){
    meta_buffer(io.write.bits.buffIdx).tag := io.write.bits.meta.tag
    meta_buffer(io.write.bits.buffIdx).index := io.write.bits.meta.index
    meta_buffer(io.write.bits.buffIdx).paddr := io.write.bits.meta.paddr
    meta_buffer(io.write.bits.buffIdx).valid := true.B

    data_buffer(io.write.bits.buffIdx).cachline := io.write.bits.data
  }


  /** move logic */
  //now we only move port 1 for simplicity
  //TODO: move 2 port
  val r_buffer_hit_s2  = RegNext(r_buffer_hit_s1, init=0.U.asTypeOf(r_buffer_hit_s1.cloneType))
  val r_buffer_hit_idx_s2 = RegNext(r_buffer_hit_idx_s1)

  io.move.meta_write.valid := RegNext(r_buffer_hit_s2(0), init = false.B)
  io.move.data_write.valid := RegNext(r_buffer_hit_s2(0), init = false.B)
  io.move.meta_write.bits  := DontCare
  io.move.data_write.bits  := DontCare

  when(r_buffer_hit_s2(0)) {
    val moveEntryMeta = RegNext(meta_buffer(r_buffer_hit_idx_s2(0)))
    val moveEntryData = RegNext(data_buffer(r_buffer_hit_idx_s2(0)))


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

  }

  /** fencei: invalid all entries */
  when(io.fencei){
    meta_buffer.map(_.valid := false.B)
  }

}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

  val enableBit = RegInit(false.B)
  val maxPrefetchCoutner = RegInit(0.U(log2Ceil(nPrefetchEntries + 1).W))

  val reachMaxSize = maxPrefetchCoutner === nPrefetchEntries.U

  when(io.prefetchEnable){
    enableBit := true.B
  }.elsewhen((enableBit && io.prefetchDisable) || (enableBit && reachMaxSize)){
    enableBit := false.B
  }

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
  val p1_discard, p2_discard, p3_discard = WireInit(false.B)
  val p0_ready, p1_ready, p2_ready, p3_ready = WireInit(false.B)

  /** Prefetch Stage 0: req from Ftq */
  val p0_valid  =   fromFtq.req.valid
  val p0_vaddr  =   addrAlign(fromFtq.req.bits.target, blockBytes, VAddrBits)
  p0_fire   :=   p0_valid && p1_ready && toITLB.fire() && !fromITLB.bits.miss && toIMeta.ready && enableBit
  //discard req when source not ready
  // p0_discard := p0_valid && ((toITLB.fire() && fromITLB.bits.miss) || !toIMeta.ready || !enableBit)

  toIMeta.valid     := p0_valid
  toIMeta.bits.vSetIdx(0) := get_idx(p0_vaddr)

  toIMeta.bits.vSetIdx(1) := DontCare
  toIMeta.bits.isDoubleLine := false.B

  toITLB.valid         := p0_valid
  toITLB.bits.size     := 3.U // TODO: fix the size
  toITLB.bits.vaddr    := p0_vaddr
  toITLB.bits.debug.pc := p0_vaddr

  toITLB.bits.kill                := DontCare
  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.debug.robIdx        := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare


  fromITLB.ready := true.B

  fromFtq.req.ready :=  true.B //(!enableBit || (enableBit && p3_ready)) && toIMeta.ready //&& GTimer() > 500.U

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

  p3_discard := p3_hit_dir || p3_check_in_mshr || (p3_valid && enableBit && !toMissUnit.enqReq.ready)

  toMissUnit.enqReq.valid             := p3_valid && enableBit && !p3_discard
  toMissUnit.enqReq.bits.paddr        := p3_paddr
  toMissUnit.enqReq.bits.vSetIdx        := get_idx(p3_vaddr)


  when(reachMaxSize){
    maxPrefetchCoutner := 0.U

    prefetch_dir.foreach(_.valid := false.B)
  }.elsewhen(toMissUnit.enqReq.fire()){
    maxPrefetchCoutner := maxPrefetchCoutner + 1.U

    prefetch_dir(maxPrefetchCoutner).valid := true.B
    prefetch_dir(maxPrefetchCoutner).paddr := p3_paddr
  }

  p3_ready := toMissUnit.enqReq.ready || !enableBit
  p3_fire  := toMissUnit.enqReq.fire()

}

class PIQEntry(edge: TLEdgeOut)(implicit p: Parameters) extends IPrefetchModule
{
  val io = IO(new Bundle{
    val id          = Input(UInt(log2Ceil(nPrefetchEntries).W))

    val req         = Flipped(DecoupledIO(new PIQReq))

    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant   = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    //write back to Prefetch Buffer
    val piq_write_ipbuffer = DecoupledIO(new IPFBufferWrite)

    //TOO: fencei flush instructions
    val fencei      = Input(Bool())
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

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)

  //8 for 64 bits bus and 2 for 256 bits
  val readBeatCnt = Reg(UInt(log2Up(refillCycles).W))
  val respDataReg = Reg(Vec(refillCycles,UInt(beatBits.W)))

  //initial
  io.mem_acquire.bits := DontCare
  io.mem_grant.ready := true.B
  io.piq_write_ipbuffer.bits:= DontCare

  io.req.ready := state === s_idle
  io.mem_acquire.valid := state === s_memReadReq

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
  io.piq_write_ipbuffer.valid := (state === s_write_back) //&& !needFlush
  io.piq_write_ipbuffer.bits.meta.tag := req_tag
  io.piq_write_ipbuffer.bits.meta.index := req_idx
  io.piq_write_ipbuffer.bits.meta.paddr := req.paddr
  io.piq_write_ipbuffer.bits.data := respDataReg.asUInt
  io.piq_write_ipbuffer.bits.data := respDataReg.asUInt
  io.piq_write_ipbuffer.bits.buffIdx := io.id - PortNumber.U

  //mem request
  io.mem_acquire.bits  := edge.Get(
    fromSource      = io.id,
    toAddress       = Cat(req.paddr(PAddrBits - 1, log2Ceil(blockBytes)), 0.U(log2Ceil(blockBytes).W)),
    lgSize          = (log2Up(cacheParams.blockBytes)).U)._2

}
