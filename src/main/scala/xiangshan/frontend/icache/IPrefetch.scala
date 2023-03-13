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
import huancun.{PreferCacheKey, DsidKey}


abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

class PIQReq(implicit p: Parameters) extends IPrefetchBundle {
  val paddr      = UInt(PAddrBits.W)
}


class IPrefetchToMissUnit(implicit  p: Parameters) extends IPrefetchBundle{
  val enqReq  = DecoupledIO(new PIQReq)
}

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  val fromFtq         = Flipped(new FtqPrefechBundle)
  val iTLBInter       = new BlockTlbRequestIO
  val pmp             =   new ICachePMPBundle
  val toIMeta         = DecoupledIO(new ICacheReadBundle)
  val fromIMeta       = Input(new ICacheMetaRespBundle)
  val toMissUnit      = new IPrefetchToMissUnit
  val fromMSHR        = Flipped(Vec(PortNumber,ValidIO(UInt(PAddrBits.W))))

  val prefetchEnable = Input(Bool())
  val prefetchDisable = Input(Bool())
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
  toIMeta.bits.readValid           := DontCare

  toITLB.valid         := p0_valid
  toITLB.bits.size     := 3.U // TODO: fix the size
  toITLB.bits.vaddr    := p0_vaddr
  toITLB.bits.debug.pc := p0_vaddr

  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.robIdx              := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare


  fromITLB.ready := true.B

  fromFtq.req.ready :=  true.B //(!enableBit || (enableBit && p3_ready)) && toIMeta.ready //&& GTimer() > 500.U

  /** Prefetch Stage 1: cache probe filter */
  val p1_valid =  generatePipeControl(lastFire = p0_fire, thisFire = p1_fire || p1_discard, thisFlush = false.B, lastFlush = false.B)

  val p1_vaddr   =  RegEnable(next = p0_vaddr,    enable=p0_fire)

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

  val p2_paddr     = RegEnable(next = tlb_resp_paddr,  enable = p1_fire)
  val p2_except_pf = RegEnable(next =tlb_resp_pf, enable = p1_fire)
  val p2_except_tlb_af = RegEnable(next = tlb_resp_af, enable = p1_fire)

  /*when a prefetch req meet with a miss req in MSHR cancle the prefetch req */
  val p2_check_in_mshr = VecInit(io.fromMSHR.map(mshr => mshr.valid && mshr.bits === addrAlign(p2_paddr, blockBytes, PAddrBits))).reduce(_||_)

  //TODO wait PMP logic
  val p2_exception  = VecInit(Seq(p2_except_tlb_af, p2_except_pf)).reduce(_||_)

  p2_ready :=   p2_fire || p2_discard || !p2_valid
  p2_fire  :=   p2_valid && !p2_exception && p3_ready
  p2_discard := p2_valid && p2_exception

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p3_valid =  generatePipeControl(lastFire = p2_fire, thisFire = p3_fire || p3_discard, thisFlush = false.B, lastFlush = false.B)

  val p3_pmp_fire = p3_valid
  val pmpExcpAF = fromPMP.instr
  val p3_paddr = RegEnable(next = p2_paddr,  enable = p2_fire)

  io.pmp.req.valid      := p3_pmp_fire
  io.pmp.req.bits.addr  := p3_paddr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  val p3_except_pmp_af = DataHoldBypass(pmpExcpAF, p3_pmp_fire) 
  val p3_check_in_mshr = RegEnable(next = p2_check_in_mshr,  enable = p2_fire)
  val p3_mmio      = DataHoldBypass(io.pmp.resp.mmio && !p3_except_pmp_af, p3_pmp_fire)

  val p3_exception  = VecInit(Seq(p3_except_pmp_af, p3_mmio)).reduce(_||_)

  val p3_hit_dir = VecInit((0 until nPrefetchEntries).map(i => prefetch_dir(i).valid && prefetch_dir(i).paddr === p3_paddr )).reduce(_||_)

  p3_discard := p3_exception || p3_hit_dir || p3_check_in_mshr || (p3_valid && enableBit && !toMissUnit.enqReq.ready)

  toMissUnit.enqReq.valid             := p3_valid && enableBit && !p3_discard
  toMissUnit.enqReq.bits.paddr        := p3_paddr

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

class IPrefetchEntry(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends ICacheMissUnitModule
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Ceil(PortNumber + nPrefetchEntries).W))

    val req = Flipped(DecoupledIO(new PIQReq))

    //tilelink channel
    val mem_hint = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_hint_ack = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

    // for lvna
    val dsid = if (hasDsid) Some(Input(UInt(dsidWidth.W))) else None
  })

  /** default value for control signals */
  io.mem_hint.bits := DontCare
  io.mem_hint_ack.ready := true.B


  val s_idle  :: s_send_hint :: s_wait_hint_ack :: Nil = Enum(3)
  val state = RegInit(s_idle)
  /** control logic transformation */
  //request register
  val req = Reg(new PIQReq)
  //initial
  io.mem_hint.bits := DontCare
  io.mem_hint_ack.ready := true.B

  io.req.ready := (state === s_idle)
  io.mem_hint.valid := (state === s_send_hint)

  //state change
  switch(state) {
    is(s_idle) {
      when(io.req.fire()) {
        state := s_send_hint
        req := io.req.bits
      }
    }

    // memory request
    is(s_send_hint) {
      when(io.mem_hint.fire()) {
        state := s_idle
      }
    }
  }

  /** refill write and meta write */
  val hint = edge.Hint(
    fromSource = io.id,
    toAddress = addrAlign(req.paddr, blockBytes, PAddrBits) + blockBytes.U,
    lgSize = (log2Up(cacheParams.blockBytes)).U,
    param = TLHints.PREFETCH_READ
  )._2
  io.mem_hint.bits := hint
  io.mem_hint.bits.user.lift(PreferCacheKey).foreach(_ := true.B)
  if (hasDsid) {
    io.mem_hint.bits.user.lift(DsidKey).foreach(_ := io.dsid.get)
  }


  XSPerfAccumulate("PrefetchEntryReq" + Integer.toString(id, 10), io.req.fire())

}