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



abstract class IPrefetchBundle(implicit p: Parameters) extends ICacheBundle
abstract class IPrefetchModule(implicit p: Parameters) extends ICacheModule

class PIQReq(implicit p: Parameters) extends IPrefetchBundle {
  val paddr      = UInt(PAddrBits.W)
}

class IPrefetchPMPBundle(implicit p: Parameters) extends ICacheBundle{
  val req  = DecoupledIO(new PMPReqBundle())
  val resp = Input(new PMPRespBundle())
}


class IPrefetchToMissUnit(implicit  p: Parameters) extends IPrefetchBundle{
  val enqReq  = DecoupledIO(new PIQReq)
}

class IPredfetchIO(implicit p: Parameters) extends IPrefetchBundle {
  val fromFtq         = Flipped(new FtqPrefechBundle)
  val iTLBInter       = new BlockTlbRequestIO
  val pmp             =   new IPrefetchPMPBundle
  val toIMeta         = Decoupled(new ICacheReadBundle)
  val fromIMeta       = Input(new ICacheMetaRespBundle)
  val toMissUnit     = new IPrefetchToMissUnit
}

class IPrefetchPipe(implicit p: Parameters) extends  IPrefetchModule
{
  val io = IO(new IPredfetchIO)

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
  val p0_vaddr  =   addrAlign(fromFtq.req.bits.target, blockBytes, PAddrBits)
  p0_fire   :=   p0_valid && p1_ready && toITLB.fire() && !fromITLB.bits.miss && toIMeta.ready

  toIMeta.valid     := p0_valid
  toIMeta.bits.vSetIdx(0) := get_idx(p0_vaddr)

  toIMeta.bits.vSetIdx(1) := DontCare
  toIMeta.bits.isDoubleLine := false.B

  toITLB.valid         := p0_valid
  toITLB.bits.size     := 3.U // TODO: fix the size
  toITLB.bits.vaddr    := p0_vaddr
  toITLB.bits.debug.pc := p0_vaddr

  toITLB.bits.cmd                 := TlbCmd.exec
  toITLB.bits.robIdx              := DontCare
  toITLB.bits.debug.isFirstIssue  := DontCare


  fromITLB.ready := true.B

  fromFtq.req.ready :=  p1_ready && GTimer() > 500.U

  /** Prefetch Stage 1: cache probe filter */
  val p1_valid =  generatePipeControl(lastFire = p0_fire, thisFire = p1_fire || p1_discard, thisFlush = false.B, lastFlush = false.B)

  val p1_vaddr   =  RegEnable(next = p0_vaddr,    enable=p0_fire)

  //tlb resp
  val tlb_resp_valid = RegNext(p0_fire)

  val tlb_resp_paddr = ResultHoldBypass(valid = tlb_resp_valid, data = fromITLB.bits.paddr)
  val tlb_resp_pf    = ResultHoldBypass(valid = tlb_resp_valid, data = fromITLB.bits.excp.pf.instr && tlb_resp_valid)
  val tlb_resp_af    = ResultHoldBypass(valid = tlb_resp_valid, data = fromITLB.bits.excp.af.instr && tlb_resp_valid)

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
  p1_fire     :=   p1_valid && p1_req_accept && p2_ready
  p1_discard  :=   p1_valid && p1_req_cancle

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p2_valid =  generatePipeControl(lastFire = p1_fire, thisFire = p2_fire || p2_discard, thisFlush = false.B, lastFlush = false.B)
  val p2_pmp_fire = p2_valid && io.pmp.req.ready
  val pmpExcpAF = fromPMP.instr

  val p2_paddr     = RegEnable(next = tlb_resp_paddr,  enable = p1_fire)
  val p2_except_pf = RegEnable(next =tlb_resp_pf, enable = p1_fire)
  val p2_except_af = DataHoldBypass(pmpExcpAF, p2_pmp_fire) || RegEnable(next = tlb_resp_af, enable = p1_fire)
  val p2_mmio      = DataHoldBypass(io.pmp.resp.mmio && !p2_except_af && !p2_except_pf, p2_pmp_fire)

  //TODO wait PMP logic
  val p2_exception  = VecInit(Seq(pmpExcpAF, p2_mmio)).reduce(_||_)

  io.pmp.req.valid      := p2_pmp_fire
  io.pmp.req.bits.addr  := p2_paddr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  p2_ready :=   p2_fire || p2_discard || !p2_valid
  p2_fire  :=   p2_valid && !p2_exception && p3_ready && p2_pmp_fire
  p2_discard := p2_valid && ((p2_exception && p2_pmp_fire) || !io.pmp.req.ready)

  io.pmp.req.valid      := p2_valid
  io.pmp.req.bits.addr  := p2_paddr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  /** Prefetch Stage 2: filtered req PIQ enqueue */
  val p3_valid =  generatePipeControl(lastFire = p2_fire, thisFire = p3_fire, thisFlush = false.B, lastFlush = false.B)

  val p3_paddr = RegEnable(next = tlb_resp_paddr,  enable = p1_fire)

  toMissUnit.enqReq.valid             := p3_valid
  toMissUnit.enqReq.bits.paddr        := p3_paddr

  p3_ready := toMissUnit.enqReq.ready
  p3_fire  := toMissUnit.enqReq.fire()

}

class IPrefetchEntry(edge: TLEdgeOut, id: Int)(implicit p: Parameters) extends ICacheMissUnitModule
{
  val io = IO(new Bundle {
    val id = Input(UInt(log2Ceil(nPrefetchEntries).W))

    val req = Flipped(DecoupledIO(new PIQReq))

    //tilelink channel
    val mem_hint = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_hint_ack = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))

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


  XSPerfAccumulate(
    "PrefetchEntryPenalty" + Integer.toString(id, 10),
    BoolStopWatch(
      start = io.req.fire(),
      stop = io.mem_hint_ack.fire(),
      startHighPriority = true)
  )
  XSPerfAccumulate("PrefetchEntryReq" + Integer.toString(id, 10), io.req.fire())

}