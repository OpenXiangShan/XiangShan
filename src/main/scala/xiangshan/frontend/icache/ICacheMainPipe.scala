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
import freechips.rocketchip.tilelink.ClientStates
import xiangshan._
import xiangshan.cache.mmu._
import utils._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

class ICacheMainPipeReq(implicit p: Parameters) extends ICacheBundle
{
  val vaddr  = UInt(VAddrBits.W)
  def vsetIdx = get_idx(vaddr)
}

class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle
{
  val vaddr    = UInt(VAddrBits.W)
  val readData = UInt(blockBits.W)
  val paddr    = UInt(PAddrBits.W)
  val tlbExcp  = new Bundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }
}

class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle
{
  val req  = Flipped(DecoupledIO(new ICacheMainPipeReq))
  val resp = ValidIO(new ICacheMainPipeResp)
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIMeta       = Decoupled(new ICacheReadBundle)
  val fromIMeta     = Input(new ICacheMetaRespBundle)
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIData       = Decoupled(new ICacheReadBundle)
  val fromIData     = Input(new ICacheDataRespBundle)
}

class ICacheMSHRBundle(implicit p: Parameters) extends ICacheBundle{
  val toMSHR        = Decoupled(new ICacheMissReq)
  val fromMSHR      = Flipped(ValidIO(new ICacheMissResp))
}

class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle{
  val req  = Valid(new PMPReqBundle())
  val resp = Input(new PMPRespBundle())
}

class ICachePerfInfo(implicit p: Parameters) extends ICacheBundle{
  val only_0_hit     = Bool()
  val only_0_miss    = Bool()
  val hit_0_hit_1    = Bool()
  val hit_0_miss_1   = Bool()
  val miss_0_hit_1   = Bool()
  val miss_0_miss_1  = Bool()
  val bank_hit       = Vec(2,Bool())
  val hit            = Bool()
}

class ICacheMainPipeInterface(implicit p: Parameters) extends ICacheBundle {
  /* internal interface */
  val metaArray   = new ICacheMetaReqBundle
  val dataArray   = new ICacheDataReqBundle
  val mshr        = Vec(PortNumber, new ICacheMSHRBundle)
  /* outside interface */
  val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  val pmp         = Vec(PortNumber, new ICachePMPBundle)
  val itlb        = Vec(PortNumber, new BlockTlbRequestIO)
  val respStall   = Input(Bool())
//  val toReleaseUnit = Vec(2, Decoupled(new ReleaseReq))
//  val victimInfor = new Bundle() {
//    val s1 = Vec(2, Output(new ICacheVictimInfor()))
//    val s2 = Vec(2, Output(new ICacheVictimInfor()))
//  }
//  val setInfor    = new Bundle(){
//    val s1 = Vec(2, Output(new ICacheSetInfor()))
//    val s2 = Vec(2, Output(new ICacheSetInfor()))
//  }
  val perfInfo = Output(new ICachePerfInfo)
}

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new ICacheMainPipeInterface)

  val (fromIFU, toIFU)    = (io.fetch.map(_.req), io.fetch.map(_.resp))
  val (toMeta, toData, metaResp, dataResp) =  (io.metaArray.toIMeta, io.dataArray.toIData, io.metaArray.fromIMeta, io.dataArray.fromIData)
  val (toMSHR, fromMSHR)  = (io.mshr.map(_.toMSHR), io.mshr.map(_.fromMSHR))
  val (toITLB, fromITLB)  = (io.itlb.map(_.req), io.itlb.map(_.resp))
  val (toPMP,  fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))

  val s0_ready, s1_ready, s2_ready = WireInit(false.B)
  val s0_fire,  s1_fire , s2_fire  = WireInit(false.B)

  //Stage 1
  val s0_valid       = fromIFU.map(_.valid).reduce(_||_)
  val s0_req_vaddr   = VecInit(fromIFU.map(_.bits.vaddr))
  val s0_req_vsetIdx = VecInit(fromIFU.map(_.bits.vsetIdx))
  val s0_only_fisrt  = fromIFU(0).valid && !fromIFU(0).valid
  val s0_double_line = fromIFU(0).valid && fromIFU(1).valid

  s0_fire        := s0_valid && s1_ready

  //fetch: send addr to Meta/TLB and Data simultaneously
  val fetch_req = List(toMeta, toData)
  for(i <- 0 until 2) {
    fetch_req(i).valid             := s0_valid
    fetch_req(i).bits.isDoubleLine := s0_double_line
    fetch_req(i).bits.vSetIdx      := s0_req_vsetIdx
  }
  //TODO: fix GTimer() condition
  fromIFU.map(_.ready := fetch_req(0).ready && fetch_req(1).ready && s1_ready && GTimer() > 500.U)


//  XSPerfAccumulate("ifu_bubble_ftq_not_valid",   !f0_valid )
//  XSPerfAccumulate("ifu_bubble_pipe_stall",    f0_valid && fetch_req(0).ready && fetch_req(1).ready && !s1_ready )
//  XSPerfAccumulate("ifu_bubble_sram_0_busy",   f0_valid && !fetch_req(0).ready  )
//  XSPerfAccumulate("ifu_bubble_sram_1_busy",   f0_valid && !fetch_req(1).ready  )

  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * Send req to ITLB and TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------

  //TODO: handle fetch exceptions

  val tlbRespAllValid = WireInit(false.B)

  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = false.B, lastFlush = false.B)

  val s1_req_vaddr   = RegEnable(next = s0_req_vaddr,    enable = s0_fire)
  val s1_req_vsetIdx = RegEnable(next = s0_req_vsetIdx, enable = s0_fire)
  val s1_only_fisrt  = RegEnable(next = s0_only_fisrt, enable = s0_fire)
  val s1_double_line = RegEnable(next = s0_double_line, enable = s0_fire)

  s1_ready := s2_ready && tlbRespAllValid  || !s1_valid
  s1_fire  := s1_valid && tlbRespAllValid && s2_ready

  toITLB(0).valid         := s1_valid
  toITLB(0).bits.size     := 3.U // TODO: fix the size
  toITLB(0).bits.vaddr    := s1_req_vaddr(0)
  toITLB(0).bits.debug.pc := s1_req_vaddr(0)

  toITLB(1).valid         := s1_valid && s1_double_line
  toITLB(1).bits.size     := 3.U // TODO: fix the size
  toITLB(1).bits.vaddr    := s1_req_vaddr(1)
  toITLB(1).bits.debug.pc := s1_req_vaddr(1)

  toITLB.map{port =>
    port.bits.cmd                 := TlbCmd.exec
    port.bits.robIdx              := DontCare
    port.bits.debug.isFirstIssue  := DontCare
  }

  fromITLB.map(_.ready := true.B)

  val (tlbRespValid, tlbRespPAddr) = (fromITLB.map(_.valid), VecInit(fromITLB.map(_.bits.paddr)))
  val (tlbRespMiss)  = fromITLB.map(port => port.bits.miss && port.valid)
  val (tlbExcpPF,    tlbExcpAF)    = (fromITLB.map(port => port.bits.excp.pf.instr && port.valid),
                                        fromITLB.map(port => (port.bits.excp.af.instr) && port.valid))

  tlbRespAllValid := tlbRespValid(0)  && (tlbRespValid(1) || !s1_double_line)

  val s1_req_paddr              = tlbRespPAddr
  val s1_req_ptags              = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  val s1_meta_ptags              = ResultHoldBypass(data = metaResp.tags, valid = RegNext(s0_fire))
  val s1_meta_cohs               = ResultHoldBypass(data = metaResp.cohs, valid = RegNext(s0_fire))
  val s1_data_cacheline          = ResultHoldBypass(data = dataResp.datas, valid = RegNext(s0_fire))

  val s1_tag_eq_vec        = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w =>  s1_meta_ptags(p)(w) ===  s1_req_ptags(p) ))))
  val s1_tag_match_vec     = VecInit((0 until PortNumber).map( k => VecInit(s1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_cohs(k)(w).isValid()})))
  val s1_tag_match         = VecInit(s1_tag_match_vec.map(vector => ParallelOR(vector)))

  val s1_port_hit          = VecInit(Seq(s1_tag_match(0) && s1_valid  && !tlbExcpPF(0) && !tlbExcpAF(0),  s1_tag_match(1) && s1_valid && s1_double_line && !tlbExcpPF(1) && !tlbExcpAF(1) ))
  val s1_bank_miss         = VecInit(Seq(!s1_tag_match(0) && s1_valid && !tlbExcpPF(0) && !tlbExcpAF(0), !s1_tag_match(1) && s1_valid && s1_double_line && !tlbExcpPF(1) && !tlbExcpAF(1) ))
  val s1_hit               = (s1_port_hit(0) && s1_port_hit(1)) || (!s1_double_line && s1_port_hit(0))

  /** choose victim cacheline */
  val replacers       = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/PortNumber))
  val s1_victim_oh    = ResultHoldBypass(data = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(s1_req_vsetIdx(i)))}), valid = RegNext(s0_fire))

  val s1_victim_coh   = VecInit(s1_victim_oh.zipWithIndex.map {case(oh, port) => Mux1H(oh, s1_meta_cohs(port))})
//  val s1_victim_tag   = VecInit(s1_victim_oh.zipWithIndex.map {case(oh, port) => Mux1H(oh, s1_meta_ptags(port))})
//  val s1_victim_data  = VecInit(s1_victim_oh.zipWithIndex.map {case(oh, port) => Mux1H(oh, s1_data_cacheline(port))})
//  val s1_need_replace = VecInit(s1_victim_coh.zipWithIndex.map{case(coh, port) => coh.isValid() && s1_bank_miss(port)})
//
//  (0 until PortNumber).map{ i =>
//    io.victimInfor.s1(i).valid := s1_valid && s1_need_replace(i)
//    io.victimInfor.s1(i).ptag  := s1_victim_tag(i)
//    io.victimInfor.s1(i).vidx  := get_idx(s1_req_vaddr(i))
//  }

//  (0 until PortNumber).map{ i =>
//    io.setInfor.s1(i).valid := s1_bank_miss(i)
//    io.setInfor.s1(i).vidx  := s1_req_vsetIdx(i)
//  }

  assert(PopCount(s1_tag_match_vec(0)) <= 1.U && PopCount(s1_tag_match_vec(1)) <= 1.U, "Multiple hit in main pipe")

  val touch_sets = Seq.fill(2)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))) )

  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}

  val s1_hit_data      =  VecInit(s1_data_cacheline.zipWithIndex.map { case(bank, i) =>
    val port_hit_data = Mux1H(s1_tag_match_vec(i).asUInt, bank)
    port_hit_data
  })

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_hit_way_" + Integer.toString(w, 10),  s1_fire && s1_port_hit(0) && OHToUInt(s1_tag_match_vec(0))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_victim_way_" + Integer.toString(w, 10),  s1_fire && !s1_port_hit(0) && OHToUInt(s1_victim_oh(0))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_1_hit_way_" + Integer.toString(w, 10),  s1_fire && s1_double_line && s1_port_hit(1) && OHToUInt(s1_tag_match_vec(1))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_1_victim_way_" + Integer.toString(w, 10),  s1_fire && s1_double_line && !s1_port_hit(1) && OHToUInt(s1_victim_oh(1))  === w.U)
  }

  XSPerfAccumulate("ifu_bubble_s1_tlb_miss",    s1_valid && !tlbRespAllValid )

  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * get data from last stage (hit from s1_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val s2_fetch_finish = Wire(Bool())

  val s2_valid          = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = false.B, lastFlush = false.B)
  val s2_miss_available = Wire(Bool())

  s2_ready      := (s2_valid && s2_fetch_finish && !io.respStall) || (!s2_valid && s2_miss_available)
  s2_fire       := s2_valid && s2_fetch_finish && !io.respStall

  val pmpExcpAF = fromPMP.map(port => port.instr)
  val mmio = fromPMP.map(port => port.mmio) // TODO: handle it

  val (s2_req_paddr , s2_req_vaddr)   = (RegEnable(next = s1_req_paddr, enable = s1_fire), RegEnable(next = s1_req_vaddr, enable = s1_fire))
  val s2_req_vsetIdx  = RegEnable(next = s1_req_vsetIdx, enable = s1_fire)
  val s2_req_ptags    = RegEnable(next = s1_req_ptags, enable = s1_fire)
  val s2_only_fisrt   = RegEnable(next = s1_only_fisrt, enable = s1_fire)
  val s2_double_line  = RegEnable(next = s1_double_line, enable = s1_fire)
  val s2_hit          = RegEnable(next = s1_hit   , enable = s1_fire)
  val s2_port_hit     = RegEnable(next = s1_port_hit, enable = s1_fire)
  val s2_bank_miss    = RegEnable(next = s1_bank_miss, enable = s1_fire)

  val sec_meet_vec = Wire(Vec(2, Bool()))
  val s2_fixed_hit_vec = VecInit((0 until 2).map(i => s2_port_hit(i) || sec_meet_vec(i)))
  val s2_fixed_hit = (s2_valid && s2_fixed_hit_vec(0) && s2_fixed_hit_vec(1) && s2_double_line) || (s2_valid && s2_fixed_hit_vec(0) && !s2_double_line)

  //replacement
  val s2_waymask      = RegEnable(next = s1_victim_oh, enable = s1_fire)
  val s2_victim_coh   = RegEnable(next = s1_victim_coh, enable = s1_fire)
//  val s2_victim_tag   = RegEnable(next = s1_victim_tag, enable = s1_fire)
//  val s2_victim_data  = RegEnable(next = s1_victim_data,  enable = s1_fire)
//  val s2_need_replace = RegEnable(next = s1_need_replace,  enable = s1_fire)
//  val s2_has_replace  = s2_need_replace.asUInt.orR

  /** exception and pmp logic **/
  //exception information
  val s2_except_pf = RegEnable(next = VecInit(tlbExcpPF), enable = s1_fire)
  val s2_except_af = VecInit(RegEnable(next = VecInit(tlbExcpAF), enable = s1_fire).zip(pmpExcpAF).map(a => a._1 || DataHoldBypass(a._2, RegNext(s1_fire)).asBool))
  val s2_except    = VecInit((0 until 2).map{i => s2_except_pf(i) || s2_except_af(i)})
  val s2_has_except = s2_valid && (s2_except_af.reduce(_||_) || s2_except_pf.reduce(_||_))
  //MMIO
  val s2_mmio      = DataHoldBypass(io.pmp(0).resp.mmio && !s2_except_af(0) && !s2_except_pf(0), RegNext(s1_fire)).asBool()

  io.pmp.zipWithIndex.map { case (p, i) =>
    p.req.valid := s2_fire
    p.req.bits.addr := s2_req_paddr(i)
    p.req.bits.size := 3.U // TODO
    p.req.bits.cmd := TlbCmd.exec
  }

  /*** cacheline miss logic ***/
  val wait_idle :: wait_queue_ready :: wait_send_req  :: wait_two_resp :: wait_0_resp :: wait_1_resp :: wait_one_resp ::wait_finish :: Nil = Enum(8)
  val wait_state = RegInit(wait_idle)

  val port_miss_fix  = VecInit(Seq(fromMSHR(0).fire() && !s2_port_hit(0),   fromMSHR(1).fire() && s2_double_line && !s2_port_hit(1) ))

  class MissSlot(implicit p: Parameters) extends  XSBundle with HasICacheParameters {
    val m_vSetIdx   = UInt(idxBits.W)
    val m_pTag      = UInt(tagBits.W)
    val m_data      = UInt(blockBits.W)
  }

  val missSlot    = Seq.fill(2)(RegInit(0.U.asTypeOf(new MissSlot)))
  val m_invalid :: m_valid :: m_refilled :: m_flushed :: m_wait_sec_miss :: m_check_final ::Nil = Enum(6)
  val missStateQueue = RegInit(VecInit(Seq.fill(2)(m_invalid)) )
  val reservedRefillData = Wire(Vec(2, UInt(blockBits.W)))

  s2_miss_available :=  VecInit(missStateQueue.map(entry => entry === m_invalid  || entry === m_wait_sec_miss)).reduce(_&&_)

  val fix_sec_miss     = Wire(Vec(4, Bool()))
  val sec_meet_0_miss = fix_sec_miss(0) || fix_sec_miss(2)
  val sec_meet_1_miss = fix_sec_miss(1) || fix_sec_miss(3)
  sec_meet_vec := VecInit(Seq(sec_meet_0_miss,sec_meet_1_miss ))
  
  //only raise at the first cycle of s2_valid
  val  only_0_miss      = RegNext(s1_fire) && !s2_hit && !s2_double_line && !s2_has_except && !sec_meet_0_miss && !s2_mmio
  val  only_0_hit       = RegNext(s1_fire) && s2_hit && !s2_double_line && !s2_mmio
  val  hit_0_hit_1      = RegNext(s1_fire) && s2_hit && s2_double_line && !s2_mmio
  val  hit_0_miss_1     = RegNext(s1_fire) && !s2_port_hit(1) && !sec_meet_1_miss && (s2_port_hit(0) || sec_meet_0_miss) && s2_double_line  && !s2_has_except && !s2_mmio
  val  miss_0_hit_1     = RegNext(s1_fire) && !s2_port_hit(0) && !sec_meet_0_miss && (s2_port_hit(1) || sec_meet_1_miss) && s2_double_line  && !s2_has_except && !s2_mmio
  val  miss_0_miss_1    = RegNext(s1_fire) && !s2_port_hit(0) && !s2_port_hit(1) && !sec_meet_0_miss && !sec_meet_1_miss && s2_double_line  && !s2_has_except && !s2_mmio

  val  hit_0_except_1   = RegNext(s1_fire) && s2_double_line &&  !s2_except(0) && s2_except(1)  &&  s2_port_hit(0)
  val  miss_0_except_1  = RegNext(s1_fire) && s2_double_line &&  !s2_except(0) && s2_except(1)  && !s2_port_hit(0)
  val  except_0         = RegNext(s1_fire) && s2_except(0)

  def holdReleaseLatch(valid: Bool, release: Bool, flush: Bool): Bool ={
    val bit = RegInit(false.B)
    when(flush)                   { bit := false.B  }
      .elsewhen(valid && !release)  { bit := true.B  }
      .elsewhen(release)            { bit := false.B}
    bit || valid
  }

  val  miss_0_hit_1_latch     =   holdReleaseLatch(valid = miss_0_hit_1,    release = s2_fire,      flush = false.B)
  val  miss_0_miss_1_latch    =   holdReleaseLatch(valid = miss_0_miss_1,   release = s2_fire,      flush = false.B)
  val  only_0_miss_latch      =   holdReleaseLatch(valid = only_0_miss,     release = s2_fire,      flush = false.B)
  val  hit_0_miss_1_latch     =   holdReleaseLatch(valid = hit_0_miss_1,    release = s2_fire,      flush = false.B)

  val  miss_0_except_1_latch  =   holdReleaseLatch(valid = miss_0_except_1, release = s2_fire,      flush = false.B)
  val  except_0_latch          =   holdReleaseLatch(valid = except_0,    release = s2_fire,      flush = false.B)
  val  hit_0_except_1_latch         =    holdReleaseLatch(valid = hit_0_except_1,    release = s2_fire,      flush = false.B)

  val only_0_hit_latch        = holdReleaseLatch(valid = only_0_hit,   release = s2_fire,      flush = false.B)
  val hit_0_hit_1_latch        = holdReleaseLatch(valid = hit_0_hit_1,   release = s2_fire,      flush = false.B)


  def waitSecondComeIn(missState: UInt): Bool = (missState === m_wait_sec_miss)

  // deal with secondary miss when s1 enter f2
  def getMissSituat(slotNum : Int, missNum : Int ) :Bool =  {
    RegNext(s1_fire) && (missSlot(slotNum).m_vSetIdx === s2_req_vsetIdx(missNum)) && (missSlot(slotNum).m_pTag  === s2_req_ptags(missNum)) && !s2_port_hit(missNum)  && waitSecondComeIn(missStateQueue(slotNum)) && !s2_mmio
  }

  val miss_0_s2_0 =   getMissSituat(slotNum = 0, missNum = 0)
  val miss_0_s2_1 =   getMissSituat(slotNum = 0, missNum = 1)
  val miss_1_s2_0 =   getMissSituat(slotNum = 1, missNum = 0)
  val miss_1_s2_1 =   getMissSituat(slotNum = 1, missNum = 1)

  val miss_0_s2_0_latch =   holdReleaseLatch(valid = miss_0_s2_0,    release = s2_fire,      flush = false.B)
  val miss_0_s2_1_latch =   holdReleaseLatch(valid = miss_0_s2_1,    release = s2_fire,      flush = false.B)
  val miss_1_s2_0_latch =   holdReleaseLatch(valid = miss_1_s2_0,    release = s2_fire,      flush = false.B)
  val miss_1_s2_1_latch =   holdReleaseLatch(valid = miss_1_s2_1,    release = s2_fire,      flush = false.B)


  val slot_0_solve = fix_sec_miss(0) || fix_sec_miss(1)
  val slot_1_solve = fix_sec_miss(2) || fix_sec_miss(3)
  val slot_slove   = VecInit(Seq(slot_0_solve, slot_1_solve))

  fix_sec_miss   := VecInit(Seq(miss_0_s2_0_latch, miss_0_s2_1_latch, miss_1_s2_0_latch, miss_1_s2_1_latch))

  reservedRefillData(0) := DataHoldBypass(data = missSlot(0).m_data, valid = miss_0_s2_0 || miss_0_s2_1)
  reservedRefillData(1) := DataHoldBypass(data = missSlot(1).m_data, valid = miss_1_s2_0 || miss_1_s2_1)

  switch(wait_state){
    is(wait_idle){
      when(miss_0_except_1_latch){
        wait_state :=  Mux(toMSHR(0).ready, wait_queue_ready ,wait_idle )
      }.elsewhen( only_0_miss_latch  || miss_0_hit_1_latch){
        wait_state :=  Mux(toMSHR(0).ready, wait_queue_ready ,wait_idle )
      }.elsewhen(hit_0_miss_1_latch){
        wait_state :=  Mux(toMSHR(1).ready, wait_queue_ready ,wait_idle )
      }.elsewhen( miss_0_miss_1_latch ){
        wait_state := Mux(toMSHR(0).ready && toMSHR(1).ready, wait_queue_ready ,wait_idle)
      }
    }

    is(wait_queue_ready){
      wait_state := wait_send_req
    }

    is(wait_send_req) {
      when(miss_0_except_1_latch || only_0_miss_latch || hit_0_miss_1_latch || miss_0_hit_1_latch){
        wait_state :=  wait_one_resp
      }.elsewhen( miss_0_miss_1_latch ){
        wait_state := wait_two_resp
      }
    }

    is(wait_one_resp) {
      when( (miss_0_except_1_latch ||only_0_miss_latch || miss_0_hit_1_latch) && fromMSHR(0).fire()){
        wait_state := wait_finish
      }.elsewhen( hit_0_miss_1_latch && fromMSHR(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_two_resp) {
      when(fromMSHR(0).fire() && fromMSHR(1).fire()){
        wait_state := wait_finish
      }.elsewhen( !fromMSHR(0).fire() && fromMSHR(1).fire() ){
        wait_state := wait_0_resp
      }.elsewhen(fromMSHR(0).fire() && !fromMSHR(1).fire()){
        wait_state := wait_1_resp
      }
    }

    is(wait_0_resp) {
      when(fromMSHR(0).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_1_resp) {
      when(fromMSHR(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_finish) {when(s2_fire) {wait_state := wait_idle }
    }
  }


  (0 until 2).map { i =>
    if(i == 1) toMSHR(i).valid   := (hit_0_miss_1_latch || miss_0_miss_1_latch) && wait_state === wait_queue_ready && !s2_mmio
        else     toMSHR(i).valid := (only_0_miss_latch || miss_0_hit_1_latch || miss_0_miss_1_latch || miss_0_except_1_latch) && wait_state === wait_queue_ready && !s2_mmio
    toMSHR(i).bits.paddr    := s2_req_paddr(i)
    toMSHR(i).bits.vaddr    := s2_req_vaddr(i)
    toMSHR(i).bits.waymask  := s2_waymask(i)
    toMSHR(i).bits.coh      := s2_victim_coh(i)


    when(toMSHR(i).fire() && missStateQueue(i) === m_invalid){
      missStateQueue(i)     := m_valid
      missSlot(i).m_vSetIdx := s2_req_vsetIdx(i)
      missSlot(i).m_pTag    := get_phy_tag(s2_req_paddr(i))
    }

    when(fromMSHR(i).fire() && missStateQueue(i) === m_valid ){
      missStateQueue(i)     := m_refilled
      missSlot(i).m_data    := fromMSHR(i).bits.data
    }


    when(s2_fire && missStateQueue(i) === m_refilled){
      missStateQueue(i)     := m_wait_sec_miss
    }

    //only the first cycle to check whether meet the secondary miss
    when(missStateQueue(i) === m_wait_sec_miss){
      //the seondary req has been fix by this slot and another also hit || the secondary req for other cacheline and hit
      when((slot_slove(i) && s2_fire) || (!slot_slove(i) && s2_fire) ) {
        missStateQueue(i)     := m_invalid
      }
      //the seondary req has been fix by this slot but another miss/f3 not ready || the seondary req for other cacheline and miss
      .elsewhen((slot_slove(i) && !s2_fire && s2_valid) ||  (s2_valid && !slot_slove(i) && !s2_fire) ){
        missStateQueue(i)     := m_check_final
      }
    }

    when(missStateQueue(i) === m_check_final && toMSHR(i).fire()){
      missStateQueue(i)     :=  m_valid
      missSlot(i).m_vSetIdx := s2_req_vsetIdx(i)
      missSlot(i).m_pTag    := get_phy_tag(s2_req_paddr(i))
    }.elsewhen(missStateQueue(i) === m_check_final) {
      missStateQueue(i)     :=  m_invalid
    }
  }


//   val release_idle  :: release_wait_fire ::Nil = Enum(2)
//   val release_state = RegInit(VecInit(Seq.fill(2)(release_idle)) )
//   val s2_need_release =  VecInit((0 until PortNumber).map(i =>s2_valid && s2_need_replace(i) && !s2_mmio && !s2_except_af(i) && !s2_except_pf(i)))
//
//   val toRealseUnit = io.toReleaseUnit
  

//  (0 until 2).map{ i =>
//    switch(release_state(i)){
//     is(release_idle){
//       when(s2_need_release(i)){
//         release_state(i) := Mux(toRealseUnit(i).fire() , release_wait_fire ,release_idle )
//       }
//     }
//
//     is(release_wait_fire){
//       when(s2_fire){ release_state(i) := release_idle}
//     }
//   }

//    toRealseUnit(i).valid          := s2_valid && s2_need_release(i) && (release_state(i) === release_idle)
//    toRealseUnit(i).bits.addr      := get_block_addr(Cat(s2_victim_tag(i), get_untag(s2_req_vaddr(i))) )
//    toRealseUnit(i).bits.param     := s2_victim_coh(i).onCacheControl(M_FLUSH)._2
//    toRealseUnit(i).bits.voluntary := true.B
//    toRealseUnit(i).bits.hasData   := s2_victim_coh(i) === ClientStates.Dirty
//    toRealseUnit(i).bits.dirty     := s2_victim_coh(i) === ClientStates.Dirty
//    toRealseUnit(i).bits.data      := s2_victim_data(i)
//    toRealseUnit(i).bits.waymask   := s2_waymask(i)
//    toRealseUnit(i).bits.vidx      := s2_req_vsetIdx(i)
//  }

//  (0 until PortNumber).map{ i =>
//    io.victimInfor.s2(i).valid := s2_valid && s2_need_release(i)
//    io.victimInfor.s2(i).ptag  := s2_victim_tag(i)
//    io.victimInfor.s2(i).vidx  := get_idx(s2_req_vaddr(i))
//  }
//
//  (0 until PortNumber).map{ i =>
//    io.setInfor.s2(i).valid := s2_bank_miss(i) && s2_valid
//    io.setInfor.s2(i).vidx  := s1_req_vsetIdx(i)
//  }

  val miss_all_fix       =  wait_state === wait_finish
//  val release_all_fix    =  VecInit((0 until PortNumber).map(i => !s2_need_release(i) || release_state(i) === release_wait_fire))
  s2_fetch_finish        := ((s2_valid && s2_fixed_hit) || miss_all_fix || hit_0_except_1_latch || except_0_latch || s2_mmio) //&& release_all_fix.reduce(_&&_)

  XSPerfAccumulate("ifu_bubble_s2_miss",    s2_valid && !s2_fetch_finish )

  (touch_ways zip touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    t_s(0)         := s1_req_vsetIdx(i)
    t_w(0).valid   := s1_port_hit(i)
    t_w(0).bits    := OHToUInt(s1_tag_match_vec(i))

    t_s(1)         := s2_req_vsetIdx(i)
    t_w(1).valid   := s2_valid && !s2_port_hit(i)
    t_w(1).bits    := OHToUInt(s2_waymask(i))
  }

  val s2_hit_datas    = RegEnable(next = s1_hit_data, enable = s1_fire)
  val s2_datas        = Wire(Vec(2, UInt(blockBits.W)))

  s2_datas.zipWithIndex.map{case(bank,i) =>
    if(i == 0) bank := Mux(s2_port_hit(i), s2_hit_datas(i),Mux(miss_0_s2_0_latch,reservedRefillData(0), Mux(miss_1_s2_0_latch,reservedRefillData(1), missSlot(0).m_data)))
    else    bank := Mux(s2_port_hit(i), s2_hit_datas(i),Mux(miss_0_s2_1_latch,reservedRefillData(0), Mux(miss_1_s2_1_latch,reservedRefillData(1), missSlot(1).m_data)))
  }


  (0 until PortNumber).map{ i =>
    if(i ==0) toIFU(i).valid          := s2_fire
       else   toIFU(i).valid          := s2_fire && s2_double_line
    toIFU(i).bits.readData  := s2_datas(i)
    toIFU(i).bits.paddr     := s2_req_paddr(i)
    toIFU(i).bits.vaddr     := s2_req_vaddr(i)
    toIFU(i).bits.tlbExcp.pageFault     := s2_except_pf(i)
    toIFU(i).bits.tlbExcp.accessFault   := s2_except_af(i)
    toIFU(i).bits.tlbExcp.mmio          := s2_mmio
  }

  io.perfInfo.only_0_hit    := only_0_miss_latch
  io.perfInfo.only_0_miss   := only_0_miss_latch
  io.perfInfo.hit_0_hit_1   := hit_0_hit_1_latch
  io.perfInfo.hit_0_miss_1  := hit_0_miss_1_latch
  io.perfInfo.miss_0_hit_1  := miss_0_hit_1_latch
  io.perfInfo.miss_0_miss_1 := miss_0_miss_1_latch
  io.perfInfo.bank_hit(0)   := only_0_miss_latch  || hit_0_hit_1_latch || hit_0_miss_1_latch || hit_0_except_1_latch
  io.perfInfo.bank_hit(1)   := miss_0_hit_1_latch || hit_0_hit_1_latch 
  io.perfInfo.hit           := hit_0_hit_1_latch
}
