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
import xiangshan.frontend.{FtqICacheInfo, FtqToICacheRequestBundle}

class ICacheMainPipeReq(implicit p: Parameters) extends ICacheBundle
{
  val vaddr  = UInt(VAddrBits.W)
  def vsetIdx = get_idx(vaddr)
}

class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle
{
  val vaddr    = UInt(VAddrBits.W)
  val registerData = UInt(blockBits.W)
  val sramData = UInt(blockBits.W)
  val select   = Bool()
  val paddr    = UInt(PAddrBits.W)
  val tlbExcp  = new Bundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }
}

class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle
{
  val req  = Flipped(Decoupled(new FtqToICacheRequestBundle))
  val resp = Vec(PortNumber, ValidIO(new ICacheMainPipeResp))
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIMeta       = DecoupledIO(new ICacheReadBundle)
  val fromIMeta     = Input(new ICacheMetaRespBundle)
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIData       = DecoupledIO(Vec(partWayNum, new ICacheReadBundle))
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
  val hit_0_except_1 = Bool()
  val miss_0_except_1 = Bool()
  val except_0       = Bool()
  val bank_hit       = Vec(2,Bool())
  val hit            = Bool()
}

class ICacheMainPipeInterface(implicit p: Parameters) extends ICacheBundle {
  /*** internal interface ***/
  val metaArray   = new ICacheMetaReqBundle
  val dataArray   = new ICacheDataReqBundle
  val mshr        = Vec(PortNumber, new ICacheMSHRBundle)
  val errors      = Output(Vec(PortNumber, new L1CacheErrorInfo))
  /*** outside interface ***/
  //val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  /* when ftq.valid is high in T + 1 cycle 
   * the ftq component must be valid in T cycle
   */
  val fetch       = new ICacheMainPipeBundle
  val pmp         = Vec(PortNumber, new ICachePMPBundle)
  val itlb        = Vec(PortNumber, new TlbRequestIO)
  val respStall   = Input(Bool())
  val perfInfo = Output(new ICachePerfInfo)

  val prefetchEnable = Output(Bool())
  val prefetchDisable = Output(Bool())
  val csr_parity_enable = Input(Bool())

}

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new ICacheMainPipeInterface)

  /** Input/Output port */
  val (fromFtq, toIFU)    = (io.fetch.req, io.fetch.resp)
  val (toMeta, metaResp)  = (io.metaArray.toIMeta, io.metaArray.fromIMeta)
  val (toData, dataResp)  = (io.dataArray.toIData,  io.dataArray.fromIData)
  val (toMSHR, fromMSHR)  = (io.mshr.map(_.toMSHR), io.mshr.map(_.fromMSHR))
  val (toITLB, fromITLB)  = (io.itlb.map(_.req), io.itlb.map(_.resp))
  val (toPMP,  fromPMP)   = (io.pmp.map(_.req), io.pmp.map(_.resp))
  io.itlb.foreach(_.req_kill := false.B)

  //Ftq RegNext Register
  val fromFtqReq = fromFtq.bits.pcMemRead
  
  /** pipeline control signal */
  val s1_ready, s2_ready = Wire(Bool())
  val s0_fire,  s1_fire , s2_fire  = Wire(Bool())

  val missSwitchBit = RegInit(false.B)

  /** replacement status register */
  val touch_sets = Seq.fill(2)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))) )

  /**
    ******************************************************************************
    * ICache Stage 0
    * - send req to ITLB and wait for tlb miss fixing
    * - send req to Meta/Data SRAM
    ******************************************************************************
    */

  /** s0 control */
  val s0_valid       = fromFtq.valid
  val s0_req_vaddr   = (0 until partWayNum + 1).map(i => VecInit(Seq(fromFtqReq(i).startAddr, fromFtqReq(i).nextlineStart)))
  val s0_req_vsetIdx = (0 until partWayNum + 1).map(i => VecInit(s0_req_vaddr(i).map(get_idx(_))))
  val s0_only_first  = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && !fromFtqReq(i).crossCacheline)
  val s0_double_line = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) &&  fromFtqReq(i).crossCacheline)

  val s0_final_valid        = s0_valid
  val s0_final_vaddr        = s0_req_vaddr.head
  val s0_final_vsetIdx      = s0_req_vsetIdx.head
  val s0_final_only_first   = s0_only_first.head
  val s0_final_double_line  = s0_double_line.head

  /** SRAM request */
  //0 -> metaread, 1,2,3 -> data, 3 -> code 4 -> itlb
  val ftq_req_to_data_doubleline  = s0_double_line.init
  val ftq_req_to_data_vset_idx    = s0_req_vsetIdx.init
  val ftq_req_to_data_valid       = fromFtq.bits.readValid.init

  val ftq_req_to_meta_doubleline  = s0_double_line.head
  val ftq_req_to_meta_vset_idx    = s0_req_vsetIdx.head

  val ftq_req_to_itlb_only_first  = s0_only_first.last
  val ftq_req_to_itlb_doubleline  = s0_double_line.last
  val ftq_req_to_itlb_vaddr       = s0_req_vaddr.last
  val ftq_req_to_itlb_vset_idx    = s0_req_vsetIdx.last


  for(i <- 0 until partWayNum) {
    toData.valid                  := ftq_req_to_data_valid(i) && !missSwitchBit
    toData.bits(i).isDoubleLine   := ftq_req_to_data_doubleline(i)
    toData.bits(i).vSetIdx        := ftq_req_to_data_vset_idx(i)
  }

  toMeta.valid               := s0_valid && !missSwitchBit
  toMeta.bits.isDoubleLine   := ftq_req_to_meta_doubleline
  toMeta.bits.vSetIdx        := ftq_req_to_meta_vset_idx


  toITLB(0).valid         := s0_valid  
  toITLB(0).bits.size     := 3.U // TODO: fix the size
  toITLB(0).bits.vaddr    := ftq_req_to_itlb_vaddr(0)
  toITLB(0).bits.debug.pc := ftq_req_to_itlb_vaddr(0)

  toITLB(1).valid         := s0_valid && ftq_req_to_itlb_doubleline
  toITLB(1).bits.size     := 3.U // TODO: fix the size
  toITLB(1).bits.vaddr    := ftq_req_to_itlb_vaddr(1)
  toITLB(1).bits.debug.pc := ftq_req_to_itlb_vaddr(1)

  toITLB.map{port =>
    port.bits.cmd                 := TlbCmd.exec
    port.bits.debug.robIdx        := DontCare
    port.bits.debug.isFirstIssue  := DontCare
  }

  /** ITLB & ICACHE sync case
   * when icache is not ready, but itlb is ready
   * because itlb is non-block, then the req will take the port
   * then itlb will unset the ready?? itlb is wrongly blocked.
   * Solution: maybe give itlb a signal to tell whether acquire the slot?
   */

  val itlb_can_go    = toITLB(0).ready && toITLB(1).ready
  val icache_can_go  = toData.ready && toMeta.ready
  val pipe_can_go    = !missSwitchBit && s1_ready
  val s0_can_go      = itlb_can_go && icache_can_go && pipe_can_go
  val s0_fetch_fire  = s0_valid && s0_can_go
  s0_fire        := s0_fetch_fire
  toITLB.map{port => port.bits.kill := !icache_can_go || !pipe_can_go}

  //TODO: fix GTimer() condition
  fromFtq.ready := s0_can_go

  /**
    ******************************************************************************
    * ICache Stage 1
    * - get tlb resp data (exceptiong info and physical addresses)
    * - get Meta/Data SRAM read responses (latched for pipeline stop)
    * - tag compare/hit check
    ******************************************************************************
    */

  /** s1 control */

  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = false.B, lastFlush = false.B)

  val s1_req_vaddr   = RegEnable(s0_final_vaddr, s0_fire)
  val s1_req_vsetIdx = RegEnable(s0_final_vsetIdx, s0_fire)
  val s1_only_first  = RegEnable(s0_final_only_first, s0_fire)
  val s1_double_line = RegEnable(s0_final_double_line, s0_fire)

  /** tlb response latch for pipeline stop */
  val tlb_back = fromITLB.map(_.fire())
  val tlb_need_back = VecInit((0 until PortNumber).map(i => ValidHold(s0_fire && toITLB(i).fire(), s1_fire, false.B)))
  val tlb_already_recv = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  val tlb_ready_recv = VecInit((0 until PortNumber).map(i => RegNext(s0_fire, false.B) || (s1_valid && !tlb_already_recv(i))))
  val tlb_resp_valid = Wire(Vec(2, Bool()))
  for (i <- 0 until PortNumber) {
    tlb_resp_valid(i) := tlb_already_recv(i) || (tlb_ready_recv(i) && tlb_back(i))
    when (tlb_already_recv(i) && s1_fire) {
      tlb_already_recv(i) := false.B
    }
    when (tlb_back(i) && tlb_ready_recv(i) && !s1_fire) {
      tlb_already_recv(i) := true.B
    }
    fromITLB(i).ready := tlb_ready_recv(i)
  }
  assert(RegNext(Cat((0 until PortNumber).map(i => tlb_need_back(i) || !tlb_resp_valid(i))).andR(), true.B),
    "when tlb should not back, tlb should not resp valid")
  assert(RegNext(!s1_valid || Cat(tlb_need_back).orR, true.B), "when s1_valid, need at least one tlb_need_back")
  assert(RegNext(s1_valid || !Cat(tlb_need_back).orR, true.B), "when !s1_valid, all the tlb_need_back should be false")
  assert(RegNext(s1_valid || !Cat(tlb_already_recv).orR, true.B), "when !s1_valid, should not tlb_already_recv")
  assert(RegNext(s1_valid || !Cat(tlb_resp_valid).orR, true.B), "when !s1_valid, should not tlb_resp_valid")

  val tlbRespPAddr = VecInit((0 until PortNumber).map(i => ResultHoldBypass(valid = tlb_back(i), data = fromITLB(i).bits.paddr(0))))
  val tlbExcpPF = VecInit((0 until PortNumber).map(i => ResultHoldBypass(valid = tlb_back(i), data = fromITLB(i).bits.excp(0).pf.instr) && tlb_need_back(i)))
  val tlbExcpAF = VecInit((0 until PortNumber).map(i => ResultHoldBypass(valid = tlb_back(i), data = fromITLB(i).bits.excp(0).af.instr) && tlb_need_back(i)))
  val tlbExcp = VecInit((0 until PortNumber).map(i => tlbExcpPF(i) || tlbExcpPF(i)))

  val tlbRespAllValid = Cat((0 until PortNumber).map(i => !tlb_need_back(i) || tlb_resp_valid(i))).andR
  s1_ready := s2_ready && tlbRespAllValid  || !s1_valid
  s1_fire  := s1_valid && tlbRespAllValid && s2_ready

  /** s1 hit check/tag compare */
  val s1_req_paddr              = tlbRespPAddr
  val s1_req_ptags              = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  val s1_meta_ptags              = ResultHoldBypass(data = metaResp.tags, valid = RegNext(s0_fire))
  val s1_meta_cohs               = ResultHoldBypass(data = metaResp.cohs, valid = RegNext(s0_fire))
  val s1_meta_errors             = ResultHoldBypass(data = metaResp.errors, valid = RegNext(s0_fire))

  val s1_data_cacheline          = ResultHoldBypass(data = dataResp.datas, valid = RegNext(s0_fire))
  val s1_data_errorBits          = ResultHoldBypass(data = dataResp.codes, valid = RegNext(s0_fire))

  val s1_tag_eq_vec        = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w =>  s1_meta_ptags(p)(w) ===  s1_req_ptags(p) ))))
  val s1_tag_match_vec     = VecInit((0 until PortNumber).map( k => VecInit(s1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_cohs(k)(w).isValid()})))
  val s1_tag_match         = VecInit(s1_tag_match_vec.map(vector => ParallelOR(vector)))

  val s1_port_hit          = VecInit(Seq(s1_tag_match(0) && s1_valid  && !tlbExcp(0),  s1_tag_match(1) && s1_valid && s1_double_line && !tlbExcp(1) ))
  val s1_bank_miss         = VecInit(Seq(!s1_tag_match(0) && s1_valid && !tlbExcp(0), !s1_tag_match(1) && s1_valid && s1_double_line && !tlbExcp(1) ))
  val s1_hit               = (s1_port_hit(0) && s1_port_hit(1)) || (!s1_double_line && s1_port_hit(0))

  /** choose victim cacheline */
  val replacers       = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/PortNumber))
  val s1_victim_oh    = ResultHoldBypass(data = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(s1_req_vsetIdx(i)))}), valid = RegNext(s0_fire))

  val s1_victim_coh   = VecInit(s1_victim_oh.zipWithIndex.map {case(oh, port) => Mux1H(oh, s1_meta_cohs(port))})

  when(s1_valid){
    assert(PopCount(s1_tag_match_vec(0)) <= 1.U && PopCount(s1_tag_match_vec(1)) <= 1.U, "Multiple hit in main pipe")
  }

  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}


  /** <PERF> replace victim way number */

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

  /**
    ******************************************************************************
    * ICache Stage 2
    * - send request to MSHR if ICache miss
    * - generate secondary miss status/data registers
    * - response to IFU
    ******************************************************************************
    */

  /** s2 control */
  val s2_fetch_finish = Wire(Bool())

  val s2_valid          = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = false.B, lastFlush = false.B)
  val s2_miss_available = Wire(Bool())

  s2_ready      := (s2_valid && s2_fetch_finish && !io.respStall) || (!s2_valid && s2_miss_available)
  s2_fire       := s2_valid && s2_fetch_finish && !io.respStall

  /** s2 data */
  val mmio = fromPMP.map(port => port.mmio) // TODO: handle it

  val (s2_req_paddr , s2_req_vaddr)   = (RegEnable(s1_req_paddr, s1_fire), RegEnable(s1_req_vaddr, s1_fire))
  val s2_req_vsetIdx  = RegEnable(s1_req_vsetIdx, s1_fire)
  val s2_req_ptags    = RegEnable(s1_req_ptags, s1_fire)
  val s2_only_first   = RegEnable(s1_only_first, s1_fire)
  val s2_double_line  = RegEnable(s1_double_line, s1_fire)
  val s2_hit          = RegEnable(s1_hit   , s1_fire)
  val s2_port_hit     = RegEnable(s1_port_hit, s1_fire)
  val s2_bank_miss    = RegEnable(s1_bank_miss, s1_fire)
  val s2_waymask      = RegEnable(s1_victim_oh, s1_fire)
  val s2_victim_coh   = RegEnable(s1_victim_coh, s1_fire)
  val s2_tag_match_vec = RegEnable(s1_tag_match_vec, s1_fire)

  assert(RegNext(!s2_valid || s2_req_paddr(0)(11,0) === s2_req_vaddr(0)(11,0), true.B))

  /** status imply that s2 is a secondary miss (no need to resend miss request) */
  val sec_meet_vec = Wire(Vec(2, Bool()))
  val s2_fixed_hit_vec = VecInit((0 until 2).map(i => s2_port_hit(i) || sec_meet_vec(i)))
  val s2_fixed_hit = (s2_valid && s2_fixed_hit_vec(0) && s2_fixed_hit_vec(1) && s2_double_line) || (s2_valid && s2_fixed_hit_vec(0) && !s2_double_line)

  val s2_meta_errors    = RegEnable(s1_meta_errors,    s1_fire)
  val s2_data_errorBits = RegEnable(s1_data_errorBits, s1_fire)
  val s2_data_cacheline = RegEnable(s1_data_cacheline, s1_fire)

  val s2_data_errors    = Wire(Vec(PortNumber,Vec(nWays, Bool())))

  (0 until PortNumber).map{ i =>
    val read_datas = s2_data_cacheline(i).asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeUnit.W))))
    val read_codes = s2_data_errorBits(i).asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeBits.W))))
    val data_full_wayBits = VecInit((0 until nWays).map( w =>
                                  VecInit((0 until dataCodeUnitNum).map(u =>
                                        Cat(read_codes(w)(u), read_datas(w)(u))))))
    val data_error_wayBits = VecInit((0 until nWays).map( w =>
                                  VecInit((0 until dataCodeUnitNum).map(u =>
                                       cacheParams.dataCode.decode(data_full_wayBits(w)(u)).error ))))
    if(i == 0){
      (0 until nWays).map{ w =>
        s2_data_errors(i)(w) := RegNext(RegNext(s1_fire)) && RegNext(data_error_wayBits(w)).reduce(_||_)
      }
    } else {
      (0 until nWays).map{ w =>
        s2_data_errors(i)(w) := RegNext(RegNext(s1_fire)) && RegNext(RegNext(s1_double_line)) && RegNext(data_error_wayBits(w)).reduce(_||_)
      }
    }
  }

  val s2_parity_meta_error  = VecInit((0 until PortNumber).map(i => s2_meta_errors(i).reduce(_||_) && io.csr_parity_enable))
  val s2_parity_data_error  = VecInit((0 until PortNumber).map(i => s2_data_errors(i).reduce(_||_) && io.csr_parity_enable))
  val s2_parity_error       = VecInit((0 until PortNumber).map(i => RegNext(s2_parity_meta_error(i)) || s2_parity_data_error(i)))

  for(i <- 0 until PortNumber){
    io.errors(i).valid            := RegNext(s2_parity_error(i) && RegNext(RegNext(s1_fire)))
    io.errors(i).report_to_beu    := RegNext(s2_parity_error(i) && RegNext(RegNext(s1_fire)))
    io.errors(i).paddr            := RegNext(RegNext(s2_req_paddr(i)))
    io.errors(i).source           := DontCare
    io.errors(i).source.tag       := RegNext(RegNext(s2_parity_meta_error(i)))
    io.errors(i).source.data      := RegNext(s2_parity_data_error(i))
    io.errors(i).source.l2        := false.B
    io.errors(i).opType           := DontCare
    io.errors(i).opType.fetch     := true.B
  }
  XSError(s2_parity_error.reduce(_||_) && RegNext(RegNext(s1_fire)), "ICache has parity error in MainPaipe!")


  /** exception and pmp logic **/
  //PMP Result
  val s2_tlb_need_back = VecInit((0 until PortNumber).map(i => ValidHold(tlb_need_back(i) && s1_fire, s2_fire, false.B)))
  val pmpExcpAF = Wire(Vec(PortNumber, Bool()))
  pmpExcpAF(0)  := fromPMP(0).instr && s2_tlb_need_back(0)
  pmpExcpAF(1)  := fromPMP(1).instr && s2_double_line && s2_tlb_need_back(1)
  //exception information
  //short delay exception signal
  val s2_except_pf        = RegEnable(tlbExcpPF, s1_fire)
  val s2_except_tlb_af    = RegEnable(tlbExcpAF, s1_fire)
  //long delay exception signal
  val s2_except_pmp_af    =  DataHoldBypass(pmpExcpAF, RegNext(s1_fire))    
  // val s2_except_parity_af =  VecInit(s2_parity_error(i) && RegNext(RegNext(s1_fire))                      )

  val s2_except    = VecInit((0 until 2).map{i => s2_except_pf(i) || s2_except_tlb_af(i)})
  val s2_has_except = s2_valid && (s2_except_tlb_af.reduce(_||_) || s2_except_pf.reduce(_||_))
  //MMIO
  val s2_mmio      = DataHoldBypass(io.pmp(0).resp.mmio && !s2_except_tlb_af(0) && !s2_except_pmp_af(0) && !s2_except_pf(0), RegNext(s1_fire)).asBool() && s2_valid

  //send physical address to PMP
  io.pmp.zipWithIndex.map { case (p, i) =>
    p.req.valid := s2_valid && !missSwitchBit
    p.req.bits.addr := s2_req_paddr(i)
    p.req.bits.size := 3.U // TODO
    p.req.bits.cmd := TlbCmd.exec
  }

  /*** cacheline miss logic ***/
  val wait_idle :: wait_queue_ready :: wait_send_req  :: wait_two_resp :: wait_0_resp :: wait_1_resp :: wait_one_resp ::wait_finish :: wait_pmp_except :: Nil = Enum(9)
  val wait_state = RegInit(wait_idle)

  val port_miss_fix  = VecInit(Seq(fromMSHR(0).fire() && !s2_port_hit(0),   fromMSHR(1).fire() && s2_double_line && !s2_port_hit(1) ))

  // secondary miss record registers
  class MissSlot(implicit p: Parameters) extends  ICacheBundle {
    val m_vSetIdx   = UInt(idxBits.W)
    val m_pTag      = UInt(tagBits.W)
    val m_data      = UInt(blockBits.W)
    val m_corrupt   = Bool()
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

  /*** miss/hit pattern: <Control Signal> only raise at the first cycle of s2_valid ***/
  val cacheline_0_hit  = (s2_port_hit(0) || sec_meet_0_miss)
  val cacheline_0_miss = !s2_port_hit(0) && !sec_meet_0_miss

  val cacheline_1_hit  = (s2_port_hit(1) || sec_meet_1_miss)
  val cacheline_1_miss = !s2_port_hit(1) && !sec_meet_1_miss

  val  only_0_miss      = RegNext(s1_fire) && cacheline_0_miss && !s2_double_line && !s2_has_except && !s2_mmio
  val  only_0_hit       = RegNext(s1_fire) && cacheline_0_hit  && !s2_double_line && !s2_mmio
  val  hit_0_hit_1      = RegNext(s1_fire) && cacheline_0_hit  && cacheline_1_hit  && s2_double_line && !s2_mmio
  val  hit_0_miss_1     = RegNext(s1_fire) && cacheline_0_hit  && cacheline_1_miss && s2_double_line  && !s2_has_except && !s2_mmio
  val  miss_0_hit_1     = RegNext(s1_fire) && cacheline_0_miss && cacheline_1_hit && s2_double_line  && !s2_has_except && !s2_mmio
  val  miss_0_miss_1    = RegNext(s1_fire) && cacheline_0_miss && cacheline_1_miss && s2_double_line  && !s2_has_except && !s2_mmio

  val  hit_0_except_1   = RegNext(s1_fire) && s2_double_line &&  !s2_except(0) && s2_except(1)  &&  cacheline_0_hit
  val  miss_0_except_1  = RegNext(s1_fire) && s2_double_line &&  !s2_except(0) && s2_except(1)  &&  cacheline_0_miss
  val  except_0         = RegNext(s1_fire) && s2_except(0)

  def holdReleaseLatch(valid: Bool, release: Bool, flush: Bool): Bool ={
    val bit = RegInit(false.B)
    when(flush)                   { bit := false.B  }
    .elsewhen(valid && !release)  { bit := true.B   }
    .elsewhen(release)            { bit := false.B  }
    bit || valid
  }

  /*** miss/hit pattern latch: <Control Signal> latch the miss/hit patter if pipeline stop ***/
  val  miss_0_hit_1_latch     =   holdReleaseLatch(valid = miss_0_hit_1,    release = s2_fire,      flush = false.B)
  val  miss_0_miss_1_latch    =   holdReleaseLatch(valid = miss_0_miss_1,   release = s2_fire,      flush = false.B)
  val  only_0_miss_latch      =   holdReleaseLatch(valid = only_0_miss,     release = s2_fire,      flush = false.B)
  val  hit_0_miss_1_latch     =   holdReleaseLatch(valid = hit_0_miss_1,    release = s2_fire,      flush = false.B)

  val  miss_0_except_1_latch  =   holdReleaseLatch(valid = miss_0_except_1, release = s2_fire,      flush = false.B)
  val  except_0_latch          =   holdReleaseLatch(valid = except_0,    release = s2_fire,      flush = false.B)
  val  hit_0_except_1_latch         =    holdReleaseLatch(valid = hit_0_except_1,    release = s2_fire,      flush = false.B)

  val only_0_hit_latch        = holdReleaseLatch(valid = only_0_hit,   release = s2_fire,      flush = false.B)
  val hit_0_hit_1_latch        = holdReleaseLatch(valid = hit_0_hit_1,   release = s2_fire,      flush = false.B)


  /*** secondary miss judgment ***/

  def waitSecondComeIn(missState: UInt): Bool = (missState === m_wait_sec_miss)

  def getMissSituat(slotNum : Int, missNum : Int ) :Bool =  {
    RegNext(s1_fire) && 
    RegNext(missSlot(slotNum).m_vSetIdx === s1_req_vsetIdx(missNum)) && 
    RegNext(missSlot(slotNum).m_pTag  === s1_req_ptags(missNum)) && 
    !s2_port_hit(missNum)  && 
    waitSecondComeIn(missStateQueue(slotNum))
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

  /*** reserved data for secondary miss ***/

  reservedRefillData(0) := DataHoldBypass(data = missSlot(0).m_data, valid = miss_0_s2_0 || miss_0_s2_1)
  reservedRefillData(1) := DataHoldBypass(data = missSlot(1).m_data, valid = miss_1_s2_0 || miss_1_s2_1)

  /*** miss state machine ***/

  //deal with not-cache-hit pmp af
  val only_pmp_af = Wire(Vec(2, Bool()))
  only_pmp_af(0) := s2_except_pmp_af(0) && cacheline_0_miss && !s2_except(0) && s2_valid
  only_pmp_af(1) := s2_except_pmp_af(1) && cacheline_1_miss && !s2_except(1) && s2_valid && s2_double_line

  switch(wait_state){
    is(wait_idle){
      when(only_pmp_af(0) || only_pmp_af(1) || s2_mmio){
        //should not send req to MissUnit when there is an access exception in PMP
        //But to avoid using pmp exception in control signal (like s2_fire), should delay 1 cycle.
        //NOTE: pmp exception cache line also could hit in ICache, but the result is meaningless. Just give the exception signals.
        wait_state := wait_finish
      }.elsewhen(miss_0_except_1_latch){
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


  /*** send request to MissUnit ***/

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
      missStateQueue(i)         := m_refilled
      missSlot(i).m_data        := fromMSHR(i).bits.data
      missSlot(i).m_corrupt     := fromMSHR(i).bits.corrupt
    }


    when(s2_fire && missStateQueue(i) === m_refilled){
      missStateQueue(i)     := m_wait_sec_miss
    }

    /*** Only the first cycle to check whether meet the secondary miss ***/
    when(missStateQueue(i) === m_wait_sec_miss){
      /*** The seondary req has been fix by this slot and another also hit || the secondary req for other cacheline and hit ***/
      when((slot_slove(i) && s2_fire) || (!slot_slove(i) && s2_fire) ) {
        missStateQueue(i)     := m_invalid
      }
      /*** The seondary req has been fix by this slot but another miss/f3 not ready || the seondary req for other cacheline and miss ***/
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

  io.prefetchEnable := false.B
  io.prefetchDisable := false.B
  when(toMSHR.map(_.valid).reduce(_||_)){
    missSwitchBit := true.B
    io.prefetchEnable := true.B
  }.elsewhen(missSwitchBit && s2_fetch_finish){
    missSwitchBit := false.B
    io.prefetchDisable := true.B
  }


  val miss_all_fix       =  wait_state === wait_finish
                              
  s2_fetch_finish        := ((s2_valid && s2_fixed_hit) || miss_all_fix || hit_0_except_1_latch || except_0_latch)
  
  /** update replacement status register: 0 is hit access/ 1 is miss access */
  (touch_ways zip touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    t_s(0)         := s2_req_vsetIdx(i)
    t_w(0).valid   := s2_valid && s2_port_hit(i)
    t_w(0).bits    := OHToUInt(s2_tag_match_vec(i))

    t_s(1)         := s2_req_vsetIdx(i)
    t_w(1).valid   := s2_valid && !s2_port_hit(i)
    t_w(1).bits    := OHToUInt(s2_waymask(i))
  }

  //** use hit one-hot select data
  val s2_hit_datas    = VecInit(s2_data_cacheline.zipWithIndex.map { case(bank, i) =>
    val port_hit_data = Mux1H(s2_tag_match_vec(i).asUInt, bank)
    port_hit_data
  })

  val s2_register_datas       = Wire(Vec(2, UInt(blockBits.W)))

  s2_register_datas.zipWithIndex.map{case(bank,i) =>
    // if(i == 0) bank := Mux(s2_port_hit(i), s2_hit_datas(i), Mux(miss_0_s2_0_latch,reservedRefillData(0), Mux(miss_1_s2_0_latch,reservedRefillData(1), missSlot(0).m_data)))
    // else    bank    := Mux(s2_port_hit(i), s2_hit_datas(i), Mux(miss_0_s2_1_latch,reservedRefillData(0), Mux(miss_1_s2_1_latch,reservedRefillData(1), missSlot(1).m_data)))
    if(i == 0) bank := Mux(miss_0_s2_0_latch,reservedRefillData(0), Mux(miss_1_s2_0_latch,reservedRefillData(1), missSlot(0).m_data))
    else    bank    := Mux(miss_0_s2_1_latch,reservedRefillData(0), Mux(miss_1_s2_1_latch,reservedRefillData(1), missSlot(1).m_data))
  }

  /** response to IFU */

  (0 until PortNumber).map{ i =>
    if(i ==0) toIFU(i).valid          := s2_fire
       else   toIFU(i).valid          := s2_fire && s2_double_line
    //when select is high, use sramData. Otherwise, use registerData.
    toIFU(i).bits.registerData  := s2_register_datas(i)
    toIFU(i).bits.sramData  := s2_hit_datas(i)
    toIFU(i).bits.select    := s2_port_hit(i)
    toIFU(i).bits.paddr     := s2_req_paddr(i)
    toIFU(i).bits.vaddr     := s2_req_vaddr(i)
    toIFU(i).bits.tlbExcp.pageFault     := s2_except_pf(i)
    toIFU(i).bits.tlbExcp.accessFault   := s2_except_tlb_af(i) || missSlot(i).m_corrupt || s2_except_pmp_af(i)
    toIFU(i).bits.tlbExcp.mmio          := s2_mmio

    when(RegNext(s2_fire && missSlot(i).m_corrupt)){
      io.errors(i).valid            := true.B
      io.errors(i).report_to_beu    := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).paddr            := RegNext(s2_req_paddr(i))
      io.errors(i).source.tag       := false.B
      io.errors(i).source.data      := false.B
      io.errors(i).source.l2        := true.B
    }
  }

  io.perfInfo.only_0_hit    := only_0_hit_latch
  io.perfInfo.only_0_miss   := only_0_miss_latch
  io.perfInfo.hit_0_hit_1   := hit_0_hit_1_latch
  io.perfInfo.hit_0_miss_1  := hit_0_miss_1_latch
  io.perfInfo.miss_0_hit_1  := miss_0_hit_1_latch
  io.perfInfo.miss_0_miss_1 := miss_0_miss_1_latch
  io.perfInfo.hit_0_except_1 := hit_0_except_1_latch
  io.perfInfo.miss_0_except_1 := miss_0_except_1_latch
  io.perfInfo.except_0      := except_0_latch
  io.perfInfo.bank_hit(0)   := only_0_miss_latch  || hit_0_hit_1_latch || hit_0_miss_1_latch || hit_0_except_1_latch
  io.perfInfo.bank_hit(1)   := miss_0_hit_1_latch || hit_0_hit_1_latch
  io.perfInfo.hit           := hit_0_hit_1_latch || only_0_hit_latch || hit_0_except_1_latch || except_0_latch

  /** <PERF> fetch bubble generated by icache miss*/

  XSPerfAccumulate("icache_bubble_s2_miss",    s2_valid && !s2_fetch_finish )

}
