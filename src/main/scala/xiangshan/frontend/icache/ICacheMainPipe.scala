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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.tilelink.ClientStates
import xiangshan._
import xiangshan.cache.mmu._
import utils._
import utility._
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
  // val registerData = UInt(blockBits.W)
  // val sramData = UInt(blockBits.W)
  // val select   = Bool()
  val data = UInt((blockBits/2).W)
  val paddr    = UInt(PAddrBits.W)
  val gpaddr    = UInt(GPAddrBits.W)
  val tlbExcp  = new Bundle{
    val pageFault = Bool()
    val guestPageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }
}

class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle
{
  val req  = Flipped(Decoupled(new FtqToICacheRequestBundle))
  val resp = Vec(PortNumber, ValidIO(new ICacheMainPipeResp))
  val topdownIcacheMiss = Output(Bool())
  val topdownItlbMiss = Output(Bool())
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
  val hartId = Input(UInt(hartIdLen.W))
  val fencei = Input(Bool())
  /*** internal interface ***/
  val metaArray   = new ICacheMetaReqBundle
  val dataArray   = new ICacheDataReqBundle
  /** prefetch io */
  val IPFBufferRead = Flipped(new IPFBufferRead)
  val PIQRead       = Flipped(new PIQRead)

  val IPFReplacer         = Flipped(new IPFReplacer)
  val ICacheMainPipeInfo  = new ICacheMainPipeInfo

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

  val csr_parity_enable = Input(Bool())
}

class ICacheDB(implicit p: Parameters) extends ICacheBundle {
  val blk_vaddr   = UInt((VAddrBits - blockOffBits).W)
  val blk_paddr   = UInt((PAddrBits - blockOffBits).W)
  val hit         = Bool()
}

class ICacheMainPipe(implicit p: Parameters) extends ICacheModule
{
  val io = IO(new ICacheMainPipeInterface)

  /** Input/Output port */
  val (fromFtq, toIFU)    = (io.fetch.req,          io.fetch.resp)
  val (toMeta, metaResp)  = (io.metaArray.toIMeta,  io.metaArray.fromIMeta)
  val (toData, dataResp)  = (io.dataArray.toIData,  io.dataArray.fromIData)
  val (toIPF,  fromIPF)   = (io.IPFBufferRead.req,  io.IPFBufferRead.resp)
  val (toPIQ,  fromPIQ)   = (io.PIQRead.req,        io.PIQRead.resp)
  val (toMSHR, fromMSHR)  = (io.mshr.map(_.toMSHR), io.mshr.map(_.fromMSHR))
  val (toITLB, fromITLB)  = (io.itlb.map(_.req),    io.itlb.map(_.resp))
  val (toPMP,  fromPMP)   = (io.pmp.map(_.req),     io.pmp.map(_.resp))

  val IPFReplacer         = io.IPFReplacer
  val toIPrefetch         = io.ICacheMainPipeInfo


  // Statistics on the frequency distribution of FTQ fire interval
  val cntFtqFireInterval = RegInit(0.U(32.W))
  cntFtqFireInterval := Mux(fromFtq.fire, 1.U, cntFtqFireInterval + 1.U)
  XSPerfHistogram("ftq2icache_fire",
                  cntFtqFireInterval, fromFtq.fire,
                  1, 300, 1, right_strict = true)

  // Ftq RegNext Register
  val fromFtqReq = fromFtq.bits.pcMemRead

  /** pipeline control signal */
  val s1_ready, s2_ready = Wire(Bool())
  val s0_fire,  s1_fire , s2_fire  = Wire(Bool())

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
  val s0_double_line = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && fromFtqReq(i).crossCacheline)

  val s0_final_valid        = s0_valid
  val s0_final_vaddr        = s0_req_vaddr.head
  val s0_final_vsetIdx      = s0_req_vsetIdx.head
  val s0_final_only_first   = s0_only_first.head
  val s0_final_double_line  = s0_double_line.head

  /** SRAM request */
  // 0,1,2,3 -> dataArray(data); 3 -> dataArray(code); 0 -> metaArray; 4 -> itlb
  val ftq_req_to_data_doubleline  = s0_double_line.init
  val ftq_req_to_data_vset_idx    = s0_req_vsetIdx.init
  val ftq_req_to_data_valid       = fromFtq.bits.readValid.init

  val ftq_req_to_meta_doubleline  = s0_double_line.head
  val ftq_req_to_meta_vset_idx    = s0_req_vsetIdx.head
  val ftq_req_to_meta_valid       = fromFtq.bits.readValid.head

  val ftq_req_to_itlb_only_first  = s0_only_first.last
  val ftq_req_to_itlb_doubleline  = s0_double_line.last
  val ftq_req_to_itlb_vaddr       = s0_req_vaddr.last
  val ftq_req_to_itlb_vset_idx    = s0_req_vsetIdx.last

  /** Data request */
  for(i <- 0 until partWayNum) {
    toData.valid                  := ftq_req_to_data_valid(i)
    toData.bits(i).isDoubleLine   := ftq_req_to_data_doubleline(i)
    toData.bits(i).vSetIdx        := ftq_req_to_data_vset_idx(i)
  }

  /** Meta request */
  toMeta.valid               := ftq_req_to_meta_valid
  toMeta.bits.isDoubleLine   := ftq_req_to_meta_doubleline
  toMeta.bits.vSetIdx        := ftq_req_to_meta_vset_idx

  val toITLB_s0_valid    = VecInit(Seq(s0_valid, s0_valid && ftq_req_to_itlb_doubleline))
  val toITLB_s0_size     = VecInit(Seq(3.U, 3.U)) // TODO: fix the size
  val toITLB_s0_vaddr    = ftq_req_to_itlb_vaddr
  val toITLB_s0_debug_pc = ftq_req_to_itlb_vaddr

  val itlb_can_go    = toITLB(0).ready && toITLB(1).ready
  val icache_can_go  = toData.ready && toMeta.ready
  val pipe_can_go    = s1_ready
  val s0_can_go      = itlb_can_go && icache_can_go && pipe_can_go && !io.fencei
  s0_fire  := s0_valid && s0_can_go

  //TODO: fix GTimer() condition
  fromFtq.ready := s0_can_go

  /**
    ******************************************************************************
    * ICache Stage 1
    * - get tlb resp data (exceptiong info and physical addresses)
    * - get Meta/Data SRAM read responses (latched for pipeline stop)
    * - tag compare/hit check
    * - check ipf and piq
    ******************************************************************************
    */

  /** s1 control */
  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = io.fencei, lastFlush = false.B)

  val s1_req_vaddr   = RegEnable(s0_final_vaddr, s0_fire)
  val s1_req_vsetIdx = RegEnable(s0_final_vsetIdx, s0_fire)
  val s1_double_line = RegEnable(s0_final_double_line, s0_fire)

  /** tlb request and response */
  fromITLB.foreach(_.ready := true.B)
  val s1_wait_itlb  = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))

  (0 until PortNumber).foreach { i =>
    when(io.fencei) {
      s1_wait_itlb(i) := false.B
    }.elsewhen(RegNext(s0_fire) && fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := true.B
    }.elsewhen(s1_wait_itlb(i) && !fromITLB(i).bits.miss) {
      s1_wait_itlb(i) := false.B
    }
  }

  val s1_need_itlb = Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && fromITLB(0).bits.miss && s1_valid,
                         (RegNext(s0_fire) || s1_wait_itlb(1)) && fromITLB(1).bits.miss && s1_valid && s1_double_line)
  val toITLB_s1_valid    = s1_need_itlb
  val toITLB_s1_size     = VecInit(Seq(3.U, 3.U)) // TODO: fix the size
  val toITLB_s1_vaddr    = s1_req_vaddr
  val toITLB_s1_debug_pc = s1_req_vaddr

  // chose tlb req between s0 and s1
  for (i <- 0 until PortNumber) {
    toITLB(i).valid         := Mux(s1_need_itlb(i), toITLB_s1_valid(i), toITLB_s0_valid(i))
    toITLB(i).bits.size     := Mux(s1_need_itlb(i), toITLB_s1_size(i), toITLB_s0_size(i))
    toITLB(i).bits.vaddr    := Mux(s1_need_itlb(i), toITLB_s1_vaddr(i), toITLB_s0_vaddr(i))
    toITLB(i).bits.debug.pc := Mux(s1_need_itlb(i), toITLB_s1_debug_pc(i), toITLB_s0_debug_pc(i))
  }
  toITLB.map{port =>
    port.bits.cmd                 := TlbCmd.exec
    port.bits.memidx              := DontCare
    port.bits.debug.robIdx        := DontCare
    port.bits.no_translate        := false.B
    port.bits.debug.isFirstIssue  := DontCare
    port.bits.kill                := DontCare
    port.bits.hlvx                := DontCare
    port.bits.hyperinst           := DontCare
  }
  io.itlb.foreach(_.req_kill := false.B)

  /** tlb response latch for pipeline stop */
  // val tlb_valid_tmp = VecInit((0 until PortNumber).map(i =>
  //                       (RegNext(s0_fire) || s1_wait_itlb(i)) && !fromITLB(i).bits.miss))
  val tlb_valid_tmp = VecInit(Seq((RegNext(s0_fire) || s1_wait_itlb(0)) && !fromITLB(0).bits.miss,
                                  (RegNext(s0_fire) || s1_wait_itlb(1)) && !fromITLB(1).bits.miss && s1_double_line))
  val tlbRespPAddr  = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = tlb_valid_tmp(i), data = fromITLB(i).bits.paddr(0))))
  val tlbRespGPAddr = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = tlb_valid_tmp(i), data = fromITLB(i).bits.gpaddr(0))))
  val tlbExcpGPF    = VecInit((0 until PortNumber).map(i => 
                        ResultHoldBypass(valid = tlb_valid_tmp(i), data = fromITLB(i).bits.excp(0).gpf.instr)))
  val tlbExcpPF     = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = tlb_valid_tmp(i), data = fromITLB(i).bits.excp(0).pf.instr)))
  val tlbExcpAF     = VecInit((0 until PortNumber).map(i =>
                        ResultHoldBypass(valid = tlb_valid_tmp(i), data = fromITLB(i).bits.excp(0).af.instr)))
  val tlbExcp       = VecInit((0 until PortNumber).map(i => tlbExcpAF(i) || tlbExcpPF(i) || tlbExcpGPF(i)))

  val s1_tlb_valid = VecInit((0 until PortNumber).map(i => ValidHoldBypass(tlb_valid_tmp(i), s1_fire, io.fencei)))
  val tlbRespAllValid = s1_tlb_valid(0) && (!s1_double_line || s1_double_line && s1_tlb_valid(1))


  def numOfStage = 3
  val itlbMissStage = RegInit(VecInit(Seq.fill(numOfStage - 1)(0.B)))
  itlbMissStage(0) := !tlbRespAllValid
  for (i <- 1 until numOfStage - 1) {
    itlbMissStage(i) := itlbMissStage(i - 1)
  }
  
  /** s1 hit check/tag compare */
  val s1_req_paddr              = tlbRespPAddr
  val s1_req_gpaddr             = tlbRespGPAddr
  val s1_req_ptags              = VecInit(s1_req_paddr.map(get_phy_tag(_)))

  val s1_meta_ptags              = ResultHoldBypass(data = metaResp.tags, valid = RegNext(s0_fire))
  val s1_meta_valids             = ResultHoldBypass(data = metaResp.entryValid, valid = RegNext(s0_fire))
  val s1_meta_errors             = ResultHoldBypass(data = metaResp.errors, valid = RegNext(s0_fire))

  val s1_data_cacheline          = ResultHoldBypass(data = dataResp.datas, valid = RegNext(s0_fire))
  val s1_data_errorBits          = ResultHoldBypass(data = dataResp.codes, valid = RegNext(s0_fire))

  val s1_tag_eq_vec        = VecInit((0 until PortNumber).map( p => VecInit((0 until nWays).map( w =>  s1_meta_ptags(p)(w) ===  s1_req_ptags(p)))))
  val s1_tag_match_vec     = VecInit((0 until PortNumber).map( k => VecInit(s1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => way_tag_eq && s1_meta_valids(k)(w)})))
  val s1_tag_match         = VecInit(s1_tag_match_vec.map(vector => ParallelOR(vector)))

  val s1_port_hit          = VecInit(Seq(s1_tag_match(0) && s1_valid, s1_tag_match(1) && s1_valid && s1_double_line))

  /** choose victim cacheline */
  val bank_vsetIdx    = VecInit((0 until PortNumber).map( i => Mux(s1_req_vsetIdx(i)(0), s1_req_vsetIdx(1)(highestIdxBit, 1), s1_req_vsetIdx(0)(highestIdxBit, 1))))
  val replacers       = Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/PortNumber))
  val bank_victim_oh  = ResultHoldBypass(data = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(bank_vsetIdx(i)))}), valid = RegNext(s0_fire))
  val s1_victim_oh    = VecInit((0 until PortNumber).map( i => Mux(s1_req_vsetIdx(i)(0), bank_victim_oh(1), bank_victim_oh(0))))

  when(s1_fire){
    assert(PopCount(s1_tag_match_vec(0)) <= 1.U && (PopCount(s1_tag_match_vec(1)) <= 1.U || !s1_double_line),
      "Multiple hit in main pipe, port0:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x port1:is=%d,ptag=0x%x,vidx=0x%x,vaddr=0x%x ",
      PopCount(s1_tag_match_vec(0)) > 1.U,s1_req_ptags(0), get_idx(s1_req_vaddr(0)), s1_req_vaddr(0),
      PopCount(s1_tag_match_vec(1)) > 1.U && s1_double_line, s1_req_ptags(1), get_idx(s1_req_vaddr(1)), s1_req_vaddr(1))
  }

  /** check ipf, get result at the same cycle */
  (0 until PortNumber).foreach { i =>
    toIPF(i).valid      := tlb_valid_tmp(i)
    toIPF(i).bits.paddr := s1_req_paddr(i)
  }
  val s1_ipf_hit        = VecInit((0 until PortNumber).map(i => toIPF(i).valid && fromIPF(i).ipf_hit))
  val s1_ipf_hit_latch  = VecInit((0 until PortNumber).map(i => holdReleaseLatch(valid = s1_ipf_hit(i), release = s1_fire, flush = false.B)))
  val s1_ipf_data       = VecInit((0 until PortNumber).map(i => ResultHoldBypass(data = fromIPF(i).cacheline, valid = s1_ipf_hit(i))))

  /** check in PIQ, if hit, wait until prefetch port hit */
  (0 until PortNumber).foreach { i =>
    toPIQ(i).valid      := tlb_valid_tmp(i)
    toPIQ(i).bits.paddr := s1_req_paddr(i)
  }
  val s1_piq_hit        = VecInit((0 until PortNumber).map(i => toIPF(i).valid && fromPIQ(i).piq_hit))
  val s1_piq_hit_latch  = VecInit((0 until PortNumber).map(i => holdReleaseLatch(valid = s1_piq_hit(i), release = s1_fire, flush = false.B)))
  val wait_piq          = VecInit((0 until PortNumber).map(i => toIPF(i).valid && fromPIQ(i).piq_hit && !fromPIQ(i).data_valid))
  val wait_piq_latch    = VecInit((0 until PortNumber).map(i => holdReleaseLatch(valid = wait_piq(i), release = s1_fire || fromPIQ(i).data_valid, flush = false.B)))
  val s1_piq_data       = VecInit((0 until PortNumber).map(i => ResultHoldBypass(data = fromPIQ(i).cacheline, valid = (s1_piq_hit(i) || wait_piq_latch(i)) && fromPIQ(i).data_valid)))

  val s1_wait           = (0 until PortNumber).map(i => wait_piq_latch(i) && !fromPIQ(i).data_valid).reduce(_||_)

  val s1_prefetch_hit = VecInit((0 until PortNumber).map(i => s1_ipf_hit_latch(i) || s1_piq_hit_latch(i)))
  val s1_prefetch_hit_data = VecInit((0 until PortNumber).map(i => Mux(s1_ipf_hit_latch(i), s1_ipf_data(i), s1_piq_data(i))))

  s1_ready := s2_ready && tlbRespAllValid && !s1_wait || !s1_valid
  s1_fire  := s1_valid && tlbRespAllValid && s2_ready && !s1_wait && !io.fencei

  // record cacheline log
  val isWriteICacheTable = Constantin.createRecord(s"isWriteICacheTable${p(XSCoreParamsKey).HartId}")
  val ICacheTable = ChiselDB.createTable("ICacheTable" + p(XSCoreParamsKey).HartId.toString, new ICacheDB)

  val ICacheDumpData_req0 = Wire(new ICacheDB)
  ICacheDumpData_req0.blk_paddr := getBlkAddr(s1_req_paddr(0))
  ICacheDumpData_req0.blk_vaddr := getBlkAddr(s1_req_vaddr(0))
  ICacheDumpData_req0.hit       := s1_port_hit(0) || s1_prefetch_hit(0)
  ICacheTable.log(
    data = ICacheDumpData_req0,
    en = isWriteICacheTable.orR && s1_fire,
    clock = clock,
    reset = reset
  )

  val ICacheDumpData_req1 = Wire(new ICacheDB)
  ICacheDumpData_req1.blk_paddr := getBlkAddr(s1_req_paddr(1))
  ICacheDumpData_req1.blk_vaddr := getBlkAddr(s1_req_vaddr(1))
  ICacheDumpData_req1.hit       := s1_port_hit(1) || s1_prefetch_hit(1)
  ICacheTable.log(
    data = ICacheDumpData_req1,
    en = isWriteICacheTable.orR && s1_fire && s1_double_line,
    clock = clock,
    reset = reset
  )

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

  XSPerfAccumulate("mainPipe_stage1_block_by_piq_cycles", s1_valid && s1_wait)

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

  s2_ready      := (s2_valid && s2_fetch_finish && !io.respStall) || !s2_valid
  s2_fire       := s2_valid && s2_fetch_finish && !io.respStall

  val s2_fencei_latch = holdReleaseLatch(valid = io.fencei && s2_valid, release = s2_fire, flush = false.B)

  /** s2 data */
  // val mmio = fromPMP.map(port => port.mmio) // TODO: handle it
  val (s2_req_paddr , s2_req_vaddr) = (RegEnable(s1_req_paddr, s1_fire), RegEnable(s1_req_vaddr, s1_fire))
  val s2_req_gpaddr           = RegEnable(s1_req_gpaddr,        s1_fire)
  val s2_req_vsetIdx          = RegEnable(s1_req_vsetIdx, 0.U.asTypeOf(s1_req_vsetIdx), s1_fire)
  val s2_req_ptags            = RegEnable(s1_req_ptags,         s1_fire)
  val s2_double_line          = RegEnable(s1_double_line,       s1_fire)
  val s2_port_hit             = RegEnable(s1_port_hit,          s1_fire)
  val s2_waymask              = RegEnable(s1_victim_oh,         s1_fire)
  val s2_tag_match_vec        = RegEnable(s1_tag_match_vec,     s1_fire)

  val s2_meta_errors          = RegEnable(s1_meta_errors,    s1_fire)
  val s2_data_errorBits       = RegEnable(s1_data_errorBits, s1_fire)
  val s2_data_cacheline       = RegEnable(s1_data_cacheline, s1_fire)

  /** send req info of s1 and s2 to IPrefetchPipe for filter request */
  toIPrefetch.s1Info(0).paddr  := s1_req_paddr(0)
  toIPrefetch.s1Info(0).valid  := s1_valid
  toIPrefetch.s1Info(1).paddr  := s1_req_paddr(1)
  toIPrefetch.s1Info(1).valid  := s1_valid && s1_double_line
  toIPrefetch.s2Info(0).paddr  := s2_req_paddr(0)
  toIPrefetch.s2Info(0).valid  := s2_valid
  toIPrefetch.s2Info(1).paddr  := s2_req_paddr(1)
  toIPrefetch.s2Info(1).valid  := s2_valid && s2_double_line

  assert(RegNext(!s2_valid || s2_req_paddr(0)(11,0) === s2_req_vaddr(0)(11,0), true.B))

  /**
    ******************************************************************************
    * tlb exception and pmp logic
    ******************************************************************************
    */
  // short delay exception signal
  val s2_except_tlb_pf  = RegEnable(tlbExcpPF, s1_fire)
  val s2_except_tlb_gpf = RegEnable(tlbExcpGPF, s1_fire)
  val s2_except_tlb_af  = RegEnable(tlbExcpAF, s1_fire)
  val s2_except_tlb     = VecInit(Seq(s2_except_tlb_pf(0) || s2_except_tlb_af(0) || s2_except_tlb_gpf(0), s2_double_line && (s2_except_tlb_pf(1) || s2_except_tlb_af(1) || s2_except_tlb_gpf(1))))
  val s2_has_except_tlb = s2_valid && s2_except_tlb.reduce(_||_)
  // long delay exception signal
  // exception information and mmio
  val pmpExcpAF = VecInit(Seq(fromPMP(0).instr, fromPMP(1).instr && s2_double_line))
  val s2_except_pmp_af = DataHoldBypass(pmpExcpAF, RegNext(s1_fire))
  val s2_mmio = s2_valid && DataHoldBypass(fromPMP(0).mmio && !s2_except_tlb(0) && !s2_except_pmp_af(0), RegNext(s1_fire)).asBool
  // pmp port
  toPMP.zipWithIndex.map { case (p, i) =>
    p.valid     := s2_valid
    p.bits.addr := s2_req_paddr(i)
    p.bits.size := 3.U // TODO
    p.bits.cmd  := TlbCmd.exec
  }

  /**
    ******************************************************************************
    * look last miss data
    ******************************************************************************
    */
  class MissSlot(implicit p: Parameters) extends ICacheBundle {
    val paddr     = RegInit(0.U(PAddrBits.W))
    val vSetIdx   = RegInit(0.U(idxBits.W))
    val waymask   = RegInit(0.U(nWays.W))
    val data      = RegInit(0.U(blockBits.W))
    val corrupt   = RegInit(false.B)
    val finish    = RegInit(true.B)
    val valid     = RegInit(false.B)
    def data_vec  = data.asTypeOf(Vec(2, UInt((blockBits/2).W)))
    def pTag      = get_phy_tag(paddr)
  }
  val missSlot    = Seq.fill(2)(new MissSlot)

  // whether hit in last miss req
  def getMissSituat(missNum : Int, slotNum : Int ) :Bool =  {
    (missSlot(slotNum).vSetIdx === s1_req_vsetIdx(missNum)) &&
    (missSlot(slotNum).pTag === s1_req_ptags(missNum)) &&
    !missSlot(slotNum).corrupt && missSlot(slotNum).finish &&
    missSlot(slotNum).valid
  }

  // s2_hit_slot(0)(1): port 0 hit slot 1
  // Use the signal of S1 to make a judgment for timing, the value of missSlot has benn set when s1 fire
  val s1_hit_slot_vec = VecInit((0 until PortNumber).map(port => VecInit((0 until PortNumber).map(getMissSituat(port, _)))))
  val s2_hit_slot_vec = RegEnable(s1_hit_slot_vec, s1_fire)

  // select one from two missSlots to handle miss for every port
  // slot(0) hit  && slot(1) hit : don't case
  // slot(0) hit  && slot(1) miss: (a) missed port(0) -> slot(1); (b) missed port(1) -> slot(1)
  // slot(0) miss && slot(1) hit : (a) missed port(0) -> slot(0); (b) missed port(1) -> slot(0)
  // slot(0) miss && slot(1) miss: missed port(0) -> slot(0)  missed port(1) -> slot(1)
  val s1_curr_slot_id = Wire(Vec(2, Bool()))
  s1_curr_slot_id(0) := s1_hit_slot_vec(0)(0) || s1_hit_slot_vec(1)(0)
  s1_curr_slot_id(1) := !(s1_hit_slot_vec(0)(1) || s1_hit_slot_vec(1)(1))
  val s2_curr_slot_id = RegEnable(s1_curr_slot_id, s1_fire)

  /**
    ******************************************************************************
    * miss handle
    ******************************************************************************
    */
  val s2_hit_slot = VecInit(s2_hit_slot_vec.map(_.asUInt.orR))
  val s2_fixed_port_hit = VecInit((0 until PortNumber).map(port => s2_port_hit(port) || s2_hit_slot(port)))

  // only handle port0 miss when port1 have tlb except or pmp except
  val s2_port_miss = Wire(Vec(PortNumber, Bool()))
  
  s2_port_miss(0) := !s2_fixed_port_hit(0) && !s2_except_tlb(0) && !s2_except_pmp_af(0) && !s2_mmio
  s2_port_miss(1) := !s2_fixed_port_hit(1) && s2_double_line && !s2_except_tlb(0) && !s2_except_tlb(1) &&
                     !s2_except_pmp_af(0) && !s2_except_pmp_af(1) && !s2_mmio

  (0 until PortNumber).map{ i =>
    when(io.fencei) {
      missSlot(i).valid  := false.B
    }.elsewhen(s2_port_miss(i) && RegNext(s1_fire)) {
      when(s2_curr_slot_id(i)) {
        missSlot(1).vSetIdx := s2_req_vsetIdx(i)
        missSlot(1).paddr   := s2_req_paddr(i)
        missSlot(1).waymask := s2_waymask(i)
        missSlot(1).finish  := false.B
        missSlot(1).valid   := true.B
      }.otherwise {
        missSlot(0).vSetIdx := s2_req_vsetIdx(i)
        missSlot(0).paddr   := s2_req_paddr(i)
        missSlot(0).waymask := s2_waymask(i)
        missSlot(0).finish  := false.B
        missSlot(0).valid   := true.B
      }
    }
  }

  // which missSlot need to be issued
  val s2_missSlot_issue = Wire(Vec(2, Bool()))
  s2_missSlot_issue(0) := (s2_port_miss(0) && !s2_curr_slot_id(0)) || (s2_port_miss(1) && !s2_curr_slot_id(1))
  s2_missSlot_issue(1) := (s2_port_miss(0) && s2_curr_slot_id(0)) || (s2_port_miss(1) && s2_curr_slot_id(1))

  // state machine
  val m_idle ::m_send_req :: m_wait_resp :: Nil = Enum(3)
  val missStateQueue = RegInit(VecInit(Seq.fill(2)(m_idle)))

  (0 until PortNumber).map{ i =>
    switch(missStateQueue(i)){
      is(m_idle) {
        missStateQueue(i) := Mux(RegNext(s1_fire) && s2_missSlot_issue(i) && !io.fencei, m_send_req, m_idle)
      }
      is(m_send_req) {
        missStateQueue(i) := Mux(toMSHR(i).fire, m_wait_resp, m_send_req)
      }
      is(m_wait_resp) {
        missStateQueue(i) := Mux(fromMSHR(i).fire, m_idle, m_wait_resp)
      }
    }
  }

  // send req to MSHR
  (0 until PortNumber).map{i =>
    toMSHR(i).valid         := missStateQueue(i) === m_send_req
    toMSHR(i).bits.paddr    := missSlot(i).paddr
    toMSHR(i).bits.vSetIdx  := missSlot(i).vSetIdx
    toMSHR(i).bits.waymask  := missSlot(i).waymask
  }

  // recrive resp from MSHR to update missSlot
  (0 until PortNumber).map{ i =>
    when((missStateQueue(i) === m_wait_resp) && fromMSHR(i).fire) {
      missSlot(i).finish  := true.B
      missSlot(i).data    := fromMSHR(i).bits.data
      missSlot(i).corrupt := fromMSHR(i).bits.corrupt
    }
  }

  // handle miss finish
  s2_fetch_finish := (!s2_port_miss(0) && !s2_port_miss(1)) || (missSlot(0).finish && missSlot(1).finish && !RegNext(s1_fire))

  /**
    ******************************************************************************
    * select data from hitted sram data, last missSlot and current missSlot
    ******************************************************************************
    */
  val s2_hit_datas = Wire(Vec(2, UInt((blockBits/2).W)))
  s2_hit_datas(0) := Mux1H(s2_tag_match_vec(0).asUInt, s2_data_cacheline(0))
  s2_hit_datas(1) := Mux1H(Mux(s2_double_line, s2_tag_match_vec(1).asUInt, s2_tag_match_vec(0).asUInt), s2_data_cacheline(1))

  // get cacheline from last slot
  val s2_last_slot_cacheline = (0 until PortNumber).map(port => Mux1H(s2_hit_slot_vec(port).asUInt, missSlot.map(_.data_vec)))
  // get cacheline from curr slot
  val s2_curr_slot_cacheline = (0 until PortNumber).map(port => Mux(s2_curr_slot_id(port), missSlot(1).data_vec, missSlot(0).data_vec))
  val s2_slot_cacheline = (0 until PortNumber).map(port => Mux(s2_hit_slot(port), s2_last_slot_cacheline(port), s2_curr_slot_cacheline(port)))
  val s2_slot_data = Wire(Vec(PortNumber, UInt((blockBits/2).W)))
  s2_slot_data(0) := Mux(s2_double_line, s2_slot_cacheline(0)(1), s2_slot_cacheline(0)(0))
  s2_slot_data(1) := Mux(s2_double_line, s2_slot_cacheline(1)(0), s2_slot_cacheline(0)(1))

  val s2_fetch_data = Wire(Vec(2, UInt((blockBits/2).W)))
  s2_fetch_data(0) := Mux(s2_port_hit(0), s2_hit_datas(0), s2_slot_data(0))
  s2_fetch_data(1) := Mux(s2_port_hit(1) || (s2_port_hit(0) && !s2_double_line), s2_hit_datas(1), s2_slot_data(1))

  val s2_corrupt = (0 until PortNumber).map(port => s2_port_miss(port) && Mux(s2_curr_slot_id(port), missSlot(1).corrupt, missSlot(0).corrupt))

  /**
    ******************************************************************************
    * IFU data resp
    ******************************************************************************
    */
  (0 until PortNumber).map{ i =>
    if(i == 0) toIFU(i).valid         := s2_fire && !s2_fencei_latch
      else     toIFU(i).valid         := s2_fire && !s2_fencei_latch && s2_double_line
    toIFU(i).bits.paddr               := s2_req_paddr(i)
    toIFU(i).bits.gpaddr              := s2_req_gpaddr(i)
    toIFU(i).bits.vaddr               := s2_req_vaddr(i)
    toIFU(i).bits.data                := s2_fetch_data(i)
    toIFU(i).bits.tlbExcp.pageFault   := s2_except_tlb_pf(i)
    toIFU(i).bits.tlbExcp.guestPageFault:= s2_except_tlb_gpf(i)
    toIFU(i).bits.tlbExcp.accessFault := s2_except_tlb_af(i) || s2_corrupt(i) || s2_except_pmp_af(i)
    toIFU(i).bits.tlbExcp.mmio        := s2_mmio
  }

  /**
    ******************************************************************************
    * error resp: MSHR error
    ******************************************************************************
    */
  // data/meta parity error
  val s2_data_errors = Wire(Vec(PortNumber,Vec(nWays, Bool())))

  val s1_fire_delay1 = RegNext(s1_fire,init = false.B)
  val s1_fire_delay2 = RegNext(s1_fire_delay1, init = false.B)
  val s1_double_line_delay2 = RegNext(RegNext(s1_double_line))

  (0 until PortNumber).map{ i =>
    val read_datas = s2_data_cacheline(i).asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeUnit.W))))
    val read_codes = s2_data_errorBits(i).asTypeOf(Vec(nWays,Vec(dataCodeUnitNum, UInt(dataCodeBits.W))))
    val data_full_wayBits = VecInit((0 until nWays).map( w =>
                                  VecInit((0 until dataCodeUnitNum).map( u =>
                                        Cat(read_codes(w)(u), read_datas(w)(u))))))
    val data_error_wayBits = VecInit((0 until nWays).map( w =>
                                  VecInit((0 until dataCodeUnitNum).map( u =>
                                       cacheParams.dataCode.decode(data_full_wayBits(w)(u)).error))))
    // register for timing
    if(i == 0){
      (0 until nWays).map{ w =>
        s2_data_errors(i)(w) := s1_fire_delay2 && RegEnable(data_error_wayBits(w),s1_fire_delay1).reduce(_||_)
      }
    } else {
      (0 until nWays).map{ w =>
        s2_data_errors(i)(w) := s1_fire_delay2 && s1_double_line_delay2 && RegEnable(data_error_wayBits(w),s1_fire_delay1).reduce(_||_)
      }
    }
  }

  val s2_parity_meta_error  = VecInit((0 until PortNumber).map(i => s2_meta_errors(i).reduce(_||_) && io.csr_parity_enable))
  val s2_parity_data_error  = VecInit((0 until PortNumber).map(i => s2_data_errors(i).reduce(_||_) && io.csr_parity_enable))
  val s2_parity_error       = VecInit((0 until PortNumber).map(i => RegNext(s2_parity_meta_error(i)) || s2_parity_data_error(i)))

  for(i <- 0 until PortNumber){
    val valid                      = s2_parity_error(i) && s1_fire_delay2
    io.errors(i).valid            := RegNext(valid)
    io.errors(i).report_to_beu    := RegNext(valid)
    io.errors(i).paddr            := RegEnable(RegEnable(s2_req_paddr(i), s1_fire_delay1), valid)
    io.errors(i).source           := DontCare
    io.errors(i).source.tag       := RegEnable(RegEnable(s2_parity_meta_error(i), s1_fire_delay1), valid)
    io.errors(i).source.data      := RegEnable(s2_parity_data_error(i), valid)
    io.errors(i).source.l2        := false.B
    io.errors(i).opType           := DontCare
    io.errors(i).opType.fetch     := true.B
  }

  // MSHR error
  (0 until PortNumber).map{ i =>
    when(RegNext(s2_fire && s2_corrupt(i))){
      io.errors(i).valid            := true.B
      io.errors(i).report_to_beu    := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).paddr            := RegEnable(s2_req_paddr(i),s1_fire_delay1)
      io.errors(i).source.tag       := false.B
      io.errors(i).source.data      := false.B
      io.errors(i).source.l2        := true.B
    }
  }

  /**
    ******************************************************************************
    * s2 prefetch port
    ******************************************************************************
    */
  (0 until PortNumber).foreach{ i =>
    // TODO: consider corrupt of missSlot
    toIPrefetch.missSlot(i).valid   := missSlot(i).valid
    toIPrefetch.missSlot(i).vSetIdx := missSlot(i).vSetIdx
    toIPrefetch.missSlot(i).ptag    := missSlot(i).pTag
  }

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  /** replacement status register */
  val port_touch_sets = Seq.fill(PortNumber)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val port_touch_ways = Seq.fill(PortNumber)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))))
  (port_touch_ways zip port_touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    /** update replacement status register: 0 is hit access/ 1 is miss access */
    t_s(0)         := s2_req_vsetIdx(i)(highestIdxBit, 1)
    // hit in slot will be ignored, which generate a repeated access
    t_w(0).valid   := s2_valid && s2_port_hit(i)
    t_w(0).bits    := OHToUInt(s2_tag_match_vec(i))

    t_s(1)         := s2_req_vsetIdx(i)(highestIdxBit, 1)
    t_w(1).valid   := s2_valid && s2_port_miss(i)
    t_w(1).bits    := OHToUInt(s2_waymask(i))
  }

  val touch_ways = VecInit((0 until PortNumber).map( i => Mux(s2_req_vsetIdx(i)(0), port_touch_ways(1), port_touch_ways(0))))
  val touch_sets = VecInit((0 until PortNumber).map( i => Mux(s2_req_vsetIdx(i)(0), port_touch_sets(1), port_touch_sets(0))))
  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
  // TODO: need choose one replacer according to the bankid
  IPFReplacer.waymask := UIntToOH(replacers(0).way(IPFReplacer.vsetIdx))

  /**
    ******************************************************************************
    * performance info. TODO: need to simplify the logic
    ***********************************************************s*******************
    */
  io.fetch.topdownIcacheMiss := s2_port_miss(0) || s2_port_miss(1)
  io.fetch.topdownItlbMiss := itlbMissStage(0)

  io.perfInfo.only_0_hit      :=  s2_fixed_port_hit(0) && !s2_double_line
  io.perfInfo.only_0_miss     := !s2_fixed_port_hit(0) && !s2_double_line
  io.perfInfo.hit_0_hit_1     :=  s2_fixed_port_hit(0) &&  s2_fixed_port_hit(1) && s2_double_line
  io.perfInfo.hit_0_miss_1    :=  s2_fixed_port_hit(0) && !s2_fixed_port_hit(1) && s2_double_line
  io.perfInfo.miss_0_hit_1    := !s2_fixed_port_hit(0) &&  s2_fixed_port_hit(1) && s2_double_line
  io.perfInfo.miss_0_miss_1   := !s2_fixed_port_hit(0) && !s2_fixed_port_hit(1) && s2_double_line
  io.perfInfo.hit_0_except_1  :=  s2_fixed_port_hit(0) && (s2_except_tlb(1) || s2_except_pmp_af(1)) && s2_double_line
  io.perfInfo.miss_0_except_1 := !s2_fixed_port_hit(0) && (s2_except_tlb(1) || s2_except_pmp_af(1)) && s2_double_line
  io.perfInfo.bank_hit(0)     :=  s2_fixed_port_hit(0)
  io.perfInfo.bank_hit(1)     :=  s2_fixed_port_hit(1) && s2_double_line
  io.perfInfo.except_0        := s2_except_tlb(0) || s2_except_pmp_af(0)
  io.perfInfo.hit             := !s2_port_miss(0) && !s2_port_miss(1)

  /** <PERF> fetch bubble generated by icache miss*/
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetch_finish )
  XSPerfAccumulate("icache_bubble_s0_tlb_miss", s1_valid && !tlbRespAllValid)

  /**
    ******************************************************************************
    * difftest refill check
    ******************************************************************************
    */
  if (env.EnableDifftest) {
    val discards = (0 until PortNumber).map { i =>
      val discard = toIFU(i).bits.tlbExcp.pageFault || toIFU(i).bits.tlbExcp.guestPageFault || toIFU(i).bits.tlbExcp.accessFault || toIFU(i).bits.tlbExcp.mmio
      discard
    }
    (0 until PortNumber).map { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index := (4 + i).U
      if (i == 0) {
        diffMainPipeOut.valid := s2_fire && !discards(0)
        diffMainPipeOut.addr  := s2_req_paddr(0)
      } else {
        diffMainPipeOut.valid := s2_fire && !discards(0) && (!s2_double_line || (s2_double_line && !discards(1)))
        diffMainPipeOut.addr  := s2_req_paddr(0) + (blockBits/2).U
      }
      diffMainPipeOut.data := Cat(0.U((blockBits/2).W), toIFU(i).bits.data).asTypeOf(diffMainPipeOut.data)
      // idtfr: 0 -> data from icache 1 -> reversedData 2 -> data from missUnit
      diffMainPipeOut.idtfr := Mux(s2_port_hit(i), 0.U, Mux(s2_fixed_port_hit(i), 1.U, 2.U))
      diffMainPipeOut
    }
  }
}
