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
  def vSetIdx = get_idx(vaddr)
}

class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle
{
  val vaddr    = UInt(VAddrBits.W)
  // val registerData = UInt(blockBits.W)
  // val sramData = UInt(blockBits.W)
  // val select   = Bool()
  val data = UInt((blockBits/2).W)
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
  val topdownIcacheMiss = Output(Bool())
  val topdownItlbMiss = Output(Bool())
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIMeta       = DecoupledIO(new ICacheReadBundle)
  val fromIMeta     = Input(new ICacheMetaRespBundle)
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle{
  val toIData       = Vec(partWayNum, DecoupledIO(new ICacheReadBundle))
  val fromIData     = Input(new ICacheDataRespBundle)
}

class ICacheMSHRBundle(implicit p: Parameters) extends ICacheBundle{
  val req   = Decoupled(new ICacheMissReq)
  val resp  = Flipped(ValidIO(new ICacheMissResp))
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
  val hartId = Input(UInt(8.W))
  /*** internal interface ***/
  val dataArray     = new ICacheDataReqBundle
  /** prefetch io */
  val touch = Vec(PortNumber,ValidIO(new ReplacerTouch))
  val wayLookupRead = Flipped(DecoupledIO(new WayLookupRead))

  val mshr          = new ICacheMSHRBundle
  val errors        = Output(Vec(PortNumber, new L1CacheErrorInfo))
  /*** outside interface ***/
  //val fetch       = Vec(PortNumber, new ICacheMainPipeBundle)
  /* when ftq.valid is high in T + 1 cycle
   * the ftq component must be valid in T cycle
   */
  val fetch       = new ICacheMainPipeBundle
  val pmp         = Vec(PortNumber, new ICachePMPBundle)
  val respStall   = Input(Bool())

  val csr_parity_enable = Input(Bool())
  val flush = Input(Bool())

  val perfInfo = Output(new ICachePerfInfo)
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
  val (toData,  fromData) = (io.dataArray.toIData,  io.dataArray.fromIData)
  val (toMSHR,  fromMSHR) = (io.mshr.req,           io.mshr.resp)
  val (toPMP,   fromPMP)  = (io.pmp.map(_.req),     io.pmp.map(_.resp))
  val fromWayLookup = io.wayLookupRead

  // Statistics on the frequency distribution of FTQ fire interval
  val cntFtqFireInterval = RegInit(0.U(32.W))
  cntFtqFireInterval := Mux(fromFtq.fire, 1.U, cntFtqFireInterval + 1.U)
  XSPerfHistogram("ftq2icache_fire_" + p(XSCoreParamsKey).HartId.toString, 
                  cntFtqFireInterval, fromFtq.fire,
                  1, 300, 1, right_strict = true)

  /** pipeline control signal */
  val s1_ready, s2_ready = Wire(Bool())
  val s0_fire,  s1_fire , s2_fire  = Wire(Bool())
  val s0_flush,  s1_flush , s2_flush  = Wire(Bool())

  /**
    ******************************************************************************
    * ICache Stage 0
    * - send req to data SRAM
    * - get waymask and tlb info from wayLookup
    ******************************************************************************
    */

  /** s0 control */
  // 0,1,2,3 -> dataArray(data); 4 -> mainPipe
  // Ftq RegNext Register
  val fromFtqReq          = fromFtq.bits.pcMemRead
  val s0_valid            = fromFtq.valid
  val s0_req_valid_all    = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i))
  val s0_req_vaddr_all    = (0 until partWayNum + 1).map(i => VecInit(Seq(fromFtqReq(i).startAddr, fromFtqReq(i).nextlineStart)))
  val s0_req_vSetIdx_all  = (0 until partWayNum + 1).map(i => VecInit(s0_req_vaddr_all(i).map(get_idx(_))))
  val s0_doubleline_all   = (0 until partWayNum + 1).map(i => fromFtq.bits.readValid(i) && fromFtqReq(i).crossCacheline)

  val s0_req_vaddr        = s0_req_vaddr_all.last
  val s0_req_vSetIdx      = s0_req_vSetIdx_all.last
  val s0_doubleline       = s0_doubleline_all.last

  /**
    ******************************************************************************
    * get waymask and tlb info from wayLookup
    ******************************************************************************
    */
  fromWayLookup.ready := s0_fire
  val s0_waymasks     = VecInit(fromWayLookup.bits.waymask.map(_.asTypeOf(Vec(nWays, Bool()))))
  val s0_req_ptags    = fromWayLookup.bits.ptag
  val s0_excp_tlb_af  = fromWayLookup.bits.excp_tlb_af
  val s0_excp_tlb_pf  = fromWayLookup.bits.excp_tlb_pf
  val s0_hits         = VecInit((0 until PortNumber).map(i=> s0_waymasks(i).reduce(_||_)))

  when(s0_fire){
    assert((0 until PortNumber).map(i => s0_req_vSetIdx(i) === fromWayLookup.bits.vSetIdx(i)).reduce(_&&_),
           "vSetIdxs from ftq and wayLookup are different! vaddr0=0x%x ftq: vidx0=0x%x vidx1=0x%x wayLookup: vidx0=0x%x vidx1=0x%x",
           s0_req_vaddr(0), s0_req_vSetIdx(0), s0_req_vSetIdx(1), fromWayLookup.bits.vSetIdx(0), fromWayLookup.bits.vSetIdx(1))
  }

  /**
    ******************************************************************************
    * data SRAM request
    ******************************************************************************
    */
  for(i <- 0 until nWays) {
    toData(i).valid             := s0_req_valid_all(i)
    toData(i).bits.isDoubleLine := s0_doubleline_all(i)
    toData(i).bits.vSetIdx      := s0_req_vSetIdx_all(i)
    toData(i).bits.waymask      := s0_waymasks
  }

  val s0_can_go = toData.last.ready && fromWayLookup.valid && s1_ready
  s0_flush  := io.flush
  s0_fire   := s0_valid && s0_can_go && !s0_flush

  fromFtq.ready := s0_can_go

  /**
    ******************************************************************************
    * ICache Stage 1
    * - PMP check
    * - get Data SRAM read responses (latched for pipeline stop)
    * - monitor missUint response port
    ******************************************************************************
    */
  val s1_valid = generatePipeControl(lastFire = s0_fire, thisFire = s1_fire, thisFlush = s1_flush, lastFlush = false.B)

  val s1_req_vaddr    = RegEnable(s0_req_vaddr, s0_fire)
  val s1_req_ptags    = RegEnable(s0_req_ptags, s0_fire)
  val s1_doubleline   = RegEnable(s0_doubleline, s0_fire)
  val s1_hits         = RegEnable(s0_hits, s0_fire)
  val s1_excp_tlb_af  = RegEnable(s0_excp_tlb_af, s0_fire)
  val s1_excp_tlb_pf  = RegEnable(s0_excp_tlb_pf, s0_fire)
  val s1_waymasks     = RegEnable(s0_waymasks, s0_fire)

  val s1_req_vSetIdx  = s1_req_vaddr.map(get_idx(_))
  val s1_req_paddr    = s1_req_vaddr.zip(s1_req_ptags).map{case(vaddr, ptag) => get_paddr_from_ptag(vaddr, ptag)}

  val s1_data_errors  = DataHoldBypass(fromData.errors, RegNext(s0_fire))

  /**
    ******************************************************************************
    * update replacement status register
    ******************************************************************************
    */
  (0 until PortNumber).foreach{ i =>
    io.touch(i).bits.vSetIdx  := s1_req_vSetIdx(i)
    io.touch(i).bits.way      := OHToUInt(s1_waymasks(i))
  }
  io.touch(0).valid := RegNext(s0_fire) && s1_hits(0)
  io.touch(1).valid := RegNext(s0_fire) && s1_hits(1) && s1_doubleline

  /**
    ******************************************************************************
    * PMP check
    ******************************************************************************
    */
  val pmpExcpAF       = VecInit(Seq(fromPMP(0).instr, fromPMP(1).instr && s1_doubleline))
  val s1_excp_pmp_af  = DataHoldBypass(pmpExcpAF, RegNext(s0_fire))
  // pmp port
  toPMP.zipWithIndex.map { case (p, i) =>
    p.valid     := s1_valid
    p.bits.addr := s1_req_paddr(i)
    p.bits.size := 3.U // TODO
    p.bits.cmd  := TlbCmd.exec
  }

  /**
    ******************************************************************************
    * select data from MSHR, SRAM
    ******************************************************************************
    */
  val s1_datas = RegInit(VecInit(Seq.fill(PortNumber)(0.U((blockBits/2).W))))

  val fromMSHR_dataVec = fromMSHR.bits.data.asTypeOf(Vec(PortNumber, UInt((blockBits/2).W)))
  val s1_MSHR_match    = VecInit((0 until PortNumber).map(i => (s1_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                               (s1_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                               fromMSHR.valid && !fromMSHR.bits.corrupt))
  val s1_MSHR_hits  = Seq(s1_valid && s1_MSHR_match(0),
                          s1_valid && ((s1_MSHR_match(0) && !s1_doubleline) || (s1_MSHR_match(1) && s1_doubleline)))
  val s1_MSHR_datas = Seq(Mux(s1_doubleline, fromMSHR_dataVec(1), fromMSHR_dataVec(0)),
                          Mux(s1_doubleline, fromMSHR_dataVec(0), fromMSHR_dataVec(1)))
  val s1_MSHR_hits_hold = (0 until PortNumber).map(i => ValidHoldBypass(s1_MSHR_hits(i), s1_fire || s1_flush))

  (0 until PortNumber).foreach{i =>
    when(s1_MSHR_hits(i)) {
      s1_datas(i) := s1_MSHR_datas(i)
    }.elsewhen(RegNext(s0_fire)) {
      s1_datas(i) := fromData.datas(i)
    }
  }

  val s1_final_datas = (0 until PortNumber).map(i => Mux(s1_MSHR_hits(i), s1_MSHR_datas(i), 
                                                         Mux(RegNext(s0_fire), fromData.datas(i), s1_datas(i))))
  val s1_final_hits = (0 until PortNumber).map(i => s1_MSHR_hits_hold(i) || s1_hits(i))

  s1_flush := io.flush
  s1_ready := s2_ready || !s1_valid
  s1_fire  := s1_valid && s2_ready && !s1_flush

  when(s1_valid) {
    assert(!(s1_hits(0) && s1_MSHR_match(0)) && (!s1_doubleline || !(s1_hits(1) && s1_MSHR_match(1))),
          "Multi hit in mainPipe s1: vaddr0=0x%x s1_hits(0)=%d s1_MSHR_match(0)=%d s1_hits(1)=%d s1_MSHR_match(1)=%d",
          s1_req_vaddr(0), s1_hits(0), s1_MSHR_match(0), s1_hits(1), s1_MSHR_match(1))
  }

  /**
    ******************************************************************************
    * ICache Stage 2
    * - send request to MSHR if ICache miss
    * - monitor missUint response port
    * - response to IFU
    ******************************************************************************
    */

  val s2_valid = generatePipeControl(lastFire = s1_fire, thisFire = s2_fire, thisFlush = s2_flush, lastFlush = false.B)

  val s2_req_vaddr      = RegEnable(s1_req_vaddr, s1_fire)
  val s2_req_ptags      = RegEnable(s1_req_ptags, s1_fire)
  val s2_doubleline     = RegEnable(s1_doubleline, s1_fire)
  val s2_excp_tlb_af    = RegEnable(s1_excp_tlb_af, s1_fire)
  val s2_excp_tlb_pf    = RegEnable(s1_excp_tlb_pf, s1_fire)
  val s2_excp_pmp_af    = RegEnable(VecInit(fromPMP.map(_.instr)), s1_fire)
  val s2_excp_pmp_mmio  = RegEnable(VecInit(fromPMP.map(_.mmio)), s1_fire)
  val s2_data_errors    = RegEnable(s1_data_errors, s1_fire)

  val s2_req_vSetIdx  = s2_req_vaddr.map(get_idx(_))
  val s2_req_paddr    = s2_req_vaddr.zip(s2_req_ptags).map{case(vaddr, ptag) => get_paddr_from_ptag(vaddr, ptag)}

  val s2_hits  = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  val s2_datas = RegInit(VecInit(Seq.fill(PortNumber)(0.U((blockBits/2).W))))

  /**
    ******************************************************************************
    * report data parity error
    ******************************************************************************
    */
  val s2_parity_error = VecInit(Seq(s2_data_errors(0) || (!s2_doubleline && s2_data_errors(1)),
                                    s2_doubleline && s2_data_errors(1)))
  (0 until PortNumber).map{ i =>
    io.errors(i).valid            := io.csr_parity_enable && RegNext(s1_fire) && s2_parity_error(i)
    io.errors(i).report_to_beu    := io.csr_parity_enable && RegNext(s1_fire) && s2_parity_error(i)
    io.errors(i).paddr            := s2_req_paddr(i)
    io.errors(i).source           := DontCare
    io.errors(i).source.tag       := false.B
    io.errors(i).source.data      := s2_parity_error(i)
    io.errors(i).source.l2        := false.B
    io.errors(i).opType           := DontCare
    io.errors(i).opType.fetch     := true.B
  }

  /**
    ******************************************************************************
    * monitor missUint response port
    ******************************************************************************
    */
  val s2_MSHR_match = VecInit((0 until PortNumber).map(i => (s2_req_vSetIdx(i) === fromMSHR.bits.vSetIdx) &&
                                                            (s2_req_ptags(i) === getPhyTagFromBlk(fromMSHR.bits.blkPaddr)) &&
                                                            fromMSHR.valid))
  val s2_MSHR_datas = Seq(Mux(s2_doubleline, fromMSHR_dataVec(1), fromMSHR_dataVec(0)),
                          Mux(s2_doubleline, fromMSHR_dataVec(0), fromMSHR_dataVec(1)))
  val s2_MSHR_hits  = Seq(s2_valid && s2_MSHR_match(0),
                          s2_valid && ((s2_MSHR_match(0) && !s2_doubleline) || (s2_MSHR_match(1) && s2_doubleline)))

  (0 until PortNumber).foreach{i =>
    when(s1_fire) {
      s2_hits(i) := s1_final_hits(i)
    }.elsewhen(s2_MSHR_hits(i)) {
      s2_hits(i) := true.B
    }
  }

  (0 until PortNumber).foreach{i =>
    when(s1_fire) {
      s2_datas(i) := s1_final_datas(i)
    }.elsewhen(s2_MSHR_hits(i)) {
      s2_datas(i) := s2_MSHR_datas(i)
    }
  }

  val s2_corrupt = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach{i =>
    when(s1_fire) {
      s2_corrupt(i) := false.B
    }.elsewhen(s2_MSHR_hits(i)) {
      s2_corrupt(i) := fromMSHR.bits.corrupt
    }
  }

  when(s1_valid) {
    assert(!(s2_hits(0) && s2_MSHR_match(0)) && (!s2_doubleline || !(s2_hits(1) && s2_MSHR_match(1))),
          "Multi hit in mainPipe s2: vaddr0=0x%x s2_hits(0)=%d s2_MSHR_match(0)=%d s2_hits(1)=%d s2_MSHR_match(1)=%d",
          s2_req_vaddr(0), s2_hits(0), s2_MSHR_match(0), s2_hits(1), s2_MSHR_match(1))
  }

  /**
    ******************************************************************************
    * send request to MSHR if ICache miss
    ******************************************************************************
    */
  val s2_excp_tlb = VecInit((0 until PortNumber).map(i => s2_excp_tlb_af(i) || s2_excp_tlb_pf(i)))
  val s2_miss = Wire(Vec(2, Bool()))
  s2_miss(0) := !s2_hits(0) && !s2_excp_tlb(0) && !s2_excp_pmp_af(0) && !s2_excp_pmp_mmio(0)
  s2_miss(1) := s2_doubleline && !s2_hits(1) && !s2_excp_tlb(0) && !s2_excp_tlb(1) &&
                !s2_excp_pmp_af(0) && !s2_excp_pmp_af(1) && !s2_excp_pmp_mmio(0)
  
  val toMSHRArbiter = Module(new Arbiter(new ICacheMissReq, PortNumber))

  // To avoid sending duplicate requests.
  val has_send = RegInit(VecInit(Seq.fill(PortNumber)(false.B)))
  (0 until PortNumber).foreach{ i =>
    when(s1_fire) {
      has_send(i) := false.B
    }.elsewhen(toMSHRArbiter.io.in(i).fire) {
      has_send(i) := true.B
    }
  }

  (0 until PortNumber).map{ i =>
    toMSHRArbiter.io.in(i).valid          := s2_valid && s2_miss(i) && !has_send(i) && !s2_flush
    toMSHRArbiter.io.in(i).bits.blkPaddr  := getBlkAddr(s2_req_paddr(i))
    toMSHRArbiter.io.in(i).bits.vSetIdx   := s2_req_vSetIdx(i)
  }
  toMSHR <> toMSHRArbiter.io.out

  XSPerfAccumulate("to_missUnit_stall",  toMSHR.valid && !toMSHR.ready)

  val s2_fetch_finish = !s2_miss.reduce(_||_)

  /**
    ******************************************************************************
    * response to IFU
    ******************************************************************************
    */
  (0 until PortNumber).map{ i =>
    if(i == 0) {
      toIFU(i).valid                    := s2_fire
      toIFU(i).bits.tlbExcp.pageFault   := s2_excp_tlb_pf(i)
      toIFU(i).bits.tlbExcp.accessFault := s2_excp_tlb_af(i) || s2_excp_pmp_af(i) || s2_corrupt(i)
      toIFU(i).bits.tlbExcp.mmio        := s2_excp_pmp_mmio(0) && !s2_excp_tlb(0) && !s2_excp_pmp_af(0)
    } else {
      toIFU(i).valid                    := s2_fire && s2_doubleline
      toIFU(i).bits.tlbExcp.pageFault   := s2_excp_tlb_pf(i) && s2_doubleline
      toIFU(i).bits.tlbExcp.accessFault := (s2_excp_tlb_af(i) || s2_excp_pmp_af(i) || s2_corrupt(i)) && s2_doubleline
      toIFU(i).bits.tlbExcp.mmio        := (s2_excp_pmp_mmio(0) && !s2_excp_tlb(0) && !s2_excp_pmp_af(0)) && s2_doubleline
    }
    toIFU(i).bits.vaddr                 := s2_req_vaddr(i)
    toIFU(i).bits.paddr                 := s2_req_paddr(i)
    toIFU(i).bits.data                  := s2_datas(i)
  }

  s2_flush := io.flush
  s2_ready := (s2_fetch_finish && !io.respStall) || !s2_valid
  s2_fire  := s2_valid && s2_fetch_finish && !io.respStall && !s2_flush
  
  /**
    ******************************************************************************
    * report Tilelink corrupt error
    ******************************************************************************
    */
  (0 until PortNumber).map{ i =>
    when(RegNext(s2_fire && s2_corrupt(i))){
      io.errors(i).valid            := true.B
      io.errors(i).report_to_beu    := false.B // l2 should have report that to bus error unit, no need to do it again
      io.errors(i).paddr            := RegNext(s2_req_paddr(i))
      io.errors(i).source.tag       := false.B
      io.errors(i).source.data      := false.B
      io.errors(i).source.l2        := true.B
    }
  }

  /**
    ******************************************************************************
    * performance info. TODO: need to simplify the logic
    ***********************************************************s*******************
    */
  io.perfInfo.only_0_hit      :=  s2_hits(0) && !s2_doubleline
  io.perfInfo.only_0_miss     := !s2_hits(0) && !s2_doubleline
  io.perfInfo.hit_0_hit_1     :=  s2_hits(0) &&  s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_miss_1    :=  s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_hit_1    := !s2_hits(0) &&  s2_hits(1) && s2_doubleline
  io.perfInfo.miss_0_miss_1   := !s2_hits(0) && !s2_hits(1) && s2_doubleline
  io.perfInfo.hit_0_except_1  :=  s2_hits(0) && (s2_excp_tlb(1) || s2_excp_pmp_af(1)) && s2_doubleline
  io.perfInfo.miss_0_except_1 := !s2_hits(0) && (s2_excp_tlb(1) || s2_excp_pmp_af(1)) && s2_doubleline
  io.perfInfo.bank_hit(0)     :=  s2_hits(0)
  io.perfInfo.bank_hit(1)     :=  s2_hits(1) && s2_doubleline
  io.perfInfo.except_0        :=  s2_excp_tlb(0) || s2_excp_pmp_af(0)
  io.perfInfo.hit             :=  s2_hits(0) && (!s2_doubleline || s2_hits(1))

  /** <PERF> fetch bubble generated by icache miss */
  XSPerfAccumulate("icache_bubble_s2_miss", s2_valid && !s2_fetch_finish )
  XSPerfAccumulate("icache_bubble_s0_wayLookup", s0_valid && !fromWayLookup.ready)

  io.fetch.topdownIcacheMiss := !s2_fetch_finish
  io.fetch.topdownItlbMiss := s0_valid && !fromWayLookup.ready

  // class ICacheTouchDB(implicit p: Parameters) extends ICacheBundle{
  //   val blkPaddr  = UInt((PAddrBits - blockOffBits).W)
  //   val vSetIdx   = UInt(idxBits.W)
  //   val waymask   = UInt(log2Ceil(nWays).W)
  // }

  // val isWriteICacheTouchTable = WireInit(Constantin.createRecord("isWriteICacheTouchTable" + p(XSCoreParamsKey).HartId.toString))
  // val ICacheTouchTable = ChiselDB.createTable("ICacheTouchTable" + p(XSCoreParamsKey).HartId.toString, new ICacheTouchDB)

  // val ICacheTouchDumpData = Wire(Vec(PortNumber, new ICacheTouchDB))
  // (0 until PortNumber).foreach{ i =>
  //   ICacheTouchDumpData(i).blkPaddr  := getBlkAddr(s2_req_paddr(i))
  //   ICacheTouchDumpData(i).vSetIdx   := s2_req_vSetIdx(i)
  //   ICacheTouchDumpData(i).waymask   := OHToUInt(s2_tag_match_vec(i))
  //   ICacheTouchTable.log(
  //     data  = ICacheTouchDumpData(i),
  //     en    = io.touch(i).valid,
  //     site  = "req_" + i.toString,
  //     clock = clock,
  //     reset = reset
  //   )
  // }

  /**
    ******************************************************************************
    * difftest refill check
    ******************************************************************************
    */
  if (env.EnableDifftest) {
    val discards = (0 until PortNumber).map { i =>
      val discard = toIFU(i).bits.tlbExcp.pageFault || toIFU(i).bits.tlbExcp.accessFault || toIFU(i).bits.tlbExcp.mmio
      discard
    }
    (0 until PortNumber).map { i =>
      val diffMainPipeOut = DifftestModule(new DiffRefillEvent, dontCare = true)
      diffMainPipeOut.coreid := io.hartId
      diffMainPipeOut.index := (3 + i).U
      if (i == 0) {
        diffMainPipeOut.valid := s2_fire && !discards(0)
        diffMainPipeOut.addr  := s2_req_paddr(0)
      } else {
        diffMainPipeOut.valid := s2_fire && !discards(0) && (!s2_doubleline || (s2_doubleline && !discards(1)))
        diffMainPipeOut.addr  := Mux(s2_doubleline,
                                     Cat(getBlkAddr(s2_req_paddr(1)), 0.U(blockOffBits.W)),
                                     s2_req_paddr(0) + (blockBytes/2).U)
      }
      diffMainPipeOut.data := Cat(0.U((blockBits/2).W), toIFU(i).bits.data).asTypeOf(diffMainPipeOut.data)
      diffMainPipeOut.idtfr := DontCare
      diffMainPipeOut
    }
  }
}