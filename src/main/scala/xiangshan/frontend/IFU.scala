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

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._
import chisel3.experimental.verification
import freechips.rocketchip.tilelink.MemoryOpCategories.M_FLUSH
import freechips.rocketchip.tilelink.TLPermissions.toN
import utils._

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth /8
  def mmioBeats = FetchWidth * 4 * 8 / mmioBusWidth
  def mmioMask  = VecInit(List.fill(PredictWidth)(true.B)).asUInt
}

trait HasIFUConst extends HasXSParameter {
  def addrAlign(addr: UInt, bytes: Int, highest: Int): UInt = Cat(addr(highest-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
}

class IfuToFtqIO(implicit p:Parameters) extends XSBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(new FtqToIfuIO)
  val toFtq   = new IfuToFtqIO
}

class ICacheInterface(implicit p: Parameters) extends XSBundle {
  val toIMeta       = Decoupled(new ICacheReadBundle)
  val toIData       = Decoupled(new ICacheReadBundle)
  val toMissQueue   = Vec(2, Decoupled(new ICacheMissReq))
  val toReleaseUnit = Vec(2, Decoupled(new RealeaseReq))
  val fromIMeta     = Input(new ICacheMetaRespBundle)
  val fromIData     = Input(new ICacheDataRespBundle)
  val fromMissQueue = Vec(2,Flipped(ValidIO(new ICacheMissResp)))
}

class NewIFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface
  val icacheInter     = new ICacheInterface
  val toIbuffer       = Decoupled(new FetchToIBuffer)
  val iTLBInter       = Vec(2, new BlockTlbRequestIO)
}

// record the situation in which fallThruAddr falls into
// the middle of an RVI inst
class LastHalfInfo(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val middlePC = UInt(VAddrBits.W)
  def matchThisBlock(startAddr: UInt) = valid && middlePC === startAddr
}

class IfuToPreDecode(implicit p: Parameters) extends XSBundle {
  val data          = if(HasCExtension) Vec(PredictWidth + 1, UInt(16.W)) else Vec(PredictWidth, UInt(32.W))
  val startAddr     = UInt(VAddrBits.W)
  val fallThruAddr  = UInt(VAddrBits.W)
  val fallThruError = Bool()
  val isDoubleLine  = Bool()
  val ftqOffset     = Valid(UInt(log2Ceil(PredictWidth).W))
  val target        = UInt(VAddrBits.W)
  val pageFault     = Vec(2, Bool())
  val accessFault   = Vec(2, Bool())
  val instValid     = Bool()
  val lastHalfMatch = Bool()
  val oversize      = Bool()
}

class NewIFU(implicit p: Parameters) extends XSModule with HasICacheParameters
{
  println(s"icache ways: ${nWays} sets:${nSets}")
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toMeta, toData, meta_resp, data_resp) =  (io.icacheInter.toIMeta, io.icacheInter.toIData, io.icacheInter.fromIMeta, io.icacheInter.fromIData)
  val (toMissQueue, fromMissQueue) = (io.icacheInter.toMissQueue, io.icacheInter.fromMissQueue)
  val toRealseUnit   = io.icacheInter.toReleaseUnit
  val (toITLB, fromITLB) = (VecInit(io.iTLBInter.map(_.req)), VecInit(io.iTLBInter.map(_.resp)))

  def isCrossLineReq(start: UInt, end: UInt): Bool = start(blockOffBits) ^ end(blockOffBits)

  def isLastInCacheline(fallThruAddr: UInt): Bool = fallThruAddr(blockOffBits - 1, 1) === 0.U


  //---------------------------------------------
  //  Fetch Stage 1 :
  //  * Send req to ICache Meta/Data
  //  * Check whether need 2 line fetch
  //---------------------------------------------

  val f0_valid                             = fromFtq.req.valid
  val f0_ftq_req                           = fromFtq.req.bits
  val f0_situation                         = VecInit(Seq(isCrossLineReq(f0_ftq_req.startAddr, f0_ftq_req.fallThruAddr), isLastInCacheline(f0_ftq_req.fallThruAddr)))
  val f0_doubleLine                        = f0_situation(0) || f0_situation(1)
  val f0_vSetIdx                           = VecInit(get_idx((f0_ftq_req.startAddr)), get_idx(f0_ftq_req.fallThruAddr))
  val f0_fire                              = fromFtq.req.fire()

  val f0_flush, f1_flush, f2_flush, f3_flush = WireInit(false.B)
  val from_bpu_f0_flush, from_bpu_f1_flush, from_bpu_f2_flush, from_bpu_f3_flush = WireInit(false.B)

  from_bpu_f0_flush := fromFtq.flushFromBpu.shouldFlushByStage2(f0_ftq_req.ftqIdx) ||
                       fromFtq.flushFromBpu.shouldFlushByStage3(f0_ftq_req.ftqIdx)

  val f3_redirect = WireInit(false.B)
  f3_flush := fromFtq.redirect.valid
  f2_flush := f3_flush || f3_redirect
  f1_flush := f2_flush || from_bpu_f1_flush
  f0_flush := f1_flush || from_bpu_f0_flush

  val f1_ready, f2_ready, f3_ready         = WireInit(false.B)

  //fetch: send addr to Meta/TLB and Data simultaneously
  val fetch_req = List(toMeta, toData)
  for(i <- 0 until 2) {
    fetch_req(i).valid := f0_fire
    fetch_req(i).bits.isDoubleLine := f0_doubleLine
    fetch_req(i).bits.vSetIdx := f0_vSetIdx
  }

  fromFtq.req.ready := fetch_req(0).ready && fetch_req(1).ready && f1_ready && GTimer() > 500.U

  XSPerfAccumulate("ifu_bubble_ftq_not_valid",   !f0_valid )
  XSPerfAccumulate("ifu_bubble_pipe_stall",    f0_valid && fetch_req(0).ready && fetch_req(1).ready && !f1_ready )
  XSPerfAccumulate("ifu_bubble_sram_0_busy",   f0_valid && !fetch_req(0).ready  )
  XSPerfAccumulate("ifu_bubble_sram_1_busy",   f0_valid && !fetch_req(1).ready  )

  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * Send req to ITLB and TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------

  //TODO: handle fetch exceptions

  val tlbRespAllValid = WireInit(false.B)

  val f1_valid      = RegInit(false.B)
  val f1_ftq_req    = RegEnable(next = f0_ftq_req,    enable=f0_fire)
  val f1_situation  = RegEnable(next = f0_situation,  enable=f0_fire)
  val f1_doubleLine = RegEnable(next = f0_doubleLine, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,    enable=f0_fire)
  val f1_fire       = f1_valid && tlbRespAllValid && f2_ready

  f1_ready := f2_ready && tlbRespAllValid || !f1_valid

  from_bpu_f1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx)

  val preDecoder      = Module(new PreDecode)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)

  //flush generate and to Ftq
  val predecodeOutValid = WireInit(false.B)

  when(f1_flush)                  {f1_valid  := false.B}
  .elsewhen(f0_fire && !f0_flush) {f1_valid  := true.B}
  .elsewhen(f1_fire)              {f1_valid  := false.B}

  toITLB(0).valid         := f1_valid
  toITLB(0).bits.vaddr    := addrAlign(f1_ftq_req.startAddr, blockBytes, VAddrBits)
  toITLB(0).bits.debug.pc := addrAlign(f1_ftq_req.startAddr, blockBytes, VAddrBits)

  toITLB(1).valid         := f1_valid && f1_doubleLine
  toITLB(1).bits.vaddr    := addrAlign(f1_ftq_req.fallThruAddr, blockBytes, VAddrBits)
  toITLB(1).bits.debug.pc := addrAlign(f1_ftq_req.fallThruAddr, blockBytes, VAddrBits)

  toITLB.map{port =>
    port.bits.cmd                 := TlbCmd.exec
    port.bits.robIdx              := DontCare
    port.bits.debug.isFirstIssue  := DontCare
  }

  fromITLB.map(_.ready := true.B)

  val (tlbRespValid, tlbRespPAddr) = (fromITLB.map(_.valid), VecInit(fromITLB.map(_.bits.paddr)))
  val (tlbRespMiss,  tlbRespMMIO)  = (fromITLB.map(port => port.bits.miss && port.valid), fromITLB.map(port => port.bits.mmio && port.valid))
  val (tlbExcpPF,    tlbExcpAF)    = (fromITLB.map(port => port.bits.excp.pf.instr && port.valid), fromITLB.map(port => (port.bits.excp.af.instr || port.bits.mmio) && port.valid)) //TODO: Temp treat mmio req as access fault

  tlbRespAllValid := tlbRespValid(0)  && (tlbRespValid(1) || !f1_doubleLine)

  val f1_pAddrs             = tlbRespPAddr
  val f1_pTags              = VecInit(f1_pAddrs.map(get_phy_tag(_)))
  val (f1_tags, f1_cohs)    = (meta_resp.tags,  meta_resp.cohs)
  val (f1_cacheline_valid, f1_datas)   = (meta_resp.valid, data_resp.datas)

  //hit and replaecement check
  //TODO: valid bit in metaData may be useless
  val release_confic_vec = Wire(Vec(4, Bool()))

  val f1_tag_eq_vec        = VecInit((0 until 2).map( k => VecInit(f1_tags(k).zipWithIndex.map{ case(way_tag,i) => f1_cacheline_valid(k)(i) && way_tag ===  f1_pTags(k) })))
  val f1_tag_match_vec     = VecInit((0 until 2).map( k => VecInit(f1_tag_eq_vec(k).zipWithIndex.map{ case(way_tag_eq, w) => f1_tag_eq_vec(k)(w) && f1_cohs(k)(w).isValid() && f1_cacheline_valid(k)(w)})))
  val f1_tag_match         = VecInit(f1_tag_match_vec.map(vector => ParallelOR(vector)))

  val f1_bank_hit          = VecInit(Seq(f1_tag_match(0) && f1_valid  && !tlbExcpPF(0) && !tlbExcpAF(0) && !(release_confic_vec(0) || release_confic_vec(2)),
                                                  f1_tag_match(1) && f1_valid && f1_doubleLine && !tlbExcpPF(1) && !tlbExcpAF(1) && !(release_confic_vec(1) || release_confic_vec(3))    ))
  val f1_bank_miss         = VecInit(Seq(!f1_tag_match(0) && f1_valid && !tlbExcpPF(0) && !tlbExcpAF(0), !f1_tag_match(1) && f1_valid && f1_doubleLine && !tlbExcpPF(1) && !tlbExcpAF(1)))
  val f1_hit               = (f1_bank_hit(0) && f1_bank_hit(1)) || (!f1_doubleLine && f1_bank_hit(0))

  val replacers       = Seq.fill(2)(ReplacementPolicy.fromString(cacheParams.replacer,nWays,nSets/2))
  val f1_victim_oh    = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(f1_vSetIdx(i)))})
  val f1_victim_coh   = VecInit(f1_victim_oh.zipWithIndex.map{case(oh, port) => Mux1H(oh, f1_cohs(port))})
  val f1_victim_tag   = VecInit(f1_victim_oh.zipWithIndex.map{case(oh, port) => Mux1H(oh, f1_tags(port))})
  val f1_victim_data  = VecInit(f1_victim_oh.zipWithIndex.map{case(oh, port) => Mux1H(oh, f1_datas(port))})
  val f1_need_replace = VecInit(f1_victim_coh.zipWithIndex.map{case(coh, port) => coh.isValid() && f1_bank_miss(port)})

  val touch_sets = Seq.fill(2)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))) )

  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}

  val f1_hit_data      =  VecInit(f1_datas.zipWithIndex.map { case(bank, i) =>
    val bank_hit_data = Mux1H(f1_tag_match_vec(i).asUInt, bank)
    bank_hit_data
  })

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_hit_way_" + Integer.toString(w, 10),  f1_fire && f1_bank_hit(0) && OHToUInt(f1_tag_match_vec(0))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_0_victim_way_" + Integer.toString(w, 10),  f1_fire && !f1_bank_hit(0) && OHToUInt(f1_victim_oh(0))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_1_hit_way_" + Integer.toString(w, 10),  f1_fire && f1_doubleLine && f1_bank_hit(1) && OHToUInt(f1_tag_match_vec(1))  === w.U)
  }

  (0 until nWays).map{ w =>
    XSPerfAccumulate("line_1_victim_way_" + Integer.toString(w, 10),  f1_fire && f1_doubleLine && !f1_bank_hit(1) && OHToUInt(f1_victim_oh(1))  === w.U)
  }

  XSPerfAccumulate("ifu_bubble_f1_tlb_miss",    f1_valid && !tlbRespAllValid )

  //---------------------------------------------
  //  Fetch Stage 3 :
  //  * get data from last stage (hit from f1_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val f2_fetchFinish = Wire(Bool())

  val f2_valid        = RegInit(false.B)
  val f2_ftq_req      = RegEnable(next = f1_ftq_req,    enable = f1_fire)
  val f2_situation    = RegEnable(next = f1_situation,  enable=f1_fire)
  val f2_doubleLine   = RegEnable(next = f1_doubleLine, enable=f1_fire)
  val f2_fire         = f2_valid && f2_fetchFinish && f3_ready

  f2_ready := (f3_ready && f2_fetchFinish) || !f2_valid

  when(f2_flush)                  {f2_valid := false.B}
  .elsewhen(f1_fire && !f1_flush) {f2_valid := true.B }
  .elsewhen(f2_fire)              {f2_valid := false.B}

  val sec_miss_reg   = RegInit(0.U.asTypeOf(Vec(4, Bool())))
  val has_fixed_0_miss = sec_miss_reg(0) || sec_miss_reg(2)
  val has_fixed_1_miss = sec_miss_reg(1) || sec_miss_reg(3)
  val has_fixed_vec = VecInit(Seq(has_fixed_0_miss,has_fixed_1_miss ))

  val (f2_pAddrs , f2_vaddr)   = (RegEnable(next = f1_pAddrs, enable = f1_fire), VecInit(Seq(f2_ftq_req.startAddr, f2_ftq_req.fallThruAddr)))
  val f2_cohs      = RegEnable(next = f1_cohs, enable = f1_fire)
  val f2_hit      = RegEnable(next = f1_hit   , enable = f1_fire)
  val f2_bank_hit = RegEnable(next = f1_bank_hit, enable = f1_fire)
  val f2_fixed_hit_vec = VecInit((0 until 2).map(i => f2_bank_hit(i) || has_fixed_vec(i)))
  val f2_fixed_hit = (f2_valid && f2_fixed_hit_vec(0) && f2_fixed_hit_vec(1) && f2_doubleLine) || (f2_valid && f2_fixed_hit_vec(0) && !f2_doubleLine)
  val f2_miss     = f2_valid && !f2_fixed_hit 
  val (f2_vSetIdx, f2_pTags) = (RegEnable(next = f1_vSetIdx, enable = f1_fire), RegEnable(next = f1_pTags, enable = f1_fire))

  //replacement
  val f2_waymask      = RegEnable(next = f1_victim_oh, enable = f1_fire)
  val f2_victim_coh   = RegEnable(next = f1_victim_coh, enable = f1_fire)
  val f2_victim_tag   = RegEnable(next = f1_victim_tag, enable = f1_fire)
  val f2_victim_data  = RegEnable(next = f1_victim_data,  enable = f1_fire)
  val f2_need_replace = RegEnable(next = f1_need_replace,  enable = f1_fire)
  val f2_has_replace  = f2_need_replace.asUInt.orR

  val release_idle :: release_ready :: release_send_req :: Nil = Enum(3)
  val release_state = RegInit(release_idle)

  switch(release_state){
    is(release_idle){
      when(f2_need_replace(0) && !f2_need_replace(1)){
        release_state := Mux(toRealseUnit(0).ready, release_ready ,release_idle )
      }.elsewhen(!f2_need_replace(0) && f2_need_replace(1)){
        release_state := Mux(toRealseUnit(1).ready, release_ready ,release_idle )
      }.elsewhen(f2_need_replace(0) && f2_need_replace(1)){
        release_state := Mux(toRealseUnit(0).ready && toRealseUnit(1).ready, release_ready ,release_idle )
      }
    }

    is(release_ready){
      release_state := release_send_req
    }

    is(release_send_req){
      when(f2_fire){ release_state := release_idle}
    }
  }

  (0 until 2).map{ i =>
    toRealseUnit(i).valid          := f2_valid && f2_need_replace(i) && (release_state === release_ready) && !f2_flush


    toRealseUnit(i).bits.addr      := get_block_addr(Cat(f2_victim_tag(i), get_untag(f2_vaddr(i))) )
    toRealseUnit(i).bits.param     := f2_victim_coh(i).onCacheControl(M_FLUSH)._2
    toRealseUnit(i).bits.voluntary := true.B  //TODO: deal witch Probe
    toRealseUnit(i).bits.hasData   := false.B //TODO: Probe has Data
    toRealseUnit(i).bits.data      := f2_victim_data(i)
  }

  //deal wich realse confilict
  val release_0_req_0  =  f2_valid && f1_valid && (f2_vSetIdx(0) === f1_vSetIdx(0)) && (f2_victim_tag(0) === get_phy_tag(f1_pAddrs(0))) && f2_need_replace(0)
  val release_0_req_1  =  f2_valid && f1_valid && (f2_vSetIdx(0) === f1_vSetIdx(1)) && (f2_victim_tag(0) === get_phy_tag(f1_pAddrs(1))) && f2_need_replace(0)
  val release_1_req_0  =  f2_valid && f1_valid && (f2_vSetIdx(1) === f1_vSetIdx(0)) && (f2_victim_tag(1) === get_phy_tag(f1_pAddrs(0))) && f2_need_replace(1)
  val release_1_req_1  =  f2_valid && f1_valid && (f2_vSetIdx(1) === f1_vSetIdx(1)) && (f2_victim_tag(1) === get_phy_tag(f1_pAddrs(1))) && f2_need_replace(1)

  release_confic_vec := VecInit(Seq(release_0_req_0,release_0_req_1,release_1_req_0,release_1_req_1))

  //exception information
  val f2_except_pf = RegEnable(next = VecInit(tlbExcpPF), enable = f1_fire)
  val f2_except_af = RegEnable(next = VecInit(tlbExcpAF), enable = f1_fire)
  val f2_except    = VecInit((0 until 2).map{i => f2_except_pf(i) || f2_except_af(i)})
  val f2_has_except = f2_valid && (f2_except_af.reduce(_||_) || f2_except_pf.reduce(_||_))

  //instruction
  val wait_idle :: wait_queue_ready :: wait_send_req  :: wait_two_resp :: wait_0_resp :: wait_1_resp :: wait_one_resp ::wait_finish :: Nil = Enum(8)
  val wait_state = RegInit(wait_idle)

  val (miss0_resp, miss1_resp) = (fromMissQueue(0).fire(), fromMissQueue(1).fire())
  val (bank0_fix, bank1_fix)   = (miss0_resp  && !f2_bank_hit(0), miss1_resp && f2_doubleLine && !f2_bank_hit(1))

  val  only_0_miss = f2_valid && !f2_hit && !f2_doubleLine && !f2_has_except && !has_fixed_0_miss
  val  only_0_hit  = f2_valid && f2_hit && !f2_doubleLine
  val  hit_0_hit_1  = f2_valid && f2_hit && f2_doubleLine
  val (hit_0_miss_1 ,  miss_0_hit_1,  miss_0_miss_1) = (  (f2_valid && !f2_bank_hit(1) && !has_fixed_1_miss && (f2_bank_hit(0) || has_fixed_0_miss) && f2_doubleLine  && !f2_has_except),
                                                          (f2_valid && !f2_bank_hit(0) && !has_fixed_0_miss && (f2_bank_hit(1) || has_fixed_1_miss) && f2_doubleLine  && !f2_has_except),
                                                          (f2_valid && !f2_bank_hit(0) && !f2_bank_hit(1) && !has_fixed_0_miss && !has_fixed_1_miss && f2_doubleLine && !f2_has_except),
                                                       )

  val  hit_0_except_1  = f2_valid && f2_doubleLine &&  !f2_except(0) && f2_except(1)  &&  f2_bank_hit(0)
  val  miss_0_except_1 = f2_valid && f2_doubleLine &&  !f2_except(0) && f2_except(1)  && !f2_bank_hit(0)
  //val  fetch0_except_1 = hit_0_except_1 || miss_0_except_1
  val  except_0        = f2_valid && f2_except(0)

  val f2_mq_datas     = Reg(Vec(2, UInt(blockBits.W)))

  when(fromMissQueue(0).fire) {f2_mq_datas(0) :=  fromMissQueue(0).bits.data}
  when(fromMissQueue(1).fire) {f2_mq_datas(1) :=  fromMissQueue(1).bits.data}

  switch(wait_state){
    is(wait_idle){
      when(miss_0_except_1){
        wait_state :=  Mux(toMissQueue(0).ready, wait_queue_ready ,wait_idle )
      }.elsewhen( only_0_miss  || miss_0_hit_1){
        wait_state :=  Mux(toMissQueue(0).ready, wait_queue_ready ,wait_idle )
      }.elsewhen(hit_0_miss_1){
        wait_state :=  Mux(toMissQueue(1).ready, wait_queue_ready ,wait_idle )
      }.elsewhen( miss_0_miss_1 ){
        wait_state := Mux(toMissQueue(0).ready && toMissQueue(1).ready, wait_queue_ready ,wait_idle)
      }
    }

    //TODO: naive logic for wait icache response
    is(wait_queue_ready){
      wait_state := wait_send_req
    }

    is(wait_send_req) {
      when(miss_0_except_1 || only_0_miss || hit_0_miss_1 || miss_0_hit_1){
        wait_state :=  wait_one_resp
      }.elsewhen( miss_0_miss_1 ){
        wait_state := wait_two_resp
      }
    }

    is(wait_one_resp) {
      when( (miss_0_except_1 ||only_0_miss || miss_0_hit_1) && fromMissQueue(0).fire()){
        wait_state := wait_finish
      }.elsewhen( hit_0_miss_1 && fromMissQueue(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_two_resp) {
      when(fromMissQueue(0).fire() && fromMissQueue(1).fire()){
        wait_state := wait_finish
      }.elsewhen( !fromMissQueue(0).fire() && fromMissQueue(1).fire() ){
        wait_state := wait_0_resp
      }.elsewhen(fromMissQueue(0).fire() && !fromMissQueue(1).fire()){
        wait_state := wait_1_resp
      }
    }

    is(wait_0_resp) {
      when(fromMissQueue(0).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_1_resp) {
      when(fromMissQueue(1).fire()){
        wait_state := wait_finish
      }
    }

    is(wait_finish) {
      when(f2_fire) {wait_state := wait_idle }
    }
  }

  when(f2_flush) {
    wait_state := wait_idle
    release_state := release_idle
  }

  (0 until 2).map { i =>
    if(i == 1) toMissQueue(i).valid := (hit_0_miss_1 || miss_0_miss_1) && wait_state === wait_queue_ready
      else     toMissQueue(i).valid := (only_0_miss || miss_0_hit_1 || miss_0_miss_1) && wait_state === wait_queue_ready
    toMissQueue(i).bits.paddr    := f2_pAddrs(i)
    toMissQueue(i).bits.vaddr    := f2_vaddr(i)
    toMissQueue(i).bits.waymask  := f2_waymask(i)
    toMissQueue(i).bits.coh      := f2_victim_coh(i)
  }

  val miss_all_fix       = (wait_state === wait_finish) && (!f2_has_replace || (release_state === release_send_req))
  f2_fetchFinish         := ((f2_valid && f2_fixed_hit) || miss_all_fix || hit_0_except_1 || except_0)

  XSPerfAccumulate("ifu_bubble_f2_miss",    f2_valid && !f2_fetchFinish )

  (touch_ways zip touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    t_s(0)         := f1_vSetIdx(i)
    t_w(0).valid   := f1_bank_hit(i)
    t_w(0).bits    := OHToUInt(f1_tag_match_vec(i))

    t_s(1)         := f2_vSetIdx(i)
    t_w(1).valid   := f2_valid && !f2_bank_hit(i)
    t_w(1).bits    := OHToUInt(f2_waymask(i))
  }
  
  //val sec_miss_reg   = RegInit(0.U.asTypeOf(Vec(4, Bool())))
  val reservedRefillData = Reg(Vec(2, UInt(blockBits.W)))
  val f2_hit_datas    = RegEnable(next = f1_hit_data, enable = f1_fire)
  val f2_datas        = Wire(Vec(2, UInt(blockBits.W)))

  f2_datas.zipWithIndex.map{case(bank,i) =>
    if(i == 0) bank := Mux(f2_bank_hit(i), f2_hit_datas(i),Mux(sec_miss_reg(2),reservedRefillData(1),Mux(sec_miss_reg(0),reservedRefillData(0), f2_mq_datas(i))))
    else bank := Mux(f2_bank_hit(i), f2_hit_datas(i),Mux(sec_miss_reg(3),reservedRefillData(1),Mux(sec_miss_reg(1),reservedRefillData(0), f2_mq_datas(i))))
  }

  val f2_jump_valids          = Fill(PredictWidth, !preDecoderOut.cfiOffset.valid)   | Fill(PredictWidth, 1.U(1.W)) >> (~preDecoderOut.cfiOffset.bits)
  val f2_predecode_valids     = VecInit(preDecoderOut.pd.map(instr => instr.valid)).asUInt & f2_jump_valids

  def cut(cacheline: UInt, start: UInt) : Vec[UInt] ={
    if(HasCExtension){
      val result   = Wire(Vec(PredictWidth + 1, UInt(16.W)))
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 2, UInt(16.W)))
      val startPtr = Cat(0.U(1.W), start(blockOffBits-1, 1))
      (0 until PredictWidth + 1).foreach( i =>
        result(i) := dataVec(startPtr + i.U)
      )
      result
    } else {
      val result   = Wire(Vec(PredictWidth, UInt(32.W)) )
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 4, UInt(32.W)))
      val startPtr = Cat(0.U(1.W), start(blockOffBits-1, 2))
      (0 until PredictWidth).foreach( i =>
        result(i) := dataVec(startPtr + i.U)
      )
      result
    }
  }

  val f2_cut_data = cut( Cat(f2_datas.map(cacheline => cacheline.asUInt ).reverse).asUInt, f2_ftq_req.startAddr )

  // deal with secondary miss in f1
  val f2_0_f1_0 =   ((f2_valid && !f2_bank_hit(0)) && f1_valid && (get_block_addr(f2_ftq_req.startAddr) === get_block_addr(f1_ftq_req.startAddr)))
  val f2_0_f1_1 =   ((f2_valid && !f2_bank_hit(0)) && f1_valid && f1_doubleLine && (get_block_addr(f2_ftq_req.startAddr) === get_block_addr(f1_ftq_req.startAddr + blockBytes.U)))
  val f2_1_f1_0 =   ((f2_valid && !f2_bank_hit(1) && f2_doubleLine) && f1_valid && (get_block_addr(f2_ftq_req.startAddr+ blockBytes.U) === get_block_addr(f1_ftq_req.startAddr) ))
  val f2_1_f1_1 =   ((f2_valid && !f2_bank_hit(1) && f2_doubleLine) && f1_valid && f1_doubleLine && (get_block_addr(f2_ftq_req.startAddr+ blockBytes.U) === get_block_addr(f1_ftq_req.startAddr + blockBytes.U) ))

  val isSameLine = f2_0_f1_0 || f2_0_f1_1 || f2_1_f1_0 || f2_1_f1_1
  val sec_miss_sit   = VecInit(Seq(f2_0_f1_0, f2_0_f1_1, f2_1_f1_0, f2_1_f1_1))
  val hasSecMiss     = RegInit(false.B)

  when(f2_flush){
    sec_miss_reg.map(sig => sig := false.B)
    hasSecMiss := false.B
  }.elsewhen(isSameLine && !f1_flush && f2_fire){
    sec_miss_reg.zipWithIndex.map{case(sig, i) => sig := sec_miss_sit(i)}
    hasSecMiss := true.B
  }.elsewhen((!isSameLine || f1_flush) && hasSecMiss && f2_fire){
    sec_miss_reg.map(sig => sig := false.B)
    hasSecMiss := false.B
  }

  when((f2_0_f1_0 || f2_0_f1_1) && f2_fire){
    reservedRefillData(0) := f2_mq_datas(0)
  }

  when((f2_1_f1_0 || f2_1_f1_1) && f2_fire){
    reservedRefillData(1) := f2_mq_datas(1)
  }


  //---------------------------------------------
  //  Fetch Stage 4 :
  //  * get data from last stage (hit from f1_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val f3_valid          = RegInit(false.B)
  val f3_ftq_req        = RegEnable(next = f2_ftq_req,    enable=f2_fire)
  val f3_situation      = RegEnable(next = f2_situation,  enable=f2_fire)
  val f3_doubleLine     = RegEnable(next = f2_doubleLine, enable=f2_fire)
  val f3_fire           = io.toIbuffer.fire()

  when(f3_flush)                  {f3_valid := false.B}
  .elsewhen(f2_fire && !f2_flush) {f3_valid := true.B }
  .elsewhen(io.toIbuffer.fire())  {f3_valid := false.B}

  f3_ready := io.toIbuffer.ready || !f2_valid

  val f3_cut_data       = RegEnable(next = f2_cut_data, enable=f2_fire)
  val f3_except_pf      = RegEnable(next = f2_except_pf, enable = f2_fire)
  val f3_except_af      = RegEnable(next = f2_except_af, enable = f2_fire)
  val f3_hit            = RegEnable(next = f2_hit   , enable = f2_fire)

  val f3_lastHalf       = RegInit(0.U.asTypeOf(new LastHalfInfo))
  val f3_lastHalfMatch  = f3_lastHalf.matchThisBlock(f3_ftq_req.startAddr)
  val f3_except         = VecInit((0 until 2).map{i => f3_except_pf(i) || f3_except_af(i)})
  val f3_has_except     = f3_valid && (f3_except_af.reduce(_||_) || f3_except_pf.reduce(_||_))

  //performance counter
  val f3_only_0_hit     = RegEnable(next = only_0_hit, enable = f2_fire)
  val f3_only_0_miss    = RegEnable(next = only_0_miss, enable = f2_fire)
  val f3_hit_0_hit_1    = RegEnable(next = hit_0_hit_1, enable = f2_fire)
  val f3_hit_0_miss_1   = RegEnable(next = hit_0_miss_1, enable = f2_fire)
  val f3_miss_0_hit_1   = RegEnable(next = miss_0_hit_1, enable = f2_fire)
  val f3_miss_0_miss_1  = RegEnable(next = miss_0_miss_1, enable = f2_fire)

  val f3_bank_hit = RegEnable(next = f2_bank_hit, enable = f2_fire)
  val f3_req_0 = io.toIbuffer.fire()
  val f3_req_1 = io.toIbuffer.fire() && f3_doubleLine
  val f3_hit_0 = io.toIbuffer.fire() & f3_bank_hit(0)
  val f3_hit_1 = io.toIbuffer.fire() && f3_doubleLine & f3_bank_hit(1)


  preDecoderIn.instValid     :=  f3_valid && !f3_has_except
  preDecoderIn.data          :=  f3_cut_data
  preDecoderIn.startAddr     :=  f3_ftq_req.startAddr
  preDecoderIn.fallThruAddr  :=  f3_ftq_req.fallThruAddr
  preDecoderIn.fallThruError :=  f3_ftq_req.fallThruError
  preDecoderIn.isDoubleLine  :=  f3_doubleLine
  preDecoderIn.ftqOffset     :=  f3_ftq_req.ftqOffset
  preDecoderIn.target        :=  f3_ftq_req.target
  preDecoderIn.oversize      :=  f3_ftq_req.oversize
  preDecoderIn.lastHalfMatch :=  f3_lastHalfMatch
  preDecoderIn.pageFault     :=  f3_except_pf
  preDecoderIn.accessFault   :=  f3_except_af


  // TODO: What if next packet does not match?
  when (f3_flush) {
    f3_lastHalf.valid := false.B
  }.elsewhen (io.toIbuffer.fire()) {
    f3_lastHalf.valid := preDecoderOut.hasLastHalf
    f3_lastHalf.middlePC := preDecoderOut.realEndPC
  }

  val f3_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt

  io.toIbuffer.valid          := f3_valid
  io.toIbuffer.bits.instrs    := preDecoderOut.instrs
  io.toIbuffer.bits.valid     := f3_predecode_range & preDecoderOut.instrRange.asUInt
  io.toIbuffer.bits.pd        := preDecoderOut.pd
  io.toIbuffer.bits.ftqPtr    := f3_ftq_req.ftqIdx
  io.toIbuffer.bits.pc        := preDecoderOut.pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := preDecoderOut.takens(i)}
  io.toIbuffer.bits.foldpc    := preDecoderOut.pc.map(i => XORFold(i(VAddrBits-1,1), MemPredPCWidth))
  io.toIbuffer.bits.ipf       := preDecoderOut.pageFault
  io.toIbuffer.bits.acf       := preDecoderOut.accessFault
  io.toIbuffer.bits.crossPageIPFFix := preDecoderOut.crossPageIPF

  //Write back to Ftq
  val finishFetchMaskReg = RegNext(f3_valid && !(f2_fire && !f2_flush))

  toFtq.pdWb.valid           := !finishFetchMaskReg && f3_valid
  toFtq.pdWb.bits.pc         := preDecoderOut.pc
  toFtq.pdWb.bits.pd         := preDecoderOut.pd
  toFtq.pdWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid :=  f3_predecode_range(i)}
  toFtq.pdWb.bits.ftqIdx     := f3_ftq_req.ftqIdx
  toFtq.pdWb.bits.ftqOffset  := f3_ftq_req.ftqOffset.bits
  toFtq.pdWb.bits.misOffset  := preDecoderOut.misOffset
  toFtq.pdWb.bits.cfiOffset  := preDecoderOut.cfiOffset
  toFtq.pdWb.bits.target     := preDecoderOut.target
  toFtq.pdWb.bits.jalTarget  := preDecoderOut.jalTarget
  toFtq.pdWb.bits.instrRange := preDecoderOut.instrRange

  val predecodeFlush     = preDecoderOut.misOffset.valid && f3_valid
  val predecodeFlushReg  = RegNext(predecodeFlush && !(f2_fire && !f2_flush))


  f3_redirect := !predecodeFlushReg && predecodeFlush

  XSPerfAccumulate("ifu_req",   io.toIbuffer.fire() )
  XSPerfAccumulate("ifu_miss",  io.toIbuffer.fire() && !f3_hit )
  XSPerfAccumulate("ifu_req_cacheline_0", f3_req_0  )
  XSPerfAccumulate("ifu_req_cacheline_1", f3_req_1  )
  XSPerfAccumulate("ifu_req_cacheline_0_hit",   f3_hit_0 )
  XSPerfAccumulate("ifu_req_cacheline_1_hit",   f3_hit_1 )
  XSPerfAccumulate("frontendFlush",  f3_redirect )
  XSPerfAccumulate("only_0_hit",      f3_only_0_hit   && io.toIbuffer.fire()  )
  XSPerfAccumulate("only_0_miss",     f3_only_0_miss  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_hit_1",     f3_hit_0_hit_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_miss_1",    f3_hit_0_miss_1 && io.toIbuffer.fire()  )
  XSPerfAccumulate("miss_0_hit_1",    f3_miss_0_hit_1  && io.toIbuffer.fire() )
  XSPerfAccumulate("miss_0_miss_1",   f3_miss_0_miss_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("cross_line_block", io.toIbuffer.fire() && f3_situation(0) )
  XSPerfAccumulate("fall_through_is_cacheline_end", io.toIbuffer.fire() && f3_situation(1) )
}
