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
import chisel3.experimental.verification
import utils._

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth /8
  def mmioBeats = FetchWidth * 4 * 8 / mmioBusWidth
  def mmioMask  = VecInit(List.fill(PredictWidth)(true.B)).asUInt
  def mmioBusAligned(pc :UInt): UInt = align(pc, mmioBusBytes)
}

trait HasIFUConst extends HasXSParameter {
  def align(pc: UInt, bytes: Int): UInt = Cat(pc(VAddrBits-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  // def groupAligned(pc: UInt)  = align(pc, groupBytes)
  // def packetAligned(pc: UInt) = align(pc, packetBytes)
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
  val toMissQueue   = Vec(2,Decoupled(new ICacheMissReq))
  val fromIMeta     = Input(new ICacheMetaRespBundle)
  val fromIData     = Input(new ICacheDataRespBundle)
  val fromMissQueue = Vec(2,Flipped(Decoupled(new ICacheMissResp)))
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
  toITLB(0).bits.vaddr    := align(f1_ftq_req.startAddr, blockBytes)
  toITLB(0).bits.debug.pc := align(f1_ftq_req.startAddr, blockBytes)
  
  toITLB(1).valid         := f1_valid && f1_doubleLine
  toITLB(1).bits.vaddr    := align(f1_ftq_req.fallThruAddr, blockBytes)
  toITLB(1).bits.debug.pc := align(f1_ftq_req.fallThruAddr, blockBytes)

  toITLB.map{port =>
    port.bits.cmd                 := TlbCmd.exec
    port.bits.roqIdx              := DontCare
    port.bits.debug.isFirstIssue  := DontCare
  }

  fromITLB.map(_.ready := true.B)

  val (tlbRespValid, tlbRespPAddr) = (fromITLB.map(_.valid), VecInit(fromITLB.map(_.bits.paddr)))
  val (tlbRespMiss,  tlbRespMMIO)  = (fromITLB.map(port => port.bits.miss && port.valid), fromITLB.map(port => port.bits.mmio && port.valid))
  val (tlbExcpPF,    tlbExcpAF)    = (fromITLB.map(port => port.bits.excp.pf.instr && port.valid), fromITLB.map(port => port.bits.excp.af.instr && port.valid))

  tlbRespAllValid := tlbRespValid(0)  && (tlbRespValid(1) || !f1_doubleLine)

  val f1_pAddrs             = tlbRespPAddr   //TODO: Temporary assignment
  val f1_pTags              = VecInit(f1_pAddrs.map(get_tag(_)))
  val (f1_tags, f1_cacheline_valid, f1_datas)   = (meta_resp.tags, meta_resp.valid, data_resp.datas)
  val bank0_hit_vec         = VecInit(f1_tags(0).zipWithIndex.map{ case(way_tag,i) => f1_cacheline_valid(0)(i) && way_tag ===  f1_pTags(0) })
  val bank1_hit_vec         = VecInit(f1_tags(1).zipWithIndex.map{ case(way_tag,i) => f1_cacheline_valid(1)(i) && way_tag ===  f1_pTags(1) })
  val (bank0_hit,bank1_hit) = (ParallelOR(bank0_hit_vec) && !tlbExcpPF(0) && !tlbExcpAF(0), ParallelOR(bank1_hit_vec) && !tlbExcpPF(1) && !tlbExcpAF(1)) 
  val f1_hit                = (bank0_hit && bank1_hit && f1_valid && f1_doubleLine) || (f1_valid && !f1_doubleLine && bank0_hit)  
  val f1_bank_hit_vec       = VecInit(Seq(bank0_hit_vec, bank1_hit_vec))
  val f1_bank_hit           = VecInit(Seq(bank0_hit, bank1_hit))

  val replacers       = Seq.fill(2)(ReplacementPolicy.fromString(Some("random"),nWays,nSets/2))
  val f1_victim_masks = VecInit(replacers.zipWithIndex.map{case (replacer, i) => UIntToOH(replacer.way(f1_vSetIdx(i)))})

  val touch_sets = Seq.fill(2)(Wire(Vec(2, UInt(log2Ceil(nSets/2).W))))
  val touch_ways = Seq.fill(2)(Wire(Vec(2, Valid(UInt(log2Ceil(nWays).W)))) )
   
  ((replacers zip touch_sets) zip touch_ways).map{case ((r, s),w) => r.access(s,w)}
  
  val f1_hit_data      =  VecInit(f1_datas.zipWithIndex.map { case(bank, i) =>
    val bank_hit_data = Mux1H(f1_bank_hit_vec(i).asUInt, bank)
    bank_hit_data
  })


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


  val f2_pAddrs   = RegEnable(next = f1_pAddrs, enable = f1_fire)
  val f2_hit      = RegEnable(next = f1_hit   , enable = f1_fire)
  val f2_bank_hit = RegEnable(next = f1_bank_hit, enable = f1_fire)
  val f2_miss     = f2_valid && !f2_hit 
  val (f2_vSetIdx, f2_pTags) = (RegEnable(next = f1_vSetIdx, enable = f1_fire), RegEnable(next = f1_pTags, enable = f1_fire))
  val f2_waymask  = RegEnable(next = f1_victim_masks, enable = f1_fire)
  //exception information
  val f2_except_pf = RegEnable(next = VecInit(tlbExcpPF), enable = f1_fire)
  val f2_except_af = RegEnable(next = VecInit(tlbExcpAF), enable = f1_fire)
  val f2_except    = VecInit((0 until 2).map{i => f2_except_pf(i) || f2_except_af(i)})
  val f2_has_except = f2_valid && (f2_except_af.reduce(_||_) || f2_except_pf.reduce(_||_))

  //instruction 
  val wait_idle :: wait_queue_ready :: wait_send_req  :: wait_two_resp :: wait_0_resp :: wait_1_resp :: wait_one_resp ::wait_finish :: Nil = Enum(8)
  val wait_state = RegInit(wait_idle)

  fromMissQueue.map{port => port.ready := true.B}

  val (miss0_resp, miss1_resp) = (fromMissQueue(0).fire(), fromMissQueue(1).fire())
  val (bank0_fix, bank1_fix)   = (miss0_resp  && !f2_bank_hit(0), miss1_resp && f2_doubleLine && !f2_bank_hit(1))

  val  only_0_miss = f2_valid && !f2_hit && !f2_doubleLine && !f2_has_except
  val (hit_0_miss_1 ,  miss_0_hit_1,  miss_0_miss_1) = (  (f2_valid && !f2_bank_hit(1) && f2_bank_hit(0) && f2_doubleLine  && !f2_has_except),
                                                          (f2_valid && !f2_bank_hit(0) && f2_bank_hit(1) && f2_doubleLine  && !f2_has_except),
                                                          (f2_valid && !f2_bank_hit(0) && !f2_bank_hit(1) && f2_doubleLine && !f2_has_except),
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

  when(f2_flush) { wait_state := wait_idle }

  (0 until 2).map { i =>
    if(i == 1) toMissQueue(i).valid := (hit_0_miss_1 || miss_0_miss_1) && wait_state === wait_queue_ready
      else     toMissQueue(i).valid := (only_0_miss || miss_0_hit_1 || miss_0_miss_1) && wait_state === wait_queue_ready
    toMissQueue(i).bits.addr    := f2_pAddrs(i)
    toMissQueue(i).bits.vSetIdx := f2_vSetIdx(i)
    toMissQueue(i).bits.waymask := f2_waymask(i)
    toMissQueue(i).bits.clientID :=0.U
  }

  val miss_all_fix       = (wait_state === wait_finish)
  
  f2_fetchFinish         := ((f2_valid && f2_hit) || miss_all_fix || hit_0_except_1 || except_0)


  (touch_ways zip touch_sets).zipWithIndex.map{ case((t_w,t_s), i) =>
    t_s(0)         := f1_vSetIdx(i)
    t_w(0).valid   := f1_bank_hit(i)
    t_w(0).bits    := OHToUInt(f1_bank_hit_vec(i))

    t_s(1)         := f2_vSetIdx(i)
    t_w(1).valid   := f2_valid && !f2_bank_hit(i)
    t_w(1).bits    := OHToUInt(f2_waymask(i))
  }
  
  val sec_miss_reg   = RegInit(0.U.asTypeOf(Vec(4, Bool())))
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

  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    // Performance Counter
    XSPerfAccumulate("req",   io.toIbuffer.fire() )
    XSPerfAccumulate("miss",  io.toIbuffer.fire() && !f3_hit )
    XSPerfAccumulate("frontendFlush",  f3_redirect )
    XSPerfAccumulate("only_0_miss",   only_0_miss )
    XSPerfAccumulate("hit_0_miss_1",  hit_0_miss_1 )
    XSPerfAccumulate("miss_0_hit_1",  miss_0_hit_1 )
    XSPerfAccumulate("miss_0_miss_1", miss_0_miss_1 )
    XSPerfAccumulate("crossLine", io.toIbuffer.fire() && f3_situation(0) )
    XSPerfAccumulate("lastInLin", io.toIbuffer.fire() && f3_situation(1) )
  }
}
