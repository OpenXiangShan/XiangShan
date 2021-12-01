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
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._
import utils._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth / 8
  def maxInstrLen = 32
}

trait HasIFUConst extends HasXSParameter {
  def addrAlign(addr: UInt, bytes: Int, highest: Int): UInt = Cat(addr(highest-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  def fetchQueueSize = 2
}

class IfuPtr(implicit p: Parameters) extends CircularQueuePtr[IfuPtr](entries = 2){
  override def cloneType = (new IfuPtr).asInstanceOf[this.type]
}

object IfuPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): IfuPtr = {
    val ptr = Wire(new IfuPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: IfuPtr)(implicit p: Parameters): IfuPtr = {
    apply(!ptr.flag, ptr.value)
  }
}

class IfuToFtqIO(implicit p:Parameters) extends XSBundle {
  val pdWb = Valid(new PredecodeWritebackBundle)
}

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val fromFtq = Flipped(new FtqToIfuIO)
  val toFtq   = new IfuToFtqIO
}

class UncacheInterface(implicit p: Parameters) extends XSBundle {
  val fromUncache = Flipped(DecoupledIO(new InsUncacheResp))
  val toUncache   = DecoupledIO( new InsUncacheReq )
}
class NewIFUIO(implicit p: Parameters) extends XSBundle {
  val ftqInter        = new FtqInterface
  val icacheInter     = Vec(2, Flipped(new ICacheMainPipeBundle))
  val icacheStop      = Output(Bool())
  val icachePerfInfo  = Input(new ICachePerfInfo)
  val toIbuffer       = Decoupled(new FetchToIBuffer)
  val uncacheInter   =  new UncacheInterface
  val frontendTrigger = Flipped(new FrontendTdataDistributeIO)
  val csrTriggerEnable = Input(Vec(4, Bool()))
  val rob_commits = Flipped(Vec(CommitWidth, Valid(new RobCommitInfo)))
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
  val mmio = Bool()
  val frontendTrigger = new FrontendTdataDistributeIO
  val csrTriggerEnable = Vec(4, Bool())
}

class NewIFU(implicit p: Parameters) extends XSModule with HasICacheParameters with HasIFUConst
with HasCircularQueuePtrHelper
{
  println(s"icache ways: ${nWays} sets:${nSets}")
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toICache, fromICache) = (VecInit(io.icacheInter.map(_.req)), VecInit(io.icacheInter.map(_.resp)))
  val (toUncache, fromUncache) = (io.uncacheInter.toUncache , io.uncacheInter.fromUncache)

  def isCrossLineReq(start: UInt, end: UInt): Bool = start(blockOffBits) ^ end(blockOffBits)

  def isLastInCacheline(fallThruAddr: UInt): Bool = fallThruAddr(blockOffBits - 1, 1) === 0.U

  class TlbExept(implicit p: Parameters) extends XSBundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }


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

  fromFtq.req.ready := toICache(0).ready && toICache(1).ready && f2_ready && GTimer() > 500.U

  toICache(0).valid       := fromFtq.req.fire() && !f0_flush
  toICache(0).bits.vaddr  := fromFtq.req.bits.startAddr
  toICache(1).valid       := fromFtq.req.fire() && f0_doubleLine && !f0_flush
  toICache(1).bits.vaddr  := fromFtq.req.bits.fallThruAddr

  
  //---------------------------------------------
  //  Fetch Stage 1 :
  //  * Send req to ITLB and TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------

  val f1_valid      = RegInit(false.B)
  val f1_ftq_req    = RegEnable(next = f0_ftq_req,    enable=f0_fire)
  val f1_situation  = RegEnable(next = f0_situation,  enable=f0_fire)
  val f1_doubleLine = RegEnable(next = f0_doubleLine, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,    enable=f0_fire)
  val f1_fire       = f1_valid && f1_ready

  f1_ready := f2_ready || !f1_valid

  from_bpu_f1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx)

  when(f1_flush)                  {f1_valid  := false.B}
  .elsewhen(f0_fire && !f0_flush) {f1_valid  := true.B}
  .elsewhen(f1_fire)              {f1_valid  := false.B}
  //---------------------------------------------
  //  Fetch Stage 2 :
  //  * Send req to ITLB and TLB Response (Get Paddr)
  //  * ICache Response (Get Meta and Data)
  //  * Hit Check (Generate hit signal and hit vector)
  //  * Get victim way
  //---------------------------------------------
  val icacheRespAllValid = WireInit(false.B)

  val f2_valid      = RegInit(false.B)
  val f2_ftq_req    = RegEnable(next = f1_ftq_req,    enable=f1_fire)
  val f2_situation  = RegEnable(next = f1_situation,  enable=f1_fire)
  val f2_doubleLine = RegEnable(next = f1_doubleLine, enable=f1_fire)
  val f2_vSetIdx    = RegEnable(next = f1_vSetIdx,    enable=f1_fire)
  val f2_fire       = f2_valid && f2_ready

  f2_ready := f3_ready && icacheRespAllValid || !f2_valid
  //TODO: addr compare may be timing critical
  val f2_icache_all_resp_wire       =  fromICache(0).valid && (fromICache(0).bits.vaddr ===  f2_ftq_req.startAddr) && ((fromICache(1).valid && (fromICache(1).bits.vaddr ===  f2_ftq_req.fallThruAddr)) || !f2_doubleLine)
  val f2_icache_all_resp_reg        = RegInit(false.B)

  icacheRespAllValid := f2_icache_all_resp_reg || f2_icache_all_resp_wire

  io.icacheStop := !f3_ready

  when(f2_flush)                                              {f2_icache_all_resp_reg := false.B}
  .elsewhen(f2_valid && f2_icache_all_resp_wire && !f3_ready) {f2_icache_all_resp_reg := true.B}
  .elsewhen(f2_fire && f2_icache_all_resp_reg)                {f2_icache_all_resp_reg := false.B}

  when(f2_flush)                  {f2_valid := false.B}
  .elsewhen(f1_fire && !f1_flush) {f2_valid := true.B }
  .elsewhen(f2_fire)              {f2_valid := false.B}

  val f2_cache_response_data = ResultHoldBypass(valid = f2_icache_all_resp_wire, data = VecInit(fromICache.map(_.bits.readData)))

  val f2_datas        = VecInit((0 until PortNumber).map(i => f2_cache_response_data(i)))
  val f2_except_pf    = VecInit((0 until PortNumber).map(i => fromICache(i).bits.tlbExcp.pageFault))
  val f2_except_af    = VecInit((0 until PortNumber).map(i => fromICache(i).bits.tlbExcp.accessFault))
  val f2_mmio         = fromICache(0).bits.tlbExcp.mmio && !fromICache(0).bits.tlbExcp.accessFault

  val f2_paddrs       = VecInit((0 until PortNumber).map(i => fromICache(i).bits.paddr))
  val f2_perf_info    = io.icachePerfInfo

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

  val preDecoder      = Module(new PreDecode)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)
  val predecodeOutValid = WireInit(false.B)

  val f2_cut_data = cut( Cat(f2_datas.map(cacheline => cacheline.asUInt ).reverse).asUInt, f2_ftq_req.startAddr )

  //---------------------------------------------
  //  Fetch Stage 3 :
  //  * get data from last stage (hit from f2_hit_data/miss from missQueue response)
  //  * if at least one needed cacheline miss, wait for miss queue response (a wait_state machine) THIS IS TOO UGLY!!!
  //  * cut cacheline(s) and send to PreDecode
  //  * check if prediction is right (branch target and type, jump direction and type , jal target )
  //---------------------------------------------
  val f3_valid          = RegInit(false.B)
  val f3_ftq_req        = RegEnable(next = f2_ftq_req,    enable=f2_fire)
  val f3_situation      = RegEnable(next = f2_situation,  enable=f2_fire)
  val f3_doubleLine     = RegEnable(next = f2_doubleLine, enable=f2_fire)
  val f3_fire           = io.toIbuffer.fire()

  f3_ready := io.toIbuffer.ready || !f3_valid

  val f3_cut_data       = RegEnable(next = f2_cut_data, enable=f2_fire)

  val f3_except_pf      = RegEnable(next = f2_except_pf, enable = f2_fire)
  val f3_except_af      = RegEnable(next = f2_except_af, enable = f2_fire)
  val f3_mmio           = RegEnable(next = f2_mmio   , enable = f2_fire)

  val f3_lastHalf       = RegInit(0.U.asTypeOf(new LastHalfInfo))
  val f3_lastHalfMatch  = f3_lastHalf.matchThisBlock(f3_ftq_req.startAddr)
  val f3_except         = VecInit((0 until 2).map{i => f3_except_pf(i) || f3_except_af(i)})
  val f3_has_except     = f3_valid && (f3_except_af.reduce(_||_) || f3_except_pf.reduce(_||_))
  val f3_pAddrs   = RegEnable(next = f2_paddrs, enable = f2_fire)

  val f3_mmio_data    = Reg(UInt(maxInstrLen.W))

  val f3_data = if(HasCExtension) Wire(Vec(PredictWidth + 1, UInt(16.W))) else Wire(Vec(PredictWidth, UInt(32.W)))
  f3_data       :=  f3_cut_data

  val mmio_idle :: mmio_send_req :: mmio_w_resp :: mmio_resend :: mmio_resend_w_resp :: mmio_w_commit :: Nil = Enum(6)
  val mmio_state = RegInit(mmio_idle)

  val f3_req_is_mmio     = f3_mmio && f3_valid
  val mmio_has_commited = VecInit(io.rob_commits.map{commit => commit.valid && commit.bits.ftqIdx === f3_ftq_req.ftqIdx &&  commit.bits.ftqOffset === 0.U}).asUInt.orR
  val f3_mmio_req_commit = f3_req_is_mmio && mmio_state === mmio_w_commit && mmio_has_commited
   
  val f3_mmio_to_commit =  f3_req_is_mmio && mmio_state === mmio_w_commit
  val f3_mmio_to_commit_next = RegNext(f3_mmio_to_commit)
  val f3_mmio_can_go      = f3_mmio_to_commit && !f3_mmio_to_commit_next

  val f3_ftq_flush_self     = fromFtq.redirect.valid && RedirectLevel.flushItself(fromFtq.redirect.bits.level)
  val f3_ftq_flush_by_older = fromFtq.redirect.valid && isBefore(fromFtq.redirect.bits.ftqIdx, f3_ftq_req.ftqIdx)

  val f3_need_not_flush = f3_req_is_mmio && fromFtq.redirect.valid && !f3_ftq_flush_self && !f3_ftq_flush_by_older

  when(f3_flush && !f3_need_not_flush)               {f3_valid := false.B}
  .elsewhen(f2_fire && !f2_flush)                    {f3_valid := true.B }
  .elsewhen(io.toIbuffer.fire() && !f3_req_is_mmio)  {f3_valid := false.B}
  .elsewhen{f3_req_is_mmio && f3_mmio_req_commit}    {f3_valid := false.B}

  val f3_mmio_use_seq_pc = RegInit(false.B)

  val (redirect_ftqIdx, redirect_ftqOffset)  = (fromFtq.redirect.bits.ftqIdx,fromFtq.redirect.bits.ftqOffset)
  val redirect_mmio_req = fromFtq.redirect.valid && redirect_ftqIdx === f3_ftq_req.ftqIdx && redirect_ftqOffset === 0.U

  when(RegNext(f2_fire && !f2_flush) && f3_req_is_mmio)        { f3_mmio_use_seq_pc := true.B  }
  .elsewhen(redirect_mmio_req)                                 { f3_mmio_use_seq_pc := false.B }

  f3_ready := Mux(f3_req_is_mmio, io.toIbuffer.ready && f3_mmio_req_commit || !f3_valid , io.toIbuffer.ready || !f3_valid)

  when(f3_req_is_mmio){
    f3_data(0) := f3_mmio_data(15, 0)
    f3_data(1) := f3_mmio_data(31, 16)
  }

  when(fromUncache.fire())    {f3_mmio_data   :=  fromUncache.bits.data}


  switch(mmio_state){
    is(mmio_idle){
      when(f3_req_is_mmio){
        mmio_state :=  mmio_send_req
      }
    }
  
    is(mmio_send_req){
      mmio_state :=  Mux(toUncache.fire(), mmio_w_resp, mmio_send_req )
    }

    is(mmio_w_resp){
      when(fromUncache.fire()){
          val isRVC =  fromUncache.bits.data(1,0) =/= 3.U
          mmio_state :=  Mux(isRVC, mmio_resend , mmio_w_commit)
      }
    }  

    is(mmio_resend){
      mmio_state :=  Mux(toUncache.fire(), mmio_resend_w_resp, mmio_resend )
    }  

    is(mmio_resend_w_resp){
      when(fromUncache.fire()){
          mmio_state :=  mmio_w_commit
      }
    }  

    is(mmio_w_commit){
      when(mmio_has_commited){
          mmio_state  :=  mmio_idle
      }
    }  
  }

  when(f3_ftq_flush_self || f3_ftq_flush_by_older)  {
    mmio_state := mmio_idle 
    f3_mmio_data := 0.U
  }

  toUncache.valid     :=  ((mmio_state === mmio_send_req) || (mmio_state === mmio_resend)) && f3_req_is_mmio
  toUncache.bits.addr := Mux((mmio_state === mmio_resend), f3_pAddrs(0) + 2.U, f3_pAddrs(0))
  fromUncache.ready   := true.B

  preDecoderIn.instValid     :=  f3_valid && !f3_has_except
  preDecoderIn.data          :=  f3_data
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
  preDecoderIn.mmio          :=  f3_mmio
  preDecoderIn.frontendTrigger := io.frontendTrigger
  preDecoderIn.csrTriggerEnable := io.csrTriggerEnable


  // TODO: What if next packet does not match?
  when (f3_flush) {
    f3_lastHalf.valid := false.B
  }.elsewhen (io.toIbuffer.fire()) {
    f3_lastHalf.valid := preDecoderOut.hasLastHalf
    f3_lastHalf.middlePC := preDecoderOut.realEndPC
  }

  val f3_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt
  val f3_mmio_range      = VecInit((0 until PredictWidth).map(i => if(i ==0) true.B else false.B))

  io.toIbuffer.valid          := f3_valid && (!f3_req_is_mmio || f3_mmio_can_go)
  io.toIbuffer.bits.instrs    := preDecoderOut.instrs
  io.toIbuffer.bits.valid     := Mux(f3_req_is_mmio, f3_mmio_range.asUInt, f3_predecode_range & preDecoderOut.instrRange.asUInt)
  io.toIbuffer.bits.pd        := preDecoderOut.pd
  io.toIbuffer.bits.ftqPtr    := f3_ftq_req.ftqIdx
  io.toIbuffer.bits.pc        := preDecoderOut.pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := preDecoderOut.takens(i) && !f3_req_is_mmio}
  io.toIbuffer.bits.foldpc    := preDecoderOut.pc.map(i => XORFold(i(VAddrBits-1,1), MemPredPCWidth))
  io.toIbuffer.bits.ipf       := preDecoderOut.pageFault
  io.toIbuffer.bits.acf       := preDecoderOut.accessFault
  io.toIbuffer.bits.crossPageIPFFix := preDecoderOut.crossPageIPF
  io.toIbuffer.bits.triggered := preDecoderOut.triggered

  //Write back to Ftq
  val f3_cache_fetch = f3_valid && !(f2_fire && !f2_flush)
  val finishFetchMaskReg = RegNext(f3_cache_fetch)


  val f3_mmio_missOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  f3_mmio_missOffset.valid := f3_req_is_mmio
  f3_mmio_missOffset.bits  := 0.U

  toFtq.pdWb.valid           := (!finishFetchMaskReg && f3_valid && !f3_req_is_mmio) || (f3_mmio_req_commit && f3_mmio_use_seq_pc)
  toFtq.pdWb.bits.pc         := preDecoderOut.pc
  toFtq.pdWb.bits.pd         := preDecoderOut.pd
  toFtq.pdWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid :=  Mux(f3_req_is_mmio, f3_mmio_range(i), f3_predecode_range(i))}
  toFtq.pdWb.bits.ftqIdx     := f3_ftq_req.ftqIdx
  toFtq.pdWb.bits.ftqOffset  := f3_ftq_req.ftqOffset.bits
  toFtq.pdWb.bits.misOffset  := Mux(f3_req_is_mmio, f3_mmio_missOffset, preDecoderOut.misOffset)
  toFtq.pdWb.bits.cfiOffset  := preDecoderOut.cfiOffset
  toFtq.pdWb.bits.target     := Mux(f3_req_is_mmio,Mux((f3_mmio_data(1,0) =/= 3.U), f3_ftq_req.startAddr + 2.U , f3_ftq_req.startAddr + 4.U) ,preDecoderOut.target)
  toFtq.pdWb.bits.jalTarget  := preDecoderOut.jalTarget
  toFtq.pdWb.bits.instrRange := Mux(f3_req_is_mmio, f3_mmio_range, preDecoderOut.instrRange)

  val predecodeFlush     = preDecoderOut.misOffset.valid && f3_valid
  val predecodeFlushReg  = RegNext(predecodeFlush && !(f2_fire && !f2_flush))


  /** performance counter */
  val f3_perf_info     = RegEnable(next = f2_perf_info, enable = f2_fire)
  val f3_req_0    = io.toIbuffer.fire()
  val f3_req_1    = io.toIbuffer.fire() && f3_doubleLine
  val f3_hit_0    = io.toIbuffer.fire() && f3_perf_info.bank_hit(0)
  val f3_hit_1    = io.toIbuffer.fire() && f3_doubleLine & f3_perf_info.bank_hit(1)
  val f3_hit      = f3_perf_info.hit

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(15))
  })

  val perfEvents = Seq(
    ("frontendFlush                ", f3_redirect                                ),
    ("ifu_req                      ", io.toIbuffer.fire()                        ),
    ("ifu_miss                     ", io.toIbuffer.fire() && !f3_perf_info.hit   ),
    ("ifu_req_cacheline_0          ", f3_req_0                                   ),
    ("ifu_req_cacheline_1          ", f3_req_1                                   ),
    ("ifu_req_cacheline_0_hit      ", f3_hit_1                                   ),
    ("ifu_req_cacheline_1_hit      ", f3_hit_1                                   ),
    ("only_0_hit                   ", f3_perf_info.only_0_hit       && io.toIbuffer.fire() ),
    ("only_0_miss                  ", f3_perf_info.only_0_miss      && io.toIbuffer.fire() ),
    ("hit_0_hit_1                  ", f3_perf_info.hit_0_hit_1      && io.toIbuffer.fire() ),
    ("hit_0_miss_1                 ", f3_perf_info.hit_0_miss_1     && io.toIbuffer.fire() ),
    ("miss_0_hit_1                 ", f3_perf_info.miss_0_hit_1     && io.toIbuffer.fire() ),
    ("miss_0_miss_1                ", f3_perf_info.miss_0_miss_1    && io.toIbuffer.fire() ),
    ("cross_line_block             ", io.toIbuffer.fire() && f3_situation(0)     ),
    ("fall_through_is_cacheline_end", io.toIbuffer.fire() && f3_situation(1)     ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }

  f3_redirect := (!predecodeFlushReg && predecodeFlush && !f3_req_is_mmio) || (f3_mmio_req_commit && f3_mmio_use_seq_pc)

  XSPerfAccumulate("ifu_req",   io.toIbuffer.fire() )
  XSPerfAccumulate("ifu_miss",  io.toIbuffer.fire() && !f3_hit )
  XSPerfAccumulate("ifu_req_cacheline_0", f3_req_0  )
  XSPerfAccumulate("ifu_req_cacheline_1", f3_req_1  )
  XSPerfAccumulate("ifu_req_cacheline_0_hit",   f3_hit_0 )
  XSPerfAccumulate("ifu_req_cacheline_1_hit",   f3_hit_1 )
  XSPerfAccumulate("frontendFlush",  f3_redirect )
  XSPerfAccumulate("only_0_hit",      f3_perf_info.only_0_hit   && io.toIbuffer.fire()  )
  XSPerfAccumulate("only_0_miss",     f3_perf_info.only_0_miss  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_hit_1",     f3_perf_info.hit_0_hit_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_miss_1",    f3_perf_info.hit_0_miss_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("miss_0_hit_1",    f3_perf_info.miss_0_hit_1   && io.toIbuffer.fire() )
  XSPerfAccumulate("miss_0_miss_1",   f3_perf_info.miss_0_miss_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("cross_line_block", io.toIbuffer.fire() && f3_situation(0) )
  XSPerfAccumulate("fall_through_is_cacheline_end", io.toIbuffer.fire() && f3_situation(1) )
}
