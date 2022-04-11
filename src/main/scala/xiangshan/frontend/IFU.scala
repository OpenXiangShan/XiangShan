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
import freechips.rocketchip.rocket.RVCDecoder
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

trait HasIFUConst extends HasXSParameter{
  def addrAlign(addr: UInt, bytes: Int, highest: Int): UInt = Cat(addr(highest-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  def fetchQueueSize = 2

  def getBasicBlockIdx( pc: UInt, start:  UInt ): UInt = {
    val byteOffset = pc - start
    (byteOffset - instBytes.U)(log2Ceil(PredictWidth),instOffsetBits)
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
  val iTLBInter       = new BlockTlbRequestIO
  val pmp             =   new ICachePMPBundle
}

// record the situation in which fallThruAddr falls into
// the middle of an RVI inst
class LastHalfInfo(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val middlePC = UInt(VAddrBits.W)
  def matchThisBlock(startAddr: UInt) = valid && middlePC === startAddr
}

class IfuToPreDecode(implicit p: Parameters) extends XSBundle {
  val data                =  if(HasCExtension) Vec(PredictWidth + 1, UInt(16.W)) else Vec(PredictWidth, UInt(32.W))
  val frontendTrigger     = new FrontendTdataDistributeIO
  val csrTriggerEnable    = Vec(4, Bool())
  val pc                  = Vec(PredictWidth, UInt(VAddrBits.W))
}


class IfuToPredChecker(implicit p: Parameters) extends XSBundle {
  val ftqOffset     = Valid(UInt(log2Ceil(PredictWidth).W))
  val jumpOffset    = Vec(PredictWidth, UInt(XLEN.W))
  val target        = UInt(VAddrBits.W)
  val instrRange    = Vec(PredictWidth, Bool())
  val instrValid    = Vec(PredictWidth, Bool())
  val pds           = Vec(PredictWidth, new PreDecodeInfo)
  val pc            = Vec(PredictWidth, UInt(VAddrBits.W))
}

class NewIFU(implicit p: Parameters) extends XSModule
  with HasICacheParameters
  with HasIFUConst
  with HasPdConst
  with HasCircularQueuePtrHelper
  with HasPerfEvents
{
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val (toICache, fromICache) = (VecInit(io.icacheInter.map(_.req)), VecInit(io.icacheInter.map(_.resp)))
  val (toUncache, fromUncache) = (io.uncacheInter.toUncache , io.uncacheInter.fromUncache)

  def isCrossLineReq(start: UInt, end: UInt): Bool = start(blockOffBits) ^ end(blockOffBits)

  def isLastInCacheline(addr: UInt): Bool = addr(blockOffBits - 1, 1) === 0.U

  class TlbExept(implicit p: Parameters) extends XSBundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }

  val preDecoder      = Module(new PreDecode)
  val predChecker     = Module(new PredChecker)
  val frontendTrigger = Module(new FrontendTrigger)
  val (preDecoderIn, preDecoderOut)   = (preDecoder.io.in, preDecoder.io.out)
  val (checkerIn, checkerOut)         = (predChecker.io.in, predChecker.io.out)

  io.iTLBInter.resp.ready := true.B

  /**
    ******************************************************************************
    * IFU Stage 0
    * - send cacheline fetch request to ICacheMainPipe
    ******************************************************************************
    */

  val f0_valid                             = fromFtq.req.valid
  val f0_ftq_req                           = fromFtq.req.bits
  val f0_doubleLine                        = fromFtq.req.bits.crossCacheline
  val f0_vSetIdx                           = VecInit(get_idx((f0_ftq_req.startAddr)), get_idx(f0_ftq_req.nextlineStart))
  val f0_fire                              = fromFtq.req.fire()

  val f0_flush, f1_flush, f2_flush, f3_flush = WireInit(false.B)
  val from_bpu_f0_flush, from_bpu_f1_flush, from_bpu_f2_flush, from_bpu_f3_flush = WireInit(false.B)

  from_bpu_f0_flush := fromFtq.flushFromBpu.shouldFlushByStage2(f0_ftq_req.ftqIdx) ||
                       fromFtq.flushFromBpu.shouldFlushByStage3(f0_ftq_req.ftqIdx)

  val wb_redirect , mmio_redirect,  backend_redirect= WireInit(false.B)
  val f3_wb_not_flush = WireInit(false.B)

  backend_redirect := fromFtq.redirect.valid
  f3_flush := backend_redirect || (wb_redirect && !f3_wb_not_flush)
  f2_flush := backend_redirect || mmio_redirect || wb_redirect
  f1_flush := f2_flush || from_bpu_f1_flush
  f0_flush := f1_flush || from_bpu_f0_flush

  val f1_ready, f2_ready, f3_ready         = WireInit(false.B)

  fromFtq.req.ready := toICache(0).ready && toICache(1).ready && f2_ready && GTimer() > 500.U

  toICache(0).valid       := fromFtq.req.valid //&& !f0_flush
  toICache(0).bits.vaddr  := fromFtq.req.bits.startAddr
  toICache(1).valid       := fromFtq.req.valid && f0_doubleLine //&& !f0_flush
  toICache(1).bits.vaddr  := fromFtq.req.bits.nextlineStart//fromFtq.req.bits.startAddr + (PredictWidth * 2).U //TODO: timing critical

  /** <PERF> f0 fetch bubble */

  XSPerfAccumulate("fetch_bubble_ftq_not_valid",   !fromFtq.req.valid && fromFtq.req.ready  )
  XSPerfAccumulate("fetch_bubble_pipe_stall",    f0_valid && toICache(0).ready && toICache(1).ready && !f1_ready )
  XSPerfAccumulate("fetch_bubble_icache_0_busy",   f0_valid && !toICache(0).ready  )
  XSPerfAccumulate("fetch_bubble_icache_1_busy",   f0_valid && !toICache(1).ready  )
  XSPerfAccumulate("fetch_flush_backend_redirect",   backend_redirect  )
  XSPerfAccumulate("fetch_flush_wb_redirect",    wb_redirect  )
  XSPerfAccumulate("fetch_flush_bpu_f1_flush",   from_bpu_f1_flush  )
  XSPerfAccumulate("fetch_flush_bpu_f0_flush",   from_bpu_f0_flush  )


  /**
    ******************************************************************************
    * IFU Stage 1
    * - calculate pc/half_pc/cut_ptr for every instruction
    ******************************************************************************
    */

  val f1_valid      = RegInit(false.B)
  val f1_ftq_req    = RegEnable(next = f0_ftq_req,    enable=f0_fire)
  // val f1_situation  = RegEnable(next = f0_situation,  enable=f0_fire)
  val f1_doubleLine = RegEnable(next = f0_doubleLine, enable=f0_fire)
  val f1_vSetIdx    = RegEnable(next = f0_vSetIdx,    enable=f0_fire)
  val f1_fire       = f1_valid && f1_ready

  f1_ready := f2_ready || !f1_valid

  from_bpu_f1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx) && f1_valid
  // from_bpu_f1_flush := false.B

  when(f1_flush)                  {f1_valid  := false.B}
  .elsewhen(f0_fire && !f0_flush) {f1_valid  := true.B}
  .elsewhen(f1_fire)              {f1_valid  := false.B}

  val f1_pc                 = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + (i * 2).U))
  val f1_half_snpc          = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + ((i+2) * 2).U))
  val f1_cut_ptr            = if(HasCExtension)  VecInit((0 until PredictWidth + 1).map(i =>  Cat(0.U(1.W), f1_ftq_req.startAddr(blockOffBits-1, 1)) + i.U ))
                                  else           VecInit((0 until PredictWidth).map(i =>     Cat(0.U(1.W), f1_ftq_req.startAddr(blockOffBits-1, 2)) + i.U ))

  /**
    ******************************************************************************
    * IFU Stage 2
    * - icache response data (latched for pipeline stop)
    * - generate exceprion bits for every instruciton (page fault/access fault/mmio)
    * - generate predicted instruction range (1 means this instruciton is in this fetch packet)
    * - cut data from cachlines to packet instruction code
    * - instruction predecode and RVC expand
    ******************************************************************************
    */

  val icacheRespAllValid = WireInit(false.B)

  val f2_valid      = RegInit(false.B)
  val f2_ftq_req    = RegEnable(next = f1_ftq_req,    enable=f1_fire)
  // val f2_situation  = RegEnable(next = f1_situation,  enable=f1_fire)
  val f2_doubleLine = RegEnable(next = f1_doubleLine, enable=f1_fire)
  val f2_vSetIdx    = RegEnable(next = f1_vSetIdx,    enable=f1_fire)
  val f2_fire       = f2_valid && f2_ready

  f2_ready := f3_ready && icacheRespAllValid || !f2_valid
  //TODO: addr compare may be timing critical
  val f2_icache_all_resp_wire       =  fromICache(0).valid && (fromICache(0).bits.vaddr ===  f2_ftq_req.startAddr) && ((fromICache(1).valid && (fromICache(1).bits.vaddr ===  f2_ftq_req.nextlineStart)) || !f2_doubleLine)
  val f2_icache_all_resp_reg        = RegInit(false.B)

  icacheRespAllValid := f2_icache_all_resp_reg || f2_icache_all_resp_wire

  io.icacheStop := !f3_ready

  when(f2_flush)                                              {f2_icache_all_resp_reg := false.B}
  .elsewhen(f2_valid && f2_icache_all_resp_wire && !f3_ready) {f2_icache_all_resp_reg := true.B}
  .elsewhen(f2_fire && f2_icache_all_resp_reg)                {f2_icache_all_resp_reg := false.B}

  when(f2_flush)                  {f2_valid := false.B}
  .elsewhen(f1_fire && !f1_flush) {f2_valid := true.B }
  .elsewhen(f2_fire)              {f2_valid := false.B}

  // val f2_cache_response_data = ResultHoldBypass(valid = f2_icache_all_resp_wire, data = VecInit(fromICache.map(_.bits.readData)))
  val f2_cache_response_data = VecInit(fromICache.map(_.bits.readData))


  val f2_except_pf    = VecInit((0 until PortNumber).map(i => fromICache(i).bits.tlbExcp.pageFault))
  val f2_except_af    = VecInit((0 until PortNumber).map(i => fromICache(i).bits.tlbExcp.accessFault))
  val f2_mmio         = fromICache(0).bits.tlbExcp.mmio && !fromICache(0).bits.tlbExcp.accessFault &&
                                                           !fromICache(0).bits.tlbExcp.pageFault

  val f2_pc               = RegEnable(next = f1_pc, enable = f1_fire)
  val f2_half_snpc        = RegEnable(next = f1_half_snpc, enable = f1_fire)
  val f2_cut_ptr          = RegEnable(next = f1_cut_ptr, enable = f1_fire)

  val f2_resend_vaddr     = RegEnable(next = f1_ftq_req.startAddr + 2.U, enable = f1_fire)

  def isNextLine(pc: UInt, startAddr: UInt) = {
    startAddr(blockOffBits) ^ pc(blockOffBits)
  }

  def isLastInLine(pc: UInt) = {
    pc(blockOffBits - 1, 0) === "b111110".U
  }

  val f2_foldpc = VecInit(f2_pc.map(i => XORFold(i(VAddrBits-1,1), MemPredPCWidth)))
  val f2_jump_range = Fill(PredictWidth, !f2_ftq_req.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~f2_ftq_req.ftqOffset.bits
  val f2_ftr_range  = Fill(PredictWidth,  f2_ftq_req.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~getBasicBlockIdx(f2_ftq_req.nextStartAddr, f2_ftq_req.startAddr)
  val f2_instr_range = f2_jump_range & f2_ftr_range
  val f2_pf_vec = VecInit((0 until PredictWidth).map(i => (!isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_except_pf(0)   ||  isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_doubleLine &&  f2_except_pf(1))))
  val f2_af_vec = VecInit((0 until PredictWidth).map(i => (!isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_except_af(0)   ||  isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_doubleLine && f2_except_af(1))))

  val f2_paddrs       = VecInit((0 until PortNumber).map(i => fromICache(i).bits.paddr))
  val f2_perf_info    = io.icachePerfInfo

  def cut(cacheline: UInt, cutPtr: Vec[UInt]) : Vec[UInt] ={
    if(HasCExtension){
      val result   = Wire(Vec(PredictWidth + 1, UInt(16.W)))
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 2, UInt(16.W)))
      (0 until PredictWidth + 1).foreach( i =>
        result(i) := dataVec(cutPtr(i))
      )
      result
    } else {
      val result   = Wire(Vec(PredictWidth, UInt(32.W)) )
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 4, UInt(32.W)))
      (0 until PredictWidth).foreach( i =>
        result(i) := dataVec(cutPtr(i))
      )
      result
    }
  }

  val f2_datas        = VecInit((0 until PortNumber).map(i => f2_cache_response_data(i)))
  val f2_cut_data = cut( Cat(f2_datas.map(cacheline => cacheline.asUInt ).reverse).asUInt, f2_cut_ptr )

  /** predecode (include RVC expander) */
  preDecoderIn.data := f2_cut_data
  preDecoderIn.frontendTrigger := io.frontendTrigger
  preDecoderIn.csrTriggerEnable := io.csrTriggerEnable
  preDecoderIn.pc  := f2_pc

  val f2_expd_instr   = preDecoderOut.expInstr
  val f2_pd           = preDecoderOut.pd
  val f2_jump_offset  = preDecoderOut.jumpOffset
  val f2_hasHalfValid  =  preDecoderOut.hasHalfValid
  val f2_crossPageFault = VecInit((0 until PredictWidth).map(i => isLastInLine(f2_pc(i)) && !f2_except_pf(0) && f2_doubleLine &&  f2_except_pf(1) && !f2_pd(i).isRVC ))

  val predecodeOutValid = WireInit(false.B)

  XSPerfAccumulate("fetch_bubble_icache_not_resp",   f2_valid && !icacheRespAllValid )


  /**
    ******************************************************************************
    * IFU Stage 3
    * - handle MMIO instruciton
    *  -send request to Uncache fetch Unit
    *  -every packet include 1 MMIO instruction
    *  -MMIO instructions will stop fetch pipeline until commiting from RoB
    *  -flush to snpc (send ifu_redirect to Ftq)
    * - Ibuffer enqueue
    * - check predict result in Frontend (jalFault/retFault/notCFIFault/invalidTakenFault/targetFault)
    * - handle last half RVI instruction
    ******************************************************************************
    */

  val f3_valid          = RegInit(false.B)
  val f3_ftq_req        = RegEnable(next = f2_ftq_req,    enable=f2_fire)
  // val f3_situation      = RegEnable(next = f2_situation,  enable=f2_fire)
  val f3_doubleLine     = RegEnable(next = f2_doubleLine, enable=f2_fire)
  val f3_fire           = io.toIbuffer.fire()

  f3_ready := io.toIbuffer.ready || !f3_valid

  val f3_cut_data       = RegEnable(next = f2_cut_data, enable=f2_fire)

  val f3_except_pf      = RegEnable(next = f2_except_pf, enable = f2_fire)
  val f3_except_af      = RegEnable(next = f2_except_af, enable = f2_fire)
  val f3_mmio           = RegEnable(next = f2_mmio   , enable = f2_fire)

  val f3_expd_instr     = RegEnable(next = f2_expd_instr,  enable = f2_fire)
  val f3_pd             = RegEnable(next = f2_pd,          enable = f2_fire)
  val f3_jump_offset    = RegEnable(next = f2_jump_offset, enable = f2_fire)
  val f3_af_vec         = RegEnable(next = f2_af_vec,      enable = f2_fire)
  val f3_pf_vec         = RegEnable(next = f2_pf_vec ,     enable = f2_fire)
  val f3_pc             = RegEnable(next = f2_pc,          enable = f2_fire)
  val f3_half_snpc        = RegEnable(next = f2_half_snpc, enable = f2_fire)
  val f3_instr_range    = RegEnable(next = f2_instr_range, enable = f2_fire)
  val f3_foldpc         = RegEnable(next = f2_foldpc,      enable = f2_fire)
  val f3_crossPageFault = RegEnable(next = f2_crossPageFault,      enable = f2_fire)
  val f3_hasHalfValid   = RegEnable(next = f2_hasHalfValid,      enable = f2_fire)
  val f3_except         = VecInit((0 until 2).map{i => f3_except_pf(i) || f3_except_af(i)})
  val f3_has_except     = f3_valid && (f3_except_af.reduce(_||_) || f3_except_pf.reduce(_||_))
  val f3_pAddrs   = RegEnable(next = f2_paddrs, enable = f2_fire)
  val f3_resend_vaddr   = RegEnable(next = f2_resend_vaddr,      enable = f2_fire)

  when(f3_valid && !f3_ftq_req.ftqOffset.valid){
    assert(f3_ftq_req.startAddr + 32.U >= f3_ftq_req.nextStartAddr , "More tha 32 Bytes fetch is not allowed!")
  }

  /*** MMIO State Machine***/
  val f3_mmio_data    = Reg(Vec(2, UInt(16.W)))
  val mmio_is_RVC     = RegInit(false.B)
  val mmio_resend_addr =RegInit(0.U(PAddrBits.W))
  val mmio_resend_af  = RegInit(false.B)
  val mmio_resend_pf  = RegInit(false.B)


  val m_idle :: m_sendReq :: m_waitResp :: m_sendTLB :: m_tlbResp :: m_sendPMP :: m_resendReq :: m_waitResendResp :: m_waitCommit :: m_commited :: Nil = Enum(10)
  val mmio_state = RegInit(m_idle)

  val f3_req_is_mmio     = f3_mmio && f3_valid
  val mmio_commit = VecInit(io.rob_commits.map{commit => commit.valid && commit.bits.ftqIdx === f3_ftq_req.ftqIdx &&  commit.bits.ftqOffset === 0.U}).asUInt.orR
  val f3_mmio_req_commit = f3_req_is_mmio && mmio_state === m_commited

  val f3_mmio_to_commit =  f3_req_is_mmio && mmio_state === m_waitCommit
  val f3_mmio_to_commit_next = RegNext(f3_mmio_to_commit)
  val f3_mmio_can_go      = f3_mmio_to_commit && !f3_mmio_to_commit_next

  val fromFtqRedirectReg = RegNext(fromFtq.redirect)
  val f3_ftq_flush_self     = fromFtqRedirectReg.valid && RedirectLevel.flushItself(fromFtqRedirectReg.bits.level)
  val f3_ftq_flush_by_older = fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, f3_ftq_req.ftqIdx)

  val f3_need_not_flush = f3_req_is_mmio && fromFtqRedirectReg.valid && !f3_ftq_flush_self && !f3_ftq_flush_by_older

  when(f3_flush && !f3_need_not_flush)               {f3_valid := false.B}
  .elsewhen(f2_fire && !f2_flush )                   {f3_valid := true.B }
  .elsewhen(io.toIbuffer.fire() && !f3_req_is_mmio)          {f3_valid := false.B}
  .elsewhen{f3_req_is_mmio && f3_mmio_req_commit}            {f3_valid := false.B}

  val f3_mmio_use_seq_pc = RegInit(false.B)

  val (redirect_ftqIdx, redirect_ftqOffset)  = (fromFtqRedirectReg.bits.ftqIdx,fromFtqRedirectReg.bits.ftqOffset)
  val redirect_mmio_req = fromFtqRedirectReg.valid && redirect_ftqIdx === f3_ftq_req.ftqIdx && redirect_ftqOffset === 0.U

  when(RegNext(f2_fire && !f2_flush) && f3_req_is_mmio)        { f3_mmio_use_seq_pc := true.B  }
  .elsewhen(redirect_mmio_req)                                 { f3_mmio_use_seq_pc := false.B }

  f3_ready := Mux(f3_req_is_mmio, io.toIbuffer.ready && f3_mmio_req_commit || !f3_valid , io.toIbuffer.ready || !f3_valid)

  // when(fromUncache.fire())    {f3_mmio_data   :=  fromUncache.bits.data}


  switch(mmio_state){
    is(m_idle){
      when(f3_req_is_mmio){
        mmio_state :=  m_sendReq
      }
    }

    is(m_sendReq){
      mmio_state :=  Mux(toUncache.fire(), m_waitResp, m_sendReq )
    }

    is(m_waitResp){
      when(fromUncache.fire()){
          val isRVC =  fromUncache.bits.data(1,0) =/= 3.U
          val needResend = !isRVC && f3_pAddrs(0)(2,1) === 3.U
          mmio_state :=  Mux(needResend, m_sendTLB , m_waitCommit)

          mmio_is_RVC := isRVC
          f3_mmio_data(0)   :=  fromUncache.bits.data(15,0)
          f3_mmio_data(1)   :=  fromUncache.bits.data(31,16)
      }
    }

    is(m_sendTLB){
      when( io.iTLBInter.req.valid && !io.iTLBInter.resp.bits.miss ){
        mmio_state :=  m_tlbResp
      }
    }

    is(m_tlbResp){
      val tlbExept = io.iTLBInter.resp.bits.excp.pf.instr ||
                     io.iTLBInter.resp.bits.excp.af.instr
      mmio_state :=  Mux(tlbExept,m_waitCommit,m_sendPMP)
      mmio_resend_addr := io.iTLBInter.resp.bits.paddr
      mmio_resend_af := mmio_resend_af || io.iTLBInter.resp.bits.excp.af.instr
      mmio_resend_pf := mmio_resend_pf || io.iTLBInter.resp.bits.excp.pf.instr
    }

    is(m_sendPMP){
          val pmpExcpAF = io.pmp.resp.instr || !io.pmp.resp.mmio
          mmio_state :=  Mux(pmpExcpAF, m_waitCommit , m_resendReq)
          mmio_resend_af := pmpExcpAF
    }

    is(m_resendReq){
      mmio_state :=  Mux(toUncache.fire(), m_waitResendResp, m_resendReq )
    }

    is(m_waitResendResp){
      when(fromUncache.fire()){
          mmio_state :=  m_waitCommit
          f3_mmio_data(1)   :=  fromUncache.bits.data(15,0)
      }
    }

    is(m_waitCommit){
      when(mmio_commit){
          mmio_state  :=  m_commited
      }
    }

    //normal mmio instruction
    is(m_commited){
        mmio_state := m_idle
        mmio_is_RVC := false.B
        mmio_resend_addr := 0.U
    }
  }

  //exception or flush by older branch prediction
  when(f3_ftq_flush_self || f3_ftq_flush_by_older)  {
    mmio_state := m_idle
    mmio_is_RVC := false.B
    mmio_resend_addr := 0.U
    mmio_resend_af := false.B
    f3_mmio_data.map(_ := 0.U)
  }

  toUncache.valid     :=  ((mmio_state === m_sendReq) || (mmio_state === m_resendReq)) && f3_req_is_mmio
  toUncache.bits.addr := Mux((mmio_state === m_resendReq), mmio_resend_addr, f3_pAddrs(0))
  fromUncache.ready   := true.B

  io.iTLBInter.req.valid         := (mmio_state === m_sendTLB) && f3_req_is_mmio
  io.iTLBInter.req.bits.size     := 3.U
  io.iTLBInter.req.bits.vaddr    := f3_resend_vaddr
  io.iTLBInter.req.bits.debug.pc := f3_resend_vaddr

  io.iTLBInter.req.bits.cmd                 := TlbCmd.exec
  io.iTLBInter.req.bits.robIdx              := DontCare
  io.iTLBInter.req.bits.debug.isFirstIssue  := DontCare

  io.pmp.req.valid := (mmio_state === m_sendPMP) && f3_req_is_mmio
  io.pmp.req.bits.addr  := mmio_resend_addr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  val f3_lastHalf       = RegInit(0.U.asTypeOf(new LastHalfInfo))

  val f3_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt
  val f3_mmio_range      = VecInit((0 until PredictWidth).map(i => if(i ==0) true.B else false.B))
  val f3_instr_valid     = Wire(Vec(PredictWidth, Bool()))

  /*** prediction result check   ***/
  checkerIn.ftqOffset   := f3_ftq_req.ftqOffset
  checkerIn.jumpOffset  := f3_jump_offset
  checkerIn.target      := f3_ftq_req.nextStartAddr
  checkerIn.instrRange  := f3_instr_range.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.instrValid  := f3_instr_valid.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.pds         := f3_pd
  checkerIn.pc          := f3_pc

  /*** handle half RVI in the last 2 Bytes  ***/

  def hasLastHalf(idx: UInt) = {
    !f3_pd(idx).isRVC && checkerOut.fixedRange(idx) && f3_instr_valid(idx) && !checkerOut.fixedTaken(idx) && !checkerOut.fixedMissPred(idx) && ! f3_req_is_mmio
  }

  val f3_last_validIdx             = ~ParallelPriorityEncoder(checkerOut.fixedRange.reverse)

  val f3_hasLastHalf         = hasLastHalf((PredictWidth - 1).U)
  val f3_false_lastHalf      = hasLastHalf(f3_last_validIdx)
  val f3_false_snpc          = f3_half_snpc(f3_last_validIdx)

  val f3_lastHalf_mask    = VecInit((0 until PredictWidth).map( i => if(i ==0) false.B else true.B )).asUInt()

  when (f3_flush) {
    f3_lastHalf.valid := false.B
  }.elsewhen (f3_fire) {
    f3_lastHalf.valid := f3_hasLastHalf
    f3_lastHalf.middlePC := f3_ftq_req.nextStartAddr
  }

  f3_instr_valid := Mux(f3_lastHalf.valid,f3_hasHalfValid ,VecInit(f3_pd.map(inst => inst.valid)))

  /*** frontend Trigger  ***/
  frontendTrigger.io.pds  := f3_pd
  frontendTrigger.io.pc   := f3_pc
  frontendTrigger.io.data   := f3_cut_data

  frontendTrigger.io.frontendTrigger  := io.frontendTrigger
  frontendTrigger.io.csrTriggerEnable := io.csrTriggerEnable

  val f3_triggered = frontendTrigger.io.triggered

  /*** send to Ibuffer  ***/

  io.toIbuffer.valid            := f3_valid && (!f3_req_is_mmio || f3_mmio_can_go) && !f3_flush
  io.toIbuffer.bits.instrs      := f3_expd_instr
  io.toIbuffer.bits.valid       := f3_instr_valid.asUInt
  io.toIbuffer.bits.enqEnable   := checkerOut.fixedRange.asUInt & f3_instr_valid.asUInt
  io.toIbuffer.bits.pd          := f3_pd
  io.toIbuffer.bits.ftqPtr      := f3_ftq_req.ftqIdx
  io.toIbuffer.bits.pc          := f3_pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := checkerOut.fixedTaken(i) && !f3_req_is_mmio}
  io.toIbuffer.bits.foldpc      := f3_foldpc
  io.toIbuffer.bits.ipf         := VecInit(f3_pf_vec.zip(f3_crossPageFault).map{case (pf, crossPF) => pf || crossPF})
  io.toIbuffer.bits.acf         := f3_af_vec
  io.toIbuffer.bits.crossPageIPFFix := f3_crossPageFault
  io.toIbuffer.bits.triggered   := f3_triggered

  val lastHalfMask = VecInit((0 until PredictWidth).map(i => if(i ==0) false.B else true.B))
  when(f3_lastHalf.valid){
    io.toIbuffer.bits.enqEnable := checkerOut.fixedRange.asUInt & f3_instr_valid.asUInt & lastHalfMask.asUInt
    io.toIbuffer.bits.valid     := f3_lastHalf_mask & f3_instr_valid.asUInt
  }

  /** external predecode for MMIO instruction */
  when(f3_req_is_mmio){
    val inst  = Cat(f3_mmio_data(1), f3_mmio_data(0))
    val currentIsRVC   = isRVC(inst)

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC)
    val brOffset  = br_offset(inst, currentIsRVC)

    io.toIbuffer.bits.instrs (0) := new RVCDecoder(inst, XLEN).decode.bits

    io.toIbuffer.bits.pd(0).valid   := true.B
    io.toIbuffer.bits.pd(0).isRVC   := currentIsRVC
    io.toIbuffer.bits.pd(0).brType  := brType
    io.toIbuffer.bits.pd(0).isCall  := isCall
    io.toIbuffer.bits.pd(0).isRet   := isRet

    io.toIbuffer.bits.acf(0) := mmio_resend_af
    io.toIbuffer.bits.ipf(0) := mmio_resend_pf
    io.toIbuffer.bits.crossPageIPFFix(0) := mmio_resend_pf

    io.toIbuffer.bits.enqEnable   := f3_mmio_range.asUInt
  }


  //Write back to Ftq
  val f3_cache_fetch = f3_valid && !(f2_fire && !f2_flush)
  val finishFetchMaskReg = RegNext(f3_cache_fetch)

  val mmioFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  val f3_mmio_missOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  f3_mmio_missOffset.valid := f3_req_is_mmio
  f3_mmio_missOffset.bits  := 0.U

  mmioFlushWb.valid           := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire())  && f3_mmio_use_seq_pc)
  mmioFlushWb.bits.pc         := f3_pc
  mmioFlushWb.bits.pd         := f3_pd
  mmioFlushWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid :=  f3_mmio_range(i)}
  mmioFlushWb.bits.ftqIdx     := f3_ftq_req.ftqIdx
  mmioFlushWb.bits.ftqOffset  := f3_ftq_req.ftqOffset.bits
  mmioFlushWb.bits.misOffset  := f3_mmio_missOffset
  mmioFlushWb.bits.cfiOffset  := DontCare
  mmioFlushWb.bits.target     := Mux(mmio_is_RVC, f3_ftq_req.startAddr + 2.U , f3_ftq_req.startAddr + 4.U)
  mmioFlushWb.bits.jalTarget  := DontCare
  mmioFlushWb.bits.instrRange := f3_mmio_range

  mmio_redirect := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire())  && f3_mmio_use_seq_pc)

  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready",   io.toIbuffer.valid && !io.toIbuffer.ready )


  /**
    ******************************************************************************
    * IFU Write Back Stage
    * - write back predecode information to Ftq to update
    * - redirect if found fault prediction
    * - redirect if has false hit last half (last PC is not start + 32 Bytes, but in the midle of an notCFI RVI instruction)
    ******************************************************************************
    */

  val wb_valid          = RegNext(RegNext(f2_fire && !f2_flush) && !f3_req_is_mmio && !f3_flush)
  val wb_ftq_req        = RegNext(f3_ftq_req)

  val wb_check_result   = RegNext(checkerOut)
  val wb_instr_range    = RegNext(io.toIbuffer.bits.enqEnable)
  val wb_pc             = RegNext(f3_pc)
  val wb_pd             = RegNext(f3_pd)
  val wb_instr_valid    = RegNext(f3_instr_valid)

  /* false hit lastHalf */
  val wb_lastIdx        = RegNext(f3_last_validIdx)
  val wb_false_lastHalf = RegNext(f3_false_lastHalf) && wb_lastIdx =/= (PredictWidth - 1).U
  val wb_false_target   = RegNext(f3_false_snpc)

  val wb_half_flush = wb_false_lastHalf
  val wb_half_target = wb_false_target

  /* false oversize */
  val lastIsRVC = wb_instr_range.asTypeOf(Vec(PredictWidth,Bool())).last  && wb_pd.last.isRVC
  val lastIsRVI = wb_instr_range.asTypeOf(Vec(PredictWidth,Bool()))(PredictWidth - 2) && !wb_pd(PredictWidth - 2).isRVC
  val lastTaken = wb_check_result.fixedTaken.last

  f3_wb_not_flush := wb_ftq_req.ftqIdx === f3_ftq_req.ftqIdx && f3_valid && wb_valid

  val checkFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  checkFlushWb.valid                  := wb_valid
  checkFlushWb.bits.pc                := wb_pc
  checkFlushWb.bits.pd                := wb_pd
  checkFlushWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid := wb_instr_valid(i)}
  checkFlushWb.bits.ftqIdx            := wb_ftq_req.ftqIdx
  checkFlushWb.bits.ftqOffset         := wb_ftq_req.ftqOffset.bits
  checkFlushWb.bits.misOffset.valid   := ParallelOR(wb_check_result.fixedMissPred) || wb_half_flush
  checkFlushWb.bits.misOffset.bits    := Mux(wb_half_flush, (PredictWidth - 1).U, ParallelPriorityEncoder(wb_check_result.fixedMissPred))
  checkFlushWb.bits.cfiOffset.valid   := ParallelOR(wb_check_result.fixedTaken)
  checkFlushWb.bits.cfiOffset.bits    := ParallelPriorityEncoder(wb_check_result.fixedTaken)
  checkFlushWb.bits.target            := Mux(wb_half_flush, wb_half_target, wb_check_result.fixedTarget(ParallelPriorityEncoder(wb_check_result.fixedMissPred)))
  checkFlushWb.bits.jalTarget         := wb_check_result.fixedTarget(ParallelPriorityEncoder(VecInit(wb_pd.zip(wb_instr_valid).map{case (pd, v) => v && pd.isJal })))
  checkFlushWb.bits.instrRange        := wb_instr_range.asTypeOf(Vec(PredictWidth, Bool()))

  toFtq.pdWb := Mux(f3_req_is_mmio, mmioFlushWb,  checkFlushWb)

  wb_redirect := checkFlushWb.bits.misOffset.valid && wb_valid


  /*write back flush type*/
  val checkFaultType = wb_check_result.faultType
  val checkJalFault =  wb_valid && checkFaultType.map(_.isjalFault).reduce(_||_)
  val checkRetFault =  wb_valid && checkFaultType.map(_.isRetFault).reduce(_||_)
  val checkTargetFault =  wb_valid && checkFaultType.map(_.istargetFault).reduce(_||_)
  val checkNotCFIFault =  wb_valid && checkFaultType.map(_.notCFIFault).reduce(_||_)
  val checkInvalidTaken =  wb_valid && checkFaultType.map(_.invalidTakenFault).reduce(_||_)


  XSPerfAccumulate("predecode_flush_jalFault",   checkJalFault )
  XSPerfAccumulate("predecode_flush_retFault",   checkRetFault )
  XSPerfAccumulate("predecode_flush_targetFault",   checkTargetFault )
  XSPerfAccumulate("predecode_flush_notCFIFault",   checkNotCFIFault )
  XSPerfAccumulate("predecode_flush_incalidTakenFault",   checkInvalidTaken )

  when(checkRetFault){
    XSDebug("startAddr:%x  nextstartAddr:%x  taken:%d    takenIdx:%d\n",
        wb_ftq_req.startAddr, wb_ftq_req.nextStartAddr, wb_ftq_req.ftqOffset.valid, wb_ftq_req.ftqOffset.bits)
  }

  /** performance counter */
  val f3_perf_info     = RegEnable(next = f2_perf_info, enable = f2_fire)
  val f3_req_0    = io.toIbuffer.fire()
  val f3_req_1    = io.toIbuffer.fire() && f3_doubleLine
  val f3_hit_0    = io.toIbuffer.fire() && f3_perf_info.bank_hit(0)
  val f3_hit_1    = io.toIbuffer.fire() && f3_doubleLine & f3_perf_info.bank_hit(1)
  val f3_hit      = f3_perf_info.hit
  val perfEvents = Seq(
    ("frontendFlush                ", wb_redirect                                ),
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
  )
  generatePerfEvent()

  XSPerfAccumulate("ifu_req",   io.toIbuffer.fire() )
  XSPerfAccumulate("ifu_miss",  io.toIbuffer.fire() && !f3_hit )
  XSPerfAccumulate("ifu_req_cacheline_0", f3_req_0  )
  XSPerfAccumulate("ifu_req_cacheline_1", f3_req_1  )
  XSPerfAccumulate("ifu_req_cacheline_0_hit",   f3_hit_0 )
  XSPerfAccumulate("ifu_req_cacheline_1_hit",   f3_hit_1 )
  XSPerfAccumulate("frontendFlush",  wb_redirect )
  XSPerfAccumulate("only_0_hit",      f3_perf_info.only_0_hit   && io.toIbuffer.fire()  )
  XSPerfAccumulate("only_0_miss",     f3_perf_info.only_0_miss  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_hit_1",     f3_perf_info.hit_0_hit_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("hit_0_miss_1",    f3_perf_info.hit_0_miss_1  && io.toIbuffer.fire()  )
  XSPerfAccumulate("miss_0_hit_1",    f3_perf_info.miss_0_hit_1   && io.toIbuffer.fire() )
  XSPerfAccumulate("miss_0_miss_1",   f3_perf_info.miss_0_miss_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("hit_0_except_1",   f3_perf_info.hit_0_except_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("miss_0_except_1",   f3_perf_info.miss_0_except_1 && io.toIbuffer.fire() )
  XSPerfAccumulate("except_0",   f3_perf_info.except_0 && io.toIbuffer.fire() )
  //my below
  XSPerfHistogram("ifu2ibuffer_validCnt", PopCount(io.toIbuffer.bits.valid & io.toIbuffer.bits.enqEnable), io.toIbuffer.fire, 0, PredictWidth, 1)
  //my above
}
