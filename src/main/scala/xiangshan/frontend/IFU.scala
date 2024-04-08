/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.RVCDecoder
import xiangshan._
import xiangshan.cache.mmu._
import xiangshan.frontend.icache._
import utils._
import utility._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import utility.ChiselDB

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

class IfuToBackendIO(implicit p:Parameters) extends XSBundle {
  // write to backend gpaddr mem
  val gpaddrMem_wen = Output(Bool())
  val gpaddrMem_waddr = Output(UInt(log2Ceil(FtqSize).W)) // Ftq Ptr
  // 2 gpaddrs, correspond to startAddr & nextLineAddr in bundle FtqICacheInfo
  // TODO: avoid cross page entry in Ftq
  val gpaddrMem_wdata = Output(UInt(GPAddrBits.W))
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
  val ftqInter         = new FtqInterface
  val icacheInter      = Flipped(new IFUICacheIO)
  val icacheStop       = Output(Bool())
  val icachePerfInfo   = Input(new ICachePerfInfo)
  val toIbuffer        = Decoupled(new FetchToIBuffer)
  val toBackend        = new IfuToBackendIO
  val uncacheInter     = new UncacheInterface
  val frontendTrigger  = Flipped(new FrontendTdataDistributeIO)
  val rob_commits      = Flipped(Vec(CommitWidth, Valid(new RobCommitInfo)))
  val iTLBInter        = new TlbRequestIO
  val pmp              = new ICachePMPBundle
  val mmioCommitRead   = new mmioCommitRead
  val illBuf          = Output(UInt(32.W))  
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
  val fire_in       = Bool()
}

class FetchToIBufferDB extends Bundle {
  val start_addr = UInt(39.W)
  val instr_count = UInt(32.W)
  val exception = Bool()
  val is_cache_hit = Bool()
}

class IfuWbToFtqDB extends Bundle {
  val start_addr = UInt(39.W)
  val is_miss_pred = Bool()
  val miss_pred_offset = UInt(32.W)
  val checkJalFault = Bool()
  val checkRetFault = Bool()
  val checkTargetFault = Bool()
  val checkNotCFIFault = Bool()
  val checkInvalidTaken = Bool()
}

class NewIFU(implicit p: Parameters) extends XSModule
  with HasICacheParameters
  with HasXSParameter
  with HasIFUConst
  with HasPdConst
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasTlbConst
{
  val io = IO(new NewIFUIO)
  val (toFtq, fromFtq)    = (io.ftqInter.toFtq, io.ftqInter.fromFtq)
  val fromICache = io.icacheInter.resp
  val (toUncache, fromUncache) = (io.uncacheInter.toUncache , io.uncacheInter.fromUncache)

  def isCrossLineReq(start: UInt, end: UInt): Bool = start(blockOffBits) ^ end(blockOffBits)

  def numOfStage = 3
  // equal lower_result overflow bit
  def PcCutPoint = (VAddrBits/4) - 1
  def CatPC(low: UInt, high: UInt, high1: UInt): UInt = {
    Mux(
      low(PcCutPoint),
      Cat(high1, low(PcCutPoint-1, 0)),
      Cat(high, low(PcCutPoint-1, 0))
    )
  }
  def CatPC(lowVec: Vec[UInt], high: UInt, high1: UInt): Vec[UInt] = VecInit(lowVec.map(CatPC(_, high, high1)))
  require(numOfStage > 1, "BPU numOfStage must be greater than 1")
  val topdown_stages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))
  // bubble events in IFU, only happen in stage 1
  val icacheMissBubble = Wire(Bool())
  val itlbMissBubble =Wire(Bool())

  // only driven by clock, not valid-ready
  topdown_stages(0) := fromFtq.req.bits.topdown_info
  for (i <- 1 until numOfStage) {
    topdown_stages(i) := topdown_stages(i - 1)
  }
  when (icacheMissBubble) {
    topdown_stages(1).reasons(TopDownCounters.ICacheMissBubble.id) := true.B
  }
  when (itlbMissBubble) {
    topdown_stages(1).reasons(TopDownCounters.ITLBMissBubble.id) := true.B
  }
  io.toIbuffer.bits.topdown_info := topdown_stages(numOfStage - 1)
  when (fromFtq.topdown_redirect.valid) {
    // only redirect from backend, IFU redirect itself is handled elsewhere
    when (fromFtq.topdown_redirect.bits.debugIsCtrl) {
      /*
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
      }
      io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
      */
      when (fromFtq.topdown_redirect.bits.ControlBTBMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
        }
        io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      } .elsewhen (fromFtq.topdown_redirect.bits.TAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.TAGEMissBubble.id) := true.B
        }
        io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      } .elsewhen (fromFtq.topdown_redirect.bits.SCMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.SCMissBubble.id) := true.B
        }
        io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
      } .elsewhen (fromFtq.topdown_redirect.bits.ITTAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
        }
        io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      } .elsewhen (fromFtq.topdown_redirect.bits.RASMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.RASMissBubble.id) := true.B
        }
        io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }
    } .elsewhen (fromFtq.topdown_redirect.bits.debugIsMemVio) {
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      }
      io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    } .otherwise {
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      }
      io.toIbuffer.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  class TlbExept(implicit p: Parameters) extends XSBundle{
    val pageFault = Bool()
    val accessFault = Bool()
    val mmio = Bool()
  }

  val preDecoder       = Module(new PreDecode)

  val predChecker     = Module(new PredChecker)
  val frontendTrigger = Module(new FrontendTrigger)
  val (checkerIn, checkerOutStage1, checkerOutStage2)         = (predChecker.io.in, predChecker.io.out.stage1Out,predChecker.io.out.stage2Out)

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
  val f0_fire                              = fromFtq.req.fire

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

  fromFtq.req.ready := f1_ready && io.icacheInter.icacheReady


  when (wb_redirect) {
    when (f3_wb_not_flush) {
      topdown_stages(2).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
    for (i <- 0 until numOfStage - 1) {
      topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
  }

  /** <PERF> f0 fetch bubble */

  XSPerfAccumulate("fetch_bubble_ftq_not_valid",   !fromFtq.req.valid && fromFtq.req.ready  )
  // XSPerfAccumulate("fetch_bubble_pipe_stall",    f0_valid && toICache(0).ready && toICache(1).ready && !f1_ready )
  // XSPerfAccumulate("fetch_bubble_icache_0_busy",   f0_valid && !toICache(0).ready  )
  // XSPerfAccumulate("fetch_bubble_icache_1_busy",   f0_valid && !toICache(1).ready  )
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
  val f1_ftq_req    = RegEnable(f0_ftq_req,    f0_fire)
  // val f1_situation  = RegEnable(f0_situation,  f0_fire)
  val f1_doubleLine = RegEnable(f0_doubleLine, f0_fire)
  val f1_vSetIdx    = RegEnable(f0_vSetIdx,    f0_fire)
  val f1_fire       = f1_valid && f2_ready

  f1_ready := f1_fire || !f1_valid

  from_bpu_f1_flush := fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx) && f1_valid
  // from_bpu_f1_flush := false.B

  when(f1_flush)                  {f1_valid  := false.B}
  .elsewhen(f0_fire && !f0_flush) {f1_valid  := true.B}
  .elsewhen(f1_fire)              {f1_valid  := false.B}

  val f1_pc_high            = f1_ftq_req.startAddr(VAddrBits-1, PcCutPoint)
  val f1_pc_high_plus1      = f1_pc_high + 1.U

  /**
   * In order to reduce power consumption, avoid calculating the full PC value in the first level.
   * code of original logic, this code has been deprecated
   * val f1_pc                 = VecInit(f1_pc_lower_result.map{ i =>  
   *  Mux(i(f1_pc_adder_cut_point), Cat(f1_pc_high_plus1,i(f1_pc_adder_cut_point-1,0)), Cat(f1_pc_high,i(f1_pc_adder_cut_point-1,0)))})
   * 
   */
  val f1_pc_lower_result    = VecInit((0 until PredictWidth).map(i => Cat(0.U(1.W), f1_ftq_req.startAddr(PcCutPoint-1, 0)) + (i * 2).U)) // cat with overflow bit
  
  val f1_pc                 = CatPC(f1_pc_lower_result, f1_pc_high, f1_pc_high_plus1)

  val f1_half_snpc_lower_result = VecInit((0 until PredictWidth).map(i => Cat(0.U(1.W), f1_ftq_req.startAddr(PcCutPoint-1, 0)) + ((i+2) * 2).U)) // cat with overflow bit
  val f1_half_snpc            = CatPC(f1_half_snpc_lower_result, f1_pc_high, f1_pc_high_plus1)

  if (env.FPGAPlatform){
    val f1_pc_diff          = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + (i * 2).U))
    val f1_half_snpc_diff   = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + ((i+2) * 2).U))

    XSError(f1_pc.zip(f1_pc_diff).map{ case (a,b) => a.asUInt =/= b.asUInt }.reduce(_||_), "f1_half_snpc adder cut fail")
    XSError(f1_half_snpc.zip(f1_half_snpc_diff).map{ case (a,b) => a.asUInt =/= b.asUInt }.reduce(_||_),  "f1_half_snpc adder cut fail")
  }

  val f1_cut_ptr            = if(HasCExtension)  VecInit((0 until PredictWidth + 1).map(i =>  Cat(0.U(2.W), f1_ftq_req.startAddr(blockOffBits-1, 1)) + i.U ))
                                  else           VecInit((0 until PredictWidth).map(i =>     Cat(0.U(2.W), f1_ftq_req.startAddr(blockOffBits-1, 2)) + i.U ))

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
  val f2_ftq_req    = RegEnable(f1_ftq_req,    f1_fire)
  // val f2_situation  = RegEnable(f1_situation,  f1_fire)
  val f2_doubleLine = RegEnable(f1_doubleLine, f1_fire)
  val f2_vSetIdx    = RegEnable(f1_vSetIdx,    f1_fire)
  val f2_fire       = f2_valid && f3_ready && icacheRespAllValid

  f2_ready := f2_fire || !f2_valid
  //TODO: addr compare may be timing critical
  val f2_icache_all_resp_wire       =  fromICache(0).valid && (fromICache(0).bits.vaddr ===  f2_ftq_req.startAddr) && ((fromICache(1).valid && (fromICache(1).bits.vaddr ===  f2_ftq_req.nextlineStart)) || !f2_doubleLine)
  val f2_icache_all_resp_reg        = RegInit(false.B)

  icacheRespAllValid := f2_icache_all_resp_reg || f2_icache_all_resp_wire

  icacheMissBubble := io.icacheInter.topdownIcacheMiss
  itlbMissBubble   := io.icacheInter.topdownItlbMiss

  io.icacheStop := !f3_ready

  when(f2_flush)                                              {f2_icache_all_resp_reg := false.B}
  .elsewhen(f2_valid && f2_icache_all_resp_wire && !f3_ready) {f2_icache_all_resp_reg := true.B}
  .elsewhen(f2_fire && f2_icache_all_resp_reg)                {f2_icache_all_resp_reg := false.B}

  when(f2_flush)                  {f2_valid := false.B}
  .elsewhen(f1_fire && !f1_flush) {f2_valid := true.B }
  .elsewhen(f2_fire)              {f2_valid := false.B}

  val f2_exception    = VecInit((0 until PortNumber).map(i => fromICache(i).bits.exception))
  // paddr and gpaddr of [startAddr, nextLineAddr]
  val f2_paddrs       = VecInit((0 until PortNumber).map(i => fromICache(i).bits.paddr))
  val f2_gpaddr       = fromICache(0).bits.gpaddr

  // FIXME: what if port 0 is not mmio, but port 1 is?
  // cancel mmio fetch if exception occurs
  val f2_mmio         = f2_exception(0) === ExceptionType.none && (
    fromICache(0).bits.pmp_mmio ||
      // currently, we do not distinguish between Pbmt.nc and Pbmt.io
      // anyway, they are both non-cacheable, and should be handled with mmio fsm and sent to Uncache module
      Pbmt.isUncache(fromICache(0).bits.itlb_pbmt)
  )


  /**
    * reduce the number of registers, origin code
    * f2_pc = RegEnable(f1_pc, f1_fire)
    */
  val f2_pc_lower_result        = RegEnable(f1_pc_lower_result, f1_fire)
  val f2_pc_high                = RegEnable(f1_pc_high, f1_fire)
  val f2_pc_high_plus1          = RegEnable(f1_pc_high_plus1, f1_fire)
  val f2_pc                     = CatPC(f2_pc_lower_result, f2_pc_high, f2_pc_high_plus1)

  val f2_cut_ptr                = RegEnable(f1_cut_ptr, f1_fire)
  val f2_resend_vaddr           = RegEnable(f1_ftq_req.startAddr + 2.U, f1_fire)

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
  val f2_exception_vec = VecInit((0 until PredictWidth).map( i => MuxCase(ExceptionType.none, Seq(
      !isNextLine(f2_pc(i), f2_ftq_req.startAddr)                   -> f2_exception(0),
      (isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_doubleLine) -> f2_exception(1)
  ))))
  val f2_perf_info    = io.icachePerfInfo

  def cut(cacheline: UInt, cutPtr: Vec[UInt]) : Vec[UInt] ={
    require(HasCExtension)
    // if(HasCExtension){
      val result   = Wire(Vec(PredictWidth + 1, UInt(16.W)))
      val dataVec  = cacheline.asTypeOf(Vec(blockBytes, UInt(16.W))) //32 16-bit data vector
      (0 until PredictWidth + 1).foreach( i =>
        result(i) := dataVec(cutPtr(i)) //the max ptr is 3*blockBytes/4-1
      )
      result
    // } else {
    //   val result   = Wire(Vec(PredictWidth, UInt(32.W)) )
    //   val dataVec  = cacheline.asTypeOf(Vec(blockBytes * 2/ 4, UInt(32.W)))
    //   (0 until PredictWidth).foreach( i =>
    //     result(i) := dataVec(cutPtr(i))
    //   )
    //   result
    // }
  }

  val f2_cache_response_data = fromICache.map(_.bits.data)
  val f2_data_2_cacheline = Cat(f2_cache_response_data(0), f2_cache_response_data(0))

  val f2_cut_data   = cut(f2_data_2_cacheline, f2_cut_ptr)

  /** predecode (include RVC expander) */
  // preDecoderRegIn.data := f2_reg_cut_data
  // preDecoderRegInIn.frontendTrigger := io.frontendTrigger
  // preDecoderRegInIn.csrTriggerEnable := io.csrTriggerEnable
  // preDecoderRegIn.pc  := f2_pc

  val preDecoderIn  = preDecoder.io.in
  preDecoderIn.valid := f2_valid
  preDecoderIn.bits.data := f2_cut_data
  preDecoderIn.bits.frontendTrigger := io.frontendTrigger
  preDecoderIn.bits.pc  := f2_pc
  val preDecoderOut = preDecoder.io.out

  //val f2_expd_instr     = preDecoderOut.expInstr
  val f2_instr          = preDecoderOut.instr
  val f2_pd             = preDecoderOut.pd
  val f2_jump_offset    = preDecoderOut.jumpOffset
  val f2_hasHalfValid   =  preDecoderOut.hasHalfValid
  /* if there is a cross-page RVI instruction, and the former page has no exception,
   * whether it has exception is actually depends on the latter page
   */
  val f2_crossPage_exception_vec = VecInit((0 until PredictWidth).map { i => Mux(
    isLastInLine(f2_pc(i)) && !f2_pd(i).isRVC && f2_doubleLine && f2_exception(0) === ExceptionType.none,
    f2_exception(1),
    ExceptionType.none
  )})
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
  val f3_ftq_req        = RegEnable(f2_ftq_req,    f2_fire)
  // val f3_situation      = RegEnable(f2_situation,  f2_fire)
  val f3_doubleLine     = RegEnable(f2_doubleLine, f2_fire)
  val f3_fire           = io.toIbuffer.fire

  val f3_cut_data       = RegEnable(f2_cut_data,   f2_fire)

  val f3_exception      = RegEnable(f2_exception,  f2_fire)
  val f3_mmio           = RegEnable(f2_mmio,       f2_fire)

  //val f3_expd_instr     = RegEnable(f2_expd_instr,  f2_fire)
  val f3_instr          = RegEnable(f2_instr, f2_fire)
  val f3_expd           = (0 until PredictWidth).map{ i =>
    val expander       = Module(new RVCExpander)
    expander.io.in := f3_instr(i)
    (expander.io.out.bits, expander.io.ill)
  }
  val f3_expd_instr     = VecInit(f3_expd.map(_._1))
  val f3_ill_raw        = VecInit(f3_expd.map(_._2))
  

  val f3_pd_wire         = RegEnable(f2_pd,            f2_fire)
  val f3_pd              = WireInit(f3_pd_wire)
  val f3_jump_offset     = RegEnable(f2_jump_offset,   f2_fire)
  val f3_exception_vec   = RegEnable(f2_exception_vec, f2_fire)
  val f3_crossPage_exception_vec = RegEnable(f2_crossPage_exception_vec, f2_fire)

  val f3_pc_lower_result = RegEnable(f2_pc_lower_result, f2_fire)
  val f3_pc_high         = RegEnable(f2_pc_high, f2_fire)
  val f3_pc_high_plus1   = RegEnable(f2_pc_high_plus1, f2_fire)
  val f3_pc              = CatPC(f3_pc_lower_result, f3_pc_high, f3_pc_high_plus1)

  val f3_pc_last_lower_result_plus2 = RegEnable(f2_pc_lower_result(PredictWidth - 1) + 2.U, f2_fire)
  val f3_pc_last_lower_result_plus4 = RegEnable(f2_pc_lower_result(PredictWidth - 1) + 4.U, f2_fire)
  //val f3_half_snpc      = RegEnable(f2_half_snpc,   f2_fire)
  
  /**
    ***********************************************************************
    * Half snpc(i) is larger than pc(i) by 4. Using pc to calculate half snpc may be a good choice.
    ***********************************************************************
    */
  val f3_half_snpc      = Wire(Vec(PredictWidth,UInt(VAddrBits.W)))
  for(i <- 0 until PredictWidth){
    if(i == (PredictWidth - 2)){
      f3_half_snpc(i)   := CatPC(f3_pc_last_lower_result_plus2, f3_pc_high, f3_pc_high_plus1)
    } else if (i == (PredictWidth - 1)){
      f3_half_snpc(i)   := CatPC(f3_pc_last_lower_result_plus4, f3_pc_high, f3_pc_high_plus1)
    } else {
      f3_half_snpc(i)   := f3_pc(i+2)
    }
  }

  val f3_instr_range    = RegEnable(f2_instr_range, f2_fire)
  val f3_foldpc         = RegEnable(f2_foldpc,      f2_fire)
  val f3_hasHalfValid   = RegEnable(f2_hasHalfValid,             f2_fire)
  val f3_paddrs         = RegEnable(f2_paddrs,  f2_fire)
  val f3_gpaddr         = RegEnable(f2_gpaddr,  f2_fire)
  val f3_resend_vaddr   = RegEnable(f2_resend_vaddr,             f2_fire)

  // Expand 1 bit to prevent overflow when assert
  val f3_ftq_req_startAddr      = Cat(0.U(1.W), f3_ftq_req.startAddr)
  val f3_ftq_req_nextStartAddr  = Cat(0.U(1.W), f3_ftq_req.nextStartAddr)
  // brType, isCall and isRet generation is delayed to f3 stage
  val f3Predecoder = Module(new F3Predecoder)

  f3Predecoder.io.in.instr := f3_instr

  f3_pd.zipWithIndex.map{ case (pd,i) =>
    pd.brType := f3Predecoder.io.out.pd(i).brType
    pd.isCall := f3Predecoder.io.out.pd(i).isCall
    pd.isRet  := f3Predecoder.io.out.pd(i).isRet
  }

  val f3PdDiff = f3_pd_wire.zip(f3_pd).map{ case (a,b) => a.asUInt =/= b.asUInt }.reduce(_||_)
  XSError(f3_valid && f3PdDiff, "f3 pd diff")

  when(f3_valid && !f3_ftq_req.ftqOffset.valid){
    assert(f3_ftq_req_startAddr + (2*PredictWidth).U >= f3_ftq_req_nextStartAddr, s"More tha ${2*PredictWidth} Bytes fetch is not allowed!")
  }

  /*** MMIO State Machine***/
  val f3_mmio_data          = Reg(Vec(2, UInt(16.W)))
  val mmio_is_RVC           = RegInit(false.B)
  val mmio_resend_addr      = RegInit(0.U(PAddrBits.W))
  val mmio_resend_exception = RegInit(0.U(ExceptionType.width.W))
  val mmio_resend_gpaddr    = RegInit(0.U(GPAddrBits.W))

  //last instuction finish
  val is_first_instr = RegInit(true.B)
  /*** Determine whether the MMIO instruction is executable based on the previous prediction block ***/
  io.mmioCommitRead.mmioFtqPtr := RegNext(f3_ftq_req.ftqIdx - 1.U)

  val m_idle :: m_waitLastCmt:: m_sendReq :: m_waitResp :: m_sendTLB :: m_tlbResp :: m_sendPMP :: m_resendReq :: m_waitResendResp :: m_waitCommit :: m_commited :: Nil = Enum(11)
  val mmio_state = RegInit(m_idle)

  val f3_req_is_mmio     = f3_mmio && f3_valid
  val mmio_commit = VecInit(io.rob_commits.map{commit => commit.valid && commit.bits.ftqIdx === f3_ftq_req.ftqIdx &&  commit.bits.ftqOffset === 0.U}).asUInt.orR
  val f3_mmio_req_commit = f3_req_is_mmio && mmio_state === m_commited

  val f3_mmio_to_commit =  f3_req_is_mmio && mmio_state === m_waitCommit
  val f3_mmio_to_commit_next = RegNext(f3_mmio_to_commit)
  val f3_mmio_can_go      = f3_mmio_to_commit && !f3_mmio_to_commit_next

  val fromFtqRedirectReg = Wire(fromFtq.redirect.cloneType)
  fromFtqRedirectReg.bits := RegEnable(fromFtq.redirect.bits, 0.U.asTypeOf(fromFtq.redirect.bits), fromFtq.redirect.valid)
  fromFtqRedirectReg.valid := RegNext(fromFtq.redirect.valid, init = false.B)
  val mmioF3Flush           = RegNext(f3_flush,init = false.B)
  val f3_ftq_flush_self     = fromFtqRedirectReg.valid && RedirectLevel.flushItself(fromFtqRedirectReg.bits.level)
  val f3_ftq_flush_by_older = fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, f3_ftq_req.ftqIdx)

  val f3_need_not_flush = f3_req_is_mmio && fromFtqRedirectReg.valid && !f3_ftq_flush_self && !f3_ftq_flush_by_older

  /**
    **********************************************************************************
    * We want to defer instruction fetching when encountering MMIO instructions to ensure that the MMIO region is not negatively impacted.
    * This is the exception when the first instruction is an MMIO instruction.
    **********************************************************************************
    */
  when(is_first_instr && f3_fire){
    is_first_instr := false.B
  }

  when(f3_flush && !f3_req_is_mmio)                                                 {f3_valid := false.B}
  .elsewhen(mmioF3Flush && f3_req_is_mmio && !f3_need_not_flush)                    {f3_valid := false.B}
  .elsewhen(f2_fire && !f2_flush )                                                  {f3_valid := true.B }
  .elsewhen(io.toIbuffer.fire && !f3_req_is_mmio)                                   {f3_valid := false.B}
  .elsewhen{f3_req_is_mmio && f3_mmio_req_commit}                                   {f3_valid := false.B}

  val f3_mmio_use_seq_pc = RegInit(false.B)

  val (redirect_ftqIdx, redirect_ftqOffset)  = (fromFtqRedirectReg.bits.ftqIdx,fromFtqRedirectReg.bits.ftqOffset)
  val redirect_mmio_req = fromFtqRedirectReg.valid && redirect_ftqIdx === f3_ftq_req.ftqIdx && redirect_ftqOffset === 0.U

  when(RegNext(f2_fire && !f2_flush) && f3_req_is_mmio)        { f3_mmio_use_seq_pc := true.B  }
  .elsewhen(redirect_mmio_req)                                 { f3_mmio_use_seq_pc := false.B }

  f3_ready := (io.toIbuffer.ready && (f3_mmio_req_commit || !f3_req_is_mmio)) || !f3_valid

  // mmio state machine
  switch(mmio_state){
    is(m_idle){
      when(f3_req_is_mmio){
        mmio_state := m_waitLastCmt
      }
    }

    is(m_waitLastCmt){
      when(is_first_instr){
        mmio_state := m_sendReq
      }.otherwise{
        mmio_state := Mux(io.mmioCommitRead.mmioLastCommit, m_sendReq, m_waitLastCmt)
      }
    }

    is(m_sendReq){
      mmio_state := Mux(toUncache.fire, m_waitResp, m_sendReq)
    }

    is(m_waitResp){
      when(fromUncache.fire){
          val isRVC = fromUncache.bits.data(1,0) =/= 3.U
          val needResend = !isRVC && f3_paddrs(0)(2,1) === 3.U
          mmio_state      := Mux(needResend, m_sendTLB, m_waitCommit)
          mmio_is_RVC     := isRVC
          f3_mmio_data(0) := fromUncache.bits.data(15,0)
          f3_mmio_data(1) := fromUncache.bits.data(31,16)
      }
    }

    is(m_sendTLB){
      mmio_state := Mux(io.iTLBInter.req.fire, m_tlbResp, m_sendTLB)
    }

    is(m_tlbResp){
      when(io.iTLBInter.resp.fire) {
        // we are using a blocked tlb, so resp.fire must have !resp.bits.miss
        assert(!io.iTLBInter.resp.bits.miss, "blocked mode iTLB miss when resp.fire")
        val tlb_exception = ExceptionType.fromTlbResp(io.iTLBInter.resp.bits)
        // if tlb has exception, abort checking pmp, just send instr & exception to ibuffer and wait for commit
        mmio_state := Mux(tlb_exception === ExceptionType.none, m_sendPMP, m_waitCommit)
        // also save itlb response
        mmio_resend_addr      := io.iTLBInter.resp.bits.paddr(0)
        mmio_resend_exception := tlb_exception
        mmio_resend_gpaddr    := io.iTLBInter.resp.bits.gpaddr(0)
      }
    }

    is(m_sendPMP){
      // if pmp re-check does not respond mmio, must be access fault
      val pmp_exception = Mux(io.pmp.resp.mmio, ExceptionType.fromPMPResp(io.pmp.resp), ExceptionType.af)
      // if pmp has exception, abort sending request, just send instr & exception to ibuffer and wait for commit
      mmio_state := Mux(pmp_exception === ExceptionType.none, m_resendReq, m_waitCommit)
      // also save pmp response
      mmio_resend_exception := pmp_exception
    }

    is(m_resendReq){
      mmio_state := Mux(toUncache.fire, m_waitResendResp, m_resendReq)
    }

    is(m_waitResendResp) {
      when(fromUncache.fire) {
        mmio_state      := m_waitCommit
        f3_mmio_data(1) := fromUncache.bits.data(15,0)
      }
    }

    is(m_waitCommit) {
      mmio_state := Mux(mmio_commit, m_commited, m_waitCommit)
    }

    //normal mmio instruction
    is(m_commited) {
      mmio_state            := m_idle
      mmio_is_RVC           := false.B
      mmio_resend_addr      := 0.U
      mmio_resend_exception := ExceptionType.none
      mmio_resend_gpaddr    := 0.U
    }
  }

  // Exception or flush by older branch prediction
  // Condition is from RegNext(fromFtq.redirect), 1 cycle after backend rediect
  when(f3_ftq_flush_self || f3_ftq_flush_by_older) {
    mmio_state            := m_idle
    mmio_is_RVC           := false.B
    mmio_resend_addr      := 0.U
    mmio_resend_exception := ExceptionType.none
    mmio_resend_gpaddr    := 0.U
    f3_mmio_data.map(_ := 0.U)
  }

  toUncache.valid     := ((mmio_state === m_sendReq) || (mmio_state === m_resendReq)) && f3_req_is_mmio
  toUncache.bits.addr := Mux((mmio_state === m_resendReq), mmio_resend_addr, f3_paddrs(0))
  fromUncache.ready   := true.B

  // send itlb request in m_sendTLB state
  io.iTLBInter.req.valid                   := (mmio_state === m_sendTLB) && f3_req_is_mmio
  io.iTLBInter.req.bits.size               := 3.U
  io.iTLBInter.req.bits.vaddr              := f3_resend_vaddr
  io.iTLBInter.req.bits.debug.pc           := f3_resend_vaddr
  io.iTLBInter.req.bits.cmd                := TlbCmd.exec
  io.iTLBInter.req.bits.kill               := false.B // IFU use itlb for mmio, doesn't need sync, set it to false
  io.iTLBInter.req.bits.no_translate       := false.B
  io.iTLBInter.req.bits.hyperinst          := DontCare
  io.iTLBInter.req.bits.hlvx               := DontCare
  io.iTLBInter.req.bits.memidx             := DontCare
  io.iTLBInter.req.bits.debug.robIdx       := DontCare
  io.iTLBInter.req.bits.debug.isFirstIssue := DontCare
  io.iTLBInter.req.bits.pmp_addr           := DontCare
  // whats the difference between req_kill and req.bits.kill?
  io.iTLBInter.req_kill := false.B
  // wait for itlb response in m_tlbResp state
  io.iTLBInter.resp.ready := (mmio_state === m_tlbResp) && f3_req_is_mmio

  io.pmp.req.valid := (mmio_state === m_sendPMP) && f3_req_is_mmio
  io.pmp.req.bits.addr  := mmio_resend_addr
  io.pmp.req.bits.size  := 3.U
  io.pmp.req.bits.cmd   := TlbCmd.exec

  val f3_lastHalf       = RegInit(0.U.asTypeOf(new LastHalfInfo))

  val f3_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt
  val f3_mmio_range      = VecInit((0 until PredictWidth).map(i => if(i ==0) true.B else false.B))
  val f3_instr_valid     = Wire(Vec(PredictWidth, Bool()))

  // Illegal instruction record
  val f3_ill            = VecInit((0 until PredictWidth).map{ i =>
    f3_ill_raw(i) && f3_instr_valid(i)
  })
  val f4_instr = RegEnable(f3_instr, f3_fire)
  val f4_ill = RegEnable(f3_ill, f3_fire)
  val illegalBuf = RegInit(0.U(32.W))

  val illBufClear = RegInit(true.B)

  dontTouch(illegalBuf)
  when (f4_ill.asUInt.orR && RegNext(f3_fire) && illBufClear) {
    illegalBuf := ParallelPriorityMux(f4_ill, f4_instr)
    illBufClear := false.B
  }

  when (backend_redirect || wb_redirect) {
    illBufClear := true.B
  }

  io.illBuf := illegalBuf

  /*** prediction result check   ***/
  checkerIn.ftqOffset   := f3_ftq_req.ftqOffset
  checkerIn.jumpOffset  := f3_jump_offset
  checkerIn.target      := f3_ftq_req.nextStartAddr
  checkerIn.instrRange  := f3_instr_range.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.instrValid  := f3_instr_valid.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.pds         := f3_pd
  checkerIn.pc          := f3_pc
  checkerIn.fire_in     := RegNext(f2_fire, init = false.B)

  /*** handle half RVI in the last 2 Bytes  ***/

  def hasLastHalf(idx: UInt) = {
    //!f3_pd(idx).isRVC && checkerOutStage1.fixedRange(idx) && f3_instr_valid(idx) && !checkerOutStage1.fixedTaken(idx) && !checkerOutStage2.fixedMissPred(idx) && ! f3_req_is_mmio
    !f3_pd(idx).isRVC && checkerOutStage1.fixedRange(idx) && f3_instr_valid(idx) && !checkerOutStage1.fixedTaken(idx) && ! f3_req_is_mmio
  }

  val f3_last_validIdx       = ParallelPosteriorityEncoder(checkerOutStage1.fixedRange)

  val f3_hasLastHalf         = hasLastHalf((PredictWidth - 1).U)
  val f3_false_lastHalf      = hasLastHalf(f3_last_validIdx)
  val f3_false_snpc          = f3_half_snpc(f3_last_validIdx)

  val f3_lastHalf_mask    = VecInit((0 until PredictWidth).map( i => if(i ==0) false.B else true.B )).asUInt
  val f3_lastHalf_disable = RegInit(false.B)

  when(f3_flush || (f3_fire && f3_lastHalf_disable)){
    f3_lastHalf_disable := false.B
  }

  when (f3_flush) {
    f3_lastHalf.valid := false.B
  }.elsewhen (f3_fire) {
    f3_lastHalf.valid := f3_hasLastHalf && !f3_lastHalf_disable
    f3_lastHalf.middlePC := f3_ftq_req.nextStartAddr
  }

  f3_instr_valid := Mux(f3_lastHalf.valid,f3_hasHalfValid ,VecInit(f3_pd.map(inst => inst.valid)))

  /*** frontend Trigger  ***/
  frontendTrigger.io.pds  := f3_pd
  frontendTrigger.io.pc   := f3_pc
  frontendTrigger.io.data   := f3_cut_data

  frontendTrigger.io.frontendTrigger  := io.frontendTrigger

  val f3_triggered = frontendTrigger.io.triggered
  val f3_toIbuffer_valid = f3_valid && (!f3_req_is_mmio || f3_mmio_can_go) && !f3_flush

  /*** send to Ibuffer  ***/
  io.toIbuffer.valid            := f3_toIbuffer_valid
  io.toIbuffer.bits.instrs      := f3_expd_instr
  io.toIbuffer.bits.valid       := f3_instr_valid.asUInt
  io.toIbuffer.bits.enqEnable   := checkerOutStage1.fixedRange.asUInt & f3_instr_valid.asUInt
  io.toIbuffer.bits.pd          := f3_pd
  io.toIbuffer.bits.ftqPtr      := f3_ftq_req.ftqIdx
  io.toIbuffer.bits.pc          := f3_pc
  io.toIbuffer.bits.ftqOffset.zipWithIndex.map{case(a, i) => a.bits := i.U; a.valid := checkerOutStage1.fixedTaken(i) && !f3_req_is_mmio}
  io.toIbuffer.bits.foldpc      := f3_foldpc
  io.toIbuffer.bits.exceptionType := ExceptionType.merge(f3_exception_vec, f3_crossPage_exception_vec)
  io.toIbuffer.bits.crossPageIPFFix := f3_crossPage_exception_vec.map(_ =/= ExceptionType.none)
  io.toIbuffer.bits.triggered   := f3_triggered

  when(f3_lastHalf.valid){
    io.toIbuffer.bits.enqEnable := checkerOutStage1.fixedRange.asUInt & f3_instr_valid.asUInt & f3_lastHalf_mask
    io.toIbuffer.bits.valid     := f3_lastHalf_mask & f3_instr_valid.asUInt
  }

  /** to backend */
  // f3_gpaddr is valid iff gpf is detected
  io.toBackend.gpaddrMem_wen   := f3_toIbuffer_valid && Mux(
    f3_req_is_mmio,
    mmio_resend_exception === ExceptionType.gpf,
    f3_exception.map(_ === ExceptionType.gpf).reduce(_||_)
  )
  io.toBackend.gpaddrMem_waddr := f3_ftq_req.ftqIdx.value
  io.toBackend.gpaddrMem_wdata := Mux(f3_req_is_mmio, mmio_resend_gpaddr, f3_gpaddr)

  //Write back to Ftq
  val f3_cache_fetch = f3_valid && !(f2_fire && !f2_flush)
  val finishFetchMaskReg = RegNext(f3_cache_fetch)

  val mmioFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  val f3_mmio_missOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  f3_mmio_missOffset.valid := f3_req_is_mmio
  f3_mmio_missOffset.bits  := 0.U

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmio_state reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  mmioFlushWb.valid           := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire) &&
    f3_mmio_use_seq_pc && !f3_ftq_flush_self && !f3_ftq_flush_by_older)
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

  /** external predecode for MMIO instruction */
  when(f3_req_is_mmio){
    val inst  = Cat(f3_mmio_data(1), f3_mmio_data(0))
    val currentIsRVC   = isRVC(inst)

    val brType::isCall::isRet::Nil = brInfo(inst)
    val jalOffset = jal_offset(inst, currentIsRVC)
    val brOffset  = br_offset(inst, currentIsRVC)

    io.toIbuffer.bits.instrs(0) := new RVCDecoder(inst, XLEN, fLen, useAddiForMv = true).decode.bits


    io.toIbuffer.bits.pd(0).valid   := true.B
    io.toIbuffer.bits.pd(0).isRVC   := currentIsRVC
    io.toIbuffer.bits.pd(0).brType  := brType
    io.toIbuffer.bits.pd(0).isCall  := isCall
    io.toIbuffer.bits.pd(0).isRet   := isRet

    io.toIbuffer.bits.exceptionType(0)   := mmio_resend_exception
    io.toIbuffer.bits.crossPageIPFFix(0) := mmio_resend_exception =/= ExceptionType.none

    io.toIbuffer.bits.enqEnable   := f3_mmio_range.asUInt

    mmioFlushWb.bits.pd(0).valid   := true.B
    mmioFlushWb.bits.pd(0).isRVC   := currentIsRVC
    mmioFlushWb.bits.pd(0).brType  := brType
    mmioFlushWb.bits.pd(0).isCall  := isCall
    mmioFlushWb.bits.pd(0).isRet   := isRet
  }

  mmio_redirect := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire)  && f3_mmio_use_seq_pc)

  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready",   io.toIbuffer.valid && !io.toIbuffer.ready )


  /**
    ******************************************************************************
    * IFU Write Back Stage
    * - write back predecode information to Ftq to update
    * - redirect if found fault prediction
    * - redirect if has false hit last half (last PC is not start + 32 Bytes, but in the midle of an notCFI RVI instruction)
    ******************************************************************************
    */
  val wb_enable         = RegNext(f2_fire && !f2_flush) && !f3_req_is_mmio && !f3_flush
  val wb_valid          = RegNext(wb_enable, init = false.B)
  val wb_ftq_req        = RegEnable(f3_ftq_req, wb_enable)

  val wb_check_result_stage1   = RegEnable(checkerOutStage1, wb_enable)
  val wb_check_result_stage2   = checkerOutStage2
  val wb_instr_range    = RegEnable(io.toIbuffer.bits.enqEnable, wb_enable)

  val wb_pc_lower_result        = RegEnable(f3_pc_lower_result, wb_enable)
  val wb_pc_high                = RegEnable(f3_pc_high, wb_enable)
  val wb_pc_high_plus1          = RegEnable(f3_pc_high_plus1, wb_enable)
  val wb_pc                     = CatPC(wb_pc_lower_result, wb_pc_high, wb_pc_high_plus1)

  //val wb_pc             = RegEnable(f3_pc, wb_enable)
  val wb_pd             = RegEnable(f3_pd, wb_enable)
  val wb_instr_valid    = RegEnable(f3_instr_valid, wb_enable)

  /* false hit lastHalf */
  val wb_lastIdx        = RegEnable(f3_last_validIdx, wb_enable)
  val wb_false_lastHalf = RegEnable(f3_false_lastHalf, wb_enable) && wb_lastIdx =/= (PredictWidth - 1).U
  val wb_false_target   = RegEnable(f3_false_snpc, wb_enable)

  val wb_half_flush = wb_false_lastHalf
  val wb_half_target = wb_false_target

  /* false oversize */
  val lastIsRVC = wb_instr_range.asTypeOf(Vec(PredictWidth,Bool())).last  && wb_pd.last.isRVC
  val lastIsRVI = wb_instr_range.asTypeOf(Vec(PredictWidth,Bool()))(PredictWidth - 2) && !wb_pd(PredictWidth - 2).isRVC
  val lastTaken = wb_check_result_stage1.fixedTaken.last

  f3_wb_not_flush := wb_ftq_req.ftqIdx === f3_ftq_req.ftqIdx && f3_valid && wb_valid

  /** if a req with a last half but miss predicted enters in wb stage, and this cycle f3 stalls,
    * we set a flag to notify f3 that the last half flag need not to be set.
    */
  //f3_fire is after wb_valid
  when(wb_valid && RegNext(f3_hasLastHalf,init = false.B)
        && wb_check_result_stage2.fixedMissPred(PredictWidth - 1) && !f3_fire  && !RegNext(f3_fire,init = false.B) && !f3_flush
      ){
    f3_lastHalf_disable := true.B
  }

  //wb_valid and f3_fire are in same cycle
  when(wb_valid && RegNext(f3_hasLastHalf,init = false.B)
        && wb_check_result_stage2.fixedMissPred(PredictWidth - 1) && f3_fire
      ){
    f3_lastHalf.valid := false.B
  }

  val checkFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  val checkFlushWbjalTargetIdx = ParallelPriorityEncoder(VecInit(wb_pd.zip(wb_instr_valid).map{case (pd, v) => v && pd.isJal }))
  val checkFlushWbTargetIdx = ParallelPriorityEncoder(wb_check_result_stage2.fixedMissPred)
  checkFlushWb.valid                  := wb_valid
  checkFlushWb.bits.pc                := wb_pc
  checkFlushWb.bits.pd                := wb_pd
  checkFlushWb.bits.pd.zipWithIndex.map{case(instr,i) => instr.valid := wb_instr_valid(i)}
  checkFlushWb.bits.ftqIdx            := wb_ftq_req.ftqIdx
  checkFlushWb.bits.ftqOffset         := wb_ftq_req.ftqOffset.bits
  checkFlushWb.bits.misOffset.valid   := ParallelOR(wb_check_result_stage2.fixedMissPred) || wb_half_flush
  checkFlushWb.bits.misOffset.bits    := Mux(wb_half_flush, wb_lastIdx, ParallelPriorityEncoder(wb_check_result_stage2.fixedMissPred))
  checkFlushWb.bits.cfiOffset.valid   := ParallelOR(wb_check_result_stage1.fixedTaken)
  checkFlushWb.bits.cfiOffset.bits    := ParallelPriorityEncoder(wb_check_result_stage1.fixedTaken)
  checkFlushWb.bits.target            := Mux(wb_half_flush, wb_half_target, wb_check_result_stage2.fixedTarget(checkFlushWbTargetIdx))
  checkFlushWb.bits.jalTarget         := wb_check_result_stage2.jalTarget(checkFlushWbjalTargetIdx)
  checkFlushWb.bits.instrRange        := wb_instr_range.asTypeOf(Vec(PredictWidth, Bool()))

  toFtq.pdWb := Mux(wb_valid, checkFlushWb,  mmioFlushWb)

  wb_redirect := checkFlushWb.bits.misOffset.valid && wb_valid

  /*write back flush type*/
  val checkFaultType = wb_check_result_stage2.faultType
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
  val f3_perf_info     = RegEnable(f2_perf_info,  f2_fire)
  val f3_req_0    = io.toIbuffer.fire
  val f3_req_1    = io.toIbuffer.fire && f3_doubleLine
  val f3_hit_0    = io.toIbuffer.fire && f3_perf_info.bank_hit(0)
  val f3_hit_1    = io.toIbuffer.fire && f3_doubleLine & f3_perf_info.bank_hit(1)
  val f3_hit      = f3_perf_info.hit
  val perfEvents = Seq(
    ("frontendFlush                ", wb_redirect                                ),
    ("ifu_req                      ", io.toIbuffer.fire                        ),
    ("ifu_miss                     ", io.toIbuffer.fire && !f3_perf_info.hit   ),
    ("ifu_req_cacheline_0          ", f3_req_0                                   ),
    ("ifu_req_cacheline_1          ", f3_req_1                                   ),
    ("ifu_req_cacheline_0_hit      ", f3_hit_1                                   ),
    ("ifu_req_cacheline_1_hit      ", f3_hit_1                                   ),
    ("only_0_hit                   ", f3_perf_info.only_0_hit       && io.toIbuffer.fire ),
    ("only_0_miss                  ", f3_perf_info.only_0_miss      && io.toIbuffer.fire ),
    ("hit_0_hit_1                  ", f3_perf_info.hit_0_hit_1      && io.toIbuffer.fire ),
    ("hit_0_miss_1                 ", f3_perf_info.hit_0_miss_1     && io.toIbuffer.fire ),
    ("miss_0_hit_1                 ", f3_perf_info.miss_0_hit_1     && io.toIbuffer.fire ),
    ("miss_0_miss_1                ", f3_perf_info.miss_0_miss_1    && io.toIbuffer.fire ),
  )
  generatePerfEvent()

  XSPerfAccumulate("ifu_req",   io.toIbuffer.fire )
  XSPerfAccumulate("ifu_miss",  io.toIbuffer.fire && !f3_hit )
  XSPerfAccumulate("ifu_req_cacheline_0", f3_req_0  )
  XSPerfAccumulate("ifu_req_cacheline_1", f3_req_1  )
  XSPerfAccumulate("ifu_req_cacheline_0_hit",   f3_hit_0 )
  XSPerfAccumulate("ifu_req_cacheline_1_hit",   f3_hit_1 )
  XSPerfAccumulate("frontendFlush",  wb_redirect )
  XSPerfAccumulate("only_0_hit",      f3_perf_info.only_0_hit   && io.toIbuffer.fire  )
  XSPerfAccumulate("only_0_miss",     f3_perf_info.only_0_miss  && io.toIbuffer.fire  )
  XSPerfAccumulate("hit_0_hit_1",     f3_perf_info.hit_0_hit_1  && io.toIbuffer.fire  )
  XSPerfAccumulate("hit_0_miss_1",    f3_perf_info.hit_0_miss_1  && io.toIbuffer.fire  )
  XSPerfAccumulate("miss_0_hit_1",    f3_perf_info.miss_0_hit_1   && io.toIbuffer.fire )
  XSPerfAccumulate("miss_0_miss_1",   f3_perf_info.miss_0_miss_1 && io.toIbuffer.fire )
  XSPerfAccumulate("hit_0_except_1",   f3_perf_info.hit_0_except_1 && io.toIbuffer.fire )
  XSPerfAccumulate("miss_0_except_1",   f3_perf_info.miss_0_except_1 && io.toIbuffer.fire )
  XSPerfAccumulate("except_0",   f3_perf_info.except_0 && io.toIbuffer.fire )
  XSPerfHistogram("ifu2ibuffer_validCnt", PopCount(io.toIbuffer.bits.valid & io.toIbuffer.bits.enqEnable), io.toIbuffer.fire, 0, PredictWidth + 1, 1)

  val hartId = p(XSCoreParamsKey).HartId
  val isWriteFetchToIBufferTable = Constantin.createRecord(s"isWriteFetchToIBufferTable$hartId")
  val isWriteIfuWbToFtqTable = Constantin.createRecord(s"isWriteIfuWbToFtqTable$hartId")
  val fetchToIBufferTable = ChiselDB.createTable(s"FetchToIBuffer$hartId", new FetchToIBufferDB)
  val ifuWbToFtqTable = ChiselDB.createTable(s"IfuWbToFtq$hartId", new IfuWbToFtqDB)

  val fetchIBufferDumpData = Wire(new FetchToIBufferDB)
  fetchIBufferDumpData.start_addr := f3_ftq_req.startAddr
  fetchIBufferDumpData.instr_count := PopCount(io.toIbuffer.bits.enqEnable)
  fetchIBufferDumpData.exception := (f3_perf_info.except_0 && io.toIbuffer.fire) || (f3_perf_info.hit_0_except_1 && io.toIbuffer.fire) || (f3_perf_info.miss_0_except_1 && io.toIbuffer.fire)
  fetchIBufferDumpData.is_cache_hit := f3_hit

  val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  ifuWbToFtqDumpData.start_addr := wb_ftq_req.startAddr
  ifuWbToFtqDumpData.is_miss_pred := checkFlushWb.bits.misOffset.valid
  ifuWbToFtqDumpData.miss_pred_offset := checkFlushWb.bits.misOffset.bits
  ifuWbToFtqDumpData.checkJalFault := checkJalFault
  ifuWbToFtqDumpData.checkRetFault := checkRetFault
  ifuWbToFtqDumpData.checkTargetFault := checkTargetFault
  ifuWbToFtqDumpData.checkNotCFIFault := checkNotCFIFault
  ifuWbToFtqDumpData.checkInvalidTaken := checkInvalidTaken

  fetchToIBufferTable.log(
    data = fetchIBufferDumpData,
    en = isWriteFetchToIBufferTable.orR && io.toIbuffer.fire,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )
  ifuWbToFtqTable.log(
    data = ifuWbToFtqDumpData,
    en = isWriteIfuWbToFtqTable.orR && checkFlushWb.valid,
    site = "IFU" + p(XSCoreParamsKey).HartId.toString,
    clock = clock,
    reset = reset
  )

}
