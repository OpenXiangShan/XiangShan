// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.Constantin
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.ParallelOR
import utility.ParallelPosteriorityEncoder
import utility.ParallelPriorityEncoder
import utility.XORFold
import utility.XSDebug
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.FrontendTdataDistributeIO
import xiangshan.RedirectLevel
import xiangshan.RobCommitInfo
import xiangshan.TopDownCounters
import xiangshan.ValidUndirectioned
import xiangshan.XSCoreParamsKey
import xiangshan.cache.mmu.HasTlbConst
import xiangshan.cache.mmu.Pbmt
import xiangshan.cache.mmu.TlbCmd
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.ICacheToIfuIO
import xiangshan.frontend.IfuToBackendIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.IfuToICacheIO
import xiangshan.frontend.InsUncacheReq
import xiangshan.frontend.InsUncacheResp
import xiangshan.frontend.PredecodeWritebackBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.PmpCheckBundle
import xiangshan.frontend.mmioCommitRead

class Ifu(implicit p: Parameters) extends IfuModule
    with HasICacheParameters
    with FetchBlockHelper
    with PreDecodeHelper
    with PcCutHelper
    with HasCircularQueuePtrHelper
    with HasPerfEvents
    with HasTlbConst {

  class IfuIO(implicit p: Parameters) extends IfuBundle {
    // Ftq: request / write back
    val fromFtq: FtqToIfuIO = Flipped(new FtqToIfuIO)
    val toFtq:   IfuToFtqIO = new IfuToFtqIO
    // ftq: mmio commit read
    val mmioCommitRead: mmioCommitRead = new mmioCommitRead

    // ICache: response / stall
    val fromICache: ICacheToIfuIO = Flipped(new ICacheToIfuIO)
    val toICache:   IfuToICacheIO = new IfuToICacheIO

    // Uncache: mmio request / response
    val fromUncache: DecoupledIO[InsUncacheResp] = Flipped(DecoupledIO(new InsUncacheResp))
    val toUncache:   DecoupledIO[InsUncacheReq]  = DecoupledIO(new InsUncacheReq)

    // IBuffer: enqueue
    val toIBuffer: DecoupledIO[FetchToIBuffer] = DecoupledIO(new FetchToIBuffer)

    // Backend: gpaMem
    val toBackend: IfuToBackendIO = new IfuToBackendIO
    // Backend: commit
    val robCommits: Vec[Valid[RobCommitInfo]] = Flipped(Vec(CommitWidth, Valid(new RobCommitInfo)))

    // debug extension: frontend trigger
    val frontendTrigger: FrontendTdataDistributeIO = Flipped(new FrontendTdataDistributeIO)

    // itlb: req / resp
    val itlb: TlbRequestIO = new TlbRequestIO

    // pmp: req / resp
    val pmp: PmpCheckBundle = new PmpCheckBundle

    // Backend: csr control
    val csrFsIsOff: Bool = Input(Bool())
  }
  val io: IfuIO = IO(new IfuIO)

  // submodule
  private val preDecoder      = Module(new PreDecode)
  private val predChecker     = Module(new PredChecker)
  private val frontendTrigger = Module(new FrontendTrigger)
  private val rvcExpanders    = Seq.fill(PredictWidth)(Module(new RvcExpander))
  private val mmioRvcExpander = Module(new RvcExpander)
  private val f3PreDecoder    = Module(new F3PreDecode)

  // alias
  private val (toFtq, fromFtq)              = (io.toFtq, io.fromFtq)
  private val fromICache                    = io.fromICache.fetchResp
  private val (toUncache, fromUncache)      = (io.toUncache, io.fromUncache)
  private val (preDecoderIn, preDecoderOut) = (preDecoder.io.req, preDecoder.io.resp)
  private val (checkerIn, checkerOutStage1, checkerOutStage2) =
    (predChecker.io.req, predChecker.io.resp.stage1Out, predChecker.io.resp.stage2Out)

  // Top-down
  def numOfStage: Int = 3
  require(numOfStage > 1, "BPU numOfStage must be greater than 1")
  val topdown_stages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))
  // bubble events in IFU, only happen in stage 1
  val icacheMissBubble = Wire(Bool())
  val itlbMissBubble   = Wire(Bool())

  // only driven by clock, not valid-ready
  topdown_stages(0) := fromFtq.req.bits.topdown_info
  for (i <- 1 until numOfStage) {
    topdown_stages(i) := topdown_stages(i - 1)
  }
  when(icacheMissBubble) {
    topdown_stages(1).reasons(TopDownCounters.ICacheMissBubble.id) := true.B
  }
  when(itlbMissBubble) {
    topdown_stages(1).reasons(TopDownCounters.ITLBMissBubble.id) := true.B
  }
  io.toIBuffer.bits.topdown_info := topdown_stages(numOfStage - 1)
  when(fromFtq.topdown_redirect.valid) {
    // only redirect from backend, IFU redirect itself is handled elsewhere
    when(fromFtq.topdown_redirect.bits.debugIsCtrl) {
      /*
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
       */
      when(fromFtq.topdown_redirect.bits.ControlBTBMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.TAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.TAGEMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.SCMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.SCMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.ITTAGEMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(fromFtq.topdown_redirect.bits.RASMissBubble) {
        for (i <- 0 until numOfStage) {
          topdown_stages(i).reasons(TopDownCounters.RASMissBubble.id) := true.B
        }
        io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }
    }.elsewhen(fromFtq.topdown_redirect.bits.debugIsMemVio) {
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      for (i <- 0 until numOfStage) {
        topdown_stages(i).reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
      }
      io.toIBuffer.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }

  /* *****************************************************************************
   * IFU Stage 0
   * - send cacheline fetch request to ICacheMainPipe
   * ***************************************************************************** */

  val f0_valid      = fromFtq.req.valid
  val f0_ftq_req    = fromFtq.req.bits
  val f0_doubleLine = fromFtq.req.bits.crossCacheline
  val f0_vSetIdx    = VecInit(get_idx(f0_ftq_req.startAddr.toUInt), get_idx(f0_ftq_req.nextlineStart.toUInt))
  val f0_fire       = fromFtq.req.fire

  val f0_flush, f1_flush, f2_flush, f3_flush = WireInit(false.B)

  val f0_flush_from_bpu = fromFtq.flushFromBpu.shouldFlushByStage2(f0_ftq_req.ftqIdx) ||
    fromFtq.flushFromBpu.shouldFlushByStage3(f0_ftq_req.ftqIdx)

  val wb_redirect, mmio_redirect, backend_redirect = WireInit(false.B)
  val f3_wb_not_flush                              = WireInit(false.B)

  backend_redirect := fromFtq.redirect.valid
  f3_flush         := backend_redirect || (wb_redirect && !f3_wb_not_flush)
  f2_flush         := backend_redirect || mmio_redirect || wb_redirect
  f1_flush         := f2_flush
  f0_flush         := f1_flush || f0_flush_from_bpu

  val f1_ready, f2_ready, f3_ready = WireInit(false.B)

  fromFtq.req.ready := f1_ready && io.fromICache.fetchReady

  when(wb_redirect) {
    when(f3_wb_not_flush) {
      topdown_stages(2).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
    for (i <- 0 until numOfStage - 1) {
      topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }
  }

  /** <PERF> f0 fetch bubble */
  XSPerfAccumulate("fetch_bubble_ftq_not_valid", !fromFtq.req.valid && fromFtq.req.ready)
  // XSPerfAccumulate("fetch_bubble_pipe_stall",    f0_valid && toICache(0).ready && toICache(1).ready && !f1_ready )
  // XSPerfAccumulate("fetch_bubble_icache_0_busy",   f0_valid && !toICache(0).ready  )
  // XSPerfAccumulate("fetch_bubble_icache_1_busy",   f0_valid && !toICache(1).ready  )
  XSPerfAccumulate("fetch_flush_backend_redirect", backend_redirect)
  XSPerfAccumulate("fetch_flush_wb_redirect", wb_redirect)
  XSPerfAccumulate("fetch_flush_f0_flush_from_bpu", f0_flush_from_bpu)

  /* *****************************************************************************
   * IFU Stage 1
   * - calculate pc/half_pc/cut_ptr for every instruction
   * ***************************************************************************** */

  val f1_valid   = RegInit(false.B)
  val f1_ftq_req = RegEnable(f0_ftq_req, f0_fire)
  // val f1_situation  = RegEnable(f0_situation,  f0_fire)
  val f1_doubleLine = RegEnable(f0_doubleLine, f0_fire)
  val f1_vSetIdx    = RegEnable(f0_vSetIdx, f0_fire)
  val f1_fire       = f1_valid && f2_ready

  f1_ready := f1_fire || !f1_valid

  assert(!(fromFtq.flushFromBpu.shouldFlushByStage3(f1_ftq_req.ftqIdx) && f1_valid))

  when(f1_flush)(f1_valid := false.B)
    .elsewhen(f0_fire && !f0_flush)(f1_valid := true.B)
    .elsewhen(f1_fire)(f1_valid := false.B)

  val f1_pc_high       = f1_ftq_req.startAddr(VAddrBits - 1, PcCutPoint)
  val f1_pc_high_plus1 = f1_pc_high + 1.U

  /**
   * In order to reduce power consumption, avoid calculating the full PC value in the first level.
   * code of original logic, this code has been deprecated
   * val f1_pc                 = VecInit(f1_pc_lower_result.map{ i =>
   *  Mux(i(f1_pc_adder_cut_point), Cat(f1_pc_high_plus1,i(f1_pc_adder_cut_point-1,0)), Cat(f1_pc_high,i(f1_pc_adder_cut_point-1,0)))})
   */
  val f1_pc_lower_result = VecInit((0 until PredictWidth).map(i =>
    Cat(0.U(1.W), f1_ftq_req.startAddr(PcCutPoint - 1, 0)) + (i * 2).U
  )) // cat with overflow bit

  val f1_pc = catPC(f1_pc_lower_result, f1_pc_high, f1_pc_high_plus1)

  val f1_half_snpc_lower_result = VecInit((0 until PredictWidth).map(i =>
    Cat(0.U(1.W), f1_ftq_req.startAddr(PcCutPoint - 1, 0)) + ((i + 2) * 2).U
  )) // cat with overflow bit
  val f1_half_snpc = catPC(f1_half_snpc_lower_result, f1_pc_high, f1_pc_high_plus1)

  if (env.FPGAPlatform) {
    val f1_pc_diff        = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + (i * 2).U))
    val f1_half_snpc_diff = VecInit((0 until PredictWidth).map(i => f1_ftq_req.startAddr + ((i + 2) * 2).U))

    XSError(
      f1_pc.zip(f1_pc_diff).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _),
      "f1_half_snpc adder cut fail"
    )
    XSError(
      f1_half_snpc.zip(f1_half_snpc_diff).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _),
      "f1_half_snpc adder cut fail"
    )
  }

  val f1_cut_ptr = if (HasCExtension)
    VecInit((0 until PredictWidth + 1).map(i => Cat(0.U(2.W), f1_ftq_req.startAddr(blockOffBits - 1, 1)) + i.U))
  else VecInit((0 until PredictWidth).map(i => Cat(0.U(2.W), f1_ftq_req.startAddr(blockOffBits - 1, 2)) + i.U))

  /* *****************************************************************************
   * IFU Stage 2
   * - icache response data (latched for pipeline stop)
   * - generate exceprion bits for every instruciton (page fault/access fault/mmio)
   * - generate predicted instruction range (1 means this instruciton is in this fetch packet)
   * - cut data from cachlines to packet instruction code
   * - instruction predecode and RVC expand
   * ***************************************************************************** */

  val icacheRespAllValid = WireInit(false.B)

  val f2_valid      = RegInit(false.B)
  val f2_ftq_req    = RegEnable(f1_ftq_req, f1_fire)
  val f2_doubleLine = RegEnable(f1_doubleLine, f1_fire)
  val f2_vSetIdx    = RegEnable(f1_vSetIdx, f1_fire)
  val f2_fire       = f2_valid && f3_ready && icacheRespAllValid

  f2_ready := f2_fire || !f2_valid
  // TODO: addr compare may be timing critical
  val f2_icache_all_resp_wire =
    fromICache.valid &&
      fromICache.bits.vAddr(0) === f2_ftq_req.startAddr &&
      (fromICache.bits.doubleline && fromICache.bits.vAddr(1) === f2_ftq_req.nextlineStart || !f2_doubleLine)
  val f2_icache_all_resp_reg = RegInit(false.B)

  icacheRespAllValid := f2_icache_all_resp_reg || f2_icache_all_resp_wire

  icacheMissBubble := io.fromICache.topdown.icacheMiss
  itlbMissBubble   := io.fromICache.topdown.itlbMiss

  io.toICache.stall := !f3_ready

  when(f2_flush)(f2_icache_all_resp_reg := false.B)
    .elsewhen(f2_valid && f2_icache_all_resp_wire && !f3_ready)(f2_icache_all_resp_reg := true.B)
    .elsewhen(f2_fire && f2_icache_all_resp_reg)(f2_icache_all_resp_reg := false.B)

  when(f2_flush)(f2_valid := false.B)
    .elsewhen(f1_fire && !f1_flush)(f2_valid := true.B)
    .elsewhen(f2_fire)(f2_valid := false.B)

  val f2_exception_in     = fromICache.bits.exception
  val f2_backendException = fromICache.bits.isBackendException
  // paddr and gpaddr of [startAddr, nextLineAddr]
  val f2_paddrs            = fromICache.bits.pAddr
  val f2_gpaddr            = fromICache.bits.gpAddr
  val f2_isForVSnonLeafPTE = fromICache.bits.isForVSnonLeafPTE

  // FIXME: raise af if one fetch block crosses the cacheable-noncacheable boundary, might not correct
  val f2_mmio_mismatch_exception = VecInit(Seq(
    ExceptionType.none, // mark the exception only on the second line
    Mux(
      // not double-line, skip check
      !fromICache.bits.doubleline || (
        // is double-line, ask for consistent pmp_mmio and itlb_pbmt value
        fromICache.bits.pmpMmio(0) === fromICache.bits.pmpMmio(1) &&
          fromICache.bits.itlbPbmt(0) === fromICache.bits.itlbPbmt(1)
      ),
      ExceptionType.none,
      ExceptionType.af
    )
  ))

  // merge exceptions
  val f2_exception = ExceptionType.merge(f2_exception_in, f2_mmio_mismatch_exception)

  // we need only the first port, as the second is asked to be the same
  val f2_pmp_mmio  = fromICache.bits.pmpMmio(0)
  val f2_itlb_pbmt = fromICache.bits.itlbPbmt(0)

  /**
    * reduce the number of registers, origin code
    * f2_pc = RegEnable(f1_pc, f1_fire)
    */
  val f2_pc_lower_result = RegEnable(f1_pc_lower_result, f1_fire)
  val f2_pc_high         = RegEnable(f1_pc_high, f1_fire)
  val f2_pc_high_plus1   = RegEnable(f1_pc_high_plus1, f1_fire)
  val f2_pc              = catPC(f2_pc_lower_result, f2_pc_high, f2_pc_high_plus1)

  val f2_cut_ptr      = RegEnable(f1_cut_ptr, f1_fire)
  val f2_resend_vaddr = RegEnable(f1_ftq_req.startAddr + 2.U, f1_fire)

  val f2_foldpc = VecInit(f2_pc.map(i => XORFold(i(VAddrBits - 1, 1), MemPredPCWidth)))
  val f2_jump_range =
    Fill(PredictWidth, !f2_ftq_req.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~f2_ftq_req.ftqOffset.bits
  require(
    isPow2(PredictWidth),
    "If PredictWidth does not satisfy the power of 2," +
      "expression: Fill(PredictWidth, 1.U(1.W)) >> ~f2_ftq_req.ftqOffset.bits is not right !!"
  )
  val f2_ftr_range = Fill(PredictWidth, f2_ftq_req.ftqOffset.valid) | Fill(PredictWidth, 1.U(1.W)) >> ~getBasicBlockIdx(
    f2_ftq_req.nextStartAddr,
    f2_ftq_req.startAddr
  )
  val f2_instr_range = f2_jump_range & f2_ftr_range
  val f2_exception_vec = VecInit((0 until PredictWidth).map(i =>
    MuxCase(
      ExceptionType.none,
      Seq(
        !isNextLine(f2_pc(i), f2_ftq_req.startAddr)                   -> f2_exception(0),
        (isNextLine(f2_pc(i), f2_ftq_req.startAddr) && f2_doubleLine) -> f2_exception(1)
      )
    )
  ))
  val f2_perf_info = io.fromICache.perf

  def cut(cacheline: UInt, cutPtr: Vec[UInt]): Vec[UInt] = {
    // FIXME: !HasCExtension
    require(HasCExtension)
    // if(HasCExtension){
    val result  = Wire(Vec(PredictWidth + 1, UInt(16.W)))
    val dataVec = cacheline.asTypeOf(Vec(blockBytes, UInt(16.W))) // 32 16-bit data vector
    (0 until PredictWidth + 1).foreach(i =>
      result(i) := dataVec(cutPtr(i)) // the max ptr is 3*blockBytes/4-1
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

  /* NOTE: the following `Cat(_data, _data)` *is* intentional.
   * Explanation:
   * In the old design, IFU is responsible for selecting requested data from two adjacent cachelines,
   *    so IFU has to receive 2*64B (2cacheline * 64B) data from ICache, and do `Cat(_data(1), _data(0))` here.
   * However, a fetch block is 34B at max, sending 2*64B is quiet a waste of power.
   * In current design (2024.06~), ICacheDataArray is responsible for selecting data from two adjacent cachelines,
   *    so IFU only need to receive 40B (5bank * 8B) valid data, and use only one port is enough.
   * For example, when pc falls on the 6th bank in cacheline0(so this is a doubleline request):
   *                                MSB                                         LSB
   *                  cacheline 1 || 1-7 | 1-6 | 1-5 | 1-4 | 1-3 | 1-2 | 1-1 | 1-0 ||
   *                  cacheline 0 || 0-7 | 0-6 | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | 0-0 ||
   *    and ICacheDataArray will respond:
   *         fromICache.bits.data || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *    therefore simply make a copy of the response and `Cat` together, and obtain the requested data from centre:
   *          f2_data_2_cacheline || 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 | 0-7 | 0-6 | xxx | xxx | xxx | 1-2 | 1-1 | 1-0 ||
   *                                             requested data: ^-----------------------------^
   * For another example, pc falls on the 1st bank in cacheline 0, we have:
   *         fromICache.bits.data || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *          f2_data_2_cacheline || xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx | xxx | xxx | 0-5 | 0-4 | 0-3 | 0-2 | 0-1 | xxx ||
   *                                                                           requested data: ^-----------------------------^
   * Each "| x-y |" block is a 8B bank from cacheline(x).bank(y)
   * Please also refer to:
   * - DataArray selects data:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L355-L381
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICache.scala#L149-L161
   * - ICache respond to IFU:
   * https://github.com/OpenXiangShan/XiangShan/blob/d4078d6edbfb4611ba58c8b0d1d8236c9115dbfc/src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala#L473
   */
  val f2_data_2_cacheline = Cat(fromICache.bits.data, fromICache.bits.data)

  val f2_cut_data = cut(f2_data_2_cacheline, f2_cut_ptr)

  // PreDecode: delimitation, does not expand RVC
  preDecoderIn.valid     := f2_valid
  preDecoderIn.bits.data := f2_cut_data
  preDecoderIn.bits.pc   := f2_pc

  val f2_instr       = preDecoderOut.instr
  val f2_pd          = preDecoderOut.pd
  val f2_jump_offset = preDecoderOut.jumpOffset
  val f2_altValid    = preDecoderOut.altValid
  /* if there is a cross-page RVI instruction, and the former page has no exception,
   * whether it has exception is actually depends on the latter page
   */
  val f2_crossPage_exception_vec = VecInit((0 until PredictWidth).map { i =>
    Mux(
      isLastInLine(f2_pc(i)) && !f2_pd(i).isRVC && f2_doubleLine && !ExceptionType.hasException(f2_exception(0)),
      f2_exception(1),
      ExceptionType.none
    )
  })
  XSPerfAccumulate("fetch_bubble_icache_not_resp", f2_valid && !icacheRespAllValid)

  /* *****************************************************************************
   * IFU Stage 3
   * - handle MMIO instruciton
   *   - send request to Uncache fetch Unit
   *   - every packet include 1 MMIO instruction
   *   - MMIO instructions will stop fetch pipeline until commiting from RoB
   *   - flush to snpc (send ifu_redirect to Ftq)
   * - Ibuffer enqueue
   * - check predict result in Frontend (jalFault/retFault/notCFIFault/invalidTakenFault/targetFault)
   * - handle last half RVI instruction
   * ***************************************************************************** */

  val f3_valid      = RegInit(false.B)
  val f3_ftq_req    = RegEnable(f2_ftq_req, f2_fire)
  val f3_doubleLine = RegEnable(f2_doubleLine, f2_fire)
  val f3_fire       = io.toIBuffer.fire

  val f3_cut_data = RegEnable(f2_cut_data, f2_fire)

  val f3_exception        = RegEnable(f2_exception, f2_fire)
  val f3_pmp_mmio         = RegEnable(f2_pmp_mmio, f2_fire)
  val f3_itlb_pbmt        = RegEnable(f2_itlb_pbmt, f2_fire)
  val f3_backendException = RegEnable(f2_backendException, f2_fire)

  val f3_instr = RegEnable(f2_instr, f2_fire)

  rvcExpanders.zipWithIndex.foreach { case (expander, i) =>
    expander.io.in      := f3_instr(i)
    expander.io.fsIsOff := io.csrFsIsOff
  }
  // Use expanded instruction only when input is legal.
  // Otherwise use origin illegal RVC instruction.
  val f3_expd_instr = VecInit(rvcExpanders.map { expander: RvcExpander =>
    Mux(expander.io.ill, expander.io.in, expander.io.out.bits)
  })
  val f3_ill = VecInit(rvcExpanders.map(_.io.ill))

  val f3_pd_wire                 = RegEnable(f2_pd, f2_fire)
  val f3_pd                      = WireInit(f3_pd_wire)
  val f3_jump_offset             = RegEnable(f2_jump_offset, f2_fire)
  val f3_exception_vec           = RegEnable(f2_exception_vec, f2_fire)
  val f3_crossPage_exception_vec = RegEnable(f2_crossPage_exception_vec, f2_fire)

  val f3_pc_lower_result = RegEnable(f2_pc_lower_result, f2_fire)
  val f3_pc_high         = RegEnable(f2_pc_high, f2_fire)
  val f3_pc_high_plus1   = RegEnable(f2_pc_high_plus1, f2_fire)
  val f3_pc              = catPC(f3_pc_lower_result, f3_pc_high, f3_pc_high_plus1)

  val f3_pc_last_lower_result_plus2 = RegEnable(f2_pc_lower_result(PredictWidth - 1) + 2.U, f2_fire)
  val f3_pc_last_lower_result_plus4 = RegEnable(f2_pc_lower_result(PredictWidth - 1) + 4.U, f2_fire)

  /**
    ***********************************************************************
    * Half snpc(i) is larger than pc(i) by 4. Using pc to calculate half snpc may be a good choice.
    ***********************************************************************
    */
  val f3_half_snpc = Wire(Vec(PredictWidth, PrunedAddr(VAddrBits)))
  for (i <- 0 until PredictWidth) {
    if (i == (PredictWidth - 2)) {
      f3_half_snpc(i) := catPC(f3_pc_last_lower_result_plus2, f3_pc_high, f3_pc_high_plus1)
    } else if (i == (PredictWidth - 1)) {
      f3_half_snpc(i) := catPC(f3_pc_last_lower_result_plus4, f3_pc_high, f3_pc_high_plus1)
    } else {
      f3_half_snpc(i) := f3_pc(i + 2)
    }
  }

  val f3_instr_range       = RegEnable(f2_instr_range, f2_fire)
  val f3_foldpc            = RegEnable(f2_foldpc, f2_fire)
  val f3_altValid          = RegEnable(f2_altValid, f2_fire)
  val f3_paddrs            = RegEnable(f2_paddrs, f2_fire)
  val f3_gpaddr            = RegEnable(f2_gpaddr, f2_fire)
  val f3_isForVSnonLeafPTE = RegEnable(f2_isForVSnonLeafPTE, f2_fire)
  val f3_resend_vaddr      = RegEnable(f2_resend_vaddr, f2_fire)

  // Expand 1 bit to prevent overflow when assert
  val f3_ftq_req_startAddr     = PrunedAddrInit(Cat(0.U(1.W), f3_ftq_req.startAddr.toUInt))
  val f3_ftq_req_nextStartAddr = PrunedAddrInit(Cat(0.U(1.W), f3_ftq_req.nextStartAddr.toUInt))
  // brType, isCall and isRet generation is delayed to f3 stage

  f3PreDecoder.io.instr := f3_instr

  f3_pd.zipWithIndex.foreach { case (pd, i) =>
    pd.brType := f3PreDecoder.io.pd(i).brType
    pd.isCall := f3PreDecoder.io.pd(i).isCall
    pd.isRet  := f3PreDecoder.io.pd(i).isRet
  }

  val f3PdDiff = f3_pd_wire.zip(f3_pd).map { case (a, b) => a.asUInt =/= b.asUInt }.reduce(_ || _)
  XSError(f3_valid && f3PdDiff, "f3 pd diff")

  when(f3_valid && !f3_ftq_req.ftqOffset.valid) {
    assert(
      f3_ftq_req_startAddr + (2 * PredictWidth).U >= f3_ftq_req_nextStartAddr,
      s"More tha ${2 * PredictWidth} Bytes fetch is not allowed!"
    )
  }

  /*** MMIO State Machine***/
  val f3_mmio_data     = Reg(Vec(2, UInt(16.W)))
  val mmio_exception   = RegInit(0.U(ExceptionType.width.W))
  val mmio_is_RVC      = RegInit(false.B)
  val mmio_has_resend  = RegInit(false.B)
  val mmio_resend_addr = RegInit(PrunedAddrInit(0.U(PAddrBits.W)))
  // NOTE: we dont use GPAddrBits here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  val mmio_resend_gpaddr            = RegInit(PrunedAddrInit(0.U(PAddrBitsMax.W)))
  val mmio_resend_isForVSnonLeafPTE = RegInit(false.B)

  // last instuction finish
  val is_first_instr = RegInit(true.B)

  /*** Determine whether the MMIO instruction is executable based on the previous prediction block ***/
  io.mmioCommitRead.mmioFtqPtr := RegNext(f3_ftq_req.ftqIdx - 1.U)

  val m_idle :: m_waitLastCmt :: m_sendReq :: m_waitResp :: m_sendTLB :: m_tlbResp :: m_sendPMP :: m_resendReq :: m_waitResendResp :: m_waitCommit :: m_commited :: Nil =
    Enum(11)
  val mmio_state = RegInit(m_idle)

  // do mmio fetch only when pmp/pbmt shows it is a uncacheable address and no exception occurs
  /* FIXME: we do not distinguish pbmt is NC or IO now
   *        but we actually can do speculative execution if pbmt is NC, maybe fix this later for performance
   */
  val f3_req_is_mmio =
    f3_valid && (f3_pmp_mmio || Pbmt.isUncache(f3_itlb_pbmt)) && !ExceptionType.hasException(f3_exception)
  val mmio_commit = VecInit(io.robCommits.map { commit =>
    commit.valid && commit.bits.ftqIdx === f3_ftq_req.ftqIdx && commit.bits.ftqOffset === 0.U
  }).asUInt.orR
  val f3_mmio_req_commit = f3_req_is_mmio && mmio_state === m_commited

  val f3_mmio_to_commit      = f3_req_is_mmio && mmio_state === m_waitCommit
  val f3_mmio_to_commit_next = RegNext(f3_mmio_to_commit)
  val f3_mmio_can_go         = f3_mmio_to_commit && !f3_mmio_to_commit_next

  val fromFtqRedirectReg = Wire(fromFtq.redirect.cloneType)
  fromFtqRedirectReg.bits := RegEnable(
    fromFtq.redirect.bits,
    0.U.asTypeOf(fromFtq.redirect.bits),
    fromFtq.redirect.valid
  )
  fromFtqRedirectReg.valid := RegNext(fromFtq.redirect.valid, init = false.B)
  val mmioF3Flush           = RegNext(f3_flush, init = false.B)
  val f3_ftq_flush_self     = fromFtqRedirectReg.valid && RedirectLevel.flushItself(fromFtqRedirectReg.bits.level)
  val f3_ftq_flush_by_older = fromFtqRedirectReg.valid && isBefore(fromFtqRedirectReg.bits.ftqIdx, f3_ftq_req.ftqIdx)

  val f3_need_not_flush = f3_req_is_mmio && fromFtqRedirectReg.valid && !f3_ftq_flush_self && !f3_ftq_flush_by_older

  /**
    **********************************************************************************
    * We want to defer instruction fetching when encountering MMIO instructions to ensure that the MMIO region is not negatively impacted.
    * This is the exception when the first instruction is an MMIO instruction.
    **********************************************************************************
    */
  when(is_first_instr && f3_fire) {
    is_first_instr := false.B
  }

  when(f3_flush && !f3_req_is_mmio)(f3_valid := false.B)
    .elsewhen(mmioF3Flush && f3_req_is_mmio && !f3_need_not_flush)(f3_valid := false.B)
    .elsewhen(f2_fire && !f2_flush)(f3_valid := true.B)
    .elsewhen(io.toIBuffer.fire && !f3_req_is_mmio)(f3_valid := false.B)
    .elsewhen(f3_req_is_mmio && f3_mmio_req_commit)(f3_valid := false.B)

  val f3_mmio_use_seq_pc = RegInit(false.B)

  val (redirect_ftqIdx, redirect_ftqOffset) = (fromFtqRedirectReg.bits.ftqIdx, fromFtqRedirectReg.bits.ftqOffset)
  val redirect_mmio_req =
    fromFtqRedirectReg.valid && redirect_ftqIdx === f3_ftq_req.ftqIdx && redirect_ftqOffset === 0.U

  when(RegNext(f2_fire && !f2_flush) && f3_req_is_mmio)(f3_mmio_use_seq_pc := true.B)
    .elsewhen(redirect_mmio_req)(f3_mmio_use_seq_pc := false.B)

  f3_ready := (io.toIBuffer.ready && (f3_mmio_req_commit || !f3_req_is_mmio)) || !f3_valid

  // mmio state machine
  switch(mmio_state) {
    is(m_idle) {
      when(f3_req_is_mmio) {
        // in idempotent spaces, we can send request directly (i.e. can do speculative fetch)
        mmio_state := Mux(f3_itlb_pbmt === Pbmt.nc, m_sendReq, m_waitLastCmt)
      }
    }

    is(m_waitLastCmt) {
      when(is_first_instr) {
        mmio_state := m_sendReq
      }.otherwise {
        mmio_state := Mux(io.mmioCommitRead.mmioLastCommit, m_sendReq, m_waitLastCmt)
      }
    }

    is(m_sendReq) {
      mmio_state := Mux(toUncache.fire, m_waitResp, m_sendReq)
    }

    is(m_waitResp) {
      when(fromUncache.fire) {
        val isRVC      = fromUncache.bits.data(1, 0) =/= 3.U
        val exception  = ExceptionType.fromTilelink(fromUncache.bits.corrupt)
        val needResend = !isRVC && f3_paddrs(0)(2, 1) === 3.U && !ExceptionType.hasException(exception)
        mmio_state      := Mux(needResend, m_sendTLB, m_waitCommit)
        mmio_exception  := exception
        mmio_is_RVC     := isRVC
        mmio_has_resend := needResend
        f3_mmio_data(0) := fromUncache.bits.data(15, 0)
        f3_mmio_data(1) := fromUncache.bits.data(31, 16)
      }
    }

    is(m_sendTLB) {
      mmio_state := Mux(io.itlb.req.fire, m_tlbResp, m_sendTLB)
    }

    is(m_tlbResp) {
      when(io.itlb.resp.fire) {
        // we are using a blocked tlb, so resp.fire must have !resp.bits.miss
        assert(!io.itlb.resp.bits.miss, "blocked mode iTLB miss when resp.fire")
        val tlb_exception = ExceptionType.fromTlbResp(io.itlb.resp.bits)
        // if itlb re-check respond pbmt mismatch with previous check, must be access fault
        val pbmt_mismatch_exception = Mux(
          io.itlb.resp.bits.pbmt(0) =/= f3_itlb_pbmt,
          ExceptionType.af,
          ExceptionType.none
        )
        val exception = ExceptionType.merge(tlb_exception, pbmt_mismatch_exception)
        // if tlb has exception, abort checking pmp, just send instr & exception to ibuffer and wait for commit
        mmio_state := Mux(ExceptionType.hasException(exception), m_waitCommit, m_sendPMP)
        // also save itlb response
        mmio_exception                := exception
        mmio_resend_addr              := io.itlb.resp.bits.paddr(0)
        mmio_resend_gpaddr            := io.itlb.resp.bits.gpaddr(0)(PAddrBitsMax - 1, 0)
        mmio_resend_isForVSnonLeafPTE := io.itlb.resp.bits.isForVSnonLeafPTE(0)
      }
    }

    is(m_sendPMP) {
      val pmp_exception = ExceptionType.fromPMPResp(io.pmp.resp)
      // if pmp re-check respond mismatch with previous check, must be access fault
      val mmio_mismatch_exception = Mux(
        io.pmp.resp.mmio =/= f3_pmp_mmio,
        ExceptionType.af,
        ExceptionType.none
      )
      val exception = ExceptionType.merge(pmp_exception, mmio_mismatch_exception)
      // if pmp has exception, abort sending request, just send instr & exception to ibuffer and wait for commit
      mmio_state := Mux(ExceptionType.hasException(exception), m_waitCommit, m_resendReq)
      // also save pmp response
      mmio_exception := exception
    }

    is(m_resendReq) {
      mmio_state := Mux(toUncache.fire, m_waitResendResp, m_resendReq)
    }

    is(m_waitResendResp) {
      when(fromUncache.fire) {
        mmio_state      := m_waitCommit
        mmio_exception  := ExceptionType.fromTilelink(fromUncache.bits.corrupt)
        f3_mmio_data(1) := fromUncache.bits.data(15, 0)
      }
    }

    is(m_waitCommit) {
      // in idempotent spaces, we can skip waiting for commit (i.e. can do speculative fetch)
      // but we do not skip m_waitCommit state, as other signals (e.g. f3_mmio_can_go relies on this)
      mmio_state := Mux(mmio_commit || f3_itlb_pbmt === Pbmt.nc, m_commited, m_waitCommit)
    }

    // normal mmio instruction
    is(m_commited) {
      mmio_state                    := m_idle
      mmio_exception                := ExceptionType.none
      mmio_is_RVC                   := false.B
      mmio_has_resend               := false.B
      mmio_resend_addr              := PrunedAddrInit(0.U(PAddrBits.W))
      mmio_resend_gpaddr            := PrunedAddrInit(0.U(PAddrBitsMax.W))
      mmio_resend_isForVSnonLeafPTE := false.B
    }
  }

  // Exception or flush by older branch prediction
  // Condition is from RegNext(fromFtq.redirect), 1 cycle after backend rediect
  when(f3_ftq_flush_self || f3_ftq_flush_by_older) {
    mmio_state                    := m_idle
    mmio_exception                := ExceptionType.none
    mmio_is_RVC                   := false.B
    mmio_has_resend               := false.B
    mmio_resend_addr              := PrunedAddrInit(0.U(PAddrBits.W))
    mmio_resend_gpaddr            := PrunedAddrInit(0.U(PAddrBitsMax.W))
    mmio_resend_isForVSnonLeafPTE := false.B
    f3_mmio_data.map(_ := 0.U)
  }

  toUncache.valid     := ((mmio_state === m_sendReq) || (mmio_state === m_resendReq)) && f3_req_is_mmio
  toUncache.bits.addr := Mux(mmio_state === m_resendReq, mmio_resend_addr, f3_paddrs(0))
  fromUncache.ready   := true.B

  // send itlb request in m_sendTLB state
  io.itlb.req.valid                   := (mmio_state === m_sendTLB) && f3_req_is_mmio
  io.itlb.req.bits.size               := 3.U
  io.itlb.req.bits.vaddr              := f3_resend_vaddr.toUInt
  io.itlb.req.bits.debug.pc           := f3_resend_vaddr.toUInt
  io.itlb.req.bits.cmd                := TlbCmd.exec
  io.itlb.req.bits.isPrefetch         := false.B
  io.itlb.req.bits.kill               := false.B // IFU use itlb for mmio, doesn't need sync, set it to false
  io.itlb.req.bits.no_translate       := false.B
  io.itlb.req.bits.fullva             := 0.U
  io.itlb.req.bits.checkfullva        := false.B
  io.itlb.req.bits.hyperinst          := DontCare
  io.itlb.req.bits.hlvx               := DontCare
  io.itlb.req.bits.memidx             := DontCare
  io.itlb.req.bits.debug.robIdx       := DontCare
  io.itlb.req.bits.debug.isFirstIssue := DontCare
  io.itlb.req.bits.pmp_addr           := DontCare
  // whats the difference between req_kill and req.bits.kill?
  io.itlb.req_kill := false.B
  // wait for itlb response in m_tlbResp state
  io.itlb.resp.ready := (mmio_state === m_tlbResp) && f3_req_is_mmio

  io.pmp.req.valid     := (mmio_state === m_sendPMP) && f3_req_is_mmio
  io.pmp.req.bits.addr := mmio_resend_addr.toUInt
  io.pmp.req.bits.size := 3.U
  io.pmp.req.bits.cmd  := TlbCmd.exec

  val f3_lastHalf = RegInit(0.U.asTypeOf(new LastHalfEntry))

  val f3_predecode_range = VecInit(preDecoderOut.pd.map(inst => inst.valid)).asUInt
  val f3_mmio_range      = VecInit((0 until PredictWidth).map(i => if (i == 0) true.B else false.B))
  val f3_instr_valid     = Wire(Vec(PredictWidth, Bool()))

  /*** prediction result check   ***/
  checkerIn.bits.ftqOffset  := f3_ftq_req.ftqOffset
  checkerIn.bits.jumpOffset := f3_jump_offset
  checkerIn.bits.target     := f3_ftq_req.nextStartAddr
  checkerIn.bits.instrRange := f3_instr_range.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.bits.instrValid := f3_instr_valid.asTypeOf(Vec(PredictWidth, Bool()))
  checkerIn.bits.pds        := f3_pd
  checkerIn.bits.pc         := f3_pc
  checkerIn.valid           := RegNext(f2_fire, init = false.B)

  /* ** handle half RVI in the last 2 Bytes ** */
  private def hasLastHalf(idx: UInt): Bool =
    !f3_pd(idx).isRVC && checkerOutStage1.fixedRange(idx) && f3_instr_valid(idx) &&
      !checkerOutStage1.fixedTaken(idx) && !f3_req_is_mmio

  val f3_last_validIdx = ParallelPosteriorityEncoder(checkerOutStage1.fixedRange)

  val f3_hasLastHalf    = hasLastHalf((PredictWidth - 1).U)
  val f3_false_lastHalf = hasLastHalf(f3_last_validIdx)
  val f3_false_snpc     = f3_half_snpc(f3_last_validIdx)

  val f3_lastHalf_mask    = VecInit((0 until PredictWidth).map(i => if (i == 0) false.B else true.B)).asUInt
  val f3_lastHalf_disable = RegInit(false.B)

  when(f3_flush || (f3_fire && f3_lastHalf_disable)) {
    f3_lastHalf_disable := false.B
  }

  when(f3_flush) {
    f3_lastHalf.valid := false.B
  }.elsewhen(f3_fire) {
    f3_lastHalf.valid    := f3_hasLastHalf && !f3_lastHalf_disable
    f3_lastHalf.middlePC := f3_ftq_req.nextStartAddr
  }

  f3_instr_valid := Mux(f3_lastHalf.valid, f3_altValid, VecInit(f3_pd.map(inst => inst.valid)))

  /*** frontend Trigger  ***/
  frontendTrigger.io.pds  := f3_pd
  frontendTrigger.io.pc   := f3_pc
  frontendTrigger.io.data := f3_cut_data

  frontendTrigger.io.frontendTrigger := io.frontendTrigger

  val f3_triggered       = frontendTrigger.io.triggered
  val f3_toIbuffer_valid = f3_valid && (!f3_req_is_mmio || f3_mmio_can_go) && !f3_flush

  /*** send to Ibuffer  ***/
  io.toIBuffer.valid          := f3_toIbuffer_valid
  io.toIBuffer.bits.instrs    := f3_expd_instr
  io.toIBuffer.bits.valid     := f3_instr_valid.asUInt
  io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedRange.asUInt & f3_instr_valid.asUInt
  io.toIBuffer.bits.pd        := f3_pd
  io.toIBuffer.bits.ftqPtr    := f3_ftq_req.ftqIdx
  io.toIBuffer.bits.pc        := f3_pc
  // Find last using PriorityMux
  io.toIBuffer.bits.isLastInFtqEntry := Reverse(PriorityEncoderOH(Reverse(io.toIBuffer.bits.enqEnable))).asBools
  io.toIBuffer.bits.ftqOffset.zipWithIndex.map { case (a, i) =>
    a.bits := i.U; a.valid := checkerOutStage1.fixedTaken(i) && !f3_req_is_mmio
  }
  io.toIBuffer.bits.foldpc        := f3_foldpc
  io.toIBuffer.bits.exceptionType := ExceptionType.merge(f3_exception_vec, f3_crossPage_exception_vec)
  // backendException only needs to be set for the first instruction.
  // Other instructions in the same block may have pf or af set,
  // which is a side effect of the first instruction and actually not necessary.
  io.toIBuffer.bits.backendException := (0 until PredictWidth).map {
    case 0 => f3_backendException
    case _ => false.B
  }
  io.toIBuffer.bits.crossPageIPFFix := f3_crossPage_exception_vec.map(ExceptionType.hasException)
  io.toIBuffer.bits.illegalInstr    := f3_ill
  io.toIBuffer.bits.triggered       := f3_triggered

  when(f3_lastHalf.valid) {
    io.toIBuffer.bits.enqEnable := checkerOutStage1.fixedRange.asUInt & f3_instr_valid.asUInt & f3_lastHalf_mask
    io.toIBuffer.bits.valid     := f3_lastHalf_mask & f3_instr_valid.asUInt
  }

  /** to backend */
  // f3_gpaddr is valid iff gpf is detected
  io.toBackend.gpaddrMem_wen := f3_toIbuffer_valid && Mux(
    f3_req_is_mmio,
    mmio_exception === ExceptionType.gpf,
    f3_exception.map(_ === ExceptionType.gpf).reduce(_ || _)
  )
  io.toBackend.gpaddrMem_waddr        := f3_ftq_req.ftqIdx.value
  io.toBackend.gpaddrMem_wdata.gpaddr := Mux(f3_req_is_mmio, mmio_resend_gpaddr.toUInt, f3_gpaddr.toUInt)
  io.toBackend.gpaddrMem_wdata.isForVSnonLeafPTE := Mux(
    f3_req_is_mmio,
    mmio_resend_isForVSnonLeafPTE,
    f3_isForVSnonLeafPTE
  )

  // Write back to Ftq
  val f3_cache_fetch     = f3_valid && !(f2_fire && !f2_flush)
  val finishFetchMaskReg = RegNext(f3_cache_fetch)

  val mmioFlushWb        = Wire(Valid(new PredecodeWritebackBundle))
  val f3_mmio_missOffset = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  f3_mmio_missOffset.valid := f3_req_is_mmio
  f3_mmio_missOffset.bits  := 0.U

  // Send mmioFlushWb back to FTQ 1 cycle after uncache fetch return
  // When backend redirect, mmio_state reset after 1 cycle.
  // In this case, mask .valid to avoid overriding backend redirect
  mmioFlushWb.valid := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire) &&
    f3_mmio_use_seq_pc && !f3_ftq_flush_self && !f3_ftq_flush_by_older)
  mmioFlushWb.bits.pc := f3_pc
  mmioFlushWb.bits.pd := f3_pd
  mmioFlushWb.bits.pd.zipWithIndex.map { case (instr, i) => instr.valid := f3_mmio_range(i) }
  mmioFlushWb.bits.ftqIdx     := f3_ftq_req.ftqIdx
  mmioFlushWb.bits.ftqOffset  := f3_ftq_req.ftqOffset.bits
  mmioFlushWb.bits.misOffset  := f3_mmio_missOffset
  mmioFlushWb.bits.cfiOffset  := DontCare
  mmioFlushWb.bits.target     := Mux(mmio_is_RVC, f3_ftq_req.startAddr + 2.U, f3_ftq_req.startAddr + 4.U)
  mmioFlushWb.bits.jalTarget  := DontCare
  mmioFlushWb.bits.instrRange := f3_mmio_range

  mmioRvcExpander.io.in      := Mux(f3_req_is_mmio, Cat(f3_mmio_data(1), f3_mmio_data(0)), 0.U)
  mmioRvcExpander.io.fsIsOff := io.csrFsIsOff

  /** external predecode for MMIO instruction */
  when(f3_req_is_mmio) {
    val inst = Cat(f3_mmio_data(1), f3_mmio_data(0))

    val (brType, isCall, isRet) = getBrInfo(inst)
    val jalOffset               = getJalOffset(inst, mmio_is_RVC)
    val brOffset                = getBrOffset(inst, mmio_is_RVC)

    io.toIBuffer.bits.instrs(0) := Mux(mmioRvcExpander.io.ill, mmioRvcExpander.io.in, mmioRvcExpander.io.out.bits)

    io.toIBuffer.bits.pd(0).valid  := true.B
    io.toIBuffer.bits.pd(0).isRVC  := mmio_is_RVC
    io.toIBuffer.bits.pd(0).brType := brType
    io.toIBuffer.bits.pd(0).isCall := isCall
    io.toIBuffer.bits.pd(0).isRet  := isRet

    io.toIBuffer.bits.exceptionType(0) := mmio_exception
    // exception can happens in next page only when resend
    io.toIBuffer.bits.crossPageIPFFix(0) := mmio_has_resend && ExceptionType.hasException(mmio_exception)
    io.toIBuffer.bits.illegalInstr(0)    := mmioRvcExpander.io.ill

    io.toIBuffer.bits.enqEnable := f3_mmio_range.asUInt

    mmioFlushWb.bits.pd(0).valid  := true.B
    mmioFlushWb.bits.pd(0).isRVC  := mmio_is_RVC
    mmioFlushWb.bits.pd(0).brType := brType
    mmioFlushWb.bits.pd(0).isCall := isCall
    mmioFlushWb.bits.pd(0).isRet  := isRet
  }

  mmio_redirect := (f3_req_is_mmio && mmio_state === m_waitCommit && RegNext(fromUncache.fire) && f3_mmio_use_seq_pc)

  XSPerfAccumulate("fetch_bubble_ibuffer_not_ready", io.toIBuffer.valid && !io.toIBuffer.ready)

  /* *****************************************************************************
   * IFU Write Back Stage
   * - write back predecode information to Ftq to update
   * - redirect if found fault prediction
   * - redirect if has false hit last half (last PC is not start + 32 Bytes, but in the midle of an notCFI RVI instruction)
   * ***************************************************************************** */
  val wb_enable  = RegNext(f2_fire && !f2_flush) && !f3_req_is_mmio && !f3_flush
  val wb_valid   = RegNext(wb_enable, init = false.B)
  val wb_ftq_req = RegEnable(f3_ftq_req, wb_enable)

  private val wb_check_result_stage1 = RegEnable(checkerOutStage1, wb_enable)
  private val wb_check_result_stage2 = checkerOutStage2
  val wb_instr_range                 = RegEnable(io.toIBuffer.bits.enqEnable, wb_enable)

  val wb_pc_lower_result = RegEnable(f3_pc_lower_result, wb_enable)
  val wb_pc_high         = RegEnable(f3_pc_high, wb_enable)
  val wb_pc_high_plus1   = RegEnable(f3_pc_high_plus1, wb_enable)
  val wb_pc              = catPC(wb_pc_lower_result, wb_pc_high, wb_pc_high_plus1)

  // val wb_pc             = RegEnable(f3_pc, wb_enable)
  val wb_pd          = RegEnable(f3_pd, wb_enable)
  val wb_instr_valid = RegEnable(f3_instr_valid, wb_enable)

  /* false hit lastHalf */
  val wb_lastIdx        = RegEnable(f3_last_validIdx, wb_enable)
  val wb_false_lastHalf = RegEnable(f3_false_lastHalf, wb_enable) && wb_lastIdx =/= (PredictWidth - 1).U
  val wb_false_target   = RegEnable(f3_false_snpc, wb_enable)

  val wb_half_flush  = wb_false_lastHalf
  val wb_half_target = wb_false_target

  /* false oversize */
  val lastIsRVC = wb_instr_range.asTypeOf(Vec(PredictWidth, Bool())).last && wb_pd.last.isRVC
  val lastIsRVI = wb_instr_range.asTypeOf(Vec(PredictWidth, Bool()))(PredictWidth - 2) && !wb_pd(PredictWidth - 2).isRVC
  val lastTaken = wb_check_result_stage1.fixedTaken.last

  f3_wb_not_flush := wb_ftq_req.ftqIdx === f3_ftq_req.ftqIdx && f3_valid && wb_valid

  /** if a req with a last half but miss predicted enters in wb stage, and this cycle f3 stalls,
    * we set a flag to notify f3 that the last half flag need not to be set.
    */
  // f3_fire is after wb_valid
  when(wb_valid && RegNext(f3_hasLastHalf, init = false.B)
    && wb_check_result_stage2.fixedMissPred(PredictWidth - 1) && !f3_fire && !RegNext(
      f3_fire,
      init = false.B
    ) && !f3_flush) {
    f3_lastHalf_disable := true.B
  }

  // wb_valid and f3_fire are in same cycle
  when(wb_valid && RegNext(f3_hasLastHalf, init = false.B)
    && wb_check_result_stage2.fixedMissPred(PredictWidth - 1) && f3_fire) {
    f3_lastHalf.valid := false.B
  }

  val checkFlushWb = Wire(Valid(new PredecodeWritebackBundle))
  val checkFlushWbjalTargetIdx = ParallelPriorityEncoder(VecInit(wb_pd.zip(wb_instr_valid).map { case (pd, v) =>
    v && pd.isJal
  }))
  val checkFlushWbTargetIdx = ParallelPriorityEncoder(wb_check_result_stage2.fixedMissPred)
  checkFlushWb.valid   := wb_valid
  checkFlushWb.bits.pc := wb_pc
  checkFlushWb.bits.pd := wb_pd
  checkFlushWb.bits.pd.zipWithIndex.map { case (instr, i) => instr.valid := wb_instr_valid(i) }
  checkFlushWb.bits.ftqIdx          := wb_ftq_req.ftqIdx
  checkFlushWb.bits.ftqOffset       := wb_ftq_req.ftqOffset.bits
  checkFlushWb.bits.misOffset.valid := ParallelOR(wb_check_result_stage2.fixedMissPred) || wb_half_flush
  checkFlushWb.bits.misOffset.bits := Mux(
    wb_half_flush,
    wb_lastIdx,
    ParallelPriorityEncoder(wb_check_result_stage2.fixedMissPred)
  )
  checkFlushWb.bits.cfiOffset.valid := ParallelOR(wb_check_result_stage1.fixedTaken)
  checkFlushWb.bits.cfiOffset.bits  := ParallelPriorityEncoder(wb_check_result_stage1.fixedTaken)
  checkFlushWb.bits.target := Mux(
    wb_half_flush,
    wb_half_target,
    wb_check_result_stage2.fixedTarget(checkFlushWbTargetIdx)
  )
  checkFlushWb.bits.jalTarget  := wb_check_result_stage2.jalTarget(checkFlushWbjalTargetIdx)
  checkFlushWb.bits.instrRange := wb_instr_range.asTypeOf(Vec(PredictWidth, Bool()))

  toFtq.pdWb := Mux(wb_valid, checkFlushWb, mmioFlushWb)

  wb_redirect := checkFlushWb.bits.misOffset.valid && wb_valid

  /* write back flush type */
  private val checkFaultType    = wb_check_result_stage2.faultType
  private val checkJalFault     = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.jalFault).reduce(_ || _)
  private val checkJalrFault    = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.jalrFault).reduce(_ || _)
  private val checkRetFault     = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.retFault).reduce(_ || _)
  private val checkTargetFault  = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.targetFault).reduce(_ || _)
  private val checkNotCFIFault  = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.notCfiFault).reduce(_ || _)
  private val checkInvalidTaken = wb_valid && checkFaultType.map(_ === PreDecodeFaultType.invalidTaken).reduce(_ || _)

  XSPerfAccumulate("predecode_flush_jalFault", checkJalFault)
  XSPerfAccumulate("predecode_flush_jalrFault", checkJalrFault)
  XSPerfAccumulate("predecode_flush_retFault", checkRetFault)
  XSPerfAccumulate("predecode_flush_targetFault", checkTargetFault)
  XSPerfAccumulate("predecode_flush_notCFIFault", checkNotCFIFault)
  XSPerfAccumulate("predecode_flush_incalidTakenFault", checkInvalidTaken)

  XSDebug(
    checkRetFault,
    "startAddr:%x  nextstartAddr:%x  taken:%d    takenIdx:%d\n",
    wb_ftq_req.startAddr.toUInt,
    wb_ftq_req.nextStartAddr.toUInt,
    wb_ftq_req.ftqOffset.valid,
    wb_ftq_req.ftqOffset.bits
  )

  /** performance counter */
  val f3_perf_info = RegEnable(f2_perf_info, f2_fire)
  val f3_req_0     = io.toIBuffer.fire
  val f3_req_1     = io.toIBuffer.fire && f3_doubleLine
  val f3_hit_0     = io.toIBuffer.fire && f3_perf_info.bankHit(0)
  val f3_hit_1     = io.toIBuffer.fire && f3_doubleLine & f3_perf_info.bankHit(1)
  val f3_hit       = f3_perf_info.hit
  val perfEvents = Seq(
    ("frontendFlush                ", wb_redirect),
    ("ifu_req                      ", io.toIBuffer.fire),
    ("ifu_miss                     ", io.toIBuffer.fire && !f3_perf_info.hit),
    ("ifu_req_cacheline_0          ", f3_req_0),
    ("ifu_req_cacheline_1          ", f3_req_1),
    ("ifu_req_cacheline_0_hit      ", f3_hit_1),
    ("ifu_req_cacheline_1_hit      ", f3_hit_1),
    ("only_0_hit                   ", f3_perf_info.only0Hit && io.toIBuffer.fire),
    ("only_0_miss                  ", f3_perf_info.only0Miss && io.toIBuffer.fire),
    ("hit_0_hit_1                  ", f3_perf_info.hit0Hit1 && io.toIBuffer.fire),
    ("hit_0_miss_1                 ", f3_perf_info.hit0Miss1 && io.toIBuffer.fire),
    ("miss_0_hit_1                 ", f3_perf_info.miss0Hit1 && io.toIBuffer.fire),
    ("miss_0_miss_1                ", f3_perf_info.miss0Miss1 && io.toIBuffer.fire)
  )
  generatePerfEvent()

  XSPerfAccumulate("ifu_req", io.toIBuffer.fire)
  XSPerfAccumulate("ifu_miss", io.toIBuffer.fire && !f3_hit)
  XSPerfAccumulate("ifu_req_cacheline_0", f3_req_0)
  XSPerfAccumulate("ifu_req_cacheline_1", f3_req_1)
  XSPerfAccumulate("ifu_req_cacheline_0_hit", f3_hit_0)
  XSPerfAccumulate("ifu_req_cacheline_1_hit", f3_hit_1)
  XSPerfAccumulate("frontendFlush", wb_redirect)
  XSPerfAccumulate("only_0_hit", f3_perf_info.only0Hit && io.toIBuffer.fire)
  XSPerfAccumulate("only_0_miss", f3_perf_info.only0Miss && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_hit_1", f3_perf_info.hit0Hit1 && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_miss_1", f3_perf_info.hit0Miss1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_hit_1", f3_perf_info.miss0Hit1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_miss_1", f3_perf_info.miss0Miss1 && io.toIBuffer.fire)
  XSPerfAccumulate("hit_0_except_1", f3_perf_info.hit0Except1 && io.toIBuffer.fire)
  XSPerfAccumulate("miss_0_except_1", f3_perf_info.miss0Except1 && io.toIBuffer.fire)
  XSPerfAccumulate("except_0", f3_perf_info.except0 && io.toIBuffer.fire)
  XSPerfHistogram(
    "ifu2ibuffer_validCnt",
    PopCount(io.toIBuffer.bits.valid & io.toIBuffer.bits.enqEnable),
    io.toIBuffer.fire,
    0,
    PredictWidth + 1,
    1
  )

  val hartId                     = p(XSCoreParamsKey).HartId
  val isWriteFetchToIBufferTable = Constantin.createRecord(s"isWriteFetchToIBufferTable$hartId")
  val isWriteIfuWbToFtqTable     = Constantin.createRecord(s"isWriteIfuWbToFtqTable$hartId")
  val fetchToIBufferTable        = ChiselDB.createTable(s"FetchToIBuffer$hartId", new FetchToIBufferDB)
  val ifuWbToFtqTable            = ChiselDB.createTable(s"IfuWbToFtq$hartId", new IfuWbToFtqDB)

  val fetchIBufferDumpData = Wire(new FetchToIBufferDB)
  fetchIBufferDumpData.start_addr  := f3_ftq_req.startAddr
  fetchIBufferDumpData.instr_count := PopCount(io.toIBuffer.bits.enqEnable)
  fetchIBufferDumpData.exception := (f3_perf_info.except0 && io.toIBuffer.fire) || (f3_perf_info.hit0Except1 && io.toIBuffer.fire) || (f3_perf_info.miss0Except1 && io.toIBuffer.fire)
  fetchIBufferDumpData.is_cache_hit := f3_hit

  val ifuWbToFtqDumpData = Wire(new IfuWbToFtqDB)
  ifuWbToFtqDumpData.start_addr        := wb_ftq_req.startAddr
  ifuWbToFtqDumpData.is_miss_pred      := checkFlushWb.bits.misOffset.valid
  ifuWbToFtqDumpData.miss_pred_offset  := checkFlushWb.bits.misOffset.bits
  ifuWbToFtqDumpData.checkJalFault     := checkJalFault
  ifuWbToFtqDumpData.checkJalrFault    := checkJalrFault
  ifuWbToFtqDumpData.checkRetFault     := checkRetFault
  ifuWbToFtqDumpData.checkTargetFault  := checkTargetFault
  ifuWbToFtqDumpData.checkNotCFIFault  := checkNotCFIFault
  ifuWbToFtqDumpData.checkInvalidTaken := checkInvalidTaken

  fetchToIBufferTable.log(
    data = fetchIBufferDumpData,
    en = isWriteFetchToIBufferTable.orR && io.toIBuffer.fire,
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
