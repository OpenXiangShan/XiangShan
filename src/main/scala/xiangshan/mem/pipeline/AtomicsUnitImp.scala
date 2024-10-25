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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.ctrlblock.DebugLsInfoBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.datapath.NewPipelineConnectPipe
import xiangshan.cache.mmu.{TlbCmd, TlbReq, TlbRequestIO, TlbResp, Pbmt}
import xiangshan.cache.{DcacheStoreRequestIO, DCacheStoreIO, MemoryOpConstants, HasDCacheParameters, StorePrefetchReq}
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO, AtomicWordIO}
import xiangshan.mem.ReplayCauseNO._
import xiangshan.mem.Bundles._
import difftest._

class AtomicsUnitIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO {
  // from
  val fromBackend = new Bundle() {
    val storeDataIn = Flipped(Valid(new MemExuOutput))
  }

  // to
  val toBackend = new Bundle() {
    val iqFeedback = ValidIO(new RSFeedback)
  }
  val flushSbuffer = new SbufferFlushBundle
  val exceptionInfo = ValidIO(new ExceptionAddrIO)
  val amoDCacheIO = new AtomicWordIO
}

class AtomicsUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
{

  io.suggestName("none")
  override lazy val io = IO(new AtomicsUnitIO).suggestName("io")

  private val toBackend = io.toBackend

  val sInvalid :: sTlbAndFlushSbufferReq :: sPm :: sWaitFlushSbufferResp :: sCacheReq :: sCacheResp :: sCacheRespLatch :: sFinish :: Nil = Enum(8)
  val state = RegInit(sInvalid)
  val in = s1In
  val outValid = RegInit(false.B)
  val srcData  = Reg(UInt())
  val dataValid = RegInit(false.B)
  val exceptionVec = RegInit(0.U.asTypeOf(ExceptionVec()))
  val atomOverrideXtval = RegInit(false.B)
  val haveSentFirstTlbReq = RegInit(false.B)
  val isLr = in.bits.uop.fuOpType === LSUOpType.lr_w || in.bits.uop.fuOpType === LSUOpType.lr_d
  // paddr after translation
  val paddr = Reg(UInt())
  val gpaddr = Reg(UInt())
  val vaddr = in.bits.src(0)
  val isMmio = Reg(Bool())
  val isForVSnonLeafPTE = Reg(Bool())

  // dcache response data
  val respData = Reg(UInt())
  val respDataWire = WireInit(0.U)
  val isLrscValid = Reg(Bool())
  // sbuffer is empty or not
  val sbufferEmpty = io.flushSbuffer.empty

  // Difftest signals
  val paddrReg = Reg(UInt(64.W))
  val dataReg = Reg(UInt(64.W))
  val maskReg = Reg(UInt(8.W))
  val fuopReg = Reg(UInt(8.W))

  io.exceptionInfo.valid := atomOverrideXtval
  io.exceptionInfo.bits := DontCare
  io.exceptionInfo.bits.vaddr := in.bits.src(0)
  io.exceptionInfo.bits.gpaddr := gpaddr
  io.exceptionInfo.bits.isForVSnonLeafPTE := isForVSnonLeafPTE

  in.ready := false.B

  io.amoDCacheIO.req.valid := false.B
  io.amoDCacheIO.req.bits  := DontCare

  toTlb.req.valid    := false.B
  toTlb.req.bits     := DontCare
  toTlb.req_kill     := false.B
  io.fromTlb.resp.ready := true.B

  io.flushSbuffer.valid := false.B

  XSDebug("state: %d\n", state)
  when (state === sInvalid) {
    in.ready := true.B
    when (s0Out.fire) {
      state := sTlbAndFlushSbufferReq
      haveSentFirstTlbReq := false.B
    }
  }

  when (io.fromBackend.storeDataIn.fire) {
    srcData := io.fromBackend.storeDataIn.bits.data
    dataValid := true.B
  }
  s1In.bits.src(1) := srcData

  XSError((io.fromBackend.storeDataIn.fire && dataValid), "atomic unit re-receive data")

  // Send TLB feedback to store issue queue
  // we send feedback right after we receives request
  // also, we always treat amo as tlb hit
  // since we will continue polling tlb all by ourself
  toBackend.iqFeedback.valid := GatedValidRegNext(GatedValidRegNext(s0Out.valid))
  toBackend.iqFeedback.bits.hit    := true.B
  toBackend.iqFeedback.bits.robIdx  := RegEnable(s0Out.bits.uop.robIdx, s0Out.valid)
  toBackend.iqFeedback.bits.sqIdx   := RegEnable(s0Out.bits.uop.sqIdx, s0Out.valid)
  toBackend.iqFeedback.bits.lqIdx   := RegEnable(s0Out.bits.uop.lqIdx, s0Out.valid)
  toBackend.iqFeedback.bits.flushState := DontCare
  toBackend.iqFeedback.bits.sourceType := DontCare
  toBackend.iqFeedback.bits.dataInvalidSqIdx := DontCare

  // tlb translation, manipulating signals && deal with exception
  // at the same time, flush sbuffer
  when (state === sTlbAndFlushSbufferReq) {
    // send req to dtlb
    // keep firing until tlb hit
    toTlb.req.valid       := true.B
    toTlb.req.bits.vaddr  := in.bits.vaddr
    toTlb.req.bits.fullva := in.bits.fullva
    toTlb.req.bits.checkfullva := true.B
    toTlb.req.bits.cmd    := Mux(isLr, TlbCmd.atom_read, TlbCmd.atom_write)
    toTlb.req.bits.debug.pc := in.bits.uop.pc
    toTlb.req.bits.debug.robIdx := in.bits.uop.robIdx
    toTlb.req.bits.debug.isFirstIssue := false.B
    io.fromTlb.resp.ready      := true.B
    io.toIssue.head.bits.uop.debugInfo.tlbFirstReqTime := GTimer() // FIXME lyq: it will be always assigned

    // send req to sbuffer to flush it if it is not empty
    io.flushSbuffer.valid := Mux(sbufferEmpty, false.B, true.B)

    // do not accept tlb resp in the first cycle
    // this limition is for hw prefetcher
    // when !haveSentFirstTlbReq, tlb resp may come from hw prefetch
    haveSentFirstTlbReq := true.B

    when(io.fromTlb.resp.fire && haveSentFirstTlbReq){
      paddr   := io.fromTlb.resp.bits.paddr(0)
      gpaddr  := io.fromTlb.resp.bits.gpaddr(0)
      isForVSnonLeafPTE := io.fromTlb.resp.bits.isForVSnonLeafPTE
      // exception handling
      val addrAligned = LookupTree(in.bits.uop.fuOpType(1,0), List(
        "b00".U   -> true.B,              //b
        "b01".U   -> (in.bits.src(0)(0) === 0.U),   //h
        "b10".U   -> (in.bits.src(0)(1,0) === 0.U), //w
        "b11".U   -> (in.bits.src(0)(2,0) === 0.U)  //d
      ))
      exceptionVec(loadAddrMisaligned)  := !addrAligned && isLr
      exceptionVec(storeAddrMisaligned) := !addrAligned && !isLr
      exceptionVec(storePageFault)      := io.fromTlb.resp.bits.excp(0).pf.st
      exceptionVec(loadPageFault)       := io.fromTlb.resp.bits.excp(0).pf.ld
      exceptionVec(storeAccessFault)    := io.fromTlb.resp.bits.excp(0).af.st
      exceptionVec(loadAccessFault)     := io.fromTlb.resp.bits.excp(0).af.ld
      exceptionVec(storeGuestPageFault) := io.fromTlb.resp.bits.excp(0).gpf.st
      exceptionVec(loadGuestPageFault)  := io.fromTlb.resp.bits.excp(0).gpf.ld

      when (!io.fromTlb.resp.bits.miss) {
        io.toIssue.head.bits.uop.debugInfo.tlbRespTime := GTimer()
        when (!addrAligned) {
          // NOTE: when addrAligned, do not need to wait tlb actually
          // check for miss aligned exceptions, tlb exception are checked next cycle for timing
          // if there are exceptions, no need to execute it
          state := sFinish
          outValid := true.B
          atomOverrideXtval := true.B
        } .otherwise {
          state := sPm
        }
      }
    }
  }
  when (state === sPm) {
    val pmp = WireInit(io.fromPmp)
    isMmio := pmp.mmio

    // NOTE: only handle load/store exception here, if other exception happens, don't send here
    val exception_va = exceptionVec(storePageFault) || exceptionVec(loadPageFault) ||
      exceptionVec(storeGuestPageFault) || exceptionVec(loadGuestPageFault) ||
      exceptionVec(storeAccessFault) || exceptionVec(loadAccessFault)
    val exception_pa = pmp.st || pmp.ld || pmp.mmio
    when (exception_va || exception_pa) {
      state := sFinish
      outValid := true.B
      atomOverrideXtval := true.B
    }.otherwise {
      // if sbuffer has been flushed, go to query dcache, otherwise wait for sbuffer.
      state := Mux(sbufferEmpty, sCacheReq, sWaitFlushSbufferResp);
    }
    // update storeAccessFault bit
    exceptionVec(loadAccessFault) := exceptionVec(loadAccessFault) || (pmp.ld || pmp.mmio) && isLr
    exceptionVec(storeAccessFault) := exceptionVec(storeAccessFault) || pmp.st || (pmp.ld || pmp.mmio) && !isLr
  }

  when (state === sWaitFlushSbufferResp) {
    when (sbufferEmpty) {
      state := sCacheReq
    }
  }

  when (state === sCacheReq) {
    val pipeReq = io.amoDCacheIO.req.bits
    pipeReq := DontCare

    pipeReq.cmd := LookupTree(in.bits.uop.fuOpType, List(
      LSUOpType.lr_w      -> M_XLR,
      LSUOpType.sc_w      -> M_XSC,
      LSUOpType.amoswap_w -> M_XA_SWAP,
      LSUOpType.amoadd_w  -> M_XA_ADD,
      LSUOpType.amoxor_w  -> M_XA_XOR,
      LSUOpType.amoand_w  -> M_XA_AND,
      LSUOpType.amoor_w   -> M_XA_OR,
      LSUOpType.amomin_w  -> M_XA_MIN,
      LSUOpType.amomax_w  -> M_XA_MAX,
      LSUOpType.amominu_w -> M_XA_MINU,
      LSUOpType.amomaxu_w -> M_XA_MAXU,

      LSUOpType.lr_d      -> M_XLR,
      LSUOpType.sc_d      -> M_XSC,
      LSUOpType.amoswap_d -> M_XA_SWAP,
      LSUOpType.amoadd_d  -> M_XA_ADD,
      LSUOpType.amoxor_d  -> M_XA_XOR,
      LSUOpType.amoand_d  -> M_XA_AND,
      LSUOpType.amoor_d   -> M_XA_OR,
      LSUOpType.amomin_d  -> M_XA_MIN,
      LSUOpType.amomax_d  -> M_XA_MAX,
      LSUOpType.amominu_d -> M_XA_MINU,
      LSUOpType.amomaxu_d -> M_XA_MAXU
    ))
    pipeReq.miss := false.B
    pipeReq.probe := false.B
    pipeReq.probe_need_data := false.B
    pipeReq.source := AMO_SOURCE.U
    pipeReq.addr   := get_block_addr(paddr)
    pipeReq.vaddr  := get_block_addr(in.bits.src(0)) // vaddr
    pipeReq.word_idx  := get_word(paddr)
    pipeReq.amo_data  := genWdata(in.bits.src(1), in.bits.uop.fuOpType(1,0))
    pipeReq.amo_mask  := genWmask(paddr, in.bits.uop.fuOpType(1,0))

    io.amoDCacheIO.req.valid := Mux(
      io.amoDCacheIO.req.bits.cmd === M_XLR,
      !io.amoDCacheIO.block_lr, // block lr to survive in lr storm
      dataValid // wait until src(1) is ready
    )

    when(io.amoDCacheIO.req.fire){
      state := sCacheResp
      paddrReg := paddr
      dataReg := io.amoDCacheIO.req.bits.amo_data
      maskReg := io.amoDCacheIO.req.bits.amo_mask
      fuopReg := in.bits.uop.fuOpType
    }
  }
  val dcacheRespData  = Reg(UInt())
  val dcacheRespId    = Reg(UInt())
  val dcacheRespError = Reg(Bool())

  when (state === sCacheResp) {
    // when not miss
    // everything is OK, simply send response back to sbuffer
    // when miss and not replay
    // wait for missQueue to handling miss and replaying our request
    // when miss and replay
    // req missed and fail to enter missQueue, manually replay it later
    // TODO: add assertions:
    // 1. add a replay delay counter?
    // 2. when req gets into MissQueue, it should not miss any more
    when(io.amoDCacheIO.resp.fire) {
      when(io.amoDCacheIO.resp.bits.miss) {
        when(io.amoDCacheIO.resp.bits.replay) {
          state := sCacheReq
        }
      } .otherwise {
        dcacheRespData := io.amoDCacheIO.resp.bits.data
        dcacheRespId := io.amoDCacheIO.resp.bits.id
        dcacheRespError := io.amoDCacheIO.resp.bits.error
        state := sCacheRespLatch
      }
    }
  }

  when (state === sCacheRespLatch) {
    isLrscValid :=  dcacheRespId
    val rdataSel = LookupTree(paddr(2, 0), List(
      "b000".U -> dcacheRespData(63, 0),
      "b001".U -> dcacheRespData(63, 8),
      "b010".U -> dcacheRespData(63, 16),
      "b011".U -> dcacheRespData(63, 24),
      "b100".U -> dcacheRespData(63, 32),
      "b101".U -> dcacheRespData(63, 40),
      "b110".U -> dcacheRespData(63, 48),
      "b111".U -> dcacheRespData(63, 56)
    ))

    respDataWire := LookupTree(in.bits.uop.fuOpType, List(
      LSUOpType.lr_w      -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.sc_w      -> dcacheRespData,
      LSUOpType.amoswap_w -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amoadd_w  -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amoxor_w  -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amoand_w  -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amoor_w   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amomin_w  -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amomax_w  -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amominu_w -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.amomaxu_w -> SignExt(rdataSel(31, 0), XLEN),

      LSUOpType.lr_d      -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.sc_d      -> dcacheRespData,
      LSUOpType.amoswap_d -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amoadd_d  -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amoxor_d  -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amoand_d  -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amoor_d   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amomin_d  -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amomax_d  -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amominu_d -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.amomaxu_d -> SignExt(rdataSel(63, 0), XLEN)
    ))

    when (dcacheRespError && io.fromCtrl.csrCtrl.cache_error_enable) {
      exceptionVec(loadAccessFault)  := isLr
      exceptionVec(storeAccessFault) := !isLr
      assert(!exceptionVec(loadAccessFault))
      assert(!exceptionVec(storeAccessFault))
    }

    respData := respDataWire
    state := sFinish
    outValid := true.B
  }

  io.toIssue.head.valid := outValid
  XSError((state === sFinish) =/= outValid, "outValid reg error\n")
  io.toIssue.head.bits := DontCare
  io.toIssue.head.bits.uop := in.bits.uop
  io.toIssue.head.bits.uop.exceptionVec := exceptionVec
  io.toIssue.head.bits.data := respData
  io.toIssue.head.bits.mmio := isMmio
  io.toIssue.head.bits.paddr := paddr
  when (io.toIssue.head.fire) {
    XSDebug("atomics writeback: pc %x data %x\n", io.toIssue.head.bits.uop.pc, io.amoDCacheIO.resp.bits.data)
    state := sInvalid
    outValid := false.B
  }

  when (state === sFinish) {
    dataValid := false.B
  }

  when (io.fromCtrl.redirect.valid) {
    atomOverrideXtval := false.B
  }

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffAtomicEvent)
    difftest.coreid := io.fromCtrl.hartId
    difftest.valid  := state === sCacheRespLatch
    difftest.addr   := paddrReg
    difftest.data   := dataReg
    difftest.mask   := maskReg
    difftest.fuop   := fuopReg
    difftest.out    := respDataWire
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val uop = io.toIssue.head.bits.uop
    val difftest = DifftestModule(new DiffLrScEvent)
    difftest.coreid := io.fromCtrl.hartId
    difftest.valid := io.toIssue.head.fire &&
      (uop.fuOpType === LSUOpType.sc_d || uop.fuOpType === LSUOpType.sc_w)
    difftest.success := isLrscValid
  }
}