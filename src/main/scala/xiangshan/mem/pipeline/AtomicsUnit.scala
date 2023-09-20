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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.cache.{AtomicWordIO, MemoryOpConstants, HasDCacheParameters}
import xiangshan.cache.mmu.{TlbCmd, TlbRequestIO}
import difftest._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.PMPRespBundle

class AtomicsUnit(implicit p: Parameters) extends XSModule with MemoryOpConstants with HasDCacheParameters{
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val in            = Flipped(Decoupled(new ExuInput))
    val storeDataIn   = Flipped(Valid(new ExuOutput)) // src2 from rs
    val out           = Decoupled(new ExuOutput)
    val dcache        = new AtomicWordIO
    val dtlb          = new TlbRequestIO(2)
    val pmpResp       = Flipped(new PMPRespBundle())
    val rsIdx         = Input(UInt(log2Up(IssQueSize).W))
    val flush_sbuffer = new SbufferFlushBundle
    val feedbackSlow  = ValidIO(new RSFeedback)
    val redirect      = Flipped(ValidIO(new Redirect))
    val exceptionAddr = ValidIO(UInt(VAddrBits.W))
    val csrCtrl       = Flipped(new CustomCSRCtrlIO)
  })

  //-------------------------------------------------------
  // Atomics Memory Accsess FSM
  //-------------------------------------------------------
  val s_invalid :: s_tlb_and_flush_sbuffer_req :: s_pm :: s_wait_flush_sbuffer_resp :: s_cache_req :: s_cache_resp :: s_cache_resp_latch :: s_finish :: Nil = Enum(8)
  val state = RegInit(s_invalid)
  val out_valid = RegInit(false.B)
  val data_valid = RegInit(false.B)
  val in = Reg(new ExuInput())
  val exceptionVec = RegInit(0.U.asTypeOf(ExceptionVec()))
  val atom_override_xtval = RegInit(false.B)
  val have_sent_first_tlb_req = RegInit(false.B)
  val isLr = in.uop.ctrl.fuOpType === LSUOpType.lr_w || in.uop.ctrl.fuOpType === LSUOpType.lr_d
  // paddr after translation
  val paddr = Reg(UInt())
  val vaddr = in.src(0)
  val is_mmio = Reg(Bool())

  // dcache response data
  val resp_data = Reg(UInt())
  val resp_data_wire = WireInit(0.U)
  val is_lrsc_valid = Reg(Bool())
  // sbuffer is empty or not
  val sbuffer_empty = io.flush_sbuffer.empty


  // Difftest signals
  val paddr_reg = Reg(UInt(64.W))
  val data_reg = Reg(UInt(64.W))
  val mask_reg = Reg(UInt(8.W))
  val fuop_reg = Reg(UInt(8.W))

  io.exceptionAddr.valid := atom_override_xtval
  io.exceptionAddr.bits  := in.src(0)

  // assign default value to output signals
  io.in.ready          := false.B

  io.dcache.req.valid  := false.B
  io.dcache.req.bits   := DontCare

  io.dtlb.req.valid    := false.B
  io.dtlb.req.bits     := DontCare
  io.dtlb.req_kill     := false.B
  io.dtlb.resp.ready   := true.B

  io.flush_sbuffer.valid := false.B

  XSDebug("state: %d\n", state)

  when (state === s_invalid) {
    io.in.ready := true.B
    when (io.in.fire) {
      in := io.in.bits
      in.src(1) := in.src(1) // leave src2 unchanged
      state := s_tlb_and_flush_sbuffer_req
      have_sent_first_tlb_req := false.B
    }
  }

  when (io.storeDataIn.fire) {
    in.src(1) := io.storeDataIn.bits.data
    data_valid := true.B
  }

  assert(!(io.storeDataIn.fire && data_valid), "atomic unit re-receive data")

  // Send TLB feedback to store issue queue
  // we send feedback right after we receives request
  // also, we always treat amo as tlb hit
  // since we will continue polling tlb all by ourself
  io.feedbackSlow.valid       := RegNext(RegNext(io.in.valid))
  io.feedbackSlow.bits.hit    := true.B
  io.feedbackSlow.bits.rsIdx  := RegEnable(io.rsIdx, io.in.valid)
  io.feedbackSlow.bits.flushState := DontCare
  io.feedbackSlow.bits.sourceType := DontCare
  io.feedbackSlow.bits.dataInvalidSqIdx := DontCare

  // tlb translation, manipulating signals && deal with exception
  // at the same time, flush sbuffer
  when (state === s_tlb_and_flush_sbuffer_req) {
    // send req to dtlb
    // keep firing until tlb hit
    io.dtlb.req.valid       := true.B
    io.dtlb.req.bits.vaddr  := in.src(0)
    io.dtlb.resp.ready      := true.B
    io.dtlb.req.bits.cmd    := Mux(isLr, TlbCmd.atom_read, TlbCmd.atom_write)
    io.dtlb.req.bits.debug.pc := in.uop.cf.pc
    io.dtlb.req.bits.debug.isFirstIssue := false.B
    io.out.bits.uop.debugInfo.tlbFirstReqTime := GTimer() // FIXME lyq: it will be always assigned

    // send req to sbuffer to flush it if it is not empty
    io.flush_sbuffer.valid := Mux(sbuffer_empty, false.B, true.B)

    // do not accept tlb resp in the first cycle
    // this limition is for hw prefetcher
    // when !have_sent_first_tlb_req, tlb resp may come from hw prefetch
    have_sent_first_tlb_req := true.B

    when(io.dtlb.resp.fire && have_sent_first_tlb_req){
      paddr := io.dtlb.resp.bits.paddr(0)
      // exception handling
      val addrAligned = LookupTree(in.uop.ctrl.fuOpType(1,0), List(
        "b00".U   -> true.B,              //b
        "b01".U   -> (in.src(0)(0) === 0.U),   //h
        "b10".U   -> (in.src(0)(1,0) === 0.U), //w
        "b11".U   -> (in.src(0)(2,0) === 0.U)  //d
      ))
      exceptionVec(loadAddrMisaligned)  := !addrAligned && isLr
      exceptionVec(storeAddrMisaligned) := !addrAligned && !isLr
      exceptionVec(storePageFault)      := io.dtlb.resp.bits.excp(0).pf.st
      exceptionVec(loadPageFault)       := io.dtlb.resp.bits.excp(0).pf.ld
      exceptionVec(storeAccessFault)    := io.dtlb.resp.bits.excp(0).af.st
      exceptionVec(loadAccessFault)     := io.dtlb.resp.bits.excp(0).af.ld

      when (!io.dtlb.resp.bits.miss) {
        io.out.bits.uop.debugInfo.tlbRespTime := GTimer()
        when (!addrAligned) {
          // NOTE: when addrAligned, do not need to wait tlb actually
          // check for miss aligned exceptions, tlb exception are checked next cycle for timing
          // if there are exceptions, no need to execute it
          state := s_finish
          out_valid := true.B
          atom_override_xtval := true.B
        } .otherwise {
          state := s_pm
        }
      }
    }
  }

  when (state === s_pm) {
    val pmp = WireInit(io.pmpResp)
    is_mmio := pmp.mmio

    // NOTE: only handle load/store exception here, if other exception happens, don't send here
    val exception_va = exceptionVec(storePageFault) || exceptionVec(loadPageFault) ||
      exceptionVec(storeAccessFault) || exceptionVec(loadAccessFault)
    val exception_pa = pmp.st || pmp.ld
    when (exception_va || exception_pa) {
      state := s_finish
      out_valid := true.B
      atom_override_xtval := true.B
    }.otherwise {
      // if sbuffer has been flushed, go to query dcache, otherwise wait for sbuffer.
      state := Mux(sbuffer_empty, s_cache_req, s_wait_flush_sbuffer_resp);
    }
    // update storeAccessFault bit
    exceptionVec(loadAccessFault) := exceptionVec(loadAccessFault) || pmp.ld && isLr
    exceptionVec(storeAccessFault) := exceptionVec(storeAccessFault) || pmp.st || pmp.ld && !isLr
  }

  when (state === s_wait_flush_sbuffer_resp) {
    when (sbuffer_empty) {
      state := s_cache_req
    }
  }

  when (state === s_cache_req) {
    val pipe_req = io.dcache.req.bits
    pipe_req := DontCare

    pipe_req.cmd := LookupTree(in.uop.ctrl.fuOpType, List(
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
    pipe_req.miss := false.B
    pipe_req.probe := false.B
    pipe_req.probe_need_data := false.B
    pipe_req.source := AMO_SOURCE.U
    pipe_req.addr   := get_block_addr(paddr)
    pipe_req.vaddr  := get_block_addr(in.src(0)) // vaddr
    pipe_req.word_idx  := get_word(paddr)
    pipe_req.amo_data  := genWdata(in.src(1), in.uop.ctrl.fuOpType(1,0))
    pipe_req.amo_mask  := genWmask(paddr, in.uop.ctrl.fuOpType(1,0))

    io.dcache.req.valid := Mux(
      io.dcache.req.bits.cmd === M_XLR,
      !io.dcache.block_lr, // block lr to survive in lr storm
      data_valid // wait until src(1) is ready
    )

    when(io.dcache.req.fire){
      state := s_cache_resp
      paddr_reg := paddr
      data_reg := io.dcache.req.bits.amo_data
      mask_reg := io.dcache.req.bits.amo_mask
      fuop_reg := in.uop.ctrl.fuOpType
    }
  }

  val dcache_resp_data  = Reg(UInt())
  val dcache_resp_id    = Reg(UInt())
  val dcache_resp_error = Reg(Bool())

  when (state === s_cache_resp) {
    // when not miss
    // everything is OK, simply send response back to sbuffer
    // when miss and not replay
    // wait for missQueue to handling miss and replaying our request
    // when miss and replay
    // req missed and fail to enter missQueue, manually replay it later
    // TODO: add assertions:
    // 1. add a replay delay counter?
    // 2. when req gets into MissQueue, it should not miss any more
    when(io.dcache.resp.fire) {
      when(io.dcache.resp.bits.miss) {
        when(io.dcache.resp.bits.replay) {
          state := s_cache_req
        }
      } .otherwise {
        dcache_resp_data := io.dcache.resp.bits.data
        dcache_resp_id := io.dcache.resp.bits.id
        dcache_resp_error := io.dcache.resp.bits.error
        state := s_cache_resp_latch
      }
    }
  }

  when (state === s_cache_resp_latch) {
    is_lrsc_valid :=  dcache_resp_id
    val rdataSel = LookupTree(paddr(2, 0), List(
      "b000".U -> dcache_resp_data(63, 0),
      "b001".U -> dcache_resp_data(63, 8),
      "b010".U -> dcache_resp_data(63, 16),
      "b011".U -> dcache_resp_data(63, 24),
      "b100".U -> dcache_resp_data(63, 32),
      "b101".U -> dcache_resp_data(63, 40),
      "b110".U -> dcache_resp_data(63, 48),
      "b111".U -> dcache_resp_data(63, 56)
    ))

    resp_data_wire := LookupTree(in.uop.ctrl.fuOpType, List(
      LSUOpType.lr_w      -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.sc_w      -> dcache_resp_data,
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
      LSUOpType.sc_d      -> dcache_resp_data,
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

    when (dcache_resp_error && io.csrCtrl.cache_error_enable) {
      exceptionVec(loadAccessFault)  := isLr
      exceptionVec(storeAccessFault) := !isLr
      assert(!exceptionVec(loadAccessFault))
      assert(!exceptionVec(storeAccessFault))
    }

    resp_data := resp_data_wire
    state := s_finish
    out_valid := true.B
  }

  io.out.valid := out_valid
  XSError((state === s_finish) =/= out_valid, "out_valid reg error\n")
  io.out.bits := DontCare
  io.out.bits.uop := in.uop
  io.out.bits.uop.cf.exceptionVec := exceptionVec
  io.out.bits.data := resp_data
  io.out.bits.redirectValid := false.B
  io.out.bits.debug.isMMIO := is_mmio
  io.out.bits.debug.paddr := paddr
  when (io.out.fire) {
    XSDebug("atomics writeback: pc %x data %x\n", io.out.bits.uop.cf.pc, io.dcache.resp.bits.data)
    state := s_invalid
    out_valid := false.B
  }

  when (state === s_finish) {
    data_valid := false.B
  }

  when (io.redirect.valid) {
    atom_override_xtval := false.B
  }

  // atomic trigger
  val csrCtrl = io.csrCtrl
  val tdata = Reg(Vec(6, new MatchTriggerIO))
  val tEnable = RegInit(VecInit(Seq.fill(6)(false.B)))
  val en = csrCtrl.trigger_enable
  tEnable := VecInit(en(2), en (3), en(7), en(4), en(5), en(9))
  when(csrCtrl.mem_trigger.t.valid) {
    tdata(csrCtrl.mem_trigger.t.bits.addr) := csrCtrl.mem_trigger.t.bits.tdata
  }
  val lTriggerMapping = Map(0 -> 2, 1 -> 3, 2 -> 5)
  val sTriggerMapping = Map(0 -> 0, 1 -> 1, 2 -> 4)

  val backendTriggerHitReg = Reg(Vec(6, Bool()))
  backendTriggerHitReg := VecInit(Seq.fill(6)(false.B))

  when(state === s_cache_req){
    // store trigger
    val store_hit = Wire(Vec(3, Bool()))
    for (j <- 0 until 3) {
        store_hit(j) := !tdata(sTriggerMapping(j)).select && TriggerCmp(
          vaddr,
          tdata(sTriggerMapping(j)).tdata2,
          tdata(sTriggerMapping(j)).matchType,
          tEnable(sTriggerMapping(j))
        )
       backendTriggerHitReg(sTriggerMapping(j)) := store_hit(j)
     }

    when(tdata(0).chain) {
      backendTriggerHitReg(0) := store_hit(0) && store_hit(1)
      backendTriggerHitReg(1) := store_hit(0) && store_hit(1)
    }

    when(!in.uop.cf.trigger.backendEn(0)) {
      backendTriggerHitReg(4) := false.B
    }

    // load trigger
    val load_hit = Wire(Vec(3, Bool()))
    for (j <- 0 until 3) {

      val addrHit = TriggerCmp(
        vaddr,
        tdata(lTriggerMapping(j)).tdata2,
        tdata(lTriggerMapping(j)).matchType,
        tEnable(lTriggerMapping(j))
      )
      load_hit(j) := addrHit && !tdata(lTriggerMapping(j)).select
      backendTriggerHitReg(lTriggerMapping(j)) := load_hit(j)
    }
    when(tdata(2).chain) {
      backendTriggerHitReg(2) := load_hit(0) && load_hit(1)
      backendTriggerHitReg(3) := load_hit(0) && load_hit(1)
    }
    when(!in.uop.cf.trigger.backendEn(1)) {
      backendTriggerHitReg(5) := false.B
    }
  }

  // addr trigger do cmp at s_cache_req
  // trigger result is used at s_finish
  // thus we can delay it safely
  io.out.bits.uop.cf.trigger.backendHit := VecInit(Seq.fill(6)(false.B))
  when(isLr){
    // enable load trigger
    io.out.bits.uop.cf.trigger.backendHit(2) := backendTriggerHitReg(2)
    io.out.bits.uop.cf.trigger.backendHit(3) := backendTriggerHitReg(3)
    io.out.bits.uop.cf.trigger.backendHit(5) := backendTriggerHitReg(5)
  }.otherwise{
    // enable store trigger
    io.out.bits.uop.cf.trigger.backendHit(0) := backendTriggerHitReg(0)
    io.out.bits.uop.cf.trigger.backendHit(1) := backendTriggerHitReg(1)
    io.out.bits.uop.cf.trigger.backendHit(4) := backendTriggerHitReg(4)
  }

  if (env.EnableDifftest) {
    val difftest = DifftestModule(new DiffAtomicEvent)
    difftest.coreid := io.hartId
    difftest.valid  := state === s_cache_resp_latch
    difftest.addr   := paddr_reg
    difftest.data   := data_reg
    difftest.mask   := mask_reg
    difftest.fuop   := fuop_reg
    difftest.out    := resp_data_wire
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val uop = io.out.bits.uop
    val difftest = DifftestModule(new DiffLrScEvent)
    difftest.coreid := io.hartId
    difftest.valid := io.out.fire &&
      (uop.ctrl.fuOpType === LSUOpType.sc_d || uop.ctrl.fuOpType === LSUOpType.sc_w)
    difftest.success := is_lrsc_valid
  }
}
