package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{DCacheWordIO, TlbRequestIO, TlbCmd, MemoryOpConstants}
import xiangshan.backend.LSUOpType

class AtomicsUnit extends XSModule with MemoryOpConstants{
  val io = IO(new Bundle() {
    val in            = Flipped(Decoupled(new ExuInput))
    val out           = Decoupled(new ExuOutput)
    val dcache        = new DCacheWordIO
    val dtlb          = new TlbRequestIO
    val rsIdx         = Input(UInt(log2Up(IssQueSize).W))
    val flush_sbuffer = new SbufferFlushBundle
    val tlbFeedback   = ValidIO(new TlbFeedback)
    val redirect      = Flipped(ValidIO(new Redirect))
    val flush      = Input(Bool())
    val exceptionAddr = ValidIO(UInt(VAddrBits.W))
  })

  val difftestIO = IO(new Bundle() {
    val atomicResp = Output(Bool())
    val atomicAddr = Output(UInt(64.W))
    val atomicData = Output(UInt(64.W))
    val atomicMask = Output(UInt(8.W))
    val atomicFuop = Output(UInt(8.W))
    val atomicOut  = Output(UInt(64.W))
  })
  difftestIO <> DontCare

  //-------------------------------------------------------
  // Atomics Memory Accsess FSM
  //-------------------------------------------------------
  val s_invalid :: s_tlb  :: s_flush_sbuffer_req :: s_flush_sbuffer_resp :: s_cache_req :: s_cache_resp :: s_finish :: Nil = Enum(7)
  val state = RegInit(s_invalid)
  val in = Reg(new ExuInput())
  val exceptionVec = RegInit(0.U.asTypeOf(ExceptionVec()))
  val atom_override_xtval = RegInit(false.B)
  // paddr after translation
  val paddr = Reg(UInt())
  val is_mmio = Reg(Bool())
  // dcache response data
  val resp_data = Reg(UInt())
  val resp_data_wire = WireInit(0.U)
  val is_lrsc_valid = Reg(Bool())

  // Difftest signals
  val paddr_reg = Reg(UInt(64.W))
  val data_reg = Reg(UInt(64.W))
  val mask_reg = Reg(UInt(8.W))
  val fuop_reg = Reg(UInt(8.W))

  io.exceptionAddr.valid := atom_override_xtval
  io.exceptionAddr.bits  := in.src1

  // assign default value to output signals
  io.in.ready          := false.B
  io.out.valid         := false.B
  io.out.bits          := DontCare

  io.dcache.req.valid  := false.B
  io.dcache.req.bits   := DontCare
  io.dcache.resp.ready := false.B

  io.dtlb.req.valid    := false.B
  io.dtlb.req.bits     := DontCare
  io.dtlb.resp.ready   := false.B

  io.flush_sbuffer.valid := false.B

  XSDebug("state: %d\n", state)

  when (state === s_invalid) {
    io.in.ready := true.B
    when (io.in.fire()) {
      in := io.in.bits
      state := s_tlb
    }
  }

  // Send TLB feedback to store issue queue
  // we send feedback right after we receives request
  // also, we always treat amo as tlb hit
  // since we will continue polling tlb all by ourself
  io.tlbFeedback.valid       := RegNext(RegNext(io.in.valid))
  io.tlbFeedback.bits.hit    := true.B
  io.tlbFeedback.bits.rsIdx  := RegEnable(io.rsIdx, io.in.valid)
  io.tlbFeedback.bits.flushState := DontCare

  // tlb translation, manipulating signals && deal with exception
  when (state === s_tlb) {
    // send req to dtlb
    // keep firing until tlb hit
    io.dtlb.req.valid       := true.B
    io.dtlb.req.bits.vaddr  := in.src1
    io.dtlb.req.bits.roqIdx := in.uop.roqIdx
    io.dtlb.resp.ready      := true.B
    val is_lr = in.uop.ctrl.fuOpType === LSUOpType.lr_w || in.uop.ctrl.fuOpType === LSUOpType.lr_d
    io.dtlb.req.bits.cmd    := Mux(is_lr, TlbCmd.atom_read, TlbCmd.atom_write)
    io.dtlb.req.bits.debug.pc := in.uop.cf.pc
    io.dtlb.req.bits.debug.isFirstIssue := false.B

    when(io.dtlb.resp.fire && !io.dtlb.resp.bits.miss){
      // exception handling
      val addrAligned = LookupTree(in.uop.ctrl.fuOpType(1,0), List(
        "b00".U   -> true.B,              //b
        "b01".U   -> (in.src1(0) === 0.U),   //h
        "b10".U   -> (in.src1(1,0) === 0.U), //w
        "b11".U   -> (in.src1(2,0) === 0.U)  //d
      ))
      exceptionVec(storeAddrMisaligned) := !addrAligned
      exceptionVec(storePageFault)      := io.dtlb.resp.bits.excp.pf.st
      exceptionVec(loadPageFault)       := io.dtlb.resp.bits.excp.pf.ld
      exceptionVec(storeAccessFault)    := io.dtlb.resp.bits.excp.af.st
      exceptionVec(loadAccessFault)     := io.dtlb.resp.bits.excp.af.ld
      val exception = !addrAligned ||
        io.dtlb.resp.bits.excp.pf.st ||
        io.dtlb.resp.bits.excp.pf.ld ||
        io.dtlb.resp.bits.excp.af.st ||
        io.dtlb.resp.bits.excp.af.ld
      is_mmio := io.dtlb.resp.bits.mmio
      when (exception) {
        // check for exceptions
        // if there are exceptions, no need to execute it
        state := s_finish
        atom_override_xtval := true.B
      } .otherwise {
        paddr := io.dtlb.resp.bits.paddr
        state := s_flush_sbuffer_req
      }
    }
  }


  when (state === s_flush_sbuffer_req) {
    io.flush_sbuffer.valid := true.B
    state := s_flush_sbuffer_resp
  }

  when (state === s_flush_sbuffer_resp) {
    when (io.flush_sbuffer.empty) {
      state := s_cache_req
    }
  }

  when (state === s_cache_req) {
    io.dcache.req.valid := true.B
    io.dcache.req.bits.cmd := LookupTree(in.uop.ctrl.fuOpType, List(
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

    io.dcache.req.bits.addr := paddr
    io.dcache.req.bits.data := genWdata(in.src2, in.uop.ctrl.fuOpType(1,0))
    // TODO: atomics do need mask: fix mask
    io.dcache.req.bits.mask := genWmask(paddr, in.uop.ctrl.fuOpType(1,0))
    io.dcache.req.bits.id   := DontCare

    when(io.dcache.req.fire()){
      state := s_cache_resp
      paddr_reg := io.dcache.req.bits.addr
      data_reg := io.dcache.req.bits.data
      mask_reg := io.dcache.req.bits.mask
      fuop_reg := in.uop.ctrl.fuOpType
    }
  }

  when (state === s_cache_resp) {
    io.dcache.resp.ready := true.B
    when(io.dcache.resp.fire()) {
      is_lrsc_valid := io.dcache.resp.bits.id
      val rdata = io.dcache.resp.bits.data
      val rdataSel = LookupTree(paddr(2, 0), List(
        "b000".U -> rdata(63, 0),
        "b001".U -> rdata(63, 8),
        "b010".U -> rdata(63, 16),
        "b011".U -> rdata(63, 24),
        "b100".U -> rdata(63, 32),
        "b101".U -> rdata(63, 40),
        "b110".U -> rdata(63, 48),
        "b111".U -> rdata(63, 56)
      ))

      resp_data_wire := LookupTree(in.uop.ctrl.fuOpType, List(
        LSUOpType.lr_w      -> SignExt(rdataSel(31, 0), XLEN),
        LSUOpType.sc_w      -> rdata,
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
        LSUOpType.sc_d      -> rdata,
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

      resp_data := resp_data_wire
      state := s_finish
    }
  }

  when (state === s_finish) {
    io.out.valid := true.B
    io.out.bits.uop := in.uop
    io.out.bits.uop.cf.exceptionVec := exceptionVec
    io.out.bits.uop.diffTestDebugLrScValid := is_lrsc_valid
    io.out.bits.data := resp_data
    io.out.bits.redirectValid := false.B
    io.out.bits.redirect := DontCare
    io.out.bits.debug.isMMIO := is_mmio
    io.out.bits.debug.paddr := paddr
    when (io.out.fire()) {
      XSDebug("atomics writeback: pc %x data %x\n", io.out.bits.uop.cf.pc, io.dcache.resp.bits.data)
      state := s_invalid
    }
  }

  when(io.redirect.valid || io.flush){
    atom_override_xtval := false.B
  }

  if (!env.FPGAPlatform) {
    difftestIO.atomicResp := WireInit(io.dcache.resp.fire())
    difftestIO.atomicAddr := WireInit(paddr_reg)
    difftestIO.atomicData := WireInit(data_reg)
    difftestIO.atomicMask := WireInit(mask_reg)
    difftestIO.atomicFuop := WireInit(fuop_reg)
    difftestIO.atomicOut  := resp_data_wire
  }
}
