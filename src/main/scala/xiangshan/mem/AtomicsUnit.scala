package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{DCacheLoadIO, TlbRequestIO, TlbCmd, MemoryOpConstants}

class AtomicsUnit extends XSModule with MemoryOpConstants{
  val io = IO(new Bundle() {
    val in            = Flipped(Decoupled(new ExuInput))
    val out           = Decoupled(new ExuOutput)
    val dcache        = new DCacheLoadIO
    val dtlb          = new TlbRequestIO
    val flush_sbuffer = new SbufferFlushBundle
  })

  //-------------------------------------------------------
  // Atomics Memory Accsess FSM
  //-------------------------------------------------------
  val s_invalid :: s_tlb  :: s_flush_sbuffer_req :: s_flush_sbuffer_resp :: s_cache_req :: s_cache_resp :: s_finish :: Nil = Enum(7)
  val state = RegInit(s_invalid)
  val in = Reg(new ExuInput())
  // paddr after translation
  val paddr = Reg(UInt())
  // dcache response data
  val resp_data = Reg(UInt())

  // assign default value to output signals
  io.in.ready          := false.B
  io.out.valid         := false.B
  io.out.bits          := DontCare

  io.dcache.req.valid  := false.B
  io.dcache.req.bits   := DontCare
  io.dcache.s1_kill    := false.B
  io.dcache.resp.ready := false.B

  io.dtlb.req.valid    := false.B
  io.dtlb.req.bits     := DontCare

  io.flush_sbuffer.valid := false.B



  when (state === s_invalid) {
    io.in.ready := true.B
    when (io.in.fire()) {
      in := io.in.bits
      state := s_tlb
    }
  }

  // tlb translation, manipulating signals && deal with exception
  when (state === s_tlb) {
    // send req to dtlb
    // keep firing until tlb hit
    io.dtlb.req.valid       := true.B
    io.dtlb.req.bits.vaddr  := in.src1
    io.dtlb.req.bits.roqIdx := in.uop.roqIdx
    io.dtlb.req.bits.cmd    := Mux(in.uop.ctrl.fuOpType === LSUOpType.lr, TlbCmd.read, TlbCmd.write)
    io.dtlb.req.bits.debug.pc := in.uop.cf.pc
    io.dtlb.req.bits.debug.lsroqIdx := in.uop.lsroqIdx

    when(io.dtlb.resp.valid && !io.dtlb.resp.bits.miss){
      paddr := io.dtlb.resp.bits.paddr
      state := s_flush_sbuffer_req
    }

    // TODO: exception handling
    val exception = WireInit(false.B)
    // there are exceptions, no need to execute it
    when (exception) {
      state := s_finish
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
      LSUOpType.lr -> M_XLR,
      LSUOpType.sc -> M_XSC,
      LSUOpType.amoswap -> M_XA_SWAP,
      LSUOpType.amoadd -> M_XA_ADD,
      LSUOpType.amoxor -> M_XA_XOR,
      LSUOpType.amoand -> M_XA_AND,
      LSUOpType.amoor -> M_XA_OR,
      LSUOpType.amomin -> M_XA_MIN,
      LSUOpType.amomax -> M_XA_MAX,
      LSUOpType.amominu -> M_XA_MINU,
      LSUOpType.amomaxu -> M_XA_MAXU
    ))

    io.dcache.req.bits.addr := paddr 
    io.dcache.req.bits.data := in.src2
    // TODO: atomics do need mask: fix mask
    io.dcache.req.bits.mask := DontCare
    io.dcache.req.bits.meta.id       := DCacheAtomicsType.atomics
    io.dcache.req.bits.meta.paddr    := paddr
    io.dcache.req.bits.meta.tlb_miss := false.B
    io.dcache.req.bits.meta.replay   := false.B

    when(io.dcache.req.fire()){
      state := s_cache_resp
    }
  }

  when (state === s_cache_resp) {
    io.dcache.resp.ready := true.B
    when(io.dcache.resp.fire()) {
      resp_data := io.dcache.resp.bits.data
      state := s_finish
    }
  }

  when (state === s_finish) {
    io.out.valid := true.B
    io.out.bits.uop := in.uop
    io.out.bits.data := resp_data
    io.out.bits.redirectValid := false.B
    io.out.bits.redirect := DontCare
    io.out.bits.brUpdate := DontCare
    io.out.bits.debug.isMMIO := AddressSpace.isMMIO(paddr)
    when (io.out.fire()) {
      XSDebug("atomics writeback: pc %x data %x\n", io.out.bits.uop.cf.pc, io.dcache.resp.bits.data)
      state := s_invalid
    }
  }
}
