package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{DCacheLoadIO, TlbRequestIO, TlbCmd, MemoryOpConstants}

class MiscUnit extends XSModule with MemoryOpConstants{
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new ExuOutput)
    val dcache = new DCacheLoadIO
    val dtlb = new TlbRequestIO
  })

  //-------------------------------------------------------
  // Misc Memory Accsess FSM
  //-------------------------------------------------------
  
  val s_tlb :: s_cache_req :: s_cache_resp :: Nil = Enum(3)
  val state = RegInit(s_tlb)
  val hasException = WireInit(false.B)

  switch (state) {
    is (s_tlb) {
      when(io.in.valid && io.dtlb.resp.valid && !io.dtlb.resp.bits.miss && !hasException){
        state := s_cache_req
      }
    }
    is (s_cache_req) {
      when(io.dcache.req.fire()){
        state := s_cache_resp
      }
    }
    is (s_cache_resp) {
      when(io.dcache.resp.fire()){
        state := s_tlb
      }
    }
  }

  // TLB   
  // send req to dtlb
  // keep firing until tlb hit
  io.dtlb.req.valid      := io.in.valid && state === s_tlb
  io.dtlb.req.bits.vaddr := io.in.bits.src1
  io.dtlb.req.bits.roqIdx   := io.in.bits.uop.roqIdx
  io.dtlb.req.bits.cmd   := Mux(io.in.bits.uop.ctrl.fuOpType === LSUOpType.lr, TlbCmd.read, TlbCmd.write)
  io.dtlb.req.bits.debug.pc := io.in.bits.uop.cf.pc
  io.dtlb.req.bits.debug.lsroqIdx := io.in.bits.uop.lsroqIdx

  // exception check
  val addrAligned = io.in.bits.src1(1, 0) === 0.U
  // Zam extension is not supported yet
  val exceptionVec = WireInit(VecInit(0.U(16.W).asBools))
  exceptionVec(loadAddrMisaligned) := state === s_tlb && !addrAligned && io.in.bits.uop.ctrl.fuOpType === LSUOpType.lr
  exceptionVec(storeAddrMisaligned) := state === s_tlb && !addrAligned && io.in.bits.uop.ctrl.fuOpType =/= LSUOpType.lr
  exceptionVec(loadPageFault) := io.dtlb.resp.bits.excp.pf.ld
  exceptionVec(storePageFault) := io.dtlb.resp.bits.excp.pf.st
  hasException := exceptionVec.asUInt.orR

  // record paddr
  val paddr = RegEnable(io.dtlb.resp.bits.paddr, io.in.fire())
  val func  = RegEnable(io.in.bits.uop.ctrl.fuOpType, io.in.fire())
  val src2  = RegEnable(io.in.bits.src2, io.in.fire())
  val uop  = RegEnable(io.in.bits.uop, io.in.fire())
  
  // DCache
  // send result to dcache
  io.dcache.req.valid := state === s_cache_req
  io.dcache.req.bits.cmd := LookupTree(func, List(
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
  io.dcache.req.bits.data := src2
  io.dcache.req.bits.mask := DontCare
  
  io.dcache.req.bits               := DontCare
  io.dcache.req.bits.meta.id       := DCacheMiscType.misc
  io.dcache.req.bits.meta.paddr    := paddr
  io.dcache.req.bits.meta.tlb_miss := false.B
  io.dcache.req.bits.meta.replay   := false.B

  io.dcache.resp.ready := true.B
  io.dcache.s1_kill := false.B
  
  // wait for cache result

  io.out.bits.uop := uop
  io.out.bits.data := io.dcache.resp.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect := DontCare
  io.out.bits.brUpdate := DontCare
  io.out.bits.debug.isMMIO := AddressSpace.isMMIO(paddr)
  io.out.bits.uop.cf.exceptionVec := exceptionVec
  XSDebug(io.out.fire(), "misc writeback: pc %x data %x\n", io.out.bits.uop.cf.pc, io.dcache.resp.bits.data)
  
  io.in.ready := state === s_tlb && io.dtlb.resp.fire() && !io.dtlb.resp.bits.miss
  // io.out.valid := io.dcache.resp.fire() && io.dcache.resp.bits.meta.id === DCacheMiscType.misc || io.in.valid && hasException && io.dtlb.resp.fire()
  io.out.valid := io.dcache.resp.fire() && io.dcache.resp.bits.meta.id === DCacheMiscType.misc// || io.in.valid && hasException && io.dtlb.resp.fire()

  // TODO: distinguish L/S/A inst, A inst should not be sent into lsroq
}
