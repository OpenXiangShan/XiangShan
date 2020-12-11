package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
// import xiangshan.cache.{DCacheWordIO, TlbRequestIO, TlbCmd, MemoryOpConstants, TlbReq, DCacheLoadReq, DCacheWordResp}
import xiangshan.backend.LSUOpType
import xiangshan.backend.fu.fpu.boxF32ToF64

class LoadToLsqIO extends XSBundle {
  val loadIn = ValidIO(new LsPipelineBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val forward = new LoadForwardQueryIO
}

// Load Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val dtlbReq = DecoupledIO(new TlbReq)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcacheReq = DecoupledIO(new DCacheLoadReq)
  })

  val s0_uop = io.in.bits.uop
  val s0_vaddr = io.in.bits.src1 + s0_uop.ctrl.imm
  val s0_paddr = io.dtlbResp.bits.paddr
  val s0_tlb_miss = io.dtlbResp.bits.miss
  val s0_mask = genWmask(s0_vaddr, s0_uop.ctrl.fuOpType(1,0))

  // query DTLB
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.vaddr := s0_vaddr
  io.dtlbReq.bits.cmd := TlbCmd.read
  io.dtlbReq.bits.roqIdx := s0_uop.roqIdx
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc
  io.dtlbResp.ready := true.B

  // feedback tlb result to RS
  // Note: can be moved to s1
  io.tlbFeedback.valid := io.out.valid
  io.tlbFeedback.bits.hit := !s0_tlb_miss
  io.tlbFeedback.bits.roqIdx := s0_uop.roqIdx

  // query DCache
  io.dcacheReq.valid := io.in.valid
  io.dcacheReq.bits.cmd  := MemoryOpConstants.M_XRD
  io.dcacheReq.bits.addr := s0_vaddr
  io.dcacheReq.bits.mask := s0_mask
  io.dcacheReq.bits.data := DontCare

  // TODO: update cache meta
  io.dcacheReq.bits.meta.id       := DontCare
  io.dcacheReq.bits.meta.vaddr    := s0_vaddr
  io.dcacheReq.bits.meta.paddr    := DontCare
  io.dcacheReq.bits.meta.uop      := s0_uop
  io.dcacheReq.bits.meta.mmio     := false.B
  io.dcacheReq.bits.meta.tlb_miss := false.B
  io.dcacheReq.bits.meta.mask     := s0_mask
  io.dcacheReq.bits.meta.replay   := false.B

  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  io.out.valid := io.in.valid && io.dcacheReq.ready && !s0_uop.roqIdx.needFlush(io.redirect)

  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.paddr := s0_paddr
  io.out.bits.tlbMiss := io.dtlbResp.bits.miss
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp.pf.ld

  io.in.ready := !io.in.valid || (io.out.ready && io.dcacheReq.ready)

  XSDebug(io.dcacheReq.fire(),
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, " +
    p"vaddr ${Hexadecimal(s0_vaddr)}, paddr will be ${Hexadecimal(s0_paddr)}\n"
  )
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val s1_paddr = Output(UInt(PAddrBits.W))
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadForwardQueryIO
  })

  val s1_uop = io.in.bits.uop
  val s1_paddr = io.in.bits.paddr
  val s1_tlb_miss = io.in.bits.tlbMiss
  val s1_mmio = !s1_tlb_miss && AddressSpace.isMMIO(s1_paddr) && !io.out.bits.uop.cf.exceptionVec.asUInt.orR
  val s1_mask = io.in.bits.mask

  io.out.bits := io.in.bits // forwardXX field will be updated in s1
  io.s1_paddr :=  s1_paddr

  // load forward query datapath
  io.sbuffer.valid := io.in.valid
  io.sbuffer.paddr := s1_paddr
  io.sbuffer.uop := s1_uop
  io.sbuffer.sqIdx := s1_uop.sqIdx
  io.sbuffer.mask := s1_mask
  io.sbuffer.pc := s1_uop.cf.pc // FIXME: remove it

  io.lsq.valid := io.in.valid
  io.lsq.paddr := s1_paddr
  io.lsq.uop := s1_uop
  io.lsq.sqIdx := s1_uop.sqIdx
  io.lsq.mask := s1_mask
  io.lsq.pc := s1_uop.cf.pc // FIXME: remove it

  io.out.bits.forwardMask := io.sbuffer.forwardMask
  io.out.bits.forwardData := io.sbuffer.forwardData

  io.out.valid := io.in.valid && !s1_tlb_miss && !s1_uop.roqIdx.needFlush(io.redirect)
  io.out.bits.paddr := s1_paddr
  io.out.bits.mmio := s1_mmio
  io.out.bits.tlbMiss := s1_tlb_miss

  io.in.ready := !io.in.valid || io.out.ready

}


// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    val lsq = new LoadForwardQueryIO
  })

  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_cache_miss = io.dcacheResp.bits.miss
  val s2_cache_nack = io.dcacheResp.bits.nack


  io.dcacheResp.ready := true.B
  assert(!(io.in.valid && !io.dcacheResp.valid), "DCache response got lost")

  val forwardMask = io.out.bits.forwardMask
  val forwardData = io.out.bits.forwardData
  val fullForward = (~forwardMask.asUInt & s2_mask) === 0.U

  XSDebug(io.out.fire(), "[FWD LOAD RESP] pc %x fwd %x(%b) + %x(%b)\n",
    s2_uop.cf.pc,
    io.lsq.forwardData.asUInt, io.lsq.forwardMask.asUInt,
    io.in.bits.forwardData.asUInt, io.in.bits.forwardMask.asUInt
  )

  // data merge
  val rdata = VecInit((0 until XLEN / 8).map(j =>
    Mux(forwardMask(j), forwardData(j), io.dcacheResp.bits.data(8*(j+1)-1, 8*j)))).asUInt
  val rdataSel = LookupTree(s2_paddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = LookupTree(s2_uop.ctrl.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN),
      LSUOpType.flw  -> boxF32ToF64(rdataSel(31, 0))
  ))

  // TODO: ECC check

  io.out.valid := io.in.valid
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  io.out.bits.data := rdataPartialLoad
  io.out.bits.miss := (s2_cache_miss || s2_cache_nack) && !fullForward
  io.out.bits.mmio := io.in.bits.mmio

  io.in.ready := io.out.ready || !io.in.valid

  // merge forward result
  io.lsq := DontCare
  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    when(io.lsq.forwardMask(i)) {
      io.out.bits.forwardMask(i) := true.B
      io.out.bits.forwardData(i) := io.lsq.forwardData(i)
    }
  }

  XSDebug(io.out.fire(), "[DCACHE LOAD RESP] pc %x rdata %x <- D$ %x + fwd %x(%b)\n",
    s2_uop.cf.pc, rdataPartialLoad, io.dcacheResp.bits.data,
    io.out.bits.forwardData.asUInt, io.out.bits.forwardMask.asUInt
  )

}

class LoadUnit extends XSModule {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcache = new DCacheLoadIO
    val dtlb = new TlbRequestIO()
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadToLsqIO
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  load_s0.io.in <> io.ldin
  load_s0.io.redirect <> io.redirect
  load_s0.io.dtlbReq <> io.dtlb.req
  load_s0.io.dtlbResp <> io.dtlb.resp
  load_s0.io.dcacheReq <> io.dcache.req
  load_s0.io.tlbFeedback <> io.tlbFeedback

  PipelineConnect(load_s0.io.out, load_s1.io.in, true.B, false.B)

  io.dcache.s1_paddr := load_s1.io.out.bits.paddr
  load_s1.io.redirect <> io.redirect
  io.dcache.s1_kill := DontCare // FIXME
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward

  PipelineConnect(load_s1.io.out, load_s2.io.in, true.B, false.B)

  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.lsq := DontCare 
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData 
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask 

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.dtlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // writeback to LSQ
  // Current dcache use MSHR
  io.lsq.loadIn.valid := load_s2.io.out.valid
  io.lsq.loadIn.bits := load_s2.io.out.bits

  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := load_s2.io.out.valid && (!load_s2.io.out.bits.miss || load_s2.io.out.bits.uop.cf.exceptionVec.asUInt.orR)
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.brUpdate := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio
  hitLoadOut.bits.fflags := DontCare

  // TODO: arbiter
  // if hit, writeback result to CDB
  // val ldout = Vec(2, Decoupled(new ExuOutput))
  // when io.loadIn(i).fire() && !io.io.loadIn(i).miss, commit load to cdb
  // val cdbArb = Module(new Arbiter(new ExuOutput, 2))
  // io.ldout <> cdbArb.io.out
  // hitLoadOut <> cdbArb.io.in(0)
  // io.lsq.ldout <> cdbArb.io.in(1) // missLoadOut
  load_s2.io.out.ready := true.B
  io.lsq.ldout.ready := !hitLoadOut.valid
  io.ldout.bits := Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.ldout.bits)
  io.ldout.valid := hitLoadOut.valid || io.lsq.ldout.valid

  when(io.ldout.fire()){
    XSDebug("ldout %x iw %x fw %x\n", io.ldout.bits.uop.cf.pc, io.ldout.bits.uop.ctrl.rfWen, io.ldout.bits.uop.ctrl.fpWen)
  }
}