package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
// import xiangshan.cache.{DCacheWordIO, TlbRequestIO, TlbCmd, MemoryOpConstants, TlbReq, DCacheLoadReq, DCacheWordResp}
import xiangshan.backend.LSUOpType

class LoadToLsqIO extends XSBundle {
  val loadIn = ValidIO(new LsPipelineBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val fpout = Flipped(DecoupledIO(new ExuOutput))
  val forward = new LoadForwardQueryIO
}

// Load Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbReq = DecoupledIO(new TlbReq)
    val dcacheReq = DecoupledIO(new DCacheLoadReq)
  })

  val s0_uop = io.in.bits.uop
  val s0_vaddr = io.in.bits.src1 + s0_uop.ctrl.imm
  val s0_mask = genWmask(s0_vaddr, s0_uop.ctrl.fuOpType(1,0))

  // query DTLB
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.vaddr := s0_vaddr
  io.dtlbReq.bits.cmd := TlbCmd.read
  io.dtlbReq.bits.roqIdx := s0_uop.roqIdx
  io.dtlbReq.bits.debug.pc := s0_uop.cf.pc

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

  io.out.valid := io.in.valid && io.dcacheReq.ready

  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned

  io.in.ready := !io.in.valid || (io.out.ready && io.dcacheReq.ready)

  XSDebug(io.dcacheReq.fire(),
    p"[DCACHE LOAD REQ] pc ${Hexadecimal(s0_uop.cf.pc)}, vaddr ${Hexadecimal(s0_vaddr)}\n"
  )
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcachePAddr = Output(UInt(PAddrBits.W))
    val sbuffer = new LoadForwardQueryIO
    val lsq = new LoadForwardQueryIO
  })

  val s1_uop = io.in.bits.uop
  val s1_paddr = io.dtlbResp.bits.paddr
  val s1_tlb_miss = io.dtlbResp.bits.miss
  val s1_mmio = !s1_tlb_miss && AddressSpace.isMMIO(s1_paddr) && !io.out.bits.uop.cf.exceptionVec.asUInt.orR
  val s1_mask = io.in.bits.mask

  io.out.bits := io.in.bits // forwardXX field will be updated in s1

  io.dtlbResp.ready := true.B
  // feedback tlb result to RS
  io.tlbFeedback.valid := io.in.valid
  io.tlbFeedback.bits.hit := !s1_tlb_miss
  io.tlbFeedback.bits.roqIdx := s1_uop.roqIdx

  io.dcachePAddr := s1_paddr

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

  io.out.valid := io.in.valid && !s1_tlb_miss
  io.out.bits.paddr := s1_paddr
  io.out.bits.mmio := s1_mmio
  io.out.bits.tlbMiss := s1_tlb_miss
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlbResp.bits.excp.pf.ld

  io.in.ready := !io.in.valid || io.out.ready

}


// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2 extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val fpout = Decoupled(new LsPipelineBundle)
    val dcacheResp = Flipped(DecoupledIO(new DCacheWordResp))
    val lsq = new LoadForwardQueryIO
    val sbuffer = new LoadForwardQueryIO
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
  val rdataPartialLoad = rdataHelper(s2_uop, rdataSel)

  // TODO: ECC check

  io.out.valid := io.in.valid && !s2_uop.ctrl.fpWen
  // Inst will be canceled in store queue / lsq,
  // so we do not need to care about flush in load / store unit's out.valid
  io.out.bits := io.in.bits
  io.out.bits.data := rdataPartialLoad
  io.out.bits.miss := (s2_cache_miss || s2_cache_nack) && !fullForward
  io.out.bits.mmio := io.in.bits.mmio

  io.in.ready := io.out.ready || !io.in.valid

  // merge forward result
  // lsq has higher priority than sbuffer
  io.lsq := DontCare
  io.sbuffer := DontCare
  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    when (io.sbuffer.forwardMask(i)) {
      io.out.bits.forwardMask(i) := true.B
      io.out.bits.forwardData(i) := io.sbuffer.forwardData(i)
    }
    when (io.lsq.forwardMask(i)) {
      io.out.bits.forwardMask(i) := true.B
      io.out.bits.forwardData(i) := io.lsq.forwardData(i)
    }
  }

  XSDebug(io.out.fire(), "[DCACHE LOAD RESP] pc %x rdata %x <- D$ %x + fwd %x(%b)\n",
    s2_uop.cf.pc, rdataPartialLoad, io.dcacheResp.bits.data,
    io.out.bits.forwardData.asUInt, io.out.bits.forwardMask.asUInt
  )

  // setup input for s3 (fp recode & writeback)
  io.fpout.bits := io.out.bits
  io.fpout.valid := io.in.valid && s2_uop.ctrl.fpWen
}

// Load Pipeline Stage 3
// FP recode & writeback
// 
// If FP load miss, it will be writeback to LoadQueue in load_s2
// if hit, FP load will mark that load as finished in lq in s2, then recode & write it back in s3
// 
// Int load will NOT enter this stage
class LoadUnit_S3 extends XSModule with HasLoadHelper {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
  })

  val s3_uop = io.in.bits.uop
  val fpdata = fpRdataHelper(s3_uop, io.in.bits.data)

  io.out.valid := io.in.valid
  io.out.bits := io.in.bits
  io.out.bits.data := fpdata

  io.in.ready := io.out.ready || !io.in.valid
}

class LoadUnit extends XSModule {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val fpout = Decoupled(new ExuOutput)
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
  val load_s3 = Module(new LoadUnit_S3)

  load_s0.io.in <> io.ldin
  load_s0.io.dtlbReq <> io.dtlb.req
  load_s0.io.dcacheReq <> io.dcache.req

  PipelineConnect(load_s0.io.out, load_s1.io.in, true.B, load_s0.io.out.bits.uop.roqIdx.needFlush(io.redirect))

  load_s1.io.dtlbResp <> io.dtlb.resp
  load_s1.io.tlbFeedback <> io.tlbFeedback
  io.dcache.s1_paddr <> load_s1.io.dcachePAddr
  io.dcache.s1_kill := DontCare // FIXME
  load_s1.io.sbuffer <> io.sbuffer
  load_s1.io.lsq <> io.lsq.forward

  PipelineConnect(load_s1.io.out, load_s2.io.in, true.B, load_s1.io.out.bits.uop.roqIdx.needFlush(io.redirect))

  load_s2.io.dcacheResp <> io.dcache.resp
  load_s2.io.lsq.forwardData <> io.lsq.forward.forwardData
  load_s2.io.lsq.forwardMask <> io.lsq.forward.forwardMask
  load_s2.io.sbuffer.forwardData <> io.sbuffer.forwardData
  load_s2.io.sbuffer.forwardMask <> io.sbuffer.forwardMask

  PipelineConnect(load_s2.io.fpout, load_s3.io.in, true.B, load_s2.io.fpout.bits.uop.roqIdx.needFlush(io.redirect))

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s0.io.out.bits.uop.lqIdx.asUInt)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid,
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, lId ${Hexadecimal(load_s1.io.out.bits.uop.lqIdx.asUInt)}, tlb_miss ${io.dtlb.resp.bits.miss}, " +
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}\n")

  // writeback to LSQ
  // Current dcache use MSHR
  // Load queue will be updated at s2 for both hit/miss int/fp load
  io.lsq.loadIn.valid := load_s2.io.out.valid
  io.lsq.loadIn.bits := load_s2.io.out.bits

  // Int load, if hit, will be writebacked at s2
  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := load_s2.io.out.valid &&
    (!load_s2.io.out.bits.miss || load_s2.io.out.bits.uop.cf.exceptionVec.asUInt.orR) &&
    !load_s2.io.out.bits.uop.ctrl.fpWen 
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.brUpdate := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio
  hitLoadOut.bits.fflags := DontCare

  // Fp load, if hit, will be recoded & writebacked at s3
  val fpLoadOut = Wire(Valid(new ExuOutput))
  fpLoadOut.valid := load_s3.io.out.valid
  fpLoadOut.bits.uop := load_s3.io.out.bits.uop
  fpLoadOut.bits.data := load_s3.io.out.bits.data
  fpLoadOut.bits.redirectValid := false.B
  fpLoadOut.bits.redirect := DontCare
  fpLoadOut.bits.brUpdate := DontCare
  fpLoadOut.bits.debug.isMMIO := load_s3.io.out.bits.mmio
  fpLoadOut.bits.fflags := DontCare

  load_s2.io.out.ready := true.B
  load_s3.io.out.ready := true.B

  io.ldout.bits := Mux(hitLoadOut.valid, hitLoadOut.bits, io.lsq.ldout.bits)
  io.ldout.valid := hitLoadOut.valid || io.lsq.ldout.valid
  io.lsq.ldout.ready := !hitLoadOut.valid

  io.fpout.bits := Mux(fpLoadOut.valid, fpLoadOut.bits, io.lsq.fpout.bits)
  io.fpout.valid := fpLoadOut.valid || io.lsq.fpout.valid
  io.lsq.fpout.ready := !fpLoadOut.valid

  when(io.ldout.fire()){
    XSDebug("ldout %x\n", io.ldout.bits.uop.cf.pc)
  }

  when(io.fpout.fire()){
    XSDebug("fpout %x\n", io.fpout.bits.uop.cf.pc)
  }
}