package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{DCacheWordIO, TlbRequestIO, TlbCmd, MemoryOpConstants}
import xiangshan.backend.LSUOpType

class LoadToLsroqIO extends XSBundle {
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
    val dtlb = Valid(new TlbReq)
    val dcache = DecoupledIO(new DCacheLoadReq)
  })

  val s0_uop = io.in.bits.uop
  val s0_vaddr = io.in.bits.src1 + s0_uop.ctrl.imm
  val s0_mask = genWmask(s0_vaddr, s0_uop.ctrl.fuOpType(1,0))

  // query DTLB
  io.dtlb.valid := io.out.valid
  io.dtlb.bits.vaddr := s0_vaddr
  io.dtlb.bits.cmd := TlbCmd.read
  io.dtlb.bits.roqIdx := s0_uop.roqIdx
  io.dtlb.bits.debug.pc := s0_uop.cf.pc
  io.dtlb.bits.debug.lsroqIdx := s0_uop.lsroqIdx

  // query DCache
  io.dcache.valid := io.out.valid
  io.dcache.bits.cmd  := MemoryOpConstants.M_XRD
  io.dcache.bits.addr := s0_vaddr
  io.dcache.bits.mask := s0_mask

  val addrAligned = LookupTree(s0_uop.ctrl.fuOpType(1, 0), List(
    "b00".U   -> true.B,                   //b
    "b01".U   -> (s0_vaddr(0)    === 0.U), //h
    "b10".U   -> (s0_vaddr(1, 0) === 0.U), //w
    "b11".U   -> (s0_vaddr(2, 0) === 0.U)  //d
  ))

  io.out.valid := io.in.valid && !s0_uop.needFlush(io.redirect)
  io.out.bits := DontCare
  io.out.bits.vaddr := s0_vaddr
  io.out.bits.mask := s0_mask
  io.out.bits.uop := s0_uop
  io.out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned

  io.in.ready := io.out.ready
}


// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class LoadUnit_S1 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dtlb = Valid(new TlbResp)
    val forward = new LoadForwardQueryIO
    val s1_kill = Output(Bool())
    val s1_paddr = Output(UInt(PAddBits.W))
  })

  val s1_uop = io.in.bits.uop
  val s1_tlb_miss = io.dtlb.resp.bits.miss
  val s1_paddr = io.dtlb.resp.bits.paddr
  val s1_mmio = !s1_tlb_miss && AddressSpace.isMMIO(s1_paddr)

  io.dtlb.ready := io.out.ready

  io.tlbFeedback.valid := io.out.valid
  io.tlbFeedback.bits.hit := !s1_tlb_miss
  io.tlbFeedback.bits.roqIdx := s1_uop.roqIdx

  // if tlb misses or mmio, kill prvious cycles dcache request
  // TODO: kill dcache request when flushed
  io.s1_kill :=  s1_tlb_miss || s1_mmio
  io.s1_paddr :=  s1_paddr

  io.forward.valid := io.out.valid
  io.forward.paddr := s1_paddr
  io.forward.mask := io.in.bits.mask
  io.forward.lsroqIdx := s1_uop.lsroqIdx
  io.forward.sqIdx := s1_uop.sqIdx
  io.forward.uop := s1_uop
  io.forward.pc := s1_uop.cf.pc

  io.out.valid := io.in.valid && !s1_uop.needFlush(io.redirect)
  io.out.bits := io.in.bits
  io.out.bits.paddr := s1_paddr
  io.out.bits.mmio := s1_mmio
  io.out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlb.resp.bits.excp.pf.ld

  io.in.ready := io.out.ready || !io.in.valid

}


// Load Pipeline Stage 2
// DCache resp
class LoadUnit_S2 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val dcache = Flipped(DecoupledIO(new DCacheWordResp))
    val sbuffer = new LoadForwardQueryIO
    val lsroq = new LoadForwardQueryIO
  })

  val s2_uop = io.in.bits.uop
  val s2_mask = io.in.bits.mask
  val s2_paddr = io.in.bits.paddr
  val s2_cache_miss = io.dcache.resp.miss

  io.dcache.ready := true.B
  assert(!(io.in.valid && !io.dcache.resp.valid), "DCache response got lost")

  val forwardMask = WireInit(io.sbuffer.forwardMask)
  val forwardData = WireInit(io.sbuffer.forwardData)
  // generate XLEN/8 Muxs
  for (i <- 0 until XLEN / 8) {
    when(io.lsroq.forwardMask(i)) {
      forwardMask(i) := true.B
      forwardData(i) := io.lsroq.forwardData(i)
    }
  }
  val fullForward = (~forwardMask.asUInt & s2_mask) === 0.U

  // data merge
  val rdata = VecInit((0 until XLEN / 8).map(j => 
    Mux(forwardMask(j), forwardData(j), io.dcache.resp.data(8*(j+1)-1, 8*j)))).asUInt
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
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))

  // TODO: ECC check

  io.out.valid := io.in.valid && !s2_uop.needFlush(io.redirect)
  io.out.bits := io.in.bits
  io.out.bits.data := rdataPartialLoad
  io.out.bits.miss := s2_cache_miss && !fullForward

  io.in.ready := io.out.ready || !io.in.valid

}


class LoadUnit extends XSModule {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcache = new DCacheWordIO
    val dtlb = new TlbRequestIO()
    val sbuffer = new LoadForwardQueryIO
    val lsroq = new LoadToLsroqIO
  })

  val load_s0 = Module(new LoadUnit_S0)
  val load_s1 = Module(new LoadUnit_S1)
  val load_s2 = Module(new LoadUnit_S2)

  load_s0.io.in <> io.ldin
  load_s0.io.redirect <> io.redirect
  load_s0.io.dtlb <> io.dtlb.req
  load_s0.io.dcache <> io.dcache.req

  PipelineConnect(load_s0.io.out, load_s1.io.in, load_s1.io.out.fire(), false.B)

  io.dcache.req.bits.paddr := load_s1.io.out.bits.paddr
  load_s1.io.redirect <> io.redirect
  load_s1.io.tlbFeedback <> io.tlbFeedback
  load_s1.io.dtlb <> io.dtlb.resp
  io.sbuffer <> load_s1.io.forward
  io.lsroq.forward <> load_s1.io.forward

  PipelineConnect(load_s1.io.out, load_s2.io.in, load_s2.io.out.fire(), false.B)

  load_s2.io.redirect <> io.redirect
  load_s2.io.dcache <> io.dcache.resp
  load_s2.io.sbuffer.forwardMask := io.sbuffer.forwardMask
  load_s2.io.sbuffer.forwardData := io.sbuffer.forwardData
  load_s2.io.lsroq.forwardMask := io.lsroq.forward.forwardMask
  load_s2.io.lsroq.forwardData := io.lsroq.forward.forwardData

  XSDebug(load_s0.io.out.valid,
    p"S0: pc ${Hexadecimal(load_s0.io.out.bits.uop.cf.pc)}, " +
    p"vaddr ${Hexadecimal(load_s0.io.out.bits.vaddr)}, mask ${Hexadecimal(load_s0.io.out.bits.mask)}\n")
  XSDebug(load_s1.io.out.valid, 
    p"S1: pc ${Hexadecimal(load_s1.io.out.bits.uop.cf.pc)}, tlb_miss ${io.dtlb.resp.bits.miss}, " + 
    p"paddr ${Hexadecimal(load_s1.io.out.bits.paddr)}, mmio ${load_s1.io.out.bits.mmio}")

  // writeback to LSROQ
  // Current dcache use MSHR
  io.lsroq.loadIn.valid := load_s2.io.out.valid
  io.lsroq.loadIn.bits := load_s2.io.out.bits

  val hitLoadOut = Wire(Valid(new ExuOutput))
  hitLoadOut.valid := load_s2.io.out.valid && !load_s2.io.out.bits.miss
  hitLoadOut.bits.uop := load_s2.io.out.bits.uop
  hitLoadOut.bits.data := load_s2.io.out.bits.data
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.brUpdate := DontCare
  hitLoadOut.bits.debug.isMMIO := load_s2.io.out.bits.mmio

  // TODO: arbiter
  // if hit, writeback result to CDB
  // val ldout = Vec(2, Decoupled(new ExuOutput))
  // when io.loadIn(i).fire() && !io.io.loadIn(i).miss, commit load to cdb
  // val cdbArb = Module(new Arbiter(new ExuOutput, 2))
  // io.ldout <> cdbArb.io.out
  // hitLoadOut <> cdbArb.io.in(0)
  // io.lsroq.ldout <> cdbArb.io.in(1) // missLoadOut
  load_s2.io.out.ready := true.B
  io.lsroq.ldout.ready := !hitLoadOut.valid
  io.ldout.bits := Mux(load_s2.io.out.ready, hitLoadOut.bits, io.lsroq.ldout.bits)

  when(io.ldout.fire()){
    XSDebug("ldout %x iw %x fw %x\n", io.ldout.bits.uop.cf.pc, io.ldout.bits.uop.ctrl.rfWen, io.ldout.bits.uop.ctrl.fpWen)
  }
}
