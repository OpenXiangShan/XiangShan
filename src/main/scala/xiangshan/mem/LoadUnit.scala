package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{DCacheLoadIO, DtlbToLsuIO, MemoryOpConstants}

class LoadToLsroqIO extends XSBundle {
  val loadIn = ValidIO(new LsPipelineBundle)
  val ldout = Flipped(DecoupledIO(new ExuOutput))
  val forward = new LoadForwardQueryIO
}

class LoadUnit extends XSModule {
  val io = IO(new Bundle() {
    val ldin = Flipped(Decoupled(new ExuInput))
    val ldout = Decoupled(new ExuOutput)
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcache = Flipped(new DCacheLoadIO)
    val dtlb = Flipped(new DtlbToLsuIO)
    val sbuffer = new LoadForwardQueryIO
    val lsroq = new LoadToLsroqIO
  })

  //-------------------------------------------------------
  // Load Pipeline
  //-------------------------------------------------------

  val l2_out = Wire(Decoupled(new LsPipelineBundle))
  val l4_out = Wire(Decoupled(new LsPipelineBundle))
  val l5_in  = Wire(Flipped(Decoupled(new LsPipelineBundle)))

  XSDebug(l2_out.valid, "L2: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x\n",
    l2_out.bits.uop.cf.pc, l2_out.bits.vaddr, l2_out.bits.paddr, l2_out.bits.uop.ctrl.fuOpType, l2_out.bits.data, l2_out.bits.mask)
  XSDebug(l4_out.valid, "L4: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x\n",
    l4_out.bits.uop.cf.pc, l4_out.bits.vaddr, l4_out.bits.paddr, l4_out.bits.uop.ctrl.fuOpType, l4_out.bits.data, l4_out.bits.mask)
  XSDebug(l5_in.valid, "L5: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x\n",
    l5_in.bits.uop.cf.pc,  l5_in.bits.vaddr , l5_in.bits.paddr , l5_in.bits.uop.ctrl.fuOpType , l5_in.bits.data,  l5_in.bits.mask )
  XSDebug(l2_out.fire(), "load req: pc 0x%x addr 0x%x -> 0x%x op %b\n",
    l2_out.bits.uop.cf.pc, l2_out.bits.vaddr, l2_out.bits.paddr, l2_out.bits.uop.ctrl.fuOpType)

  //-------------------------------------------------------
  // LD Pipeline Stage 2
  // Generate addr, use addr to query DCache Tag and DTLB
  //-------------------------------------------------------

  // l2_out is used to generate dcache req
  l2_out.bits := DontCare
  l2_out.bits.vaddr := io.ldin.bits.src1 + io.ldin.bits.uop.ctrl.imm
  l2_out.bits.paddr := io.dtlb.resp.bits.paddr
  l2_out.bits.uop := io.ldin.bits.uop
  l2_out.bits.mask := genWmask(l2_out.bits.vaddr, io.ldin.bits.uop.ctrl.fuOpType(1,0))
  l2_out.valid := io.ldin.valid && !io.ldin.bits.uop.needFlush(io.redirect)
  l2_out.ready := io.dcache.req.ready
  io.ldin.ready := l2_out.ready

  // send req to dtlb
  io.dtlb.req.valid := l2_out.valid
  io.dtlb.req.bits.vaddr := l2_out.bits.vaddr
  
  // send result to dcache
  io.dcache.req.valid     := io.dtlb.resp.valid && !io.dtlb.resp.bits.miss

  io.dcache.req.bits.cmd  := MemoryOpConstants.M_XRD
  io.dcache.req.bits.addr := io.dtlb.resp.bits.paddr
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := l2_out.bits.mask

  io.dcache.req.bits.meta.id       := DontCare
  io.dcache.req.bits.meta.vaddr    := l2_out.bits.vaddr
  io.dcache.req.bits.meta.paddr    := io.dtlb.resp.bits.paddr
  io.dcache.req.bits.meta.uop      := l2_out.bits.uop
  io.dcache.req.bits.meta.mmio     := AddressSpace.isMMIO(io.dcache.req.bits.meta.paddr)
  io.dcache.req.bits.meta.tlb_miss := io.dtlb.resp.bits.miss
  io.dcache.req.bits.meta.mask     := l2_out.bits.mask
  io.dcache.req.bits.meta.replay   := false.B


  val l2_tlbFeedback = Wire(new TlbFeedback)
  l2_tlbFeedback.hit := !io.dtlb.resp.bits.miss
  l2_tlbFeedback.roqIdx := l2_out.bits.uop.roqIdx

  //-------------------------------------------------------
  // LD Pipeline Stage 3
  // Compare tag, use addr to query DCache Data
  //-------------------------------------------------------

  val l3_tlbFeedback = RegNext(l2_tlbFeedback)
  val l3_valid = RegNext(l2_out.fire(), false.B)
  io.tlbFeedback.valid := l3_valid
  io.tlbFeedback.bits := l3_tlbFeedback

  // Done in Dcache

  //-------------------------------------------------------
  // LD Pipeline Stage 4
  // Dcache return result, do tag ecc check and forward check
  //-------------------------------------------------------

  // result from dcache
  io.dcache.resp.ready := true.B
  l4_out.bits := DontCare
  l4_out.bits.data  := io.dcache.resp.bits.data
  l4_out.bits.paddr := io.dcache.resp.bits.meta.paddr
  l4_out.bits.uop   := io.dcache.resp.bits.meta.uop
  l4_out.bits.mmio  := io.dcache.resp.bits.meta.mmio
  l4_out.bits.mask  := io.dcache.resp.bits.meta.mask
  l4_out.valid      := io.dcache.resp.valid && !l4_out.bits.uop.needFlush(io.redirect)

  // Store addr forward match
  // If match, get data / fmask from store queue / store buffer

  io.lsroq.forward.paddr := l4_out.bits.paddr
  io.lsroq.forward.mask := io.dcache.resp.bits.meta.mask
  io.lsroq.forward.lsroqIdx := l4_out.bits.uop.lsroqIdx
  io.lsroq.forward.pc := l4_out.bits.uop.cf.pc
  io.lsroq.forward.valid := io.dcache.resp.valid //TODO: opt timing

  io.sbuffer.paddr := l4_out.bits.paddr
  io.sbuffer.mask := io.dcache.resp.bits.meta.mask
  io.sbuffer.lsroqIdx := l4_out.bits.uop.lsroqIdx
  io.sbuffer.pc := l4_out.bits.uop.cf.pc
  io.sbuffer.valid := l4_out.valid

  val forwardVec = WireInit(io.lsroq.forward.forwardData)
  val forwardMask = WireInit(io.lsroq.forward.forwardMask)
  // generate XLEN/8 Muxs
  (0 until XLEN/8).map(j => {
    when(io.sbuffer.forwardMask(j)) {
      forwardMask(j) := true.B
      forwardVec(j) := io.sbuffer.forwardData(j)
    }
  })
  l4_out.bits.forwardMask := forwardMask
  l4_out.bits.forwardData := forwardVec
  
  PipelineConnect(l4_out, l5_in, io.ldout.fire(), false.B)

  //-------------------------------------------------------
  // LD Pipeline Stage 5
  // Do data ecc check, merge result and write back to LS ROQ
  // If cache hit, return writeback result to CDB
  //-------------------------------------------------------

  val loadWriteBack = l5_in.fire()

  // data merge
  val rdata = VecInit((0 until 8).map(j => {
    Mux(l5_in.bits.forwardMask(j),
      l5_in.bits.forwardData(j),
      l5_in.bits.data(8*(j+1)-1, 8*j)
    )
  })).asUInt
  val func = l5_in.bits.uop.ctrl.fuOpType
  val raddr = l5_in.bits.paddr
  val rdataSel = LookupTree(raddr(2, 0), List(
    "b000".U -> rdata(63, 0),
    "b001".U -> rdata(63, 8),
    "b010".U -> rdata(63, 16),
    "b011".U -> rdata(63, 24),
    "b100".U -> rdata(63, 32),
    "b101".U -> rdata(63, 40),
    "b110".U -> rdata(63, 48),
    "b111".U -> rdata(63, 56)
  ))
  val rdataPartialLoad = LookupTree(func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN),
      LSUOpType.ldu  -> ZeroExt(rdataSel(63, 0), XLEN)
  ))

  // ecc check
  // TODO

  // if hit, writeback result to CDB
  // val ldout = Vec(2, Decoupled(new ExuOutput))
  // when io.loadIn(i).fire() && !io.io.loadIn(i).miss, commit load to cdb
  val hitLoadOut = Wire(Decoupled(new ExuOutput))
  hitLoadOut.bits.uop := l5_in.bits.uop
  hitLoadOut.bits.data := rdataPartialLoad
  hitLoadOut.bits.redirectValid := false.B
  hitLoadOut.bits.redirect := DontCare
  hitLoadOut.bits.brUpdate := DontCare
  hitLoadOut.bits.debug.isMMIO := l5_in.bits.mmio
  hitLoadOut.valid := l5_in.valid
  XSDebug(hitLoadOut.fire(), "load writeback: pc %x data %x (%x + %x(%b))\n",
    hitLoadOut.bits.uop.cf.pc, rdataPartialLoad, l5_in.bits.data,
    l5_in.bits.forwardData.asUInt, l5_in.bits.forwardMask.asUInt
  )

  // writeback to LSROQ
  // Current dcache use MSHR

  io.lsroq.loadIn.bits := l5_in.bits
  io.lsroq.loadIn.bits.data := rdataPartialLoad // for debug
  io.lsroq.loadIn.valid := loadWriteBack

  // pipeline control
  l5_in.ready := io.ldout.ready

  io.lsroq.ldout.ready := false.B // TODO
  // TODO: writeback missed loads

  val cdbArb = Module(new Arbiter(new ExuOutput, 2))
  io.ldout <> cdbArb.io.out
  hitLoadOut <> cdbArb.io.in(0)
  io.lsroq.ldout <> cdbArb.io.in(1) // missLoadOut
}
