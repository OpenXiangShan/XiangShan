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
  
  when(io.ldin.valid){
    XSDebug("load enpipe %x iw %x fw %x\n", io.ldin.bits.uop.cf.pc, io.ldin.bits.uop.ctrl.rfWen, io.ldin.bits.uop.ctrl.fpWen)
  }

  //-------------------------------------------------------
  // Load Pipeline
  //-------------------------------------------------------

  val l2_out = Wire(Decoupled(new LsPipelineBundle))
  val l4_out = Wire(Decoupled(new LsPipelineBundle))
  val l5_in  = Wire(Flipped(Decoupled(new LsPipelineBundle)))

  //-------------------------------------------------------
  // LD Pipeline Stage 2
  // Generate addr, use addr to query DCache Tag and DTLB
  //-------------------------------------------------------

  val l2_dtlb_hit  = Wire(new Bool())
  val l2_dtlb_miss = Wire(new Bool())
  val l2_dcache = Wire(new Bool())
  val l2_mmio = Wire(new Bool())
  val isMMIOReq = Wire(new Bool())

  // send req to dtlb
  io.dtlb.req.valid := l2_out.valid
  io.dtlb.req.bits.vaddr := l2_out.bits.vaddr
  io.dtlb.req.bits.cmd := TlbCmd.read
  io.dtlb.req.bits.roqIdx := l2_out.bits.uop.roqIdx
  io.dtlb.req.bits.debug.pc := l2_out.bits.uop.cf.pc
  io.dtlb.req.bits.debug.lsroqIdx := l2_out.bits.uop.lsroqIdx

  l2_dtlb_hit  := io.dtlb.resp.valid && !io.dtlb.resp.bits.miss
  l2_dtlb_miss := io.dtlb.resp.valid && io.dtlb.resp.bits.miss
  isMMIOReq := AddressSpace.isMMIO(io.dtlb.resp.bits.paddr)
  l2_dcache := l2_dtlb_hit && !isMMIOReq
  l2_mmio   := l2_dtlb_hit && isMMIOReq

  // l2_out is used to generate dcache req
  l2_out.bits := DontCare
  l2_out.bits.vaddr := io.ldin.bits.src1 + io.ldin.bits.uop.ctrl.imm
  l2_out.bits.paddr := io.dtlb.resp.bits.paddr
  l2_out.bits.mask  := genWmask(l2_out.bits.vaddr, io.ldin.bits.uop.ctrl.fuOpType(1,0))
  l2_out.bits.uop   := io.ldin.bits.uop
  l2_out.bits.miss  := false.B
  l2_out.bits.mmio  := l2_mmio
  l2_out.valid := io.ldin.valid && !io.ldin.bits.uop.needFlush(io.redirect)
  // when we are sure it's a MMIO req, we do not need to wait for cache ready
  l2_out.ready := (l2_dcache && io.dcache.req.ready) || l2_mmio || l2_dtlb_miss
  io.ldin.ready := l2_out.ready

  // exception check
  val addrAligned = LookupTree(io.ldin.bits.uop.ctrl.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (l2_out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (l2_out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (l2_out.bits.vaddr(2,0) === 0.U)  //d
  ))
  l2_out.bits.uop.cf.exceptionVec(loadAddrMisaligned) := !addrAligned
  l2_out.bits.uop.cf.exceptionVec(loadPageFault) := io.dtlb.resp.bits.excp.pf.ld

  // send result to dcache
  // never send tlb missed or MMIO reqs to dcache
  io.dcache.req.valid     := l2_dcache

  io.dcache.req.bits.cmd  := MemoryOpConstants.M_XRD
  // TODO: vaddr
  io.dcache.req.bits.addr := io.dtlb.resp.bits.paddr 
  io.dcache.req.bits.data := DontCare
  io.dcache.req.bits.mask := l2_out.bits.mask

  io.dcache.req.bits.meta.id       := DontCare
  io.dcache.req.bits.meta.vaddr    := l2_out.bits.vaddr
  io.dcache.req.bits.meta.paddr    := io.dtlb.resp.bits.paddr
  io.dcache.req.bits.meta.uop      := l2_out.bits.uop
  io.dcache.req.bits.meta.mmio     := isMMIOReq
  io.dcache.req.bits.meta.tlb_miss := io.dtlb.resp.bits.miss
  io.dcache.req.bits.meta.mask     := l2_out.bits.mask
  io.dcache.req.bits.meta.replay   := false.B


  val l2_tlbFeedback = Wire(new TlbFeedback)
  l2_tlbFeedback.hit := !io.dtlb.resp.bits.miss
  l2_tlbFeedback.roqIdx := l2_out.bits.uop.roqIdx

  // dump l2
  XSDebug(l2_out.valid, "L2: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x dltb_miss %b dcache %b mmio %b\n",
    l2_out.bits.uop.cf.pc, l2_out.bits.vaddr, l2_out.bits.paddr,
    l2_out.bits.uop.ctrl.fuOpType, l2_out.bits.data, l2_out.bits.mask,
    l2_dtlb_miss, l2_dcache, l2_mmio)

  XSDebug(l2_out.fire(), "load req: pc 0x%x addr 0x%x -> 0x%x op %b\n",
    l2_out.bits.uop.cf.pc, l2_out.bits.vaddr, l2_out.bits.paddr, l2_out.bits.uop.ctrl.fuOpType)

  XSDebug(io.dcache.req.valid, p"dcache req(${io.dcache.req.valid} ${io.dcache.req.ready}): pc:0x${Hexadecimal(io.dcache.req.bits.meta.uop.cf.pc)} roqIdx:${io.dcache.req.bits.meta.uop.roqIdx} lsroqIdx:${io.dcache.req.bits.meta.uop.lsroqIdx} addr:0x${Hexadecimal(io.dcache.req.bits.addr)} vaddr:0x${Hexadecimal(io.dcache.req.bits.meta.vaddr)} paddr:0x${Hexadecimal(io.dcache.req.bits.meta.paddr)} mmio:${io.dcache.req.bits.meta.mmio} tlb_miss:${io.dcache.req.bits.meta.tlb_miss} mask:${io.dcache.req.bits.meta.mask}\n")

  //-------------------------------------------------------
  // LD Pipeline Stage 3
  // Compare tag, use addr to query DCache Data
  //-------------------------------------------------------

  val l3_valid = RegNext(l2_out.fire(), false.B)
  val l3_dtlb_miss = RegEnable(next = l2_dtlb_miss, enable = l2_out.fire(), init = false.B)
  val l3_dcache = RegEnable(next = l2_dcache, enable = l2_out.fire(), init = false.B)
  val l3_tlbFeedback = RegEnable(next = l2_tlbFeedback, enable = l2_out.fire())
  val l3_bundle = RegEnable(next = l2_out.bits, enable = l2_out.fire())
  val l3_uop = l3_bundle.uop
  // dltb miss reqs ends here
  val l3_passdown = l3_valid && !l3_dtlb_miss && !l3_uop.needFlush(io.redirect)

  io.tlbFeedback.valid := l3_valid
  io.tlbFeedback.bits := l3_tlbFeedback
  io.dcache.s1_kill := l3_valid && l3_dcache && l3_uop.needFlush(io.redirect)

  // dump l3
  XSDebug(l3_valid, "l3: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x dltb_miss %b dcache %b mmio %b\n",
    l3_bundle.uop.cf.pc, l3_bundle.vaddr, l3_bundle.paddr,
    l3_bundle.uop.ctrl.fuOpType, l3_bundle.data, l3_bundle.mask,
    l3_dtlb_miss, l3_dcache, l3_bundle.mmio)

  XSDebug(io.tlbFeedback.valid, "tlbFeedback: hit %b roqIdx %d\n",
    io.tlbFeedback.bits.hit, io.tlbFeedback.bits.roqIdx)

  XSDebug(io.dcache.s1_kill, "l3: dcache s1_kill\n")

  // Done in Dcache

  //-------------------------------------------------------
  // LD Pipeline Stage 4
  // Dcache return result, do tag ecc check and forward check
  //-------------------------------------------------------

  val l4_valid = RegNext(l3_passdown, false.B)
  val l4_dcache = RegNext(l3_dcache, false.B)
  val l4_bundle = RegNext(l3_bundle)

  val fullForward = Wire(Bool())

  assert(!(io.dcache.resp.ready && !io.dcache.resp.valid), "DCache response got lost")
  io.dcache.resp.ready := l4_valid && l4_dcache
  when (io.dcache.resp.fire()) {
    l4_out.bits := DontCare
    l4_out.bits.data  := io.dcache.resp.bits.data
    l4_out.bits.paddr := io.dcache.resp.bits.meta.paddr
    l4_out.bits.uop   := io.dcache.resp.bits.meta.uop
    l4_out.bits.mmio  := io.dcache.resp.bits.meta.mmio
    l4_out.bits.mask  := io.dcache.resp.bits.meta.mask
    // when we can get the data completely from forward
    // we no longer need to access dcache
    // treat nack as miss
    l4_out.bits.miss  := Mux(fullForward, false.B,
      io.dcache.resp.bits.miss || io.dcache.resp.bits.nack)
    XSDebug(io.dcache.resp.fire(), p"DcacheResp(l4): data:0x${Hexadecimal(io.dcache.resp.bits.data)} paddr:0x${Hexadecimal(io.dcache.resp.bits.meta.paddr)} pc:0x${Hexadecimal(io.dcache.resp.bits.meta.uop.cf.pc)} roqIdx:${io.dcache.resp.bits.meta.uop.roqIdx} lsroqIdx:${io.dcache.resp.bits.meta.uop.lsroqIdx} miss:${io.dcache.resp.bits.miss}\n")
  } .otherwise {
    l4_out.bits := l4_bundle
  }
  l4_out.valid := l4_valid && !l4_out.bits.uop.needFlush(io.redirect)

  // Store addr forward match
  // If match, get data / fmask from store queue / store buffer

  io.lsroq.forward.paddr := l4_out.bits.paddr
  io.lsroq.forward.mask := io.dcache.resp.bits.meta.mask
  io.lsroq.forward.lsroqIdx := l4_out.bits.uop.lsroqIdx
  io.lsroq.forward.uop := l4_out.bits.uop
  io.lsroq.forward.pc := l4_out.bits.uop.cf.pc
  io.lsroq.forward.valid := io.dcache.resp.valid //TODO: opt timing

  io.sbuffer.paddr := l4_out.bits.paddr
  io.sbuffer.mask := io.dcache.resp.bits.meta.mask
  io.sbuffer.lsroqIdx := l4_out.bits.uop.lsroqIdx
  io.sbuffer.uop := DontCare
  io.sbuffer.pc := l4_out.bits.uop.cf.pc
  io.sbuffer.valid := l4_out.valid

  val forwardVec = WireInit(io.sbuffer.forwardData)
  val forwardMask = WireInit(io.sbuffer.forwardMask)
  // generate XLEN/8 Muxs
  (0 until XLEN/8).map(j => {
    when(io.lsroq.forward.forwardMask(j)) {
      forwardMask(j) := true.B
      forwardVec(j) := io.lsroq.forward.forwardData(j)
    }
  })
  l4_out.bits.forwardMask := forwardMask
  l4_out.bits.forwardData := forwardVec
  fullForward := (~l4_out.bits.forwardMask.asUInt & l4_out.bits.mask) === 0.U

  PipelineConnect(l4_out, l5_in, io.ldout.fire() || (l5_in.bits.miss || l5_in.bits.mmio) && l5_in.valid, false.B)

  XSDebug(l4_valid, "l4: out.valid:%d pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x forwardData: 0x%x forwardMask: %x dcache %b mmio %b miss:%d\n",
    l4_out.valid, l4_out.bits.uop.cf.pc, l4_out.bits.vaddr, l4_out.bits.paddr,
    l4_out.bits.uop.ctrl.fuOpType, l4_out.bits.data, l4_out.bits.mask,
    l4_out.bits.forwardData.asUInt, l4_out.bits.forwardMask.asUInt, l4_dcache, l4_out.bits.mmio, l4_out.bits.miss)

  XSDebug(l5_in.valid, "L5(%d %d): pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x mask %x forwardData: 0x%x forwardMask: %x\n",
    l5_in.valid, l5_in.ready, l5_in.bits.uop.cf.pc,  l5_in.bits.vaddr, l5_in.bits.paddr,
    l5_in.bits.uop.ctrl.fuOpType , l5_in.bits.data,  l5_in.bits.mask,
    l5_in.bits.forwardData.asUInt, l5_in.bits.forwardMask.asUInt)

  XSDebug(l4_valid, "l4: sbuffer forwardData: 0x%x forwardMask: %x\n",
    io.sbuffer.forwardData.asUInt, io.sbuffer.forwardMask.asUInt)

  XSDebug(l4_valid, "l4: lsroq forwardData: 0x%x forwardMask: %x\n",
    io.lsroq.forward.forwardData.asUInt, io.lsroq.forward.forwardMask.asUInt)

  XSDebug(io.redirect.valid, p"Redirect: excp:${io.redirect.bits.isException} flushPipe:${io.redirect.bits.isFlushPipe} misp:${io.redirect.bits.isMisPred} replay:${io.redirect.bits.isReplay} pc:0x${Hexadecimal(io.redirect.bits.pc)} target:0x${Hexadecimal(io.redirect.bits.target)} brTag:${io.redirect.bits.brTag} l2:${io.ldin.bits.uop.needFlush(io.redirect)} l3:${l3_uop.needFlush(io.redirect)} l4:${l4_out.bits.uop.needFlush(io.redirect)}\n")
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
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
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
  hitLoadOut.valid := l5_in.valid && !l5_in.bits.mmio && !l5_in.bits.miss // MMIO will be done in lsroq
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

  val cdbArb = Module(new Arbiter(new ExuOutput, 2))
  io.ldout <> cdbArb.io.out
  hitLoadOut <> cdbArb.io.in(0)
  io.lsroq.ldout <> cdbArb.io.in(1) // missLoadOut

  when(io.ldout.fire()){
    XSDebug("ldout %x iw %x fw %x\n", io.ldout.bits.uop.cf.pc, io.ldout.bits.uop.ctrl.rfWen, io.ldout.bits.uop.ctrl.fpWen)
  }
}
