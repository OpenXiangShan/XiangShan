package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.{TlbRequestIO, TlbCmd}

// Store Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class LoadUnit_S0 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val dtlbReq = Valid(new TlbReq)
    val dtlbResp = Flipped(Valid(new TlbResp))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dcacheReq = DecoupledIO(new DCacheLoadReq)
  })

  // send req to dtlb
  val saddr = io.in.bits.src1 + io.in.bits.uop.ctrl.imm

  io.dtlbReq.bits.vaddr := saddr
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.cmd := TlbCmd.write
  io.dtlbReq.bits.roqIdx := io.in.bits.uop.roqIdx
  io.dtlbReq.bits.debug.pc := io.in.bits.uop.cf.pc

  io.out.bits := DontCare
  io.out.bits.vaddr := saddr
  io.out.bits.paddr := io.dtlbResp.bits.paddr
  io.out.bits.data := genWdata(io.in.bits.src2, io.in.bits.uop.ctrl.fuOpType(1,0))
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.miss := io.dtlbResp.bits.miss
  io.out.bits.mask := genWmask(io.out.bits.vaddr, io.in.bits.uop.ctrl.fuOpType(1,0))
  io.out.valid := io.in.valid && !io.dtlbResp.bits.miss && !io.out.bits.uop.roqIdx.needFlush(io.redirect)
  io.in.ready := io.out.ready

  // exception check
  val addrAligned = LookupTree(io.in.bits.uop.ctrl.fuOpType(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (io.out.bits.vaddr(0) === 0.U),   //h
    "b10".U   -> (io.out.bits.vaddr(1,0) === 0.U), //w
    "b11".U   -> (io.out.bits.vaddr(2,0) === 0.U)  //d
  ))
  io.out.bits.uop.cf.exceptionVec(storeAddrMisaligned) := !addrAligned
  io.out.bits.uop.cf.exceptionVec(storePageFault) := io.dtlbResp.bits.excp.pf.st

}

// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class StoreUnit_S1 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
  })

  // Send TLB feedback to store issue queue
  io.tlbFeedback.valid := RegNext(io.in.valid && io.out.ready)
  io.tlbFeedback.bits.hit := RegNext(!io.out.bits.miss)
  io.tlbFeedback.bits.roqIdx := RegNext(io.out.bits.uop.roqIdx)
  XSDebug(io.tlbFeedback.valid,
    "S1 Store: tlbHit: %d roqIdx: %d\n",
    io.tlbFeedback.bits.hit,
    io.tlbFeedback.bits.roqIdx.asUInt
  )

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  // writeback to LSQ
  io.in.ready := true.B
  io.lsq.bits := io.in.bits
  io.lsq.bits.miss := false.B
  io.lsq.bits.mmio := AddressSpace.isMMIO(io.in.bits.paddr)
  io.lsq.valid := io.in.fire()

}

class StoreUnit extends XSModule {
  val io = IO(new Bundle() {
    val stin = Flipped(Decoupled(new ExuInput))
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dtlb = new TlbRequestIO()
    val lsq = ValidIO(new LsPipelineBundle)
  })

  val store_s0 = Module(new StoreUnit_S0)
  val store_s1 = Module(new StoreUnit_S1)

  store_s0.io.in <> io.stin
  store_s0.io.redirect <> io.redirect
  store_s0.io.dtlbReq <> io.dtlb.req
  store_s0.io.dtlbResp <> io.dtlbResp

  PipelineConnect(store_s0.io.out, store_s1.io.in, true.B, false.B)

  store_s1.io.redirect <> io.redirect
  store_s1.io.tlbFeedback <> io.tlbFeedback
  // send result to sq
  io.lsq.valid := store_s1.io.out.valid
  io.lsq.bits := store_s1.io.out.bits
  
  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.ctrl.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(store_s0.io.in.bits, store_s0.io.in.bits, "S0")
  printPipeLine(store_s1.io.in.bits, store_s1.io.in.bits, "S1")

}
