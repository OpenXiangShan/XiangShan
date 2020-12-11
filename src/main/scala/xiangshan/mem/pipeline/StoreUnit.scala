package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._

// Store Pipeline Stage 0
// Generate addr, use addr to query DCache and DTLB
class StoreUnit_S0 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new ExuInput))
    val out = Decoupled(new LsPipelineBundle)
    val redirect = Flipped(ValidIO(new Redirect))
    val dtlbReq = DecoupledIO(new TlbReq)
    val dtlbResp = Flipped(DecoupledIO(new TlbResp))
    val tlbFeedback = ValidIO(new TlbFeedback)
  })

  // send req to dtlb
  val saddr = io.in.bits.src1 + io.in.bits.uop.ctrl.imm

  io.dtlbReq.bits.vaddr := saddr
  io.dtlbReq.valid := io.in.valid
  io.dtlbReq.bits.cmd := TlbCmd.write
  io.dtlbReq.bits.roqIdx := io.in.bits.uop.roqIdx
  io.dtlbReq.bits.debug.pc := io.in.bits.uop.cf.pc
  io.dtlbResp.ready := true.B // TODO: why dtlbResp needs a ready?

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

  // Send TLB feedback to store issue queue
  // TODO: should be moved to S1
  io.tlbFeedback.valid := RegNext(io.in.valid && io.out.ready)
  io.tlbFeedback.bits.hit := RegNext(!io.out.bits.miss)
  io.tlbFeedback.bits.roqIdx := RegNext(io.out.bits.uop.roqIdx)
  XSDebug(io.tlbFeedback.valid,
    "S1 Store: tlbHit: %d roqIdx: %d\n",
    io.tlbFeedback.bits.hit,
    io.tlbFeedback.bits.roqIdx.asUInt
  )
}

// Load Pipeline Stage 1
// TLB resp (send paddr to dcache)
class StoreUnit_S1 extends XSModule {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new LsPipelineBundle))
    val out = Decoupled(new LsPipelineBundle)
    // val fp_out = Decoupled(new LsPipelineBundle)
    val stout = DecoupledIO(new ExuOutput) // writeback store
    val redirect = Flipped(ValidIO(new Redirect))
  })

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsq
  // writeback to LSQ
  io.in.ready := true.B
  io.out.bits := io.in.bits
  io.out.bits.miss := false.B
  io.out.bits.mmio := AddressSpace.isMMIO(io.in.bits.paddr)
  io.out.valid := io.in.valid // TODO: && ! FP

  io.stout.bits.uop := io.in.bits.uop
  // io.stout.bits.uop.cf.exceptionVec := // TODO: update according to TLB result
  io.stout.bits.data := DontCare
  io.stout.bits.redirectValid := false.B
  io.stout.bits.redirect := DontCare
  io.stout.bits.brUpdate := DontCare
  io.stout.bits.debug.isMMIO := io.out.bits.mmio
  io.stout.bits.fflags := DontCare

  val hasException = io.out.bits.uop.cf.exceptionVec.asUInt.orR
  io.stout.valid := io.in.valid && (!io.out.bits.mmio || hasException) // mmio inst will be writebacked immediately

  // if fp
  // io.fp_out.valid := ...
  // io.fp_out.bits := ...

}

// class StoreUnit_S2 extends XSModule {
//   val io = IO(new Bundle() {
//     val in = Flipped(Decoupled(new LsPipelineBundle))
//     val out = Decoupled(new LsPipelineBundle)
//     val redirect = Flipped(ValidIO(new Redirect))
//   })

//   io.in.ready := true.B
//   io.out.bits := io.in.bits
//   io.out.valid := io.in.valid && !io.out.bits.uop.roqIdx.needFlush(io.redirect)
// }

class StoreUnit extends XSModule {
  val io = IO(new Bundle() {
    val stin = Flipped(Decoupled(new ExuInput))
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dtlb = new TlbRequestIO()
    val lsq = ValidIO(new LsPipelineBundle)
    val stout = DecoupledIO(new ExuOutput) // writeback store
  })

  val store_s0 = Module(new StoreUnit_S0)
  val store_s1 = Module(new StoreUnit_S1)
  // val store_s2 = Module(new StoreUnit_S2)

  store_s0.io.in <> io.stin
  store_s0.io.redirect <> io.redirect
  store_s0.io.dtlbReq <> io.dtlb.req
  store_s0.io.dtlbResp <> io.dtlb.resp
  store_s0.io.tlbFeedback <> io.tlbFeedback

  PipelineConnect(store_s0.io.out, store_s1.io.in, true.B, false.B)
  // PipelineConnect(store_s1.io.fp_out, store_s2.io.in, true.B, false.B)

  store_s1.io.redirect <> io.redirect
  store_s1.io.stout <> io.stout
  // send result to sq
  io.lsq.valid := store_s1.io.out.valid
  io.lsq.bits := store_s1.io.out.bits

  store_s1.io.out.ready := true.B
  
  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.ctrl.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  printPipeLine(store_s0.io.out.bits, store_s0.io.out.valid, "S0")
  printPipeLine(store_s1.io.out.bits, store_s1.io.out.valid, "S1")

}