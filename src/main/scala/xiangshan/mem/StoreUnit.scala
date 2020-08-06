package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache.DtlbToLsuIO

class StoreUnit extends XSModule {
  val io = IO(new Bundle() {
    val stin = Flipped(Decoupled(new ExuInput))
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbFeedback = ValidIO(new TlbFeedback)
    val dtlb = Flipped(new DtlbToLsuIO)
    val lsroq = ValidIO(new LsPipelineBundle)
  })

  //-------------------------------------------------------
  // Store Pipeline
  //-------------------------------------------------------
  val s2_out = Wire(Decoupled(new LsPipelineBundle))
  val s3_in  = Wire(Decoupled(new LsPipelineBundle))


  private def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    val uop = pipeline.uop
    XSDebug(cond,
      name + p" pc ${Hexadecimal(uop.cf.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(uop.ctrl.fuOpType)} " +
        p"data ${pipeline.data} " +
        p"mask ${pipeline.mask}\n"
    )
  }

  printPipeLine(s2_out.bits, s2_out.valid, "S2")
  // TODO: is this nesscary ?
  XSDebug(s2_out.fire(), "store req: pc 0x%x addr 0x%x -> 0x%x op %b data 0x%x\n",
    s2_out.bits.uop.cf.pc,
    s2_out.bits.vaddr,
    s2_out.bits.paddr,
    s2_out.bits.uop.ctrl.fuOpType,
    s2_out.bits.data
  )
  printPipeLine(s3_in.bits, s3_in.valid, "S3")


  
  //-------------------------------------------------------
  // ST Pipeline Stage 2
  // Generate addr, use addr to query DTLB
  //-------------------------------------------------------
  
  // send req to dtlb
  val saddr = io.stin.bits.src1 + io.stin.bits.uop.ctrl.imm

  io.dtlb.req.bits.vaddr := saddr
  io.dtlb.req.valid := io.stin.valid

  s2_out.bits := DontCare
  s2_out.bits.vaddr := saddr
  s2_out.bits.paddr := io.dtlb.resp.bits.paddr
  s2_out.bits.data := genWdata(io.stin.bits.src2, io.stin.bits.uop.ctrl.fuOpType(1,0))
  s2_out.bits.uop := io.stin.bits.uop
  s2_out.bits.miss := io.dtlb.resp.bits.miss
  s2_out.bits.mask := genWmask(s2_out.bits.vaddr, io.stin.bits.uop.ctrl.fuOpType(1,0))
  s2_out.valid := io.stin.valid && !io.dtlb.resp.bits.miss
  io.stin.ready := s2_out.ready

  PipelineConnect(s2_out, s3_in, true.B, s3_in.valid && s3_in.bits.uop.needFlush(io.redirect))

  //-------------------------------------------------------
  // ST Pipeline Stage 3
  // Write paddr to LSROQ
  //-------------------------------------------------------

  // Send TLB feedback to store issue queue
  io.tlbFeedback.valid := RegNext(io.stin.valid && s2_out.ready)
  io.tlbFeedback.bits.hit := RegNext(!s2_out.bits.miss)
  io.tlbFeedback.bits.roqIdx := RegNext(s2_out.bits.uop.roqIdx)
  XSDebug(io.tlbFeedback.valid, 
    "S3 Store: tlbHit: %d roqIdx: %d\n",
    io.tlbFeedback.bits.hit,
    io.tlbFeedback.bits.roqIdx
  )

  // get paddr from dtlb, check if rollback is needed
  // writeback store inst to lsroq
  // writeback to LSROQ
  s3_in.ready := true.B
  io.lsroq.bits := s3_in.bits
  io.lsroq.bits.mmio := AddressSpace.isMMIO(s3_in.bits.paddr)
  io.lsroq.valid := s3_in.fire()

  //-------------------------------------------------------
  // ST Pipeline Stage 4
  // Store writeback, send store request to store buffer
  //-------------------------------------------------------

  // Writeback to CDB
  // (0 until LoadPipelineWidth).map(i => {
  //   io.ldout <> hitLoadOut
  // })

  //-------------------------------------------------------
  // ST Pipeline Async Stage 1
  // Read paddr from store buffer, query DTAG in DCache
  //-------------------------------------------------------


  //-------------------------------------------------------
  // ST Pipeline Async Stage 2
  // DTAG compare, write data to DCache
  //-------------------------------------------------------

  // Done in DCache

  //-------------------------------------------------------
  // ST Pipeline Async Stage 2
  // DCache miss / Shared cache wirte
  //-------------------------------------------------------

  // update store buffer according to store fill buffer

}
