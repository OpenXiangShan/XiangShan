package xiangshan.mem.pipeline

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.cache._
import bus.simplebus._

object LSUOpType {
  def lb   = "b000000".U
  def lh   = "b000001".U
  def lw   = "b000010".U
  def ld   = "b000011".U
  def lbu  = "b000100".U
  def lhu  = "b000101".U
  def lwu  = "b000110".U
  def sb   = "b001000".U
  def sh   = "b001001".U
  def sw   = "b001010".U
  def sd   = "b001011".U

  def lr      = "b100010".U
  def sc      = "b100011".U
  def amoswap = "b100001".U
  def amoadd  = "b100000".U
  def amoxor  = "b100100".U
  def amoand  = "b101100".U
  def amoor   = "b101000".U
  def amomin  = "b110000".U
  def amomax  = "b110100".U
  def amominu = "b111000".U
  def amomaxu = "b111100".U
  
  def isStore(func: UInt): Bool = func(3)
  def isAtom(func: UInt): Bool = func(5)

  def atomW = "010".U
  def atomD = "011".U
}

class LsPipelineBundle extends XSBundle with HasMEMConst {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val func = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  // val moqIdx = UInt(log2Up(LSRoqSize).W)
  val uop = new MicroOp

  val miss = Bool()
  val mmio = Bool()
  val rollback = Bool()

  val forwardMask = UInt(8.W)
  val forwardData = UInt(XLEN.W)
}

class LoadForwardQueryIO extends XSBundle with HasMEMConst {
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val moqIdx = Output(UInt(log2Up(LSRoqSize).W))
  val pc = Output(UInt(VAddrBits.W)) //for debug

  val forwardMask = Input(UInt(8.W))
  val forwardData = Input(Vec(8, UInt(8.W)))
}

class LduReq extends XSBundle with HasMEMConst {
  val src1 = UInt(VAddrBits.W)
  val src2 = UInt(VAddrBits.W)
  val func = UInt(6.W)
  val data = UInt(XLEN.W)
  val uop = new MicroOp
  // val moqIdx = UInt(log2Up(LSRoqSize).W)
  // val pc = UInt(VAddrBits.W) //for debug
}

class StuReq extends XSBundle with HasMEMConst {
  val src1 = UInt(VAddrBits.W)
  val src2 = UInt(VAddrBits.W)
  val func = UInt(6.W)
  val data = UInt(XLEN.W)
  val uop = new MicroOp
  // val moqIdx = UInt(log2Up(LSRoqSize).W)
  // val pc = UInt(VAddrBits.W) //for debug
}

class LsuIO extends XSBundle with HasMEMConst {
  val ldin = Vec(2, Flipped(Decoupled(new LduReq)))
  val stin = Vec(2, Flipped(Decoupled(new StuReq)))
  val out = Vec(2, Decoupled(new ExuOutput))
  val redirect = Flipped(ValidIO(new Redirect))
  val dcache = Flipped(new DCacheIO)
  val dtlb = Flipped(new DtlbIO)
  // lsroq
  // sbuffer
}

// 2l2s out of order lsu for XiangShan
class Lsu(implicit val p: XSConfig) extends XSModule with HasMEMConst with NeedImpl{
  override def toString: String = "Ldu"
  val io = IO(new LsuIO)

  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)
  }

  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }

  // store buffer
  // TODO

  // lsroq
  // TODO

//-------------------------------------------------------
// Load Pipeline
//-------------------------------------------------------

  val l2_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val l4_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val l5_in  = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val l5_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  l2_out := DontCare
  l4_out := DontCare
  l5_in := DontCare
  l5_out := DontCare

//-------------------------------------------------------
// LD Pipeline Stage 2
// Generate addr, use addr to query DCache Tag and DTLB
//-------------------------------------------------------

  (0 until LoadPipelineWidth).map(i => {
    l2_out(i).bits := DontCare
    l2_out(i).bits.vaddr := io.ldin(i).bits.src1 + io.ldin(i).bits.src2
    // l2_out(i).bits.data := io.ldin(i).bits.data
    l2_out(i).bits.func := io.ldin(i).bits.func
    l2_out(i).bits.uop.cf.pc := io.ldin(i).bits.uop.cf.pc
    l2_out(i).bits.uop.moqIdx := io.ldin(i).bits.uop.moqIdx
    l2_out(i).valid := io.ldin(i).valid
  })

  // send req to d$tag and dtlb
  // (0 until LoadPipelineWidth).map(i => {
  //   io.dmem.req(i).bits.vaddr := l2_out(i).bits.vaddr
  //   io.dmem.req(i).bits.func := l2_out(i).bits.func
  //   io.dmem.req(i).bits.user := Cat(l2_out(i).bits.pc, l2_out(i).bits.moqIdx)
  //   io.dmem.req(i).valid := l2_out(i).valid
  // } // TODO

//-------------------------------------------------------
// LD Pipeline Stage 3
// Compare tag, use addr to query DCache Data
//-------------------------------------------------------

// Done in Dcache

//-------------------------------------------------------
// LD Pipeline Stage 4
// Dcache return result, do tag ecc check and forward check
//-------------------------------------------------------

  // result from dcache
  (0 until LoadPipelineWidth).map(i => {
    l4_out(i).bits := DontCare
    // l4_out(i).bits.paddr := io.dmem.resp(i).bits.addr
    // l4_out(i).bits.data := io.dmem.resp(i).bits.data
    // l4_out(i).bits.func := io.dmem.resp(i).bits.user ...
    // l4_out(i).bits.pc := io.dmem.resp(i).bits.user ...
    // l4_out(i).moqIdx := io.dmem.resp(i).bits.user ...
    // l4_out(i).valid := io.dmem(i).resp.valid
  })

  // Store addr forward match
  // If match, get data / fmask from store queue / store buffer
  // val dataBackVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  // val dataBack = dataBackVec.asUInt
  val forwardVec = Wire(Vec(8, UInt(8.W)))
  val forwardWmask = WireInit(0.U(8.W))
  forwardVec := DontCare

  // generate dependency mask
  def getDependencyMask(addr1: UInt, wmask1: UInt, id1: UInt, addr2: UInt, wmask2: UInt, id2: UInt): UInt = {
    0.U // TODO
  }

  // TODO: update forwardVec/forwardWmask accoring to store buffer
  // forwardVec := VecInit(List.tabulate(storeQueueSize)(i => {
  //   i.U < storeHeadPtr && io.dmem.req.bits.addr(PAddrBits-1, log2Up(XLEN/8)) === storeQueue(i).paddr(PAddrBits-1, log2Up(XLEN/8)) && storeQueue(i).valid
  // }))
  // forwardWmask := List.tabulate(storeQueueSize)(i => storeQueue(i).wmask & Fill(XLEN/8, forwardVec(i))).foldRight(0.U)((sum, i) => sum | i)
  // for(j <- (0 to (XLEN/8 - 1))){
  //   dataBackVec(j) := MuxCase( 
  //     // default = dmem.resp.bits.rdata(8*(j+1)-1, 8*j), 
  //     default = 0.U,
  //     mapping = List.tabulate(storeQueueSize)(i => {
  //       (forwardVec(i) && storeQueue(i).wmask(j), storeQueue(i).data(8*(j+1)-1, 8*j))
  //     }).reverse
  //   )
  // }

  // TODO: update forwardVec/forwardWmask accoring to store buffer
  // forwardVec :=
  // forwardWmask :=

  // generate forward mask / data
  // (0 until LoadPipelineWidth).map(i => {
  //   when(s4_out(i).valid){
  //     s4_out(i).forwardMask := forwardWmask
  //     s4_out(i).forwardData := forwardVec.asUInt
  //   }
  // })

  // TODO: add brIdx
  (0 until LoadPipelineWidth).map(i => {
    PipelineConnect(l4_out(i), l5_in(i), l5_out(i).fire(), io.redirect.valid)
  })

//-------------------------------------------------------
// LD Pipeline Stage 5
// Do data ecc check, merge result and write back to LS ROQ
// If cache hit, return writeback result to CDB
//-------------------------------------------------------

  (0 until LoadPipelineWidth).map(i => {
    // data merge
    val rdata = Wire(UInt(XLEN.W))
    rdata := DontCare // TODO
    // val rdata = VecInit((0 until 8).map(j => {
    //   Mux(l5_in(i).bits.forwardMask(j), 
    //     l5_in(i).bits.forwardData(8*(j+1)-1, 8*j), 
    //     l5_in(i).bits.data(8*(j+1)-1, 8*j)
    //   )
    // })).asUInt
    val func = l5_in(i).bits.func
    val raddr = l5_in(i).bits.paddr
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
        LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
        LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
        LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
    ))

    // ecc check
    // TODO

    // if hit, writeback result to CDB
    // val ldout = Vec(2, Decoupled(new ExuOutput))
    // when io.loadIn(i).fire() && !io.io.loadIn(i).miss, commit load to cdb


    // writeback to LSROQ
  })

//-------------------------------------------------------
// Store Pipeline
//-------------------------------------------------------

  val s2_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s3_in  = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s3_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s4_in  = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s4_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  s2_out := DontCare
  s3_in := DontCare
  s3_out := DontCare
  s4_in := DontCare
  s4_out := DontCare

//-------------------------------------------------------
// ST Pipeline Stage 2
// Generate addr, use addr to query DTLB
//-------------------------------------------------------

  // (0 until StorePipelineWidth).map(i => {
  //   s2_out(i).bits := DontCare
  //   s2_out(i).bits.vaddr := io.stin(i).bits.src1 + io.stin(i).bits.src2
  //   s2_out(i).bits.data := io.stin(i).bits.data
  //   s2_out(i).bits.func := io.stin(i).bits.func
  //   s2_out(i).bits.pc := io.stin(i).bits.pc
  //   s2_out(i).bits.moqIdx := io.stin(i).bits.moqIdx
  //   s2_out(i).valid := io.stin(i).valid
  // })

  // send req to dtlb
  // (0 until LoadPipelineWidth).map(i => {
  //   io.dmem.req(i).bits.vaddr := l2_out(i).bits.vaddr
  //   io.dmem.req(i).bits.func := l2_out(i).bits.func
  //   io.dmem.req(i).bits.user := Cat(l2_out(i).bits.pc, l2_out(i).bits.moqIdx)
  //   io.dmem.req(i).valid := l2_out(i).valid
  // } // TODO

//-------------------------------------------------------
// ST Pipeline Stage 3
// Write paddr to LSROQ
//-------------------------------------------------------

  // get paddr from dtlb
  (0 until StorePipelineWidth).map(i => {
    s3_in(i).bits := DontCare
    // TODO
    // s3_in(i).bits.paddr := io.dtlb.store(i).resp.bits.rdata
    // s3_in(i).bits.data := io.dtlb.store(i).resp.bits.user.get
    // s3_in(i).bits.func := io.dtlb.store(i).resp.bits.user.get
    // s3_in(i).bits.pc := io.dtlb.store(i).resp.bits.user.get
    // s3_in(i).bits.moqIdx := io.dtlb.store(i).resp.bits.user.get
    // s3_in(i).valid := io.dtlb.store(i).resp.valid
  })

  // check if rollback is needed
  def needRollback(addr1: UInt, wmask1: UInt, id1: UInt, addr2: UInt, wmask2: UInt, id2: UInt): Bool = {
    false.B //TODO
  }

  (0 until StorePipelineWidth).map(i => {
    val rollback = WireInit(false.B)
    // check if needRollback
    // s3_out(i).bits := DontCare
    // TODO
    s3_out(i).bits.rollback := rollback
  })

  // TODO: add brIdx
  (0 until StorePipelineWidth).map(i => {
    PipelineConnect(s3_out(i), s4_in(i), s4_out(i).fire(), io.redirect.valid)
  })

//-------------------------------------------------------
// ST Pipeline Stage 4
// Store writeback, send store request to store buffer
//-------------------------------------------------------

// writeback to LSROQ

// TODO

// LSROQ writeback

//-------------------------------------------------------
// ST Pipeline Async Stage 1
// Read paddr from store buffer, query DTAG in DCache
//-------------------------------------------------------

// LSROQ to store buffer

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