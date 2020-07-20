package xiangshan.mem.pipeline

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
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

  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))
}

class LoadForwardQueryIO extends XSBundle with HasMEMConst {
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val moqIdx = Output(UInt(MoqIdxWidth.W))
  val pc = Output(UInt(VAddrBits.W)) //for debug

  val forwardMask = Input(Vec(8, Bool()))
  val forwardData = Input(Vec(8, UInt(8.W)))
}

class LduReq extends XSBundle with HasMEMConst {
  val src1 = UInt(VAddrBits.W)
  val src2 = UInt(VAddrBits.W)
  // val func = UInt(6.W)
  val data = UInt(XLEN.W)
  val uop = new MicroOp
  // val moqIdx = UInt(log2Up(LSRoqSize).W)
  // val pc = UInt(VAddrBits.W) //for debug
}

class StuReq extends XSBundle with HasMEMConst {
  val src1 = UInt(VAddrBits.W)
  val src2 = UInt(VAddrBits.W)
  // val func = UInt(6.W)
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
  val rollback = Output(Valid(new Redirect))
  val mcommit = Input(UInt(3.W))
  val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
  val moqIdxs = Output(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
  val dcache = Flipped(new DcacheToLsuIO)
  val dtlb = Flipped(new DtlbToLsuIO)
}

// 2l2s out of order lsu for XiangShan
class Lsu(implicit val p: XSConfig) extends XSModule with HasMEMConst {
  override def toString: String = "Ldu"
  val io = IO(new LsuIO)

  val lsroq = Module(new LsRoq)
  val sbuffer = Module(new FakeSbuffer)
  lsroq.io := DontCare // FIXME
  sbuffer.io := DontCare // FIXME

  lsroq.io.mcommit <> io.mcommit
  lsroq.io.dp1Req <> io.dp1Req
  lsroq.io.moqIdxs <> io.moqIdxs
  io.rollback <> lsroq.io.rollback
  io.dcache.redirect := io.redirect

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

//-------------------------------------------------------
// Load Pipeline
//-------------------------------------------------------

  val l2_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val l4_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val l5_in  = Wire(Vec(2, Flipped(Decoupled(new LsPipelineBundle))))
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
    l2_out(i).bits.uop := io.ldin(i).bits.uop
    l2_out(i).bits.mask := genWmask(l2_out(i).bits.vaddr, io.ldin(i).bits.uop.ctrl.fuOpType)
    l2_out(i).valid := io.ldin(i).valid
    io.ldin(i).ready := l2_out(i).ready
  })

  // send req to dtlb
  (0 until LoadPipelineWidth).map(i => {
    io.dtlb.req(i).valid := l2_out(i).valid
    io.dtlb.req(i).bits.vaddr := l2_out(i).bits.vaddr
  })
  
  // send result to dcache
  (0 until LoadPipelineWidth).map(i => {
    io.dcache.load(i).req.valid := io.dtlb.resp(i).valid && !io.dtlb.resp(i).bits.miss
    io.dcache.load(i).req.bits.vaddr := l2_out(i).bits.vaddr
    io.dcache.load(i).req.bits.paddr := io.dtlb.resp(i).bits.paddr
    io.dcache.load(i).req.bits.miss := io.dtlb.resp(i).bits.miss
    io.dcache.load(i).req.bits.user := DontCare
    io.dcache.load(i).req.bits.user.uop := l2_out(i).bits.uop
  })

  // TODO: TLB miss to load/store issue queue

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
    io.dcache.load(i).resp.ready := true.B
    l4_out(i).bits := DontCare
    l4_out(i).bits.paddr := io.dcache.load(i).resp.bits.paddr
    l4_out(i).bits.data := io.dcache.load(i).resp.bits.data
    l4_out(i).bits.uop := io.dcache.load(i).resp.bits.user.uop
    l4_out(i).bits.mmio := io.dcache.load(i).resp.bits.user.mmio
    l4_out(i).valid := io.dcache.load(i).resp.valid
  })

  // Store addr forward match
  // If match, get data / fmask from store queue / store buffer

  (0 until LoadPipelineWidth).map(i => {

    lsroq.io.forward(i).paddr := l4_out(i).bits.paddr
    lsroq.io.forward(i).mask := io.dcache.load(i).resp.bits.user.mask
    lsroq.io.forward(i).moqIdx := l4_out(i).bits.uop.moqIdx
    lsroq.io.forward(i).pc := l4_out(i).bits.uop.cf.pc
    
    sbuffer.io.forward(i).paddr := l4_out(i).bits.paddr
    sbuffer.io.forward(i).mask := io.dcache.load(i).resp.bits.user.mask
    sbuffer.io.forward(i).moqIdx := l4_out(i).bits.uop.moqIdx
    sbuffer.io.forward(i).pc := l4_out(i).bits.uop.cf.pc
    
    val forwardVec = WireInit(lsroq.io.forward(i).forwardData)
    val forwardMask = WireInit(lsroq.io.forward(i).forwardMask)
    (0 until XLEN/8).map(j => {
      when(sbuffer.io.forward(i).forwardMask(j)){
        forwardMask(j) := true.B
        forwardVec(j) := sbuffer.io.forward(i).forwardData(j)
      }
    // generate XLEN/8 Muxs
    })
    
    l4_out(i).bits.forwardMask := forwardMask
    l4_out(i).bits.forwardData := forwardVec
  })
  
  (0 until LoadPipelineWidth).map(i => {
    PipelineConnect(l4_out(i), l5_in(i), l5_out(i).fire(), l5_in(i).bits.uop.brTag.needFlush(io.redirect))
  })

//-------------------------------------------------------
// LD Pipeline Stage 5
// Do data ecc check, merge result and write back to LS ROQ
// If cache hit, return writeback result to CDB
//-------------------------------------------------------

  val loadWriteBack = (0 until LoadPipelineWidth).map(i => { l5_in(i).valid })
  val loadOut = (0 until LoadPipelineWidth).map(_ => Wire(Decoupled(new ExuOutput)))
  (0 until LoadPipelineWidth).map(i => {
    // data merge
    val rdata = VecInit((0 until 8).map(j => {
      Mux(l5_in(i).bits.forwardMask(j), 
        l5_in(i).bits.forwardData(j), 
        l5_in(i).bits.data(8*(j+1)-1, 8*j)
      )
    })).asUInt
    val func = l5_in(i).bits.uop.ctrl.fuOpType
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
    loadOut(i).bits.uop := l5_in(i).bits.uop
    loadOut(i).bits.data := rdataPartialLoad
    loadOut(i).bits.redirectValid := false.B
    loadOut(i).bits.redirect := DontCare
    loadOut(i).bits.debug.isMMIO := l5_in(i).bits.mmio
    loadOut(i).valid := loadWriteBack(i)

    // writeback to LSROQ
    // Current dcache use MSHR
  })

//-------------------------------------------------------
// Store Pipeline
//-------------------------------------------------------

  val s2_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s3_in  = Wire(Vec(2, Flipped(Decoupled(new LsPipelineBundle))))
  val s3_out = Wire(Vec(2, Decoupled(new LsPipelineBundle)))
  val s4_in  = Wire(Vec(2, Flipped(Decoupled(new LsPipelineBundle))))
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
  
  // send req to dtlb
  val saddr = VecInit((0 until StorePipelineWidth).map(i => {
    io.stin(i).bits.src1 + io.stin(i).bits.src2
  }))

  (0 until StorePipelineWidth).map(i => {
    io.dtlb.req(LoadPipelineWidth + i).bits.vaddr := saddr(i)
    io.dtlb.req(LoadPipelineWidth + i).valid := io.stin(i).valid
  })

  (0 until StorePipelineWidth).map(i => {
    s2_out(i).bits := DontCare
    s2_out(i).bits.vaddr := saddr(i)
    s2_out(i).bits.paddr := io.dtlb.resp(LoadPipelineWidth + i).bits.paddr
    s2_out(i).bits.data := genWdata(io.stin(i).bits.data, io.stin(i).bits.uop.ctrl.fuOpType(1,0))
    s2_out(i).bits.uop := io.stin(i).bits.uop
    s2_out(i).bits.mask := genWmask(s2_out(i).bits.vaddr, io.stin(i).bits.uop.ctrl.fuOpType)
    s2_out(i).valid := io.stin(i).valid && !io.dtlb.resp(LoadPipelineWidth + i).bits.miss
    io.stin(i).ready := s2_out(i).ready
  })

  //TODO: tlb miss to store issue queue

  (0 until StorePipelineWidth).map(i => {
    PipelineConnect(s2_out(i), s3_in(i), s3_out(i).fire(), s3_in(i).bits.uop.brTag.needFlush(io.redirect))
  })

//-------------------------------------------------------
// ST Pipeline Stage 3
// Write paddr to LSROQ
//-------------------------------------------------------

  // get paddr from dtlb, check if rollback is needed

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

  (0 until StorePipelineWidth).map(i => {
    PipelineConnect(s3_out(i), s4_in(i), s4_out(i).fire(), s4_in(i).bits.uop.brTag.needFlush(io.redirect))
  })
  
//-------------------------------------------------------
// ST Pipeline Stage 4
// Store writeback, send store request to store buffer
//-------------------------------------------------------

  (0 until StorePipelineWidth).map(i => {
    // writeback to LSROQ
    s4_out(i).ready := true.B // lsroq is always ready for store writeback
    lsroq.io.storeIn(i).bits := s4_in(i).bits

    // LSROQ to store buffer
    lsroq.io.sbuffer(i) <> sbuffer.io.in(i)
  })
  
//-------------------------------------------------------
// Writeback to CDB
//-------------------------------------------------------

  (0 until 2).map(i => {
    val cdbArb = Module(new Arbiter(new ExuOutput, 2))
    io.out(i) <> cdbArb.io.out
    loadOut(i) <> cdbArb.io.in(0)
    lsroq.io.out(i) <> cdbArb.io.in(1)
  })

//-------------------------------------------------------
// ST Pipeline Async Stage 1
// Read paddr from store buffer, query DTAG in DCache
//-------------------------------------------------------

  sbuffer.io.dcache <> io.dcache.store

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