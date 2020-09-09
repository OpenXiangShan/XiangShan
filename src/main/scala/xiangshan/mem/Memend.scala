package xiangshan.mem

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils

import xiangshan.cache._
import bus.tilelink.{TLArbiter, TLCached, TLMasterUtilities, TLParameters}

object LSUOpType {
    // normal load/store
    // bit(1, 0) are size
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

    // float/double load store
    def flw  = "b010110".U

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    def lr_w      = "b000010".U
    def sc_w      = "b000110".U
    def amoswap_w = "b001010".U
    def amoadd_w  = "b001110".U
    def amoxor_w  = "b010010".U
    def amoand_w  = "b010110".U
    def amoor_w   = "b011010".U
    def amomin_w  = "b011110".U
    def amomax_w  = "b100010".U
    def amominu_w = "b100110".U
    def amomaxu_w = "b101010".U

    def lr_d      = "b000011".U
    def sc_d      = "b000111".U
    def amoswap_d = "b001011".U
    def amoadd_d  = "b001111".U
    def amoxor_d  = "b010011".U
    def amoand_d  = "b010111".U
    def amoor_d   = "b011011".U
    def amomin_d  = "b011111".U
    def amomax_d  = "b100011".U
    def amominu_d = "b100111".U
    def amomaxu_d = "b101011".U
}

object DCacheAtomicsType {
  def miss      = "b00".U
  def mmio      = "b01".U
  def atomics      = "b10".U
}

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt()
  }
}

object genWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }
}

class LsPipelineBundle extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val func = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val uop = new MicroOp

  val miss = Bool()
  val mmio = Bool()
  val rollback = Bool()

  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))
}

class LoadForwardQueryIO extends XSBundle {
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val lsroqIdx = Output(UInt(LsroqIdxWidth.W))
  val uop = Output(new MicroOp) // for replay
  val pc = Output(UInt(VAddrBits.W)) //for debug
  val valid = Output(Bool()) //for debug

  val forwardMask = Input(Vec(8, Bool()))
  val forwardData = Input(Vec(8, UInt(8.W)))
}

class MemToBackendIO extends XSBundle {
  val ldin = Vec(exuParameters.LduCnt, Flipped(Decoupled(new ExuInput)))
  val stin = Vec(exuParameters.StuCnt, Flipped(Decoupled(new ExuInput)))
  val ldout = Vec(exuParameters.LduCnt, Decoupled(new ExuOutput))
  val stout = Vec(exuParameters.StuCnt, Decoupled(new ExuOutput))
  val redirect = Flipped(ValidIO(new Redirect))
  // replay all instructions form dispatch
  val replayAll = ValidIO(new Redirect)
  // replay mem instructions form Load Queue/Store Queue
  val tlbFeedback = Vec(exuParameters.LduCnt + exuParameters.LduCnt, ValidIO(new TlbFeedback))
  val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
  val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
  val lsroqIdxs = Output(Vec(RenameWidth, UInt(LsroqIdxWidth.W)))
}

class Memend extends XSModule {
  val io = IO(new Bundle{
    val backend = new MemToBackendIO
    val loadUnitToDcacheVec = Vec(exuParameters.LduCnt, new DCacheLoadIO)
    val loadMiss = new DCacheLoadIO
    val atomics  = new DCacheLoadIO
    val sbufferToDcache = new DCacheStoreIO
    val uncache = new DCacheLoadIO
    val ptw = new TlbPtwIO
  })

  // inner modules
  val loadUnits = (0 until exuParameters.LduCnt).map(_ => Module(new LoadUnit))
  val storeUnits = (0 until exuParameters.StuCnt).map(_ => Module(new StoreUnit))
  val atomicsUnit = Module(new AtomicsUnit)
  val dtlb = Module(new TLB(Width = DTLBWidth, isDtlb = true))
  val lsroq = Module(new Lsroq)
  val sbuffer = Module(new Sbuffer)
  // if you wants to stress test dcache store, use FakeSbuffer
  // val sbuffer = Module(new FakeSbuffer)

  // dtlb
  io.ptw <> dtlb.io.ptw

  // LoadUnit
  for (i <- 0 until exuParameters.LduCnt) {
    // get input form dispatch
    loadUnits(i).io.ldin          <> io.backend.ldin(i)
    loadUnits(i).io.ldout         <> io.backend.ldout(i)
    loadUnits(i).io.redirect      <> io.backend.redirect
    loadUnits(i).io.tlbFeedback   <> io.backend.tlbFeedback(i)
    // dtlb access
    loadUnits(i).io.dtlb          <> dtlb.io.requestor(i)
    // dcache access
    loadUnits(i).io.dcache        <> io.loadUnitToDcacheVec(i)
    // forward
    loadUnits(i).io.lsroq.forward <> lsroq.io.forward(i)
    loadUnits(i).io.sbuffer       <> sbuffer.io.forward(i)

    // passdown to lsroq
    lsroq.io.loadIn(i)            <> loadUnits(i).io.lsroq.loadIn
    lsroq.io.ldout(i)             <> loadUnits(i).io.lsroq.ldout
  }

  // StoreUnit
  for (i <- 0 until exuParameters.StuCnt) {
    // get input form dispatch
    storeUnits(i).io.stin        <> io.backend.stin(i)
    storeUnits(i).io.redirect    <> io.backend.redirect
    storeUnits(i).io.tlbFeedback <> io.backend.tlbFeedback(exuParameters.LduCnt + i)

    // dtlb access
    storeUnits(i).io.dtlb        <> dtlb.io.requestor(exuParameters.LduCnt + i) // FIXME

    // passdown to lsroq
    storeUnits(i).io.lsroq       <> lsroq.io.storeIn(i)
  }

  // Lsroq
  lsroq.io.stout       <> io.backend.stout
  lsroq.io.commits     <> io.backend.commits
  lsroq.io.dp1Req      <> io.backend.dp1Req
  lsroq.io.lsroqIdxs   <> io.backend.lsroqIdxs
  lsroq.io.brqRedirect := io.backend.redirect
  io.backend.replayAll <> lsroq.io.rollback

  lsroq.io.dcache      <> io.loadMiss
  lsroq.io.uncache     <> io.uncache

  // LSROQ to store buffer
  lsroq.io.sbuffer     <> sbuffer.io.in

  // Sbuffer
  sbuffer.io.dcache <> io.sbufferToDcache

  // flush sbuffer
  val fenceFlush = WireInit(false.B)
  val atomicsFlush = atomicsUnit.io.flush_sbuffer.valid
  BoringUtils.addSink(fenceFlush, "FenceUnitSbufferFlush")
  val sbEmpty = WireInit(false.B)
  sbEmpty := sbuffer.io.flush.empty
  BoringUtils.addSource(sbEmpty, "SBufferEmpty")
  // if both of them tries to flush sbuffer at the same time
  // something must have gone wrong
  assert(!(fenceFlush && atomicsFlush))
  sbuffer.io.flush.valid := fenceFlush || atomicsFlush

  // TODO: make 0/1 configurable
  // AtomicsUnit
  // AtomicsUnit will override other control signials,
  // as atomics insts (LR/SC/AMO) will block the pipeline
  val st0_atomics = io.backend.stin(0).valid && io.backend.stin(0).bits.uop.ctrl.fuType === FuType.mou
  val st1_atomics = io.backend.stin(1).valid && io.backend.stin(1).bits.uop.ctrl.fuType === FuType.mou

  atomicsUnit.io.dtlb.resp.valid := false.B
  atomicsUnit.io.dtlb.resp.bits  := DontCare
  atomicsUnit.io.out.ready       := false.B

  // dispatch 0 takes priority
  atomicsUnit.io.in.valid := st0_atomics || st1_atomics
  atomicsUnit.io.in.bits  := Mux(st0_atomics, io.backend.stin(0).bits, io.backend.stin(1).bits)
  when (st0_atomics) {
    io.backend.stin(0).ready := atomicsUnit.io.in.ready
    // explitly set st1 ready to false, do not let it fire
    when (st1_atomics) { io.backend.stin(1).ready := false.B }
  }

  when (!st0_atomics && st1_atomics) { io.backend.stin(1).ready := atomicsUnit.io.in.ready }

  // for atomics, do not let them enter store unit
  when (st0_atomics) { storeUnits(0).io.stin.valid := false.B }
  when (st1_atomics) { storeUnits(1).io.stin.valid := false.B }

  when(atomicsUnit.io.dtlb.req.valid) {
    dtlb.io.requestor(0) <> atomicsUnit.io.dtlb // TODO: check it later
    // take load unit 0's tlb port
    // make sure not to disturb loadUnit
    assert(!loadUnits(0).io.dtlb.req.valid)
    loadUnits(0).io.dtlb.resp.valid := false.B
  }
  atomicsUnit.io.dcache        <> io.atomics
  atomicsUnit.io.flush_sbuffer.empty := sbEmpty

  when(atomicsUnit.io.out.valid){
    io.backend.ldout(0) <> atomicsUnit.io.out
    // take load unit 0's write back port
    assert(!loadUnits(0).io.ldout.valid)
    loadUnits(0).io.ldout.ready := false.B
  }
}
