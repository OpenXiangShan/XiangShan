package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem.cache._
import xiangshan.mem.pipeline._
import bus.simplebus._


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
  val mcommit = Flipped(Vec(CommitWidth, Valid(UInt(MoqIdxWidth.W))))
  val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
  val moqIdxs = Output(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
  val csr = Flipped(new TlbCsrIO)
  // val issQue = new TlbIssQueIO
}

class Memend extends XSModule {
  val io = IO(new Bundle{
    val backend = new MemToBackendIO
    val dmem = new SimpleBusUC(userBits = (new DcacheUserBundle).getWidth)
    val pmem = new SimpleBusUC(addrBits = PAddrBits)
  })

  // io <> DontCare

  val lsu = Module(new Lsu)
  val dcache = Module(new Dcache)
  // val mshq = Module(new MSHQ)
  // val dtlb = Module(new FakeDtlb)
  val dtlb = Module(new TLB(DTLBWidth))
  val ptw = Module(new PTW)

  dcache.io := DontCare
  dtlb.io.csr <> io.backend.csr
  ptw.io.tlb(0) <> dtlb.io.ptw
  ptw.io.tlb(1) <> DontCare //mem.io.itlb
  ptw.io.csr <> io.backend.csr // TODO: from backend.csr
  ptw.io.mem <> io.pmem // TODO: ptw mem access
  // mshq.io := DontCare

  lsu.io.ldin <> io.backend.ldin
  lsu.io.stin <> io.backend.stin
  lsu.io.ldout <> io.backend.ldout
  lsu.io.stout <> io.backend.stout
  lsu.io.redirect <> io.backend.redirect
  lsu.io.rollback <> io.backend.replayAll
  lsu.io.tlbFeedback <> io.backend.tlbFeedback
  lsu.io.mcommit <> io.backend.mcommit
  lsu.io.dp1Req <> io.backend.dp1Req
  lsu.io.moqIdxs <> io.backend.moqIdxs
  lsu.io.dcache <> dcache.io.lsu
  lsu.io.dtlb <> dtlb.io.requestor
  lsu.io.refill <> DontCare // TODO
  lsu.io.miss <> DontCare //TODO
  
  //  // for ls pipeline test
  dcache.io.dmem <> io.dmem
  dcache.io.lsu.refill <> DontCare
  
}