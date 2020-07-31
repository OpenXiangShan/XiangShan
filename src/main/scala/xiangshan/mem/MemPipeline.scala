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

trait HasMEMConst{
  val LoadPipelineWidth = 2
  val StorePipelineWidth = 2
  val LSRoqSize = 64
  val StoreBufferSize = 16
  val RefillSize = 512
  val DcacheUserBundleWidth = (new DcacheUserBundle).getWidth
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
  val mcommit = Input(UInt(3.W))
  val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
  val moqIdxs = Output(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
}

class Memend(implicit val p: XSConfig) extends XSModule with HasMEMConst {
  val io = IO(new Bundle{
    val backend = new MemToBackendIO
    val dmem = new SimpleBusUC(userBits = DcacheUserBundleWidth)
  })

  // io <> DontCare

  val lsu = Module(new Lsu)
  val dcache = Module(new Dcache)
  // val mshq = Module(new MSHQ)
  val dtlb = Module(new Dtlb)

  dcache.io := DontCare
  dtlb.io := DontCare
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
  lsu.io.dtlb <> dtlb.io.lsu
  lsu.io.refill <> DontCare // TODO
  lsu.io.miss <> DontCare //TODO
  
  //  // for ls pipeline test
  dcache.io.dmem <> io.dmem
  dcache.io.lsu.refill <> DontCare
  
}