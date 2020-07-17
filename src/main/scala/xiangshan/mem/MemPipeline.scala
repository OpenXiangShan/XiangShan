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
  val DcacheUserBundleWidth = 64
}

class MemPipeline(implicit val p: XSConfig) extends XSModule with NeedImpl{
  val io = IO(new Bundle{
    val ldin = Vec(2, Flipped(Decoupled(new LduReq)))
    val stin = Vec(2, Flipped(Decoupled(new StuReq)))
    val out = Vec(2, Decoupled(new ExuOutput))
    val redirect = Flipped(ValidIO(new Redirect))
    val rollback = ValidIO(new Redirect)
  })

  val lsu = Module(new Lsu)
  val dcache = Module(new Dcache)
  // val mshq = Module(new MSHQ)
  val dtlb = Module(new Dtlb)

  lsu.io.ldin <> io.ldin
  lsu.io.stin <> io.stin
  lsu.io.out <> io.out
  lsu.io.redirect <> io.redirect
  lsu.io.dcache <> dcache.io.lsu
  lsu.io.dtlb <> dtlb.io.lsu
  dcache.io := DontCare
  dtlb.io := DontCare
  // mshq.io := DontCare

  io.rollback <> lsu.io.rollback

}