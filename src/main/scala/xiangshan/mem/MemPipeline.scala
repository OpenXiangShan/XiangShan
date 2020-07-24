package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
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

  })

  val lsu = Module(new Lsu)
  val dcache = Module(new DCache)
  val mshq = Module(new MSHQ)
  val dtlb = Module(new Dtlb)
  val lsroq = Module(new LsRoq)
  val sbuffer = Module(new Sbuffer)

  lsu.io := DontCare
  dcache.io := DontCare
  mshq.io := DontCare
  dtlb.io := DontCare
  lsroq.io := DontCare
  sbuffer.io := DontCare

}
