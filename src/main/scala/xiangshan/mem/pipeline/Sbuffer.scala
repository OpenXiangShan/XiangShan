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

class SbufferUserBundle extends XSBundle with HasMEMConst {
  val pc = UInt(VAddrBits.W) //for debug
  val lsroqId = UInt(log2Up(LSRoqSize).W)
}

// Store buffer for XiangShan Out of Order LSU
class Sbuffer(implicit val p: XSConfig) extends XSModule with HasMEMConst with NeedImpl{
  val io = IO(new Bundle() {
    val in = Vec(StorePipelineWidth, Flipped(new SimpleBusUC(addrBits = PAddrBits, userBits = (new SbufferUserBundle).getWidth)))
    val dcache = new SimpleBusUC(dataBits = L1CacheLineSize, addrBits = PAddrBits, userBits = (new SbufferUserBundle).getWidth)
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
  })
  
}
  