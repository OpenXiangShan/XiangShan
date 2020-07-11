package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

class DtlbIO extends XSBundle with HasMEMConst {
  val tlbload = Vec(LoadPipelineWidth, Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth)))
  val tlbstore = Vec(LoadPipelineWidth, Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth)))
//   val l2cache = TODO
}

class Dtlb extends XSModule with NeedImpl{
  val io = IO(new DtlbIO)
  // Dtlb has 4 ports: 2 for load, 2 fore store 
}