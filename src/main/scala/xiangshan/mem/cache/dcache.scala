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

// object CacheOp {
//   def load   = "b00".U
//   def store  = "b01".U
//   def refill = "b11".U
//   def probe  = "b10".U

//   def width  = load.getWidth
// }

class DcacheUserBundle extends XSBundle with HasMEMConst {
  val lsroqId = Output(UInt(log2Up(LSRoqSize).W))
  val pc = Output(UInt(VAddrBits.W)) //for debug
}

class DcacheIO extends XSBundle with HasMEMConst {
  val load = Vec(LoadPipelineWidth, Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth)))
  val store = Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth))
  val refill = Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth))
  val tlbload = Vec(LoadPipelineWidth, Flipped(new SimpleBusUC(addrBits = VAddrBits, userBits = (new DcacheUserBundle).getWidth))) // dcache controls 2 dtlb ports
//   val dmem = TODO
}

class Dcache extends XSModule with NeedImpl{
  val io = IO(new DcacheIO)

  // Arbitor for 2 dcache ports in built in decache
  // store/refill only use port0, port1 is always assigned to load request

  // priority:
  // load
  // store
  // refill
}