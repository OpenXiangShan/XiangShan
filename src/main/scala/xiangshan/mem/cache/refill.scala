package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

class MSHQIO extends XSBundle with HasMEMConst {
  val miss = Flipped(Valid(new MissReqIO))
  val refill = Flipped(new DCacheStoreIO)
//   val l2cache = TODO
}

// miss status handling queue
class MSHQ extends XSModule with NeedImpl{
  val io = IO(new MSHQIO)
}