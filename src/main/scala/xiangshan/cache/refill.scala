package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class MSHQIO extends XSBundle {
  val miss = Flipped(Valid(new MissReqIO))
  val refill = Flipped(new DCacheStoreIO)
//   val l2cache = TODO
}

// miss status handling queue
class MSHQ extends XSModule with NeedImpl{
  val io = IO(new MSHQIO)
}