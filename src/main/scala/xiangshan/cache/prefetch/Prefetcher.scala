package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

trait HasPrefetchParameters extends HasXSParameter {
  
}

abstract class PrefetchModule extends XSModule with HasPrefetchParameters
abstract class PrefetchBundle extends XSBundle with HasPrefetchParameters

class PrefetchReq extends PrefetchBundle {
  val addr = UInt(PAddrBits.W)
  val write = Bool()
}

class PrefetchResp extends PrefetchBundle {
  
}

class PrefetchTrain extends PrefetchBundle {
  val addr = UInt(PAddrBits.W)
  val write = Bool()
  val miss = Bool() // TODO: delete this
}

class PrefetchIO extends PrefetchBundle {
  val train = Flipped(ValidIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
}

// class FakePrefetcher extends PrefetchModule {
//   val io = IO(new PrefetchIO)

//   io.req.valid := false.B
//   io.req.bits := DontCare
//   io.resp.ready := true.B

//   assert(!io.resp.fire(), "FakePrefetcher should not receive resp")
// }