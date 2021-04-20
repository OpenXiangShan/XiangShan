package xiangshan.cache.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

trait HasPrefetchParameters extends HasXSParameter {
  val bopParams = p(BOPParamsKey)
  val streamParams = p(StreamParamsKey)
}


abstract class PrefetchModule(implicit p: Parameters) extends XSModule with HasPrefetchParameters
abstract class PrefetchBundle(implicit p: Parameters) extends XSBundle with HasPrefetchParameters

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val addr = UInt(PAddrBits.W)
  val write = Bool()

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} w=${write}"
  }
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  
}

class PrefetchFinish(implicit p: Parameters) extends PrefetchBundle {
  
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  val addr = UInt(PAddrBits.W)
  val write = Bool()
  val miss = Bool() // TODO: delete this

  override def toPrintable: Printable = {
    p"addr=0x${Hexadecimal(addr)} w=${write} miss=${miss}"
  }
}
