package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr    = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val ftqIdx       = UInt(log2Ceil(FTQSIZE).W)
  val ftqOffset    = Valid(UInt(log2Ceil(FETCHWIDTH).W))
  val target       = UInt(VAddrBits.W)
}

class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pd        = Vec(16, new PredecodeInfo) // TODO: redefine Predecode
  val ftqIdx    = UInt(log2Ceil(FTQSIZE).W)
  val ftqOffset = UInt(log2Ceil(FETCHWIDTH).W)
  val misPred   = Bool()
  val target    = UInt(VAddrBits.W)
}

class BpuToFtq(implicit p: Parameters) extends XSBundle {
 val resp = DecoupledIO(new BranchPredictionBundle)
}

class FtqToBpu(implicit p: Parameters) extends XSBundle {
  val update = Flipped(Valid(new BranchPredictionUpdate))
  val redirect = Flipped(Valid(new BranchPredictionRedirect))
}

class FetchToBpu(implicit p: Parameters) extends XSBundle {
  val ifu_redirect = Flipped(Valid(UInt(VAddrBits.W)))
}

class Exception(implicit p: Parameters) extends XSBundle {

}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(PredictWidth, UInt(32.W))
  val mask      = UInt(PredictWidth.W)
  val pd        = Vec(PredictWidth, new PreDecodeInfo)
  val exception = new Exception
  val ftqPtr    = new FtqPtr
}
