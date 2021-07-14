package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr    = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val ftqIdx       = UInt(log2Ceil(48).W)
  val ftqOffset    = Valid(UInt(log2Ceil(32).W))
  val target       = UInt(VAddrBits.W)
}

class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pc          = Vec(16, UInt(VAddrBits.W))
  val pd           = Vec(16, new PreDecodeInfo) // TODO: redefine Predecode
  val ftqIdx       = UInt(log2Ceil(FTQSIZE).W)
  val ftqOffset    = UInt(log2Ceil(FETCHWIDTH).W)
  val misPred      = Bool()
  val jalTarget    = UInt(VAddrBits.W)
  val brTarget     = UInt(VAddrBits.W)
  val jumpOffset  = ValidUndirectioned(UInt(4.W))
  val brOffset    = UInt(4.W)
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
  val instrs    = Vec(16, UInt(32.W))
  val valid     = UInt(16.W)
  val pd        = Vec(PredictWidth, new PreDecodeInfo)
  val exception = new Exception
  val ftqIdx       = UInt(log2Ceil(FTQSIZE).W)
  val ftqOffset    = Valid(UInt(log2Ceil(FETCHWIDTH).W))

}
