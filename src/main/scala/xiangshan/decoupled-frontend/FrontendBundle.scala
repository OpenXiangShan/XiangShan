package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FtqToFetch(implicit p:Parameters) extends XSBundle {
  val startAddr = UInt(VAddrBits.W)
  val endOffset = UInt(log2Ceil(FETCHBYTE).W)
  val ftqIdx    = UInt(log2Ceil(FTQSIZE).W)
}

class FetchToFtq(implicit p: Parameters) extends XSBundle {
}

class IMetaToFetch(implicit p: Parameters) extends XSBundle {

}

class IDataToFetch(implicit p: Parameters) extends XSBundle {

}

class FetchToICache(implicit p: Parameters) extends XSBundle {

}

class FetchToMissQueue(implicit p: Parameters) extends XSBudnle {

}

class MissQueueToFetch(implicit p: Parameters) extends XSBundle{

}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs = Vec(PredictWidth, UInt(32.W))
  val mask = UInt(PredictWidth.W)
  val pdmask = UInt(PredictWidth.W)
  //val pc = Vec(PredictWidth, UInt(VAddrBits.W))
  //val foldpc = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  val pd = Vec(PredictWidth, new PreDecodeInfo)
  val ipf = Bool()
  val acf = Bool()
  val crossPageIPFFix = Bool()
  val pred_taken = UInt(PredictWidth.W)
  val ftqPtr = new FtqPtr
}
