package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr    = UInt(VAddrBits.W)
  val fallThruAddr = UInt(VAddrBits.W)
  val ftqIdx       = new FtqPtr
  val ftqOffset    = Valid(UInt(log2Ceil(32).W))
  val target       = UInt(VAddrBits.W)
}

class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pc           = Vec(16, UInt(VAddrBits.W))
  val pd           = Vec(16, new PreDecodeInfo) // TODO: redefine Predecode
  val ftqIdx       = new FtqPtr
  val ftqOffset    = UInt(log2Ceil(16).W)
  val misOffset    = ValidUndirectioned(UInt(4.W))
  val cfiOffset    = ValidUndirectioned(UInt(4.W))
  val target       = UInt(VAddrBits.W)
}

class Exception(implicit p: Parameters) extends XSBundle {

}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(16, UInt(32.W))
  val valid     = UInt(16.W)
  val pd        = Vec(16, new PreDecodeInfo)
  val pc        = Vec(16, UInt(VAddrBits.W))
  val foldpc    = Vec(16, UInt(MemPredPCWidth.W))
  //val exception = new Exception
  val ftqPtr       = new FtqPtr
  val ftqOffset    = Vec(16, ValidUndirectioned(UInt(log2Ceil(16).W)))

}

// Move from BPU
class GlobalHistory(implicit p: Parameters) extends XSBundle {
  val predHist = UInt(HistoryLength.W)
  def update(sawNTBr: Bool, takenOnBr: Bool, hist: UInt = predHist): GlobalHistory = {
    val g = Wire(new GlobalHistory)
    val shifted = takenOnBr || sawNTBr
    g.predHist := Mux(shifted, (hist << 1) | takenOnBr.asUInt, hist)
    g
  }

  final def === (that: GlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: GlobalHistory): Bool = !(this === that)

  implicit val name = "IFU"
  def debug(where: String) = XSDebug(p"[${where}_GlobalHistory] hist=${Binary(predHist)}\n")
  // override def toString(): String = "histPtr=%d, sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d".format(histPtr, sawNTBr, takenOnBr, saveHalfRVI)
}

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends XSBundle{
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: UInt) = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}
class BranchPrediction(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val taken_mask = Vec(numBr+1, Bool())
  val is_br = Vec(numBr, Bool())
  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val call_is_rvc = Bool()
  val target = UInt(VAddrBits.W)

  def taken = taken_mask.reduce(_||_) // || (is_jal || is_jalr)
}

class BranchPredictionBundle(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val hit = Bool()
  val preds = new BranchPrediction

  val ghist = new GlobalHistory()
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val specCnt = Vec(PredictWidth, UInt(10.W))
  val meta = UInt(MaxMetaLength.W)

  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq
}

class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val s1 = new BranchPredictionBundle()
  val s2 = new BranchPredictionBundle()
  val s3 = new BranchPredictionBundle()
}

class BranchPredictionUpdate(implicit p: Parameters) extends BranchPredictionBundle with HasBPUConst {
  val mispred_mask = Vec(numBr+1, Bool())
  // val ghist = new GlobalHistory() This in spec_meta
}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {}
