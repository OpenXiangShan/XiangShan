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
  val ftqOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target       = UInt(VAddrBits.W)
  val oversize     = Bool()

  override def toPrintable: Printable = {
    p"[start] ${Hexadecimal(startAddr)} [pft] ${Hexadecimal(fallThruAddr)}" +
      p"[tgt] ${Hexadecimal(target)} [ftqIdx] $ftqIdx [jmp] v:${ftqOffset.valid}" +
      p" offset: ${ftqOffset.bits}\n"
  }
}

class PredecodeWritebackBundle(implicit p:Parameters) extends XSBundle {
  val pc           = Vec(PredictWidth, UInt(VAddrBits.W))
  val pd           = Vec(PredictWidth, new PreDecodeInfo) // TODO: redefine Predecode
  val ftqIdx       = new FtqPtr
  val ftqOffset    = UInt(log2Ceil(PredictWidth).W)
  val misOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val cfiOffset    = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target       = UInt(VAddrBits.W)
  val jalTarget    = UInt(VAddrBits.W)
  val instrRange   = Vec(PredictWidth, Bool())
}

class Exception(implicit p: Parameters) extends XSBundle {

}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(PredictWidth, UInt(32.W))
  val valid     = UInt(PredictWidth.W)
  val pd        = Vec(PredictWidth, new PreDecodeInfo)
  val pc        = Vec(PredictWidth, UInt(VAddrBits.W))
  val foldpc    = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  //val exception = new Exception
  val ftqPtr       = new FtqPtr
  val ftqOffset    = Vec(PredictWidth, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))

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
  val taken_mask = Vec(numBr, Bool())
  val is_br = Vec(numBr, Bool())
  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val call_is_rvc = Bool()
  val target = UInt(VAddrBits.W)
  val hit = Bool()

  def taken = taken_mask.reduce(_||_) // || (is_jal || is_jalr)
  def real_taken_mask(): Vec[Bool] = {
    Mux(hit,
      VecInit(taken_mask.zip(is_br).map{ case(m, b) => m && b } :+ (is_jal || is_jalr)),
      VecInit(Seq.fill(numBr+1)(false.B)))
  }

  def real_br_taken_mask(): Vec[Bool] = {
    Mux(hit,
      VecInit(taken_mask.zip(is_br).map{ case(m, b) => m && b }),
      VecInit(Seq.fill(numBr)(false.B)))
  }
  def hit_taken_on_call = !VecInit(real_taken_mask.take(numBr)).asUInt.orR && hit && is_call
  def hit_taken_on_ret  = !VecInit(real_taken_mask.take(numBr)).asUInt.orR && hit && is_ret

  // override def toPrintable: Printable = {
  //   p"-----------BranchPrediction----------- " +
  //     p"[taken_mask] ${Binary(taken_mask.asUInt)} " +
  //     p"[is_br] ${Binary(is_br.asUInt)}, [is_jal] ${Binary(is_jal.asUInt)} " +
  //     p"[is_jalr] ${Binary(is_jalr.asUInt)}, [is_call] ${Binary(is_call.asUInt)}, [is_ret] ${Binary(is_ret.asUInt)} " +
  //     p"[target] ${Hexadecimal(target)}}, [hit] $hit "
  // }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[taken_mask] ${Binary(taken_mask.asUInt)} [is_br] ${Binary(is_br.asUInt)} [is_jal] $is_jal [is_jalr] $is_jalr [is_call] $is_call [is_ret] $is_ret\n")
    XSDebug(cond, p"[hit] $hit [target] ${Hexadecimal(target)}\n")
  }
}

class BranchPredictionBundle(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  // val hit = Bool()
  val preds = new BranchPrediction

  val ghist = new GlobalHistory()
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val specCnt = Vec(numBr, UInt(10.W))
  // val meta = UInt(MaxMetaLength.W)

  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq

  // override def toPrintable: Printable = {
  //   p"-----------BranchPredictionBundle----------- " +
  //     p"[pc] ${Hexadecimal(pc)} " +
  //     p"[ghist] ${Binary(ghist.predHist)}  " +
  //     preds.toPrintable +
  //     ftb_entry.toPrintable
  // }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[pc] ${Hexadecimal(pc)}\n")
    XSDebug(cond, p"[ghist] ${Binary(ghist.predHist)}\n")
    preds.display(cond)
    ftb_entry.display(cond)
  }
}

class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle()
  val s2 = new BranchPredictionBundle()
  val s3 = new BranchPredictionBundle()
}

class BPUToFtqBundle(implicit p: Parameters) extends BranchPredictionBundle with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}

object BPUToFtqBundle {
  def apply(resp: BranchPredictionBundle)(implicit p: Parameters): BPUToFtqBundle = {
    val e = Wire(new BPUToFtqBundle())
    e.pc := resp.pc
    e.preds := resp.preds
    e.ghist := resp.ghist
    e.rasSp := resp.rasSp
    e.rasTop := resp.rasTop
    e.specCnt := resp.specCnt
    e.ftb_entry := resp.ftb_entry

    e.meta := DontCare
    e
  }
}

class BranchPredictionUpdate(implicit p: Parameters) extends BranchPredictionBundle with HasBPUConst {
  val mispred_mask = Vec(numBr+1, Bool())
  val false_hit = Bool()
  val new_br_insert_pos = Vec(numBr, Bool())
  val old_entry = Bool()
  val meta = UInt(MaxMetaLength.W)
  // val ghist = new GlobalHistory() This in spec_meta

  // override def toPrintable: Printable = {
  //   p"-----------BranchPredictionUpdate----------- " +
  //     p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] ${Binary(false_hit)} " +
  //     p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)} " +
  //     super.toPrintable +
  //     p"\n"
  // }

  override def display(cond: Bool) {
    XSDebug(cond, p"-----------BranchPredictionUpdate-----------\n")
    XSDebug(cond, p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] $false_hit\n")
    XSDebug(cond, p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)}\n")
    super.display(cond)
    XSDebug(cond, p"--------------------------------------------\n")
  }
}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {
  // override def toPrintable: Printable = {
  //   p"-----------BranchPredictionRedirect----------- " +
  //     p"-----------cfiUpdate----------- " +
  //     p"[pc] ${Hexadecimal(cfiUpdate.pc)} " +
  //     p"[predTaken] ${cfiUpdate.predTaken}, [taken] ${cfiUpdate.taken}, [isMisPred] ${cfiUpdate.isMisPred} " +
  //     p"[target] ${Hexadecimal(cfiUpdate.target)} " +
  //     p"------------------------------- " +
  //     p"[roqPtr] f=${roqIdx.flag} v=${roqIdx.value} " +
  //     p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} " +
  //     p"[ftqOffset] ${ftqOffset} " +
  //     p"[level] ${level}, [interrupt] ${interrupt} " +
  //     p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value} " +
  //     p"[stFtqOffset] ${stFtqOffset} " +
  //     p"\n"

  // }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"-----------BranchPredictionRedirect----------- \n")
    XSDebug(cond, p"-----------cfiUpdate----------- \n")
    XSDebug(cond, p"[pc] ${Hexadecimal(cfiUpdate.pc)}\n")
    XSDebug(cond, p"[pred_taken] ${cfiUpdate.predTaken} [taken] ${cfiUpdate.taken} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[target] ${Hexadecimal(cfiUpdate.target)} \n")
    XSDebug(cond, p"------------------------------- \n")
    XSDebug(cond, p"[roqPtr] f=${roqIdx.flag} v=${roqIdx.value}\n")
    XSDebug(cond, p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} \n")
    XSDebug(cond, p"[ftqOffset] ${ftqOffset} \n")
    XSDebug(cond, p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value}\n")
    XSDebug(cond, p"[stFtqOffset] ${stFtqOffset}\n")
    XSDebug(cond, p"---------------------------------------------- \n")
  }
}
