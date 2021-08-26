/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr       = UInt(VAddrBits.W)
  val fallThruAddr    = UInt(VAddrBits.W)
  val fallThruError   = Bool()
  val ftqIdx          = new FtqPtr
  val ftqOffset       = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target          = UInt(VAddrBits.W)
  val oversize        = Bool()

  def fallThroughError() = {
    def carryPos = instOffsetBits+log2Ceil(PredictWidth)+1
    def getLower(pc: UInt) = pc(instOffsetBits+log2Ceil(PredictWidth), instOffsetBits)
    val carry = (startAddr(carryPos) =/= fallThruAddr(carryPos)).asUInt
    val startLower        = Cat(0.U(1.W), getLower(startAddr))
    val endLowerwithCarry = Cat(carry,    getLower(fallThruAddr))
    require(startLower.getWidth == log2Ceil(PredictWidth)+2)
    require(endLowerwithCarry.getWidth == log2Ceil(PredictWidth)+2)
    startLower >= endLowerwithCarry || (endLowerwithCarry - startLower) > (PredictWidth+1).U
  }
  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.fallThruAddr := b.getFallThrough()
    this.oversize := b.oversize
    this
  }
  def fromBpuResp(resp: BranchPredictionBundle) = {
    // only used to bypass, so some fields remains unchanged
    this.startAddr := resp.pc
    this.target := resp.target
    this.ftqOffset := resp.genCfiIndex
    this.fallThruAddr := resp.fallThroughAddr
    this.oversize := resp.ftb_entry.oversize
    this
  }
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
  val ipf          = Vec(PredictWidth, Bool())
  val acf          = Vec(PredictWidth, Bool())
  val crossPageIPFFix = Vec(PredictWidth, Bool())
}

// Move from BPU
class GlobalHistory(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val predHist = UInt(HistoryLength.W)
  // def update(sawNTBr: Bool, takenOnBr: Bool, hist: UInt = predHist): GlobalHistory = {
  //   val g = Wire(new GlobalHistory)
  //   val shifted = takenOnBr || sawNTBr
  //   g.predHist := Mux(shifted, (hist << 1) | takenOnBr.asUInt, hist)
  //   g
  // }

  // def update(brValids: UInt, taken_mask: UInt, hist: UInt = predHist): GlobalHistory = {
  //   val shift = PopCount(brValids & Mux(taken_mask =/= 0.U, LowerMask(taken_mask), ((1.U<<numBr) - 1.U)))
  //   val g = Wire(new GlobalHistory)
  //   g.predHist := (hist << shift) | (taken_mask =/= 0.U)
  //   g
  // }

  def update(shift: UInt, taken: Bool, hist: UInt = predHist): GlobalHistory = {
    val g = Wire(new GlobalHistory)
    g.predHist := (hist << shift) | taken
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

  val br_valids = Vec(numBr, Bool())
  val br_targets = Vec(numBr, UInt(VAddrBits.W))

  val jmp_valid = Bool()
  val jmp_target = UInt(VAddrBits.W)

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  def taken = taken_mask.reduce(_||_) // || (is_jal || is_jalr)

  def fromFtbEntry(entry: FTBEntry, pc: UInt) = {
    br_valids := entry.brValids
    br_targets := entry.getBrTargets(pc)
    jmp_valid := entry.jmpValid
    jmp_target := entry.getJmpTarget(pc)
    is_jal := entry.jmpValid && entry.isJal
    is_jalr := entry.jmpValid && entry.isJalr
    is_call := entry.jmpValid && entry.isCall
    is_ret := entry.jmpValid && entry.isRet
  }
  // override def toPrintable: Printable = {
  //   p"-----------BranchPrediction----------- " +
  //     p"[taken_mask] ${Binary(taken_mask.asUInt)} " +
  //     p"[is_br] ${Binary(is_br.asUInt)}, [is_jal] ${Binary(is_jal.asUInt)} " +
  //     p"[is_jalr] ${Binary(is_jalr.asUInt)}, [is_call] ${Binary(is_call.asUInt)}, [is_ret] ${Binary(is_ret.asUInt)} " +
  //     p"[target] ${Hexadecimal(target)}}, [hit] $hit "
  // }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[taken_mask] ${Binary(taken_mask.asUInt)} [hit] $hit\n")
  }
}

class BranchPredictionBundle(implicit p: Parameters) extends XSBundle with HasBPUConst with BPUUtils{
  val pc = UInt(VAddrBits.W)
  
  val valid = Bool()
  
  val hasRedirect = Bool()
  val ftq_idx = new FtqPtr
  // val hit = Bool()
  val preds = new BranchPrediction

  val ghist = new GlobalHistory()
  val phist = UInt(PathHistoryLength.W)
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val specCnt = Vec(numBr, UInt(10.W))
  // val meta = UInt(MaxMetaLength.W)

  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq

  def real_taken_mask(): Vec[Bool] = {
    Mux(preds.hit,
      VecInit(preds.taken_mask.zip(preds.br_valids).map{ case(m, b) => m && b } :+ preds.jmp_valid),
      VecInit(Seq.fill(numBr+1)(false.B)))
  }

  def real_br_taken_mask(): Vec[Bool] = {
    Mux(preds.hit,
      VecInit(preds.taken_mask.zip(preds.br_valids).map{ case(m, b) => m && b }),
      VecInit(Seq.fill(numBr)(false.B)))
  }
  def hit_taken_on_call = !VecInit(real_taken_mask.take(numBr)).asUInt.orR && preds.hit && preds.is_call && preds.jmp_valid
  def hit_taken_on_ret  = !VecInit(real_taken_mask.take(numBr)).asUInt.orR && preds.hit && preds.is_ret && preds.jmp_valid

  def fallThroughAddr = getFallThroughAddr(pc, ftb_entry.carry, ftb_entry.pftAddr)
  def target(): UInt = {
    Mux(preds.hit,
      // when hit
      Mux((real_taken_mask.asUInt & preds.br_valids.asUInt) =/= 0.U,
        PriorityMux(real_taken_mask.asUInt & preds.br_valids.asUInt, preds.br_targets),
        Mux(preds.jmp_valid, preds.jmp_target, fallThroughAddr)),
      //otherwise
      pc + (FetchWidth*4).U
    )
  }
  def genCfiIndex = {
    val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    cfiIndex.valid := real_taken_mask.asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits :=
      ParallelPriorityMux(real_taken_mask, ftb_entry.getOffsetVec) |
      Fill(log2Ceil(PredictWidth), (!real_taken_mask.asUInt.orR).asUInt)
    cfiIndex
  }


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

  def selectedResp =
    PriorityMux(Seq(
      ((s3.valid && s3.hasRedirect) -> s3),
      ((s2.valid && s2.hasRedirect) -> s2),
      (s1.valid -> s1)
    ))
  def selectedRespIdx =
    PriorityMux(Seq(
      ((s3.valid && s3.hasRedirect) -> BP_S3),
      ((s2.valid && s2.hasRedirect) -> BP_S2),
      (s1.valid -> BP_S1)
    ))
  def lastStage = s3
}

class BpuToFtqBundle(implicit p: Parameters) extends BranchPredictionResp with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}

object BpuToFtqBundle {
  def apply(resp: BranchPredictionResp)(implicit p: Parameters): BpuToFtqBundle = {
    val e = Wire(new BpuToFtqBundle())
    e.s1 := resp.s1
    e.s2 := resp.s2
    e.s3 := resp.s3

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

  def fromFtqRedirectSram(entry: Ftq_Redirect_SRAMEntry) = {
    ghist := entry.ghist
    phist := entry.phist
    rasSp := entry.rasSp
    rasTop := entry.rasEntry
    specCnt := entry.specCnt
    this
  }
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
    XSDebug(cond, p"[hist] ${Binary(cfiUpdate.hist.predHist)}\n")
    XSDebug(cond, p"[predHist] ${Binary(cfiUpdate.predHist.predHist)}\n")
    XSDebug(cond, p"[br_hit] ${cfiUpdate.br_hit} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[pred_taken] ${cfiUpdate.predTaken} [taken] ${cfiUpdate.taken} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[target] ${Hexadecimal(cfiUpdate.target)} \n")
    XSDebug(cond, p"[shift] ${cfiUpdate.shift}\n")
    XSDebug(cond, p"------------------------------- \n")
    XSDebug(cond, p"[roqPtr] f=${roqIdx.flag} v=${roqIdx.value}\n")
    XSDebug(cond, p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} \n")
    XSDebug(cond, p"[ftqOffset] ${ftqOffset} \n")
    XSDebug(cond, p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value}\n")
    XSDebug(cond, p"[stFtqOffset] ${stFtqOffset}\n")
    XSDebug(cond, p"---------------------------------------------- \n")
  }
}
