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
import chisel3.experimental.chiselName
import xiangshan._
import utils._
import scala.math._

@chiselName
class FetchRequestBundle(implicit p: Parameters) extends XSBundle {
  val startAddr       = UInt(VAddrBits.W)
  val fallThruAddr    = UInt(VAddrBits.W)
  val fallThruError   = Bool()
  val ftqIdx          = new FtqPtr
  val ftqOffset       = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target          = UInt(VAddrBits.W)
  val oversize        = Bool()

  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    val ftError = b.fallThroughError()
    this.startAddr := b.startAddr
    this.fallThruError := ftError
    this.fallThruAddr := Mux(ftError, b.nextRangeAddr, b.getFallThrough())
    this.oversize := b.oversize
    this
  }
  def fromBpuResp(resp: BranchPredictionBundle) = {
    // only used to bypass, so some fields remains unchanged
    this.startAddr := resp.pc
    this.target := resp.target
    this.ftqOffset := resp.genCfiIndex
    this.fallThruAddr := resp.preds.fallThroughAddr
    this.oversize := resp.preds.oversize
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
  val triggered    = Vec(PredictWidth, new TriggerCf)
}

// class BitWiseUInt(val width: Int, val init: UInt) extends Module {
//   val io = IO(new Bundle {
//     val set
//   })
// }
// Move from BPU
abstract class GlobalHistory(implicit p: Parameters) extends XSBundle with HasBPUConst {
  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): GlobalHistory
}

class ShiftingGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val predHist = UInt(HistoryLength.W)

  def update(shift: UInt, taken: Bool, hist: UInt = this.predHist): ShiftingGlobalHistory = {
    val g = Wire(new ShiftingGlobalHistory)
    g.predHist := (hist << shift) | taken
    g
  }

  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): ShiftingGlobalHistory = {
    require(br_valids.length == numBr)
    require(real_taken_mask.length == numBr)
    val last_valid_idx = PriorityMux(
      br_valids.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: real_taken_mask)
    val smaller = Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
    val shift = smaller
    val taken = real_taken_mask.reduce(_||_)
    update(shift, taken, this.predHist)
  }

  // static read
  def read(n: Int): Bool = predHist.asBools()(n)

  final def === (that: ShiftingGlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: ShiftingGlobalHistory): Bool = !(this === that)
}

// circular global history pointer
class CGHPtr(implicit p: Parameters) extends CircularQueuePtr[CGHPtr](
  p => p(XSCoreParamsKey).HistoryLength
){
  override def cloneType = (new CGHPtr).asInstanceOf[this.type]
}
class CircularGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val buffer = Vec(HistoryLength, Bool())
  type HistPtr = UInt
  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): CircularGlobalHistory = {
    this
  }
}

class FoldedHistory(val len: Int, val compLen: Int, val max_update_num: Int)(implicit p: Parameters)
  extends XSBundle with HasBPUConst {
  require(compLen >= 1)
  require(len > 0)
  // require(folded_len <= len)
  require(compLen >= max_update_num)
  val folded_hist = UInt(compLen.W)

  def info = (len, compLen)
  def oldest_bit_to_get_from_ghr = (0 until max_update_num).map(len - _ - 1)
  def oldest_bit_pos_in_folded = oldest_bit_to_get_from_ghr map (_ % compLen)
  def oldest_bit_wrap_around = oldest_bit_to_get_from_ghr map (_ / compLen > 0)
  def oldest_bit_start = oldest_bit_pos_in_folded.head

  def get_oldest_bits_from_ghr(ghr: Vec[Bool], histPtr: CGHPtr) = {
    // TODO: wrap inc for histPtr value
    oldest_bit_to_get_from_ghr.map(i => ghr((histPtr + (i+1).U).value))
  }

  def circular_shift_left(src: UInt, shamt: Int) = {
    val srcLen = src.getWidth
    val src_doubled = Cat(src, src)
    val shifted = src_doubled(srcLen*2-1-shamt, srcLen-shamt)
    shifted
  }


  def update(ghr: Vec[Bool], histPtr: CGHPtr, num: Int, taken: Bool): FoldedHistory = {
    // do xors for several bitsets at specified bits
    def bitsets_xor(len: Int, bitsets: Seq[Seq[Tuple2[Int, Bool]]]) = {
      val res = Wire(Vec(len, Bool()))
      // println(f"num bitsets: ${bitsets.length}")
      // println(f"bitsets $bitsets")
      val resArr = Array.fill(len)(List[Bool]())
      for (bs <- bitsets) {
        for ((n, b) <- bs) {
          resArr(n) = b :: resArr(n)
        }
      }
      // println(f"${resArr.mkString}")
      // println(f"histLen: ${this.len}, foldedLen: $folded_len")
      for (i <- 0 until len) {
        // println(f"bit[$i], ${resArr(i).mkString}")
        if (resArr(i).length > 2) {
          println(f"[warning] update logic of foldest history has two or more levels of xor gates! " +
            f"histlen:${this.len}, compLen:$compLen")
        }
        if (resArr(i).length == 0) {
          println(f"[error] bits $i is not assigned in folded hist update logic! histlen:${this.len}, compLen:$compLen")
        }
        res(i) := resArr(i).foldLeft(false.B)(_^_)
      }
      res.asUInt
    }
    val oldest_bits = get_oldest_bits_from_ghr(ghr, histPtr)

    // mask off bits that do not update
    val oldest_bits_masked = oldest_bits.zipWithIndex.map{
      case (ob, i) => ob && (i < num).B
    }
    // if a bit does not wrap around, it should not be xored when it exits
    val oldest_bits_set = (0 until max_update_num).filter(oldest_bit_wrap_around).map(i => (oldest_bit_pos_in_folded(i), oldest_bits_masked(i)))
    
    // println(f"old bits pos ${oldest_bits_set.map(_._1)}")

    // only the last bit could be 1, as we have at most one taken branch at a time
    val newest_bits_masked = VecInit((0 until max_update_num).map(i => taken && ((i+1) == num).B)).asUInt
    // if a bit does not wrap around, newest bits should not be xored onto it either
    val newest_bits_set = (0 until max_update_num).map(i => (compLen-1-i, newest_bits_masked(i)))

    // println(f"new bits set ${newest_bits_set.map(_._1)}")
    //
    val original_bits_masked = VecInit(folded_hist.asBools.zipWithIndex.map{
      case (fb, i) => fb && !(num >= (len-i)).B
    })
    val original_bits_set = (0 until compLen).map(i => (i, original_bits_masked(i)))

    
    // histLen too short to wrap around
    val new_folded_hist =
      if (len <= compLen) {
        ((folded_hist << num) | taken)(compLen-1,0)
        // circular_shift_left(max_update_num)(Cat(Reverse(newest_bits_masked), folded_hist(compLen-max_update_num-1,0)), num)
      } else {
        // do xor then shift
        val xored = bitsets_xor(compLen, Seq(original_bits_set, oldest_bits_set, newest_bits_set))
        circular_shift_left(xored, num)
      }
    val fh = WireInit(this)
    fh.folded_hist := new_folded_hist
    fh
  }

  // def update(ghr: Vec[Bool], histPtr: CGHPtr, valids: Vec[Bool], takens: Vec[Bool]): FoldedHistory = {
  //   val fh = WireInit(this)
  //   require(valids.length == max_update_num)
  //   require(takens.length == max_update_num)
  //   val last_valid_idx = PriorityMux(
  //     valids.reverse :+ true.B,
  //     (max_update_num to 0 by -1).map(_.U(log2Ceil(max_update_num+1).W))
  //     )
  //   val first_taken_idx = PriorityEncoder(false.B +: takens)
  //   val smaller = Mux(last_valid_idx < first_taken_idx,
  //     last_valid_idx,
  //     first_taken_idx
  //   )
  //   // update folded_hist
  //   fh.update(ghr, histPtr, smaller, takens.reduce(_||_))
  // }
  // println(f"folded hist original length: ${len}, folded len: ${folded_len} " +
  //   f"oldest bits' pos in folded: ${oldest_bit_pos_in_folded}")

  
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

@chiselName
class BranchPrediction(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets = Vec(totalSlot, UInt(VAddrBits.W))
  val offsets = Vec(totalSlot, UInt(log2Ceil(PredictWidth).W))
  val fallThroughAddr = UInt(VAddrBits.W)
  val oversize = Bool()

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val is_br_sharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  def br_slot_valids = slot_valids.init
  def tail_slot_valid = slot_valids.last

  def br_valids = {
    VecInit(
      if (shareTailSlot)
        br_slot_valids :+ (tail_slot_valid && is_br_sharing)
      else
        br_slot_valids
    )
  }

  def taken_mask_on_slot = {
    VecInit(
      if (shareTailSlot)
        (br_slot_valids zip br_taken_mask.init).map{ case (t, v) => t && v } :+ (
          (br_taken_mask.last && tail_slot_valid && is_br_sharing) ||
          tail_slot_valid && !is_br_sharing
        )
      else
        (br_slot_valids zip br_taken_mask).map{ case (v, t) => v && t } :+
        tail_slot_valid
    )
  }

  def taken = br_taken_mask.reduce(_||_) || slot_valids.last // || (is_jal || is_jalr)

  def fromFtbEntry(entry: FTBEntry, pc: UInt) = {
    slot_valids := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
    targets := entry.getTargetVec(pc)
    offsets := entry.getOffsetVec
    fallThroughAddr := entry.getFallThrough(pc)
    oversize := entry.oversize
    is_jal := entry.tailSlot.valid && entry.isJal
    is_jalr := entry.tailSlot.valid && entry.isJalr
    is_call := entry.tailSlot.valid && entry.isCall
    is_ret := entry.tailSlot.valid && entry.isRet
    is_br_sharing := entry.tailSlot.valid && entry.tailSlot.sharing
  }

  def fromMicroBTBEntry(entry: MicroBTBEntry) = {
    slot_valids := entry.slot_valids
    targets := entry.targets
    offsets := entry.offsets
    fallThroughAddr := entry.fallThroughAddr
    oversize := entry.oversize
    is_jal := DontCare
    is_jalr := DontCare
    is_call := DontCare
    is_ret := DontCare
    is_br_sharing := entry.last_is_br
  }
  // override def toPrintable: Printable = {
  //   p"-----------BranchPrediction----------- " +
  //     p"[taken_mask] ${Binary(taken_mask.asUInt)} " +
  //     p"[is_br] ${Binary(is_br.asUInt)}, [is_jal] ${Binary(is_jal.asUInt)} " +
  //     p"[is_jalr] ${Binary(is_jalr.asUInt)}, [is_call] ${Binary(is_call.asUInt)}, [is_ret] ${Binary(is_ret.asUInt)} " +
  //     p"[target] ${Hexadecimal(target)}}, [hit] $hit "
  // }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[taken_mask] ${Binary(br_taken_mask.asUInt)} [hit] $hit\n")
  }
}

@chiselName
class BranchPredictionBundle(implicit p: Parameters) extends XSBundle with HasBPUConst with BPUUtils{
  val pc = UInt(VAddrBits.W)

  val valid = Bool()

  val hasRedirect = Bool()
  val ftq_idx = new FtqPtr
  // val hit = Bool()
  val preds = new BranchPrediction

  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val histPtr = new CGHPtr
  val phist = UInt(PathHistoryLength.W)
  val rasSp = UInt(log2Ceil(RasSize).W)
  val rasTop = new RASEntry
  val specCnt = Vec(numBr, UInt(10.W))
  // val meta = UInt(MaxMetaLength.W)

  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq

  def real_slot_taken_mask(): Vec[Bool] = {
    VecInit(preds.taken_mask_on_slot.map(_ && preds.hit))
  }

  // len numBr
  def real_br_taken_mask(): Vec[Bool] = {
    if (shareTailSlot)
      VecInit(
        preds.taken_mask_on_slot.map(_ && preds.hit).init :+
        (preds.br_taken_mask.last && preds.tail_slot_valid && preds.is_br_sharing && preds.hit)
      )
    else
      VecInit(real_slot_taken_mask().init)
  }

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec =
    VecInit(preds.br_valids.zipWithIndex.map{ case (v, i) =>
      v && !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B)})

  def lastBrPosOH =
    (!preds.hit || !preds.br_valids.reduce(_||_)) +: // not hit or no brs in entry
    VecInit((0 until numBr).map(i =>
      preds.br_valids(i) &&
      !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B) && // no brs taken in front it
      (real_br_taken_mask()(i) || !preds.br_valids.drop(i+1).reduceOption(_||_).getOrElse(false.B)) && // no brs behind it
      preds.hit
    ))

  def br_count(): UInt = {
    val last_valid_idx = PriorityMux(
      preds.br_valids.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
      )
    val first_taken_idx = PriorityEncoder(false.B +: real_br_taken_mask)
    Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
  }

  def hit_taken_on_jmp = 
    !real_slot_taken_mask().init.reduce(_||_) &&
    real_slot_taken_mask().last && !preds.is_br_sharing
  def hit_taken_on_call = hit_taken_on_jmp && preds.is_call
  def hit_taken_on_ret  = hit_taken_on_jmp && preds.is_ret
  def hit_taken_on_jalr = hit_taken_on_jmp && preds.is_jalr

  def target(): UInt = {
    val targetVecOnHit = preds.targets :+ preds.fallThroughAddr
    val targetOnNotHit = pc + (FetchWidth * 4).U
    val taken_mask = preds.taken_mask_on_slot
    val selVecOHOnHit =
      taken_mask.zipWithIndex.map{ case (t, i) => !taken_mask.take(i).fold(false.B)(_||_) && t} :+ !taken_mask.asUInt.orR
    val targetOnHit = Mux1H(selVecOHOnHit, targetVecOnHit)
    Mux(preds.hit, targetOnHit, targetOnNotHit)
  }

  def targetDiffFrom(addr: UInt) = {
    val targetVec = preds.targets :+ preds.fallThroughAddr :+ (pc + (FetchWidth*4).U)
    val taken_mask = preds.taken_mask_on_slot
    val selVecOH =
      taken_mask.zipWithIndex.map{ case (t, i) => !taken_mask.take(i).fold(false.B)(_||_) && t && preds.hit} :+
      (!taken_mask.asUInt.orR && preds.hit) :+ !preds.hit
    val diffVec = targetVec map (_ =/= addr)
    Mux1H(selVecOH, diffVec)
  }

  def genCfiIndex = {
    val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    cfiIndex.valid := real_slot_taken_mask().asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits :=
      ParallelPriorityMux(real_slot_taken_mask(), preds.offsets) |
      Fill(log2Ceil(PredictWidth), (!real_slot_taken_mask().asUInt.orR).asUInt)
    cfiIndex
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[pc] ${Hexadecimal(pc)}\n")
    folded_hist.display(cond)
    preds.display(cond)
    ftb_entry.display(cond)
  }
}

@chiselName
class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle()
  val s2 = new BranchPredictionBundle()

  def selectedResp =
    PriorityMux(Seq(
      ((s2.valid && s2.hasRedirect) -> s2),
      (s1.valid -> s1)
    ))
  def selectedRespIdx =
    PriorityMux(Seq(
      ((s2.valid && s2.hasRedirect) -> BP_S2),
      (s1.valid -> BP_S1)
    ))
  def lastStage = s2
}

class BpuToFtqBundle(implicit p: Parameters) extends BranchPredictionResp with HasBPUConst {
  val meta = UInt(MaxMetaLength.W)
}

object BpuToFtqBundle {
  def apply(resp: BranchPredictionResp)(implicit p: Parameters): BpuToFtqBundle = {
    val e = Wire(new BpuToFtqBundle())
    e.s1 := resp.s1
    e.s2 := resp.s2

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
  val full_target = UInt(VAddrBits.W)

  def fromFtqRedirectSram(entry: Ftq_Redirect_SRAMEntry) = {
    folded_hist := entry.folded_hist
    histPtr := entry.histPtr
    phist := entry.phist
    rasSp := entry.rasSp
    rasTop := entry.rasEntry
    specCnt := entry.specCnt
    this
  }

  override def display(cond: Bool) = {
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
  //     p"[robPtr] f=${robIdx.flag} v=${robIdx.value} " +
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
    // XSDebug(cond, p"[hist] ${Binary(cfiUpdate.hist.predHist)}\n")
    XSDebug(cond, p"[br_hit] ${cfiUpdate.br_hit} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[pred_taken] ${cfiUpdate.predTaken} [taken] ${cfiUpdate.taken} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(cond, p"[target] ${Hexadecimal(cfiUpdate.target)} \n")
    XSDebug(cond, p"[shift] ${cfiUpdate.shift}\n")
    XSDebug(cond, p"------------------------------- \n")
    XSDebug(cond, p"[robPtr] f=${robIdx.flag} v=${robIdx.value}\n")
    XSDebug(cond, p"[ftqPtr] f=${ftqIdx.flag} v=${ftqIdx.value} \n")
    XSDebug(cond, p"[ftqOffset] ${ftqOffset} \n")
    XSDebug(cond, p"[stFtqIdx] f=${stFtqIdx.flag} v=${stFtqIdx.value}\n")
    XSDebug(cond, p"[stFtqOffset] ${stFtqOffset}\n")
    XSDebug(cond, p"---------------------------------------------- \n")
  }
}
