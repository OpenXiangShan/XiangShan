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
import xiangshan.frontend.icache._
import utils._
import utility._
import scala.math._
import java.util.ResourceBundle.Control

class FrontendTopDownBundle(implicit p: Parameters) extends XSBundle {
  val reasons = Vec(TopDownCounters.NumStallReasons.id, Bool())
  val stallWidth = UInt(log2Ceil(PredictWidth).W)
}

@chiselName
class FetchRequestBundle(implicit p: Parameters) extends XSBundle with HasICacheParameters {

  //fast path: Timing critical
  val startAddr       = UInt(VAddrBits.W)
  val nextlineStart   = UInt(VAddrBits.W)
  val nextStartAddr   = UInt(VAddrBits.W)
  //slow path
  val ftqIdx          = new FtqPtr
  val ftqOffset       = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))

  val topdown_info    = new FrontendTopDownBundle

  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U

  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    when (b.fallThruError) {
      val nextBlockHigherTemp = Mux(startAddr(log2Ceil(PredictWidth)+instOffsetBits), b.startAddr, b.nextLineAddr)
      val nextBlockHigher = nextBlockHigherTemp(VAddrBits-1, log2Ceil(PredictWidth)+instOffsetBits+1)
      this.nextStartAddr :=
        Cat(nextBlockHigher,
          startAddr(log2Ceil(PredictWidth)+instOffsetBits) ^ 1.U(1.W),
          startAddr(log2Ceil(PredictWidth)+instOffsetBits-1, instOffsetBits),
          0.U(instOffsetBits.W)
        )
    }
    this
  }
  override def toPrintable: Printable = {
    p"[start] ${Hexadecimal(startAddr)} [next] ${Hexadecimal(nextlineStart)}" +
      p"[tgt] ${Hexadecimal(nextStartAddr)} [ftqIdx] $ftqIdx [jmp] v:${ftqOffset.valid}" +
      p" offset: ${ftqOffset.bits}\n"
  }
}

class FtqICacheInfo(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val startAddr           = UInt(VAddrBits.W)
  val nextlineStart       = UInt(VAddrBits.W)
  def crossCacheline =  startAddr(blockOffBits - 1) === 1.U
  def fromFtqPcBundle(b: Ftq_RF_Components) = {
    this.startAddr := b.startAddr
    this.nextlineStart := b.nextLineAddr
    this
  }
}

class IFUICacheIO(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val icacheReady       = Output(Bool())
  val resp              = Vec(PortNumber, ValidIO(new ICacheMainPipeResp))
  val topdownIcacheMiss = Output(Bool())
  val topdownItlbMiss = Output(Bool())
}

class FtqToICacheRequestBundle(implicit p: Parameters)extends XSBundle with HasICacheParameters{
  val pcMemRead           = Vec(5, new FtqICacheInfo)
  val readValid           = Vec(5, Bool())
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

// Ftq send req to Prefetch
class PrefetchRequest(implicit p:Parameters) extends XSBundle {
  val target          = UInt(VAddrBits.W)
}

class FtqPrefechBundle(implicit p:Parameters) extends XSBundle {
  val req = DecoupledIO(new PrefetchRequest)
}

class mmioCommitRead(implicit p: Parameters) extends XSBundle {
  val mmioFtqPtr = Output(new FtqPtr)
  val mmioLastCommit = Input(Bool())
}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs    = Vec(PredictWidth, UInt(32.W))
  val valid     = UInt(PredictWidth.W)
  val enqEnable = UInt(PredictWidth.W)
  val pd        = Vec(PredictWidth, new PreDecodeInfo)
  val pc        = Vec(PredictWidth, UInt(VAddrBits.W))
  val foldpc    = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  val ftqPtr       = new FtqPtr
  val ftqOffset    = Vec(PredictWidth, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
  val ipf          = Vec(PredictWidth, Bool())
  val acf          = Vec(PredictWidth, Bool())
  val crossPageIPFFix = Vec(PredictWidth, Bool())
  val triggered    = Vec(PredictWidth, new TriggerCf)

  val topdown_info = new FrontendTopDownBundle
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
}

object CGHPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): CGHPtr = {
    val ptr = Wire(new CGHPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: CGHPtr)(implicit p: Parameters): CGHPtr = {
    apply(!ptr.flag, ptr.value)
  }
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

  def need_oldest_bits = len > compLen
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

  // slow path, read bits from ghr
  def update(ghr: Vec[Bool], histPtr: CGHPtr, num: Int, taken: Bool): FoldedHistory = {
    val oldest_bits = VecInit(get_oldest_bits_from_ghr(ghr, histPtr))
    update(oldest_bits, num, taken)
  }


  // fast path, use pre-read oldest bits
  def update(ob: Vec[Bool], num: Int, taken: Bool): FoldedHistory = {
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
            f"histlen:${this.len}, compLen:$compLen, at bit $i")
        }
        if (resArr(i).length == 0) {
          println(f"[error] bits $i is not assigned in folded hist update logic! histlen:${this.len}, compLen:$compLen")
        }
        res(i) := resArr(i).foldLeft(false.B)(_^_)
      }
      res.asUInt
    }

    val new_folded_hist = if (need_oldest_bits) {
      val oldest_bits = ob
      require(oldest_bits.length == max_update_num)
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

      // do xor then shift
      val xored = bitsets_xor(compLen, Seq(original_bits_set, oldest_bits_set, newest_bits_set))
      circular_shift_left(xored, num)
    } else {
      // histLen too short to wrap around
      ((folded_hist << num) | taken)(compLen-1,0)
    }

    val fh = WireInit(this)
    fh.folded_hist := new_folded_hist
    fh
  }
}

class AheadFoldedHistoryOldestBits(val len: Int, val max_update_num: Int)(implicit p: Parameters) extends XSBundle {
  val bits = Vec(max_update_num*2, Bool())
  // def info = (len, compLen)
  def getRealOb(brNumOH: UInt): Vec[Bool] = {
    val ob = Wire(Vec(max_update_num, Bool()))
    for (i <- 0 until max_update_num) {
      ob(i) := Mux1H(brNumOH, bits.drop(i).take(numBr+1))
    }
    ob
  }
}

class AllAheadFoldedHistoryOldestBits(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val afhob = MixedVec(gen.filter(t => t._1 > t._2).map{_._1}
    .toSet.toList.map(l => new AheadFoldedHistoryOldestBits(l, numBr))) // remove duplicates
  require(gen.toSet.toList.equals(gen))
  def getObWithInfo(info: Tuple2[Int, Int]) = {
    val selected = afhob.filter(_.len == info._1)
    require(selected.length == 1)
    selected(0)
  }
  def read(ghv: Vec[Bool], ptr: CGHPtr) = {
    val hisLens = afhob.map(_.len)
    val bitsToRead = hisLens.flatMap(l => (0 until numBr*2).map(i => l-i-1)).toSet // remove duplicates
    val bitsWithInfo = bitsToRead.map(pos => (pos, ghv((ptr+(pos+1).U).value)))
    for (ob <- afhob) {
      for (i <- 0 until numBr*2) {
        val pos = ob.len - i - 1
        val bit_found = bitsWithInfo.filter(_._1 == pos).toList
        require(bit_found.length == 1)
        ob.bits(i) := bit_found(0)._2
      }
    }
  }
}

class AllFoldedHistories(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val hist = MixedVec(gen.map{case (l, cl) => new FoldedHistory(l, cl, numBr)})
  // println(gen.mkString)
  require(gen.toSet.toList.equals(gen))
  def getHistWithInfo(info: Tuple2[Int, Int]) = {
    val selected = hist.filter(_.info.equals(info))
    require(selected.length == 1)
    selected(0)
  }
  def autoConnectFrom(that: AllFoldedHistories) = {
    require(this.hist.length <= that.hist.length)
    for (h <- this.hist) {
      h := that.getHistWithInfo(h.info)
    }
  }
  def update(ghv: Vec[Bool], ptr: CGHPtr, shift: Int, taken: Bool): AllFoldedHistories = {
    val res = WireInit(this)
    for (i <- 0 until this.hist.length) {
      res.hist(i) := this.hist(i).update(ghv, ptr, shift, taken)
    }
    res
  }
  def update(afhob: AllAheadFoldedHistoryOldestBits, lastBrNumOH: UInt, shift: Int, taken: Bool): AllFoldedHistories = {
    val res = WireInit(this)
    for (i <- 0 until this.hist.length) {
      val fh = this.hist(i)
      if (fh.need_oldest_bits) {
        val info = fh.info
        val selectedAfhob = afhob.getObWithInfo(info)
        val ob = selectedAfhob.getRealOb(lastBrNumOH)
        res.hist(i) := this.hist(i).update(ob, shift, taken)
      } else {
        val dumb = Wire(Vec(numBr, Bool())) // not needed
        dumb := DontCare
        res.hist(i) := this.hist(i).update(dumb, shift, taken)
      }
    }
    res
  }

  def display(cond: Bool) = {
    for (h <- hist) {
      XSDebug(cond, p"hist len ${h.len}, folded len ${h.compLen}, value ${Binary(h.folded_hist)}\n")
    }
  }
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

trait BasicPrediction extends HasXSParameter {
  def cfiIndex: ValidUndirectioned[UInt]
  def target(pc: UInt): UInt
  def lastBrPosOH: Vec[Bool]
  def brTaken: Bool
  def shouldShiftVec: Vec[Bool]
  def fallThruError: Bool
}

// selectByTaken selects some data according to takenMask
// allTargets should be in flattened 2-dim Vec, like [taken, not taken, not hit, taken, ...]
def selectByTaken[T <: Data](takenMask: Vec[Bool], hit: Bool, allTargets: Vec[T]): T = {
  val selVecOH =
    takenMask.zipWithIndex.map { case (t, i) => !takenMask.take(i).fold(false.B)(_ || _) && t && hit } :+
      (!takenMask.asUInt.orR && hit) :+ !hit
  Mux1H(selVecOH, allTargets)
}

@chiselName
class FullBranchPrediction(implicit p: Parameters) extends XSBundle with HasBPUConst with BasicPrediction {
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets = Vec(totalSlot, UInt(VAddrBits.W))
  val jalr_target = UInt(VAddrBits.W) // special path for indirect predictors
  val offsets = Vec(totalSlot, UInt(log2Ceil(PredictWidth).W))
  val fallThroughAddr = UInt(VAddrBits.W)
  val fallThroughErr = Bool()

  val is_jal = Bool()
  val is_jalr = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val last_may_be_rvi_call = Bool()
  val is_br_sharing = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  val predCycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  def br_slot_valids = slot_valids.init
  def tail_slot_valid = slot_valids.last

  def br_valids = {
    VecInit(br_slot_valids :+ (tail_slot_valid && is_br_sharing))
  }

  def taken_mask_on_slot = {
    VecInit(
      (br_slot_valids zip br_taken_mask.init).map{ case (t, v) => t && v } :+ (
        tail_slot_valid && (
          is_br_sharing && br_taken_mask.last || !is_br_sharing
        )
      )
    )
  }

  def real_slot_taken_mask(): Vec[Bool] = {
    VecInit(taken_mask_on_slot.map(_ && hit))
  }

  // len numBr
  def real_br_taken_mask(): Vec[Bool] = {
    VecInit(
      taken_mask_on_slot.map(_ && hit).init :+
      (br_taken_mask.last && tail_slot_valid && is_br_sharing && hit)
    )
  }

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec =
    VecInit(br_valids.zipWithIndex.map{ case (v, i) =>
      v && !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B)})

  def lastBrPosOH =
    VecInit((!hit || !br_valids.reduce(_||_)) +: // not hit or no brs in entry
      (0 until numBr).map(i =>
        br_valids(i) &&
        !real_br_taken_mask.take(i).reduceOption(_||_).getOrElse(false.B) && // no brs taken in front it
        (real_br_taken_mask()(i) || !br_valids.drop(i+1).reduceOption(_||_).getOrElse(false.B)) && // no brs behind it
        hit
      )
    )

  def brTaken = (br_valids zip br_taken_mask).map{ case (a, b) => a && b && hit}.reduce(_||_)

  def target(pc: UInt): UInt = {
    selectByTaken(taken_mask_on_slot, hit, allTarget(pc))
  }

  // allTarget return a flattened 2-dim Vec of all possible target of a BP stage
  // in the following order: [0:totalSlot][taken_targets, fallThroughAddr, not hit (plus fetch width)]
  // after flatten looks like [t0, f0, n0, t1, f1, n0, ...] (t,f,n stands for taken, fallthrough, not hit)
  //
  // This exposes internal targets for timing optimization,
  // since usually targets are generated quicker than taken
  def allTarget(pc: UInt): Vec[UInt] = {
    VecInit(targets :+ fallThroughAddr :+ (pc + (FetchWidth * 4).U))
  }

  def fallThruError: Bool = hit && fallThroughErr

  def hit_taken_on_jmp = 
    !real_slot_taken_mask().init.reduce(_||_) &&
    real_slot_taken_mask().last && !is_br_sharing
  def hit_taken_on_call = hit_taken_on_jmp && is_call
  def hit_taken_on_ret  = hit_taken_on_jmp && is_ret
  def hit_taken_on_jalr = hit_taken_on_jmp && is_jalr

  def cfiIndex = {
    val cfiIndex = Wire(ValidUndirectioned(UInt(log2Ceil(PredictWidth).W)))
    cfiIndex.valid := real_slot_taken_mask().asUInt.orR
    // when no takens, set cfiIndex to PredictWidth-1
    cfiIndex.bits :=
      ParallelPriorityMux(real_slot_taken_mask(), offsets) |
      Fill(log2Ceil(PredictWidth), (!real_slot_taken_mask().asUInt.orR).asUInt)
    cfiIndex
  }

  def taken = br_taken_mask.reduce(_||_) || slot_valids.last // || (is_jal || is_jalr)

  def fromFtbEntry(
                    entry: FTBEntry,
                    pc: UInt,
                    last_stage_pc: Option[Tuple2[UInt, Bool]] = None,
                    last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
                  ) = {
    slot_valids := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
    targets := entry.getTargetVec(pc, last_stage_pc) // Use previous stage pc for better timing
    jalr_target := targets.last
    offsets := entry.getOffsetVec
    is_jal := entry.tailSlot.valid && entry.isJal
    is_jalr := entry.tailSlot.valid && entry.isJalr
    is_call := entry.tailSlot.valid && entry.isCall
    is_ret := entry.tailSlot.valid && entry.isRet
    last_may_be_rvi_call := entry.last_may_be_rvi_call
    is_br_sharing := entry.tailSlot.valid && entry.tailSlot.sharing
    predCycle.map(_ := GTimer())
    
    val startLower        = Cat(0.U(1.W),    pc(instOffsetBits+log2Ceil(PredictWidth)-1, instOffsetBits))
    val endLowerwithCarry = Cat(entry.carry, entry.pftAddr)
    fallThroughErr := startLower >= endLowerwithCarry
    fallThroughAddr := Mux(fallThroughErr, pc + (FetchWidth * 4).U, entry.getFallThrough(pc, last_stage_entry))
  }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[taken_mask] ${Binary(br_taken_mask.asUInt)} [hit] $hit\n")
  }
}

class SpeculativeInfo(implicit p: Parameters) extends XSBundle
  with HasBPUConst with BPUUtils {
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val afhob = new AllAheadFoldedHistoryOldestBits(foldedGHistInfos)
  val lastBrNumOH = UInt((numBr+1).W)
  val histPtr = new CGHPtr
  val ssp = UInt(log2Up(RasSize).W)
  val sctr = UInt(log2Up(RasCtrSize).W)
  val TOSW = new RASPtr
  val TOSR = new RASPtr
  val NOS = new RASPtr
  val topAddr = UInt(VAddrBits.W)
}

@chiselName
class BranchPredictionBundle(implicit p: Parameters) extends XSBundle
  with HasBPUConst with BPUUtils {
  val pc    = Vec(numDup, UInt(VAddrBits.W))
  val valid = Vec(numDup, Bool())
  val hasRedirect  = Vec(numDup, Bool())
  val ftq_idx = new FtqPtr
  val full_pred    = Vec(numDup, new FullBranchPrediction)


  def target(pc: UInt) = VecInit(full_pred.map(_.target(pc)))
  def targets(pc: Vec[UInt]) = VecInit(pc.zipWithIndex.map{case (pc, idx) => full_pred(idx).target(pc)})
  def allTargets(pc: Vec[UInt]) = VecInit(pc.zipWithIndex.map{case (pc, idx) => full_pred(idx).allTarget(pc)})
  def cfiIndex         = VecInit(full_pred.map(_.cfiIndex))
  def lastBrPosOH      = VecInit(full_pred.map(_.lastBrPosOH))
  def brTaken          = VecInit(full_pred.map(_.brTaken))
  def shouldShiftVec   = VecInit(full_pred.map(_.shouldShiftVec))
  def fallThruError    = VecInit(full_pred.map(_.fallThruError))

  def taken = VecInit(cfiIndex.map(_.valid))

  def getTarget = targets(pc)
  def getAllTargets = allTargets(pc)

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[pc] ${Hexadecimal(pc(0))}\n")
    full_pred(0).display(cond)
  }
}

@chiselName
class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  // val valids = Vec(3, Bool())
  val s1 = new BranchPredictionBundle
  val s2 = new BranchPredictionBundle
  val s3 = new BranchPredictionBundle

  val last_stage_meta = UInt(MaxMetaLength.W)
  val last_stage_spec_info = new SpeculativeInfo
  val last_stage_ftb_entry = new FTBEntry

  val topdown_info = new FrontendTopDownBundle

  def selectedResp ={
    val res =
      PriorityMux(Seq(
        ((s3.valid(3) && s3.hasRedirect(3)) -> s3),
        ((s2.valid(3) && s2.hasRedirect(3)) -> s2),
        (s1.valid(3) -> s1)
      ))
    res
  }
  def selectedRespIdxForFtq =
    PriorityMux(Seq(
      ((s3.valid(3) && s3.hasRedirect(3)) -> BP_S3),
      ((s2.valid(3) && s2.hasRedirect(3)) -> BP_S2),
      (s1.valid(3) -> BP_S1)
    ))
  def lastStage = s3
}

class BpuToFtqBundle(implicit p: Parameters) extends BranchPredictionResp {}

class BranchPredictionUpdate(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val spec_info = new SpeculativeInfo
  val ftb_entry = new FTBEntry()

  val cfi_idx = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val br_taken_mask = Vec(numBr, Bool())
  val br_committed = Vec(numBr, Bool()) // High only when br valid && br committed
  val jmp_taken = Bool()
  val mispred_mask = Vec(numBr+1, Bool())
  val pred_hit = Bool()
  val false_hit = Bool()
  val new_br_insert_pos = Vec(numBr, Bool())
  val old_entry = Bool()
  val meta = UInt(MaxMetaLength.W)
  val full_target = UInt(VAddrBits.W)
  val from_stage = UInt(2.W)
  val ghist = UInt(HistoryLength.W)

  def is_jal = ftb_entry.tailSlot.valid && ftb_entry.isJal
  def is_jalr = ftb_entry.tailSlot.valid && ftb_entry.isJalr
  def is_call = ftb_entry.tailSlot.valid && ftb_entry.isCall
  def is_ret = ftb_entry.tailSlot.valid && ftb_entry.isRet

  def is_call_taken = is_call && jmp_taken && cfi_idx.valid && cfi_idx.bits === ftb_entry.tailSlot.offset
  def is_ret_taken = is_ret && jmp_taken && cfi_idx.valid && cfi_idx.bits === ftb_entry.tailSlot.offset

  def display(cond: Bool) = {
    XSDebug(cond, p"-----------BranchPredictionUpdate-----------\n")
    XSDebug(cond, p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] $false_hit\n")
    XSDebug(cond, p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)}\n")
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

  // TODO: backend should pass topdown signals here
  // must not change its parent since BPU has used asTypeOf(this type) from its parent class
  require(isInstanceOf[Redirect])
  val BTBMissBubble = Bool()
  def ControlRedirectBubble = debugIsCtrl
  // if mispred br not in ftb, count as BTB miss
  def ControlBTBMissBubble = ControlRedirectBubble && !cfiUpdate.br_hit && !cfiUpdate.jr_hit
  def TAGEMissBubble = ControlRedirectBubble && cfiUpdate.br_hit && !cfiUpdate.sc_hit
  def SCMissBubble = ControlRedirectBubble && cfiUpdate.br_hit && cfiUpdate.sc_hit
  def ITTAGEMissBubble = ControlRedirectBubble && cfiUpdate.jr_hit && !cfiUpdate.pd.isRet
  def RASMissBubble = ControlRedirectBubble && cfiUpdate.jr_hit && cfiUpdate.pd.isRet
  def MemVioRedirectBubble = debugIsMemVio
  def OtherRedirectBubble = !debugIsCtrl && !debugIsMemVio

  def connectRedirect(source: Redirect): Unit = {
    for ((name, data) <- this.elements) {
      if (source.elements.contains(name)) {
        data := source.elements(name)
      }
    }
  }

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
