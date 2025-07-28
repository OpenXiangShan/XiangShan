/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._
import chisel3.util._
import ftq.BpuFlushInfo
import ftq.FtqPtr
import ftq.FtqRedirectSramEntry
import org.chipsalliance.cde.config.Parameters
import utility._
import utils.EnumUInt
import xiangshan._
import xiangshan.backend.GPAMemEntry
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.TlbResp
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculativeMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BPUUtils
import xiangshan.frontend.bpu.FTBEntry
import xiangshan.frontend.bpu.HasBPUConst
import xiangshan.frontend.bpu.OldPredictorMeta // TODO: remove this
import xiangshan.frontend.bpu.RasPtr
import xiangshan.frontend.icache._
import xiangshan.frontend.instruncache.InstrUncacheReq
import xiangshan.frontend.instruncache.InstrUncacheResp

class FrontendTopDownBundle(implicit p: Parameters) extends XSBundle {
  val reasons    = Vec(TopDownCounters.NumStallReasons.id, Bool())
  val stallWidth = UInt(log2Ceil(PredictWidth).W)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val prediction:      DecoupledIO[BpuPrediction]      = DecoupledIO(new BpuPrediction)
  val speculativeMeta: DecoupledIO[BpuSpeculativeMeta] = DecoupledIO(new BpuSpeculativeMeta)
  val meta:            DecoupledIO[BpuMeta]            = DecoupledIO(new BpuMeta)
  val s3FtqPtr:        FtqPtr                          = Output(new FtqPtr)
  // TODO: topdown, etc.
}

class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect:        Valid[BpuRedirect] = Valid(new BpuRedirect)
  val train:           Valid[BpuTrain]    = Valid(new BpuTrain)
  val bpuPtr:          FtqPtr             = Output(new FtqPtr)
  val redirectFromIFU: Bool               = Output(Bool())
}

class FetchRequestBundle(implicit p: Parameters) extends FrontendBundle with HasICacheParameters {

  // fast path: Timing critical
  val valid:              Bool       = Bool()
  val startVAddr:         PrunedAddr = PrunedAddr(VAddrBits)
  val nextCachelineVAddr: PrunedAddr = PrunedAddr(VAddrBits)
  val nextStartVAddr:     PrunedAddr = PrunedAddr(VAddrBits)
  // slow path
  val ftqIdx    = new FtqPtr
  val ftqOffset = Valid(UInt(CfiPositionWidth.W))

  def crossCacheline: Bool = startVAddr(blockOffBits - 1) === 1.U

  override def toPrintable: Printable =
    p"[start] ${Hexadecimal(startVAddr.toUInt)} [next] ${Hexadecimal(nextCachelineVAddr.toUInt)}" +
      p"[tgt] ${Hexadecimal(nextStartVAddr.toUInt)} [ftqIdx] $ftqIdx [jmp] v:${ftqOffset.valid}" +
      p" offset: ${ftqOffset.bits}\n"
}

class FtqICacheInfo(implicit p: Parameters) extends XSBundle with HasICacheParameters {
  val startVAddr:         PrunedAddr = PrunedAddr(VAddrBits)
  val nextCachelineVAddr: PrunedAddr = PrunedAddr(VAddrBits)
  val ftqIdx:             FtqPtr     = new FtqPtr

  def crossCacheline: Bool = startVAddr(blockOffBits - 1) === 1.U
}

class FtqToPrefetchBundle(implicit p: Parameters) extends XSBundle {
  val req:              FtqICacheInfo = new FtqICacheInfo
  val backendException: ExceptionType = new ExceptionType
}

class FtqToFetchBundle(implicit p: Parameters) extends XSBundle with HasICacheParameters {
  val req:                Vec[FtqICacheInfo] = Vec(5, new FtqICacheInfo)
  val readValid:          Vec[Bool]          = Vec(5, Bool())
  val isBackendException: Bool               = Bool()
}

class FtqToICacheIO(implicit p: Parameters) extends XSBundle {
  // NOTE: req.bits must be prepared in T cycle
  // while req.valid is set true in T + 1 cycle
  val fetchReq:      DecoupledIO[FtqToFetchBundle]    = DecoupledIO(new FtqToFetchBundle)
  val prefetchReq:   DecoupledIO[FtqToPrefetchBundle] = DecoupledIO(new FtqToPrefetchBundle)
  val flushFromBpu:  BpuFlushInfo                     = new BpuFlushInfo
  val redirectFlush: Bool                             = Output(Bool())
}

class ICacheToIfuIO(implicit p: Parameters) extends XSBundle {
  val fetchResp:  Valid[ICacheRespBundle] = ValidIO(new ICacheRespBundle)
  val topdown:    ICacheTopdownInfo       = Output(new ICacheTopdownInfo)
  val perf:       ICachePerfInfo          = Output(new ICachePerfInfo)
  val fetchReady: Bool                    = Output(Bool())
}

class IfuToICacheIO(implicit p: Parameters) extends XSBundle {
  val stall: Bool = Output(Bool())
}

class IfuToInstrUncacheIO(implicit p: Parameters) extends XSBundle {
  val req: DecoupledIO[InstrUncacheReq] = DecoupledIO(new InstrUncacheReq)
}

class InstrUncacheToIfuIO(implicit p: Parameters) extends XSBundle {
  val resp: DecoupledIO[InstrUncacheResp] = DecoupledIO(new InstrUncacheResp)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle {
  class FtqToIfuReq(implicit p: Parameters) extends XSBundle {
    val fetch:       Vec[FetchRequestBundle] = Vec(FetchPorts, new FetchRequestBundle)
    val topdownInfo: FrontendTopDownBundle   = new FrontendTopDownBundle
  }
  val req:              DecoupledIO[FtqToIfuReq]        = Decoupled(new FtqToIfuReq)
  val redirect:         Valid[BranchPredictionRedirect] = Valid(new BranchPredictionRedirect)
  val topdown_redirect: Valid[BranchPredictionRedirect] = Valid(new BranchPredictionRedirect)
  val flushFromBpu:     BpuFlushInfo                    = new BpuFlushInfo
}

class IfuToFtqIO(implicit p: Parameters) extends XSBundle {
  val mmioCommitRead: MmioCommitRead                        = new MmioCommitRead
  val pdWb:           Vec[Valid[PredecodeWritebackBundle]]  = Vec(FetchPorts, Valid(new PredecodeWritebackBundle))
}

class PredecodeWritebackBundle(implicit p: Parameters) extends XSBundle {
  val pd         = Vec(PredictWidth, new PreDecodeInfo) // TODO: redefine Predecode
  val pc         = PrunedAddr(VAddrBits)
  val ftqIdx     = new FtqPtr
  val ftqOffset  = UInt(log2Ceil(PredictWidth).W)
  val misOffset  = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val cfiOffset  = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val target     = PrunedAddr(VAddrBits)
  val jalTarget  = PrunedAddr(VAddrBits)
  val instrRange = Vec(PredictWidth, Bool())
}

class MmioCommitRead(implicit p: Parameters) extends XSBundle {
  val mmioFtqPtr     = Output(new FtqPtr)
  val mmioLastCommit = Input(Bool())
}

class ExceptionType extends Bundle {
  val value: UInt = ExceptionType.Value()

  def isNone: Bool = value === ExceptionType.Value.None
  def isPf:   Bool = value === ExceptionType.Value.Pf
  def isGpf:  Bool = value === ExceptionType.Value.Gpf
  def isAf:   Bool = value === ExceptionType.Value.Af

  def hasException: Bool = value =/= ExceptionType.Value.None

  /** merge exception from multiple sources, the leftmost one has higher priority
   * @example {{{
   *   val itlbException = ExceptionType(io.itlb.resp.bits)
   *   val pmpException = ExceptionType(io.pmp.resp)
   *
   *   // itlb has higher priority than pmp, as when itlb has exception, pAddr is not valid
   *   val exception = itlbException || pmpException
   * }}}
   */
  def ||(that: ExceptionType): ExceptionType =
    Mux(this.hasException, this, that)
}

object ExceptionType {
  private object Value extends EnumUInt(4) {
    def None: UInt = 0.U(width.W)
    def Pf:   UInt = 1.U(width.W) // instruction page fault
    def Gpf:  UInt = 2.U(width.W) // instruction guest page fault
    def Af:   UInt = 3.U(width.W) // instruction access fault
  }

  def apply(that: UInt): ExceptionType = {
    Value.assertLegal(that)
    val e = Wire(new ExceptionType)
    e.value := that
    e
  }

  def None: ExceptionType = apply(Value.None)
  def Pf:   ExceptionType = apply(Value.Pf)
  def Gpf:  ExceptionType = apply(Value.Gpf)
  def Af:   ExceptionType = apply(Value.Af)

  def apply(hasPf: Bool, hasGpf: Bool, hasAf: Bool): ExceptionType = {
    assert(
      PopCount(VecInit(hasPf, hasGpf, hasAf)) <= 1.U,
      "ExceptionType receives input that is not one-hot: pf=%d, gpf=%d, af=%d",
      hasPf,
      hasGpf,
      hasAf
    )
    // input is at-most-one-hot encoded, so we don't worry about priority here.
    MuxCase(
      None,
      Seq(
        hasPf  -> Pf,
        hasGpf -> Gpf,
        hasAf  -> Af
      )
    )
  }

  // only af is used most frequently (pmp / ecc / tilelink), so we define a shortcut
  // we cannot use default parameter in apply(), as it is overloaded and scala won't allow
  def apply(hasAf: Bool): ExceptionType =
    apply(hasPf = false.B, hasGpf = false.B, hasAf = hasAf)

  // raise pf/gpf/af according to itlb response
  def fromTlbResp(resp: TlbResp, useDup: Int = 0): ExceptionType = {
    require(useDup >= 0 && useDup < resp.excp.length)
    // itlb is guaranteed to respond at most one exception
    apply(
      hasPf = resp.excp(useDup).pf.instr,
      hasGpf = resp.excp(useDup).gpf.instr,
      hasAf = resp.excp(useDup).af.instr
    )
  }

  // raise af if pmp check failed
  def fromPmpResp(resp: PMPRespBundle): ExceptionType =
    apply(hasAf = resp.instr)
}

object BrType extends EnumUInt(4) {
  def NotCfi: UInt = 0.U(width.W)
  def Branch: UInt = 1.U(width.W)
  def Jal:    UInt = 2.U(width.W)
  def Jalr:   UInt = 3.U(width.W)
}

class PreDecodeInfo extends Bundle { // 8 bit
  val valid  = Bool()
  val isRVC  = Bool()
  val brType = UInt(2.W)
  val isCall = Bool()
  val isRet  = Bool()
  // val excType = UInt(3.W)
  def isBr   = brType === BrType.Branch
  def isJal  = brType === BrType.Jal
  def isJalr = brType === BrType.Jalr
  def notCFI = brType === BrType.NotCfi
}

// pc = ftq.startAddr + Cat(offset, 0.U(1.W)) - Cat(borrow, 0.U(1.W))
class FtqPcOffset(implicit p: Parameters) extends XSBundle {
  val borrow = Bool()
  val offset = UInt(log2Ceil(PredictWidth).W)
}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs           = Vec(PredictWidth, UInt(32.W))
  val valid            = UInt(PredictWidth.W)
  val enqEnable        = UInt(PredictWidth.W)
  val pd               = Vec(PredictWidth, new PreDecodeInfo)
  val foldpc           = Vec(PredictWidth, UInt(MemPredPCWidth.W))
  val ftqPcOffset      = Vec(PredictWidth, ValidUndirectioned(new FtqPcOffset))
  val backendException = Vec(PredictWidth, Bool())
  val exceptionType    = Vec(PredictWidth, new ExceptionType)
  val crossPageIPFFix  = Vec(PredictWidth, Bool())
  val illegalInstr     = Vec(PredictWidth, Bool())
  val triggered        = Vec(PredictWidth, TriggerAction())
  val isLastInFtqEntry = Vec(PredictWidth, Bool())

  val pc             = Vec(PredictWidth, PrunedAddr(VAddrBits))
  val prevIBufEnqPtr = new IBufPtr
  val debug_seqNum   = Vec(PredictWidth, InstSeqNum())
  val ftqPtr         = new FtqPtr
  val topdown_info   = new FrontendTopDownBundle
}

class IfuToBackendIO(implicit p: Parameters) extends XSBundle {
  // write to backend gpaddr mem
  val gpaddrMem_wen   = Output(Bool())
  val gpaddrMem_waddr = Output(UInt(log2Ceil(FtqSize).W)) // Ftq Ptr
  // 2 gpaddrs, correspond to startAddr & nextLineAddr in bundle FtqICacheInfo
  // TODO: avoid cross page entry in Ftq
  val gpaddrMem_wdata = Output(new GPAMemEntry)
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
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr + 1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: real_taken_mask)
    val smaller         = Mux(last_valid_idx < first_taken_idx, last_valid_idx, first_taken_idx)
    val shift           = smaller
    val taken           = real_taken_mask.reduce(_ || _)
    update(shift, taken, this.predHist)
  }

  // static read
  def read(n: Int): Bool = predHist.asBools(n)

  final def ===(that: ShiftingGlobalHistory): Bool =
    predHist === that.predHist

  final def =/=(that: ShiftingGlobalHistory): Bool = !(this === that)
}

// circular global history pointer
class CGHPtr(implicit p: Parameters) extends CircularQueuePtr[CGHPtr](p => p(XSCoreParamsKey).HistoryLength) {}

object CGHPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): CGHPtr = {
    val ptr = Wire(new CGHPtr)
    ptr.flag  := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: CGHPtr)(implicit p: Parameters): CGHPtr =
    apply(!ptr.flag, ptr.value)
}

class CircularGlobalHistory(implicit p: Parameters) extends GlobalHistory {
  val buffer = Vec(HistoryLength, Bool())
  type HistPtr = UInt
  def update(br_valids: Vec[Bool], real_taken_mask: Vec[Bool]): CircularGlobalHistory =
    this
}

class FoldedHistory(val len: Int, val compLen: Int, val max_update_num: Int)(implicit p: Parameters)
    extends XSBundle with HasBPUConst {
  require(compLen >= 1)
  require(len > 0)
  // require(folded_len <= len)
  require(compLen >= max_update_num)
  val folded_hist = UInt(compLen.W)

  def need_oldest_bits           = len > compLen
  def info                       = (len, compLen)
  def oldest_bit_to_get_from_ghr = (0 until max_update_num).map(len - _ - 1)
  def oldest_bit_pos_in_folded   = oldest_bit_to_get_from_ghr map (_ % compLen)
  def oldest_bit_wrap_around     = oldest_bit_to_get_from_ghr map (_ / compLen > 0)
  def oldest_bit_start           = oldest_bit_pos_in_folded.head

  def get_oldest_bits_from_ghr(ghr: Vec[Bool], histPtr: CGHPtr) =
    // TODO: wrap inc for histPtr value
    oldest_bit_to_get_from_ghr.map(i => ghr((histPtr + (i + 1).U).value))

  def circular_shift_left(src: UInt, shamt: Int) = {
    val srcLen      = src.getWidth
    val src_doubled = Cat(src, src)
    val shifted     = src_doubled(srcLen * 2 - 1 - shamt, srcLen - shamt)
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
        if (resArr(i).length == 0) {
          println(f"[error] bits $i is not assigned in folded hist update logic! histlen:${this.len}, compLen:$compLen")
        }
        res(i) := resArr(i).foldLeft(false.B)(_ ^ _)
      }
      res.asUInt
    }

    val new_folded_hist = if (need_oldest_bits) {
      val oldest_bits = ob
      require(oldest_bits.length == max_update_num)
      // mask off bits that do not update
      val oldest_bits_masked = oldest_bits.zipWithIndex.map {
        case (ob, i) => ob && (i < num).B
      }
      // if a bit does not wrap around, it should not be xored when it exits
      val oldest_bits_set = (0 until max_update_num).filter(oldest_bit_wrap_around).map(i =>
        (oldest_bit_pos_in_folded(i), oldest_bits_masked(i))
      )

      // println(f"old bits pos ${oldest_bits_set.map(_._1)}")

      // only the last bit could be 1, as we have at most one taken branch at a time
      val newest_bits_masked = VecInit((0 until max_update_num).map(i => taken && ((i + 1) == num).B)).asUInt
      // if a bit does not wrap around, newest bits should not be xored onto it either
      val newest_bits_set = (0 until max_update_num).map(i => (compLen - 1 - i, newest_bits_masked(i)))

      // println(f"new bits set ${newest_bits_set.map(_._1)}")
      //
      val original_bits_masked = VecInit(folded_hist.asBools.zipWithIndex.map {
        case (fb, i) => fb && !(num >= (len - i)).B
      })
      val original_bits_set = (0 until compLen).map(i => (i, original_bits_masked(i)))

      // do xor then shift
      val xored = bitsets_xor(compLen, Seq(original_bits_set, oldest_bits_set, newest_bits_set))
      circular_shift_left(xored, num)
    } else {
      // histLen too short to wrap around
      ((folded_hist << num) | taken)(compLen - 1, 0)
    }

    val fh = WireInit(this)
    fh.folded_hist := new_folded_hist
    fh
  }
}

class AheadFoldedHistoryOldestBits(val len: Int, val max_update_num: Int)(implicit p: Parameters) extends XSBundle {
  val bits = Vec(max_update_num * 2, Bool())
  // def info = (len, compLen)
  def getRealOb(brNumOH: UInt): Vec[Bool] = {
    val ob = Wire(Vec(max_update_num, Bool()))
    for (i <- 0 until max_update_num) {
      ob(i) := Mux1H(brNumOH, bits.drop(i).take(numBr + 1))
    }
    ob
  }
}

class AllAheadFoldedHistoryOldestBits(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle
    with HasBPUConst {
  val afhob = MixedVec(gen.filter(t => t._1 > t._2).map(_._1)
    .toSet.toList.map(l => new AheadFoldedHistoryOldestBits(l, numBr))) // remove duplicates
  require(gen.toSet.toList.equals(gen))
  def getObWithInfo(info: Tuple2[Int, Int]) = {
    val selected = afhob.filter(_.len == info._1)
    require(selected.length == 1)
    selected(0)
  }
  def read(ghv: Vec[Bool], ptr: CGHPtr) = {
    val hisLens      = afhob.map(_.len)
    val bitsToRead   = hisLens.flatMap(l => (0 until numBr * 2).map(i => l - i - 1)).toSet // remove duplicates
    val bitsWithInfo = bitsToRead.map(pos => (pos, ghv((ptr + (pos + 1).U).value)))
    for (ob <- afhob) {
      for (i <- 0 until numBr * 2) {
        val pos       = ob.len - i - 1
        val bit_found = bitsWithInfo.filter(_._1 == pos).toList
        require(bit_found.length == 1)
        ob.bits(i) := bit_found(0)._2
      }
    }
  }
}

class AllFoldedHistories(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val hist = MixedVec(gen.map { case (l, cl) => new FoldedHistory(l, cl, numBr) })
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
        val info          = fh.info
        val selectedAfhob = afhob.getObWithInfo(info)
        val ob            = selectedAfhob.getRealOb(lastBrNumOH)
        res.hist(i) := this.hist(i).update(ob, shift, taken)
      } else {
        val dumb = Wire(Vec(numBr, Bool())) // not needed
        dumb        := DontCare
        res.hist(i) := this.hist(i).update(dumb, shift, taken)
      }
    }
    res
  }

  def display(cond: Bool) =
    for (h <- hist) {
      XSDebug(cond, p"hist len ${h.len}, folded len ${h.compLen}, value ${Binary(h.folded_hist)}\n")
    }
}

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends XSBundle {
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
//  val offset = UInt(instOffsetBits.W)

  def fromUInt(x:   PrunedAddr): TableAddr = x.asTypeOf(PrunedAddr(VAddrBits)).asTypeOf(this)
  def getTag(x:     PrunedAddr): UInt      = fromUInt(x).tag
  def getIdx(x:     PrunedAddr): UInt      = fromUInt(x).idx
  def getBank(x:    PrunedAddr): UInt      = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: PrunedAddr): UInt      = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}

trait BasicPrediction extends HasXSParameter {
  def cfiIndex: ValidUndirectioned[UInt]
  def target(pc: PrunedAddr): PrunedAddr
  def lastBrPosOH:    Vec[Bool]
  def brTaken:        Bool
  def shouldShiftVec: Vec[Bool]
  def fallThruError:  Bool
}

// selectByTaken selects some data according to takenMask
// allTargets should be in a Vec, like [taken0, taken1, ..., not taken, not hit]
object selectByTaken {
  def apply[T <: Data](takenMask: Vec[Bool], hit: Bool, allTargets: Vec[T]): T = {
    val selVecOH =
      takenMask.zipWithIndex.map { case (t, i) =>
        !takenMask.take(i).fold(false.B)(_ || _) && t && hit
      } :+
        (!takenMask.asUInt.orR && hit) :+ !hit
    Mux1H(selVecOH, allTargets)
  }
}

class OldFullBranchPrediction(val isNotS3: Boolean)(implicit p: Parameters) extends XSBundle with HasBPUConst
    with BasicPrediction {
  val br_taken_mask = Vec(numBr, Bool())

  val slot_valids = Vec(totalSlot, Bool())

  val targets         = Vec(totalSlot, PrunedAddr(VAddrBits))
  val jalr_target     = PrunedAddr(VAddrBits) // special path for indirect predictors
  val offsets         = Vec(totalSlot, UInt(log2Ceil(PredictWidth).W))
  val fallThroughAddr = PrunedAddr(VAddrBits)
  val fallThroughErr  = Bool()
  val multiHit        = Bool()

  val is_jal               = Bool()
  val is_jalr              = Bool()
  val is_call              = Bool()
  val is_ret               = Bool()
  val last_may_be_rvi_call = Bool()
  val is_br_sharing        = Bool()

  // val call_is_rvc = Bool()
  val hit = Bool()

  val predCycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  def br_slot_valids  = slot_valids.init
  def tail_slot_valid = slot_valids.last

  def br_valids =
    VecInit(br_slot_valids :+ (tail_slot_valid && is_br_sharing))

  def taken_mask_on_slot =
    VecInit(
      (br_slot_valids zip br_taken_mask.init).map { case (t, v) => t && v } :+ (
        tail_slot_valid && (
          is_br_sharing && br_taken_mask.last || !is_br_sharing
        )
      )
    )

  def real_slot_taken_mask(): Vec[Bool] =
    VecInit(taken_mask_on_slot.map(_ && hit))

  // len numBr
  def real_br_taken_mask(): Vec[Bool] =
    VecInit(
      taken_mask_on_slot.map(_ && hit).init :+
        (br_taken_mask.last && tail_slot_valid && is_br_sharing && hit)
    )

  // the vec indicating if ghr should shift on each branch
  def shouldShiftVec =
    VecInit(br_valids.zipWithIndex.map { case (v, i) =>
      v && hit && !real_br_taken_mask().take(i).reduceOption(_ || _).getOrElse(false.B)
    })

  def lastBrPosOH =
    VecInit((!hit || !br_valids.reduce(_ || _)) +: // not hit or no brs in entry
      (0 until numBr).map(i =>
        br_valids(i) &&
          !real_br_taken_mask().take(i).reduceOption(_ || _).getOrElse(false.B) && // no brs taken in front it
          (real_br_taken_mask()(i) || !br_valids.drop(i + 1).reduceOption(_ || _).getOrElse(
            false.B
          )) && // no brs behind it
          hit
      ))

  def brTaken = (br_valids zip br_taken_mask).map { case (a, b) => a && b && hit }.reduce(_ || _)

  def target(pc: PrunedAddr): PrunedAddr =
    if (isNotS3) {
      selectByTaken(taken_mask_on_slot, hit, allTarget(pc))
    } else {
      selectByTaken(taken_mask_on_slot, hit && !fallThroughErr, allTarget(pc))
    }

  // allTarget return a Vec of all possible target of a BP stage
  // in the following order: [taken_target0, taken_target1, ..., fallThroughAddr, not hit (plus fetch width)]
  //
  // This exposes internal targets for timing optimization,
  // since usually targets are generated quicker than taken
  def allTarget(pc: PrunedAddr): Vec[PrunedAddr] =
    VecInit(targets :+ fallThroughAddr :+ (pc + (FetchWidth * 4).U))

  def fallThruError: Bool = hit && fallThroughErr
  def ftbMultiHit:   Bool = hit && multiHit

  def hit_taken_on_jmp =
    !real_slot_taken_mask().init.reduce(_ || _) &&
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

  def taken = br_taken_mask.reduce(_ || _) || slot_valids.last // || (is_jal || is_jalr)

  def fromFtbEntry(
      entry:            FTBEntry,
      pc:               PrunedAddr,
      last_stage_pc:    Option[Tuple2[PrunedAddr, Bool]] = None,
      last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None
  ) = {
    slot_valids          := entry.brSlots.map(_.valid) :+ entry.tailSlot.valid
    targets              := entry.getTargetVec(pc, last_stage_pc) // Use previous stage pc for better timing
    jalr_target          := targets.last
    offsets              := entry.getOffsetVec
    is_jal               := entry.tailSlot.valid && entry.isJal
    is_jalr              := entry.tailSlot.valid && entry.isJalr
    is_call              := entry.tailSlot.valid && entry.isCall
    is_ret               := entry.tailSlot.valid && entry.isRet
    last_may_be_rvi_call := entry.last_may_be_rvi_call
    is_br_sharing        := entry.tailSlot.valid && entry.tailSlot.sharing
    predCycle.map(_ := GTimer())

    val startLower        = Cat(0.U(1.W), pc(instOffsetBits + log2Ceil(PredictWidth) - 1, instOffsetBits))
    val endLowerwithCarry = Cat(entry.carry, entry.pftAddr)
    fallThroughErr  := startLower >= endLowerwithCarry || endLowerwithCarry > (startLower + PredictWidth.U)
    fallThroughAddr := Mux(fallThroughErr, pc + (FetchWidth * 4).U, entry.getFallThrough(pc, last_stage_entry))
  }

  def display(cond: Bool): Unit =
    XSDebug(cond, p"[taken_mask] ${Binary(br_taken_mask.asUInt)} [hit] $hit\n")
}

class RasSpeculativeInfo(implicit p: Parameters) extends XSBundle with HasBPUConst with BPUUtils {
  val ssp     = UInt(log2Up(RasSize).W)
  val sctr    = UInt(RasCtrSize.W)
  val TOSW    = new RasPtr
  val TOSR    = new RasPtr
  val NOS     = new RasPtr
  val topAddr = PrunedAddr(VAddrBits)
}

class BranchPredictionBundle(val isNotS3: Boolean)(implicit p: Parameters) extends XSBundle
    with HasBPUConst with BPUUtils {
  val pc          = PrunedAddr(VAddrBits)
  val valid       = Bool()
  val hasRedirect = Bool()
  val ftq_idx     = new FtqPtr
  val full_pred   = new OldFullBranchPrediction(isNotS3)

  def target(pc:     PrunedAddr) = full_pred.target(pc)
  def allTargets(pc: PrunedAddr) = full_pred.allTarget(pc)
  def cfiIndex       = full_pred.cfiIndex
  def lastBrPosOH    = full_pred.lastBrPosOH
  def brTaken        = full_pred.brTaken
  def shouldShiftVec = full_pred.shouldShiftVec
  def fallThruError  = full_pred.fallThruError
  def ftbMultiHit    = full_pred.ftbMultiHit

  def taken = cfiIndex.valid

  def getTarget     = target(pc)
  def getAllTargets = allTargets(pc)

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"[pc] ${Hexadecimal(pc.toUInt)}\n")
    full_pred.display(cond)
  }
}

class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val s1 = new BranchPredictionBundle(isNotS3 = true)
  val s2 = new BranchPredictionBundle(isNotS3 = true)
  val s3 = new BranchPredictionBundle(isNotS3 = false)

  val s3_specInfo = new FtqRedirectSramEntry
  val s3_ftbEntry = new FTBEntry
  val s3_meta     = new OldPredictorMeta

  val topdown_info = new FrontendTopDownBundle

  def stage(idx: Int): BranchPredictionBundle = {
    require(idx >= 1 && idx <= 3)
    idx match {
      case 1 => s1
      case 2 => s2
      case 3 => s3
    }
  }

  def selectedResp = {
    val res =
      PriorityMux(Seq(
        (s3.valid && s3.hasRedirect) -> s3,
        (s2.valid && s2.hasRedirect) -> s2,
        s1.valid                     -> s1
      ))
    res
  }
  def selectedRespIdxForFtq =
    PriorityMux(Seq(
      (s3.valid && s3.hasRedirect) -> BP_S3,
      (s2.valid && s2.hasRedirect) -> BP_S2,
      s1.valid                     -> BP_S1
    ))
  def lastStage = s3
}

class BranchPredictionUpdate(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc        = PrunedAddr(VAddrBits)
  val spec_info = new FtqRedirectSramEntry
  val ftb_entry = new FTBEntry

  val ftqOffset         = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val br_taken_mask     = Vec(numBr, Bool())
  val br_committed      = Vec(numBr, Bool()) // High only when br valid && br committed
  val jmp_taken         = Bool()
  val mispred_mask      = Vec(numBr + 1, Bool())
  val pred_hit          = Bool()
  val false_hit         = Bool()
  val new_br_insert_pos = Vec(numBr, Bool())
  val old_entry         = Bool()
  val meta              = new BpuMeta
  val full_target       = PrunedAddr(VAddrBits)
  val from_stage        = UInt(2.W)
  val ghist             = UInt(HistoryLength.W)

  def is_jal  = ftb_entry.tailSlot.valid && ftb_entry.isJal
  def is_jalr = ftb_entry.tailSlot.valid && ftb_entry.isJalr
  def is_call = ftb_entry.tailSlot.valid && ftb_entry.isCall
  def is_ret  = ftb_entry.tailSlot.valid && ftb_entry.isRet

  def is_call_taken = is_call && jmp_taken && ftqOffset.valid && ftqOffset.bits === ftb_entry.tailSlot.offset
  def is_ret_taken  = is_ret && jmp_taken && ftqOffset.valid && ftqOffset.bits === ftb_entry.tailSlot.offset

  def display(cond: Bool) = {
    XSDebug(cond, p"-----------BranchPredictionUpdate-----------\n")
    XSDebug(cond, p"[mispred_mask] ${Binary(mispred_mask.asUInt)} [false_hit] $false_hit\n")
    XSDebug(cond, p"[new_br_insert_pos] ${Binary(new_br_insert_pos.asUInt)}\n")
    XSDebug(cond, p"--------------------------------------------\n")
  }
}

class BranchPredictionRedirect(implicit p: Parameters) extends Redirect with HasBPUConst {
  // TODO: backend should pass topdown signals here
  // must not change its parent since BPU has used asTypeOf(this type) from its parent class
  require(isInstanceOf[Redirect])
  val BTBMissBubble         = Bool()
  def ControlRedirectBubble = debugIsCtrl
  // if mispred br not in ftb, count as BTB miss
  def ControlBTBMissBubble = ControlRedirectBubble && !cfiUpdate.br_hit && !cfiUpdate.jr_hit
  def TAGEMissBubble       = ControlRedirectBubble && cfiUpdate.br_hit && !cfiUpdate.sc_hit
  def SCMissBubble         = ControlRedirectBubble && cfiUpdate.br_hit && cfiUpdate.sc_hit
  def ITTAGEMissBubble     = ControlRedirectBubble && cfiUpdate.jr_hit && !cfiUpdate.pd.isRet
  def RASMissBubble        = ControlRedirectBubble && cfiUpdate.jr_hit && cfiUpdate.pd.isRet
  def MemVioRedirectBubble = debugIsMemVio
  def OtherRedirectBubble  = !debugIsCtrl && !debugIsMemVio

  def connectRedirect(source: Redirect): Unit =
    for ((name, data) <- this.elements) {
      if (source.elements.contains(name)) {
        data := source.elements(name)
      }
    }

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"-----------BranchPredictionRedirect----------- \n")
    XSDebug(cond, p"-----------cfiUpdate----------- \n")
    XSDebug(cond, p"[pc] ${Hexadecimal(cfiUpdate.pc)}\n")
    // XSDebug(cond, p"[hist] ${Binary(cfiUpdate.hist.predHist)}\n")
    XSDebug(cond, p"[br_hit] ${cfiUpdate.br_hit} [isMisPred] ${cfiUpdate.isMisPred}\n")
    XSDebug(
      cond,
      p"[pred_taken] ${cfiUpdate.predTaken} [taken] ${cfiUpdate.taken} [isMisPred] ${cfiUpdate.isMisPred}\n"
    )
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
