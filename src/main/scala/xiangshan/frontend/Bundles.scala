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
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.GTimer
import utility.ParallelPriorityMux
import utility.XSDebug
import utils.EnumUInt
import xiangshan._
import xiangshan.backend.GPAMemEntry
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.TlbResp
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.icache._
import xiangshan.frontend.instruncache.InstrUncacheReq
import xiangshan.frontend.instruncache.InstrUncacheResp

class FrontendTopDownBundle(implicit p: Parameters) extends XSBundle {
  val reasons    = Vec(TopDownCounters.NumStallReasons.id, Bool())
  val stallWidth = UInt(log2Ceil(PredictWidth).W)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val prediction:      DecoupledIO[BpuPrediction]      = DecoupledIO(new BpuPrediction)
  val speculationMeta: DecoupledIO[BpuSpeculationMeta] = DecoupledIO(new BpuSpeculationMeta)
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
  val ftqIdx:         FtqPtr      = new FtqPtr
  val takenCfiOffset: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
  val identifiedCfi:  Vec[Bool]   = Vec(FetchBlockInstNum, Bool())

  def crossCacheline: Bool = startVAddr(blockOffBits - 1) === 1.U

  override def toPrintable: Printable =
    p"[start] ${Hexadecimal(startVAddr.toUInt)} [next] ${Hexadecimal(nextCachelineVAddr.toUInt)}" +
      p"[tgt] ${Hexadecimal(nextStartVAddr.toUInt)} [ftqIdx] $ftqIdx [jmp] v:${takenCfiOffset.valid}" +
      p" offset: ${takenCfiOffset.bits}\n"
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

class FtqToFetchBundle(implicit p: Parameters) extends XSBundle {
  val req:                FtqICacheInfo = new FtqICacheInfo
  val isBackendException: Bool          = Bool()
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

class FtqToIfuIO(implicit p: Parameters) extends FrontendBundle {
  class FtqToIfuReq(implicit p: Parameters) extends Bundle {
    val fetch:       Vec[FetchRequestBundle] = Vec(FetchPorts, new FetchRequestBundle)
    val topdownInfo: FrontendTopDownBundle   = new FrontendTopDownBundle
  }
  val req:              DecoupledIO[FtqToIfuReq] = Decoupled(new FtqToIfuReq)
  val redirect:         Valid[Redirect]          = Valid(new Redirect)
  val topdown_redirect: Valid[Redirect]          = Valid(new Redirect)
  val flushFromBpu:     BpuFlushInfo             = new BpuFlushInfo
}

class IfuToFtqIO(implicit p: Parameters) extends XSBundle {
  val mmioCommitRead: MmioCommitRead                       = new MmioCommitRead
  val pdWb:           Vec[Valid[PredecodeWritebackBundle]] = Vec(FetchPorts, Valid(new PredecodeWritebackBundle))
}

class PredecodeWritebackBundle(implicit p: Parameters) extends XSBundle {
  val pd             = Vec(PredictWidth, new PreDecodeInfo) // TODO: redefine Predecode
  val pc             = PrunedAddr(VAddrBits)
  val ftqIdx         = new FtqPtr
  val takenCfiOffset = UInt(log2Ceil(PredictWidth).W)
  val misEndOffset   = Valid(UInt(log2Ceil(PredictWidth).W))
  val cfiEndOffset   = Valid(UInt(log2Ceil(PredictWidth).W))
  val target         = PrunedAddr(VAddrBits)
  val jalTarget      = PrunedAddr(VAddrBits)
  val instrRange     = Vec(PredictWidth, Bool())
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
  def isIll:  Bool = value === ExceptionType.Value.Ill
  def isHwe:  Bool = value === ExceptionType.Value.Hwe

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
  private object Value extends EnumUInt(6) {
    def None: UInt = 0.U(width.W)
    def Pf:   UInt = 1.U(width.W) // instruction page fault
    def Gpf:  UInt = 2.U(width.W) // instruction guest page fault
    def Af:   UInt = 3.U(width.W) // instruction access fault
    def Ill:  UInt = 4.U(width.W) // illegal instruction
    def Hwe:  UInt = 5.U(width.W) // hardware error
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
  def Ill:  ExceptionType = apply(Value.Ill)
  def Hwe:  ExceptionType = apply(Value.Hwe)

  def apply(hasPf: Bool, hasGpf: Bool, hasAf: Bool, hasIll: Bool, hasHwe: Bool): ExceptionType = {
    assert(
      PopCount(VecInit(hasPf, hasGpf, hasAf, hasIll, hasHwe)) <= 1.U,
      "ExceptionType receives input that is not one-hot: pf=%d, gpf=%d, af=%d, ill=%d, hwe=%d",
      hasPf,
      hasGpf,
      hasAf,
      hasIll,
      hasHwe
    )
    // input is at-most-one-hot encoded, so we don't worry about priority here.
    MuxCase(
      None,
      Seq(
        hasPf  -> Pf,
        hasGpf -> Gpf,
        hasAf  -> Af,
        hasIll -> Ill,
        hasHwe -> Hwe
      )
    )
  }

  // only af is used most frequently (pmp / ecc / tilelink), so we define a shortcut
  // we cannot use default parameter in apply(), as it is overloaded and scala won't allow
  def apply(hasAf: Bool): ExceptionType =
    apply(hasPf = false.B, hasGpf = false.B, hasAf = hasAf, hasIll = false.B, hasHwe = false.B)

  // raise pf/gpf/af according to backend request (via redirect.cfiUpdate)
  def fromBackend(redirect: Redirect): ExceptionType =
    apply(
      hasPf = redirect.backendIPF,
      hasGpf = redirect.backendIGPF,
      hasAf = redirect.backendIAF,
      hasIll = false.B,
      hasHwe = false.B
    )

  // raise pf/gpf/af according to itlb response
  def fromTlbResp(resp: TlbResp, useDup: Int = 0): ExceptionType = {
    require(useDup >= 0 && useDup < resp.excp.length)
    // itlb is guaranteed to respond at most one exception
    apply(
      hasPf = resp.excp(useDup).pf.instr,
      hasGpf = resp.excp(useDup).gpf.instr,
      hasAf = resp.excp(useDup).af.instr,
      hasIll = false.B,
      hasHwe = false.B
    )
  }

  // raise af if pmp check failed
  def fromPmpResp(resp: PMPRespBundle): ExceptionType =
    apply(hasAf = resp.instr)

  // raise af / hwe according to tilelink response
  def fromTileLink(corrupt: Bool, denied: Bool): ExceptionType = {
    //  corrupt && denied -> access fault
    //  corrupt && !denied -> hardware error
    //  !corrupt && !denied -> no exception
    //  !corrupt && denied -> violates tilelink specification, should not happen
    assert(!corrupt || denied, "TileLink response should not be !corrupt && denied at the same time")
    apply(hasPf = false.B, hasGpf = false.B, hasAf = denied, hasIll = false.B, hasHwe = corrupt && !denied)
  }

  // raise ill according to rvc expander
  def fromRvcExpander(ill: Bool): ExceptionType =
    apply(hasPf = false.B, hasGpf = false.B, hasAf = false.B, hasIll = ill, hasHwe = false.B)
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

class InstrEndOffset(implicit p: Parameters) extends XSBundle {
  val taken  = Bool()
  val offset = UInt(log2Ceil(PredictWidth).W)
}

class FetchToIBuffer(implicit p: Parameters) extends XSBundle {
  val instrs         = Vec(IBufEnqWidth, UInt(32.W))
  val valid          = UInt(IBufEnqWidth.W)
  val enqEnable      = UInt(IBufEnqWidth.W)
  val pd             = Vec(IBufEnqWidth, new PreDecodeInfo)
  val foldpc         = Vec(IBufEnqWidth, UInt(MemPredPCWidth.W))
  val instrEndOffset = Vec(IBufEnqWidth, new InstrEndOffset)
  // valftqPcOffset        = Vec(IBufEnqWidth, ValidUndirectioned(new FtqPcOffset))
  val isBackendException = Vec(IBufEnqWidth, Bool())
  val exceptionType      = Vec(IBufEnqWidth, new ExceptionType)
  val exceptionCrossPage = Vec(IBufEnqWidth, Bool())
  val triggered          = Vec(IBufEnqWidth, TriggerAction())
  val isLastInFtqEntry   = Vec(IBufEnqWidth, Bool())

  val pc             = Vec(IBufEnqWidth, PrunedAddr(VAddrBits))
  val prevIBufEnqPtr = new IBufPtr
  val debug_seqNum   = Vec(IBufEnqWidth, InstSeqNum())
  val ftqPtr         = new FtqPtr
  val topdown_info   = new FrontendTopDownBundle

  val identifiedCfi: Vec[Bool] = Vec(IBufEnqWidth, Bool())
}

class IfuToBackendIO(implicit p: Parameters) extends XSBundle {
  // write to backend gpaddr mem
  val gpaddrMem_wen   = Output(Bool())
  val gpaddrMem_waddr = Output(UInt(log2Ceil(FtqSize).W)) // Ftq Ptr
  // 2 gpaddrs, correspond to startAddr & nextLineAddr in bundle FtqICacheInfo
  // TODO: avoid cross page entry in Ftq
  val gpaddrMem_wdata = Output(new GPAMemEntry)
}
