// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend

import chisel3._
import chisel3.util._
import ftq.BpuFlushInfo
import ftq.FtqPtr
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.InstSeqNum
import xiangshan.Redirect
import xiangshan.TopDownCounters
import xiangshan.TriggerAction
import xiangshan.backend.GPAMemEntry
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.TlbResp
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.ibuffer.IBufPtr
import xiangshan.frontend.icache.ICacheCacheLineHelper
import xiangshan.frontend.icache.ICachePerfInfo
import xiangshan.frontend.icache.ICacheRespBundle
import xiangshan.frontend.icache.ICacheTopdownInfo
import xiangshan.frontend.instruncache.InstrUncacheReq
import xiangshan.frontend.instruncache.InstrUncacheResp

class FrontendTopDownBundle(implicit p: Parameters) extends FrontendBundle {
  val reasons:    Vec[Bool] = Vec(TopDownCounters.NumStallReasons.id, Bool())
  val stallWidth: UInt      = UInt(FetchBlockInstOffsetWidth.W)
}

class BpuToFtqIO(implicit p: Parameters) extends FrontendBundle {
  val prediction:      DecoupledIO[BpuPrediction]      = Decoupled(new BpuPrediction)
  val speculationMeta: DecoupledIO[BpuSpeculationMeta] = Decoupled(new BpuSpeculationMeta)
  val meta:            DecoupledIO[BpuMeta]            = Decoupled(new BpuMeta)
  val s3FtqPtr:        FtqPtr                          = Output(new FtqPtr)
  // TODO: topdown, etc.
}

class FtqToBpuIO(implicit p: Parameters) extends FrontendBundle {
  val redirect:        Valid[BpuRedirect] = Valid(new BpuRedirect)
  val train:           Valid[BpuTrain]    = Valid(new BpuTrain)
  val bpuPtr:          FtqPtr             = Output(new FtqPtr)
  val redirectFromIFU: Bool               = Output(Bool())
}

// TODO: unify FetchRequestBundle (Ftq->Ifu) with FtqFetchRequest (Ftq->ICache.MainPipe)
class FetchRequestBundle(implicit p: Parameters) extends FrontendBundle with ICacheCacheLineHelper {

  // fast path: Timing critical
  val valid:              Bool       = Bool()
  val startVAddr:         PrunedAddr = PrunedAddr(VAddrBits)
  val nextCachelineVAddr: PrunedAddr = PrunedAddr(VAddrBits)
  val nextStartVAddr:     PrunedAddr = PrunedAddr(VAddrBits)
  // slow path
  val ftqIdx:         FtqPtr      = new FtqPtr
  val takenCfiOffset: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))

  def crossCacheline: Bool = super.isCrossLine(this.startVAddr, this.takenCfiOffset.bits)

  override def toPrintable: Printable =
    p"[start] ${Hexadecimal(startVAddr.toUInt)} [next] ${Hexadecimal(nextCachelineVAddr.toUInt)}" +
      p"[tgt] ${Hexadecimal(nextStartVAddr.toUInt)} [ftqIdx] $ftqIdx [jmp] v:${takenCfiOffset.valid}" +
      p" offset: ${takenCfiOffset.bits}\n"
}

class FtqPrefetchRequest(implicit p: Parameters) extends FrontendBundle with ICacheCacheLineHelper {
  val startVAddr:         PrunedAddr    = PrunedAddr(VAddrBits)
  val nextCachelineVAddr: PrunedAddr    = PrunedAddr(VAddrBits)
  val ftqIdx:             FtqPtr        = new FtqPtr
  val takenCfiOffset:     UInt          = UInt(CfiPositionWidth.W)
  val backendException:   ExceptionType = new ExceptionType

  def crossCacheline: Bool = super.isCrossLine(this.startVAddr, this.takenCfiOffset)
}

class FtqFetchRequest(implicit p: Parameters) extends FrontendBundle with ICacheCacheLineHelper {
  val startVAddr:         PrunedAddr = PrunedAddr(VAddrBits)
  val nextCachelineVAddr: PrunedAddr = PrunedAddr(VAddrBits)
  val ftqIdx:             FtqPtr     = new FtqPtr
  val takenCfiOffset:     UInt       = UInt(CfiPositionWidth.W)
  val isBackendException: Bool       = Bool()

  def crossCacheline: Bool = super.isCrossLine(this.startVAddr, this.takenCfiOffset)
}

class FtqToICacheIO(implicit p: Parameters) extends FrontendBundle {
  // NOTE: req.bits must be prepared in T cycle
  // while req.valid is set true in T + 1 cycle
  val fetchReq:      DecoupledIO[FtqFetchRequest]    = Decoupled(new FtqFetchRequest)
  val prefetchReq:   DecoupledIO[FtqPrefetchRequest] = Decoupled(new FtqPrefetchRequest)
  val flushFromBpu:  BpuFlushInfo                    = new BpuFlushInfo
  val redirectFlush: Bool                            = Output(Bool())
}

class ICacheToIfuIO(implicit p: Parameters) extends FrontendBundle {
  val fetchResp:  Valid[ICacheRespBundle] = Valid(new ICacheRespBundle)
  val topdown:    ICacheTopdownInfo       = Output(new ICacheTopdownInfo)
  val perf:       ICachePerfInfo          = Output(new ICachePerfInfo)
  val fetchReady: Bool                    = Output(Bool())
}

class IfuToICacheIO(implicit p: Parameters) extends FrontendBundle {
  val stall: Bool = Output(Bool())
}

class IfuToInstrUncacheIO(implicit p: Parameters) extends FrontendBundle {
  val req: DecoupledIO[InstrUncacheReq] = Decoupled(new InstrUncacheReq)
}

class InstrUncacheToIfuIO(implicit p: Parameters) extends FrontendBundle {
  val resp: DecoupledIO[InstrUncacheResp] = Decoupled(new InstrUncacheResp)
}

class FtqToIfuIO(implicit p: Parameters) extends FrontendBundle {
  class FtqToIfuReq extends Bundle {
    val fetch:       Vec[FetchRequestBundle] = Vec(FetchPorts, new FetchRequestBundle)
    val topdownInfo: FrontendTopDownBundle   = new FrontendTopDownBundle
  }
  val req:             DecoupledIO[FtqToIfuReq] = Decoupled(new FtqToIfuReq)
  val redirect:        Valid[Redirect]          = Valid(new Redirect)
  val topdownRedirect: Valid[Redirect]          = Valid(new Redirect) // TODO: what's this for?
  val flushFromBpu:    BpuFlushInfo             = new BpuFlushInfo
}

class IfuToFtqIO(implicit p: Parameters) extends FrontendBundle {
  val mmioCommitRead: MmioCommitRead                       = new MmioCommitRead
  val pdWb:           Vec[Valid[PredecodeWritebackBundle]] = Vec(FetchPorts, Valid(new PredecodeWritebackBundle))
}

class PredecodeWritebackBundle(implicit p: Parameters) extends FrontendBundle {
  val pd:             Vec[PreDecodeInfo] = Vec(FetchBlockInstNum, new PreDecodeInfo) // TODO: redefine Predecode
  val pc:             PrunedAddr         = PrunedAddr(VAddrBits)
  val ftqIdx:         FtqPtr             = new FtqPtr
  val takenCfiOffset: UInt               = UInt(FetchBlockInstOffsetWidth.W)
  val misEndOffset:   Valid[UInt]        = Valid(UInt(FetchBlockInstOffsetWidth.W))
  val cfiEndOffset:   Valid[UInt]        = Valid(UInt(FetchBlockInstOffsetWidth.W))
  val target:         PrunedAddr         = PrunedAddr(VAddrBits)
  val jalTarget:      PrunedAddr         = PrunedAddr(VAddrBits)
  val instrRange:     Vec[Bool]          = Vec(FetchBlockInstNum, Bool())
}

class MmioCommitRead(implicit p: Parameters) extends FrontendBundle {
  val mmioFtqPtr:     FtqPtr = Output(new FtqPtr)
  val mmioLastCommit: Bool   = Input(Bool())
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
  val valid:  Bool = Bool()
  val isRVC:  Bool = Bool()
  val brType: UInt = UInt(2.W)
  val isCall: Bool = Bool()
  val isRet:  Bool = Bool()
  // val excType = UInt(3.W)
  def isBr:   Bool = brType === BrType.Branch
  def isJal:  Bool = brType === BrType.Jal
  def isJalr: Bool = brType === BrType.Jalr
  def notCFI: Bool = brType === BrType.NotCfi
}

// pc = ftq.startAddr + Cat(offset, 0.U(1.W)) - Cat(borrow, 0.U(1.W))
class FtqPcOffset(implicit p: Parameters) extends FrontendBundle {
  val borrow: Bool = Bool()
  val offset: UInt = UInt(FetchBlockInstOffsetWidth.W)
}

class InstrEndOffset(implicit p: Parameters) extends FrontendBundle {
  val taken:  Bool = Bool()
  val offset: UInt = UInt(FetchBlockInstOffsetWidth.W)
}

class FetchToIBuffer(implicit p: Parameters) extends FrontendBundle {
  val instrs:         Vec[UInt]           = Vec(IBufferEnqueueWidth, UInt(32.W))
  val valid:          UInt                = UInt(IBufferEnqueueWidth.W)
  val enqEnable:      UInt                = UInt(IBufferEnqueueWidth.W)
  val pd:             Vec[PreDecodeInfo]  = Vec(IBufferEnqueueWidth, new PreDecodeInfo)
  val foldpc:         Vec[UInt]           = Vec(IBufferEnqueueWidth, UInt(MemPredPCWidth.W))
  val instrEndOffset: Vec[InstrEndOffset] = Vec(IBufferEnqueueWidth, new InstrEndOffset)
  // val ftqPcOffset:      Vec[Valid[FtqPcOffset]] = Vec(IBufferEnqueueWidth, Valid(new FtqPcOffset))
  val backendException: Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())
  val exceptionType:    Vec[ExceptionType] = Vec(IBufferEnqueueWidth, new ExceptionType)
  val crossPageIPFFix:  Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())
  val illegalInstr:     Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())
  val triggered:        Vec[UInt]          = Vec(IBufferEnqueueWidth, TriggerAction())
  val isLastInFtqEntry: Vec[Bool]          = Vec(IBufferEnqueueWidth, Bool())

  val pc:             Vec[PrunedAddr]       = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
  val prevIBufEnqPtr: IBufPtr               = new IBufPtr
  val debug_seqNum:   Vec[UInt]             = Vec(IBufferEnqueueWidth, InstSeqNum())
  val ftqPtr:         FtqPtr                = new FtqPtr
  val topdownInfo:    FrontendTopDownBundle = new FrontendTopDownBundle
}

class IfuToBackendIO(implicit p: Parameters) extends FrontendBundle {
  // write to backend gpaddr mem
  class ToGpAddrMem extends Bundle {
    val wen:   Bool        = Bool()
    val waddr: UInt        = UInt(log2Ceil(FtqSize).W)
    val wdata: GPAMemEntry = new GPAMemEntry
  }
  val gpAddrMem: ToGpAddrMem = new ToGpAddrMem
}
