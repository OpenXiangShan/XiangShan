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
import utility.InstSeqNum
import utils.EnumUInt
import xiangshan.Redirect
import xiangshan.RedirectLevel
import xiangshan.TopDownCounters
import xiangshan.TriggerAction
import xiangshan.backend.GPAMemEntry
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.TlbResp
import xiangshan.frontend.bpu.BpuCommit
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPerfMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo
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

  // perfMeta uses the same valid signal as meta
  val perfMeta:       BpuPerfMeta           = Output(new BpuPerfMeta)
  val topdownReasons: FrontendTopDownBundle = Output(new FrontendTopDownBundle())
}

class FtqToBpuIO(implicit p: Parameters) extends FrontendBundle {
  val redirect:        Valid[BpuRedirect]    = Valid(new BpuRedirect)
  val train:           DecoupledIO[BpuTrain] = Decoupled(new BpuTrain)
  val commit:          Valid[BpuCommit]      = Valid(new BpuCommit)
  val bpuPtr:          FtqPtr                = Output(new FtqPtr)
  val redirectFromIFU: Bool                  = Output(Bool())
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

class FrontendRedirect(implicit p: Parameters) extends FrontendBundle {
  val ftqIdx: FtqPtr = new FtqPtr
  val pc:     UInt   = UInt(VAddrBits.W)
  val taken:  Bool   = Bool()
  // The early end position may not always be a branch instruction.
  val ftqOffset: UInt            = UInt(FetchBlockInstOffsetWidth.W) // maybe use later
  val isRVC:     Bool            = Bool()                            // seems unused for now, keep it.
  val attribute: BranchAttribute = new BranchAttribute
  val target:    UInt            = UInt(VAddrBits.W)
}

class IfuToFtqIO(implicit p: Parameters) extends FrontendBundle {
  val mmioCommitRead: MmioCommitRead          = new MmioCommitRead
  val wbRedirect:     Valid[FrontendRedirect] = Valid(new FrontendRedirect)
}

class MmioCommitRead(implicit p: Parameters) extends FrontendBundle {
  val valid:          Bool   = Output(Bool())
  val mmioFtqPtr:     FtqPtr = Output(new FtqPtr)
  val mmioLastCommit: Bool   = Input(Bool())
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

  def apply(
      hasPf:     Bool,
      hasGpf:    Bool,
      hasAf:     Bool,
      hasIll:    Bool,
      hasHwe:    Bool,
      canAssert: Bool = true.B // prevent assert(x)
  ): ExceptionType = {
    assert(
      !canAssert || PopCount(VecInit(hasPf, hasGpf, hasAf, hasIll, hasHwe)) <= 1.U,
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
  def hasAf(hasAf: Bool, canAssert: Bool = true.B): ExceptionType =
    apply(false.B, false.B, hasAf, false.B, false.B, canAssert)

  // raise pf/gpf/af according to backend redirect request (tlb pre-check)
  def fromBackend(redirect: Redirect, canAssert: Bool = true.B): ExceptionType =
    apply(
      redirect.backendIPF,
      redirect.backendIGPF,
      redirect.backendIAF,
      false.B,
      false.B,
      canAssert
    )

  // raise pf/gpf/af according to itlb response
  def fromTlbResp(resp: TlbResp, canAssert: Bool = true.B, useDup: Int = 0): ExceptionType = {
    require(useDup >= 0 && useDup < resp.excp.length)
    // itlb is guaranteed to respond at most one exception
    apply(
      resp.excp(useDup).pf.instr,
      resp.excp(useDup).gpf.instr,
      resp.excp(useDup).af.instr,
      false.B,
      false.B,
      canAssert
    )
  }

  // raise af if pmp check failed
  def fromPmpResp(resp: PMPRespBundle, canAssert: Bool = true.B): ExceptionType =
    hasAf(resp.instr, canAssert)

  // raise af / hwe according to tilelink response
  def fromTileLink(corrupt: Bool, denied: Bool, canAssert: Bool = true.B): ExceptionType = {
    //  corrupt && denied -> access fault
    //  corrupt && !denied -> hardware error
    //  !corrupt && !denied -> no exception
    //  !corrupt && denied -> violates tilelink specification, should not happen
    assert(!canAssert || !(denied && !corrupt), "TileLink response should not be denied but !corrupt")
    apply(false.B, false.B, denied, false.B, corrupt && !denied, canAssert)
  }

  // raise ill according to rvc expander
  def fromRvcExpander(ill: Bool, canAssert: Bool = true.B): ExceptionType =
    apply(false.B, false.B, false.B, ill, false.B, canAssert)
}

class PreDecodeInfo extends Bundle { // 8 bit
  val valid:       Bool            = Bool()
  val isRVC:       Bool            = Bool()
  val brAttribute: BranchAttribute = new BranchAttribute
  // val excType = UInt(3.W)
  def isBr:   Bool = brAttribute.isConditional
  def isJal:  Bool = brAttribute.isDirect
  def isJalr: Bool = brAttribute.isIndirect
  def notCFI: Bool = brAttribute.isNone
}

// pc = ftq.startAddr + Cat(offset, 0.U(1.W)) - Cat(borrow, 0.U(1.W))
class FtqPcOffset(implicit p: Parameters) extends FrontendBundle {
  val borrow: Bool = Bool()
  val offset: UInt = UInt(FetchBlockInstOffsetWidth.W)
}

class InstrEndOffset(implicit p: Parameters) extends FrontendBundle {
  val predTaken:  Bool = Bool()
  val fixedTaken: Bool = Bool()
  val offset:     UInt = UInt(FetchBlockInstOffsetWidth.W)
}

class FetchToIBuffer(implicit p: Parameters) extends FrontendBundle {
  val instrs:         Vec[UInt]           = Vec(IBufferEnqueueWidth, UInt(32.W))
  val valid:          UInt                = UInt(IBufferEnqueueWidth.W)
  val enqEnable:      UInt                = UInt(IBufferEnqueueWidth.W)
  val isRvc:          Vec[Bool]           = Vec(IBufferEnqueueWidth, Bool())
  val foldpc:         Vec[UInt]           = Vec(IBufferEnqueueWidth, UInt(MemPredPCWidth.W))
  val instrEndOffset: Vec[InstrEndOffset] = Vec(IBufferEnqueueWidth, new InstrEndOffset)

  val exceptionType:      ExceptionType = new ExceptionType
  val isBackendException: Bool          = Bool()
  val exceptionCrossPage: Bool          = Bool()
  val exceptionOffset:    UInt          = UInt(log2Ceil(IBufferEnqueueWidth).W)

  val triggered:        Vec[UInt] = Vec(IBufferEnqueueWidth, TriggerAction())
  val isLastInFtqEntry: Vec[Bool] = Vec(IBufferEnqueueWidth, Bool())

  val pc:             Vec[PrunedAddr]       = Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits))
  val prevIBufEnqPtr: IBufPtr               = new IBufPtr
  val prevInstrCount: UInt                  = UInt(log2Ceil(IBufferEnqueueWidth).W)
  val debug_seqNum:   Vec[InstSeqNum]       = Vec(IBufferEnqueueWidth, InstSeqNum())
  val ftqPtr:         Vec[FtqPtr]           = Vec(IBufferEnqueueWidth, new FtqPtr)
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

object BlameBpuSource {
  object BlameType extends EnumUInt(5) {
    def BTB:    UInt = 0.U(width.W)
    def TAGE:   UInt = 1.U(width.W)
    def RAS:    UInt = 2.U(width.W)
    def ITTAGE: UInt = 3.U(width.W)
    def SC:     UInt = 4.U(width.W)
  }

  def apply(perf: BpuPerfMeta, branch: BranchInfo): UInt = {
    import BlameType.{BTB, TAGE, RAS, ITTAGE, SC}
    val src  = perf.bpSource
    val pred = perf.bpPred
    val attr = branch.attribute

    // Check mispredict type
    val onlyDirectionWrong = branch.taken =/= pred.taken && branch.cfiPosition === pred.cfiPosition
    val blame              = WireInit(BTB) // Default to BTB

    when(src.s3Ras) {
      when(attr.isConditional) {
        // If cond before, TAGE mispredicts
        // If cond after, should trigger assertion, TODO
        blame := TAGE
      }.elsewhen(attr.isReturn) {
        blame := RAS
      }.otherwise {
        // Other branch type mismatch
        blame := BTB
      }
    }.elsewhen(src.s3ITTage) {
      when(attr.isIndirect) {
        blame := ITTAGE
      }.elsewhen(attr.isConditional) {
        blame := TAGE
        // If cond after, should trigger assertion, TODO
      }.otherwise {
        blame := BTB
      }
    }.elsewhen(src.s3MbtbSc) {
      when(attr.isConditional) {
        blame := Mux(onlyDirectionWrong, SC, BTB)
      }.otherwise {
        blame := BTB
      }
    }.elsewhen(src.s3MbtbTage || src.s3FallthroughTage) {
      when(attr.isConditional) {
        blame := Mux(onlyDirectionWrong, TAGE, BTB)
      }.otherwise {
        blame := BTB
      }
    }.elsewhen(src.s3Mbtb) {
      when(attr.isConditional) {
        blame := Mux(onlyDirectionWrong, TAGE, BTB)
      }.elsewhen(attr.isIndirect) {
        blame := ITTAGE
      }.otherwise {
        blame := BTB
      }
    }.elsewhen(src.s3Fallthrough) {
      blame := BTB
    }
    blame
  }
}

class BpuPerfInfo(implicit p: Parameters) extends FrontendBundle {
  val bpRight: UInt = UInt(XLEN.W)
  val bpWrong: UInt = UInt(XLEN.W)
}

class BpuTopDownInfo(implicit p: Parameters) extends FrontendBundle {
  val btbMissBubble:    Bool = Bool()
  val tageMissBubble:   Bool = Bool()
  val scMissBubble:     Bool = Bool()
  val ittageMissBubble: Bool = Bool()
  val rasMissBubble:    Bool = Bool()
}

class FrontendPerfInfo(implicit p: Parameters) extends FrontendBundle {
  val ibufFull: Bool        = Bool()
  val bpuInfo:  BpuPerfInfo = new BpuPerfInfo
}

class FrontendDebugTopDownInfo(implicit p: Parameters) extends FrontendBundle {
  val robHeadVaddr: Valid[PrunedAddr] = Valid(PrunedAddr(VAddrBits))
}
