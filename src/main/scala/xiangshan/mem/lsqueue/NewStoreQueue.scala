/***************************************************************************************
 * Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.mem

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.DifftestMem
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility._
import xiangshan.ExceptionNO.hardwareError
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, ExuOutput, UopIdx, connectSamePort}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuConfig.StaCfg
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.{DCacheWordReqWithVaddrAndPfFlag, MemoryOpConstants, UncacheWordIO}
import xiangshan.backend.fu.FuType
import xiangshan.mem.Bundles.SQForward

class SqPtr(implicit p: Parameters) extends CircularQueuePtr[SqPtr](
  p => p(XSCoreParamsKey).StoreQueueSize
){
}

object SqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): SqPtr = {
    val ptr = Wire(new SqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

// Unalign queue Ptr
class UqPtr(implicit p: Parameters) extends CircularQueuePtr[UqPtr](
  p => p(XSCoreParamsKey).SQUnalignQueueSize
){
}

object UqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): UqPtr = {
    val ptr = Wire(new UqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class DataQueuePtr(implicit p: Parameters) extends CircularQueuePtr[DataQueuePtr](
  p => p(XSCoreParamsKey).EnsbufferWidth
){
}

object DataQueuePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): DataQueuePtr = {
    val ptr = Wire(new DataQueuePtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}


// don't need to initial
class SQDataEntryBundle(implicit p: Parameters) extends MemBlockBundle {
  class UopInfo(implicit p: Parameters) extends MemBlockBundle {
    // load inst will not be executed until former store (predicted by mdp) addr calcuated
    val loadWaitBit      = Bool()
    // If (loadWaitBit && loadWaitStrict), strict load wait is needed
    // load inst will not be executed until ALL former store addr calcuated
    val loadWaitStrict   = Bool()
    val ssid             = UInt(SSIDWidth.W)
    val storeSetHit      = Bool() // inst has been allocated an store set

    val robIdx           = new RobPtr
    val uopIdx           = UopIdx()

  }
  val uop                = new UopInfo
  val size               = UInt(MemorySize.Size.width.W)
  // data storage
  val vaddr                    = UInt(VAddrBits.W)
  val paddrHigh                = UInt((PAddrBits - pageOffset).W) //don't need to storage low 12 bit, which is same as vaddr(11, 0)
  val byteMask                 = UInt((VLEN/8).W)
  def paddr :UInt              = Cat(paddrHigh, vaddr(pageOffset - 1, 0))
  val data                     = UInt(VLEN.W)

  def byteStart: UInt          = vaddr(log2Ceil(VLEN/8) - 1, 0)
  def byteEnd: UInt            = byteStart + MemorySize.ByteOffset(size)

  val memoryType               = MemoryType()
  val cboType                  = CboType()
  val prefetch                 = Bool() //TODO: need it ?

  // debug signal
  val debugPaddr               = Option.when(debugEn)(UInt((PAddrBits).W))
  val debugVaddr               = Option.when(debugEn)(UInt((VAddrBits).W))
  val debugData                = Option.when(debugEn)(UInt((XLEN).W))
  // only use for unit-stride difftest
  val debugVecUnalignedStart   = Option.when(debugEn)(UInt((log2Up(XLEN)).W))
  val debugVecUnalignedOffset  = Option.when(debugEn)(UInt((log2Up(XLEN)).W))
  val debugUop                 = Option.when(debugEn)(new DynInst())

}

// need initial when reset
class SQCtrlEntryBundle(implicit p: Parameters) extends MemBlockBundle {
  val dataValid          = Bool()
  val addrValid          = Bool()

  val waitStoreS2        = Bool() //TODO: will be remove in the feature
  val isVec              = Bool() // TODO: need it ?
  // vecInactive indicate storage a inactive vector element, it will not write to Sbuffer. written when vector split.
  val vecInactive        = Bool()
  val cross16Byte        = Bool()
  val hasException       = Bool()
  val committed          = Bool()
  val allocated          = Bool()
  val handleFinish       = Bool() // this signal is for deqPtr move, true.B indicate NC/MMIO/cbo request can deq

  val isCbo              = Bool() // Indicate if is cbo request, true is cbo request
  val vecMbCommit        = Bool() //TODO: request was committed by MergeBuffer, will be remove in the future.

  //debug information
  val unalignWithin16Byte = Option.when(debugEn)(Bool())

  def allValid: Bool     = dataValid && addrValid
}

class UnalignBufferEntry(implicit p: Parameters) extends MemBlockBundle {
  val paddrHigh          = UInt((PAddrBits - pageOffset).W)
  def paddr :UInt        = Cat(paddrHigh, 0.U(pageOffset.W))
  val robIdx             = new RobPtr
  val sqIdx              = new SqPtr
}

class WriteToSbufferReqEntry(implicit p: Parameters) extends MemBlockBundle {
  val addr         = UInt(PAddrBits.W)
  val prefetch     = Bool()
  val vecValid     = Bool() //TODO: need to remove.
  val wline        = Bool()
  val vaddr        = UInt(VAddrBits.W)
  val data         = UInt(VLEN.W)
  val mask         = UInt((VLEN/8).W)
}

abstract class NewStoreQueueBase(implicit p: Parameters) extends LSQModule {

  def isMmio(in: UInt): Bool = {
    require(in.getWidth == MemoryType.width)
    MemoryType.isMMIO(in)
  }
  def isPbmtIO(in: UInt): Bool = {
    require(in.getWidth == MemoryType.width)
    MemoryType.isPbmtIO(in)
  }
  // is pbmt nc
  def isPbmtNC(in: UInt): Bool = {
    require(in.getWidth == MemoryType.width)
    MemoryType.isPbmtNC(in)
  }
  // is cacheable
  def isCacheable(in: UInt): Bool = {
    require(in.getWidth == MemoryType.width)
    MemoryType.isCacheable(in)
  }
  // is cbo zero
  def isCboZero(in: UInt): Bool = {
    require(in.getWidth == CboType.width)
    CboType.isCboZero(in)
  }

  def isCboClean(in: UInt): Bool = {
    require(in.getWidth == CboType.width)
    CboType.isCboClean(in)
  }

  def isCboFlush(in: UInt): Bool = {
    require(in.getWidth == CboType.width)
    CboType.isCboFlush(in)
  }

  def isCboInval(in: UInt): Bool = {
    require(in.getWidth == CboType.width)
    CboType.isCboInval(in)
  }

  /**
   * Circular Right Shift [step] byte
   * */
  def rotateByteRight(in: UInt, step: Int): UInt = {
    val maxLen = in.getWidth
    if(step == 0) in
    else Cat(in(step - 1, 0), in(maxLen - 1, step))
  }

  val param = staParams.head
  param.bindBackendParam(backendParams)

//  val DCacheVWordBytes  = VLEN / 8
//  val DCacheVWordOffset = log2Up(DCacheVWordBytes)
  val DCacheLineBytes   = CacheLineSize / 8
  val DCacheLineVWords  = DCacheLineBytes / DCacheVWordBytes
  val DCacheLineVWordsOffset = log2Up(DCacheLineVWords)
  val VWordOffset            = log2Up(VLENB)

  private class ForwardModule(val param: ExeUnitParams)(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val query           = Flipped(Vec(LoadPipelineWidth, new SQForward))
      val dataEntriesIn   = Vec(StoreQueueSize, Input(new SQDataEntryBundle())) // from storeQueue data
      val ctrlEntriesIn   = Vec(StoreQueueSize, Input(new SQCtrlEntryBundle())) // from storeQueue ctrl info
      val ctrlInfo = new Bundle {
        val deqPtr = Input(new SqPtr())
        val enqPtr = Input(new SqPtr())
      }
    })

    /**
     * @param in The select vector
     * @return (result, multiHit)
     *
     *         result: The one-hot vec of first true.
     *
     *         multiHit: select vector is not one-hot.
     * @example
     *         in: b00010100
     *         -> lowHasOne: b11111000 => (result, multiHit): (b00000100, true.B)
     *
     *         in: b00000010
     *         -> lowHasOne: b11111100 => (result, multiHit): (b00000010, false.B)
     * */
    def findYoungest(in: UInt): (UInt, Bool) = {
      val lowHasOne = VecInit(Seq.fill(in.getWidth)(false.B))
      for (i <- 1 until in.getWidth) {
        lowHasOne(i) := lowHasOne(i - 1) | in(i - 1)
      }
      // (one-hot result, has multi match)
      (in & (~lowHasOne.asUInt).asUInt, (in & lowHasOne.asUInt).orR)
    }

    /**
     * [Load Forward Query]
     *
     * Checks Store Queue for older stores that can forward data to the load.
     * Response becomes valid 2 cycles after request.
     *
     * Pipeline Overview:
     *   Stage 0: Prepare masks and address ranges
     *   Stage 1: Match stores and select youngest valid candidate
     *   Stage 2: Generate forwarded data and mask
     *
     * +----------+     +----------+     +----------+
     * | Stage 0  | --> | Stage 1  | --> | Stage 2  |
     * | (Cycle 0)|     | (Cycle 1)|     | (Cycle 2)|
     * +----------+     +----------+     +----------+
     */
    for (i <- 0 until LoadPipelineWidth) {
      // Stage breakdown:
      //   Stage 0:
      //     1. Generate load sqIdx mask
      //     2. Calculate byte start/end for load
      //   Stage 1:
      //     1. Match physical/virtual addresses
      //     2. Check byte range overlap
      //     3. Select youngest matching store
      //   Stage 2:
      //     1. Extract correct bytes from store data
      //     2. Generate final forwarded data and mask

      val addrValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j => io.ctrlEntriesIn(j).addrValid)))
      // if is cbo zero, it can forward; other cbo type's data is invalid.
      val dataValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j =>
        io.ctrlEntriesIn(j).dataValid && (isCboZero(io.dataEntriesIn(j).cboType) || !io.ctrlEntriesIn(j).isCbo))))
      val allValidVec  = WireInit(VecInit((0 until StoreQueueSize).map(j =>
        io.ctrlEntriesIn(j).allValid)))

      /*================================================== Stage 0 ===================================================*/
      // Circular Queue Handling:
      //   Store Queue is circular (like a ring buffer). When deqPtr and sqIdx wrap around,
      //   we need to check two segments:
      //
      //   Case 1: same flag (no wrap)
      //              sqIdx            deqPtr
      //               |                |
      //               v                v
      //     +-----+-----+-----+-----+-----+-----+-----+-----+
      //     |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
      //     +-----+-----+-----+-----+-----+-----+-----+-----+
      //               ^^^^^^^^^^^^^^^^
      //               deqPtr -> sqIdx (one segment)
      //
      //   Case 2: different flags (wrap around)
      //             deqPtr           sqIdx
      //               |                |
      //               v                v
      //     +-----+-----+-----+-----+-----+-----+-----+-----+
      //     |  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  |
      //     +-----+-----+-----+-----+-----+-----+-----+-----+
      //     ^^^^^^^^^                   ^^^^^^^^^^^^^^^^^^^
      //     end <- deqPtr        +      sqIdx <- 0
      //
      //   Implementation:
      //     ageMaskLow  = deqMask & forwardMask & differentFlag
      //     ageMaskHigh = ~deqMask & (differentFlag | forwardMask)
      //
      // Example: SQ size=8, deqPtr=6 (flag=0), sqIdx=3 (flag=1)
      //   differentFlag = true
      //   ageMaskLow  = 0b00001111 (bits 0-3)
      //   ageMaskHigh = 0b11000000 (bits 6-7)

      val s0Req              = io.query(i).s0Req
      val s0Valid            = s0Req.valid
      val s0DeqMask          = UIntToMask(io.ctrlInfo.deqPtr.value, StoreQueueSize)
      val s0DifferentFlag    = io.ctrlInfo.deqPtr.flag =/= s0Req.bits.sqIdx.flag
      val s0ForwardMask      = UIntToMask(s0Req.bits.sqIdx.value, StoreQueueSize)
      // generate load byte start and end
      val s0LoadStart        = s0Req.bits.vaddr(VWordOffset - 1, 0)
      val s0ByteOffset       = MemorySize.ByteOffset(s0Req.bits.size)
      val s0LoadEnd          = s0LoadStart + s0ByteOffset

      // mdp mask
      val lfstEnable = Constantin.createRecord("LFSTEnable", LFSTEnable)
      val s0StoreSetHitVec = Mux(lfstEnable,
        WireInit(VecInit((0 until StoreQueueSize).map(j =>
          s0Req.bits.loadWaitBit && io.dataEntriesIn(j).uop.robIdx === s0Req.bits.waitForRobIdx))),
        WireInit(VecInit((0 until StoreQueueSize).map(j =>
          io.dataEntriesIn(j).uop.storeSetHit && io.dataEntriesIn(j).uop.ssid === s0Req.bits.ssid)))
      )

      val s0AgeMaskLow     = s0DeqMask & s0ForwardMask & VecInit(Seq.fill(StoreQueueSize)(s0DifferentFlag)).asUInt
      val s0AgeMaskHigh    = (~s0DeqMask).asUInt & (VecInit(Seq.fill(StoreQueueSize)(s0DifferentFlag)).asUInt | s0ForwardMask)

      val s1ForwardMask  = RegEnable(s0ForwardMask, s0Valid)
      val s1LoadVaddr    = RegEnable(s0Req.bits.vaddr(VAddrBits - 1, VWordOffset), s0Valid)
      val s1deqMask      = RegEnable(s0DeqMask, s0Valid)
      val s1LoadStart    = RegEnable(s0LoadStart, s0Valid)
      val s1LoadEnd      = RegEnable(s0LoadEnd, s0Valid)
      val s1StoreSetHitVec = RegEnable(s0StoreSetHitVec, s0Valid)

      val s1AgeMaskLow   = RegEnable(s0AgeMaskLow, s0Valid)
      val s1AgeMaskHigh  = RegEnable(s0AgeMaskHigh, s0Valid)
      val s1Kill         = io.query(i).s1Kill
      val s1Valid        = RegNext(s0Valid) && !s1Kill


      /*================================================== Stage 1 ===================================================*/
      // Matching Process:
      //
      //   Step 1: Virtual Address Match (high bits only)
      //     +-------+-----------------+--------+
      //     | Store | vaddr (high)    |  size  |
      //     +-------+-----------------+--------+
      //     |   0   | 0x100 (0x1000)  |   4B   |
      //     |   1   | 0x100 (0x1004)  |   2B   |
      //     |   2   | 0x200 (0x2000)  |   4B   |
      //     |   3   | 0x100 (0x1002)  |   4B   |
      //     +-------+-----------------+--------+
      //     Load vaddr = 0x1003 -> high=0x100 -> matches stores 0 and 3
      //
      //   Step 2: Byte Overlap Check
      //     Store 0: [0,3] vs Load [3,3] -> overlap (0<=3<=3)
      //     Store 3: [2,5] vs Load [3,3] -> overlap (2<=3<=5)
      //
      //   Step 3: Select Youngest Valid Store
      //     canForward = ageMask & overlap & vaddrMatch
      //     Example: canForward = 0b1001 (stores 0 and 3 match)
      //     findYoungest(Reverse(0b1001)) -> selects store 3 (index 3)

      val s1Req = io.query(i).s1Req
      val s1QueryPaddr = s1Req.paddr(PAddrBits - 1, VWordOffset)
      // prevent X-state
      // Virtual address match (high bits only, ignore byte offset)
      val s1VaddrMatchVec  = VecInit(io.dataEntriesIn.zip(io.ctrlEntriesIn).map { case (dataEntry, ctrlEntry) =>
        val storeIsCboZero = ctrlEntry.isCbo && isCboZero(dataEntry.cboType)
        val isCross16B     = ctrlEntry.cross16Byte
        // vaddr two part match:
        // [1]: not cross 16B: vaddr[VaddrBits - 1, log2Ceil(CacheLineSize / 8)] addr(maxLen -> cacheline) or
        //      cross 16B: vaddr[VaddrBits - 1, log2Ceil(CacheLineSize / 8)] + 1.U addr(maxLen -> cacheline) [next cacheline]
        // [2]: not cross 16B: vaddr[log2Ceil(CacheLineSize / 8) - 1, log2Ceil(VLENB)] or
        //      cross 16B: vaddr[log2Ceil(CacheLineSize / 8) - 1, log2Ceil(VLENB)] + 1.U [next 16B] or
        //      The bits within cacheline, if store is cboZero, it can be ignored.
        //
        ((dataEntry.vaddr(DCacheLineOffset - 1, VWordOffset) === s1LoadVaddr(DCacheLineOffset - VWordOffset - 1, 0) ||
          isCross16B && (dataEntry.vaddr(DCacheLineOffset - 1, VWordOffset) + 1.U) === s1LoadVaddr(DCacheLineOffset - VWordOffset - 1, 0) ||
          storeIsCboZero) &&
        (dataEntry.vaddr(VAddrBits - 1, DCacheLineOffset) === s1LoadVaddr(s1LoadVaddr.getWidth - 1, DCacheLineOffset - VWordOffset) ||
          isCross16B && (dataEntry.vaddr(VAddrBits - 1, DCacheLineOffset) + 1.U) === s1LoadVaddr(s1LoadVaddr.getWidth - 1, DCacheLineOffset - VWordOffset))) &&
        ctrlEntry.addrValid
      }).asUInt

      // Byte overlap check: store covers any part of load's range
      //   Example: store [2,5] and load [3,3] -> overlap (2<=3 && 5>=3)
      val s1OverlapMask  = VecInit((0 until StoreQueueSize).map(j =>
        io.dataEntriesIn(j).byteStart <= s1LoadEnd && io.dataEntriesIn(j).byteEnd >= s1LoadStart ||
        io.ctrlEntriesIn(j).cross16Byte && io.dataEntriesIn(j).byteEnd(VWordOffset - 1, 0) <= s1LoadEnd // next 16B, store start always 0.
      )).asUInt

      XSError((s1LoadEnd < s1LoadStart) && s1Valid, "ByteStart > ByteEnd!\n")

      // Two-step selection to handle circular queue segments
      val s1CanForwardLow = s1AgeMaskLow & s1OverlapMask & s1VaddrMatchVec
      val s1CanForwardHigh = s1AgeMaskHigh & s1OverlapMask & s1VaddrMatchVec

      // find youngest entry, which is one-hot
      // Find youngest store (highest index = most recent)
      //   Reverse vector so we can find leftmost 1 (highest index)
      val (s1SelectLowOH, _)             = findYoungest(Reverse(s1CanForwardLow))
      val (s1ForwardHighOH, _)           = findYoungest(Reverse(s1CanForwardHigh))
      val s1SelectHighOH                 = s1ForwardHighOH & VecInit(Seq.fill(StoreQueueSize)(!s1CanForwardLow.orR)).asUInt
      val s1SelectOH                     = Reverse(s1SelectLowOH | s1SelectHighOH) // index higher, mean it younger
      val s1SelectDataEntry              = Mux1H(s1SelectOH, io.dataEntriesIn)
      val s1SelectCtrlEntry              = Mux1H(s1SelectOH, io.ctrlEntriesIn)
      val s1DataInvalid                  = !(s1SelectOH & dataValidVec.asUInt).orR
      val (_, s1MultiMatch)              = findYoungest(s1CanForwardLow | s1CanForwardHigh) // don't care

      // select offset generate
      val s1ByteSelectOffset   = s1LoadStart - s1SelectDataEntry.byteStart

      val s1HasAddrInvalid = ((s1AgeMaskLow | s1AgeMaskHigh) & VecInit(addrValidVec.map(!_)).asUInt & s1StoreSetHitVec.asUInt).orR

      // find youngest addrInvalid store
      val s1AddrInvalidLow     = s1AgeMaskLow & VecInit(addrValidVec.map(!_)).asUInt & s1StoreSetHitVec.asUInt
      val s1AddrInvalidHigh    = s1AgeMaskHigh & VecInit(addrValidVec.map(!_)).asUInt & s1StoreSetHitVec.asUInt &
        VecInit(Seq.fill(StoreQueueSize)(!s1AddrInvalidLow.orR)).asUInt

      val (s1AddrInvLowOH, _)   = findYoungest(Reverse(s1AddrInvalidLow))
      val (s1AddrInvHighOH, _)  = findYoungest(Reverse(s1AddrInvalidHigh))
      val s1AddrInvSelectOH     = Reverse(s1AddrInvLowOH | s1AddrInvHighOH)

      val s1DataInvalidSqIdx   = Wire(new SqPtr)
      val s1AddrInvalidSqIdx   = Wire(new SqPtr)

      s1DataInvalidSqIdx.value := OHToUInt(s1SelectOH)
      s1DataInvalidSqIdx.flag  := Mux(s1SelectLowOH.orR, io.ctrlInfo.enqPtr.flag, io.ctrlInfo.deqPtr.flag)

      s1AddrInvalidSqIdx.value := OHToUInt(s1AddrInvSelectOH)
      s1AddrInvalidSqIdx.flag  := Mux(s1AddrInvLowOH.orR, io.ctrlInfo.enqPtr.flag, io.ctrlInfo.deqPtr.flag)

      val s2ByteSelectOffset = RegEnable(s1ByteSelectOffset, s1Valid)
      val s2SelectDataEntry  = RegEnable(s1SelectDataEntry, s1Valid)
      val s2SelectCtrlEntry  = RegEnable(s1SelectCtrlEntry, s1Valid)
      val s2DataInValid      = RegEnable(s1DataInvalid, s1Valid)
      val s2HasAddrInvalid   = RegEnable(s1HasAddrInvalid, s1Valid)
      val s2CanForward       = RegEnable((s1AgeMaskLow | s1AgeMaskHigh) & s1OverlapMask, s1Valid)
      val s2SelectOH         = RegEnable(s1SelectOH, s1Valid)
      val s2LoadMaskEnd      = RegEnable(UIntToMask(MemorySize.CaculateSelectMask(s1LoadStart, s1LoadEnd), VLENB), s1Valid)
      val s2DataInvalidSqIdx = RegEnable(s1DataInvalidSqIdx, s1Valid)
      val s2AddrInvalidSqIdx = RegEnable(s1AddrInvalidSqIdx, s1Valid)
      val s2MultiMatch       = RegEnable(s1MultiMatch, s1Valid)
      val s2LoadPaddr        = RegEnable(s1QueryPaddr, s1Valid)
      val s2LoadStart        = RegEnable(s1LoadStart, s1Valid)
      val s2ForwardValid     = RegEnable(s1SelectOH.orR, s1Valid) // indicate whether forward is valid.
      val s2Valid            = RegNext(s1Valid)
      // debug
      XSError(s1SelectOH.orR && !s1SelectCtrlEntry.allocated && s1Valid, "forward select a invalid entry!\n")
      /*================================================== Stage 2 ===================================================*/

      // Data Generation Process:
      //     Original Store Data (byteStart=1, size=4B):
      //     +--------+--------+--------+--------+
      //     | 0x88   | 0x77   | 0x66   | 0x55   |  <- Memory (LE)
      //     +--------+--------+--------+--------+
      //       0x1004   0x1003   0x1002   0x1001
      //                                  ^^^^^^
      //                                    Store starts here
      //
      //
      //   Load at s2ByteSelectOffset=2 (loadStart=3, loadSize=1B):
      //     +--------+--------+--------+--------+
      //     | 0x66   | 0x55   | 0x88   | 0x77   |  <- rotateByteRight && ParallelLookUp
      //     +--------+--------+--------+--------+
      //                                  ^^^^
      //                                  Load needs this byte (0x77)

      // !Paddrmatch
      val s2PaddrMatchVec       = VecInit(io.dataEntriesIn.zip(io.ctrlEntriesIn).map { case (dataEntry, ctrlEntry) =>
        val storeIsCboZero      = ctrlEntry.isCbo && isCboZero(dataEntry.cboType)
        val isCross16B          = ctrlEntry.cross16Byte

        (dataEntry.paddr(DCacheLineOffset - 1, VWordOffset) === s2LoadPaddr(DCacheLineOffset - VWordOffset - 1, 0) ||
          isCross16B && (dataEntry.paddr(DCacheLineOffset - 1, VWordOffset) + 1.U) === s2LoadPaddr(DCacheLineOffset - VWordOffset - 1, 0) || // next 16B
          storeIsCboZero) &&
        (dataEntry.paddr(pageOffset - 1, DCacheLineOffset) === s2LoadPaddr(pageOffset - VWordOffset - 1, DCacheLineOffset - VWordOffset) ||
          isCross16B && (dataEntry.paddr(pageOffset - 1, DCacheLineOffset) + 1.U) === s2LoadPaddr(pageOffset - VWordOffset - 1, DCacheLineOffset - VWordOffset)) && // next Cacheline
        dataEntry.paddr(PAddrBits - 1, pageOffset) === s2LoadPaddr(s2LoadPaddr.getWidth - 1, pageOffset - VWordOffset) &&
        ctrlEntry.addrValid
      }).asUInt

      val s2PaddrNoMatch       = (
        ((s2PaddrMatchVec ^ s2SelectOH) & s2CanForward).orR
        )

      val s2SelectData         = (0 until VLENB).map(j =>
        j.U -> rotateByteRight(s2SelectDataEntry.data, j * 8)
      )
      val s2OutData            = ParallelLookUp(s2ByteSelectOffset, s2SelectData)

      val s2SelectMask         = (0 until VLENB).map(j =>
        j.U -> rotateByteRight(s2SelectDataEntry.byteMask, j)
      )
      val s2OutMask            = ParallelLookUp(s2ByteSelectOffset, s2SelectMask) & s2LoadMaskEnd

      val s2FullOverlap        = (s2SelectDataEntry.byteMask & s2LoadMaskEnd) === s2LoadMaskEnd
      // First condition: access extends beyond the lower log2Ceil(VLEN/8) bits.
      // Second condition: higher bits of the virtual address within the page offset are non-zero, indicating a potential cross-page access.
      val s2Cross4KPage        = s2SelectDataEntry.byteEnd(VWordOffset) && s2SelectDataEntry.vaddr(pageOffset - 1, VWordOffset).andR && s2ForwardValid
      val s2SafeForward        = !s2MultiMatch || s2FullOverlap

      //TODO: only use for 128-bit align forward, should revert when other forward source support rotate forward !!!!
      val s2FinalData          = s2OutData << (s2LoadStart * 8.U)
      val s2FinalMask          = s2OutMask << s2LoadStart

      val s1Resp = io.query(i).s1Resp
      val s2Resp = io.query(i).s2Resp
      s1Resp.valid := false.B //TODO: need it?
      s1Resp.bits := DontCare
//      s2Resp.bits.forwardData.zipWithIndex.map{case (sink, j) =>
//        sink := outData((j + 1) * 8 - 1, j * 8)}
//      s2Resp.bits.forwardMask.zipWithIndex.map{case (sink, j) =>
//        sink := outMask(j) && s2Valid} // TODO: FIX ME, when Resp.valid is false, do not use ByteMask!!
      s2Resp.bits.forwardData.zipWithIndex.map{case (sink, j) =>
        sink := s2FinalData((j + 1) * 8 - 1, j * 8)}
      s2Resp.bits.forwardMask.zipWithIndex.map{case (sink, j) =>
        sink := s2FinalMask(j) && s2ForwardValid} // TODO: FIX ME, when Resp.valid is false, do not use ByteMask!!
      s2Resp.bits.dataInvalid.valid := s2DataInValid && s2ForwardValid // select is valid
      s2Resp.bits.dataInvalid.bits := s2DataInvalidSqIdx
      s2Resp.bits.addrInvalid.valid := s2HasAddrInvalid // maby can't select a entry
      s2Resp.bits.addrInvalid.bits := s2AddrInvalidSqIdx
      s2Resp.bits.forwardInvalid   := !s2SafeForward || s2Cross4KPage // do not support cross page forward.
      s2Resp.bits.matchInvalid     := s2PaddrNoMatch && !s2Cross4KPage && s2SafeForward // if cross Page/multi match, let load replay.
      s2Resp.valid                 := s2Valid

      if(debugEn) {
        dontTouch(s1OverlapMask)
        dontTouch(s1AgeMaskLow)
        dontTouch(s1AgeMaskHigh)
        dontTouch(s1CanForwardLow)
        dontTouch(s1CanForwardHigh)
        dontTouch(s1MultiMatch)
        dontTouch(s1AddrInvLowOH)
        dontTouch(s1AddrInvHighOH)
        dontTouch(s1SelectOH)
        dontTouch(s1AddrInvSelectOH)
        dontTouch(s2OutMask)
        dontTouch(s2OutData)
        dontTouch(s2SafeForward)
        dontTouch(s2PaddrMatchVec)
        dontTouch(s2CanForward)
      }
    }
  }

  /*
  * EnterSbufferQueue is a sequentially written data buffer for eliminating timing paths between the Sbuffer and StoreQueue.
  *
  * [NOTES]: Ideally, the n data at the StoreQueue head can be written into the Sbuffer, EnterSbufferQueue is a pipeline.
  *          However, when the sbuffer becomes unable to write the n data in a single cycle,
  *          the EnterSbufferQueue ensures that the n data are written into the Sbuffer in the correct order
  *          while they are in EnterSbufferQueue.
  *
  * The structure of StoreQueue write to Sbuffer are as shown below:
  *     +------------+                        +-------------------+
  *     | StoreQueue |                        |                   |
  *     +------------+                        |                   |
  *     |      .     |                        | EnterSbufferQueue |
  *     |      .     |                        |                   |
  *     |      .     |                        |                   |
  *     +------------+  [n = EnsbufferWidth]  +-------------------+               +-------------------+
  *     |   head n   | ---------------------->|      Entry n      | ------------> |                   |
  *     +------------+                        +-------------------+               |                   |
  *     |      .     |                        |         .         |               |                   |
  *     |      .     |                        |         .         |               |       Sbuffer     |
  *     |      .     |                        |         .         |               |                   |
  *     +------------+                        +-------------------+               |                   |
  *     |   head 0   |----------------------> |      Entry 0      | ------------> |                   |
  *     +------------+                        +-------------------+               +-------------------+
  * */
  private class EnterSbufferQueue(val param: ExeUnitParams)(implicit  p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val fromDeqModule = Vec(EnsbufferWidth, Flipped(DecoupledIO(new WriteToSbufferReqEntry)))
      val toSbuffer     = new SbufferWriteIO
      val empty         = Output(Bool())
      val full          = Output(Bool())
      val freeCount     = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    })
    def ToSbufferConnect(source: WriteToSbufferReqEntry, sink: DCacheWordReqWithVaddrAndPfFlag) = {
      sink          := WireInit(0.U.asTypeOf(new DCacheWordReqWithVaddrAndPfFlag)) // TODO: init here.
      sink.data     := source.data
      sink.mask     := source.mask
      sink.vaddr    := source.vaddr
      sink.wline    := source.wline
      sink.addr     := source.addr
      sink.vecValid := source.vecValid
      sink.prefetch := source.prefetch
      sink
    }

    private val enqWidth: Int  = io.fromDeqModule.length
    private val queueSize: Int = EnsbufferWidth

    private val entries    = Reg(Vec(queueSize, new WriteToSbufferReqEntry)) // no need to reset!
    private val allocated  = RegInit(VecInit(Seq.fill(queueSize)(false.B)))
    private val enqPtrVec  = RegInit(VecInit((0 until io.fromDeqModule.length).map(_.U.asTypeOf(new DataQueuePtr))))
    private val deqPtrVec  = RegInit(VecInit((0 until io.fromDeqModule.length).map(_.U.asTypeOf(new DataQueuePtr))))
    private val headEntry  = entries(deqPtrVec.head.value)

    private val empty      = enqPtrVec.head.value === deqPtrVec.head.value && enqPtrVec.head.flag === deqPtrVec.head.flag
    private val full       = enqPtrVec.head.value === deqPtrVec.head.value && enqPtrVec.head.flag =/= deqPtrVec.head.flag

    // enq
    private val canEnq    = io.fromDeqModule.map(_.fire)
    private val enqReq    = io.fromDeqModule.map(_.bits)

    enqPtrVec.zip(canEnq).zipWithIndex.map{case ((ptr, v), i) =>
      when(v) {
        entries(ptr.value) := enqReq(i)
      }
    }

    private val deqSameCycle   = WireInit(VecInit(Seq.fill(EnsbufferWidth)(false.B)))

    // if Sbuffer counsume i request, within the same cycle, i entries may enter new request.
    (0 until EnsbufferWidth).map {i =>
      deqSameCycle(i) := deqPtrVec.zipWithIndex.map{case (ptr, j) =>
        ptr.value === i.U && io.toSbuffer.req(j).fire
      }.reduce(_ || _)
    }

    /**
     * Update allocation status for each queue slot:
     *   - Enqueue sets allocated = true (higher priority)
     *   - Dequeue sets allocated = false (lower priority)
     *
     * Priority: Enqueue > Dequeue (allows same-cycle reuse)
     */
    (0 until queueSize).map{i =>
      val deqCancel = deqPtrVec.zipWithIndex.map{case (ptr, j) =>
        ptr.value === i.U && io.toSbuffer.req(j).fire
      }.reduce(_ || _)
      val enqSet    = enqPtrVec.zipWithIndex.map{case (ptr, j) =>
        ptr.value === i.U && io.fromDeqModule(j).fire
      }.reduce(_ || _)

      when(enqSet) { // enq has high priority.
        allocated(i) := true.B
      }.elsewhen(deqCancel) {
        allocated(i) := false.B
      }
    }

    //update enq pointer
    private val enqNum = PopCount(canEnq)
    enqPtrVec := VecInit(enqPtrVec.map(_ + enqNum))

    // deq
    private val doDeqNum = PopCount(io.toSbuffer.req.map(_.fire))
    deqPtrVec := VecInit(deqPtrVec.map(_ + doDeqNum))
    // When enqPtr.flag = 1, enqPtr.value = 0, deqPtr.flag=0, deqPtr.value = 0, the API '<'  fails to function correctly.
    XSError(enqPtrVec.head < deqPtrVec.head && !full, s"Something wrong in DataBufferQueue!\n")

    // connection
    for (i <- 0 until EnsbufferWidth) {
      // if port 0, it can be enter queue whenever possible. However, for other ports, enter queue requires that
      //  the port with the smaller sequencer number be ready.
      if(i == 0) {
        io.fromDeqModule(i).ready := !allocated(enqPtrVec(i).value) || deqSameCycle(enqPtrVec(i).value)
      }
      else {
        io.fromDeqModule(i).ready := (!allocated(enqPtrVec(i).value) || deqSameCycle(enqPtrVec(i).value)) && io.fromDeqModule(i - 1).ready
      }

    }

    for (i <- 0 until EnsbufferWidth) {
      ToSbufferConnect(entries(deqPtrVec(i).value), io.toSbuffer.req(i).bits)
      io.toSbuffer.req(i).valid := allocated(deqPtrVec(i).value)
      if(i > 0){
        XSError(io.toSbuffer.req(i).valid && !io.toSbuffer.req(i - 1).valid, s"low port is invalid, but ${i} port is valid!\n")
      }
    }

    io.freeCount := PopCount((~allocated.asUInt).asUInt)
    io.empty     := empty
    io.full      := full

    if(debugEn) {
      dontTouch(deqSameCycle)
      dontTouch(enqPtrVec)
      dontTouch(deqPtrVec)
    }
  }

  private class DeqModule(val param: ExeUnitParams)(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val redirect         = Flipped(ValidIO(new Redirect))
      //The head request of StoreQueue that will write to sbuffer. The rdataPtr point entries.
      val rdataDataEntries = Vec(EnsbufferWidth, Input(new SQDataEntryBundle))
      val rdataCtrlEntries = Vec(EnsbufferWidth, Input(new SQCtrlEntryBundle))
      //The head request of StoreQueue that will dequeue, The deqPtr point entries.
      val deqCtrlEntries  = Vec(EnsbufferWidth, Input(new SQCtrlEntryBundle))
      val deqDataEntries  = Vec(EnsbufferWidth, Input(new SQDataEntryBundle))

      val toUncacheBuffer = new UncacheWordIO
      val toDCache        = new ToCacheIO
      val fromRob         = Input(new FromRobIO)
      val writeToSbuffer  = new SbufferWriteIO
      val writeBack       = DecoupledIO(new ExuOutput(param))
      val exceptionInfo   = ValidIO(new MemExceptionInfo)
      val sbufferCtrl     = new SbufferCtrlIO

      val deqPtrExtNext   = Output(Vec(EnsbufferWidth, new SqPtr))
      val rdataPtrMoveCnt  = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
      val sqDeqCnt        = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
      val rdataPtrExt     = Input(Vec(EnsbufferWidth, new SqPtr))
      val deqPtrExt       = Input(Vec(EnsbufferWidth, new SqPtr))
      val validCnt        = Input(UInt(log2Ceil(StoreQueueSize + 1).W))
      val fromUnalignQueue = Flipped(ValidIO(new Bundle {
        val paddr         = UInt(PAddrBits.W)
        val sqIdx         = new SqPtr
      }))
      // for debug
      val pmaStore        = Option.when(debugEn)(Vec(EnsbufferWidth, ValidIO(new DifftestPmaStoreIO)))
      // for perf
      val perfMmioBusy    = Output(Bool())
    })

    private object UncacheState extends ChiselEnum {
      val idle      = Value
      val sendReq   = Value
      val waitResp  = Value
      val writeback = Value
    }

    private object CboState extends ChiselEnum {
      val idle      = Value
      val writeZero = Value
      val flushSb   = Value
      val sendReq   = Value
      val waitResp  = Value
      val writeback = Value
    }

    private val dataQueue        = Module(new EnterSbufferQueue(param))

    private val dataEntries      = io.rdataDataEntries //The head request of StoreQueue that will write to sbuffer. The rdataPtr point entries.
    private val ctrlEntries      = io.rdataCtrlEntries
    private val deqCtrlEntries   = io.deqCtrlEntries //The deqPtr point entries
    private val deqDataEntries   = io.deqDataEntries
    private val headDataEntry    = dataEntries.head
    private val headCtrlEntry    = ctrlEntries.head
    private val headDeqPtr       = io.deqPtrExt.head
    private val headrdataPtr     = io.rdataPtrExt.head

    /*============================================ force write sbuffer ===============================================*/
//    io.sqCancelCnt := redirectCancelCount
    val ForceWriteUpper = Wire(UInt(log2Up(StoreQueueSize + 1).W))
    ForceWriteUpper := Constantin.createRecord(s"ForceWriteUpper_${p(XSCoreParamsKey).HartId}", initValue = StoreQueueForceWriteSbufferUpper)
    val ForceWriteLower = Wire(UInt(log2Up(StoreQueueSize + 1).W))
    ForceWriteLower := Constantin.createRecord(s"ForceWriteLower_${p(XSCoreParamsKey).HartId}", initValue = StoreQueueForceWriteSbufferLower)

    val valid_cnt = io.validCnt
    io.sbufferCtrl.req.forceWrite := RegNext(Mux(valid_cnt >= ForceWriteUpper,
      true.B,
      valid_cnt >= ForceWriteLower && io.sbufferCtrl.req.forceWrite),
      init = false.B)

    /*=========================================== Data and Mask Generate =============================================*/
    /**/

    private val outData        = Wire(Vec(EnsbufferWidth , UInt(VLEN.W)))
    private val outMask        = Wire(Vec(EnsbufferWidth , UInt((VLENB).W)))


    for (i <- 0 until EnsbufferWidth) {
      val selectOffset       = 0.U - dataEntries(i).vaddr(3, 0) // need to generate 0 align data and mask
      val selectData         = (0 until VLENB).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(dataEntries(i).data, j * 8)
      )

      val byteMask           = dataEntries(i).byteMask
      val selectMsk          = (0 until VLENB).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(byteMask, j)
      )

      outData(i)         := ParallelLookUp(selectOffset, selectData)
      outMask(i)         := ParallelLookUp(selectOffset, selectMsk)

    }

    /*================================================================================================================*/
    /*================================================= CBO.FSM ======================================================*/
    /*                                           zero
    *                           + ----------------------------------- +
    *                           |                                     |
    *       clean/flush/inval   |                                     v
    *  idle ----------------> flushSb --> sednReq --> waitResp --> writeback
    *   |                       ^
    *   |   zero                |
    *   + -------> writeZero -- +
    * */
    /*================================================================================================================*/

    private val cboState: CboState.Type = RegInit(CboState.idle)
    private val cboStateNext: CboState.Type = WireInit(cboState)
    cboState := cboStateNext

    private val cboCanHandle = headCtrlEntry.allValid && !headCtrlEntry.hasException && headCtrlEntry.allocated &&
      headCtrlEntry.isCbo

    switch(cboState) {
      is(CboState.idle) {
        when(cboCanHandle) {
          cboStateNext := Mux(isCboZero(headDataEntry.cboType), CboState.writeZero, CboState.flushSb)
        }
      }
      is(CboState.writeZero) {
        when(io.writeToSbuffer.req.head.fire) {
          cboStateNext := CboState.flushSb
        }
      }
      is(CboState.flushSb) {
        when(io.sbufferCtrl.resp.empty && dataQueue.io.empty) { // Ensure there are no in-flight request.
          cboStateNext := Mux(isCboZero(headDataEntry.cboType), CboState.writeback, CboState.sendReq)
        }
      }
      is(CboState.sendReq) {
        when(io.toDCache.req.fire) {
          cboStateNext := CboState.waitResp
        }
      }
      is(CboState.waitResp) {
        when(io.toDCache.resp.fire) {
          cboStateNext := CboState.writeback
        }
      }
      is(CboState.writeback) {
        when(io.writeBack.fire) {
          cboStateNext := CboState.idle
        }
      }
    }

    // cbo handle connection
    io.sbufferCtrl.req.flush     := cboState === CboState.flushSb

    io.toDCache.req.valid        := cboState === CboState.sendReq
    io.toDCache.req.bits.address := headDataEntry.paddr
    io.toDCache.req.bits.opcode  := headDataEntry.cboType
    io.toDCache.resp.ready       := cboState === CboState.waitResp

    /*================================================================================================================*/
    /*=============================================== MMIO/NC.FSM ====================================================*/
    /*
    *       + --------- +
    *       |    isNC   |
    *       v           |
    *     idel ---> sendReq ---> waitResp ---> writeback
    * */
    /*================================================================================================================*/
    private val uncacheState: UncacheState.Type = RegInit(UncacheState.idle)
    private val uncacheStateNext: UncacheState.Type = WireInit(uncacheState)
    uncacheState := uncacheStateNext

    private val isNC             = isPbmtNC(headDataEntry.memoryType)
    private val uncacheCanHandle = !isCacheable(headDataEntry.memoryType) &&
      headCtrlEntry.allValid && !headCtrlEntry.hasException && headCtrlEntry.allocated && headCtrlEntry.committed
    private val hasHardwareError = RegInit(false.B)

    when(uncacheState === UncacheState.waitResp) {
      hasHardwareError := io.toUncacheBuffer.resp.fire && io.toUncacheBuffer.resp.bits.nderr
    }.elsewhen(uncacheState === UncacheState.writeback) {
      hasHardwareError := false.B
    }

    switch(uncacheState) {
      is(UncacheState.idle) {
        when(uncacheCanHandle) {
          uncacheStateNext := UncacheState.sendReq
        }
      }
      is(UncacheState.sendReq) {
        when(io.toUncacheBuffer.req.fire) {
          uncacheStateNext := Mux(isNC, UncacheState.idle, UncacheState.waitResp)
        }
      }
      is(UncacheState.waitResp) {
        when(io.toUncacheBuffer.resp.fire){
          uncacheStateNext := UncacheState.writeback
        }
      }
      is(UncacheState.writeback) {
        when(io.writeBack.fire) {
          uncacheStateNext := UncacheState.idle
        }
      }
    }

    // requestor, to UncacheBuffer.
    private val brodenId = Wire(UInt(uncacheIdxBits.W))
    if(uncacheIdxBits > headrdataPtr.value.getWidth){
      brodenId := Cat(0.U((uncacheIdxBits - headrdataPtr.value.getWidth).W), headrdataPtr.value)
    }
    else {
      brodenId := headrdataPtr.value
    }
    io.toUncacheBuffer.req.valid              := uncacheState === UncacheState.sendReq
    io.toUncacheBuffer.req.bits.cmd           := MemoryOpConstants.M_XWR
    io.toUncacheBuffer.req.bits.vaddr         := headDataEntry.vaddr
    io.toUncacheBuffer.req.bits.addr          := headDataEntry.paddr
    io.toUncacheBuffer.req.bits.data          := Mux(headDataEntry.vaddr(3), outData.head(VLEN - 1, 64), outData.head(63,0))
    io.toUncacheBuffer.req.bits.mask          := Mux(headDataEntry.vaddr(3), outMask.head(VLENB - 1 , 8), outMask.head(7,0))
    io.toUncacheBuffer.req.bits.robIdx        := headDataEntry.uop.robIdx
    io.toUncacheBuffer.req.bits.memBackTypeMM := isNC
    io.toUncacheBuffer.req.bits.nc            := isNC //TODO: remove it, why not use memBackTypeMM ?!
    io.toUncacheBuffer.req.bits.id            := brodenId

    // resp
    io.toUncacheBuffer.resp.ready             := true.B

    //stout
    io.writeBack.valid                        := (uncacheState === UncacheState.writeback) || (cboState === CboState.writeback)
    io.writeBack.bits                         := DontCare // init, TODO: fix it!!!!!!
    io.writeBack.bits.robIdx := dataEntries.head.uop.robIdx
    io.writeBack.bits.exceptionVec.foreach(_(hardwareError) := hasHardwareError)// override
    // for difftest, ref will skip mmio store
    if(debugEn) {
      io.writeBack.bits.debug.vaddr   := dataEntries.head.debugVaddr.get
      io.writeBack.bits.debug.paddr   := dataEntries.head.debugPaddr.get
      io.writeBack.bits.debug_seqNum.foreach(_ := dataEntries.head.debugUop.get.debug_seqNum)
      io.writeBack.bits.perfDebugInfo.foreach(_ := dataEntries.head.debugUop.get.perfDebugInfo)
    }
    if(basicDebugEn) {
      io.writeBack.bits.debug.isMMIO  := isMmio(dataEntries.head.memoryType) || isPbmtIO(dataEntries.head.memoryType)
      io.writeBack.bits.debug.isNCIO  := isPbmtNC(dataEntries.head.memoryType)
    }

    io.exceptionInfo.valid             := (uncacheState === UncacheState.writeback) || (cboState === CboState.writeback)
    io.exceptionInfo.bits.robIdx       := dataEntries.head.uop.robIdx
    io.exceptionInfo.bits.exceptionVec := ExceptionNO.selectByFu(io.writeBack.bits.exceptionVec.get, StaCfg)
    // TODO: why not fullVaddr and why don't have gpaddr ?
    io.exceptionInfo.bits.vaddr        := dataEntries.head.vaddr
    io.exceptionInfo.bits.gpaddr       := 0.U.asTypeOf(io.exceptionInfo.bits.gpaddr)
    io.exceptionInfo.bits.isForVSnonLeafPTE := false.B
    io.exceptionInfo.bits.vaNeedExt    := true.B
    io.exceptionInfo.bits.uopIdx       := 0.U.asTypeOf(io.exceptionInfo.bits.uopIdx)
    io.exceptionInfo.bits.vl           := 0.U.asTypeOf(io.exceptionInfo.bits.vl)
    io.exceptionInfo.bits.vstart       := 0.U.asTypeOf(io.exceptionInfo.bits.vstart)
    io.exceptionInfo.bits.isHyper      := false.B

    /*============================================ cacheable handle ==================================================*/
    /**
    * This section has three functions:
    * [1]. All aligned requestor will write to Sbuffer
    * [2]. All unaligned requestor will be splited, then write to Sbuffer
    * [3]. The cbo.zero will write zero to Sbuffer
    * */

    /*----------------------------------------------- Unalign Split --------------------------------------------------*/
    /* unalign write situation
    * [1]. effective bytes within 16B                  -----> shift to align with 16B
    *                                                    +--> cross Cacheline     --+
    *                                                    |                          v
    * [2]. effective bytes cross 16B, not cross page   --+    shift to align with 16B, split into two write request
    *                                                    |                          ^
    *                                                    +--> not cross Cacheline --+
    * [3]. effective bytes cross 16B, cross page       -----> shift to align with 16B, split into two write request
    *                                                          (second paddr is from Unalign Queue)
    *
    * The request of split will be write to Sbuffer through writeToSbuffer[0] and writeToSbuffer[1].
    */
    /*----------------------------------------------------------------------------------------------------------------*/

    private val unalignMask       = Wire(Vec(EnsbufferWidth , UInt((VLENB).W))) // select active bytes of split request
    private val writeSbufferData  = Wire(Vec(EnsbufferWidth , UInt(VLEN.W)))
    private val writeSbufferMask  = Wire(Vec(EnsbufferWidth , UInt((VLENB).W)))
    private val writeSbufferPaddr = Wire(Vec(EnsbufferWidth , UInt(PAddrBits.W)))
    private val writeSbufferVaddr = Wire(Vec(EnsbufferWidth , UInt(VAddrBits.W)))
    private val headCross16B      = headCtrlEntry.cross16Byte
    private val headCrossPage     = headrdataPtr === io.fromUnalignQueue.bits.sqIdx && io.fromUnalignQueue.valid
    private val diffIsHighPart    = Wire(Vec(EnsbufferWidth, Bool())) //only for difftest

    // paddrHigh and vaddrHigh only for cross16Byte split
    private val paddrLow          = Cat(headDataEntry.paddr(headDataEntry.paddr.getWidth - 1, 4), 0.U(4.W))
    private val paddrHigh         = Cat(headDataEntry.paddr(headDataEntry.paddr.getWidth - 1, 4), 0.U(4.W)) + 16.U
    private val vaddrLow          = Cat(headDataEntry.vaddr(headDataEntry.vaddr.getWidth - 1, 4), 0.U(4.W))
    private val vaddrHigh         = Cat(headDataEntry.vaddr(headDataEntry.vaddr.getWidth - 1, 4), 0.U(4.W)) + 16.U

    for (i <- 0 until EnsbufferWidth) {
      unalignMask(i)         := VecInit(Seq.fill(VLENB)(false.B)).asUInt >> dataEntries(i).vaddr(3, 0)
      // unalignWithin16Byte is for old unalign framework difftest, will be remove in the future.
      val unalignWithin16Byte = (if (debugEn) ctrlEntries(i).unalignWithin16Byte.get else false.B)
      if(i == 0) {
        writeSbufferData(i)  := outData(i)
        writeSbufferMask(i)  := outMask(i) & unalignMask(i)
        writeSbufferPaddr(i) := paddrLow
        writeSbufferVaddr(i) := vaddrLow
        diffIsHighPart(i)    := dataEntries(i).paddr(3) && !unalignWithin16Byte //TODO: will be fix in thefuture
      }
      if(i == 1) {
        writeSbufferData(i)  := Mux(headCross16B, outData(0), outData(i))
        writeSbufferMask(i)  := Mux(headCross16B, outMask(0) & (~unalignMask(0)).asUInt, outMask(i))
        writeSbufferPaddr(i) := Mux(headCrossPage,
          io.fromUnalignQueue.bits.paddr,
          Mux(headCross16B, paddrHigh, Cat(dataEntries(i).paddr(dataEntries(i).paddr.getWidth - 1, 4), 0.U(4.W))))
        // if unalign cross Page, it is must cross 16Byte
        writeSbufferVaddr(i) := Mux(headCross16B,
          vaddrHigh,
          Cat(dataEntries(i).vaddr(dataEntries(i).vaddr.getWidth - 1, 4), 0.U(4.W)))
        diffIsHighPart(i)    := Mux(headCross16B,
                                      false.B,
                                      dataEntries(i).paddr(3) && !unalignWithin16Byte //TODO: will be fix in thefuture
                                    ) // if cross 16B, port 1 must low part
      }
      else {
        writeSbufferData(i)  := outData(i)
        writeSbufferMask(i)  := outMask(i)
        writeSbufferPaddr(i) := Cat(dataEntries(i).paddr(dataEntries(i).paddr.getWidth - 1, 4), 0.U(4.W)) //align 128-bit
        writeSbufferVaddr(i) := Cat(dataEntries(i).vaddr(dataEntries(i).vaddr.getWidth - 1, 4), 0.U(4.W)) //align 128-bit
        diffIsHighPart(i)    := dataEntries(i).paddr(3) && !unalignWithin16Byte //TODO: will be fix in thefuture
      }
    }

    /*---------------------------------------- Write to Sbuffer Interface --------------------------------------------*/
    private val writeSbufferWire = Wire(Vec(EnsbufferWidth, DecoupledIO(new WriteToSbufferReqEntry)))
    private val uncacheStall     = Wire(Vec(EnsbufferWidth, Bool()))
    private val unalignStall     = Wire(Vec(EnsbufferWidth, Bool()))
    private val cboStall         = Wire(Vec(EnsbufferWidth, Bool()))
    private val toSbufferValid   = Wire(Vec(EnsbufferWidth, Bool()))
    // cross16B will occupy two write port, so only need to use port 0 fire.
    private val cross16BDeqReg   = RegEnable(headCross16B, writeSbufferWire(0).fire)

    // when deq is MMIO/NC/CMO request, don't need to write sbuffer.
    for (i <- 0 until EnsbufferWidth) {
      val ctrlEntry = ctrlEntries(i)
      val dataEntry = dataEntries(i)

      if(i == 0) {
        uncacheStall(i) := !isCacheable(dataEntry.memoryType)
        cboStall(i)     := ctrlEntry.isCbo
      }
      else {
        uncacheStall(i) := !isCacheable(dataEntry.memoryType) || uncacheStall(i - 1)
        cboStall(i)     := ctrlEntry.isCbo || cboStall(i - 1)
      }
    }
    // generate to sbuffer valid
    /*
    * NOTE: [1] only two port of dataQueue is ready, the request of cross16B can write to dataQueue.
    *       [2] dataQueue.io.empty means dataQueue can enter two request at same time.
    *       [3] entry.committed contains entry.allocated && entry.allValid && !entry.hasException && isRobHead.
    */

    // toSbufferValid(0) use dataQueue.io.empty to judge unalign split valid, need to modify if  EnsbufferWifth > 2,
    // can use dataQueue.io.freeCount
    require(EnsbufferWidth == 2)

    for(i <- 0 until EnsbufferWidth) {
      val ctrlEntry = ctrlEntries(i)
      if(i == 0) {
        toSbufferValid(i) := !uncacheStall(i) && !cboStall(i) && (!headCross16B || dataQueue.io.empty) &&
          !unalignStall(i) && ctrlEntry.committed &&
          !(ctrlEntry.vecMbCommit && !ctrlEntry.allValid || ctrlEntry.vecInactive) //TODO: vecMbCommit will be remove in the future
        // [NOTE1]: entry.committed contains entry.allocated && entry.allValid && !entry.hasException && isRobHead.
        // [NOTE2]: here I use dataQueue.io.empty because EnsbufferWifth == 2, if EnsbufferWifth > 2, need to modify.

        unalignStall(i) := false.B // if first port is unalign, make it can write to sbuffer.
      }
      else if(i == 1) { // override port 1 to write second request of cross16B
        // Regarding writing to port 1's Sbuffer, only the following two scenarios permit writing:
        //  1. Port 0 write a unaligned request cross 16 bytes, preempting port 1's write port.
        //  2. Port 0 is ready, and the Sbuffer can process two write requests simultaneously.
        toSbufferValid(i) := !uncacheStall(i) && !cboStall(i) && ctrlEntry.committed &&
          !(ctrlEntry.vecMbCommit && !ctrlEntry.allValid || ctrlEntry.vecInactive) && //TODO: vecMbCommit will be remove in the future
          toSbufferValid(i - 1) || (headCross16B && toSbufferValid(0)) && !unalignStall(i)
        // [NOTE]: entry.committed contains entry.allocated && entry.allValid && !entry.hasException && isRobHead.

        unalignStall(i) := ctrlEntry.cross16Byte && !headCross16B
      }
      else {
        toSbufferValid(i) := !uncacheStall(i) && !cboStall(i) && !unalignStall(i) && ctrlEntry.committed &&
          !(ctrlEntry.vecMbCommit && !ctrlEntry.allValid || ctrlEntry.vecInactive) && //TODO: vecMbCommit will be remove in the future
          toSbufferValid(i - 1)
        // [NOTE]: entry.committed contains entry.allocated && entry.allValid && !entry.hasException && isRobHead.

        unalignStall(i) := ctrlEntry.cross16Byte || headCross16B
      }
    }

    for(i <- 0 until EnsbufferWidth) {
      val port      = writeSbufferWire(i)
      val dataEntry = dataEntries(i)
      val ctrlEntry = ctrlEntries(i)

      port.bits.data     := writeSbufferData(i)
      port.bits.mask     := writeSbufferMask(i)
      port.bits.addr     := writeSbufferPaddr(i)
      port.bits.vaddr    := writeSbufferVaddr(i)

      port.bits.wline    := ctrlEntry.isCbo && isCboZero(dataEntry.cboType)
      port.bits.prefetch := dataEntry.prefetch
      port.bits.vecValid := true.B
      port.valid         := toSbufferValid(i)

      XSError(ctrlEntry.vecInactive && !ctrlEntry.isVec, s"inactive element must be vector! ${i}")
      XSError(ctrlEntry.vecMbCommit && !ctrlEntry.isVec, s"vecMbCommit element must be vector! ${i}")
    }

    dataQueue.io.fromDeqModule.zip(writeSbufferWire).map{ case (sink, source) =>
      sink               <> source
    }

    io.writeToSbuffer    <> dataQueue.io.toSbuffer

    /*============================================ deqPtr generate ===================================================*/
    /*
    * NOTE: Only when port 0 and port 1 are ready can write cross16B request, so only use io.writeToSbuffer.req.head.fire
    *       to caculate sbufferFireNum.
    * deqPtr will move when [write to sbuffer / writeback / vector inactive element]
    * rdataPtr will move when [nc request fire / write to SQ2SBPipelineConnect_i / vector inactive element]
    * NOTE: when deq mmio/cbo, rdataPtr === deqPtr, because mmio/cbo need to execute at head of StoreQueue.
    * */
    private val sbufferFireNum = Mux(cross16BDeqReg,
      Cat(RegNext(io.writeToSbuffer.req.head.fire), 0.U),
      Cat(io.writeToSbuffer.req.map{case p => RegNext(p.fire)}))

    // [NOTE]: when point a inactive entry, move pointer.
    private val deqPtrVectorInactiveValid = WireInit(VecInit(Seq.fill(EnsbufferWidth)(false.B)))

    deqCtrlEntries.zip(deqDataEntries).zipWithIndex.map{case ((ctrl, data), i) =>
      deqPtrVectorInactiveValid(i) := ctrl.allocated && ctrl.committed &&
        (ctrl.vecMbCommit && !ctrl.allValid || ctrl.vecInactive) //TODO: vecMbCommit will be remove in the future
    }

    private val deqPtrVectorInactiveMove = Cat(deqPtrVectorInactiveValid.zipWithIndex.map{case (v, i) =>
      if(i == 0) v
      else v && (deqPtrVectorInactiveValid(i - 1) || sbufferFireNum(i - 1).asBool)
    })

    private val uncacheMove = VecInit(deqCtrlEntries.map(x => x.allocated && x.handleFinish && x.committed)).asUInt

    // sbufferFireNum need to RegNext, because write to sbuffer need 2 cycle, storeQueue need to forward 1 more cycle
    val deqCount = Cat(sbufferFireNum, deqPtrVectorInactiveMove, uncacheMove) // timing is ok ?

    io.sqDeqCnt := PopCount(VecInit(deqCount).asUInt)
    io.deqPtrExtNext := io.deqPtrExt.map(_ + io.sqDeqCnt)

    private val pipelineConnectFireNum = Mux(headCross16B,
      Cat(writeSbufferWire.head.fire, 0.U),
      Cat(writeSbufferWire.map(_.fire)))
    // nc/mmio/cbo deq
    private val otherMove        = uncacheState === UncacheState.sendReq && io.toUncacheBuffer.req.fire && isNC ||
      io.writeBack.fire

    // [NOTE]: when point a inactive entry, move pointer.
    private val rdataPtrVectorInactiveValid = WireInit(VecInit(Seq.fill(EnsbufferWidth)(false.B)))

    ctrlEntries.zip(dataEntries).zipWithIndex.map{case ((ctrl, data), i) =>
      rdataPtrVectorInactiveValid(i) := ctrl.allocated && ctrl.committed && dataQueue.io.empty &&
      (ctrl.vecMbCommit && !ctrl.allValid || ctrl.vecInactive) //TODO: vecMbCommit will be remove in the future
    }

    private val rdataPtrVectorInactiveMove = Cat(rdataPtrVectorInactiveValid.zipWithIndex.map{case (v, i) =>
      if(i == 0) v
      else v && (rdataPtrVectorInactiveValid(i - 1) || pipelineConnectFireNum(i - 1).asBool)
    })

    private val rdataMoveCnt = Cat(pipelineConnectFireNum, rdataPtrVectorInactiveMove, otherMove)

    io.rdataPtrMoveCnt        := PopCount(rdataMoveCnt)

    /*============================================ other connection ==================================================*/
    io.perfMmioBusy := uncacheState =/= UncacheState.idle

    if(debugEn) {
      // [NOTE]: low 4 bit of addr/vaddr will be omitted in the sbuffer, but it will be used for difftest.
      for (i <- 0 until EnsbufferWidth) {
        io.pmaStore.foreach { case sink =>
          sink(i).valid := writeSbufferWire(i).fire
          sink(i).bits.addr := writeSbufferWire(i).bits.addr
          sink(i).bits.data := writeSbufferWire(i).bits.data
          sink(i).bits.mask := writeSbufferWire(i).bits.mask
          sink(i).bits.wline := writeSbufferWire(i).bits.wline
          sink(i).bits.vecValid := writeSbufferWire(i).bits.vecValid
          sink(i).bits.diffIsHighPart := diffIsHighPart(i) // indicate whether valid data in high 64-bit, only for scalar store event!
        }
      }
    }

    /*=============================================== debug dontTouch =================================================*/
    if(debugEn) {
      dontTouch(toSbufferValid)
      dontTouch(writeSbufferData)
      dontTouch(writeSbufferMask)
      dontTouch(writeSbufferPaddr)
      dontTouch(writeSbufferVaddr)
      dontTouch(unalignMask)
      dontTouch(deqCount)
      dontTouch(outMask)
      dontTouch(outData)
      dontTouch(writeSbufferWire)
      dontTouch(deqPtrVectorInactiveValid)
      dontTouch(deqPtrVectorInactiveMove)
      dontTouch(rdataPtrVectorInactiveValid)
      dontTouch(rdataPtrVectorInactiveMove)
    }
  }
  /*==================================================================================================================*/
  /* UnalignQueue will save the second physical address of the oldest SQUnalignQueueSize crossPage unaligned requests.*/
  private class UnalignQueue(val param: ExeUnitParams)(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val redirect       = Flipped(ValidIO(new Redirect))
      val fromStaS2      = Vec(StorePipelineWidth, Flipped(DecoupledIO(new UnalignQueueIO)))
      val fromSQ = new Bundle {
        val addrReadyPtr = Input(new SqPtr)
      }
      val toDeqModule = ValidIO(new Bundle {
        val paddr        = UInt(PAddrBits.W)
        val sqIdx        = new SqPtr
      })
    })
    private val enqWidth: Int  = io.fromStaS2.length
    private val queueSize: Int = SQUnalignQueueSize

    private val entries    = Reg(Vec(queueSize, new UnalignBufferEntry())) // no need to reset!
    private val allocated  = RegInit(VecInit(Seq.fill(queueSize)(false.B)))
    private val enqPtr     = RegInit(0.U.asTypeOf(new UqPtr))
    private val deqPtr     = RegInit(0.U.asTypeOf(new UqPtr))
    private val needCancel = WireInit(VecInit(Seq.fill(queueSize)(false.B)))

    private val headEntry  = entries(deqPtr.value)

    private val empty      = enqPtr.value === deqPtr.value && enqPtr.flag === deqPtr.flag
    private val full       = enqPtr.value === deqPtr.value && enqPtr.flag =/= deqPtr.flag

    // enq
    private val canEnq     = io.fromStaS2.map{case port => port.fire} // one-hot, only second request of the unaligned need to enter.
    private val doEnq      = canEnq.reduce(_ || _)
    private val doEnqReq   = Mux1H(canEnq, io.fromStaS2.map(_.bits))

    when(doEnq) {
      entries(enqPtr.value).robIdx     := doEnqReq.robIdx
      entries(enqPtr.value).paddrHigh  := doEnqReq.paddr(PAddrBits - 1, PageOffsetWidth)
      entries(enqPtr.value).sqIdx      := doEnqReq.sqIdx
    }

    (0 until queueSize).map{i =>
      when(needCancel(i)) { // when redirect, unalignQueue not allow enqueue.
        allocated(i) := false.B
      }.otherwise{
        allocated(i) := (i.U === enqPtr.value) && doEnq
      }
    }

    // update pointer
    needCancel.zipWithIndex.map{case (sink, i) =>
      sink := entries(i).robIdx.needFlush(io.redirect) && allocated(i)
    }

    private val redirectCount = PopCount(needCancel)

    when(io.redirect.valid) {
      enqPtr := enqPtr - redirectCount
    }.otherwise {
      when(doEnq) {
        enqPtr := enqPtr + 1.U
      }
    }

    when(io.toDeqModule.fire) {
      deqPtr := deqPtr + 1.U
    }
    // When enqPtr.flag = 1, enqPtr.value = 0, deqPtr.flag=0, deqPtr.value = 0, the API '<'  fails to function correctly.
    XSError(enqPtr < deqPtr && !full, s"Something wrong in UnalignQueue!")
    // connection
    io.toDeqModule.bits.paddr := headEntry.paddr
    io.toDeqModule.bits.sqIdx := headEntry.sqIdx
    io.toDeqModule.valid      := !empty

    io.fromStaS2.map{case sink =>
      sink.ready := !full && io.fromSQ.addrReadyPtr === sink.bits.sqIdx && !io.redirect.valid
    }

  }



  val io = IO(new StoreQueueIO(param))
  println("StoreQueue: size:" + StoreQueueSize)

  // entries define
  val dataEntries        = Reg(Vec(StoreQueueSize, new SQDataEntryBundle())) // no need to reset
  val ctrlEntries        = RegInit(VecInit(Seq.fill(StoreQueueSize)(0.U.asTypeOf(new SQCtrlEntryBundle)))) // need to reset

  // ptr define
  val enqPtrExt          = RegInit(VecInit((0 until io.enq.req.length).map(_.U.asTypeOf(new SqPtr))))
  // when io.writeToSbuffer_i.fire or writeback.fire, deqPtr will move.
  //
  // It should be noted that the deqPtr move is a store request at the end of the store queue lifecycle,
  // whereas the rdataPtr move is not.
  //
  val deqPtrExt          = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new SqPtr))))
  // Because deq need multi cycle, use rdataPtr to read and split next EnsbufferWidth entries.
  // when
  // 1. head[Ctrl & Data]entries write to pipeline that between StoreQueue and Sbuffer.
  // 2. nc send to uncacheBuffer.
  // it will be move.
  //
  // rdataPtr may be equal to deqPtr when [MMIO/CBO].
  val rdataPtrExt        = RegInit(VecInit((0 until EnsbufferWidth).map(_.U.asTypeOf(new SqPtr))))
  val cmtPtrExt          = RegInit(VecInit((0 until CommitWidth).map(_.U.asTypeOf(new SqPtr))))
  val addrReadyPtrExt    = RegInit(0.U.asTypeOf(new SqPtr))
  val dataReadyPtrExt    = RegInit(0.U.asTypeOf(new SqPtr))

  val validCount         = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val allowEnqueue       = validCount <= (StoreQueueSize - LSQStEnqWidth).U
  val needCancel         = Wire(Vec(StoreQueueSize, Bool()))

  // the means of `head` is the next request that StoreQueue need to process.
  val rdataDataEntries    = rdataPtrExt.map{ case ptr =>
    dataEntries(ptr.value)
  }
  val rdataCtrlEntries    = rdataPtrExt.map{ case ptr =>
    ctrlEntries(ptr.value)
  }
  val deqCtrlEntries      = deqPtrExt.map{ case ptr =>
    ctrlEntries(ptr.value)
  }
  val deqDataEntries      = deqPtrExt.map{ case ptr =>
    dataEntries(ptr.value)
  }

  /*========================================== Module define & connection ============================================*/
  // use `private` to limit module connection within this module.

  private val forwardModule         = Module(new ForwardModule(param))
  private val deqModule             = Module(new DeqModule(param))
  private val unalignQueue          = Module(new UnalignQueue(param))

  // forward connection
  forwardModule.io.query           <> io.forward
  forwardModule.io.ctrlInfo.deqPtr := deqPtrExt(0)
  forwardModule.io.ctrlInfo.enqPtr := enqPtrExt(0)
  dataEntries.zip(forwardModule.io.dataEntriesIn).foreach{ case (source, sink) =>
    sink := source
  }
  ctrlEntries.zip(forwardModule.io.ctrlEntriesIn).foreach { case (source, sink) =>
    sink := source
  }

  // deqModule connection
  deqModule.io.redirect         := io.redirect
  deqModule.io.rdataCtrlEntries.zip(rdataCtrlEntries).foreach{ case (sink, source) =>
    sink := source
  }
  deqModule.io.rdataDataEntries.zip(rdataDataEntries).foreach{ case (sink, source) =>
    sink := source
  }
  deqModule.io.deqCtrlEntries.zip(deqCtrlEntries).foreach{ case (sink, source) =>
    sink := source
  }
  deqModule.io.deqDataEntries.zip(deqDataEntries).foreach{ case (sink, source) =>
    sink := source
  }
  deqModule.io.toUncacheBuffer  <> io.toUncacheBuffer
  deqModule.io.toDCache         <> io.toDCache
  deqModule.io.fromRob          <> io.fromRob
  deqModule.io.writeToSbuffer   <> io.writeToSbuffer
  io.writeBack                  <> deqModule.io.writeBack
  io.sbufferCtrl                <> deqModule.io.sbufferCtrl
  deqModule.io.fromUnalignQueue <> unalignQueue.io.toDeqModule
  deqModule.io.deqPtrExt        := deqPtrExt
  deqModule.io.rdataPtrExt      := rdataPtrExt
  deqModule.io.validCnt         := validCount
  io.exceptionInfo              := deqModule.io.exceptionInfo

  val deqPtrExtNext = deqModule.io.deqPtrExtNext
  val sqDeqCnt      = deqModule.io.sqDeqCnt
  val mmioBusy      = deqModule.io.perfMmioBusy
  val diffPmaStore  = deqModule.io.pmaStore
  val rdataMoveCnt  = deqModule.io.rdataPtrMoveCnt

  // unalignQueue connection
  unalignQueue.io.redirect            := io.redirect
  unalignQueue.io.fromSQ.addrReadyPtr := addrReadyPtrExt
  unalignQueue.io.fromStaS2.zip(io.fromStoreUnit.unalignQueueReq).map{case (sink, source) =>
    sink <> source
  }

}


class NewStoreQueue(implicit p: Parameters) extends NewStoreQueueBase with HasPerfEvents {

  /**
   * Enqueue at dispatch
   *
   * Currently, StoreQueue only allows enqueue when #emptyEntries > EnqWidth
   * Dynamic enq based on numLsElem number
   */

  io.enq.canAccept := allowEnqueue
  val canEnqueue = io.enq.req.map(_.valid)
  val enqCancel = io.enq.req.map(_.bits.uop.robIdx.needFlush(io.redirect))
  val vStoreFlow = io.enq.req.map(_.bits.uop.numLsElem.asTypeOf(UInt(elemIdxBits.W)))
  val validVStoreFlow = vStoreFlow.zipWithIndex.map{case (vStoreFlowNumItem, index) =>
    Mux(!RegNext(io.redirect.valid) && canEnqueue(index), vStoreFlowNumItem, 0.U)}
  val validVStoreOffset = vStoreFlow.zip(io.enq.req).map{case (flow, req) => Mux(req.bits.needAlloc, flow, 0.U)}
  val validVStoreOffsetRShift = 0.U +: validVStoreOffset.take(vStoreFlow.length - 1)

  val enqLowBound = io.enq.req.map(_.bits.uop.sqIdx)
  val enqUpBound  = io.enq.req.map(x => x.bits.uop.sqIdx + x.bits.uop.numLsElem)
  val enqCrossLoop = enqLowBound.zip(enqUpBound).map{case (low, up) => low.flag =/= up.flag}

  // TODO: vecMbCommit will be remove in the future.
  val vecCommittmp = Wire(Vec(StoreQueueSize, Vec(VecStorePipelineWidth, Bool())))
  val vecCommit = Wire(Vec(StoreQueueSize, Bool()))

  for(i <- 0 until StoreQueueSize) {

    /*================================================================================================================*/
    /*================================================= enq ==========================================================*/
    /*================================================================================================================*/

    val entryCanEnqSeq = (0 until io.enq.req.length).map { j =>
      val entryHitBound = Mux(
        enqCrossLoop(j),
        enqLowBound(j).value <= i.U || i.U < enqUpBound(j).value,
        enqLowBound(j).value <= i.U && i.U < enqUpBound(j).value
      )
      canEnqueue(j) && !enqCancel(j) && entryHitBound
    }

    val entryCanEnq = entryCanEnqSeq.reduce(_ || _)
    val selectBits = ParallelPriorityMux(entryCanEnqSeq, io.enq.req.map(_.bits))

    val deqCancel = VecInit(deqPtrExt.zipWithIndex.map{case (ptr, j) =>
      ptr.value === i.U && sqDeqCnt > j.U
    }).asUInt.orR

    val handleFinishSet = rdataPtrExt.head.value === i.U &&
      (io.writeBack.fire || io.toUncacheBuffer.req.fire && isPbmtNC(dataEntries(i).memoryType))

    when (entryCanEnq) {
      connectSamePort(dataEntries(i).uop, selectBits.uop) //TODO: will be remove in the future.
    }.elsewhen(deqCancel || needCancel(i)) {

    }
    if(debugEn) {
      when (entryCanEnq){
        dataEntries(i).debugUop.get := selectBits.debugUop.get
      }
    }

    when (entryCanEnq) {
      ctrlEntries(i).allocated  := true.B
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).allocated  := false.B
    }

    when(entryCanEnq) {
      ctrlEntries(i).handleFinish := false.B
    }.elsewhen(handleFinishSet) {
      ctrlEntries(i).handleFinish := true.B
    }

    XSError(ctrlEntries(i).allocated && entryCanEnq, s"entry double allocate! index: ${i}\n")
    XSError(!ctrlEntries(i).allocated && (ctrlEntries(i).addrValid || ctrlEntries(i).dataValid), s"invalid entry have addrValid or dataValid! index: ${i}\n")

    for (i <- 0 until io.enq.req.length) {
      val sqIdx = enqPtrExt(0) + validVStoreOffsetRShift.take(i + 1).reduce(_ + _)
      val index = io.enq.req(i).bits.uop.sqIdx
      XSError(canEnqueue(i) && !enqCancel(i) && (!io.enq.canAccept || !io.enq.lqCanAccept), s"must accept $i\n")
      XSError(canEnqueue(i) && !enqCancel(i) && index.value =/= sqIdx.value, s"must be the same entry $i\n")
      io.enq.resp(i).sqIdx := sqIdx
    }
    XSDebug(p"(ready, valid): ${io.enq.canAccept}, ${Binary(Cat(io.enq.req.map(_.valid)))}\n")

    /*================================================================================================================*/
    /*=============================================== sta ctrl =======================================================*/
    /*================================================================================================================*/
    /**
     * In storeUnit stage 1, paddr is ready                --> set addrValid
     *                       unalign check is ready        --> set unaligned, cross16Byte
     * In storeUnit stage 2, PMP/PMA check result is ready --> set hasException, memoryType
     *                       dcache resp is ready          --> set prefetch
     * */

    /*======================================== staIn [sta Stage 1] ===================================================*/

    val staValidSetVec  = VecInit(io.fromStoreUnit.storeAddrIn.map{case port =>
      val index         = port.bits.uop.sqIdx.value
      val setValid      = index === i.U && port.fire && !needCancel(i)
      setValid
    }) // one-hot select vec

    val staSetValid    = staValidSetVec.reduce(_ || _)
    val addrValidSet   = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      port.bits.isLastRequest && !port.bits.tlbMiss && staValidSetVec(j)
    }.reduce(_ || _)
    val cross16ByteSet = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      port.bits.isUnalign && !port.bits.unalignWithin16Byte && staValidSetVec(j)
    }.reduce(_ || _)
    val cboSetVec = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      LSUOpType.isCboAll(port.bits.uop.fuOpType) && staValidSetVec(j)
    }
    val isCboSet = cboSetVec.reduce(_ || _)

    when(staSetValid) {
      ctrlEntries(i).addrValid    := addrValidSet // need hasException?
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).addrValid    := false.B
    }

    when(staSetValid) { // no need to clean when deq or cancel, because it will be set when set addrValid
      ctrlEntries(i).cross16Byte  := cross16ByteSet
      ctrlEntries(i).isCbo        := isCboSet
    } // don't need to set false for low power, it will be set every instruction.

    io.fromStoreUnit.storeAddrIn.zipWithIndex.map{case (port, j) =>
      val index         = port.bits.uop.sqIdx.value
      val setValid      = cboSetVec(j)
      when(setValid) {
        dataEntries(i).cboType   := Mux1H(List(
          isCboClean(port.bits.uop.fuOpType(1, 0)) -> CboType.clean, // TODO: don't use (1, 0)
          isCboFlush(port.bits.uop.fuOpType(1, 0)) -> CboType.flush,
          isCboInval(port.bits.uop.fuOpType(1, 0)) -> CboType.inval,
          isCboZero(port.bits.uop.fuOpType(1, 0))  -> CboType.zero
        ))
      }
    }


    if(debugEn) {
      val unalignWithin16BSet = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
        port.bits.isUnalign && port.bits.unalignWithin16Byte && staValidSetVec(j)
      }.reduce(_ || _)
      when(staSetValid) {
        ctrlEntries(i).unalignWithin16Byte.foreach(_ := unalignWithin16BSet)
      }
    }

    // TODO: fix this for unalign store @LWD
    //TODO: vector element maybe set addrValid twice because of replay uop, which will be remove in the future.
//    XSError(ctrlEntries(i).addrValid && staSetValid && !ctrlEntries(i).isVec, s"[addrValid] double allocate! index: ${i}\n")

    /*======================================= staInRe [sta Stage 2] ==================================================*/

    val staReValidVec = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      RegNext(!port.bits.tlbMiss && staValidSetVec(j)) && !needCancel(i) //TODO: use valid of s1, will be remove in the future.
    } // at s2 stage of storeUnit

    val staReValid = (0 until staReValidVec.length).map { j =>
      staReValidVec(j) && io.fromStoreUnit.storeAddrInRe(j).isLastRequest
    }.reduce(_ || _) // at s2 stage of storeUnit

    val prefetchSet     = io.fromStoreUnit.storeAddrInRe.zipWithIndex.map { case (port, j) =>
      port.cacheMiss && staReValidVec(j)
    }.reduce(_ || _)
    val hasExceptionSet = io.fromStoreUnit.storeAddrInRe.zipWithIndex.map { case (port, j) =>
      port.hasException && staReValidVec(j)
    }.reduce(_ || _)
    val ncSet           = io.fromStoreUnit.storeAddrInRe.zipWithIndex.map { case (port, j) =>
      port.nc && staReValidVec(j)
    }.reduce(_ || _)
    val mmioSet         = io.fromStoreUnit.storeAddrInRe.zipWithIndex.map { case (port, j) =>
      port.mmio && staReValidVec(j)
    }.reduce(_ || _)
    val memBackTypeSet  = io.fromStoreUnit.storeAddrInRe.zipWithIndex.map { case (port, j) =>
      port.memBackTypeMM && staReValidVec(j)
    }.reduce(_ || _) // memBackTypeMM  = true.B means it is main memory region , false.B means it is IO region.

    when(staReValid) { // no need to clean when deq or cancel, because it will be used when waitStoreS2 == false
      dataEntries(i).prefetch := prefetchSet
    }//  don't need to set false for low power, it will be set every instruction.

    when(staSetValid) {
      ctrlEntries(i).waitStoreS2  := true.B
    }.elsewhen(staReValid) { // no need to clean when deq or cancel, because it will be set when set addrValid
      ctrlEntries(i).waitStoreS2  := false.B
    }

    when(staReValid) {
      ctrlEntries(i).hasException := hasExceptionSet
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).hasException := false.B
    }

    when(staReValid) { // no need to clean when deq or cancel, because it will be used when waitStoreS2 == false
      /*
         cacheable: "00".U
         pbmtNc:    "01".U
         pbmtIo:    "10".U
         io:        "11".U // IO device
      */
      dataEntries(i).memoryType := Cat(mmioSet, ncSet || !memBackTypeSet)
      /*
       * [NOTE]: To explain the logical operations above, the truth table is as follows:
       * The signal of [memBackTypeMM] means request is main memory region.
       *
       *           |  memBackTypeSet | !memBackTypeSet | ncSet | mmioSet | memoryType[1] | memoryType[0] |
       * Cacheable |       1         |       0         |   0   |    0    |        0      |       0       |
       * NC        |       1         |       0         |   1   |    0    |        0      |       1       |
       * PbmtIO    |       1         |       0         |   0   |    1    |        1      |       0       |
       * IO        |       0         |       1         |   0   |    1    |        1      |       1       |
       *                                     |             |        |             ^              ^
       *                                     |             |        +-------------+              |
       *                                     +----- or ----+                                     |
       *                                            |                                            |
       *                                            +--------------------------------------------+
       * */
    }//  don't need to set false for low power, it will be set every instruction.

    XSError(!mmioSet && !memBackTypeSet && !hasExceptionSet && staReValid, s"mmio not set but memBackTypeMM is zero! ${i}\n")

    /*================================================================================================================*/
    /*=============================================== std ctrl =======================================================*/
    /*================================================================================================================*/

    val dataValidSet = VecInit(io.storeDataIn.map{ case port =>
      val index = port.bits.sqIdx.value
      index === i.U && port.fire && !needCancel(i)
    }).asUInt.orR

    when(dataValidSet) {
      ctrlEntries(i).dataValid := true.B
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).dataValid := false.B
    }

    //TODO: vector element maybe set dataValid twice because of replay uop, which will be remove in the future.
//    XSError(ctrlEntries(i).dataValid && dataValidSet && !ctrlEntries(i).isVec, s"[dataValid] double allocate! index: ${i}\n")
    XSError(!ctrlEntries(i).allocated && deqCancel, s"double deq! index: ${i}\n")

    /*================================================================================================================*/
    /*============================================== vector ctrl =====================================================*/
    /*================================================================================================================*/

    when(entryCanEnq) {
      ctrlEntries(i).isVec := FuType.isVStore(selectBits.uop.fuType)
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).isVec := false.B
    }

    // TODO: vecMbCommit will be remove in the future.
    val fbk = io.fromVMergeBuffer
    for (j <- 0 until VecStorePipelineWidth) {
      vecCommittmp(i)(j) := fbk(j).valid && (fbk(j).bits.isCommit || fbk(j).bits.isFlush) &&
        dataEntries(i).uop.robIdx === fbk(j).bits.robidx && dataEntries(i).uop.uopIdx === fbk(j).bits.uopidx
    }
    // vector feedback may occur with deqCancel/needCancel at the same time
    vecCommit(i) := vecCommittmp(i).reduce(_ || _) && !needCancel(i) && !deqCancel && ctrlEntries(i).allocated

    when (vecCommit(i)) {
      ctrlEntries(i).vecMbCommit := true.B
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).vecMbCommit := false.B
    }

    ctrlEntries(i).vecInactive := false.B //TODO: will be use in the future

    /*================================================================================================================*/
    /*============================================== cancel ctrl =====================================================*/
    /*================================================================================================================*/

    needCancel(i) := !ctrlEntries(i).committed && dataEntries(i).uop.robIdx.needFlush(io.redirect) && ctrlEntries(i).allocated

    // debug don't touch
    if(debugEn) {
      dontTouch(deqCancel)
      dontTouch(staSetValid)
      dontTouch(staReValid)
      dontTouch(prefetchSet)
      dontTouch(hasExceptionSet)
      dontTouch(ncSet)
      dontTouch(memBackTypeSet)
      dontTouch(vecCommittmp)
      dontTouch(vecCommit)
    }
  }

  /*=============================================== update ptr =======================================================*/

  /**
   * Update enqPtr
   * */
  val enqCancelValid = canEnqueue.zip(io.enq.req).map{case (v , x) =>
    v && x.bits.uop.robIdx.needFlush(io.redirect)
  }
  val enqCancelNum = enqCancelValid.zip(vStoreFlow).map{case (v, flow) =>
    Mux(v, flow, 0.U)
  }
  val lastEnqCancel = RegEnable(enqCancelNum.reduce(_ + _), io.redirect.valid) // 1 cycle after redirect

  val lastCycleCancelCount = PopCount(RegEnable(needCancel, io.redirect.valid)) // 1 cycle after redirect
  val lastCycleRedirect = RegNext(io.redirect.valid) // 1 cycle after redirect
  val enqNumber = validVStoreFlow.reduce(_ + _)

  val lastlastCycleRedirect=RegNext(lastCycleRedirect)// 2 cycle after redirect
  val redirectCancelCount = RegEnable(lastCycleCancelCount + lastEnqCancel, 0.U, lastCycleRedirect) // 2 cycle after redirect

  when (lastlastCycleRedirect) {
    // we recover the pointers in 2 cycle after redirect for better timing
    enqPtrExt := VecInit(enqPtrExt.map(_ - redirectCancelCount))
  }.otherwise {
    // lastCycleRedirect.valid or nornal case
    // when lastCycleRedirect.valid, enqNumber === 0.U, enqPtrExt will not change
    enqPtrExt := VecInit(enqPtrExt.map(_ + enqNumber))
  }
  assert(!(lastCycleRedirect && enqNumber =/= 0.U))

  /**
   * Update addr/dataReadyPtr when issue from rs
   */
  // update issuePtr
  val IssuePtrMoveStride = 4
  require(IssuePtrMoveStride >= 2)

  val addrReadyLookupVec = (0 until IssuePtrMoveStride).map(addrReadyPtrExt + _.U)
  val addrReadyLookup = addrReadyLookupVec.map(ptr =>
//    (MemoryType.isPMPIO(ctrlEntries(ptr.value).memoryType) || ctrlEntries(ptr.value).addrValid || ctrlEntries(ptr.value).vecInactive)
      (ctrlEntries(ptr.value).addrValid || ctrlEntries(ptr.value).vecInactive || ctrlEntries(ptr.value).vecMbCommit) &&
        ctrlEntries(ptr.value).allocated && ptr =/= enqPtrExt(0))
  val nextAddrReadyPtr = addrReadyPtrExt + PriorityEncoder(VecInit(addrReadyLookup.map(!_) :+ true.B))
  addrReadyPtrExt := nextAddrReadyPtr

  val stAddrReadyVecWire = Wire(Vec(StoreQueueSize, Bool()))
  (0 until StoreQueueSize).map(i => {
//    stAddrReadyVecReg(i) := ctrlEntries(i).allocated && (mmio(i) || addrvalid(i) || (isVec(i) && vecMbCommit(i)))
    stAddrReadyVecWire(i) := (ctrlEntries(i).addrValid || ctrlEntries(i).vecInactive || ctrlEntries(i).vecMbCommit) &&
      ctrlEntries(i).allocated
  })

  when (io.redirect.valid) {
    addrReadyPtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )

    dataReadyPtrExt := Mux(
      isAfter(cmtPtrExt(0), deqPtrExt(0)),
      cmtPtrExt(0),
      deqPtrExtNext(0) // for mmio insts, deqPtr may be ahead of cmtPtr
    )
  }

    // enqPtr update
  val dataReadyLookupVec = (0 until IssuePtrMoveStride).map(dataReadyPtrExt + _.U)
  val dataReadyLookup = dataReadyLookupVec.map(ptr =>
      (ctrlEntries(ptr.value).addrValid && !ctrlEntries(ptr.value).waitStoreS2 && //TODO: remove waitStoreS2 in the future
        (isMmio(dataEntries(ptr.value).memoryType) || ctrlEntries(ptr.value).dataValid) ||
        ctrlEntries(ptr.value).vecMbCommit) && //TODO: vecMbCommit will be remove in the future, entry maybe inactive, so we nned to or vecMbCommit.
      ctrlEntries(ptr.value).allocated &&
      ptr =/= enqPtrExt(0)
  )
  val nextDataReadyPtr = dataReadyPtrExt + PriorityEncoder(VecInit(dataReadyLookup.map(!_) :+ true.B))
  dataReadyPtrExt := nextDataReadyPtr

  val stDataReadyVecReg = Wire(Vec(StoreQueueSize, Bool()))
  (0 until StoreQueueSize).map(i => {
    stDataReadyVecReg(i) := (ctrlEntries(i).addrValid && !ctrlEntries(i).waitStoreS2 && // ctrl memoryType is ready.
        (isMmio(dataEntries(i).memoryType) || ctrlEntries(i).dataValid) ||
      ctrlEntries(i).vecMbCommit) &&
      ctrlEntries(i).allocated
  })

  // deqPtr logic
  deqPtrExt := deqPtrExtNext
  rdataPtrExt := rdataPtrExt.map(_ + rdataMoveCnt)

  XSError(deqPtrExt(0) > rdataPtrExt(0), "Why deqPtr > rdataPtr? something error!")
  XSError(deqPtrExt(0) > enqPtrExt(0),   "Why deqPtr > enqPtr? something error!")
  /******************************************** store pipeline write **************************************************/
  for (i <- 0 until StorePipelineWidth) {
    val storeAddrIn   = io.fromStoreUnit.storeAddrIn(i)
    val storeAddrInRe = io.fromStoreUnit.storeAddrInRe(i)
    val stWbIdx       = storeAddrIn.bits.uop.sqIdx.value
    val byteStart     = storeAddrIn.bits.vaddr(VWordOffset - 1, 0)
    val byteOffset    = MemorySize.ByteOffset(storeAddrIn.bits.size)

    // !isLastRequest && cross4KPage means it is first request of cross page unalign  --> save paddr
    //  isLastRequest && cross4KPage means it is second request of cross page unalign --> not save paddr
    // isLastRequest && !cross4KPage means it is normal request                       --> save paddr
    when(storeAddrIn.fire && (!storeAddrIn.bits.isLastRequest || !storeAddrIn.bits.cross4KPage)){
      // the second paddr of cross4KPage request will be write to unalign queue
      dataEntries(stWbIdx).vaddr     := storeAddrIn.bits.vaddr
      dataEntries(stWbIdx).paddrHigh := storeAddrIn.bits.paddr(PAddrBits - 1, PageOffsetWidth)
      // only unit-stride use it, because unit-stride mask is not continue true.
      dataEntries(stWbIdx).byteMask  := Mux(MemorySize.sizeIs(storeAddrIn.bits.size, MemorySize.Q),
        storeAddrIn.bits.mask,
        UIntToMask(MemorySize.CaculateSelectMask(byteStart, byteStart + byteOffset), VLENB))
      dataEntries(stWbIdx).size      := storeAddrIn.bits.size

      // debug singal
      if(debugEn) {
        dataEntries(stWbIdx).debugPaddr.get := storeAddrIn.bits.paddr
      }
    }
    XSError(byteStart + byteOffset < byteStart && storeAddrIn.fire &&
    (!storeAddrIn.bits.isLastRequest || !storeAddrIn.bits.cross4KPage),
     "ByteStart > ByteEnd! at pipeline ${i}\n")
  }

  for (i <- 0 until StorePipelineWidth) {
    val storeDataIn   = io.storeDataIn(i)
    val stWbIdx       = storeDataIn.bits.sqIdx.value
    when(storeDataIn.fire){
      // if it's a cbo.zero, write zero.
      dataEntries(stWbIdx).data  := Mux(storeDataIn.bits.fuOpType === LSUOpType.cbo_zero, 0.U, storeDataIn.bits.data)

      // debug signal
      if(debugEn) {
        dataEntries(stWbIdx).debugVecUnalignedStart.get  := io.storeDataIn(i).bits.vecDebug.get.start
        dataEntries(stWbIdx).debugVecUnalignedOffset.get := io.storeDataIn(i).bits.vecDebug.get.offset
      }
    }
  }

  /************************************************ commit logic ******************************************************/

  /*
  * If store have interrupt, do not to commit !!!!!!!!!
  * At present don't have this situation.
  * if is MMIO/NC/CBO, don't committed.
  * */
  val commitVec = WireDefault(VecInit(Seq.fill(CommitWidth)(false.B))) // default is false.B

  for (i <- 0 until CommitWidth) {
    val ptr = cmtPtrExt(i).value
    val ctrlEntry = ctrlEntries(ptr)
    val dataEntry = dataEntries(ptr)
    val ptrNoRotate = cmtPtrExt(i) >= deqPtrExt.head // prevent pointer rotation
    //TODO:
    /*
    * Currently three commit situation:
    * [1]. normal Scalar Store Commit:      ptrNoRotate && allocated && noFlush && isRobHead && noException && allValid --> move cmtPtr, set committed
    * [2]. activate Vector Store Commit:    ptrNoRotate && allocated && noFlush && isRobHead && noException && allValid && [vecMbcommit] --> move cmtPtr, set committed
    * [3]. inactivate Vector Store Commit:  ptrNoRotate && allocated && noFlush && vecInactive  --> move cmtPtr, set committed
    *
    * Future three commit situation:
    * [1]. normal Scalar Store Commit:      ptrNoRotate && allocated && noFlush && isRobHead && noException && allValid --> move cmtPtr, set committed
    * [2]. activate Vector Store Commit:    ptrNoRotate && allocated && noFlush && isRobHead && noException && allValid --> move cmtPtr, set committed
    * [3]. inactivate Vector Store Commit:  ptrNoRotate && allocated && noFlush && vecInactive  --> move cmtPtr, set committed
    * */
    when(ctrlEntries(ptr).allocated && !needCancel(ptr) &&
      (isNotAfter(dataEntries(ptr).uop.robIdx, GatedRegNext(io.fromRob.pendingPtr)) &&
      !ctrlEntries(ptr).hasException && !ctrlEntries(ptr).waitStoreS2 && (ctrlEntries(ptr).vecMbCommit || !ctrlEntries(ptr).isVec) &&
      ctrlEntries(ptr).allValid || (ctrlEntries(ptr).vecMbCommit && !ctrlEntries(ptr).allValid || ctrlEntries(ptr).vecInactive))) { //TODO: vecMbCommit will be remove in the future
      if(i == 0) {
        commitVec(i)               := true.B
      }
      else {
        commitVec(i)               := commitVec(i - 1)
      }
    } // commitVec default is false.B
    //TODO: vecMbCommit will be remove in the future
    ctrlEntries(ptr).committed   := Mux(ptrNoRotate, commitVec(i), ctrlEntries(ptr).committed)
    XSError(!ctrlEntries(ptr).allocated && ctrlEntries(ptr).committed, "commit not allocated entry!\n")
    XSError(ctrlEntries(ptr).allocated && ctrlEntries(ptr).vecInactive && !ctrlEntries(ptr).isVec, "inactive entry must be vector!\n")
    XSError(ctrlEntries(ptr).allocated && ctrlEntries(ptr).vecMbCommit && !ctrlEntries(ptr).isVec, "vecMbCommit entry must be vector!\n")
  }

  val commitCount = PopCount(commitVec)
  cmtPtrExt       := cmtPtrExt.map(_ + commitCount)

  for (i <- 0 until EnsbufferWidth) {
    val ptr = deqPtrExt(i).value
    when(sqDeqCnt > i.U) {
      ctrlEntries(ptr).committed := false.B
    }
  }

  XSError(cmtPtrExt.head < deqPtrExt.head || cmtPtrExt.head < rdataPtrExt.head, "pointer update error!\n")
  /************************************************* IO Assign ********************************************************/

  io.toLoadQueue.stAddrReadySqPtr := addrReadyPtrExt
  io.toLoadQueue.stDataReadySqPtr := dataReadyPtrExt

  io.toLoadQueue.stDataReadyVec := GatedValidRegNext(stDataReadyVecReg)
  io.toLoadQueue.stAddrReadyVec := GatedValidRegNext(stAddrReadyVecWire)

  io.toLoadQueue.stIssuePtr := enqPtrExt(0)
  // TODO: fix pr #5233 @LWD
  io.sqDeqPtr := deqPtrExt(0)
  io.sqDeqUopIdx := dataEntries(deqPtrExt(0).value).uop.uopIdx
  io.sqDeqRobIdx := dataEntries(deqPtrExt(0).value).uop.robIdx

  // Currently, storeQueue will always safe, no other uncommitted instructions may precede the wfi instruction.
  io.wfi.wfiSafe := true.B
  io.sqEmpty     := deqPtrExt(0) === enqPtrExt(0)
  io.sqCancelCnt := redirectCancelCount
  io.sqDeq       := RegNext(sqDeqCnt)

  /*=============================================== debug ============================================================*/
  if(debugEn) {
    dontTouch(enqNumber)
    dontTouch(lastlastCycleRedirect)
    dontTouch(enqPtrExt)
    dontTouch(deqPtrExt)
    dontTouch(dataEntries)
    dontTouch(commitVec)
    dontTouch(ctrlEntries)
  }

  /************************************************* Difftest *********************************************************/
  // Initialize when unenabled difftest.
  io.diffStore.foreach(_ := DontCare) //TODO: FIX ME!!
  // Consistent with the logic above.
  // Only the vector store difftest required signal is separated from the rtl code.
  val deqCanDoCbo = deqCtrlEntries.head.allValid && !deqCtrlEntries.head.hasException && deqCtrlEntries.head.allocated &&
    deqCtrlEntries.head.isCbo
  if (debugEn) {
    /*=========================================== Data and Mask Generate =============================================*/

    val outData        = Wire(Vec(EnsbufferWidth , UInt(VLEN.W)))
    val outMask        = Wire(Vec(EnsbufferWidth , UInt((VLENB).W)))

    for (i <- 0 until EnsbufferWidth) {
      val selectOffset       = 0.U - dataEntries(i).byteStart // need to generate 0 align data and mask
      val selectData         = (0 until VLENB).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(dataEntries(i).data, j * 8)
      )

      val byteMask           = dataEntries(i).byteMask
      val selectMsk          = (0 until VLENB).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(byteMask, j)
      )

      outData(i)         := ParallelLookUp(selectOffset, selectData)
      outMask(i)         := ParallelLookUp(selectOffset, selectMsk)
    }
//     commit cbo.inval to difftest
    val cmoInvalEvent = DifftestModule(new DiffCMOInvalEvent)
    cmoInvalEvent.coreid := io.hartId
    cmoInvalEvent.valid  := io.writeBack.fire && deqCanDoCbo && isCboInval(deqDataEntries.head.cboType)
    cmoInvalEvent.addr   := deqDataEntries.head.paddr

//     DiffStoreEvent happens when rdataPtr moves.
//     That is, pmsStore enter dataBuffer or ncStore enter Ubuffer
    (0 until EnsbufferWidth).foreach { i =>
      // when i = 0, the sqPtr is rdataPtr(0), which is rdataPtrExt(0), so it applies to NC as well.
      val ptr = rdataPtrExt(i).value
      io.diffStore.foreach{case sink =>
        sink.diffInfo(i).uop            := dataEntries(ptr).debugUop.get
        sink.diffInfo(i).start          := dataEntries(ptr).debugVecUnalignedStart.get
        sink.diffInfo(i).offset         := dataEntries(ptr).debugVecUnalignedOffset.get
        sink.pmaStore(i).valid          := diffPmaStore.get(i).valid
        sink.pmaStore(i).bits           := diffPmaStore.get(i).bits
      }
    }

    io.diffStore.foreach { case sink =>
      sink.ncStore.valid := io.toUncacheBuffer.req.fire && io.toUncacheBuffer.req.bits.nc
      sink.ncStore.bits := io.toUncacheBuffer.req.bits
    }


    (1 until EnsbufferWidth).foreach(i => when(io.writeToSbuffer.req(i).fire) { assert(io.writeToSbuffer.req(i - 1).fire) })
    if (coreParams.dcacheParametersOpt.isEmpty) {
      for (i <- 0 until EnsbufferWidth) {
        val ptr = deqPtrExt(i).value
        val ram = DifftestMem(64L * 1024 * 1024 * 1024, 8)
        val wen = ctrlEntries(ptr).allocated && ctrlEntries(ptr).committed(ptr) && isCacheable(dataEntries(ptr).memoryType)
        val waddr = ((rdataDataEntries(i).paddr - "h80000000".U) >> 3).asUInt
        val wdata = Mux(rdataDataEntries(i).paddr(3), rdataDataEntries(i).data(127, 64), rdataDataEntries(i).data(63, 0))
        val wmask = Mux(rdataDataEntries(i).paddr(3), outMask(i)(15, 8), outMask(i)(7, 0))
        when (wen) {
          ram.write(waddr, wdata.asTypeOf(Vec(8, UInt(8.W))), wmask.asBools)
        }
      }
    }
  }


  /*********************************************** perf event *********************************************************/
  QueuePerf(StoreQueueSize, validCount, !allowEnqueue)
//  val vecValidVec = WireInit(VecInit((0 until StoreQueueSize).map(i => allocated(i) && isVec(i))))
//  QueuePerf(StoreQueueSize, PopCount(vecValidVec), !allowEnqueue)
  io.sqFull := !allowEnqueue
  XSPerfAccumulate("mmioCycle", (mmioBusy)) // lq is busy dealing with uncache req
  XSPerfAccumulate("mmioCnt", io.writeBack.fire && isMmio(rdataDataEntries.head.memoryType))
  XSPerfAccumulate("mmio_wb_success", io.writeBack.fire && isMmio(rdataDataEntries.head.memoryType))
  XSPerfAccumulate("mmio_wb_blocked", (io.writeBack.valid && !io.writeBack.ready && isMmio(rdataDataEntries.head.memoryType)))
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfValidCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val perfEvents = Seq(
    ("mmioCycle      ", WireInit(mmioBusy)),
    ("mmioCnt        ", io.toUncacheBuffer.req.fire && !io.toUncacheBuffer.req.bits.nc),
    ("mmio_wb_success", io.writeBack.fire && isMmio(rdataDataEntries.head.memoryType)),
    ("mmio_wb_blocked", io.writeBack.valid && !io.writeBack.ready && isMmio(rdataDataEntries.head.memoryType)),
    ("stq_1_4_valid  ", (perfValidCount < (StoreQueueSize.U/4.U))),
    ("stq_2_4_valid  ", (perfValidCount > (StoreQueueSize.U/4.U)) & (perfValidCount <= (StoreQueueSize.U/2.U))),
    ("stq_3_4_valid  ", (perfValidCount > (StoreQueueSize.U/2.U)) & (perfValidCount <= (StoreQueueSize.U*3.U/4.U))),
    ("stq_4_4_valid  ", (perfValidCount > (StoreQueueSize.U*3.U/4.U))),
  )
  generatePerfEvent()

}
import top.Generator
object NewStoreQueueMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "storeQueue" :+ "--throw-on-first-error",
    new NewStoreQueue()(defaultConfig),
    firtoolOpts :+ "-O=release" :+ "--disable-annotation-unknown" :+ "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
//  emitVerilog(new NewStoreQueue()(defaultConfig), Array("--target-dir", "build/storeQueue", "--full-stacktrace"))

  println("done")
}
