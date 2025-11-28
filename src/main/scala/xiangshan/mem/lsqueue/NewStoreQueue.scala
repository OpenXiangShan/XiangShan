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
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, UopIdx, connectSamePort}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.FuConfig.StaCfg
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.{DCacheWordReqWithVaddrAndPfFlag, MemoryOpConstants, UncacheWordIO}

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


// don't need to initial
class SQEntryBundle(implicit p: Parameters) extends MemBlockBundle {
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
  val byteStart          = UInt(log2Ceil(VLEN/8 + 1).W)
  val byteEnd            = UInt(log2Ceil(VLEN/8 + 1).W)
  // data storage
  val vaddr                    = UInt(VAddrBits.W)
  val paddrHigh                = UInt((PAddrBits - pageOffset).W) //don't need to storage low 12 bit, which is same as vaddr(11, 0)
  def paddr :UInt              = Cat(paddrHigh, vaddr(pageOffset - 1, 0))
  val data                     = UInt(VLEN.W)
  val wline                    = Bool() // write whole cacheline

  // debug signal
  val debugPaddr               = Option.when(debugEn)(UInt((PAddrBits).W))
  val debugVaddr               = Option.when(debugEn)(UInt((VAddrBits).W))
  val debugData                = Option.when(debugEn)(UInt((XLEN).W))
  // only use for unit-stride difftest
  val debugVecUnalignedStart   = Option.when(debugEn)(UInt((log2Up(XLEN)).W))
  val debugVecUnalignedOffset  = Option.when(debugEn)(UInt((log2Up(XLEN)).W))
  val debugUop                 = Option.when(debugEn)(new DynInst())

}

class SQCtrlEntryBundle(implicit p: Parameters) extends MemBlockBundle {
  val dataValid          = Bool()
  val addrValid          = Bool()
  val memoryType         = MemoryType()
  val waitStoreS2        = Bool() //TODO: will be remove in the feature
  val prefetch           = Bool()
  val isVec              = Bool() // TODO: need it ?
  // vecInactive indicate storage a inactive vector element, it will not write to Sbuffer. written when vector split.
  val vecInactive        = Bool()
  val unaligned          = Bool() // TODO: need it ?
  val cross16Byte        = Bool() // TODO: need it ?
  val hasException       = Bool() // TODO: need it ?
  val committed          = Bool()
  val allocated          = Bool()
  val cboType            = CboType()
  val isCbo              = Bool()
  val vecMbCommit        = Bool() //TODO: request was committed by MergeBuffer, will be remove in the future.

  //debug information

  def allValid: Bool     = dataValid && addrValid
}

class UnalignBufferEntry(implicit p: Parameters) extends MemBlockBundle {
  val paddrHigh          = UInt((PAddrBits - pageOffset).W)
  def paddr :UInt        = Cat(paddrHigh, 0.U(pageOffset.W))
  val robIdx             = new RobPtr
  val sqIdx              = new SqPtr
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

  private class ForwardModule(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val query           = Vec(LoadPipelineWidth, new ForwardQueryIO)
      val dataEntriesIn   = Vec(StoreQueueSize, Input(new SQEntryBundle())) // from storeQueue data
      val ctrlEntriesIn   = Vec(StoreQueueSize, Input(new SQCtrlEntryBundle())) // from storeQueue ctrl info
      val ctrlInfo = new Bundle {
        val deqPtr = Input(new SqPtr())
        val enqPtr = Input(new SqPtr())
      }
    })

    def findYoungest(in: UInt): (UInt, Bool) = {
      val lowHasOne = VecInit(Seq.fill(in.getWidth)(false.B))
      for (i <- 1 until in.getWidth) {
        lowHasOne(i) := lowHasOne(i - 1) | in(i - 1)
      }
      // (one-hot result, has multi match)
      (in & (~lowHasOne.asUInt).asUInt, (in & lowHasOne.asUInt).orR)
    }

    /**
     * load forward query
     *
     * Check store queue for instructions that is older than the load.
     * The response will be valid at the next 2 cycle after req.
     */
    for (i <- 0 until LoadPipelineWidth) {
      /**
       * Stage 0:
       *        1. generate load sqIdx mask
       *        2. generate load byte start and byte end
       *        3. compare byte start and byte end to judge whether have address overlap
       * Stage 1:
       *        1. match paddr and vaddr
       *        2. select youngest entry to forward
       * Stage 2:
       *        select data byte
       * */

      // vector store will consider all inactive || secondInvalid flows as valid
      val addrValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j =>
        io.ctrlEntriesIn(j).addrValid)))
      val dataValidVec = WireInit(VecInit((0 until StoreQueueSize).map(j =>
        io.ctrlEntriesIn(j).dataValid)))
      val allValidVec  = WireInit(VecInit((0 until StoreQueueSize).map(j =>
        io.ctrlEntriesIn(j).allValid)))

      /*================================================== Stage 0 ===================================================*/
      // Compare deqPtr (deqPtr) and forward.sqIdx, we have two cases:
      // (1) if they have the same flag, we need to check range(tail, sqIdx)
      // (2) if they have different flags, we need to check range(tail, VirtualLoadQueueSize) and range(0, sqIdx)
      // Forward1: Mux(same_flag, range(tail, sqIdx), range(tail, VirtualLoadQueueSize))
      // Forward2: Mux(same_flag, 0.U,                   range(0, sqIdx)    )
      // i.e. forward1 is the target entries with the same flag bits and forward2 otherwise

      val s0Valid          = io.query(i).req.lduStage0ToSq.valid
      val deqMask          = UIntToMask(io.ctrlInfo.deqPtr.value, StoreQueueSize)
      val differentFlag    = io.ctrlInfo.deqPtr.flag =/= io.query(i).req.lduStage0ToSq.bits.sqIdx.flag
      val forwardMask      = UIntToMask(io.query(i).req.lduStage0ToSq.bits.sqIdx.value, StoreQueueSize)
      // generate load byte start and end
      val loadStart        = io.query(i).req.lduStage0ToSq.bits.vaddr(log2Ceil(VLEN/8) - 1, 0)
      val byteOffset       = MemorySize.ByteOffset(io.query(i).req.lduStage0ToSq.bits.size)
      val loadEnd          = loadStart + byteOffset

      // mdp mask
      val lfstEnable = Constantin.createRecord("LFSTEnable", LFSTEnable)
      val storeSetHitVec = Mux(lfstEnable,
        WireInit(VecInit((0 until StoreQueueSize).map(j =>
          io.query(i).req.lduStage0ToSq.bits.uop.loadWaitBit &&
            io.dataEntriesIn(j).uop.robIdx === io.query(i).req.lduStage0ToSq.bits.uop.waitForRobIdx))),
        WireInit(VecInit((0 until StoreQueueSize).map(j =>
          io.dataEntriesIn(j).uop.storeSetHit && io.dataEntriesIn(j).uop.ssid === io.query(i).req.lduStage0ToSq.bits.uop.ssid)))
      )

      /**
       *  ageMaskLow:
       *
       *  ageMaskHigh:
       *
       * */
      val ageMaskLow     = deqMask & forwardMask & VecInit(Seq.fill(StoreQueueSize)(differentFlag)).asUInt
      val ageMaskHigh    = (~deqMask).asUInt & (VecInit(Seq.fill(StoreQueueSize)(differentFlag)).asUInt | forwardMask)

      val s1ForwardMask  = RegEnable(forwardMask, s0Valid)
      val s1LoadVaddr    = RegEnable(io.query(i).req.lduStage0ToSq.bits.vaddr(VAddrBits - 1, log2Ceil(VLENB/8)), s0Valid)
      val s1deqMask      = RegEnable(deqMask, s0Valid)
      val s1LoadStart    = RegEnable(loadStart, s0Valid)
      val s1LoadEnd      = RegEnable(loadEnd, s0Valid)

      val s1AgeMaskLow   = RegEnable(ageMaskLow, s0Valid)
      val s1AgeMaskHigh  = RegEnable(ageMaskHigh, s0Valid)
      val s1Kill         = io.query(i).req.lduStage1ToSq.kill
      val s1Valid        = RegNext(s0Valid) && !s1Kill


      /*================================================== Stage 1 ===================================================*/

      val s1QueryPaddr = io.query(i).req.lduStage1ToSq.paddr(PAddrBits - 1, log2Ceil(VLENB/8))
      // prevent X-state
      val vaddrMatchVec  = VecInit(io.dataEntriesIn.zip(io.ctrlEntriesIn).map { case (dataEntry, ctrlEntry) =>
        (dataEntry.vaddr(VAddrBits - 1, log2Ceil(VLENB / 8)) === s1LoadVaddr) && ctrlEntry.addrValid
      }).asUInt

      val s1OverlapMask  = VecInit((0 until StoreQueueSize).map(j =>
        io.dataEntriesIn(j).byteStart <= s1LoadEnd && io.dataEntriesIn(j).byteEnd >= s1LoadStart
      )).asUInt

      XSError(loadEnd < loadStart, "ByteStart > ByteEnd!\n")

      // new select
      /**
       * if canFoewardLow not zero, means High is not youngest
       * */

      val canForwardLow = s1AgeMaskLow & s1OverlapMask & vaddrMatchVec
      val canForwardHigh = s1AgeMaskHigh & s1OverlapMask & VecInit(Seq.fill(StoreQueueSize)(!canForwardLow.orR)).asUInt &
        vaddrMatchVec

      // find youngest entry, which is one-hot
      val (maskLowOH, multiMatchLow)   = findYoungest(canForwardLow)
      val (maskHighOH, multiMatchHigh) = findYoungest(canForwardHigh)
      val selectOH                     = maskLowOH | maskHighOH
      val selectDataEntry              = Mux1H(selectOH, io.dataEntriesIn)
      val dataInvalid                  = !(selectOH & dataValidVec.asUInt).orR
      val multiMatch                   = multiMatchLow | multiMatchHigh

      // select offset generate
      val byteSelectOffset   = s1LoadStart - selectDataEntry.byteStart

      val HasAddrInvalid = ((s1AgeMaskLow | canForwardHigh) & VecInit(addrValidVec.map(!_)).asUInt).orR

      // find youngest addrInvalid store
      val addrInvalidLow     = s1AgeMaskLow & VecInit(addrValidVec.map(!_)).asUInt
      val addrInvalidHigh    = s1AgeMaskHigh & VecInit(addrValidVec.map(!_)).asUInt &
        VecInit(Seq.fill(StoreQueueSize)(!addrInvalidLow.orR)).asUInt

      val (addrInvLowOH, _)   = findYoungest(canForwardLow)
      val (addrInvHighOH, _)  = findYoungest(canForwardHigh)
      val addrInvSelectOH     = addrInvLowOH | addrInvHighOH

      val dataInvalidSqIdx   = Wire(new SqPtr)
      val addrInvalidSqIdx   = Wire(new SqPtr)

      dataInvalidSqIdx.value := OHToUInt(selectOH)
      dataInvalidSqIdx.flag  := Mux(maskLowOH.orR, io.ctrlInfo.enqPtr.flag, io.ctrlInfo.deqPtr.flag)

      addrInvalidSqIdx.value := OHToUInt(addrInvSelectOH)
      addrInvalidSqIdx.flag  := Mux(addrInvLowOH.orR, io.ctrlInfo.enqPtr.flag, io.ctrlInfo.deqPtr.flag)

      val forwardValid       = s1Valid && selectOH.orR // indicate whether forward is valid.
      val s2ByteSelectOffset = RegEnable(byteSelectOffset, s1Valid)
      val s2SelectDataEntry  = RegEnable(selectDataEntry, s1Valid)
      val s2DataInValid      = RegEnable(dataInvalid, s1Valid)
      val s2HasAddrInvalid   = RegEnable(HasAddrInvalid, s1Valid)
      val s2SelectByteMask   = RegEnable(UIntToMask(MemorySize.CaculateSelectMask(selectDataEntry.byteStart, selectDataEntry.byteEnd), VLEN/8), s1Valid)
      val s2LoadMaskEnd      = RegEnable(UIntToMask(MemorySize.CaculateSelectMask(s1LoadStart, s1LoadEnd), VLEN/8), s1Valid)
      val s2DataInvalidSqIdx = RegEnable(dataInvalidSqIdx, s1Valid)
      val s2AddrInvalidSqIdx = RegEnable(addrInvalidSqIdx, s1Valid)
      val s2MultiMatch       = RegEnable(multiMatch, s1Valid)
      val s2LoadPaddr        = RegEnable(s1QueryPaddr, s1Valid)
      val s2FullOverlap      = RegEnable((selectDataEntry.byteEnd >= s1LoadEnd && selectDataEntry.byteStart <= s1LoadStart), s1Valid)
      val s2LoadStart        = RegEnable(s1LoadStart, s1Valid) // TODO: remove in the tuture
      val s2Valid            = RegNext(forwardValid)
      // debug
      val selectCtrlEntry    = Mux1H(selectOH, io.ctrlEntriesIn)
      XSError(selectOH.orR && !selectCtrlEntry.allocated, "forward select a invalid entry!\n")
      /*================================================== Stage 2 ===================================================*/

      //TODO: consumer need to choise whether use paddrNoMatch or not.
      val paddrNoMatch       = (s2SelectDataEntry.paddr(PAddrBits - 1, log2Ceil(VLENB/8)) === s2LoadPaddr) && s2Valid

      val selectData         = (0 until VLEN/8).map(j =>
        j.U -> rotateByteRight(s2SelectDataEntry.data, j * 8)
      )
      val outData            = ParallelLookUp(s2ByteSelectOffset, selectData)

      val selectMask         = (0 until VLEN/8).map(j =>
        j.U -> rotateByteRight(s2SelectByteMask, j)
      )
      val outMask            = ParallelLookUp(s2ByteSelectOffset, selectMask) & s2LoadMaskEnd

      val safeForward        = s2MultiMatch && s2FullOverlap

      //TODO: only use for 128-bit align forward, should revert when other forward source support rotate forward !!!!
      val finalData          = outData << (s2LoadStart * 8.U)
      val finalMask          = outMask << s2LoadStart

      io.query(i).resp.sqToLduStage1 <> DontCare //TODO: need it?
      val s2Resp = io.query(i).resp.sqToLduStage2
//      s2Resp.bits.forwardData.zipWithIndex.map{case (sink, j) =>
//        sink := outData((j + 1) * 8 - 1, j * 8)}
//      s2Resp.bits.forwardMask.zipWithIndex.map{case (sink, j) =>
//        sink := outMask(j) && s2Valid} // TODO: FIX ME, when Resp.valid is false, do not use ByteMask!!
      s2Resp.bits.forwardData.zipWithIndex.map{case (sink, j) =>
        sink := finalData((j + 1) * 8 - 1, j * 8)}
      s2Resp.bits.forwardMask.zipWithIndex.map{case (sink, j) =>
        sink := finalMask(j) && s2Valid && safeForward} // TODO: FIX ME, when Resp.valid is false, do not use ByteMask!!
      s2Resp.bits.dataInvalid      := s2DataInValid || !safeForward
      s2Resp.bits.dataInvalidSqIdx := s2DataInvalidSqIdx
      s2Resp.bits.addrInvalid      := s2HasAddrInvalid || !safeForward
      s2Resp.bits.addrInvalidSqIdx := s2AddrInvalidSqIdx
      s2Resp.bits.forwardInvalid   := !safeForward
      s2Resp.bits.matchInvalid     := paddrNoMatch
      s2Resp.valid                 := s2Valid

      if(debugEn) {
        dontTouch(s1OverlapMask)
        dontTouch(ageMaskLow)
        dontTouch(ageMaskHigh)
        dontTouch(canForwardLow)
        dontTouch(canForwardHigh)
        dontTouch(maskLowOH)
        dontTouch(multiMatchLow)
        dontTouch(maskHighOH)
        dontTouch(multiMatchHigh)
        dontTouch(addrInvLowOH)
        dontTouch(addrInvHighOH)
        dontTouch(selectOH)
        dontTouch(safeForward)
      }
    }
  }

  private class DeqModule(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val headDataEntries = Vec(EnsbufferWidth, Input(new SQEntryBundle))
      val headCtrlEntries = Vec(EnsbufferWidth, Input(new SQCtrlEntryBundle))

      val toUncacheBuffer = new UncacheWordIO
      val toDCache        = new ToCacheIO
      val fromRob         = Input(new FromRobIO)
      val writeToSbuffer  = new SbufferWriteIO
      val writeBack       = DecoupledIO(new MemExuOutput)
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

    private val dataEntries      = io.headDataEntries
    private val ctrlEntries      = io.headCtrlEntries
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
    private val outMask        = Wire(Vec(EnsbufferWidth , UInt((VLEN/8).W)))


    for (i <- 0 until EnsbufferWidth) {
      val selectOffset       = 0.U - dataEntries(i).vaddr(3, 0) // need to generate 0 align data and mask
      val selectData         = (0 until VLEN/8).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(dataEntries(i).data, j * 8)
      )

      val byteMask           = UIntToMask(MemorySize.CaculateSelectMask(dataEntries(i).byteStart, dataEntries(i).byteEnd), VLEN/8)
      val selectMsk          = (0 until VLEN/8).map(j => // generate circular right shift byte data.
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
          cboStateNext := Mux(isCboZero(headCtrlEntry.cboType), CboState.writeZero, CboState.flushSb)
        }
      }
      is(CboState.writeZero) {
        when(io.writeToSbuffer.req.head.fire) {
          cboStateNext := CboState.flushSb
        }
      }
      is(CboState.flushSb) {
        when(io.sbufferCtrl.resp.empty) {
          cboStateNext := Mux(isCboZero(headCtrlEntry.cboType), CboState.writeback, CboState.sendReq)
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
    io.toDCache.req.bits.opcode  := headCtrlEntry.cboType
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

    private val isNC             = isPbmtNC(headCtrlEntry.memoryType)
    private val uncacheCanHandle = !isCacheable(headCtrlEntry.memoryType) &&
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
    io.toUncacheBuffer.req.bits.mask          := Mux(headDataEntry.vaddr(3), outMask.head(VLEN/8 - 1 , 8), outMask.head(7,0))
    io.toUncacheBuffer.req.bits.robIdx        := headDataEntry.uop.robIdx
    io.toUncacheBuffer.req.bits.memBackTypeMM := isNC
    io.toUncacheBuffer.req.bits.nc            := isNC //TODO: remove it, why not use memBackTypeMM ?!
    io.toUncacheBuffer.req.bits.id            := brodenId

    // resp
    io.toUncacheBuffer.resp.ready             := true.B

    //stout
    io.writeBack.valid                        := (uncacheState === UncacheState.writeback) || (cboState === CboState.writeback)
    io.writeBack.bits                         := DontCare // init, TODO: fix it!!!!!!
    connectSamePort(io.writeBack.bits.uop, dataEntries.head.uop)
    io.writeBack.bits.uop.exceptionVec(hardwareError) := hasHardwareError // override
    io.writeBack.bits.uop.flushPipe := cboState === CboState.writeback && !isCboZero(headCtrlEntry.cboType) // cbo need to use it
    // for difftest, ref will skip mmio store
    io.writeBack.bits.debug.isMMIO  := isMmio(ctrlEntries.head.memoryType) || isPbmtIO(ctrlEntries.head.memoryType)

    io.exceptionInfo.valid             := (uncacheState === UncacheState.writeback) || (cboState === CboState.writeback)
    io.exceptionInfo.bits.robIdx       := dataEntries.head.uop.robIdx
    io.exceptionInfo.bits.exceptionVec := ExceptionNO.selectByFu(io.writeBack.bits.uop.exceptionVec, StaCfg)
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
    /*
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

    private val unalignMask       = Wire(Vec(EnsbufferWidth , UInt((VLEN/8).W))) // select active bytes of split request
    private val writeSbufferData  = Wire(Vec(EnsbufferWidth , UInt(VLEN.W)))
    private val writeSbufferMask  = Wire(Vec(EnsbufferWidth , UInt((VLEN/8).W)))
    private val writeSbufferPaddr = Wire(Vec(EnsbufferWidth , UInt(PAddrBits.W)))
    private val writeSbufferVaddr = Wire(Vec(EnsbufferWidth , UInt(VAddrBits.W)))
    private val headCross16B      = headCtrlEntry.cross16Byte
    private val headCrossPage     = headrdataPtr === io.fromUnalignQueue.bits.sqIdx && io.fromUnalignQueue.valid
    private val diffIsHighPart     = Wire(Vec(EnsbufferWidth, Bool())) //only for difftest

    // paddrHigh and vaddrHigh only for cross16Byte split
    private val paddrLow          = Cat(headDataEntry.paddr(headDataEntry.paddr.getWidth - 1, 4), 0.U(4.W))
    private val paddrHigh         = Cat(headDataEntry.paddr(headDataEntry.paddr.getWidth - 1, 4), 0.U(4.W)) + 16.U
    private val vaddrLow          = Cat(headDataEntry.vaddr(headDataEntry.vaddr.getWidth - 1, 4), 0.U(4.W))
    private val vaddrHigh         = Cat(headDataEntry.vaddr(headDataEntry.vaddr.getWidth - 1, 4), 0.U(4.W)) + 16.U

    for (i <- 0 until EnsbufferWidth) {
      unalignMask(i)         := VecInit(Seq.fill(VLEN/8)(false.B)).asUInt >> dataEntries(i).vaddr(3, 0)
      if(i == 0) {
        writeSbufferData(i)  := outData(i)
        writeSbufferMask(i)  := outMask(i) & unalignMask(i)
        writeSbufferPaddr(i) := paddrLow
        writeSbufferVaddr(i) := vaddrLow
        diffIsHighPart(i)    := dataEntries(i).paddr(3)
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
        diffIsHighPart(i)    := Mux(headCross16B, false.B, dataEntries(i).paddr(3)) // if cross 16B, port 1 must low part
      }
      else {
        writeSbufferData(i)  := outData(i)
        writeSbufferMask(i)  := outMask(i)
        writeSbufferPaddr(i) := Cat(dataEntries(i).paddr(dataEntries(i).paddr.getWidth - 1, 4), 0.U(4.W)) //align 128-bit
        writeSbufferVaddr(i) := Cat(dataEntries(i).vaddr(dataEntries(i).vaddr.getWidth - 1, 4), 0.U(4.W)) //align 128-bit
        diffIsHighPart(i)    := dataEntries(i).paddr(3)
      }
    }

    /*---------------------------------------- Write to Sbuffer Interface --------------------------------------------*/
    private val writeSbufferWire = Wire(Vec(EnsbufferWidth, DecoupledIO(new DCacheWordReqWithVaddrAndPfFlag)))
    private val uncacheStall     = Wire(Vec(EnsbufferWidth, Bool()))
    private val unalignStall     = Wire(Vec(EnsbufferWidth, Bool()))
    private val cboStall         = Wire(Vec(EnsbufferWidth, Bool()))
    private val toSbufferValid   = Wire(Vec(EnsbufferWidth, Bool()))
    // cross16B will occupy two write port, so only need to use port 0 fire.
    private val cross16BDeqReg   = RegEnable(headCross16B, writeSbufferWire(0).fire)

    writeSbufferWire             := DontCare //// init , TODO: fix it!!!!!!
    // when deq is MMIO/NC/CMO request, don't need to write sbuffer.
    for (i <- 0 until EnsbufferWidth) {
      val ctrlEntry = ctrlEntries(i)

      if(i == 0) {
        uncacheStall(i) := !isCacheable(ctrlEntry.memoryType)
        cboStall(i)     := ctrlEntry.isCbo
      }
      else {
        uncacheStall(i) := !isCacheable(ctrlEntry.memoryType) || uncacheStall(i - 1)
        cboStall(i)     := ctrlEntry.isCbo || cboStall(i - 1)
      }
    }
    // generate to sbuffer valid
    /*
    * NOTE: [1] only two port of sbuffer is ready, the request of cross16B can write to sbuffer.
    *       [2] canAccept means sbuffer can enter two request at same time.
    */

    for(i <- 0 until EnsbufferWidth) {
      val ctrlEntry = ctrlEntries(i)
      if(i == 0) {
        toSbufferValid(i) := (ctrlEntry.allValid || ctrlEntry.vecInactive) &&
          !ctrlEntry.hasException && !uncacheStall(i) && !cboStall(i) && ctrlEntry.allocated &&
          (!headCross16B || io.writeToSbuffer.canAccept) && !unalignStall(i) && ctrlEntry.committed

        unalignStall(i) := false.B // if first port is unalign, make it can write to sbuffer.
      }
      else if(i == 1) { // override port 1 to write second request of cross16B
        toSbufferValid(i) := (ctrlEntry.allValid || ctrlEntry.vecInactive) &&
          !ctrlEntry.hasException && !uncacheStall(i) && !cboStall(i) && ctrlEntry.allocated && ctrlEntry.committed &&
          toSbufferValid(i - 1) || (headCross16B && toSbufferValid(0)) && !unalignStall(i)

        unalignStall(i) := ctrlEntry.cross16Byte && !headCross16B
      }
      else {
        toSbufferValid(i) := (ctrlEntry.allValid || ctrlEntry.vecInactive) &&
          !ctrlEntry.hasException && !uncacheStall(i) && !cboStall(i) && ctrlEntry.allocated &&
          !unalignStall(i) && ctrlEntry.committed && toSbufferValid(i - 1)

        unalignStall(i) := ctrlEntry.cross16Byte || headCross16B
      }
    }

    for(i <- 0 until EnsbufferWidth) {
      val port      = writeSbufferWire(i)
      val dataEntry = dataEntries(i)
      val ctrlEntry = ctrlEntries(i)
      // if element is inactivate or Mask is all false, vecValid is false, inform sbuffer do not write.
      val vecValid  = ctrlEntry.vecMbCommit && ctrlEntry.allValid && writeSbufferMask(i).orR || ctrlEntry.isVec

      port.bits.data     := writeSbufferData(i)
      port.bits.mask     := writeSbufferMask(i)
      port.bits.addr     := writeSbufferPaddr(i)
      port.bits.vaddr    := writeSbufferVaddr(i)

      port.bits.wline    := dataEntry.wline
      port.bits.prefetch := ctrlEntry.prefetch
      port.bits.cmd      := MemoryOpConstants.M_XWR
      port.bits.vecValid := vecValid // vector used, will be remove in feature.
      port.valid         := toSbufferValid(i)

      XSError(ctrlEntry.vecInactive && !ctrlEntry.isVec, s"inactive element must be vector! ${i}")
    }

    io.writeToSbuffer.req.zip(writeSbufferWire).zipWithIndex.map{case ((sink, source), i) =>
      NewPipelineConnect(
        source, sink, sink.fire,
        false.B,
        Option(s"SQ2SBPipelineConnect${i}")
      )
    }

    /*============================================ deqPtr generate ===================================================*/
    /*
    * NOTE: Only when port 0 and port 1 are ready can write cross16B request, so only use io.writeToSbuffer.req.head.fire
    *       to caculate sbufferFireNum.
    * deqPtr will move when request write to sbuffer.
    * rdataPtr will move when nc request fire / write to SQ2SBPipelineConnect_i
    * NOTE: when deq mmio/cbo, rdataPtr === deqPtr, because mmio/cbo need to execute at head of StoreQueue.
    * */
    private val sbufferFireNum = Mux(cross16BDeqReg,
      Cat(io.writeToSbuffer.req.head.fire, 0.U),
      Cat(io.writeToSbuffer.req.map(_.fire)))

    val deqCount = Cat(sbufferFireNum, io.writeBack.fire) // timing is ok ?

    io.sqDeqCnt := PopCount(VecInit(deqCount).asUInt)
    io.deqPtrExtNext := io.deqPtrExt.map(_ + io.sqDeqCnt)

    private val pipelineConnectFireNum = Mux(headCross16B,
      Cat(writeSbufferWire.head.fire, 0.U),
      Cat(writeSbufferWire.map(_.fire)))
    // nc/mmio/cbo deq
    private val otherMove        = uncacheState === UncacheState.sendReq && io.toUncacheBuffer.req.fire && isNC ||
      io.writeBack.fire
    private val rdataMoveCnt = Cat(pipelineConnectFireNum, otherMove)

    io.rdataPtrMoveCnt        := PopCount(rdataMoveCnt)

    /*============================================ other connection ==================================================*/
    io.perfMmioBusy := uncacheState =/= UncacheState.idle

    // [NOTE]: low 4 bit of addr/vaddr will be omitted in the sbuffer, but it will be used for difftest.
    for(i <- 0 until EnsbufferWidth){
      io.pmaStore.foreach { case sink =>
        sink(i).valid          := writeSbufferWire(i).valid
        sink(i).bits.addr      := writeSbufferWire(i).bits.addr
        sink(i).bits.data      := writeSbufferWire(i).bits.data
        sink(i).bits.mask      := writeSbufferWire(i).bits.mask
        sink(i).bits.wline     := writeSbufferWire(i).bits.wline
        sink(i).bits.vecValid  := writeSbufferWire(i).bits.vecValid
        sink(i).bits.diffIsHighPart := diffIsHighPart(i)  // indicate whether valid data in high 64-bit, only for scalar store event!
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
    }
  }
  /*==================================================================================================================*/
  /* UnalignQueue will save the second physical address of the oldest SQUnalignQueueSize crossPage unaligned requests.*/
  private class UnalignQueue(implicit p: Parameters) extends LSQModule {
    val io = IO(new Bundle {
      val redirect       = Flipped(ValidIO(new Redirect))
      val fromStaS1      = Vec(StorePipelineWidth, Flipped(DecoupledIO(new UnalignQueueIO)))
      val fromSQ = new Bundle {
        val addrReadyPtr = Input(new SqPtr)
      }
      val toDeqModule = ValidIO(new Bundle {
        val paddr        = UInt(PAddrBits.W)
        val sqIdx        = new SqPtr
      })
    })
    private val enqWidth: Int  = io.fromStaS1.length
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
    private val canEnq     = io.fromStaS1.map{case port =>
      port.fire && port.bits.isLastRequest && port.bits.cross4KPage
    } // one-hot, only second request of the unaligned need to enter.
    private val doEnq      = canEnq.reduce(_ || _)
    private val doEnqReq   = Mux1H(canEnq, io.fromStaS1.map(_.bits))

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

    XSError(enqPtr < deqPtr, s"Something wrong in UnalignQueue!")
    // connection
    io.toDeqModule.bits.paddr := headEntry.paddr
    io.toDeqModule.bits.sqIdx := headEntry.sqIdx
    io.toDeqModule.valid      := !empty

    io.fromStaS1.map{case sink =>
      sink.ready := !full && io.fromSQ.addrReadyPtr === sink.bits.sqIdx && !io.redirect.valid
    }

  }

  val io = IO(new StoreQueueIO)
  println("StoreQueue: size:" + StoreQueueSize)

  // entries define
  val dataEntries        = Reg(Vec(StoreQueueSize, new SQEntryBundle())) // no need to reset
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
  val headDataEntries    = rdataPtrExt.map{ case ptr =>
    dataEntries(ptr.value)
  }
  val headCtrlEntries    = rdataPtrExt.map{ case ptr =>
    ctrlEntries(ptr.value)
  }

  /*========================================== Module define & connection ============================================*/
  // use `private` to limit module connection within this module.

  private val forwardModule         = Module(new ForwardModule)
  private val deqModule             = Module(new DeqModule)
  private val unalignQueue          = Module(new UnalignQueue)

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
  deqModule.io.headCtrlEntries.zip(headCtrlEntries).foreach{ case (sink, source) =>
    sink := source
  }
  deqModule.io.headDataEntries.zip(headDataEntries).foreach{ case (sink, source) =>
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
  unalignQueue.io.fromStaS1.zip(io.fromStoreUnit.unalignQueueReq).map{case (sink, source) =>
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

    when (entryCanEnq) {
      connectSamePort(dataEntries(i).uop, selectBits.uop)
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

//    when(entryCanEnq) {
//      ctrlEntries(i).isVec       :=  FuType.isVStore(selectBits.info.fuType)
//    }// don't need to unset for low power

    XSError(ctrlEntries(i).allocated && entryCanEnq, s"entry double allocate! index: ${i}\n")

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
    val unalignSet     = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      port.bits.isUnsalign && staValidSetVec(j)
    }.reduce(_ || _)
    val cross16ByteSet = io.fromStoreUnit.storeAddrIn.zipWithIndex.map { case (port, j) =>
      port.bits.isUnsalign && !port.bits.unalignWith16Byte && staValidSetVec(j)
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
      ctrlEntries(i).unaligned    := unalignSet
      ctrlEntries(i).cross16Byte  := cross16ByteSet
      ctrlEntries(i).isCbo        := isCboSet
    } // don't need to set false for low power, it will be set every instruction.

    io.fromStoreUnit.storeAddrIn.zipWithIndex.map{case (port, j) =>
      val index         = port.bits.uop.sqIdx.value
      val setValid      = cboSetVec(j)
      when(setValid) {
        ctrlEntries(i).cboType   := Mux1H(List(
          isCboClean(port.bits.uop.fuOpType(1, 0)) -> CboType.clean, // TODO: don't use (1, 0)
          isCboFlush(port.bits.uop.fuOpType(1, 0)) -> CboType.flush,
          isCboInval(port.bits.uop.fuOpType(1, 0)) -> CboType.inval,
          isCboZero(port.bits.uop.fuOpType(1, 0))  -> CboType.zero
        ))
      }
    }

    XSError(ctrlEntries(i).addrValid && staSetValid, s"[addrValid] double allocate! index: ${i}\n")

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
      ctrlEntries(i).prefetch := prefetchSet
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
      ctrlEntries(i).memoryType := Cat(mmioSet, ncSet || !memBackTypeSet)
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

    XSError(ctrlEntries(i).dataValid && dataValidSet, s"[dataValid] double allocate! index: ${i}\n")
    XSError(!ctrlEntries(i).allocated && deqCancel, s"double deq! index: ${i}\n")

    /*================================================================================================================*/
    /*============================================== vector ctrl =====================================================*/
    /*================================================================================================================*/

    when(entryCanEnq) {
      ctrlEntries(i).isVec := true.B
    }.elsewhen(deqCancel || needCancel(i)) {
      ctrlEntries(i).isVec := false.B
    }

    // TODO: vecMbCommit will be remove in the future.
    val fbk = io.fromVMergeBuffer
    for (j <- 0 until VecStorePipelineWidth) {
      vecCommittmp(i)(j) := fbk(j).valid && (fbk(j).bits.isCommit || fbk(j).bits.isFlush) &&
        dataEntries(i).uop.robIdx === fbk(j).bits.robidx && dataEntries(i).uop.uopIdx === fbk(j).bits.uopidx &&
        ctrlEntries(i).allocated
    }
    vecCommit(i) := vecCommittmp(i).reduce(_ || _)

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
      (ctrlEntries(ptr.value).addrValid &&
        (isMmio(ctrlEntries(ptr.value).memoryType) || ctrlEntries(ptr.value).dataValid) || ctrlEntries(ptr.value).vecMbCommit) &&
      !ctrlEntries(ptr.value).unaligned && ctrlEntries(ptr.value).allocated &&
      ptr =/= enqPtrExt(0)
  )
  val nextDataReadyPtr = dataReadyPtrExt + PriorityEncoder(VecInit(dataReadyLookup.map(!_) :+ true.B))
  dataReadyPtrExt := nextDataReadyPtr

  // move unalign ptr
  val deqGroupHasUnalign = deqPtrExt.map { case ptr => ctrlEntries(ptr.value).unaligned }.reduce(_|_)
  val dataPtrInDeqGroupRangeVec = VecInit(deqPtrExt.zipWithIndex.map { case (ptr, i) =>
    dataReadyPtrExt === ptr && sqDeqCnt > i.U
  })
  val unalignedCanMove = deqGroupHasUnalign && dataPtrInDeqGroupRangeVec.asUInt.orR
  when (unalignedCanMove) {
    val step = sqDeqCnt - PriorityEncoder(dataPtrInDeqGroupRangeVec)
    dataReadyPtrExt := dataReadyPtrExt + step
  }

  val stDataReadyVecReg = Wire(Vec(StoreQueueSize, Bool()))
  (0 until StoreQueueSize).map(i => {
    stDataReadyVecReg(i) := (ctrlEntries(i).addrValid &&
        (isMmio(ctrlEntries(i).memoryType) || ctrlEntries(i).dataValid) || (ctrlEntries(i).isVec && ctrlEntries(i).vecMbCommit)) &&
      !ctrlEntries(i).unaligned && ctrlEntries(i).allocated
  })

  // deqPtr logic
  deqPtrExt := deqPtrExt.map(_ + sqDeqCnt)
  rdataPtrExt := rdataPtrExt.map(_ + rdataMoveCnt)

  XSError(deqPtrExt(0) > rdataPtrExt(0), "Why deqPtr > rdataPtr? something error!")
  XSError(deqPtrExt(0) > enqPtrExt(0),   "Why deqPtr > enqPtr? something error!")
  /******************************************** store pipeline write **************************************************/
  for (i <- 0 until StorePipelineWidth) {
    val storeAddrIn   = io.fromStoreUnit.storeAddrIn(i)
    val storeAddrInRe = io.fromStoreUnit.storeAddrInRe(i)
    val stWbIdx       = storeAddrIn.bits.uop.sqIdx.value
    val byteStart     = storeAddrIn.bits.vaddr(log2Ceil(VLEN/8) - 1, 0)
    val byteOffset    = MemorySize.ByteOffset(storeAddrIn.bits.size)
    when(storeAddrIn.fire && (!storeAddrIn.bits.isLastRequest || !storeAddrIn.bits.cross4KPage)){
      // the second paddr of cross4KPage request will be write to unalign queue
      dataEntries(stWbIdx).vaddr     := storeAddrIn.bits.vaddr
      dataEntries(stWbIdx).paddrHigh := storeAddrIn.bits.paddr(PAddrBits - 1, PageOffsetWidth)
      dataEntries(stWbIdx).byteStart := byteStart
      dataEntries(stWbIdx).byteEnd   := byteStart + byteOffset

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
      dataEntries(stWbIdx).wline := storeDataIn.bits.fuOpType === LSUOpType.cbo_zero

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
    when(isNotAfter(dataEntries(ptr).uop.robIdx, GatedRegNext(io.fromRob.pendingPtr)) && ctrlEntries(ptr).allocated &&
      !needCancel(ptr) && !ctrlEntries(ptr).hasException && !ctrlEntries(ptr).waitStoreS2 && ctrlEntries(ptr).allValid) {
      if(i == 0) {
        commitVec(i)               := true.B
      }
      else {
        commitVec(i)               := commitVec(i - 1)
      }
    } // commitVec default is false.B
    ctrlEntries(ptr).committed   := commitVec(i)
    XSError(!ctrlEntries(ptr).allocated && ctrlEntries(ptr).committed, "commit not allocated entry!\n")
  }

  val commitCount = PopCount(commitVec)
  cmtPtrExt       := cmtPtrExt.map(_ + commitCount)

  for (i <- 0 until EnsbufferWidth) {
    val ptr = deqPtrExt(i).value
    when(sqDeqCnt > i.U) {
      ctrlEntries(ptr).committed := false.B
    }
  }

  /************************************************* IO Assign ********************************************************/

  io.toLoadQueue.stAddrReadySqPtr := addrReadyPtrExt
  io.toLoadQueue.stDataReadySqPtr := dataReadyPtrExt

  io.toLoadQueue.stDataReadyVec := GatedValidRegNext(stDataReadyVecReg)
  io.toLoadQueue.stAddrReadyVec := GatedValidRegNext(stAddrReadyVecWire)

  io.toLoadQueue.stIssuePtr := enqPtrExt(0)
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
  val deqCtrlEntries = deqPtrExt.map(x => ctrlEntries(x.value))
  val deqDataEntries = deqPtrExt.map(x => dataEntries(x.value))
  val deqCanDoCbo = deqCtrlEntries.head.allValid && !deqCtrlEntries.head.hasException && deqCtrlEntries.head.allocated &&
    deqCtrlEntries.head.isCbo
  if (debugEn) {
    /*=========================================== Data and Mask Generate =============================================*/

    val outData        = Wire(Vec(EnsbufferWidth , UInt(VLEN.W)))
    val outMask        = Wire(Vec(EnsbufferWidth , UInt((VLEN/8).W)))

    for (i <- 0 until EnsbufferWidth) {
      val selectOffset       = 0.U - dataEntries(i).byteStart // need to generate 0 align data and mask
      val selectData         = (0 until VLEN/8).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(dataEntries(i).data, j * 8)
      )

      val byteMask           = UIntToMask(MemorySize.CaculateSelectMask(dataEntries(i).byteStart, dataEntries(i).byteEnd), VLEN/8)
      val selectMsk          = (0 until VLEN/8).map(j => // generate circular right shift byte data.
        j.U -> rotateByteRight(byteMask, j)
      )

      outData(i)         := ParallelLookUp(selectOffset, selectData)
      outMask(i)         := ParallelLookUp(selectOffset, selectMsk)
    }
//     commit cbo.inval to difftest
    val cmoInvalEvent = DifftestModule(new DiffCMOInvalEvent)
    cmoInvalEvent.coreid := io.hartId
    cmoInvalEvent.valid  := io.writeBack.fire && deqCanDoCbo && isCboInval(deqCtrlEntries.head.cboType)
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
        val wen = ctrlEntries(ptr).allocated && ctrlEntries(ptr).committed(ptr) && isCacheable(ctrlEntries(ptr).memoryType)
        val waddr = ((headDataEntries(i).paddr - "h80000000".U) >> 3).asUInt
        val wdata = Mux(headDataEntries(i).paddr(3), headDataEntries(i).data(127, 64), headDataEntries(i).data(63, 0))
        val wmask = Mux(headDataEntries(i).paddr(3), outMask(i)(15, 8), outMask(i)(7, 0))
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
  XSPerfAccumulate("mmioCnt", io.writeBack.fire && isMmio(headCtrlEntries.head.memoryType))
  XSPerfAccumulate("mmio_wb_success", io.writeBack.fire && isMmio(headCtrlEntries.head.memoryType))
  XSPerfAccumulate("mmio_wb_blocked", (io.writeBack.valid && !io.writeBack.ready && isMmio(headCtrlEntries.head.memoryType)))
  XSPerfAccumulate("validEntryCnt", distanceBetween(enqPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("cmtEntryCnt", distanceBetween(cmtPtrExt(0), deqPtrExt(0)))
  XSPerfAccumulate("nCmtEntryCnt", distanceBetween(enqPtrExt(0), cmtPtrExt(0)))

  val perfValidCount = distanceBetween(enqPtrExt(0), deqPtrExt(0))
  val perfEvents = Seq(
    ("mmioCycle      ", WireInit(mmioBusy)),
    ("mmioCnt        ", io.toUncacheBuffer.req.fire && !io.toUncacheBuffer.req.bits.nc),
    ("mmio_wb_success", io.writeBack.fire && isMmio(headCtrlEntries.head.memoryType)),
    ("mmio_wb_blocked", io.writeBack.valid && !io.writeBack.ready && isMmio(headCtrlEntries.head.memoryType)),
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
