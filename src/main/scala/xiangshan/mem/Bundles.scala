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

package xiangshan.mem


import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.Bundles._
import xiangshan.mem.prefetch.PrefetchReqBundle
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._
import math._

object Bundles {

  class LsPipelineBundle(implicit p: Parameters) extends XSBundle
    with HasDCacheParameters
    with HasVLSUParameters
    with HasTlbConst {
    val uop = new DynInst
    val vaddr = UInt(VAddrBits.W)
    val fullva = UInt(XLEN.W)
    val vaNeedExt = Bool()
    val paddr = UInt(PAddrBits.W)
    val gpaddr = UInt(XLEN.W)
    val mask = UInt((VLEN/8).W)
    val data = UInt((VLEN+1).W)
    val wlineflag = Bool() // store write the whole cache line
    val miss = Bool()
    val tlbMiss = Bool()
    val ptwBack = Bool()
    val af = Bool()
    val nc = Bool()
    val ncWithData = Bool()
    val mmio = Bool()
    val atomic = Bool()
    val memBackTypeMM = Bool() // 1: main memory, 0: IO
    val hasException = Bool()
    val isHyper = Bool()
    val isForVSnonLeafPTE = Bool()
    val isPrefetch = Bool()
    val isHWPrefetch = Bool()
    val isStore = Bool()
    val src = Vec(3, UInt(XLEN.W))
    val iqIdx = UInt(log2Up(MemIQSizeMax).W)

    // vector
    val isVector = Bool()
    val isLastElem = Bool()
    val is128Bits = Bool()
    val isFirstEle = Bool()
    val isFinalSplit = Bool()
    val uopUnitStrideFof = Bool()
    val usSecondInv = Bool()
    val elemIdx = UInt(elemIdxBits.W)
    val alignType = UInt(alignTypeBits.W)
    val mbIdx = UInt(max(vlmBindexBits, vsmBindexBits).W)
    val regOffset = UInt(vOffsetBits.W)
    val elemIdxInsideVd = UInt(elemIdxBits.W)
    val vecBaseVaddr = UInt(VAddrBits.W)
    val vecVaddrOffset = UInt(VAddrBits.W)
    val vecTriggerMask = UInt((VLEN/8).W)
    val vecActive = Bool() // 1: vector active element or scala mem operation, 0: vector not active element
    val flowNum = NumLsElem()
    // val flowPtr             = new VlflowPtr() // VLFlowQueue ptr
    // val sflowPtr            = new VsFlowPtr() // VSFlowQueue ptr
    // val rob_idx_valid       = Vec(2,Bool())
    // val inner_idx           = Vec(2,UInt(3.W))
    // val rob_idx             = Vec(2,new RobPtr)
    // val offset              = Vec(2,UInt(4.W))

    // replay
    val isLoadReplay = Bool()
    val isFastPath = Bool()
    val isFastReplay = Bool()
    val isFirstIssue = Bool()
    val replayCarry = new ReplayCarry(nWays)
    val hasROBEntry = Bool()
    val replacementUpdated  = Bool()
    val missDbUpdated = Bool()
    val ldCancel = Bool()
    val lateKill = Bool()
    val feedbacked = Bool()
    val schedIndex = UInt(log2Up(LoadQueueReplaySize).W)
    val tlbNoQuery = Bool()
    val lastBeat = Bool()
    val mshrHandled = Bool()
    val mshrId = UInt(log2Up(cfg.nMissEntries).W)
    val tlbHandled = Bool()
    val tlbId = UInt(log2Up(loadfiltersize).W)
    val dataInvalidSqIdx = new SqPtr
    val addrInvalidSqIdx = new SqPtr
    val fullForward = Bool()
    val forwardTLDchannel = Bool()
    val schedIdx = UInt(log2Up(LoadQueueReplaySize).W)
    val cause = ReplayCauseNO()

    // misalign
    val isMisAlignBuf = Bool()
    val misalign = Bool()
    val misalignWith16Byte = Bool()
    val misalignNeedWakeUp = Bool()
    val updateAddrValid = Bool()

    // pfSource
    val pfSource = new L1PrefetchSource
    val confidence = UInt(1.W)

    def isSWPrefetch: Bool = isPrefetch && !isHWPrefetch

    def isPrefetchI: Bool = uop.fuOpType === LSUOpType.prefetch_i

    def isPrefetchRead: Bool = uop.fuOpType === LSUOpType.prefetch_r

    def isPrefetchWrite: Bool = uop.fuOpType === LSUOpType.prefetch_w

    def isLoad: Bool = !isStore

    def needReplay: Bool = cause.asUInt.orR

    def fromMemExuInputBundle(input: MemExuInput, isStore: Boolean = false) = {
      this := 0.U.asTypeOf(this)
      connectSamePort(this, input)
      this.isStore := isStore.B
      input.flowNum.foreach { this.flowNum := _}
    }

    def toMemExuInputBundle(): MemExuInput = {
      val res = WireInit(0.U.asTypeOf(new MemExuInput))
      connectSamePort(res, this)
      res
    }

    def fromMemExuOutputBundle(input: MemExuOutput) = {
      this := 0.U.asTypeOf(this)
      connectSamePort(this, input)
      this.vaddr := input.debug.vaddr
      this.paddr := input.debug.paddr
      this.mmio := input.debug.isMMIO
      this.isStore := !input.isFromLoadUnit
      input.mask.foreach { this.mask := _ }
    }

    def toMemExuOutputBundle(isVector: Boolean = false): MemExuOutput = {
      val res = Wire(new MemExuOutput(isVector = isVector))
      connectSamePort(res, this)
      res.isFromLoadUnit := this.isLoad
      res.debug.isMMIO := this.mmio
      res.debug.isNC := this.nc
      res.debug.vaddr := this.vaddr
      res.debug.paddr := this.paddr
      res.debug.isPerfCnt := false.B
      res
    }

    def fromVecPipeBundle(input: VecPipeBundle, isVStore: Boolean = false) = {
      this := 0.U.asTypeOf(this)
      connectSamePort(this, input)
      this.vecBaseVaddr := input.basevaddr
      this.isVector := true.B
      this.isStore := isVStore.B
      this.uopUnitStrideFof := input.uop_unit_stride_fof
      this.regOffset := input.reg_offset
      this.alignType := input.alignedType
      this.isFirstEle := input.is_first_ele
      this.usSecondInv := input.usSecondInv
      this.mbIdx := input.mBIndex
    }

    def toVecPipelineFeedbackBundle(isVStore: Boolean = false): VecPipelineFeedbackIO = {
      val res = Wire(new VecPipelineFeedbackIO(isVStore = isVStore))
      res.mBIndex := this.mbIdx
      res.hit := this.feedbacked && this.isVector
      res.isvec := true.B
      res.flushState := DontCare
      res.sourceType := Mux(isVStore.B, RSFeedbackType.tlbMiss, RSFeedbackType.lrqFull)
      res.trigger := this.uop.trigger
      res.nc := this.nc
      res.mmio := this.mmio
      res.exceptionVec := this.uop.exceptionVec
      res.usSecondInv := this.usSecondInv
      res.vecFeedback := this.feedbacked && this.isVector
      res.elemIdx := this.elemIdx
      res.alignedType := this.alignType
      res.mask := this.mask
      res.vaddr := this.vaddr
      res.vaNeedExt := this.vaNeedExt
      res.gpaddr := this.gpaddr
      res.isForVSnonLeafPTE := this.isForVSnonLeafPTE
      res.hasException := this.hasException
      res.vstart := this.uop.vpu.vstart
      res.vecTriggerMask := this.vecTriggerMask
      res.reg_offset.foreach { _ := this.regOffset }
      res.elemIdxInsideVd.foreach { _ := this.elemIdxInsideVd }
      res.vecdata.foreach { _ := this.data }
      res
    }

    def toVecMemOutputBundle(): VecMemExuOutput = {
      val res = Wire(new VecMemExuOutput(isVector = true))
      res.output := this.toMemExuOutputBundle(isVector = true)
      res.vecFeedback := this.feedbacked && this.isVector
      res.nc := this.nc
      res.mmio := this.mmio
      res.usSecondInv := this.usSecondInv
      res.elemIdx := this.elemIdx
      res.alignedType := this.alignType
      res.mbIndex := this.mbIdx
      res.mask := this.mask
      res.vaddr := this.vaddr
      res.gpaddr := this.gpaddr
      res.vaNeedExt := this.vaNeedExt
      res.isForVSnonLeafPTE := this.isForVSnonLeafPTE
      res.vecTriggerMask := this.vecTriggerMask
      res
    }

    def fromStorePrefetchReqBundle(input: StorePrefetchReq) = {
      this := 0.U.asTypeOf(this)
      this.paddr := input.paddr
      this.vaddr := input.vaddr
      this.isStore := true.B
      this.isHWPrefetch := true.B
    }

    def toStorePrefetchReqBundle(): StorePrefetchReq = {
      val res = Wire(new StorePrefetchReq)
      res.paddr := this.paddr
      res.vaddr := this.vaddr
      res
    }

    def fromL1PrefetchReqBundle(input: L1PrefetchReq) = {
      this := 0.U.asTypeOf(this)
      this.paddr := input.paddr
      this.vaddr := input.getVaddr()
      this.isStore := input.is_store
      this.pfSource := input.pf_source
      this.confidence := input.confidence
      this.isHWPrefetch := true.B
    }

    def toL1PrefetchReqBundle(): L1PrefetchReq = {
      val res = Wire(new L1PrefetchReq)
      res.paddr := this.paddr
      res.alias := this.paddr(13, 12)
      res.is_store := this.isStore
      res.pf_source := this.pfSource
      res.confidence := this.confidence
      res
    }

    def toLoadForwardReqBundle(): LoadForwardReqBundle = {
      val res = Wire(new LoadForwardReqBundle)
      connectSamePort(res, this)
      res.uop.pc := this.uop.pc
      res.sqIdx := this.uop.sqIdx
      res.sqIdxMask := DontCare
      res
    }

    def toMissQueueForwardReqBundle(): MissQueueForwardReqBundle = {
      val res = Wire(new MissQueueForwardReqBundle)
      res.paddr := this.paddr
      res.mshrId := this.mshrId
      res
    }

    def toRSFeedbackBundle(): RSFeedback = {
      val res = WireInit(0.U.asTypeOf(new RSFeedback))
      res.flushState := this.ptwBack
      res.robIdx := this.uop.robIdx
      res.sqIdx := this.uop.sqIdx
      res.lqIdx := this.uop.lqIdx
      res
    }

    def toRedirectBundle(flushLevel: UInt): Redirect = {
      val res = WireInit(0.U.asTypeOf(new Redirect))
      res.isRVC  := this.uop.preDecodeInfo.isRVC
      res.robIdx := this.uop.robIdx
      res.ftqIdx := this.uop.ftqPtr
      res.ftqOffset := this.uop.ftqOffset
      res.level := flushLevel
      res.cfiUpdate.target  := this.uop.pc
      res.debug_runahead_checkpoint_id := this.uop.debugInfo.runahead_checkpoint_id
      res
    }
  }

 class LsPrefetchTrainBundle(implicit p: Parameters) extends LsPipelineBundle {
    val metaPrefetch = UInt(L1PfSourceBits.W)
    val metaAccess = Bool()

    def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
      val inputReg = latch match {
        case true   => RegEnable(input, enable)
        case false  => input
      }
      connectSamePort(this, inputReg)
      this.metaPrefetch := DontCare
      this.metaAccess := DontCare
    }

    def toPrefetchReqBundle(): PrefetchReqBundle = {
      val res = Wire(new PrefetchReqBundle)
      connectSamePort(res, this)
      res.pc := this.uop.pc
      res.pfHitStream := isFromStream(this.metaPrefetch)
      res
    }
  }

  class LsPrefetchTrainIO(implicit p: Parameters) extends XSBundle {
    val req = ValidIO(new LsPrefetchTrainBundle)
    val canAcceptLowConfPrefetch  = Output(Bool())
    val canAcceptHighConfPrefetch = Output(Bool())
    val s1PrefetchSpec = Output(Bool())
    val s2PrefetchSpec = Output(Bool())
  }

  class LoadForwardReqBundle(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst
    val vaddr = UInt(VAddrBits.W)
    val paddr = UInt(PAddrBits.W)
    val mask = UInt((VLEN/8).W)
    val sqIdx = new SqPtr
    val sqIdxMask = UInt(StoreQueueSize.W)
    val nc = Bool()
  }

  class LoadForwardRespBundle(implicit p: Parameters) extends XSBundle {
    val forwardMaskFast = Vec((VLEN/8), Bool())
    val forwardMask = Vec((VLEN/8), Bool())
    val forwardData = Vec((VLEN/8), UInt(8.W))
    // dataInvalid suggests store to load forward found forward should happen,
    // but data is not available for now. If dataInvalid, load inst should
    // be replayed from Iq. Feedback type should be RSFeedbackType.dataInvalid
    val dataInvalid = Bool()
    val dataInvalidFast = Bool()
    val dataInvalidSqIdx = new SqPtr
    // matchInvalid suggests in store to load forward logic, paddr cam result does
    // to equal to vaddr cam result. If matchInvalid, a microarchitectural exception
    // should be raised to flush SQ and committed sbuffer.
    val matchInvalid = Bool()
    // addrInvalid suggests store to load forward found forward should happen,
    // but address (SSID) is not available for now. If addrInvalid, load inst should
    // be replayed from Iq. Feedback type should be RSFeedbackType.addrInvalid
    val addrInvalid = Bool()
    val addrInvalidSqIdx  = new SqPtr
  }

  class LoadForwardIO(implicit p: Parameters) extends XSBundle {
    val req = Valid(new LoadForwardReqBundle)
    val resp = Input(new LoadForwardRespBundle)
  }

  // Query load queue for ld-ld violation
  // Req should be send in load_s1
  // Resp will be generated 1 cycle later
  // Note that query req may be !ready, as dcache is releasing a block
  // If it happens, a replay from rs is needed.
  class LoadNukeQueryReqBundle(implicit p: Parameters) extends XSBundle { // provide lqIdx
    val uop = new DynInst
    val mask = UInt((VLEN/8).W)
    val paddr = UInt(PAddrBits.W)
    val dataValid = Bool()
    val nc = Bool()
  }

  class LoadNukeQueryReqIO(implicit p: Parameters) extends XSBundle {
    val req = Decoupled(new LoadNukeQueryReqBundle)
    val revoke = Output(Bool())
  }

  class LoadNukeQueryRespBundle(implicit p: Parameters) extends XSBundle {
    // repayInst: ld-ld violation check success, replay from fetch.
    val replayInst = Bool()
  }

  class LoadNukeQueryIO(implicit p: Parameters) extends XSBundle {
    val req = Decoupled(new LoadNukeQueryReqBundle)
    val resp = Flipped(Valid(new LoadNukeQueryRespBundle))
    val revoke = Output(Bool())
  }

  class StoreNukeQueryBundle(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst
    val paddr = UInt(PAddrBits.W)
    val mask = UInt((VLEN/8).W)
    val matchLine = Bool()
  }

  class StoreMaBufToSqCtrlControlBundle(implicit p: Parameters) extends XSBundle {
    // This entry is a cross page
    val crossPageWithHit = Bool()
    val crossPageCanDeq = Bool()
    // High page Paddr
    val paddr = UInt(PAddrBits.W)
    val withSameUop = Bool()
  }
  class StoreMaBufToSqCtrlStoreInfoBundle(implicit p: Parameters) extends XSBundle {
    val uop = new DynInst()
    val doDeq = Bool()
  }

  class StoreMaBufToSqControlIO(implicit p: Parameters) extends XSBundle {
    // from storeMisalignBuffer to storeQueue, control it's sbuffer write
    val toStoreQueue = Output(new StoreMaBufToSqCtrlControlBundle)
    // from storeQueue to storeMisalignBuffer, provide detail info of this store
    val toStoreMisalignBuffer = Input(new StoreMaBufToSqCtrlStoreInfoBundle)
  }

  class StoreMaBufToVecStoreMergeBufferIO(implicit p: Parameters)  extends VLSUBundle{
    val mbIndex = Output(UInt(vsmBindexBits.W))
    val flush = Output(Bool())
  }

  // Store byte valid mask write bundle
  //
  // Store byte valid mask write to SQ takes 2 cycles
  class StoreMaskBundle(implicit p: Parameters) extends XSBundle {
    val sqIdx = new SqPtr
    val mask = UInt((VLEN/8).W)
  }

  class LoadDataFromDcacheBundle(implicit p: Parameters) extends DCacheBundle {
    // old dcache: optimize data sram read fanout
    // val bankedDcacheData = Vec(DCacheBanks, UInt(64.W))
    // val bank_oh = UInt(DCacheBanks.W)
    val uop = new DynInst // for data selection, only fwen and fuOpType are used
    val dcacheData = UInt(VLEN.W)
    val forwardMask = Vec(VLEN/8, Bool())
    val forwardData = Vec(VLEN/8, UInt(8.W))
    val addrOffset = UInt(4.W) // for data selection
    // mshr and tilelink forward
    val forwardResultValid = Bool()
    // forward tilelink D channel
    val forwardDchan = Bool()
    val forwardDataDchan = Vec(VLEN/8, UInt(8.W))
    // forward mshr data
    val forwardMshr = Bool()
    val forwardDataMshr = Vec(VLEN/8, UInt(8.W))

    def mergeTLData(): UInt = {
      // merge TL D or MSHR data at load s2
      val useDchannel = forwardDchan && forwardResultValid
      val useMSHR = forwardMshr && forwardResultValid
      Mux(
        useDchannel || useMSHR,
        Mux(
          useDchannel,
          forwardDataDchan.asUInt,
          forwardDataMshr.asUInt
        ),
        dcacheData
      )
    }

    def mergeLsqFwdData(dcacheData: UInt): UInt = {
      // merge dcache and lsq forward data at load s3
      val rdataVec = VecInit((0 until VLEN / 8).map(j =>
        Mux(forwardMask(j), forwardData(j), dcacheData(8*(j+1)-1, 8*j))
      ))
      rdataVec.asUInt
    }
  }

  // Load writeback data from load queue (refill)
  class LoadDataFromLQBundle(implicit p: Parameters) extends XSBundle {
    val lqData = UInt(64.W) // load queue has merged data
    val uop = new DynInst // for data selection, only fwen and fuOpType are used
    val addrOffset = UInt(3.W) // for data selection

    def mergedData(): UInt = lqData
  }

  // Bundle for load / store wait waking up
  class MemWaitUpdateReqBundle(implicit p: Parameters) extends XSBundle {
    val robIdx = Vec(backendParams.StaExuCnt, ValidIO(new RobPtr))
    val sqIdx = Vec(backendParams.StdCnt, ValidIO(new SqPtr))
  }

  class MisalignBufferIO(implicit p: Parameters) extends XSBundle {
    val enq = DecoupledIO(new LsPipelineBundle)
    val revoke = Output(Bool())
    val writeback = ValidIO(new LsPipelineBundle)
  }

  class MisalignBufferEnqIO(implicit p: Parameters) extends XSBundle {
    val req = DecoupledIO(new LsPipelineBundle)
    val revoke = Output(Bool())
  }
}
