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


import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.PreDecodeInfo
import xiangshan.mem.prefetch.{PrefetchReqBundle, TrainReqBundle}

import scala.math._

object Bundles {

  object StLdNukeMatchType {
    def Normal      = "b00".U
    def QuadWord    = "b01".U
    def CacheLine   = "b10".U

    def isNormal(matchType: UInt)    = matchType === Normal
    def isQuadWord(matchType: UInt)  = matchType === QuadWord
    def isCacheLine(matchType: UInt) = matchType === CacheLine

    def apply() = UInt(2.W)
  }

  class LsPipelineBundle(implicit p: Parameters) extends XSBundle
    with HasDCacheParameters
    with HasVLSUParameters {
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
    val mmio = Bool()
    val memBackTypeMM = Bool() // 1: main memory, 0: IO
    val hasException = Bool()
    val isHyper = Bool()
    val isForVSnonLeafPTE = Bool()
    val isPrefetch = Bool()
    val isHWPrefetch = Bool()
    val forwardMask = Vec(VLEN/8, Bool())
    val forwardData = Vec(VLEN/8, UInt(8.W))
    val ldCancel = ValidUndirectioned(UInt(log2Ceil(LoadPipelineWidth).W))
    // val func                = UInt(6.W)

    // vector
    val isvec = Bool()
    val isLastElem = Bool()
    val is128bit = Bool()
    val uop_unit_stride_fof = Bool()
    val usSecondInv = Bool()
    val elemIdx = UInt(elemIdxBits.W)
    val alignedType = UInt(alignTypeBits.W)
    val mbIndex = UInt(max(vlmBindexBits, vsmBindexBits).W)
    val reg_offset = UInt(vOffsetBits.W)
    val elemIdxInsideVd = UInt(elemIdxBits.W)
    val is_first_ele = Bool()
    val vecBaseVaddr = UInt(VAddrBits.W)
    val vecVaddrOffset = UInt(VAddrBits.W)
    val vecTriggerMask = UInt((VLEN/8).W)
    // 1: vector active element or scala mem operation, 0: vector not active element
    val vecActive = Bool()
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
    val replayCarry = new ReplayCarry(nWays)
    val isFirstIssue = Bool()
    val hasROBEntry = Bool()
    val mshrid = UInt(log2Up(cfg.nMissEntries).W)
    val handledByMSHR= Bool()
    val replacementUpdated  = Bool()
    val missDbUpdated = Bool()
    val forward_tlDchannel = Bool()
    val dcacheRequireReplay = Bool()
    val delayedLoadError = Bool()
    val lateKill = Bool()
    val feedbacked = Bool()
    val schedIndex = UInt(log2Up(LoadQueueReplaySize).W)
    val tlbNoQuery = Bool()

    // misalign
    val isFrmMisAlignBuf = Bool()
    val isMisalign = Bool()
    val isFinalSplit = Bool()
    val misalignWith16Byte = Bool()
    val misalignNeedWakeUp = Bool()
    val updateAddrValid = Bool()

    def isSWPrefetch: Bool = isPrefetch && !isHWPrefetch
  }

  class LsPrefetchTrainBundle(implicit p: Parameters) extends LsPipelineBundle {
    val meta_prefetch = UInt(L1PfSourceBits.W)
    val meta_access = Bool()
    val is_from_hw_pf = Bool() // s0 source is from prefetch
    val refillLatency = UInt(LATENCY_WIDTH.W)

    def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
      val inputReg = latch match {
        case true   => RegEnable(input, enable)
        case false  => input
      }
      connectSamePort(this, inputReg)
      // The remaining variables must be assigned outside the function to ensure correctness
    }

    def toPrefetchReqBundle(): PrefetchReqBundle = {
      val res = Wire(new PrefetchReqBundle)
      res.vaddr := this.vaddr
      res.paddr := this.paddr
      res.pc := this.uop.pc
      res.miss := this.miss
      res.pfHitStream := isFromStream(this.meta_prefetch)
      res
    }

    def toTrainReqBundle(): TrainReqBundle = {
      val res = Wire(new TrainReqBundle)
      res.vaddr := this.vaddr
      res.paddr := this.paddr
      res.pc := this.uop.pc
      res.miss := this.miss
      res.metaSource := this.meta_prefetch
      res.refillLatency := this.refillLatency
      res
    }
  }

  class LqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
    // load inst replay informations
    val rep_info = new LoadToLsqReplayIO
    val nc_with_data = Bool() // nc access with data
    // queue entry data, except flag bits, will be updated if writeQueue is true,
    // valid bit in LqWriteBundle will be ignored
    val data_wen_dup = Vec(6, Bool()) // dirty reg dup

    def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
      val inputReg = latch match {
        case true   => RegEnable(input, enable)
        case false  => input
      }
      connectSamePort(this, inputReg)
      this.rep_info := DontCare
      this.nc_with_data := DontCare
      this.data_wen_dup := DontCare
    }
  }

  class SqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
    val need_rep = Bool()
  }

  class StoreForwardReqS0(implicit p: Parameters) extends XSBundle {
    val vaddr = UInt(VAddrBits.W)
    val sqIdx = new SqPtr
    val size = UInt(MemorySize.Size.width.W)
    // MDP
    // load inst will not be executed until former store (predicted by mdp) addr calcuated
    val loadWaitBit = Bool()
    // If (loadWaitBit && loadWaitStrict), strict load wait is needed
    // load inst will not be executed until ALL former store addr calcuated
    val loadWaitStrict = Bool()
    val ssid = UInt(SSIDWidth.W)
    val storeSetHit = Bool() // inst has been allocated an store set
    val waitForRobIdx = new RobPtr // store set predicted previous store robIdx
  }

  class StoreForwardReqS1(implicit p: Parameters) extends XSBundle {
    val paddr = UInt(PAddrBits.W)
  }

  class SbufferForwardResp(implicit p: Parameters) extends XSBundle {
    val forwardMask = Vec((VLEN/8), Bool())
    val forwardData = Vec((VLEN/8), UInt(8.W))
    val matchInvalid = Bool()
  }

  class SQForwardRespS1(implicit p: Parameters) extends XSBundle {
    // dataInvalid: addr match, but data is not valid for now
    val dataInvalidFast  = Bool() // resp to load_s1
    val forwardMaskFast  = Vec((VLEN/8), Bool()) // resp to load_s1
  }

  class SQForwardRespS2(implicit p: Parameters) extends XSBundle {
    val forwardMask = Vec((VLEN/8), Bool())
    val forwardData = Vec((VLEN/8), UInt(8.W))
    val forwardInvalid = Bool()
    val matchInvalid = Bool()
    val addrInvalid = Valid(new SqPtr)
    val dataInvalid = Valid(new SqPtr)
  }

  class UncacheForwardResp(implicit p: Parameters) extends SbufferForwardResp // ?

  class SbufferForward(implicit p: Parameters) extends XSBundle {
    val s0Req = ValidIO(new StoreForwardReqS0)
    val s1Req = Output(new StoreForwardReqS1)
    val s1Kill = Output(Bool())
    val s2Resp = Flipped(ValidIO(new SbufferForwardResp))
  }

  class SQForward(implicit p: Parameters) extends XSBundle {
    val s0Req = ValidIO(new StoreForwardReqS0)
    val s1Req = Output(new StoreForwardReqS1)
    val s1Kill = Output(Bool())
    val s1Resp = Flipped(ValidIO(new SQForwardRespS1))
    val s2Resp = Flipped(ValidIO(new SQForwardRespS2))
  }

  class UncacheForward(implicit p: Parameters) extends XSBundle {
    val s0Req = ValidIO(new StoreForwardReqS0)
    val s1Req = Output(new StoreForwardReqS1)
    val s1Kill = Output(Bool())
    val s2Resp = Flipped(ValidIO(new UncacheForwardResp))
  }

  class UncacheBypassReqS0(implicit p: Parameters) extends XSBundle {
    val lqIdx = new LqPtr
    val isNCReplay = Bool()
    val isMMIOReplay = Bool()
  }

  class UncacheBypassRespS1(implicit p: Parameters) extends XSBundle {
    val paddr = UInt(PAddrBits.W)
  }

  class UncacheBypassRespS2(implicit p: Parameters) extends XSBundle {
    val data = UInt(VLEN.W)
    val denied = Bool()
    val corrupt = Bool()
  }

  class UncacheBypass(implicit p: Parameters) extends XSBundle {
    val s0Req = ValidIO(new UncacheBypassReqS0)
    val s1Resp = Flipped(ValidIO(new UncacheBypassRespS1))
    val s2Resp = Flipped(ValidIO(new UncacheBypassRespS2))
  }

  // TODO: LoadForwardQueryIO = LoadForwardReq + LoadForwardResp
  class LoadForwardQueryIO(implicit p: Parameters) extends XSBundle {
    val vaddr = Output(UInt(VAddrBits.W))
    val paddr = Output(UInt(PAddrBits.W))
    val mask = Output(UInt((VLEN/8).W))
    val uop = Output(new DynInst) // for replay
    val pc = Output(UInt(VAddrBits.W)) //for debug
    val valid = Output(Bool())

    val forwardMaskFast = Input(Vec((VLEN/8), Bool())) // resp to load_s1
    val forwardMask = Input(Vec((VLEN/8), Bool())) // resp to load_s2
    val forwardData = Input(Vec((VLEN/8), UInt(8.W))) // resp to load_s2

    // val lqIdx = Output(UInt(LoadQueueIdxWidth.W))
    val sqIdx = Output(new SqPtr)

    // dataInvalid suggests store to load forward found forward should happen,
    // but data is not available for now. If dataInvalid, load inst should
    // be replayed from RS. Feedback type should be RSFeedbackType.dataInvalid
    val dataInvalid = Input(Bool()) // Addr match, but data is not valid for now

    // matchInvalid suggests in store to load forward logic, paddr cam result does
    // to equal to vaddr cam result. If matchInvalid, a microarchitectural exception
    // should be raised to flush SQ and committed sbuffer.
    val matchInvalid = Input(Bool()) // resp to load_s2

    // addrInvalid suggests store to load forward found forward should happen,
    // but address (SSID) is not available for now. If addrInvalid, load inst should
    // be replayed from RS. Feedback type should be RSFeedbackType.addrInvalid
    val addrInvalid = Input(Bool())
  }

  // LoadForwardQueryIO used in load pipeline
  //
  // Difference between PipeLoadForwardQueryIO and LoadForwardQueryIO:
  // PipeIO use predecoded sqIdxMask for better forward timing
  class PipeLoadForwardQueryIO(implicit p: Parameters) extends LoadForwardQueryIO {
    // val sqIdx = Output(new SqPtr) // for debug, should not be used in pipeline for timing reasons
    // sqIdxMask is calcuated in earlier stage for better timing
    val sqIdxMask = Output(UInt(StoreQueueSize.W))

    // dataInvalid: addr match, but data is not valid for now
    val dataInvalidFast = Input(Bool()) // resp to load_s1
    // val dataInvalid = Input(Bool()) // resp to load_s2
    val dataInvalidSqIdx = Input(new SqPtr) // resp to load_s2, sqIdx
    val addrInvalidSqIdx = Input(new SqPtr) // resp to load_s2, sqIdx
  }


  // TODO: remove these

  // Query load queue for ld-ld violation
  //
  // Req should be send in load_s1
  // Resp will be generated 1 cycle later
  //
  // Note that query req may be !ready, as dcache is releasing a block
  // If it happens, a replay from rs is needed.
  class LoadNukeQueryReqBundle(implicit p: Parameters) extends XSBundle { // provide lqIdx
    val uop = new DynInst
    // mask: load's data mask.
    val mask = UInt((VLEN/8).W)

    // paddr: load's paddr.
    val paddr      = UInt(PAddrBits.W)
    // TODO: remove data_valid
    // dataInvalid: load data is invalid.
    val data_valid = Bool()
    // nc: is NC access
    val is_nc = Bool()
  }

  class LoadNukeQueryRespBundle(implicit p: Parameters) extends XSBundle {
    // rep_frm_fetch: ld-ld violation check success, replay from fetch.
    val rep_frm_fetch = Bool()
  }

  class LoadNukeQueryIO(implicit p: Parameters) extends XSBundle {
    val req    = Decoupled(new LoadNukeQueryReqBundle)
    val resp   = Flipped(Valid(new LoadNukeQueryRespBundle))
    val revoke = Output(Bool())
  }

  class LoadNukeQueryReq(implicit p: Parameters) extends XSBundle {
    val robIdx = new RobPtr
    val paddr = UInt(PAddrBits.W)
    val lqIdx = new LqPtr
    val sqIdx = new SqPtr
    val nc = Bool() // always mark a writebacked NC load as released in RAR
    val mask = UInt((VLEN/8).W)
    val isRVC = Bool()
    val ftqPtr = new FtqPtr
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  }

  class LoadNukeQueryResp(implicit p: Parameters) extends XSBundle {
    val nuke = Bool()
  }

  class LoadRARNukeQuery(implicit p: Parameters) extends XSBundle {
    val req = DecoupledIO(new LoadNukeQueryReq)
    val resp = Flipped(ValidIO(new LoadNukeQueryResp))
    val revokeLastCycle = Output(Bool()) // revoke the req in the last cycle
    val revokeLastLastCycle = Output(Bool()) // revoke the req in the last cycle before last cycle
  }

  class LoadRAWNukeQuery(implicit p: Parameters) extends XSBundle {
    // RAW nuke is generated in LoadQueueRAW, therefore there is no response to LDU
    val req = DecoupledIO(new LoadNukeQueryReq)
    val revokeLastCycle = Output(Bool())
    val revokeLastLastCycle = Output(Bool())
  }

  class StoreNukeQueryReq(implicit p: Parameters) extends XSBundle {
    //  robIdx: Requestor's (a store instruction) rob index for match logic.
    val robIdx = new RobPtr

    //  paddr: requestor's (a store instruction) physical address for match logic.
    val paddr  = UInt(PAddrBits.W)

    //  mask: requestor's (a store instruction) data width mask for match logic.
    val mask = UInt((VLEN/8).W)

    // matchType: store load nuke match type. See this class for details.
    val matchType = StLdNukeMatchType()
  }

  class StoreMaBufToSqControlIO(implicit p: Parameters) extends XSBundle {
    // from storeMisalignBuffer to storeQueue, control it's sbuffer write
    val toStoreQueue = Output(new XSBundle {
      // This entry is a cross page
      val crossPageWithHit = Bool()
      val crossPageCanDeq  = Bool()
      // High page Paddr
      val paddr = UInt(PAddrBits.W)

      val withSameUop = Bool()
    })
    // from storeQueue to storeMisalignBuffer, provide detail info of this store
    val toStoreMisalignBuffer = Input(new XSBundle {
      val sqPtr = new SqPtr
      val doDeq = Bool()

      val uop = new DynInst()
    })
  }

  class StoreMaBufToVecStoreMergeBufferIO(implicit p: Parameters)  extends VLSUBundle{
    val mbIndex = Output(UInt(vsmBindexBits.W))
    val flush   = Output(Bool())
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

    // new dcache
    val respDcacheData = UInt(VLEN.W)
    val forwardMask = Vec(VLEN/8, Bool())
    val forwardData = Vec(VLEN/8, UInt(8.W))
    val uop = new DynInst // for data selection, only fwen and fuOpType are used
    val addrOffset = UInt(4.W) // for data selection

    // forward tilelink D channel
    val forward_D = Bool()
    val forwardData_D = Vec(VLEN/8, UInt(8.W))

    // forward mshr data
    val forward_mshr = Bool()
    val forwardData_mshr = Vec(VLEN/8, UInt(8.W))

    val forward_result_valid = Bool()

    def mergeTLData(): UInt = {
      // merge TL D or MSHR data at load s2
      val dcache_data = respDcacheData
      val use_D = forward_D && forward_result_valid
      val use_mshr = forward_mshr && forward_result_valid
      Mux(
        use_D || use_mshr,
        Mux(
          use_D,
          forwardData_D.asUInt,
          forwardData_mshr.asUInt
        ),
        dcache_data
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

    def mergedData(): UInt = {
      lqData
    }
  }

  // Bundle for load / store wait waking up
  class MemWaitUpdateReqBundle(implicit p: Parameters) extends XSBundle {
    val robIdx = Vec(backendParams.StaExuCnt, ValidIO(new RobPtr))
    val sqIdx = Vec(backendParams.StdCnt, ValidIO(new SqPtr))
  }

  class MisalignBufferEnqIO(implicit p: Parameters) extends XSBundle {
    val req = DecoupledIO(new LqWriteBundle)
    val revoke = Output(Bool())
  }

}

// for vector difftest store event
class ToSbufferDifftestInfoBundle(implicit p: Parameters) extends XSBundle{
  val uop        = new DynInst
  val start      = UInt(log2Up(XLEN).W) // indicate first byte position of first unit-stride's element when unaligned
  val offset     = UInt(log2Up(XLEN).W) // indicate byte offset of unit-stride's element when unaligned
}


class VecMissalignedDebugBundle (implicit p: Parameters) extends XSBundle {
  val start      = UInt(log2Up(XLEN).W) // indicate first byte position of first unit-stride's element when unaligned
  val offset     = UInt(log2Up(XLEN).W) // indicate byte offset of unit-stride's element when unaligned
}

class DifftestPmaStoreIO(implicit p: Parameters) extends XSBundle {
  val data           = UInt(VLEN.W)
  val mask           = UInt((VLEN/8).W)
  val addr           = UInt(PAddrBits.W)
  val wline          = Bool()
  val vecValid       = Bool()
  val diffIsHighPart = Bool() // indicate whether valid data in high 64-bit, only for scalar store event!
}

class DiffStoreIO(implicit p: Parameters) extends XSBundle{
  val diffInfo = Vec(EnsbufferWidth, Flipped(new ToSbufferDifftestInfoBundle()))
  val pmaStore = Vec(EnsbufferWidth, Flipped(Valid(new DifftestPmaStoreIO)))
  val ncStore = Flipped(Valid(new UncacheWordReq()))
}