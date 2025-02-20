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
    val atomic = Bool()
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

    def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
      val inputReg = latch match {
        case true   => RegEnable(input, enable)
        case false  => input
      }
      connectSamePort(this, inputReg)
      this.meta_prefetch := DontCare
      this.meta_access := DontCare
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
      this.data_wen_dup   := DontCare
    }
  }

  class SqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
    val need_rep = Bool()
  }

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

  class StoreNukeQueryBundle(implicit p: Parameters) extends XSBundle {
    //  robIdx: Requestor's (a store instruction) rob index for match logic.
    val robIdx = new RobPtr

    //  paddr: requestor's (a store instruction) physical address for match logic.
    val paddr  = UInt(PAddrBits.W)

    //  mask: requestor's (a store instruction) data width mask for match logic.
    val mask = UInt((VLEN/8).W)

    // matchLine: if store is vector 128-bits, load unit need to compare 128-bits vaddr.
    val matchLine = Bool()
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

}
