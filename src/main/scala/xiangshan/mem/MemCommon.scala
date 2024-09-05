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
import xiangshan.backend.Bundles.{DynInst, MemExuInput}
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.mem.prefetch.PrefetchReqBundle
import math._

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt
  }
}

object genVWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(3, 0)).asUInt
  }
}

object genWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(16, data(7, 0)),
      "b01".U -> Fill(8, data(15, 0)),
      "b10".U -> Fill(4, data(31, 0)),
      "b11".U -> Fill(2, data(63,0))
    ))
  }
}

object shiftDataToLow {
  def apply(addr: UInt,data : UInt): UInt = {
    Mux(addr(3), (data >> 64).asUInt,data)
  }
}
object shiftMaskToLow {
  def apply(addr: UInt,mask: UInt): UInt = {
    Mux(addr(3),(mask >> 8).asUInt,mask)
  }
}

class LsPipelineBundle(implicit p: Parameters) extends XSBundle
  with HasDCacheParameters
  with HasVLSUParameters {
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val gpaddr = UInt(GPAddrBits.W)
  // val func = UInt(6.W)
  val mask = UInt((VLEN/8).W)
  val data = UInt((VLEN+1).W)
  val wlineflag = Bool() // store write the whole cache line

  val miss = Bool()
  val tlbMiss = Bool()
  val ptwBack = Bool()
  val af = Bool()
  val mmio = Bool()
  val atomic = Bool()

  val forwardMask = Vec(VLEN/8, Bool())
  val forwardData = Vec(VLEN/8, UInt(8.W))

  // prefetch
  val isPrefetch = Bool()
  val isHWPrefetch = Bool()
  def isSWPrefetch = isPrefetch && !isHWPrefetch

  // misalignBuffer
  val isFrmMisAlignBuf = Bool()

  // vector
  val isvec = Bool()
  val isLastElem = Bool()
  val is128bit = Bool()
  val uop_unit_stride_fof = Bool()
  val usSecondInv = Bool()
  val elemIdx = UInt(elemIdxBits.W)
  val alignedType = UInt(alignTypeBits.W)
  val mbIndex = UInt(max(vlmBindexBits, vsmBindexBits).W)
  // val rob_idx_valid = Vec(2,Bool())
  // val inner_idx = Vec(2,UInt(3.W))
  // val rob_idx = Vec(2,new RobPtr)
  val reg_offset = UInt(vOffsetBits.W)
  val elemIdxInsideVd = UInt(elemIdxBits.W)
  // val offset = Vec(2,UInt(4.W))
  val vecActive = Bool() // 1: vector active element or scala mem operation, 0: vector not active element
  val is_first_ele = Bool()
  // val flowPtr = new VlflowPtr() // VLFlowQueue ptr
  // val sflowPtr = new VsFlowPtr() // VSFlowQueue ptr

  // For debug usage
  val isFirstIssue = Bool()
  val hasROBEntry = Bool()

  // For load replay
  val isLoadReplay = Bool()
  val isFastPath = Bool()
  val isFastReplay = Bool()
  val replayCarry = new ReplayCarry(nWays)

  // For dcache miss load
  val mshrid = UInt(log2Up(cfg.nMissEntries).W)
  val handledByMSHR = Bool()
  val replacementUpdated = Bool()
  val missDbUpdated = Bool()

  val forward_tlDchannel = Bool()
  val dcacheRequireReplay = Bool()
  val delayedLoadError = Bool()
  val lateKill = Bool()
  val feedbacked = Bool()
  val ldCancel = ValidUndirectioned(UInt(log2Ceil(LoadPipelineWidth).W))
  // loadQueueReplay index.
  val schedIndex = UInt(log2Up(LoadQueueReplaySize).W)
  // hardware prefetch and fast replay no need to query tlb
  val tlbNoQuery = Bool()
}

class LdPrefetchTrainBundle(implicit p: Parameters) extends LsPipelineBundle {
  val meta_prefetch = UInt(L1PfSourceBits.W)
  val meta_access = Bool()

  def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
    if (latch) vaddr := RegEnable(input.vaddr, enable) else vaddr := input.vaddr
    if (latch) paddr := RegEnable(input.paddr, enable) else paddr := input.paddr
    if (latch) gpaddr := RegEnable(input.gpaddr, enable) else gpaddr := input.gpaddr
    if (latch) mask := RegEnable(input.mask, enable) else mask := input.mask
    if (latch) data := RegEnable(input.data, enable) else data := input.data
    if (latch) uop := RegEnable(input.uop, enable) else uop := input.uop
    if (latch) wlineflag := RegEnable(input.wlineflag, enable) else wlineflag := input.wlineflag
    if (latch) miss := RegEnable(input.miss, enable) else miss := input.miss
    if (latch) tlbMiss := RegEnable(input.tlbMiss, enable) else tlbMiss := input.tlbMiss
    if (latch) ptwBack := RegEnable(input.ptwBack, enable) else ptwBack := input.ptwBack
    if (latch) af := RegEnable(input.af, enable) else af := input.af
    if (latch) mmio := RegEnable(input.mmio, enable) else mmio := input.mmio
    if (latch) forwardMask := RegEnable(input.forwardMask, enable) else forwardMask := input.forwardMask
    if (latch) forwardData := RegEnable(input.forwardData, enable) else forwardData := input.forwardData
    if (latch) isPrefetch := RegEnable(input.isPrefetch, enable) else isPrefetch := input.isPrefetch
    if (latch) isHWPrefetch := RegEnable(input.isHWPrefetch, enable) else isHWPrefetch := input.isHWPrefetch
    if (latch) isFrmMisAlignBuf := RegEnable(input.isFrmMisAlignBuf, enable) else isFrmMisAlignBuf := input.isFrmMisAlignBuf
    if (latch) isFirstIssue := RegEnable(input.isFirstIssue, enable) else isFirstIssue := input.isFirstIssue
    if (latch) hasROBEntry := RegEnable(input.hasROBEntry, enable) else hasROBEntry := input.hasROBEntry
    if (latch) dcacheRequireReplay := RegEnable(input.dcacheRequireReplay, enable) else dcacheRequireReplay := input.dcacheRequireReplay
    if (latch) schedIndex := RegEnable(input.schedIndex, enable) else schedIndex := input.schedIndex
    if (latch) tlbNoQuery := RegEnable(input.tlbNoQuery, enable) else tlbNoQuery := input.tlbNoQuery
    if (latch) isvec               := RegEnable(input.isvec, enable)               else isvec               := input.isvec
    if (latch) isLastElem          := RegEnable(input.isLastElem, enable)          else isLastElem          := input.isLastElem
    if (latch) is128bit            := RegEnable(input.is128bit, enable)            else is128bit            := input.is128bit
    if (latch) vecActive           := RegEnable(input.vecActive, enable)           else vecActive           := input.vecActive
    if (latch) is_first_ele        := RegEnable(input.is_first_ele, enable)        else is_first_ele        := input.is_first_ele
    if (latch) uop_unit_stride_fof := RegEnable(input.uop_unit_stride_fof, enable) else uop_unit_stride_fof := input.uop_unit_stride_fof
    if (latch) usSecondInv         := RegEnable(input.usSecondInv, enable)         else usSecondInv         := input.usSecondInv
    if (latch) reg_offset          := RegEnable(input.reg_offset, enable)          else reg_offset          := input.reg_offset
    if (latch) elemIdx             := RegEnable(input.elemIdx, enable)             else elemIdx             := input.elemIdx
    if (latch) alignedType         := RegEnable(input.alignedType, enable)         else alignedType         := input.alignedType
    if (latch) mbIndex             := RegEnable(input.mbIndex, enable)             else mbIndex             := input.mbIndex
    if (latch) elemIdxInsideVd     := RegEnable(input.elemIdxInsideVd, enable)     else elemIdxInsideVd     := input.elemIdxInsideVd
    // if (latch) flowPtr             := RegEnable(input.flowPtr, enable)             else flowPtr             := input.flowPtr
    // if (latch) sflowPtr            := RegEnable(input.sflowPtr, enable)            else sflowPtr            := input.sflowPtr

    meta_prefetch := DontCare
    meta_access := DontCare
    forward_tlDchannel := DontCare
    mshrid := DontCare
    replayCarry := DontCare
    atomic := DontCare
    isLoadReplay := DontCare
    isFastPath := DontCare
    isFastReplay := DontCare
    handledByMSHR := DontCare
    replacementUpdated := DontCare
    missDbUpdated := DontCare
    delayedLoadError := DontCare
    lateKill := DontCare
    feedbacked := DontCare
    ldCancel := DontCare
  }

  def asPrefetchReqBundle(): PrefetchReqBundle = {
    val res = Wire(new PrefetchReqBundle)
    res.vaddr       := this.vaddr
    res.paddr       := this.paddr
    res.pc          := this.uop.pc
    res.miss        := this.miss
    res.pfHitStream := isFromStream(this.meta_prefetch)

    res
  }
}

class StPrefetchTrainBundle(implicit p: Parameters) extends LdPrefetchTrainBundle {}

class LqWriteBundle(implicit p: Parameters) extends LsPipelineBundle {
  // load inst replay informations
  val rep_info = new LoadToLsqReplayIO
  // queue entry data, except flag bits, will be updated if writeQueue is true,
  // valid bit in LqWriteBundle will be ignored
  val data_wen_dup = Vec(6, Bool()) // dirty reg dup


  def fromLsPipelineBundle(input: LsPipelineBundle, latch: Boolean = false, enable: Bool = true.B) = {
    if(latch) vaddr := RegEnable(input.vaddr, enable) else vaddr := input.vaddr
    if(latch) paddr := RegEnable(input.paddr, enable) else paddr := input.paddr
    if(latch) gpaddr := RegEnable(input.gpaddr, enable) else gpaddr := input.gpaddr
    if(latch) mask := RegEnable(input.mask, enable) else mask := input.mask
    if(latch) data := RegEnable(input.data, enable) else data := input.data
    if(latch) uop := RegEnable(input.uop, enable) else uop := input.uop
    if(latch) wlineflag := RegEnable(input.wlineflag, enable) else wlineflag := input.wlineflag
    if(latch) miss := RegEnable(input.miss, enable) else miss := input.miss
    if(latch) tlbMiss := RegEnable(input.tlbMiss, enable) else tlbMiss := input.tlbMiss
    if(latch) ptwBack := RegEnable(input.ptwBack, enable) else ptwBack := input.ptwBack
    if(latch) mmio := RegEnable(input.mmio, enable) else mmio := input.mmio
    if(latch) atomic := RegEnable(input.atomic, enable) else atomic := input.atomic
    if(latch) forwardMask := RegEnable(input.forwardMask, enable) else forwardMask := input.forwardMask
    if(latch) forwardData := RegEnable(input.forwardData, enable) else forwardData := input.forwardData
    if(latch) isPrefetch := RegEnable(input.isPrefetch, enable) else isPrefetch := input.isPrefetch
    if(latch) isHWPrefetch := RegEnable(input.isHWPrefetch, enable) else isHWPrefetch := input.isHWPrefetch
    if(latch) isFrmMisAlignBuf := RegEnable(input.isFrmMisAlignBuf, enable) else isFrmMisAlignBuf := input.isFrmMisAlignBuf
    if(latch) isFirstIssue := RegEnable(input.isFirstIssue, enable) else isFirstIssue := input.isFirstIssue
    if(latch) hasROBEntry := RegEnable(input.hasROBEntry, enable) else hasROBEntry := input.hasROBEntry
    if(latch) isLoadReplay := RegEnable(input.isLoadReplay, enable) else isLoadReplay := input.isLoadReplay
    if(latch) isFastPath := RegEnable(input.isFastPath, enable) else isFastPath := input.isFastPath
    if(latch) isFastReplay := RegEnable(input.isFastReplay, enable) else isFastReplay := input.isFastReplay
    if(latch) mshrid := RegEnable(input.mshrid, enable) else mshrid := input.mshrid
    if(latch) forward_tlDchannel := RegEnable(input.forward_tlDchannel, enable) else forward_tlDchannel := input.forward_tlDchannel
    if(latch) replayCarry := RegEnable(input.replayCarry, enable) else replayCarry := input.replayCarry
    if(latch) dcacheRequireReplay := RegEnable(input.dcacheRequireReplay, enable) else dcacheRequireReplay := input.dcacheRequireReplay
    if(latch) schedIndex := RegEnable(input.schedIndex, enable) else schedIndex := input.schedIndex
    if(latch) handledByMSHR := RegEnable(input.handledByMSHR, enable) else handledByMSHR := input.handledByMSHR
    if(latch) replacementUpdated := RegEnable(input.replacementUpdated, enable) else replacementUpdated := input.replacementUpdated
    if(latch) missDbUpdated := RegEnable(input.missDbUpdated, enable) else missDbUpdated := input.missDbUpdated
    if(latch) delayedLoadError := RegEnable(input.delayedLoadError, enable) else delayedLoadError := input.delayedLoadError
    if(latch) lateKill := RegEnable(input.lateKill, enable) else lateKill := input.lateKill
    if(latch) feedbacked := RegEnable(input.feedbacked, enable) else feedbacked := input.feedbacked
    if(latch) isvec               := RegEnable(input.isvec, enable)               else isvec               := input.isvec
    if(latch) is128bit            := RegEnable(input.is128bit, enable)            else is128bit            := input.is128bit
    if(latch) vecActive           := RegEnable(input.vecActive, enable)           else vecActive           := input.vecActive
    if(latch) uop_unit_stride_fof := RegEnable(input.uop_unit_stride_fof, enable) else uop_unit_stride_fof := input.uop_unit_stride_fof
    if(latch) reg_offset          := RegEnable(input.reg_offset, enable)          else reg_offset          := input.reg_offset
    if(latch) mbIndex             := RegEnable(input.mbIndex, enable)             else mbIndex             := input.mbIndex
    if(latch) elemIdxInsideVd     := RegEnable(input.elemIdxInsideVd, enable)     else elemIdxInsideVd     := input.elemIdxInsideVd

    rep_info := DontCare
    data_wen_dup := DontCare
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
class LoadNukeQueryReq(implicit p: Parameters) extends XSBundle { // provide lqIdx
  val uop = new DynInst
  // mask: load's data mask.
  val mask = UInt((VLEN/8).W)

  // paddr: load's paddr.
  val paddr      = UInt(PAddrBits.W)
  // dataInvalid: load data is invalid.
  val data_valid = Bool()
}

class LoadNukeQueryResp(implicit p: Parameters) extends XSBundle {
  // rep_frm_fetch: ld-ld violation check success, replay from fetch.
  val rep_frm_fetch = Bool()
}

class LoadNukeQueryIO(implicit p: Parameters) extends XSBundle {
  val req    = Decoupled(new LoadNukeQueryReq)
  val resp   = Flipped(Valid(new LoadNukeQueryResp))
  val revoke = Output(Bool())
}

class StoreNukeQueryIO(implicit p: Parameters) extends XSBundle {
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
  val control = Output(new XSBundle {
    // control sq to write-into sb
    val writeSb = Bool()
    val wdata = UInt(VLEN.W)
    val wmask = UInt((VLEN / 8).W)
    val paddr = UInt(PAddrBits.W)
    val vaddr = UInt(VAddrBits.W)
    val last  = Bool()
    val hasException = Bool()
    // remove this entry in sq
    val removeSq = Bool()
  })
  // from storeQueue to storeMisalignBuffer, provide detail info of this store
  val storeInfo = Input(new XSBundle {
    val data = UInt(VLEN.W)
    // is the data of the unaligned store ready at sq?
    val dataReady = Bool()
    // complete a data transfer from sq to sb
    val completeSbTrans = Bool()
  })
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
class MemWaitUpdateReq(implicit p: Parameters) extends XSBundle {
  val robIdx = Vec(backendParams.StaExuCnt, ValidIO(new RobPtr))
  val sqIdx = Vec(backendParams.StdCnt, ValidIO(new SqPtr))
}

object AddPipelineReg {
  class PipelineRegModule[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_reg_valid")
    when (io.out.fire) { valid := false.B }
    when (io.in.fire) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := !valid || io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.fire)
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], isFlush: Bool,
   moduleName: Option[String] = None
  ): Unit = {
    val pipelineReg = Module(new PipelineRegModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineReg.suggestName(moduleName.get)
    pipelineReg.io.in <> left
    right <> pipelineReg.io.out
    pipelineReg.io.isFlush := isFlush
  }
}