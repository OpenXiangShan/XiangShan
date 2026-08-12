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
import org.chipsalliance.cde.config.Parameters
import utility.{InstSeqNum, MultiFlagCircularQueuePtr}
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, ExuOutput, MemExuOutput, MemToRob, UopIdx}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.{CMOReq, CMOResp, DCacheWordReqWithVaddrAndPfFlag, UncacheWordIO}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.Bundles.{SQForward, StoreMaskBundle}

  class LRQWakeUpCancelBundle(implicit p: Parameters) extends XSBundle {
    val og0Cancel = Bool()
    val og1Cancel = Bool()
    val s0Cancel  = Bool()
    val s1Cancel  = Bool()
  }

class StoreQueueEnqIO(implicit p: Parameters) extends MemBlockBundle {
  // Bundle define

  // from Dispatch
  class ReqUopInfo(implicit p: Parameters) extends MemBlockBundle {
    val robIdx          = new RobPtr
    val numLsElem       = NumLsElem()
    val sqIdx           = new SqPtr
    val lastUop         = Bool()
    val fuType          = FuType()
    val fuOpType        = FuOpType()
    val uopIdx          = UopIdx()
    // load inst will not be executed until former store (predicted by mdp) addr calcuated
    val loadWaitBit     = Bool()
    // If (loadWaitBit && loadWaitStrict), strict load wait is needed
    // load inst will not be executed until ALL former store addr calcuated
    val loadWaitStrict  = Bool()
    val ssid            = UInt(SSIDWidth.W)
    val storeSetHit     = Bool() // inst has been allocated an store set
    // debug signal
    val pc              = Option.when(debugEn)(UInt(VAddrBits.W))
  }
  class FromDispatchReq(implicit p: Parameters) extends MemBlockBundle {
    val needAlloc       = Bool()
    val uop             = new ReqUopInfo
    // debug signal
    val debugUop        = Option.when(debugEn)(new DynInst()) // only for difftest
  }

  // to Dispatch
  class ToDispatchResp(implicit p: Parameters) extends MemBlockBundle {
    val sqIdx           = new SqPtr
  }

  // IO define
  val lqCanAccept       = Input(Bool())
  val canAccept         = Output(Bool())
  val req               = Vec(LSQEnqWidth, Flipped(ValidIO(new FromDispatchReq)))
  val resp              = Vec(LSQEnqWidth, new ToDispatchResp)
}

class UnalignQueueIO(implicit p: Parameters) extends MemBlockBundle {
  val sqIdx              = new SqPtr
  val robIdx             = new RobPtr
  val paddr              = UInt(PAddrBits.W)
}

class StaUopInfo(implicit p: Parameters) extends MemBlockBundle {
  val sqIdx           = new SqPtr
  val fuOpType        = FuOpType()
  val robIdx          = new RobPtr

  // used in RAW check, for MDP train
  val ftqPtr          = new FtqPtr
  val ftqOffset       = UInt(FetchBlockInstOffsetWidth.W)

  // mdp
  val isFirstIssue    = Bool()
  val isRVC           = Bool()

  // debug info
  val pc              = Option.when(debugEn)(UInt(VAddrBits.W))
  val debugInfo       = Option.when(debugEn)(new PerfDebugInfo)
  val debug_seqNum    = Option.when(debugEn)(InstSeqNum())
}
// TODO: distinguish storeAddrIn and storeAddrInRe
class StoreAddrIO(implicit p: Parameters) extends MemBlockBundle {
  val uop             = new StaUopInfo
  val tlbMiss         = Bool()
  val cacheMiss       = Bool()
  val vaddr           = UInt(VAddrBits.W)
  val paddr           = UInt(PAddrBits.W)
  val nc              = Bool() // indicate request is none-cacheable.
  val mmio            = Bool()
  val mask            = UInt((VLEN/8).W)
  val size            = UInt(MemorySize.Size.width.W)
  val memBackTypeMM   = Bool() // 1: main memory, 0: IO.
  val hasException    = Bool() // indicate request has exception.
  val isHyper         = Bool()

  /* only use in cmo.zero
  * means this write request need to write whole cacheline.
  * */
  val wlineflag          = Bool() // store write the whole cache line.

  // misalign
  val isUnalign   = Bool()
  val cross16Byte = Bool()

  // ctrl signal
  val isLastRequest      = Bool() /* It's last request to write to storeQueue. if is normal request, it will be true,
                                      if it was unalign splited, first request will be false, second will be true. */
  val debugUop           = Option.when(debugEn)(new DynInst())
}

class StoreQueueDataWrite(implicit p: Parameters) extends MemBlockBundle {
  val fuType             = FuType()
  val fuOpType           = FuOpType()
  val data               = UInt(VLEN.W)
  val sqIdx              = new SqPtr
  val vecDebug           = Option.when(debugEn)(new VecMissalignedDebugBundle)

}

class StaIO(implicit p: Parameters) extends MemBlockBundle {
  val storeAddrIn      = Vec(StorePipelineWidth, Flipped(ValidIO(new StoreAddrIO))) // store addr, data is not included
  // this bundle will be removed in the feature.
  val storeAddrInRe    = Vec(StorePipelineWidth, Input(new StoreAddrIO)) // store more mmio and exception
  // ready indicate unaligned queue reject this unaligned request
  val unalignQueueReq = Vec(StorePipelineWidth, Flipped(DecoupledIO(new UnalignQueueIO)))
}

class ToCacheIO(implicit p: Parameters) extends MemBlockBundle {
  val req             = DecoupledIO(new CMOReq)
  val resp            = Flipped(DecoupledIO(new CMOResp))
}

class toRobIO(implicit p: Parameters) extends XSBundle {
  val mmioBusy      = Bool()
}

class SbufferCtrlIO(implicit p: Parameters) extends XSBundle {
  class Req(implicit p: Parameters) extends XSBundle {
    val flush            = Bool() // flush is to empty sbuffer
    val forceWrite       = Bool() // force write is to evict some sbuffer entries.
    val physicalStoreQueueFull = Bool() // physical store queue full, for perf.
  }
  class Resp(implicit p: Parameters) extends XSBundle {
    val empty            = Bool()
  }

  val req                = Output(new Req)
  val resp               = Input(new Resp)
}

class StoreQueueToLoadQueueIO(implicit p: Parameters) extends XSBundle {
  val stAddrReadySqPtr   = new SqPtr
  val stAddrReadyVec     = Vec(StoreQueuePhysicalSize, Bool())
  val stDataReadySqPtr   = new SqPtr
  val stDataReadyVec     = Vec(StoreQueuePhysicalSize, Bool())
}

class SbufferWriteIO(implicit p : Parameters) extends XSBundle {
  val req                = Vec(EnsbufferWidth, DecoupledIO(new DCacheWordReqWithVaddrAndPfFlag))
}

class StoreQueueIO(val param: ExeUnitParams) (implicit p: Parameters) extends MemBlockBundle {
  val hartId             = Input(UInt(hartIdLen.W)) // for mulit Core Difftest
  val redirect           = Flipped(ValidIO(new Redirect)) // to VirtualStoreQueue
  val enq                = new VirtualStoreQueueEnqIO(new SqPtr) // from dispatch to VirtualStoreQueue
  val fromRob            = new ROBToVirtualStoreQueueIO // from ROB to VirtualStoreQueue
  val toLsqEnqCtrl       = new ToLsqEnqCtrl(hasStore = true, hasLoad = false) // to lsqEnqCtrl
  // when VStoreMergeBuffer writeback micro-op, storeQueue need to set `vecMbCommit`
  val fromVMergeBuffer   = Vec(VecStorePipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO))) //TODO: will be remove in the feature
  val storeDataIn        = Vec(StorePipelineWidth, Flipped(Valid(new StoreQueueDataWrite))) // store data, send to sq from rs
  val fromStoreUnit      = new StaIO // from storeUnit
  val writeToSbuffer     = new SbufferWriteIO // write committed store to sbuffer
  // conctrl sbuffer, has two function:
  // 1. It will evict some entries of sbuffer to dcache; 2. flush sbuffer
  val sbufferCtrl        = new SbufferCtrlIO
  val toDCache           = new ToCacheIO // cmo handle send clean, invalid, flush to dcache
  val forward            = Flipped(Vec(LoadPipelineWidth, new SQForward)) // from loadUnit, forward query
  val toRob              = Output(new toRobIO) // write store request to uncacheBuffer
  val toUncacheBuffer    = new UncacheWordIO
  val writeBack          = DecoupledIO(new MemToRob(param))// to backend , used to writeback uop when request is mmio, cmo.
  val wfi                = Flipped(new WfiReqBundle)
  val sqEmpty            = Output(Bool())
  val sqFull             = Output(Bool())
  val toLoadQueue        = Output(new StoreQueueToLoadQueueIO)
  val exceptionInfo      = ValidIO(new MemExceptionInfo)// to exceptionInfoGen, only for mmio/cbo writeback exception gen
  val sqDeq              = ValidIO(UInt(log2Ceil(EnsbufferWidth + 1).W)) // to backend, dispatch
  val sqDeqPtr           = Output(new SqPtr) // to store unit
  val diffStore          = Option.when(debugEn)(Flipped(new DiffStoreIO)) // for store difftest
  val physicalUpperSqIdx = Output(new SqPtr)
}

class PhysicalStoreQueueIO(val param: ExeUnitParams) (implicit p: Parameters) extends MemBlockBundle {
  val hartId             = Input(UInt(hartIdLen.W)) // for mulit Core Difftest
  val storeDataIn        = Vec(StorePipelineWidth, Flipped(Valid(new StoreQueueDataWrite))) // store data, send to sq from rs
  val fromStoreUnit      = new StaIO // from storeUnit
  val writeToSbuffer     = new SbufferWriteIO // write committed store to sbuffer
  // conctrl sbuffer, has two function:
  // 1. It will evict some entries of sbuffer to dcache; 2. flush sbuffer.
  val sbufferCtrl        = new SbufferCtrlIO
  val toDCache           = new ToCacheIO // cmo handle send clean, invalid, flush to dcache
  val forward            = Flipped(Vec(LoadPipelineWidth, new SQForward)) // from loadUnit, forward query
  val fromVirtualStoreQueue = Flipped(new VirtualStoreQueueToPhysicalQueueIO(new SqPtr))
  val toRob              = Output(new toRobIO)
  val toUncacheBuffer    = new UncacheWordIO // write store request to uncacheBuffer
  val writeBack          = DecoupledIO(new MemToRob(param)) // to backend , used to writeback uop when request is mmio, cmo
  val wfi                = Flipped(new WfiReqBundle)
  val empty              = Output(Bool())
  val full               = Output(Bool())
  val toLoadQueue        = Output(new StoreQueueToLoadQueueIO)
  // to exceptionInfoGen, only for mmio/cbo writeback exception gen
  val exceptionInfo      = ValidIO(new MemExceptionInfo)
  // to backend, dispatch
  val sqDeq              = ValidIO(UInt(log2Ceil(EnsbufferWidth + 1).W))
  // to store unit
  val sqDeqPtr           = Output(new SqPtr)
  val physicalUpperSqIdx = Output(new SqPtr)
  // for store difftest
  val diffStore          = Option.when(debugEn)(Flipped(new DiffStoreIO))
}

class VirtualStoreQueueEnqIO(PhysicalQueuePtr: MultiFlagCircularQueuePtr[_]) (implicit p: Parameters) extends MemBlockBundle {
  // from Dispatch
  class ReqUopInfo (implicit p: Parameters) extends MemBlockBundle {
    val robIdx          = new RobPtr
    val numLsElem       = NumLsElem()
    val uopIdx          = UopIdx()
    val isVec           = Bool()
    // debug info
    val ssid            = UInt(SSIDWidth.W) // maybe not used
    val storeSetHit     = Bool() // inst has been allocated an store set. maybe not used
    // debug signal
    val pc              = Option.when(debugEn)(UInt(VAddrBits.W))
  }
  class FromDispatchReq (implicit p: Parameters) extends MemBlockBundle {
    val needAlloc       = Bool()
    val uop             = new ReqUopInfo
    val reqEndPtr       = new LSIdx // uop end lqIdx/sqIdx
    val reqStartPtr     = new LSIdx // uop start lqIdx/sqIdx
    // debug signal
    val debugUop        = Option.when(debugEn)(new DynInst()) // only for difftest
  }
  class ToDispatchResp(PhysicalQueuePtr: MultiFlagCircularQueuePtr[_]) (implicit p: Parameters) extends MemBlockBundle {
    val physicalQueuePtr = Output(PhysicalQueuePtr.cloneType)
  }

  // IO define
//  val otherCanAccept    = Input(Bool()) // for storeQueue, is lqCanAccept; for loadQueue, is sqCanAccept
  // from Dispatch
  val req               = Vec(LSQEnqWidth, Flipped(ValidIO(new FromDispatchReq)))
  // to Dispatch
  val canAccept         = Output(Bool())
  val resp              = Vec(LSQEnqWidth, new ToDispatchResp(PhysicalQueuePtr))
}

class VirtualStoreQueueToPhysicalQueueIO(PhysicalQueuePtr: MultiFlagCircularQueuePtr[_])
                                         (implicit p: Parameters) extends XSBundle {

  val redirectPtr        = ValidIO(PhysicalQueuePtr.cloneType)
  val preCommitPtr       = ValidIO(PhysicalQueuePtr.cloneType) // uop is rob head, which can be set committed in physicalQueue
  val retiredPtr         = PhysicalQueuePtr.cloneType
  val physicalQueueEnqPtr = Output(PhysicalQueuePtr.cloneType)
  val mdpHitPtr           = Vec(LoadPipelineWidth, ValidIO(PhysicalQueuePtr.cloneType)) // for MDP predict, indicate which entry is hit by load
  val headRobIdx          = Output(new RobPtr) // for writeback of mmio/cbo
}

class ROBToVirtualStoreQueueIO(implicit p: Parameters) extends XSBundle {
  val robHeadPtr         = Input(new RobPtr)
}

class ToLsqEnqCtrl(hasStore: Boolean, hasLoad: Boolean) (implicit p: Parameters) extends XSBundle {

  val sqDeq              = OptionWrapper(hasStore, ValidIO(UInt(log2Ceil(EnsbufferWidth + 1).W)))
  val lqDeq              = OptionWrapper(hasLoad, ValidIO(UInt(log2Up(CommitWidth + 1).W)))
  val lqRedirectPtr      = OptionWrapper(hasLoad, ValidIO(new LqPtr))
  val sqRedirectPtr      = OptionWrapper(hasStore, ValidIO(new SqPtr))
  val lqRecoverStall     = OptionWrapper(hasLoad, Bool())
  val sqRecoverStall     = OptionWrapper(hasStore, Bool())
}

class PhysicalQueueToVirtualStoreQueueIO[PhysicalQueuePtrType <: MultiFlagCircularQueuePtr[PhysicalQueuePtrType]](PhysicalQueuePtr: PhysicalQueuePtrType) (implicit p: Parameters) extends XSBundle {
  val deqPtr             = Input(PhysicalQueuePtr.cloneType)
  val deqCount           = Flipped(ValidIO(UInt(log2Ceil(EnsbufferWidth + 1).W)))
}

class MDPQueryIO (implicit p: Parameters) extends XSBundle {
  // MDP
  // load inst will not be executed until former store (predicted by mdp) addr calcuated
  val loadWaitBit        = Bool()
  val waitForRobIdx      = new RobPtr // store set predicted previous store robIdx
}

class VirtualStoreQueueIO[PhysicalQueuePtrType <: MultiFlagCircularQueuePtr[PhysicalQueuePtrType]](PhysicalQueuePtr: PhysicalQueuePtrType)
                         (implicit p: Parameters) extends XSBundle {
  // for mulit Core Difftest
  val hartId             = Input(UInt(hartIdLen.W))
  val redirect           = Flipped(ValidIO(new Redirect))
  // from dispatch
  val enq                = new VirtualStoreQueueEnqIO(PhysicalQueuePtr)
  // from ROB
  val fromRob            = new ROBToVirtualStoreQueueIO
  // mdp query, from forward
  val mdpQuery           = Vec(LoadPipelineWidth, Flipped(ValidIO(new MDPQueryIO)))
  //to physical queue
  val toPhysicalQueue    = new VirtualStoreQueueToPhysicalQueueIO(PhysicalQueuePtr)
  val fromPhysicalQueue  = new PhysicalQueueToVirtualStoreQueueIO(PhysicalQueuePtr)
  // to lsqEnqCtrl
  val sqRecoverStall     = Output(Bool())
  val fromVMergeBuffer   = Vec(VecStorePipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO))) //TODO: will be remove in the feature

  val empty              = Output(Bool())
}
