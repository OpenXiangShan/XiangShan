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
import utility.InstSeqNum
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, ExuOutput, UopIdx}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.NumLsElem
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.{CMOReq, CMOResp, DCacheWordReqWithVaddrAndPfFlag, UncacheWordIO}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.Bundles.{SQForward, StoreMaskBundle}


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
  val isLastRequest      = Bool() // is last request of unaligned request
  val cross4KPage        = Bool() // this unalign request is cross 4KPage
}

class StaUopInfo(implicit p: Parameters) extends MemBlockBundle {
  val sqIdx           = new SqPtr
  val fuOpType        = FuOpType()
  val robIdx          = new RobPtr

  // used in RAW check, for MDP train
  val ftqPtr          = new FtqPtr
  val ftqOffset       = UInt(log2Up(FetchBlockInstOffsetWidth).W)

  // debug info
  val pc              = Option.when(debugEn)(UInt(VAddrBits.W))
  val debugInfo       = Option.when(debugEn)(new PerfDebugInfo)
  val debug_seqNum    = Option.when(debugEn)(InstSeqNum())
}
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
  val af              = Bool() // indicate access fault.


  /* only use in cmo.zero
  * means this write request need to write whole cacheline.
  * */
  val wlineflag          = Bool() // store write the whole cache line.

  // vector
  val isvec              = Bool() // indicate vector request.

  // misalign
  val isUnsalign         = Bool()
  val unalignWith16Byte  = Bool()

  // ctrl signal
  val isLastRequest      = Bool() /* It's last request to write to storeQueue. if is normal request, it will be true,
                                      if it was unalign splited, first request will be false, second will be true. */
  val cross4KPage        = Bool() // this unalign request is cross 4KPage
}

class StoreQueueDataWrite(implicit p: Parameters) extends MemBlockBundle {
  val fuType             = FuType()
  val fuOpType           = FuOpType()
  val data               = UInt(VLEN.W)
  val sqIdx              = new SqPtr
  val vecDebug           = Option.when(debugEn)(new VecMissalignedDebugBundle)

}

class StaIO(implicit p: Parameters) extends MemBlockBundle {

  val storeMaskIn      = Vec(StorePipelineWidth, Flipped(ValidIO(new StoreMaskBundle))) // store mask, send to sq
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

class FromRobIO(implicit p: Parameters) extends XSBundle {
  val pendingPtr         = new RobPtr
  val pendingPtrNext     = new RobPtr
}

class SbufferCtrlIO(implicit p: Parameters) extends XSBundle {
  class Req(implicit p: Parameters) extends XSBundle {
    val flush            = Bool() // flush is to empty sbuffer
    val forceWrite       = Bool() // force write is to evict some sbuffer entries.
  }
  class Resp(implicit p: Parameters) extends XSBundle {
    val empty            = Bool()
  }

  val req                = Output(new Req)
  val resp               = Input(new Resp)
}

class StoreQueueToLoadQueueIO(implicit p: Parameters) extends XSBundle {
  val stAddrReadySqPtr   = new SqPtr
  val stAddrReadyVec     = Vec(StoreQueueSize, Bool())
  val stDataReadySqPtr   = new SqPtr
  val stDataReadyVec     = Vec(StoreQueueSize, Bool())

  val stIssuePtr         = new SqPtr
}

class SbufferWriteIO(implicit p : Parameters) extends XSBundle {
  val req                = Vec(EnsbufferWidth, DecoupledIO(new DCacheWordReqWithVaddrAndPfFlag))
}

class StoreQueueIO(val param: ExeUnitParams)(implicit p: Parameters) extends MemBlockBundle {
  // for mulit Core Difftest
  val hartId             = Input(UInt(hartIdLen.W))
  val redirect           = Flipped(ValidIO(new Redirect))
  // from dispatch
  val enq                = new StoreQueueEnqIO
  // when VStoreMergeBuffer writeback micro-op, storeQueue need to set `vecMbCommit`
  val fromVMergeBuffer   = Vec(VecStorePipelineWidth, Flipped(ValidIO(new FeedbackToLsqIO))) //TODO: will be remove in the feature
  // from std
  val storeDataIn        = Vec(StorePipelineWidth, Flipped(Valid(new StoreQueueDataWrite))) // store data, send to sq from rs
  // from storeUnit.
  val fromStoreUnit      = new StaIO
  // write committed store to sbuffer
  val writeToSbuffer     = new SbufferWriteIO
  // conctrl sbuffer, has two function:
  // 1. It will evict some entries of sbuffer to dcache; 2. flush sbuffer.
  val sbufferCtrl        = new SbufferCtrlIO
  // cmo handle send clean, invalid, flush to dcache.
  val toDCache           = new ToCacheIO
  // from loadUnit, forward query.
  val forward            = Flipped(Vec(LoadPipelineWidth, new SQForward))
  val fromRob            = Input(new FromRobIO)
  // write store request to uncacheBuffer.
  val toUncacheBuffer    = new UncacheWordIO
  // to backend , used to writeback uop when request is mmio, cmo.
  val writeBack          = DecoupledIO(new ExuOutput(param))
  // from misalignBuffer, will be remove in the feature
//  val maControl          = Flipped(new StoreMaBufToSqControlIO)
  val wfi                = Flipped(new WfiReqBundle)
  val sqEmpty            = Output(Bool())
  val sqFull             = Output(Bool())
  val toLoadQueue        = Output(new StoreQueueToLoadQueueIO)
  // to exceptionInfoGen, only for mmio/cbo writeback exception gen
  val exceptionInfo      = ValidIO(new MemExceptionInfo)
  // to backend, dispatch
  val sqCancelCnt        = Output(UInt(log2Up(StoreQueueSize + 1).W))
  val sqDeq              = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
  // to store unit
  val sqDeqPtr           = Output(new SqPtr)
  val sqDeqUopIdx        = Output(UopIdx())
  val sqDeqRobIdx        = Output(new RobPtr)
  // for store difftest
  val diffStore          = Option.when(debugEn)(Flipped(new DiffStoreIO))
}




