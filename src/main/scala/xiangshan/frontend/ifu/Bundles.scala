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

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.cache.mmu.Pbmt
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.Pc
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ibuffer.IBufPtr
import xiangshan.frontend.icache.FetchBlocktoIfuReq
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.MainPipeToIfuReq

/* ***
 * Naming:
 * - I/O:
 *   - Ifu inner use only: xxxBundle
 *   - Other modules use: IfuXxxBundle, consider move to Bundles.scala
 * - Sram/register: xxxEntry
 *
 * Try avoiding directed Bundle, unless it's req-resp pair
 * *** */

/* ***** PreDecode ***** */
object PreDecodeFaultType extends EnumUInt(7) {
  def NoFault:      UInt = 0.U(width.W)
  def JalFault:     UInt = 1.U(width.W) // not CFI taken or invalid instruction taken
  def RetFault:     UInt = 2.U(width.W) // not CFI taken or invalid instruction taken
  def TargetFault:  UInt = 3.U(width.W)
  def NotCfiFault:  UInt = 4.U(width.W) // not CFI taken or invalid instruction taken
  def InvalidTaken: UInt = 5.U(width.W)
  def JalrFault:    UInt = 6.U(width.W)
}

/* ***** Ifu last half ***** */
// record the situation in which fallThruAddr falls into the middle of an RVI inst
class LastHalfEntry(implicit p: Parameters) extends IfuBundle {
  val valid:    Bool = Bool()
  val middlePC: Pc   = Pc()
}

class EndHalfRviInfo(implicit p: Parameters) extends IfuBundle {
  val isHalfRvi: Bool       = Bool()
  val pc:        PrunedAddr = PrunedAddr(VAddrBits)
  val data:      UInt       = UInt(16.W)
}

class InstrIndexEntry(implicit p: Parameters) extends IfuBundle {
  val valid: Bool = Bool()
  val value: UInt = UInt(log2Ceil(ICacheLineBytes / 2).W)
}

class FetchBlock(implicit p: Parameters) extends IfuBundle {
  val valid:          Bool        = Bool()
  val ftqIdx:         FtqPtr      = new FtqPtr
  val startVAddr:     GuardedPc   = GuardedPc()
  val takenCfiOffset: Valid[UInt] = Valid(UInt(FetchBlockInstOffsetWidth.W))
  // val range:          UInt        = UInt(FetchBlockInstNum.W)
  val size: UInt = UInt(log2Ceil(FetchBlockInstNum + 1).W)

  val pcUpperBitsPlus1: UInt = UInt((GuardedVAddrBits - PcCutPoint).W)

  def pcUpperBits: UInt = startVAddr(GuardedVAddrBits - 1, PcCutPoint)

  def fromICacheReq(req: FetchBlocktoIfuReq): FetchBlock = {
    valid          := req.valid
    ftqIdx         := req.ftqIdx
    startVAddr     := req.startVAddr
    takenCfiOffset := req.takenCfiOffset
    // range            := req.range
    size             := req.size
    pcUpperBitsPlus1 := req.startVAddr(GuardedVAddrBits - 1, PcCutPoint) + 1.U
    this
  }
}
class IfuData(implicit p: Parameters) extends IfuBundle with HasICacheParameters {
  val index:       Vec[UInt] = Vec(FetchBlockInstNum, UInt(log2Ceil(FetchBlockInstNum).W))
  val maybeRvcMap: UInt      = UInt(FetchBlockInstNum.W)
  val firstRange:  UInt      = UInt(FetchBlockInstNum.W)
  val totalRange:  UInt      = UInt(FetchBlockInstNum.W)
  val blockSel:    UInt      = UInt(FetchBlockInstNum.W)

  def fromICacheReq(req: MainPipeToIfuReq): IfuData = {
    val reqStartOffset = req.info.map(_.startVAddr(5, 1))

    val dupData = VecInit((0 until MaxFetchReqNum).map { i =>
      Cat(req.info(i).data, req.info(i).data).asTypeOf(Vec(FetchBlockInstNum * 2, UInt(16.W)))
    })

    def getDataIndex(i: Int): (Bool, UInt, UInt) = {
      val fromReq0 = i.U < req.info(0).size
      val req0Idx  = (reqStartOffset(0) +& i.U)(log2Ceil(FetchBlockInstNum * 2) - 1, 0)
      val req1Idx  = (reqStartOffset(1) +& (i.U - req.info(0).size))(log2Ceil(FetchBlockInstNum * 2) - 1, 0)
      (fromReq0, req0Idx, req1Idx)
    }

    this.index := VecInit((0 until FetchBlockInstNum).map { i =>
      val (fromReq0, req0Idx, req1Idx) = getDataIndex(i)
      Mux(fromReq0, req0Idx, req1Idx)
    })

    this.maybeRvcMap := req.maybeRvcMap
    this.firstRange  := req.firstRange
    this.totalRange  := req.totalRange
    this.blockSel :=
      VecInit((0 until FetchBlockInstNum).map(i => req.info(1).valid && !firstRange(i))).asUInt

    this
  }

}

class InstSlot extends Bundle {
  val valid = Bool()
  val isRvc = Bool()
  val inst  = UInt(32.W)
}
class Instruction(implicit p: Parameters) extends IfuBundle with HasICacheParameters {
  val valid:             Bool = Bool()
  val data:              UInt = UInt(32.W)
  val isRvc:             Bool = Bool()
  val isPredTaken:       Bool = Bool()
  val invalidTaken:      Bool = Bool()
  val blockSel:          Bool = Bool() // 0: first block, 1: second block
  val startOffset:       UInt = UInt(FetchBlockInstOffsetWidth.W)
  val endOffset:         UInt = UInt(FetchBlockInstOffsetWidth.W)
  val isPrevEndHalfRvi:  Bool = Bool()
  val isCrossBlockInstr: Bool = Bool()
  // Compatible with miniConfig
  val index: UInt = UInt(log2Ceil(ICacheLineBytes / 2).W)
}

class PredCheckRedirect(implicit p: Parameters) extends IfuBundle {
  val target:       GuardedPc       = GuardedPc()
  val misIdx:       Valid[UInt]     = Valid(UInt(log2Ceil(IBufferEnqueueWidth).W))
  val taken:        Bool            = Bool()
  val invalidTaken: Bool            = Bool()
  val notCfiTaken:  Bool            = Bool()
  val isRVC:        Bool            = Bool()
  val blockSel:     Bool            = Bool()
  val attribute:    BranchAttribute = new BranchAttribute
  val mispredPc:    Pc              = Pc()
  val endOffset:    UInt            = UInt(FetchBlockInstOffsetWidth.W)
  val isCrossBlockInstr: Bool       = Bool()
}

/* ***** DB ***** */
class FetchToIBufferDB(implicit p: Parameters) extends IfuBundle {
  val startAddr:  Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W)) // do not use PrunedAddr for DB
  val instrCount: UInt      = UInt(32.W)                         // magic number: just uint32_t field
  val exception:  Bool      = Bool()
  val isCacheHit: Bool      = Bool()
}

class IfuWbToFtqDB(implicit p: Parameters) extends IfuBundle {
  val startAddr:         Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W))
  val misId:             UInt      = UInt(log2Ceil(FetchPorts).W)
  val isMispred:         Bool      = Bool()
  val misPredOffset:     UInt      = UInt(FetchBlockInstOffsetWidth.W)
  val checkJalFault:     Bool      = Bool()
  val checkJalrFault:    Bool      = Bool()
  val checkRetFault:     Bool      = Bool()
  val checkTargetFault:  Bool      = Bool()
  val checkNotCFIFault:  Bool      = Bool()
  val checkInvalidTaken: Bool      = Bool()
}

class IfuRedirectInternal(implicit p: Parameters) extends IfuBundle {
  val valid:          Bool    = Bool()
  val instrCount:     UInt    = UInt(log2Ceil(FetchBlockInstNum + 1).W)
  val prevIBufEnqPtr: IBufPtr = new IBufPtr
  // A fallthrough does not always correspond to a half RVI instruction.
  val isHalfInstr: Bool      = Bool()
  val halfPc:      GuardedPc = GuardedPc()
  val halfData:    UInt      = UInt(16.W)
}

class InstrCompactBundle(width: Int)(implicit p: Parameters) extends IfuBundle {
  val instrIndex:     Vec[InstrIndexEntry] = Vec(width, new InstrIndexEntry)
  val instrIsRvc:     Vec[Bool]            = Vec(width, Bool())
  val selectBlock:    Vec[Bool]            = Vec(width, Bool())
  val instrPcLower:   Vec[UInt]            = Vec(width, UInt((PcCutPoint + 1).W))
  val instrEndOffset: Vec[UInt]            = Vec(width, UInt(log2Ceil(FetchBlockInstNum).W))
}
