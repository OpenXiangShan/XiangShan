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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ibuffer.IBufPtr
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
  val valid:    Bool       = Bool()
  val middlePC: PrunedAddr = PrunedAddr(VAddrBits)
}

class InstrIndexEntry(implicit p: Parameters) extends IfuBundle {
  val valid: Bool = Bool()
  val value: UInt = UInt(log2Ceil(ICacheLineBytes / 2).W)
}

class FetchBlock(implicit p: Parameters) extends IfuBundle {
  val valid:          Bool        = Bool()
  val ftqIdx:         FtqPtr      = new FtqPtr
  val startVAddr:     PrunedAddr  = PrunedAddr(VAddrBits)
  val takenCfiOffset: Valid[UInt] = Valid(UInt(FetchBlockInstOffsetWidth.W))
  val range:          UInt        = UInt(FetchBlockInstNum.W)
  val size:           UInt        = UInt(log2Ceil(FetchBlockInstNum + 1).W)

  val pcUpperBitsPlus1: UInt = UInt((VAddrBits - PcCutPoint).W)

  def pcUpperBits: UInt = startVAddr(VAddrBits - 1, PcCutPoint)

  def fromICacheReq(req: MainPipeToIfuReq): FetchBlock = {
    valid            := req.valid
    ftqIdx           := req.ftqIdx
    startVAddr       := req.startVAddr
    takenCfiOffset   := req.takenCfiOffset
    range            := req.range
    size             := req.size
    pcUpperBitsPlus1 := req.startVAddr(VAddrBits - 1, PcCutPoint) + 1.U
    this
  }
}
class IfuData(implicit p: Parameters) extends IfuBundle with HasICacheParameters {
  val data:        Vec[UInt] = Vec(FetchBlockInstNum, UInt(16.W))
  val maybeRvcMap: UInt      = UInt(FetchBlockInstNum.W)
  val range:       UInt      = UInt(FetchBlockInstNum.W)
  val blockSel:    UInt      = UInt(FetchBlockInstNum.W)

  def fromICacheReq(req: Vec[MainPipeToIfuReq]): IfuData = {
    val reqStartOffset = req.map(_.startVAddr(5, 1))

    val totalSize = req(0).size +& Mux(req(1).valid, req(1).size, 0.U.asTypeOf(req(1).size))

    val dupData = VecInit((0 until MaxFetchReqNum).map { i =>
      Cat(req(i).data, req(i).data).asTypeOf(Vec(FetchBlockInstNum * 2, UInt(16.W)))
    })
    val dupMaybeRvcMap = VecInit((0 until MaxFetchReqNum).map(i => Cat(req(i).maybeRvcMap, req(i).maybeRvcMap)))

    def getDataIndex(i: Int): (Bool, UInt, UInt) = {
      val fromReq0 = i.U < req(0).size
      val req0Idx  = (reqStartOffset(0) +& i.U)(log2Ceil(FetchBlockInstNum * 2) - 1, 0)
      val req1Idx  = (reqStartOffset(1) +& (i.U - req(0).size))(log2Ceil(FetchBlockInstNum * 2) - 1, 0)
      (fromReq0, req0Idx, req1Idx)
    }

    this.data := VecInit((0 until FetchBlockInstNum).map { i =>
      val (fromReq0, req0Idx, req1Idx) = getDataIndex(i)
      Mux(fromReq0, dupData(0)(req0Idx), Mux(req(1).valid, dupData(1)(req1Idx), 0.U.asTypeOf(dupData(1)(req1Idx))))
    })

    this.maybeRvcMap := VecInit((0 until FetchBlockInstNum).map { i =>
      val (fromReq0, req0Idx, req1Idx) = getDataIndex(i)
      Mux(fromReq0, dupMaybeRvcMap(0)(req0Idx), Mux(req(1).valid, dupMaybeRvcMap(1)(req1Idx), 0.U(1.W)))
    }).asUInt

    this.range    := VecInit((0 until FetchBlockInstNum).map(i => i.U < totalSize)).asUInt
    this.blockSel := VecInit((0 until FetchBlockInstNum).map(i => req(1).valid && i.U >= req(0).size)).asUInt

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
}

class PredCheckRedirect(implicit p: Parameters) extends IfuBundle {
  val target:       PrunedAddr      = PrunedAddr(VAddrBits)
  val misIdx:       Valid[UInt]     = Valid(UInt(log2Ceil(IBufferEnqueueWidth).W))
  val taken:        Bool            = Bool()
  val invalidTaken: Bool            = Bool()
  val isRVC:        Bool            = Bool()
  val selectBlock:  Bool            = Bool()
  val attribute:    BranchAttribute = new BranchAttribute
  val mispredPc:    PrunedAddr      = PrunedAddr(VAddrBits)
  val endOffset:    UInt            = UInt(FetchBlockInstOffsetWidth.W)
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
  val isHalfInstr: Bool       = Bool()
  val halfPc:      PrunedAddr = PrunedAddr(VAddrBits)
  val halfData:    UInt       = UInt(16.W)
}

class InstrCompactBundle(width: Int)(implicit p: Parameters) extends IfuBundle {
  val instrIndex:     Vec[InstrIndexEntry] = Vec(width, new InstrIndexEntry)
  val instrIsRvc:     Vec[Bool]            = Vec(width, Bool())
  val selectBlock:    Vec[Bool]            = Vec(width, Bool())
  val instrPcLower:   Vec[UInt]            = Vec(width, UInt((PcCutPoint + 1).W))
  val instrEndOffset: Vec[UInt]            = Vec(width, UInt(log2Ceil(FetchBlockInstNum).W))
}
