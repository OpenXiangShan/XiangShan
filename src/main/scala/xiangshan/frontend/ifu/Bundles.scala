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
import xiangshan.frontend.icache.ICacheRespBundle

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

class FetchBlockInfo(implicit p: Parameters) extends IfuBundle {
  val ftqIdx:         FtqPtr      = new FtqPtr
  val doubleline:     Bool        = Bool()
  val predTakenIdx:   Valid[UInt] = Valid(UInt(IfuIdxWidth.W))
  val takenCfiOffset: Valid[UInt] = Valid(UInt(FetchBlockInstOffsetWidth.W))
  val invalidTaken:   Bool        = Bool()
  val startVAddr:     PrunedAddr  = PrunedAddr(VAddrBits)
  val target:         PrunedAddr  = PrunedAddr(VAddrBits)
  val instrRange:     UInt        = UInt(FetchBlockInstNum.W)
  val rawInstrEndVec: UInt        = UInt(FetchBlockInstNum.W)
  val pcHighPlus1:    UInt        = UInt((VAddrBits - PcCutPoint).W)
  val fetchSize:      UInt        = UInt(log2Ceil(FetchBlockInstNum + 1).W)

  def pcHigh: UInt = startVAddr(VAddrBits - 1, PcCutPoint)

  def getBasicBlockIdx(pc: PrunedAddr, start: PrunedAddr): UInt = {
    val byteOffset = (pc - start).toUInt
    (byteOffset - instBytes.U)(FetchBlockInstOffsetWidth, instOffsetBits)
  }

  def fromFtqRequest(ftqFetch: FetchRequestBundle, flush: Bool): FetchBlockInfo = {
    val cfiOffset      = ftqFetch.takenCfiOffset.bits
    val taken          = ftqFetch.takenCfiOffset.valid
    val calcInstrRange = Fill(FetchBlockInstNum, 1.U(1.W)) >> (~cfiOffset).asUInt
    val calcFetchSize  = cfiOffset + 1.U(log2Ceil(FetchBlockInstNum + 1).W)
    when(ftqFetch.valid && !taken && !flush) {
      assert(
        cfiOffset === getBasicBlockIdx(ftqFetch.nextStartVAddr, ftqFetch.startVAddr),
        "when not taken, cfiOffset must match fetch block range."
      )
    }
    ftqIdx         := ftqFetch.ftqIdx
    predTakenIdx   := DontCare // It will be overwritten later with the required value.
    invalidTaken   := DontCare // It will be overwritten later with the required value.
    rawInstrEndVec := DontCare // It will be overwritten later with the required value.
    doubleline     := ftqFetch.crossCacheline
    takenCfiOffset := ftqFetch.takenCfiOffset
    instrRange     := calcInstrRange
    startVAddr     := ftqFetch.startVAddr
    target         := ftqFetch.nextStartVAddr
    pcHighPlus1    := ftqFetch.startVAddr(VAddrBits - 1, PcCutPoint) + 1.U
    fetchSize      := calcFetchSize
    this
  }
}

// HasICacheParameters is for PortNumber
class ICacheMeta(implicit p: Parameters) extends IfuBundle with HasICacheParameters {
  val exception:          ExceptionType = new ExceptionType
  val pmpMmio:            Bool          = Bool()
  val itlbPbmt:           UInt          = UInt(Pbmt.width.W)
  val isBackendException: Bool          = Bool()
  val isForVSnonLeafPTE:  Bool          = Bool()
  val gpAddr:             PrunedAddr    = PrunedAddr(PAddrBitsMax)
  val pAddr:              PrunedAddr    = PrunedAddr(PAddrBits)

  def isUncache: Bool = pmpMmio || Pbmt.isUncache(itlbPbmt)

  def fromICacheResp(fromICache: ICacheRespBundle): ICacheMeta = {
    exception          := fromICache.exception
    pmpMmio            := fromICache.pmpMmio
    itlbPbmt           := fromICache.itlbPbmt
    isBackendException := fromICache.isBackendException
    pAddr              := fromICache.pAddr
    gpAddr             := fromICache.gpAddr
    isForVSnonLeafPTE  := fromICache.isForVSnonLeafPTE
    this
  }
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
