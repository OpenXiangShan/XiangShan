// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.ValidUndirectioned
import xiangshan.cache.mmu.Pbmt
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.IBufPtr
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.ftq.FtqPtr

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

class InstrIndexEntry(implicit p: Parameters) extends IfuBundle with HasIfuParameters {
  val valid: Bool = Bool()
  val value: UInt = UInt(log2Ceil(ICacheLineBytes / 2).W)
}

class FetchBlockInfo(implicit p: Parameters) extends IfuBundle {
  val ftqIdx:        FtqPtr                   = new FtqPtr
  val doubline:      Bool                     = Bool()
  val predTakenIdx:  ValidUndirectioned[UInt] = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val ftqEndOffset:  ValidUndirectioned[UInt] = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
  val invalidTaken:  Bool                     = Bool()
  val startVAddr:    PrunedAddr               = PrunedAddr(VAddrBits)
  val target:        PrunedAddr               = PrunedAddr(VAddrBits)
  val instrRange:    UInt                     = UInt(PredictWidth.W)
  val rawInstrValid: UInt                     = UInt(PredictWidth.W)
  val pcHigh:        UInt                     = UInt((VAddrBits - PcCutPoint).W)
  val pcHighPlus1:   UInt                     = UInt((VAddrBits - PcCutPoint).W)
  val fetchSize:     UInt                     = UInt(log2Ceil(PredictWidth + 1).W)
}

class ICacheInfo(implicit p: Parameters) extends IfuBundle {
  val exception:          Vec[ExceptionType] = Vec(PortNumber, new ExceptionType)
  val pmpMmio:            Bool               = Bool()
  val itlbPbmt:           UInt               = UInt(Pbmt.width.W)
  val isBackendException: Bool               = Bool()
  val isForVSnonLeafPTE:  Bool               = Bool()
  val gpAddr:             PrunedAddr         = PrunedAddr(PAddrBitsMax)
  val pAddr:              Vec[PrunedAddr]    = Vec(PortNumber, PrunedAddr(PAddrBits))
}

class FinalPredCheckResult(implicit p: Parameters) extends IfuBundle {
  val target     = PrunedAddr(VAddrBits)
  val misIdx     = Valid(UInt(log2Ceil(IBufferInPortNum).W))
  val cfiIdx     = Valid(UInt(log2Ceil(IBufferInPortNum).W))
  val instrRange = UInt(PredictWidth.W)
}

/* ***** DB ***** */
class FetchToIBufferDB(implicit p: Parameters) extends IfuBundle {
  val startAddr:  Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W)) // do not use PrunedAddr for DB
  val instrCount: UInt      = UInt(32.W)                         // magic number: just uint32_t field
  val exception:  Bool      = Bool()
  val isCacheHit: Bool      = Bool()
}

class IfuWbToFtqDB(implicit p: Parameters) extends IfuBundle {
  val startAddr:         Vec[UInt] = Vec(FetchPorts, UInt(VAddrBits.W)) // do not use PrunedAddr for DB
  val isMissPred:        Vec[Bool] = Vec(FetchPorts, Bool())
  val missPredOffset:    Vec[UInt] = Vec(FetchPorts, UInt(log2Ceil(PredictWidth).W))
  val checkJalFault:     Bool      = Bool()
  val checkJalrFault:    Bool      = Bool()
  val checkRetFault:     Bool      = Bool()
  val checkTargetFault:  Bool      = Bool()
  val checkNotCFIFault:  Bool      = Bool()
  val checkInvalidTaken: Bool      = Bool()
}

class IfuRedirectInternal(implicit p: Parameters) extends IfuBundle {
  val valid:          Bool    = Bool()
  val instrCount:     UInt    = UInt(log2Ceil(PredictWidth + 1).W)
  val prevIBufEnqPtr: IBufPtr = new IBufPtr
}
