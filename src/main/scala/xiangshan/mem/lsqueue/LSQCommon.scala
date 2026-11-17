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
import utility.{HasCircularQueuePtrHelper, HasPerfEvents}
import utils.NamedUInt
import xiangshan.{DebugOptionsKey, LSUOpType, XSBundle, XSModule}
import xiangshan.cache.HasDCacheParameters
import xiangshan.mem.HasVLSUParameters

abstract class LSQModule(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasVLSUParameters
  with HasMemBlockParameters


object MemoryType {
  def cacheable: UInt     = "b000".U
  def memoryPbmtNc: UInt  = "b001".U
  def memoryPbmtIo: UInt  = "b010".U
  def devicePbmtNc: UInt  = "b101".U
  def deviceIo: UInt      = "b111".U // device IO & pbmt device IO

  def isDeviceRegion(in: UInt): Bool = in(2) // device region
  def isMemoryRegion(in: UInt): Bool = !isDeviceRegion(in) // memory region
  def isPbmtIO(in: UInt): Bool = !in(0) && in(1)
  def isPbmtNC(in: UInt): Bool = in(0) && !in(1)
  def isCacheable(in: UInt): Bool = !in(0) && !in(1)
  def isMMIO(in: UInt):   Bool = in(1) // pbmt io and device io

  def width: Int = 3
  def apply() = UInt(width.W)
}

// CboType is a compatibility wrapper over the CBO sub-opcode defined in LSUOpType,
// keeping the original compact 2-bit encoding so that existing cboType usage is unchanged.
object CboType extends NamedUInt(2) {
  // cbo sub-opcode values are referenced from LSUOpType (single source of truth)
  def clean: UInt = LSUOpType.getCboOpcode(LSUOpType.cbo_clean)
  def flush: UInt = LSUOpType.getCboOpcode(LSUOpType.cbo_flush)
  def inval: UInt = LSUOpType.getCboOpcode(LSUOpType.cbo_inval)
  def zero:  UInt = LSUOpType.getCboOpcode(LSUOpType.cbo_zero)

  def isCboClean(in: UInt): Bool = in === this.clean
  def isCboFlush(in: UInt): Bool = in === this.flush
  def isCboInval(in: UInt): Bool = in === this.inval
  def isCboZero(in: UInt):  Bool = in === this.zero
}

