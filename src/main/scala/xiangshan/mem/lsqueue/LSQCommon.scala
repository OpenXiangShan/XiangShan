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
import xiangshan.{DebugOptionsKey, XSBundle, XSModule}
import xiangshan.cache.HasDCacheParameters
import xiangshan.mem.HasVLSUParameters

abstract class LSQModule(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasVLSUParameters
  with HasMemBlockParameters


object MemoryType {
  def cacheable: UInt     = "b00".U
  def pbmtNc: UInt        = "b01".U
  def pbmtIo: UInt        = "b10".U
  def io: UInt            = "b11".U // IO device

  def isPMPIO(in: UInt):  Bool = in(0) && in(1)
  def isMMIO(in: UInt):   Bool = in(1) // pbmt io and device io
  def isPbmtIO(in: UInt): Bool = !in(0) && in(1)
  def isPbmtNC(in: UInt): Bool = in(0) && !in(1)
  def isCacheable(in: UInt): Bool = !in(0) && !in(1)

  def width: Int = 2
  def apply() = UInt(width.W)
}

object CboType {
  def clean: UInt       = "b00".U
  def flush: UInt       = "b01".U
  def inval: UInt       = "b10".U
  def zero:  UInt       = "b11".U

  def isCboClean(in: UInt): Bool = in === this.clean
  def isCboFlush(in: UInt): Bool = in === this.flush
  def isCboInval(in: UInt): Bool = in === this.inval
  def isCboZero(in: UInt):  Bool = in === this.zero

  def width: Int = 2
  def apply() = UInt(width.W)
}

