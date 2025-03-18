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
import org.chipsalliance.cde.config.Parameters
import utils.NamedUInt
import xiangshan.frontend.PrunedAddr

/* ***
 * Naming:
 * - I/O:
 *   - Ifu inner use only: xxxBundle
 *   - Other modules use: IfuXxxBundle, consider move to FrontendBundle.scala
 * - Sram/register: xxxEntry
 *
 * Try avoiding directed Bundle, unless it's req-resp pair
 * *** */

/* ***** PreDecode ***** */
object PreDecodeFaultType extends NamedUInt(3) {
  def noFault:      UInt = "b000".U(width.W)
  def jalFault:     UInt = "b001".U(width.W) // not CFI taken or invalid instruction taken
  def retFault:     UInt = "b010".U(width.W) // not CFI taken or invalid instruction taken
  def targetFault:  UInt = "b011".U(width.W)
  def notCfiFault:  UInt = "b100".U(width.W) // not CFI taken or invalid instruction taken
  def invalidTaken: UInt = "b101".U(width.W)
  def jalrFault:    UInt = "b110".U(width.W)
}

/* ***** Ifu last half ***** */
// record the situation in which fallThruAddr falls into the middle of an RVI inst
class LastHalfEntry(implicit p: Parameters) extends IfuBundle {
  val valid:    Bool       = Bool()
  val middlePC: PrunedAddr = PrunedAddr(VAddrBits)
}

/* ***** DB ***** */
class FetchToIBufferDB(implicit p: Parameters) extends IfuBundle {
  val start_addr:   PrunedAddr = PrunedAddr(VAddrBits)
  val instr_count:  UInt       = UInt(32.W)
  val exception:    Bool       = Bool()
  val is_cache_hit: Bool       = Bool()
}

class IfuWbToFtqDB(implicit p: Parameters) extends IfuBundle {
  val start_addr:        PrunedAddr = PrunedAddr(VAddrBits)
  val is_miss_pred:      Bool       = Bool()
  val miss_pred_offset:  UInt       = UInt(32.W)
  val checkJalFault:     Bool       = Bool()
  val checkJalrFault:    Bool       = Bool()
  val checkRetFault:     Bool       = Bool()
  val checkTargetFault:  Bool       = Bool()
  val checkNotCFIFault:  Bool       = Bool()
  val checkInvalidTaken: Bool       = Bool()
}
