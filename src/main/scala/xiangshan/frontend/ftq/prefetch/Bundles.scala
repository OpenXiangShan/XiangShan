// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.ftq.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.ftq.FtqBundle

class PrefetchQueueEntry(implicit p: Parameters) extends FtqBundle {
  val vAddr:  GuardedPc      = GuardedPc()
  val source: PrefetchSource = new PrefetchSource
}

class PrefetchSource extends Bundle {
  val value: UInt = PrefetchSource.Value()

  def isFdip: Bool = value === PrefetchSource.Value.Fdip
  def isSw:   Bool = value === PrefetchSource.Value.Sw

  def inStream: Bool = isFdip // in instruction stream, i.e. should be sent to mainPipe\ifu\backend

  def getValidSeq: Seq[(String, Bool)] = PrefetchSource.Value.getValidSeq(value)
}

object PrefetchSource {
  object Value extends EnumUInt(3) {
    def Fdip: UInt = 0.U(width.W) // Fetch-directed instruction prefetch
    def Sw:   UInt = 1.U(width.W) // Zicbop prefetch.i
    def Pdip: UInt = 2.U(width.W) // Priority-directed instruction prefetch
  }

  def apply(that: UInt): PrefetchSource = {
    val source = Wire(new PrefetchSource)
    source.value := that
    source
  }

  def Fdip: PrefetchSource = apply(Value.Fdip)
  def Sw:   PrefetchSource = apply(Value.Sw)
  def Pdip: PrefetchSource = apply(Value.Pdip)
}
