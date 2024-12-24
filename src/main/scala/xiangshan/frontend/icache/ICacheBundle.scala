/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class ICacheReadBundle(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:      Vec[UInt]      = Vec(2, UInt(idxBits.W))
  val waymask:      Vec[Vec[Bool]] = Vec(2, Vec(nWays, Bool()))
  val blkOffset:    UInt           = UInt(log2Ceil(blockBytes).W)
  val isDoubleLine: Bool           = Bool()
}

class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val phyTag:  UInt = UInt(tagBits.W)
  val waymask: UInt = UInt(nWays.W)
  val bankIdx: Bool = Bool()
  val poison:  Bool = Bool()

  def generate(tag: UInt, idx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
    this.virIdx  := idx
    this.phyTag  := tag
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.poison  := poison
  }
}

class ICacheMetaFlushBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val waymask: UInt = UInt(nWays.W)
}

class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val data:    UInt = UInt(blockBits.W)
  val waymask: UInt = UInt(nWays.W)
  val bankIdx: Bool = Bool()
  val poison:  Bool = Bool()

  def generate(data: UInt, idx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
    this.virIdx  := idx
    this.data    := data
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.poison  := poison
  }
}

class ICacheMetaRespBundle(implicit p: Parameters) extends ICacheBundle {
  val metas:      Vec[Vec[ICacheMetadata]] = Vec(PortNumber, Vec(nWays, new ICacheMetadata))
  val codes:      Vec[Vec[UInt]]           = Vec(PortNumber, Vec(nWays, UInt(ICacheMetaCodeBits.W)))
  val entryValid: Vec[Vec[Bool]]           = Vec(PortNumber, Vec(nWays, Bool()))

  // for compatibility
  def tags: Vec[Vec[UInt]] = VecInit(metas.map(port => VecInit(port.map(way => way.tag))))
}

class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle {
  val datas: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataBits.W))
  val codes: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataCodeBits.W))
}

class ReplacerTouch(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx: UInt = UInt(idxBits.W)
  val way:     UInt = UInt(wayBits.W)
}

class ReplacerVictim(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx: Valid[UInt] = ValidIO(UInt(idxBits.W))
  val way:     UInt        = Input(UInt(wayBits.W))
}
