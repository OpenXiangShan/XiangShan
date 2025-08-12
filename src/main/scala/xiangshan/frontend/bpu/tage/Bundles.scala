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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteReqBundle

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:  Bool            = Bool()
  val tag:    UInt            = UInt(TagWidth.W)
  val ctr:    SaturateCounter = new SaturateCounter(CtrWidth)
  val useful: SaturateCounter = new SaturateCounter(UsefulWidth)
}

class BaseTableSramWriteReq(implicit p: Parameters) extends WriteReqBundle with HasTageParameters {
  val setIdx:   UInt                 = UInt(BaseTableSetIdxLen.W)
  val ctrs:     Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableCtrWidth))
  val waymasks: UInt                 = UInt(FetchBlockAlignInstNum.W)
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val valid:               Bool                 = Bool()
  val baseTableCtrs:       Vec[SaturateCounter] = Vec(FetchBlockInstNum, new SaturateCounter(BaseTableCtrWidth))
  val debug_taken:         Bool                 = Bool()
  val debug_takenPosition: UInt                 = UInt(FetchBlockInstNum.W)
}
