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

package xiangshan.frontend.instruncache

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr

class InstrUncacheReq(implicit p: Parameters) extends InstrUncacheBundle {
  val addr:          PrunedAddr = PrunedAddr(PAddrBits)
  val memBackTypeMM: Bool       = Bool() // !pmp.mmio, pbmt.nc/io on a main memory region
  val memPageTypeNC: Bool       = Bool() // pbmt.nc
}

class InstrUncacheResp(implicit p: Parameters) extends InstrUncacheBundle {
  val data:    UInt = UInt(32.W) // TODO: add a const for InstrLen, maybe in XSParameters, and use it all over the repo
  val corrupt: Bool = Bool()
  val incomplete: Bool = Bool() // whether this.data is incomplete (e.g. crossing a page boundary)
}
