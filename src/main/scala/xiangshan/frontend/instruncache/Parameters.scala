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
import chisel3.util.log2Up
import freechips.rocketchip.amba.axi4.{AXI4BundleA, AXI4Parameters}
import xiangshan.HasXSParameter
import xscache.coupledL2.{MemBackTypeMM, MemPageTypeNC}

trait HasInstrUncacheConst {
  def MmioBusWidth: Int = 64
  def MmioBusBytes: Int = MmioBusWidth / 8

  // we can't do speculative fetch in Mmio region, so more than 1 MmioEntry should be useless?
  def nMmioEntry: Int = 1
  // Diplomacy idBits = log2Up(endId); endId=1 yields 0, but AXI requires idBits >= 1
  def nMmioAxiIdEnd: Int = math.max(2, nMmioEntry)
  def instrUncacheAxiIdBits: Int = math.max(1, log2Up(nMmioAxiIdEnd))

  def axiDenied(resp: UInt): Bool =
    resp === AXI4Parameters.RESP_SLVERR || resp === AXI4Parameters.RESP_DECERR

  def fillAxiAddr[T <: AXI4BundleA](ax: T, id: UInt, addr: UInt, lgSize: UInt,
      memBackTypeMM: Bool, pageTypeNC: Bool): Unit = {
    ax.id := id
    ax.addr := addr
    ax.len := 0.U
    ax.size := lgSize
    ax.burst := AXI4Parameters.BURST_INCR
    ax.lock := 0.U
    ax.cache := 0.U
    ax.prot := 0.U
    ax.qos := 0.U
    ax.user.lift(MemBackTypeMM).foreach(_ := memBackTypeMM)
    ax.user.lift(MemPageTypeNC).foreach(_ := pageTypeNC)
  }
}

trait HasInstrUncacheParameters extends HasXSParameter with HasInstrUncacheConst
