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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy

class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  class ICacheReplacerIO(implicit p: Parameters) extends ICacheBundle {
    val touch:  ReplacerTouchBundle  = Flipped(new ReplacerTouchBundle)
    val victim: ReplacerVictimBundle = Flipped(new ReplacerVictimBundle)
  }

  val io: ICacheReplacerIO = IO(new ICacheReplacerIO)

  private val replacers =
    Seq.fill(PortNumber)(ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets / PortNumber))

  // touch
  private val touchSets = Seq.fill(PortNumber)(Wire(Vec(PortNumber, UInt(log2Ceil(nSets / PortNumber).W))))
  private val touchWays = Seq.fill(PortNumber)(Wire(Vec(PortNumber, Valid(UInt(wayBits.W)))))
  (0 until PortNumber).foreach { i =>
    touchSets(i)(0) := Mux(
      io.touch.req(i).bits.vSetIdx(0),
      io.touch.req(1).bits.vSetIdx(highestIdxBit, 1),
      io.touch.req(0).bits.vSetIdx(highestIdxBit, 1)
    )
    touchWays(i)(0).bits  := Mux(io.touch.req(i).bits.vSetIdx(0), io.touch.req(1).bits.way, io.touch.req(0).bits.way)
    touchWays(i)(0).valid := Mux(io.touch.req(i).bits.vSetIdx(0), io.touch.req(1).valid, io.touch.req(0).valid)
  }

  // victim
  io.victim.resp.way := Mux(
    io.victim.req.bits.vSetIdx(0),
    replacers(1).way(io.victim.req.bits.vSetIdx(highestIdxBit, 1)),
    replacers(0).way(io.victim.req.bits.vSetIdx(highestIdxBit, 1))
  )

  // touch the victim in next cycle
  private val victimVSetIdxReg =
    RegEnable(io.victim.req.bits.vSetIdx, 0.U.asTypeOf(io.victim.req.bits.vSetIdx), io.victim.req.valid)
  private val victimWayReg = RegEnable(io.victim.resp.way, 0.U.asTypeOf(io.victim.resp.way), io.victim.req.valid)
  (0 until PortNumber).foreach { i =>
    touchSets(i)(1)       := victimVSetIdxReg(highestIdxBit, 1)
    touchWays(i)(1).bits  := victimWayReg
    touchWays(i)(1).valid := RegNext(io.victim.req.valid) && (victimVSetIdxReg(0) === i.U)
  }

  ((replacers zip touchSets) zip touchWays).foreach { case ((r, s), w) => r.access(s, w) }
}
