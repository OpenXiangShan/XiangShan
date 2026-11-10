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
import utils.VecRotate

class ICacheReplacer(implicit p: Parameters) extends ICacheModule {
  class ICacheReplacerIO(implicit p: Parameters) extends ICacheBundle {
    val touch:  ReplacerTouchBundle  = Flipped(new ReplacerTouchBundle)
    val victim: ReplacerVictimBundle = Flipped(new ReplacerVictimBundle)
  }

  private def NumSetsPerPort = nSets / PortNumber
  private def SetIdxWidth    = log2Ceil(NumSetsPerPort)

  private def getReplacerId(vSetIdx:  UInt): UInt = vSetIdx(log2Ceil(PortNumber) - 1, 0)
  private def getReplacerSet(vSetIdx: UInt): UInt = vSetIdx(idxBits - 1, log2Ceil(PortNumber))

  val io: ICacheReplacerIO = IO(new ICacheReplacerIO)

  // for each FetchReq, rotate its 2 ports (so even vSetIdx becomes physical idx 0, odd becomes idx 1), then transpose
  // e.g. 2 FetchReqs requesting touch 4 sets: (0, 1) from fb(0) and (5, 6) from fb(1)
  //      we do a vecRotate (Mux) first: (0, 1) and (6, 5)
  //      then to a transpose for each replacer: (0, 6) for replacer(0) and (1, 5) for replacer(1)
  private val touches = io.touch.req.map(req => VecRotate(getReplacerId(req(0).bits.vSetIdx)).rotate(req)).transpose

  // latch victim info to touch it in the next cycle
  private val victimVSetIdx = RegEnable(io.victim.req.bits.vSetIdx, io.victim.req.valid)
  private val victimWay     = RegEnable(io.victim.resp.way, io.victim.req.valid)
  private val victimValid   = RegNext(io.victim.req.valid)

  private val replacers = Seq.tabulate(PortNumber) { idx =>
    val replacer = ReplacementPolicy.fromString(Replacer, nWays, NumSetsPerPort)

    // PortNumber touch access + 1 victim access
    val sets = Seq.fill(PortNumber + 1)(Wire(UInt(SetIdxWidth.W)))
    val ways = Seq.fill(PortNumber + 1)(Wire(Valid(UInt(wayBits.W))))

    (sets.init lazyZip ways.init lazyZip touches(idx)).foreach { case (set, way, touch) =>
      set       := getReplacerSet(touch.bits.vSetIdx)
      way.bits  := touch.bits.way
      way.valid := touch.valid
    }

    sets.last       := getReplacerSet(victimVSetIdx)
    ways.last.bits  := victimWay
    ways.last.valid := victimValid && idx.U === getReplacerId(victimVSetIdx)

    // generate access logic
    replacer.access(sets, ways)
    replacer
  }

  // send victim to missUnit
  io.victim.resp.way := Mux1H(replacers.zipWithIndex.map { case (replacer, idx) =>
    (idx.U === getReplacerId(io.victim.req.bits.vSetIdx)) -> replacer.way(getReplacerSet(io.victim.req.bits.vSetIdx))
  })
}
