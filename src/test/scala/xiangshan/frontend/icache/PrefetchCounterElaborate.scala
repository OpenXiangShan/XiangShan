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

package xiangshan.frontend.icache

import chisel3._
import chisel3.experimental.UnlocatableSourceInfo
import circt.stage.ChiselStage
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.diplomacy.IdRange
import freechips.rocketchip.diplomacy.TransferSizes
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility.ChiselDB
import utility.LogPerfIO
import utility.XSLog
import xiangshan.DebugOptionsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import xiangshan.frontend.ftq.prefetch.Pdip
import xiangshan.frontend.ftq.prefetch.PrefetchQueue

/** Small elaboration target for counter wiring; does not construct a core or an emulator. */
object PrefetchCounterElaborate extends App {
  private val (config, _, _) = ArgParser.parse(Array("--config", "DefaultConfig"))
  private val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(EnableDifftest = false, AlwaysBasicDiff = false)
  }
  ChiselDB.init(false)

  private class Harness(implicit val p: Parameters) extends Module with HasICacheParameters {
    private val client = TLMasterPortParameters.v1(
      Seq(TLMasterParameters.v1(name = "prefetchCounterTest", sourceId = IdRange(0, NumAllMshr)))
    )
    private val manager = TLSlavePortParameters.v1(
      Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0, (BigInt(1) << PAddrBits) - 1)),
        supportsGet = TransferSizes(1, blockBytes)
      )),
      beatBytes = 32
    )
    private val edge = new TLEdgeOut(client, manager, p, UnlocatableSourceInfo)

    val pdip = Module(new Pdip)
    val pdipIO = IO(chiselTypeOf(pdip.io))
    pdipIO <> pdip.io
    val queue = Module(new PrefetchQueue)
    val queueIO = IO(chiselTypeOf(queue.io))
    queueIO <> queue.io
    val prefetch = Module(new ICachePrefetchPipe)
    val prefetchIO = IO(chiselTypeOf(prefetch.io))
    prefetchIO <> prefetch.io
    val mainPipe = Module(new ICacheMainPipe)
    val mainIO = IO(chiselTypeOf(mainPipe.io))
    mainIO <> mainPipe.io
    val miss = Module(new ICacheMissUnit(edge))
    val missIO = IO(chiselTypeOf(miss.io))
    missIO <> miss.io
    val perf = IO(Input(new LogPerfIO))
    XSLog.collect(perf)
  }

  ChiselStage.emitSystemVerilogFile(new Harness()(params), args)
}
