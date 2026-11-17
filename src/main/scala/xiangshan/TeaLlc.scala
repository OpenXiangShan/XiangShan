/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.bore
import org.chipsalliance.cde.config.Parameters
import system.SoCParamsKey
import xscache.openLLC.OpenLLC

// Six 64-bit words, shared with TeaNativeLlcTracker::Event. Zero kind means invalid.
class TeaLlcEvent extends Bundle {
  val kind = UInt(64.W)
  val id = UInt(64.W)
  val a = UInt(64.W)
  val b = UInt(64.W)
  val c = UInt(64.W)
  val d = UInt(64.W)
}

class TeaLlcTrace(implicit p: Parameters) extends XSBundle {
  private val reqs = MissReqPortCount
  private val banks = coreParams.L2NBanks
  private val llcBanks = p(SoCParamsKey).OpenLLCParamsOpt.map(_.banks).getOrElse(0)
  val supported = Input(Bool())
  val events = Input(Vec(reqs + LoadPipelineWidth + 1 + 3 * banks + llcBanks, new TeaLlcEvent))
}

object TeaLlcTrace {
  def sink(implicit p: Parameters): TeaLlcTrace = {
    val trace = Wire(new TeaLlcTrace)
    trace := 0.U.asTypeOf(trace)
    trace
  }
}

class TeaLlcRecord extends Bundle {
  val cycle = UInt(64.W)
  val event = new TeaLlcEvent
}

// Read-only probes, installed only by the integrated OpenLLC simulation top.
// Functional cache bundles and their source/transaction IDs remain unchanged.
object TeaLlcObserver {
  def connect(tile: XSTile, llc: OpenLLC, rn: Int): Unit = {
    val rob = tile.core.backend.inner.ctrlBlock.rob.module
    val trace = bore(rob.teaLlc.get)
    trace := 0.U.asTypeOf(trace)
    trace.supported := true.B
    val dc = tile.memBlock.dcache.dcache.module
    val mq = dc.missQueue
    val loads = tile.memBlock.module.newLoadUnits
    val l2 = tile.l2top.inner.l2cache.get.module
    require(mq.entries.size <= 64 && l2.slices.forall(_.mshrCtl.mshrs.size <= 64))
    val events = scala.collection.mutable.ArrayBuffer.empty[TeaLlcEvent]
    def emit(valid: Bool, kind: Int, id: UInt, args: UInt*): Unit = {
      val e = Wire(new TeaLlcEvent)
      e := 0.U.asTypeOf(e)
      e.kind := Mux(valid, kind.U, 0.U)
      e.id := id
      Seq(e.a, e.b, e.c, e.d).zip(args).foreach { case (sink, src) => sink := src }
      events += e
    }
    // Allocation is observed at the accepted S0 handoff, including non-load owners.
    mq.io.resp.indices.foreach { i =>
      val cancelled = bore(mq.io.queryMQ(i).req.bits.cancel)
      val r = bore(mq.io.resp(i))
      emit(r.handled && r.allocated && !cancelled && !bore(mq.io.wbq_block_miss_req(i)),
        1, r.id)
    }
    dc.ldu.zip(loads).foreach { case (cache, load) =>
      // Bore only output leaves: a Decoupled bundle also contains the reverse ready input.
      val responseValid = bore(cache.io.lsu.resp.valid)
      val handled = bore(cache.io.lsu.resp.bits.handled)
      val request = bore(cache.io.miss_req.bits)
      val pointer = bore(load.s2.uop.robIdx)
      val rank = bore(load.s2.uop.perfDebugInfo.teaMemberRank)
      val member = bore(load.s2.in.hasROBEntry)
      val kill = bore(load.s2.kill)
      val demand = !bore(load.s2.isPrefetch) && bore(load.s2.needDCacheAccess)
      emit(responseValid && handled && request.isFromLoad && member && !kill && demand,
        2, bore(cache.io.miss_resp.id), pointer.value, pointer.flag, pointer.isFormer, rank)
    }
    emit(true.B, 3, 0.U, VecInit(mq.entries.map(e => bore(e.release_entry) && bore(e.req_valid))).asUInt)
    l2.slices.zipWithIndex.foreach { case (slice, bank) =>
      val ctl = slice.mshrCtl
      val allocation = bore(ctl.io.fromMainPipe.mshr_alloc_s3)
      val merge = bore(ctl.io.aMergeTask)
      // Diplomacy remaps each client's source range at the L2 input.
      val clients = slice.edgeIn.client.clients.filter(c =>
        c.name == "dcache" || c.name == "dcache_ch1")
      def source(sourceId: UInt): UInt = {
        val result = WireDefault("hffffffffffffffff".U(64.W))
        clients.foreach { client =>
          when(sourceId >= client.sourceId.start.U && sourceId < client.sourceId.end.U) {
            result := sourceId - client.sourceId.start.U
          }
        }
        result
      }
      def txn(id: UInt): UInt = l2.setSliceID(id.pad(l2.TXNID_WIDTH), bank.U, false.B)
      // Locally generated L2 Hint tasks have a prefetcher source, not an L1 source ID.
      val fromL1 = allocation.bits.task.fromA &&
        allocation.bits.task.opcode =/= freechips.rocketchip.tilelink.TLMessages.Hint
      emit(allocation.valid, 4, txn(bore(ctl.io.toMainPipe.mshr_alloc_ptr)),
        Mux(fromL1, source(allocation.bits.task.sourceId), "hffffffffffffffff".U))
      emit(merge.valid, 5, txn(OHToUInt(merge.bits.idOH)), source(merge.bits.task.sourceId))
      emit(true.B, 7, txn(0.U), VecInit(ctl.mshrs.map(m =>
        bore(m.io.status.valid) && bore(m.io.status.bits.will_free))).asUInt)
    }
    llc.slices.foreach { slice =>
      val pipe = slice.mainPipe
      val task = bore(pipe.task_s3)
      emit(bore(pipe.l3DemandRead_s3) && task.bits.srcID === rn.U,
        6, task.bits.txnID, !bore(pipe.self_hit_s3))
    }
    require(events.size == trace.events.size)
    trace.events := VecInit(events.toSeq)
  }
}
