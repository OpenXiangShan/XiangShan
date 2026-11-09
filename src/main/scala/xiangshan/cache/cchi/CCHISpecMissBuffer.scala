/***************************************************************************************
 * Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import oceanus.compactchi.FlitREQ
import org.chipsalliance.cde.config.Parameters
import xiangshan.SpecMissABuffer

/** The DCache upREQ buffer at the MemBlock boundary.
  * S1 candidates arbitrate with ordinary requests before entering the queue.
  * S2 resolves the accepted candidates here, before L2 can observe them.
  */
class CCHISpecMissBuffer(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val normal = Flipped(Vec(nCCHIChannels, Decoupled(new FlitREQ)))
    val out = Vec(nCCHIChannels, Decoupled(new FlitREQ))
    val spec = Flipped(new SpecMissFabricIO)
  })

  val buffers = (0 until nCCHIChannels).map { ch =>
    val arb = Module(new RRArbiter(new FlitREQ, 1 + LoadPipelineWidth))
    arb.io.in(0) <> io.normal(ch)
    for (w <- 0 until LoadPipelineWidth) {
      val request = io.spec.candidate(w)
      val candidate = arb.io.in(1 + w)
      candidate.valid := request.valid && request.bits.channel === ch.U
      DCacheCCHI.Tx.missReq(
        candidate.bits,
        request.bits.id,
        request.bits.paddr,
        request.bits.alias(DCacheCCHI.tagAliasWidth - 1, 0),
        request.bits.grow,
        false.B,
        cchiDcacheSrcId(ch.U)
      )
    }

    val buffer = Module(new SpecMissABuffer(new FlitREQ, 2, LoadPipelineWidth, log2Up(cfg.nMissEntries)))
    buffer.io.enq.valid := arb.io.out.valid
    arb.io.out.ready := buffer.io.enq.ready
    buffer.io.enq.bits.data := arb.io.out.bits
    val speculative = arb.io.chosen =/= 0.U
    buffer.io.enq.bits.speculative := speculative
    buffer.io.enq.bits.committed := !speculative
    buffer.io.enq.bits.owner := Mux(speculative, arb.io.chosen - 1.U, 0.U)
    buffer.io.enq.bits.id := arb.io.out.bits.TxnID
    for (w <- 0 until LoadPipelineWidth) {
      buffer.io.resolve(w) := io.spec.resolve(w)
    }
    io.out(ch) <> buffer.io.deq
    (arb, buffer)
  }

  for (w <- 0 until LoadPipelineWidth) {
    val request = io.spec.candidate(w)
    request.ready := Mux1H((0 until nCCHIChannels).map(ch =>
      (request.bits.channel === ch.U) -> buffers(ch)._1.io.in(1 + w).ready))
    when(request.valid) {
      assert(request.bits.channel < nCCHIChannels.U, "SpecMiss selected an invalid CCHI channel")
    }
    val matched = VecInit(buffers.map(_._2.io.matched(w)))
    val committed = VecInit(buffers.map(_._2.io.committed(w)))
    when(io.spec.resolve(w).valid) {
      assert(PopCount(matched) === 1.U, "SpecMiss resolution must match one CCHI buffer slot")
    }
    assert(PopCount(committed) <= 1.U, "SpecMiss committed on multiple CCHI channels")
    io.spec.committed(w) := committed.asUInt.orR
  }
}
