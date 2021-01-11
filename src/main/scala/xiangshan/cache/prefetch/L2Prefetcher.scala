package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import xiangshan.cache._
import utils._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters,
  TLMasterParameters, TLMasterPortParameters, TLArbiter,
  TLEdgeOut, TLBundleA, TLBundleD,
  ClientStates, ClientMetadata, TLHints
}

case class L2PrefetcherParameters(
  enable: Boolean,
  _type: String,
  streamParams: StreamPrefetchParameters
) {
  def nEntries: Int = streamParams.streamCnt * streamParams.streamSize
}

class L2Prefetcher()(implicit p: Parameters) extends LazyModule with HasPrefetchParameters {
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "l2prefetcher",
      sourceId = IdRange(0, l2PrefetcherParameters.nEntries)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new L2PrefetcherImp(this)
}

// prefetch DCache lines in L2 using StreamPrefetch
class L2PrefetcherImp(outer: L2Prefetcher) extends LazyModuleImp(outer) with HasPrefetchParameters with HasXSLog {  
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MissReq))
    // prefetch
    // val mem_acquire = Decoupled(new TLBundleA(edge.bundle))
    // val mem_grant   = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    // val mem_finish  = Decoupled(new TLBundleE(edge.bundle))
  })

  val (bus, edge) = outer.clientNode.out.head
  if (l2PrefetcherParameters.enable && l2PrefetcherParameters._type == "stream") {
    val streamParams = l2PrefetcherParameters.streamParams
    val dPrefetch = Module(new StreamPrefetch(streamParams))
    dPrefetch.io.train.valid := io.in.fire()
    dPrefetch.io.train.bits.addr := io.in.bits.addr
    dPrefetch.io.train.bits.write := MemoryOpConstants.isWrite(io.in.bits.cmd)
    dPrefetch.io.train.bits.miss := true.B
    io.in.ready := true.B

    bus.a.valid := dPrefetch.io.req.valid
    bus.a.bits := DontCare
    bus.a.bits := edge.Hint(
      fromSource = dPrefetch.io.req.bits.id,
      toAddress = dPrefetch.io.req.bits.addr,
      lgSize = log2Up(streamParams.blockBytes).U,
      param = Mux(dPrefetch.io.req.bits.write, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ) // TODO
    )._2
    dPrefetch.io.req.ready := bus.a.ready

    bus.b.ready := true.B

    bus.c.valid := false.B
    bus.c.bits := DontCare

    dPrefetch.io.resp.valid := bus.d.valid
    dPrefetch.io.resp.bits.id := bus.d.bits.source(streamParams.totalWidth - 1, 0)
    bus.d.ready := dPrefetch.io.resp.ready

    bus.e.valid := false.B
    bus.e.bits := DontCare
    dPrefetch.io.finish.ready := true.B

  } else {
    bus.a.valid := false.B
    bus.a.bits := DontCare
    bus.b.ready := true.B
    bus.c.valid := false.B
    bus.c.bits := DontCare
    bus.d.ready := true.B
    bus.e.valid := false.B
    bus.e.bits := DontCare
  }
}

