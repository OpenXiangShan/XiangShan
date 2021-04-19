package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientMetadata
import xiangshan._
import xiangshan.cache._
import utils.{XSPerfAccumulate, _}
import chisel3.ExcitingUtils._

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters,
  TLMasterParameters, TLMasterPortParameters, TLArbiter,
  TLEdgeOut, TLBundleA, TLBundleD,
  ClientStates, ClientMetadata, TLHints
}
import sifive.blocks.inclusivecache.PrefetcherIO

case class L2PrefetcherParameters(
  enable: Boolean,
  _type: String,
  streamParams: StreamPrefetchParameters,
  bopParams: BOPParameters
) {
  // def nEntries: Int = streamParams.streamCnt * streamParams.streamSize
  def nEntries: Int = {
    if (enable && _type == "stream") { streamParams.streamCnt * streamParams.streamSize }
    else if (enable && _type == "bop") { bopParams.nEntries }
    else 1
  }
  def totalWidth: Int = {
    if (enable && _type == "stream") streamParams.totalWidth
    else if (enable && _type == "bop") bopParams.totalWidth
    else 1
  }
  def blockBytes: Int = {
    if (enable && _type == "stream") streamParams.blockBytes
    else if (enable && _type == "bop") bopParams.blockBytes
    else 64
  }
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

class L2PrefetcherIO extends XSBundle with HasPrefetchParameters {
  // val in = Flipped(DecoupledIO(new MissReq))
  val in = Flipped(new PrefetcherIO(PAddrBits))
  val enable = Input(Bool())
}

// prefetch DCache lines in L2 using StreamPrefetch
class L2PrefetcherImp(outer: L2Prefetcher) extends LazyModuleImp(outer) with HasPrefetchParameters {  
  val io = IO(new L2PrefetcherIO)

  val enable_prefetcher = RegNext(io.enable)

  val (bus, edge) = outer.clientNode.out.head
  if (l2PrefetcherParameters.enable && l2PrefetcherParameters._type == "bop") {
    val bopParams = l2PrefetcherParameters.bopParams
    val dPrefetch = Module(new BestOffsetPrefetch(bopParams))
    dPrefetch.io.train.valid := io.in.acquire.valid && enable_prefetcher
    dPrefetch.io.train.bits.addr := io.in.acquire.bits.address
    dPrefetch.io.train.bits.write := io.in.acquire.bits.write
    dPrefetch.io.train.bits.miss := true.B

    bus.a.valid := dPrefetch.io.req.valid && enable_prefetcher
    bus.a.bits := DontCare
    bus.a.bits := edge.Hint(
      fromSource = dPrefetch.io.req.bits.id,
      toAddress = dPrefetch.io.req.bits.addr,
      lgSize = log2Up(bopParams.blockBytes).U,
      param = Mux(dPrefetch.io.req.bits.write, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
    )._2
    dPrefetch.io.req.ready := Mux(enable_prefetcher, bus.a.ready, true.B)

    dPrefetch.io.resp.valid := bus.d.valid && enable_prefetcher
    dPrefetch.io.resp.bits.id := bus.d.bits.source(bopParams.totalWidth - 1, 0)
    bus.d.ready := Mux(enable_prefetcher, dPrefetch.io.resp.ready, true.B)

    dPrefetch.io.finish.ready := true.B

  } else if (l2PrefetcherParameters.enable && l2PrefetcherParameters._type == "stream") {
    val streamParams = l2PrefetcherParameters.streamParams
    val dPrefetch = Module(new StreamPrefetch(streamParams))
    dPrefetch.io.train.valid := io.in.acquire.valid && enable_prefetcher
    dPrefetch.io.train.bits.addr := io.in.acquire.bits.address
    dPrefetch.io.train.bits.write := io.in.acquire.bits.write
    dPrefetch.io.train.bits.miss := true.B

    bus.a.valid := dPrefetch.io.req.valid && enable_prefetcher
    bus.a.bits := DontCare
    bus.a.bits := edge.Hint(
      fromSource = dPrefetch.io.req.bits.id,
      toAddress = dPrefetch.io.req.bits.addr,
      lgSize = log2Up(l2PrefetcherParameters.blockBytes).U,
      param = Mux(dPrefetch.io.req.bits.write, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ) // TODO
    )._2
    dPrefetch.io.req.ready := Mux(enable_prefetcher, bus.a.ready, true.B)

    dPrefetch.io.resp.valid := bus.d.valid && enable_prefetcher
    dPrefetch.io.resp.bits.id := bus.d.bits.source(l2PrefetcherParameters.totalWidth - 1, 0)
    bus.d.ready := Mux(enable_prefetcher, dPrefetch.io.resp.ready, true.B)

    dPrefetch.io.finish.ready := true.B

  } else {
    bus.a.valid := false.B
    bus.a.bits := DontCare
    bus.d.ready := true.B
  }

  bus.b.ready := true.B

  bus.c.valid := false.B
  bus.c.bits := DontCare

  bus.e.valid := false.B
  bus.e.bits := DontCare

  XSPerfAccumulate("reqCnt", bus.a.fire())
  (0 until l2PrefetcherParameters.nEntries).foreach(i => {
    XSPerfAccumulate(
      "entryPenalty" + "%02d".format(i),
      BoolStopWatch(
        start = bus.a.fire() && bus.a.bits.source(l2PrefetcherParameters.totalWidth - 1, 0) === i.U,
        stop = bus.d.fire() && bus.d.bits.source(l2PrefetcherParameters.totalWidth - 1, 0) === i.U,
        startHighPriority = true
      )
    )
    // XSPerfAccumulate("entryReq" + "%02d".format(i), bus.a.fire() && bus.a.bits.source(l2PrefetcherParameters.totalWidth - 1, 0) === i.U)
  })
}

