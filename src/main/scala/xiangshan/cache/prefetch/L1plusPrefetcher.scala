package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class L1plusPrefetcherParameters(
  enable: Boolean,
  _type: String,
  streamParams: StreamPrefetchParameters
) {
  def nEntries: Int = streamParams.streamCnt * streamParams.streamSize
}

// prefetch ICache lines in L1plusCache using StreamPrefetch
class L1plusPrefetcher extends PrefetchModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new IcacheMissReq))
    // prefetch
    val mem_acquire = DecoupledIO(new L1plusCacheReq)
    val mem_grant = Flipped(DecoupledIO(new L1plusCacheResp))
  })

  if (l1plusPrefetcherParameters.enable && l1plusPrefetcherParameters._type == "stream") {
    val streamParams = l1plusPrefetcherParameters.streamParams
    val pft = Module(new StreamPrefetch(streamParams))
    pft.io.train.valid := io.in.fire()
    pft.io.train.bits.addr := io.in.bits.addr
    pft.io.train.bits.write := false.B
    pft.io.train.bits.miss := true.B
    io.in.ready := true.B

    io.mem_acquire.valid := pft.io.req.valid
    io.mem_acquire.bits.cmd := Mux(pft.io.req.bits.write, MemoryOpConstants.M_PFW, MemoryOpConstants.M_PFR)
    io.mem_acquire.bits.addr := pft.io.req.bits.addr
    io.mem_acquire.bits.id := pft.io.req.bits.id
    pft.io.req.ready := io.mem_acquire.ready

    pft.io.resp.valid := io.mem_grant.valid
    pft.io.resp.bits.id := io.mem_grant.bits.id(streamParams.totalWidth - 1, 0)
    io.mem_grant.ready := pft.io.resp.ready

  } else {
    io.in.ready := true.B
    io.mem_acquire.valid := false.B
    io.mem_acquire.bits := DontCare
    io.mem_grant.ready := true.B
  }
}