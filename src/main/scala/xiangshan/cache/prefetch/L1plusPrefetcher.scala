package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._

case class L1plusPrefetcherParameters(
  _type: String,
  streamParams: StreamPrefetchParameters
) {
  def nEntries: Int = streamParams.streamCnt * streamParams.streamSize
}

// prefetch ICache lines in L1plusCache using StreamPrefetch
class L1plusPrefetcher(enable: Boolean) extends PrefetchModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new IcacheMissReq))
    // prefetch
    val mem_acquire = DecoupledIO(new L1plusCacheReq)
    val mem_grant = Flipped(DecoupledIO(new L1plusCacheResp))
  })

  if (enable && l1plusPrefetchParams._type == "stream") {
    val streamParams = l1plusPrefetchParams.streamParams
    val pft = Module(new StreamPrefetch(streamParams))
    pft.io.train.valid := io.in.fire()
    pft.io.train.bits.addr := io.in.bits.addr
    pft.io.train.bits.write := false.B
    pft.io.train.bits.miss := true.B
    io.in.ready := true.B

  } else {
    io.in.ready := true.B
    io.mem_acquire.valid := false.B
    io.mem_acquire.bits := DontCare
    io.mem_grant.ready := true.B
  }
}