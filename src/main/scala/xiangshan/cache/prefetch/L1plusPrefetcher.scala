package xiangshan.cache.prefetch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache._
import utils._
import chisel3.ExcitingUtils._

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
    // switch
    val enable = Input(Bool())
  })

  if (l1plusPrefetcherParameters.enable && l1plusPrefetcherParameters._type == "stream") {
    val streamParams = l1plusPrefetcherParameters.streamParams
    val pft = Module(new StreamPrefetch(streamParams))
    pft.io.train.valid := io.in.fire() && io.enable
    pft.io.train.bits.addr := io.in.bits.addr
    pft.io.train.bits.write := false.B
    pft.io.train.bits.miss := true.B
    io.in.ready := true.B

    io.mem_acquire.valid := pft.io.req.valid && io.enable
    io.mem_acquire.bits.cmd := Mux(pft.io.req.bits.write, MemoryOpConstants.M_PFW, MemoryOpConstants.M_PFR)
    io.mem_acquire.bits.addr := pft.io.req.bits.addr
    io.mem_acquire.bits.id := pft.io.req.bits.id
    pft.io.req.ready := Mux(io.enable, io.mem_acquire.ready, true.B)

    pft.io.resp.valid := io.mem_grant.valid && io.enable
    pft.io.resp.bits.id := io.mem_grant.bits.id(streamParams.totalWidth - 1, 0)
    io.mem_grant.ready := Mux(io.enable, pft.io.resp.ready, true.B)

    pft.io.finish.ready := true.B

    // debug info
    XSDebug(p"io.in:          v=${io.in.valid} r=${io.in.ready} ${io.in.bits}\n")
    XSDebug(p"io.mem_acquire: v=${io.mem_acquire.valid} r=${io.mem_acquire.ready} ${io.mem_acquire.bits}\n")
    XSDebug(p"io.mem_grant:   v=${io.mem_grant.valid} r=${io.mem_grant.ready} ${io.mem_grant.bits}\n")

    XSPerf("L1+Prefetch_reqCnt", io.mem_acquire.fire())
    def idWidth: Int = log2Up(l1plusPrefetcherParameters.nEntries)
    (0 until l1plusPrefetcherParameters.nEntries).foreach(i =>
      XSPerf(
        "L1+Prefetch_penaltyEntry" + Integer.toString(i, 10),
        BoolStopWatch(
          start = io.mem_acquire.fire() && io.mem_acquire.bits.id(idWidth - 1, 0) === i.U,
          stop = io.mem_grant.fire() && io.mem_grant.bits.id(idWidth - 1, 0) === i.U,
          startHighPriority = true
        )
      )
    )

  } else {
    io.in.ready := true.B
    io.mem_acquire.valid := false.B
    io.mem_acquire.bits := DontCare
    io.mem_grant.ready := true.B
  }
}