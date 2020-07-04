package xiangshan.testutils

import chisel3._
import chisel3.util.experimental.BoringUtils

object AddSinks {
  def apply(dispBegin: Int = 0, dispEnd: Int = -1) = {
    val tmp = WireInit(0.U)
    val sinks = Array[String](
      "DTLBFINISH",
      "DTLBPF",
      "DTLBENABLE",
      "perfCntCondMdcacheLoss",
      "perfCntCondMl2cacheLoss",
      "perfCntCondMdcacheHit",
      "lsuMMIO",
      "perfCntCondMl2cacheHit",
      "perfCntCondMl2cacheReq",
      "mtip",
      "perfCntCondMdcacheReq",
      "meip"
    )
    for (s <- sinks){ BoringUtils.addSink(tmp, s) }

    val disp_begin = WireInit(dispBegin.S(64.W).asUInt())
    val disp_end = WireInit(dispEnd.S(64.W).asUInt())
    BoringUtils.addSource(disp_begin, "DISPLAY_LOG_START")
    BoringUtils.addSource(disp_end, "DISPLAY_LOG_END")

  }
}
