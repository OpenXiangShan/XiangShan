package xiangshan.mem.prefetch

import org.chipsalliance.cde.config.Field

// Depths are measured in cache blocks. Outer-level depths stay fixed when L1 changes.
case class StreamDepthParameters(
  useMonitor: Boolean = false,
  fixedL1: Int = 64,
  initial: Int = 16,
  enableLegacyControl: Boolean = false
) {
  require(fixedL1 > 0 && fixedL1 < 4096)
  require(initial > 0 && initial < 4096)
  require(!enableLegacyControl || useMonitor,
    "Legacy depth control requires the Stream monitor depth input")
  require(!enableLegacyControl || (initial <= 2048 && (initial & (initial - 1)) == 0),
    "The legacy doubling controller requires a power-of-two initial depth up to 2048")
}

case object StreamDepthKey extends Field[StreamDepthParameters](StreamDepthParameters())
