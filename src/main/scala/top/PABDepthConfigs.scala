package top

import org.chipsalliance.cde.config.Config
import xiangshan.mem.prefetch.{PABDepthKey, PABDepthParameters}

class WithPABDepth(params: PABDepthParameters) extends Config((site, here, up) => {
  case PABDepthKey => params
})

// Retain PAB/victim observations while fixing the Stream distance for comparison.
class PABFixedDepthConfig(n: Int = 1) extends Config(
  new WithPABDepth(PABDepthParameters(enabled = true, autoDepth = false)) ++ new DefaultConfig(n)
)

// Explicit controls are available on both experiment branches; DefaultConfig
// selects the branch's intended experiment without requiring a custom config.
class DCacheVictimFillConfig(n: Int = 1) extends Config(
  new WithPABDepth(PABDepthParameters(enabled = true)) ++ new DefaultConfig(n)
)
class DCacheVictimCycleConfig(n: Int = 1) extends Config(
  new WithPABDepth(PABDepthParameters(enabled = true, windowCycles = 16384, minWindowFills = 128)) ++ new DefaultConfig(n)
)
