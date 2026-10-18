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
