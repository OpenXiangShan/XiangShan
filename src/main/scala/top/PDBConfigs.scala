package top

import org.chipsalliance.cde.config.Config
import xiangshan.XSTileKey
import xiangshan.mem.prefetch.{StreamDepthKey, StreamDepthParameters}

// Preserve the core, cache geometry and other prefetchers of the supplied base config.
class WithPDB(entries: Int, replacement: String = "rr") extends Config((site, here, up) => {
  case XSTileKey => up(XSTileKey).map { core =>
    core.copy(dcacheParametersOpt = core.dcacheParametersOpt.map(_.copy(
      nPBEntries = entries, pbReplacer = replacement)))
  }
})

class WithStreamDepth(depth: StreamDepthParameters) extends Config((site, here, up) => {
  case StreamDepthKey => depth
})

class PDB16Config(n: Int = 1) extends Config(new WithPDB(16) ++ new DefaultConfig(n))
class PDB64Config(n: Int = 1) extends Config(new WithPDB(64) ++ new DefaultConfig(n))
class PDB16LRUConfig(n: Int = 1) extends Config(new WithPDB(16, "lru") ++ new DefaultConfig(n))
class PDB64LRUConfig(n: Int = 1) extends Config(new WithPDB(64, "lru") ++ new DefaultConfig(n))
class NoPDBConfig(n: Int = 1) extends Config(new WithPDB(0) ++ new DefaultConfig(n))

// Wiring validation only: the monitor holds initial depth until a controller is enabled.
class PDB64MonitorDepthConfig(n: Int = 1) extends Config(
  new WithStreamDepth(StreamDepthParameters(useMonitor = true)) ++ new PDB64LRUConfig(n)
)
