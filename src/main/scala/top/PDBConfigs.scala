package top

import org.chipsalliance.cde.config.Config
import xiangshan.XSTileKey
import xiangshan.mem.prefetch.{PDBDepthKey, PDBDepthParameters, StreamDepthKey, StreamDepthParameters}

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

class WithPDBDepth(params: PDBDepthParameters = PDBDepthParameters(enabled = true)) extends Config(
  new Config((site, here, up) => { case PDBDepthKey => params }) ++
    new WithStreamDepth(StreamDepthParameters(useMonitor = params.enabled))
)

// Historical diagnostic presets remain fixed64 even though DefaultConfig is H3.
class PDB16Config(n: Int = 1) extends Config(
  new WithPDBDepth(PDBDepthParameters()) ++ new WithPDB(16) ++ new DefaultConfig(n))
class PDB64Config(n: Int = 1) extends Config(
  new WithPDBDepth(PDBDepthParameters()) ++ new WithPDB(64) ++ new DefaultConfig(n))
class PDB16LRUConfig(n: Int = 1) extends Config(
  new WithPDBDepth(PDBDepthParameters()) ++ new WithPDB(16, "lru") ++ new DefaultConfig(n))
class PDB64LRUConfig(n: Int = 1) extends Config(
  new WithPDBDepth(PDBDepthParameters()) ++ new WithPDB(64, "lru") ++ new DefaultConfig(n))
class NoPDBConfig(n: Int = 1) extends Config(
  new WithPDBDepth(PDBDepthParameters()) ++ new WithPDB(0) ++ new DefaultConfig(n))

// Wiring validation only: the monitor holds initial depth until a controller is enabled.
class PDB64MonitorDepthConfig(n: Int = 1) extends Config(
  new WithStreamDepth(StreamDepthParameters(useMonitor = true)) ++ new PDB64LRUConfig(n)
)
