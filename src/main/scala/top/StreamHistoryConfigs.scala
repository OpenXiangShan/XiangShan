package top

import org.chipsalliance.cde.config.Config
import xiangshan.mem.prefetch.StreamHistoryKey

/** Matched A/B control: retain PC/context wiring and counters, remove filtering. */
class StreamHistoryBaselineConfig(n: Int = 1) extends Config(
      new Config((_, _, _) => { case StreamHistoryKey => None }) ++ new DefaultConfig(n)
    )
