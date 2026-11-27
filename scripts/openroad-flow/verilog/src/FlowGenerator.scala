// Elaborate XiangShan for Verilog generation, synthesis PPA reports, and the
// experimental OpenROAD physical-design flow (scripts/openroad-flow/README.md).

package openroadflow

import circt.stage.ChiselStage
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.chipsalliance.cde.config.{Config, Parameters}
import top._
import utility._
import xiangshan.{DFTOptionsKey, DebugOptionsKey, XSTileKey}

/** Turn off memory BIST wrappers so firtool emits clean `array_<depth>x<width>`
  * SRAM modules that synthesis and ORFS's `AUTO_MEMORIES` recognize.
  */
class WithoutMbist
    extends Config((site, here, up) => {
      case DFTOptionsKey => up(DFTOptionsKey).copy(EnableMbist = false)
    })

/** Unpatched DefaultConfig for synthesis / Verilog generation (FPU & VPU kept). */
class SynthDefaultConfig(n: Int = 1)
    extends Config(new WithoutMbist ++ new DefaultConfig(n))

/** Unpatched MinimalConfig for fast-turnaround synthesis PPA & smoke builds (FPU & VPU kept). */
class SynthMinimalConfig(n: Int = 1)
    extends Config(
      new WithoutMbist
        ++ OpenLLCConfig("1MB", ways = 8, banks = 1)
        ++ new MinimalConfig(n)
    )

/** XiangShan V3 (Kunminghu) as the XSTile physical flow hardens it:
  * integer-only ISA, 512 KB L2, 1 MB LLC, no MBIST.
  */
class WithIntegerOnlyCore
    extends Config((site, here, up) => {
      case XSTileKey =>
        up(XSTileKey).map(
          _.copy(
            HasFPU = false,
            HasVPU = false,
            HasBitmapCheck = false,
            HasCustomCSRCacheOp = false
          )
        )
      case DFTOptionsKey => up(DFTOptionsKey).copy(EnableMbist = false)
    })

class FlowConfig(n: Int = 1)
    extends Config(
      new WithIntegerOnlyCore
        ++ OpenLLCConfig("1MB", ways = 8, banks = 1)
        ++ L2CacheConfig("512KB", inclusive = true, banks = 1, tp = false)
        ++ WithNKBL1D(64, ways = 4, numMemChannels = 1)
        ++ new BaseConfig(n)
    )

class FlowMinimalConfig(n: Int = 1) extends Config(new MinimalConfig(n))

object FlowGeneratorBase {
  private def withArgParserKeys(config: Config): Parameters =
    config.alter((site, here, up) => {
      case DebugOptionsKey =>
        up(DebugOptionsKey).copy(
          FPGAPlatform = true,
          EnablePerfDebug = false,
          EnableDifftest = false,
          AlwaysBasicDiff = false,
          EnableChiselDB = false,
          EnableConstantin = false
        )
      case LogUtilsOptionsKey =>
        LogUtilsOptions(
          enableDebug = here(DebugOptionsKey).EnableDebug,
          enablePerf = here(DebugOptionsKey).EnablePerfDebug,
          fpgaPlatform = here(DebugOptionsKey).FPGAPlatform,
          enableXMR = here(DebugOptionsKey).EnableXMR
        )
      case PerfCounterOptionsKey =>
        PerfCounterOptions(
          enablePerfPrint = here(DebugOptionsKey).EnablePerfDebug && !here(DebugOptionsKey).FPGAPlatform,
          enablePerfDB = here(DebugOptionsKey).EnableRollingDB && !here(DebugOptionsKey).FPGAPlatform,
          perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
          perfDBHartID = 0
        )
    })

  private def prepare(): Unit = {
    Constantin.init(false)
    ChiselDB.init(false)
  }

  def emit(config: Config, args: Array[String]): Unit = {
    prepare()
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(withArgParserKeys(config))
    ChiselStage.emitHWDialect(soc.module, Array(), args)
  }
}

object SynthDefaultGenerator extends App {
  FlowGeneratorBase.emit(new SynthDefaultConfig(1), args)
}

object SynthMinimalGenerator extends App {
  FlowGeneratorBase.emit(new SynthMinimalConfig(1), args)
}

object FlowGenerator extends App {
  FlowGeneratorBase.emit(new FlowConfig(1), args)
}

object FlowGeneratorMinimal extends App {
  FlowGeneratorBase.emit(new FlowMinimalConfig(1), args)
}
