// Elaborate XiangShan for the experimental open-source flow (README.md).
//
// The flow hardens XSTile -- the core with its L1I and L1D and the L2, one
// clock and one voltage domain -- but XSTile is a diplomacy LazyModule
// whose nodes bind upward into the SoC, so it cannot be elaborated on its
// own without hand-writing the tie-off. Elaborating upstream's own XSTop
// avoids that entirely: firtool emits one Verilog module per Chisel
// module, so XSTile arrives as its own module with its own ports, and the
// synthesis top is a choice made in the flow rather than surgery done here.
//
// Everything outside XSTile is elaborated because the tile cannot run
// without it, and hardened by nobody.
//
// Ported from bazel-orfs (test/coremark_joule/xiangshan/src), where this
// configuration was measured; only names and comments differ.

package openroadflow

import circt.stage.ChiselStage
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.chipsalliance.cde.config.{Config, Parameters}
import top._
import utility._
import xiangshan.{DFTOptionsKey, DebugOptionsKey, XSTileKey}

/** XiangShan V3 (Kunminghu) as this flow builds it: the reference
  * configuration bazel-orfs measured.
  *
  * Derived from upstream's DefaultConfig with these changes, outside the
  * tile's core logic except for the first:
  *
  *   - An integer-only core (WithIntegerOnlyCore, below).
  *   - A small last-level cache. XiangShan V3 hardcodes enableCHI, so the
  *     non-CHI path with its plain AXI SoC is dead code and OpenLLC is not
  *     optional: XSTop instantiates it whenever CHI is on, and a config
  *     without OpenLLCParamsOpt fails to elaborate. It shrinks from 32 MB
  *     to 1 MB; the flow hardens nothing outside XSTile.
  *   - A 512 KB L2 in one bank, instead of 2 MB.
  *
  * The L1s are untouched: 64 KB each for instruction and data.
  */
class FlowConfig(n: Int = 1)
    extends Config(
      new WithIntegerOnlyCore
        ++ OpenLLCConfig("1MB", ways = 8, banks = 1)
        ++ L2CacheConfig("512KB", inclusive = true, banks = 1, tp = false)
        ++ WithNKBL1D(64, ways = 4, numMemChannels = 1)
        ++ new BaseConfig(n)
    )

/** The floating-point unit, the vector unit's ISA, the bitmap check, the
  * custom cache-op CSRs and the memory BIST logic, turned off.
  *
  * Two things this deliberately does not do, both measured rather than
  * assumed.
  *
  * HasFPU and HasVPU are set, but they do not remove the datapath.
  * backendParams unconditionally carries FpScheduler and VecScheduler
  * with their full execution-unit lists -- elaboration still builds
  * VFEX1, vialu, vfma and vimac -- so the flags gate ISA behaviour rather
  * than hardware. Removing the units means overriding the scheduler
  * parameters inside XSCoreParameters, which the wake-up configs,
  * register files, dispatch widths and ROB uop fields all reference by
  * name. That is a fork of the parameterisation, not a configuration, and
  * it is not attempted here. FltRegionModule and VecRegionModule are
  * therefore still hardened blocks of the flow.
  *
  * Two further knobs do not elaborate at all. HasHExtension = false fails
  * in MMUBundle with "High index 37 is out of range [0, 35]", and
  * EnableSv48 = false makes the ICache's address-field formatter compute
  * a negative field index. Both are left on.
  *
  * What this fragment buys is about four percent: 1443 modules and
  * 2.82 M lines become 1379 and 2.71 M.
  *
  * This is a departure from Kunminghu as delivered, so the result is not
  * directly comparable with XiangShan's own published figures.
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
      // Memory BIST wraps every SRAM in the design. It is test logic, and
      // it wraps exactly the memories AUTO_MEMORIES converts to macros.
      case DFTOptionsKey => up(DFTOptionsKey).copy(EnableMbist = false)
    })

/** The same shape at MinimalConfig, for proving the pipeline cheaply.
  *
  * Not a measurement configuration -- its caches and structures are far
  * smaller -- but it elaborates in a fraction of the time and memory, which
  * makes it the right thing to fail on first.
  */
class FlowMinimalConfig(n: Int = 1) extends Config(new MinimalConfig(n))

object FlowGeneratorBase {
  /** The two keys upstream sets in ArgParser rather than in any Config.
    *
    * Elaborating a Config directly, without going through the command-line
    * parser, leaves them undefined and XSTop fails partway through with
    * "Key PerfCounterOptionsKey is not defined in Parameters". The values
    * here are what upstream derives for a build with no debug options.
    */
  private def withArgParserKeys(config: Config): Parameters =
    config.alter((site, here, up) => {
      // Measure the design, not the instrumentation.
      //
      // XiangShan's default build carries ChiselDB instruction tables,
      // difftest probes and performance-counter timestamps through the
      // pipeline. All of it is analysis apparatus, none of it is the CPU,
      // and synthesising it would put its area into the result.
      //
      // FPGAPlatform is upstream's name for "no simulation-only
      // instrumentation" -- it reads oddly for an ASIC flow, but it is the
      // switch that turns the apparatus off, and it is not used anywhere
      // in the memory implementations, so it does not change the design
      // being measured. Leaving it false with EnablePerfDebug off also
      // does not elaborate: Rob.scala reads perfDebugInfo.get from a
      // block guarded only by !FPGAPlatform.
      case DebugOptionsKey =>
        up(DebugOptionsKey).copy(
          FPGAPlatform = true,
          EnablePerfDebug = false,
          EnableDifftest = false,
          // Off. With it on, the core carries DiffRatStateBuffer: eight
          // 704x776 register banks, 4.4 Mbit of difftest storage read
          // combinationally. The CSR's vl port, the one reader that kept
          // it live, is guarded by basicDebugEn upstream, so the whole
          // difftest apparatus goes with this flag.
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
    // Both write files into the working directory as a side effect of
    // elaboration; neither is wanted here.
    Constantin.init(false)
    ChiselDB.init(false)
  }

  /** XSTop alone: what the flow hardens XSTile out of. */
  def emit(config: Config, args: Array[String]): Unit = {
    prepare()
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(withArgParserKeys(config))
    ChiselStage.emitHWDialect(soc.module, Array(), args)
  }
}

object FlowGenerator extends App {
  FlowGeneratorBase.emit(new FlowConfig(1), args)
}

object FlowGeneratorMinimal extends App {
  FlowGeneratorBase.emit(new FlowMinimalConfig(1), args)
}
