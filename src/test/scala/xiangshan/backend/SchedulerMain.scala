package xiangshan.backend

import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.backend.issue.Scheduler
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object SchedulerMain extends App {
  val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(args)

  val backendParams = config(XSCoreParamsKey).backendParams

  val schdParams = backendParams.intSchdParams.get
  val schd = LazyModule(new Scheduler(schdParams)(config))

  Generator.execute(
    firrtlOpts,
    schd.module,
    firrtlComplier,
    firtoolOpts
  )
}
