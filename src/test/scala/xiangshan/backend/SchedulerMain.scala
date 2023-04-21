package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.backend.issue.Scheduler
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object SchedulerMain extends App {
  override def main(args: Array[String]): Unit = {
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
}
