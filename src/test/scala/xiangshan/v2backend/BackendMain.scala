package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, Generator, BaseConfig}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object BackendMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    implicit val config: Parameters = new BaseConfig(1).alterPartial({ case XSCoreParamsKey => XSCoreParameters() })

    val backendParams = config(XSCoreParamsKey).backendParams
    val backend = LazyModule(new Backend(backendParams)(config))

    Generator.execute(
      firrtlOpts,
      backend.intScheduler.get.module,
      firrtlComplier
    )
    println("done")
  }
}

