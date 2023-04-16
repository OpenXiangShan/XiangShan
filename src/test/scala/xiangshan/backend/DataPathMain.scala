package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.backend.datapath.DataPath
import xiangshan.{XSCoreParameters, XSCoreParamsKey}


object DataPathMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    implicit val config: Parameters = new BaseConfig(1).alterPartial({ case XSCoreParamsKey => XSCoreParameters() })

    val backendParams = config(XSCoreParamsKey).backendParams
    val dataPath = LazyModule(new DataPath(backendParams)(config))

    Generator.execute(
      firrtlOpts,
      dataPath.module,
      firrtlComplier
    )
  }
}
