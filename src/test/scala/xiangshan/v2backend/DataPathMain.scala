package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}


object DataPathMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config: Parameters = new BaseConfig(1)

    val intSchdParams = SchdBlockParams.dummyIntParams()
    val vfSchdParams = SchdBlockParams.dummyVfParams()
    val memSchdParams = SchdBlockParams.dummyMemParams()
    val dataPathParams = DataPathParams(Map(
      IntScheduler() -> intSchdParams,
      VfScheduler() -> vfSchdParams,
      MemScheduler() -> memSchdParams,
    ))
    val dataPath = LazyModule(new DataPath(dataPathParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    Generator.execute(
      firrtlOpts,
      dataPath.module,
      firrtlComplier
    )
  }
}
