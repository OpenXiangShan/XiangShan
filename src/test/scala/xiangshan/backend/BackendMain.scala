package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.backend.regfile.IntPregParams
import xiangshan.{XSCoreParameters, XSCoreParamsKey, XSTileKey}

object BackendMain extends App {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(
      args :+ "--disable-always-basic-diff" :+ "--disable-all" :+ "--remove-assert" :+ "--fpga-platform")

    val defaultConfig = config.alterPartial({
      // Get XSCoreParams and pass it to the "small module"
      case XSCoreParamsKey => config(XSTileKey).head
    })

    val backendParams = defaultConfig(XSCoreParamsKey).backendParams
    val backend = LazyModule(new Backend(backendParams)(defaultConfig))

    Generator.execute(
      firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
      backend.module,
      firrtlComplier,
      firtoolOpts
    )
    println("done")
  }
}

