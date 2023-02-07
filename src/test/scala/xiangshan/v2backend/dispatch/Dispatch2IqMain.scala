package xiangshan.v2backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}
import xiangshan.v2backend.{SchdBlockParams}


object Dispatch2IqMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config: Parameters = new BaseConfig(1)

    implicit val schdBlockParams : SchdBlockParams = SchdBlockParams.dummyIntParams()
    val d2iq: Dispatch2Iq = LazyModule(new Dispatch2Iq(schdBlockParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    Generator.execute(
      firrtlOpts,
      d2iq.module,
      firrtlComplier
    )
  }

}
