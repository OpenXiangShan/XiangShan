package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.v2backend.IssueBlockParams
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object IssueQueueMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config: Parameters = new BaseConfig(1)

    val iqParams: IssueBlockParams = DummyIQParams()(config)
     val iq: IssueQueue = LazyModule(new IssueQueue(iqParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    Generator.execute(
      firrtlOpts,
      iq.module,
      firrtlComplier
    )
  }

}
