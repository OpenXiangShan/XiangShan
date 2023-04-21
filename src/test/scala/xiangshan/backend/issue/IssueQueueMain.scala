package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object IssueQueueMain extends App {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(args)

    val backendParams = config(XSCoreParamsKey).backendParams

    val iqParams: IssueBlockParams = backendParams.intSchdParams.get.issueBlockParams.head
    val iq: IssueQueue = LazyModule(new IssueQueue(iqParams)(config))

    Generator.execute(
      firrtlOpts,
      iq.module,
      firrtlComplier,
      firtoolOpts
    )
  }

}
