package xiangshan.backend.issue

import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object IssueQueueMain extends App {
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
