package xiangshan.backend.issue

import freechips.rocketchip.diplomacy.DisableMonitors
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object StatusArrayMain extends App {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(args)

    val backendParams = config(XSCoreParamsKey).backendParams

    val iqParams: IssueBlockParams = backendParams.intSchdParams.get.issueBlockParams.head

    Generator.execute(
      firrtlOpts,
      DisableMonitors(p => StatusArray(p, iqParams))(config),
      firrtlComplier,
      firtoolOpts
    )
  }
}
