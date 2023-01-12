package xiangshan.v2backend.issue

import freechips.rocketchip.diplomacy.DisableMonitors
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object StatusArrayMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config = new BaseConfig(1)
    implicit val iqParams: IssueQueueParams = DummyIQParams()

    Generator.execute(
      firrtlOpts,
      DisableMonitors(p => StatusArray(p.alterPartial({
        case XSCoreParamsKey => XSCoreParameters()
      }), iqParams))(config),
      firrtlComplier
    )
  }
}
