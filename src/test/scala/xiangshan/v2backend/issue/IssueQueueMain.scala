package xiangshan.v2backend.issue

import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object IssueQueueMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config: BaseConfig = new BaseConfig(1)

    val p = config.alterPartial({case XSCoreParamsKey => XSCoreParameters})
    val iq: IssueQueue = LazyModule(new IssueQueue()(config.alterPartial({
      case XSCoreParamsKey => XSCoreParameters()
    })))

    Generator.execute(
      firrtlOpts,
      iq.module,
      firrtlComplier
    )
  }

}
