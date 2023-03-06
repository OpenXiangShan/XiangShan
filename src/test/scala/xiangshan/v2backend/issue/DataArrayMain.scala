package xiangshan.v2backend.issue

import chisel3._
import freechips.rocketchip.diplomacy.DisableMonitors
import top.{ArgParser, BaseConfig, Generator}
import xiangshan.v2backend.IssueBlockParams
import xiangshan.{XSCoreParameters, XSCoreParamsKey}

object DataArrayMain extends App {
  override def main(args: Array[String]): Unit = {
    val (_, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val config = new BaseConfig(1).alterPartial({ case XSCoreParamsKey => XSCoreParameters() })
    implicit val iqParams: IssueBlockParams = DummyIQParams()(config)

    Generator.execute(
      firrtlOpts,
      // DataArray
      DisableMonitors(p =>
        new DataArray(Vec(iqParams.dataBitsMax, Bool()), iqParams.numDeq, iqParams.numEnq, iqParams.numEntries)(
          p.alterPartial({
            case XSCoreParamsKey => XSCoreParameters()
          })))(config),
      firrtlComplier
    )
  }
}