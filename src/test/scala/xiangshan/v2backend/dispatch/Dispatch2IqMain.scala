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

    implicit val intSchdBlockParams : SchdBlockParams = SchdBlockParams.dummyIntParams()
    implicit val vfSchdBlockParams : SchdBlockParams = SchdBlockParams.dummyVFParams()
    implicit val memSchdBlockParams : SchdBlockParams = SchdBlockParams.dummyMemParams()
    val intD2iq: Dispatch2Iq = LazyModule(new Dispatch2Iq(intSchdBlockParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    Generator.execute(
      firrtlOpts,
      intD2iq.module,
      firrtlComplier
    )

    // implicit val memSchdBlockParams : SchdBlockParams = SchdBlockParams.dummyMemParams()
    // val memD2iq: Dispatch2Iq = LazyModule(new Dispatch2Iq(memSchdBlockParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    // Generator.execute(
    //   firrtlOpts,
    //   MemD2iq.module,
    //   firrtlComplier
    // )


    // implicit val vfSchdBlockParams : SchdBlockParams = SchdBlockParams.dummyVFParams()
    // val vfD2iq: Dispatch2Iq = LazyModule(new Dispatch2Iq(vfSchdBlockParams)(config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })))

    // Generator.execute(
    //   firrtlOpts,
    //   vfD2iq.module,
    //   firrtlComplier
    // )
  }

}
