package xiangshan.backend.dispatch

import chisel3._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import top.{ArgParser, Generator, XSTop}
import xiangshan.XSCoreParamsKey


object Dispatch2IqMain extends App {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier, firtoolOpts) = ArgParser.parse(args)

    val backendParams = config(XSCoreParamsKey).backendParams
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

    Generator.execute(
      firrtlOpts,
      soc.core_with_l2(0).core.backend.intScheduler.get.dispatch2Iq.module,
      firrtlComplier,
      firtoolOpts
    )
  }

}
