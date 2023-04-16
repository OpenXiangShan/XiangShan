package xiangshan.backend.dispatch

import chisel3._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import top.{ArgParser, Generator, XSTop}


object Dispatch2IqMain extends App {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts, firrtlComplier) = ArgParser.parse(args)
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

//    val d2iq: Dispatch2Iq = LazyModule(new Dispatch2Iq(schdBlockParams)(config2)))

    Generator.execute(
      firrtlOpts,
      soc.core_with_l2(0).core.backend.intScheduler.get.dispatch2Iq.module,
      firrtlComplier
    )
  }

}
