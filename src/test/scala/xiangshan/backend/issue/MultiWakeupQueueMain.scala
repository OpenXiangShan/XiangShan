package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import top.ArgParser
import xiangshan.backend.Bundles.DynInst
import xiangshan.{Redirect, XSCoreParameters, XSCoreParamsKey}

object MultiWakeupQueueMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val p = config.alterPartial({ case XSCoreParamsKey => XSCoreParameters() })

  emitVerilog(
    new MultiWakeupQueue[DynInst, ValidIO[Redirect]](
      new DynInst()(p),
      ValidIO(new Redirect()(p)),
      Set(2, 4),
      (dynInst: DynInst, flush: ValidIO[Redirect], stage: Int) => dynInst.robIdx.needFlush(flush)
    ),
    Array("--full-stacktrace", "--target-dir", "build/issue")
  )
}
