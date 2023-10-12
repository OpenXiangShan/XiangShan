package xiangshan.utils

import chisel3.emitVerilog
import chisel3.util.ValidIO
import top.ArgParser
import utils.PipeWithFlush
import xiangshan.{Redirect, XSCoreParamsKey, XSTileKey}
import xiangshan.backend.Bundles.DynInst

object GenPipeWithFlush extends App {
  println("Generating the VerilogPipeWithFlush hardware")
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  val p = config.alterPartial({ case XSCoreParamsKey => config(XSTileKey).head })

  emitVerilog(
    new PipeWithFlush[DynInst, ValidIO[Redirect]](
      new DynInst()(p),
      ValidIO(new Redirect()(p)),
      2,
      (dynInst: DynInst, flush: ValidIO[Redirect], stage: Int) => dynInst.robIdx.needFlush(flush)
    ),
    Array("--target-dir", "build/vifu"))
}
