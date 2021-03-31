package top

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Config
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3._
import device.AXI4RAMWrapper
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.XLen

class SimTop(useDRAMSim: Boolean)(implicit p: config.Parameters) extends Module {

  val l_soc = LazyModule(new XSTopWithoutDMA())
  val soc = Module(l_soc.module)

  val l_simMMIO = LazyModule(new SimMMIO(l_soc.peripheralNode.in.head._2))
  val simMMIO = Module(l_simMMIO.module)

  val l_simAXIMem = LazyModule(new AXI4RAMWrapper(
    l_soc.memAXI4SlaveNode, 128 * 1024 * 1024, useBlackBox = true
  ))
  val simAXIMem = Module(l_simAXIMem.module)

  l_simMMIO.connectToSoC(l_soc)
  l_simAXIMem.connectToSoC(l_soc)

  soc.io.extIntrs := 0.U
  simMMIO.io.uart.in := DontCare

  val io = IO(new Bundle(){})

}

object SimTop extends App {
  override def main(args: Array[String]): Unit = {
    val useDRAMSim = args.contains("--with-dramsim3")

    // set soc parameters
    val socArgs = args.filterNot(_ == "--with-dramsim3")
    Parameters.set(
      (socArgs.contains("--fpga-platform"), socArgs.contains("--dual-core"), socArgs.contains("--disable-log")) match {
        case (true,  false, _)     => Parameters()
        case (true,   true, _)     => Parameters.dualCoreParameters
        case (false,  true,  true) => Parameters.simDualCoreParameters
        case (false, false,  true) => Parameters.simParameters
        case (false,  true, false) => Parameters.debugDualCoreParameters
        case (false, false, false) => Parameters.debugParameters
      }
    )

    val otherArgs = socArgs.filterNot(_ == "--disable-log").filterNot(_ == "--fpga-platform").filterNot(_ == "--dual-core")
    implicit val p = new Config((_, _, _) => {
      case XLen => 64
    })
    // generate verilog
    XiangShanStage.execute(
      otherArgs,
      Seq(
        ChiselGeneratorAnnotation(() => new SimTop(useDRAMSim))
      )
    )
  }
}
