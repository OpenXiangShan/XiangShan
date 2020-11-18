package device

import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, TransferSizes}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4.{AXI4Parameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}
import xiangshan.HasXSLog

class AXI4FakeMemNode(
  address: Seq[AddressSet],
  executable: Boolean = true,
  beatBytes: Int = 8,
  burstLen: Int = 16,
)(implicit p: Parameters) extends LazyModule {
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address,
      regionType = RegionType.UNCACHED,
      executable = executable,
      supportsWrite = TransferSizes(1, beatBytes * burstLen),
      supportsRead = TransferSizes(1, beatBytes * burstLen),
      interleavedId = Some(0)
    )),
    beatBytes = beatBytes
  ))
  )
  lazy val module = new LazyModuleImp(this){
    val (in, edge) = node.in.head
    val io = IO(chiselTypeOf(in))
  }
}
