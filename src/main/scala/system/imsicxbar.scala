
package uncoreTest

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._

case class imsicParams(
   ports: Int,
   baseAddress: (BigInt, BigInt), /* baseAddress of M/S register */
   regSize: (Int, Int) /* size for M/S register */
)

class imsicxbar(params: imsicParams)(implicit p: Parameters) extends LazyModule {
  val idBits = 11
  val slaveDataBytes = 8

  val masterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "s_axi_",
      id = IdRange(0, 1 << idBits)
    ))
  )))

  println(f"IMSICXbar: ${params.ports} ports")

  val imsicSlaveNodes = Seq.tabulate(params.ports)(n => {
    val m_mode = (params.baseAddress._1 + n * params.regSize._1, params.regSize._1 - 1)
    val s_mode = (params.baseAddress._2 + n * params.regSize._2, params.regSize._2 - 1)
    println(f"IMSICXbar: #${n}%-2d    M-mode [0x${m_mode._1}%x, 0x${m_mode._1 + m_mode._2}%x]")
    println(f"IMSICXbar:    S/VS-mode [0x${s_mode._1}%x, 0x${s_mode._1 + s_mode._2}%x]")
    AXI4SlaveNode(Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        address = Seq(AddressSet(m_mode._1, m_mode._2),
          AddressSet(s_mode._1, s_mode._2)),
        regionType = RegionType.UNCACHED,
        supportsWrite = TransferSizes(1, slaveDataBytes),
        supportsRead = TransferSizes(1, slaveDataBytes)
      )),
      beatBytes = 4)))
  })

  imsicSlaveNodes.foreach(_ := AXI4Buffer() := masterNode)

  val input = imsicSlaveNodes.zipWithIndex.map { case (node, i) =>
    InModuleBody {
      node.makeIOs()(ValName(s"m_imsic1to$params.ports"))
    }
  }
  val out = InModuleBody {masterNode.makeIOs()(ValName(s"s_imsic1to$params.ports"))}
  lazy val module = new LazyModuleImp(this)
}