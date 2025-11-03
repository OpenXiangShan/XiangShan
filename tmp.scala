val imsicSlaveNodes = Seq.tabulate(ports)(n => {
  val m_mode = (baseAddress._1 + n * regSize._1, regSize._1 - 1)
  val s_mode = (baseAddress._2 + n * regSize._2, regSize._2 - 1)
  println(f"IMSICXbar: #${n}%2d    M-mode [0x${m_mode._1}%x, 0x${m_mode._1 + m_mode._2}%x]")
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



class Top(
  ports: Int,
  useAXILite: Boolean,
  baseAddress: (BigInt, BigInt),  /* baseAddress of M/S register */
  regSize: (Int, Int)            /* size for M/S register */
)(implicit p: Parameters) extends LazyModule
    with HasSoCParameter
{
  val idBits = 11
  val slaveDataBytes = 16

  val masterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "s_axi_",
      id = IdRange(0, 1 << idBits)
    ))
  )))

  println(f"IMSICXbar: ${ports} ports")

  val imsicSlaveNodes = Seq.tabulate(ports)(n => {
    val m_mode = (baseAddress._1 + n * regSize._1, regSize._1 - 1)
    val s_mode = (baseAddress._2 + n * regSize._2, regSize._2 - 1)
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

  private val errorDevice = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x0, 0x7fffffff)).flatMap( x => x.subtract(
                  AddressSet(baseAddress._1, ports * regSize._1 -1))).flatMap( x => x.subtract(
                  AddressSet(baseAddress._2, ports * regSize._2 -1))),
      maxAtomic = 1,
      maxTransfer = slaveDataBytes),
    beatBytes = 4
  ))

  val xbar = TLXbar()
  xbar :=
    TLFIFOFixer() :=
    TLWidthWidget(slaveDataBytes) :=
    AXI4ToTL() :=
    AXI4UserYanker(Some(1)) :=
    AXI4Fragmenter() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(1) :=
}



import chisel3._
import chisel3.util.ValidIO
import circt.stage._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.experimental.dataview._
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

import top.{ArgParser, Generator}
import utility.ResetGen
import util.Cat
import utils.VerilogAXI4Record
import utils.{AXI4LiteBundle, VerilogAXI4LiteRecord}
import xiangshan.DebugOptionsKey
import system.HasSoCParameter  /* for SoCParamsKey.XSTopPrefix */
