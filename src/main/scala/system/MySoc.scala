package system

import chisel3._
import chisel3.util._
import xiangshan._
import chipsalliance.rocketchip.config.Parameters
import chisel3.stage.ChiselGeneratorAnnotation
import device.TLTimer
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import sifive.blocks.inclusivecache._
import top.XiangShanStage


abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule with HasSoCParameter {
  val bankedNode = BankBinder(L3NBanks, L3BlockSize)
  val mmioXbar = TLXbar()
}

trait HaveAXI4MemPort {
  this: BaseXSSoc =>

  val memAXI4SlaveNode = AXI4SlaveNode(Seq.tabulate(L3NBanks) { i =>

    AXI4SlavePortParameters(
      slaves = Seq(
        AXI4SlaveParameters(
          address = Seq(AddressSet(0x80000000L, 0x7FFFFFFFL)),
          regionType = RegionType.UNCACHED,
          executable = true,
          supportsRead = TransferSizes(1, L3BlockSize),
          supportsWrite = TransferSizes(1, L3BlockSize),
          interleavedId = Some(0)
        )
      ),
      beatBytes = L3BusWidth / 8
    )
  })

  memAXI4SlaveNode :=*
    AXI4UserYanker() :=*
    AXI4IdIndexer(idBits = 1) :=*
    TLToAXI4() :=*
    TLWidthWidget(L3BusWidth / 8) :=*
    TLCacheCork() :=*
    bankedNode

  val memAXI4 = InModuleBody {
    memAXI4SlaveNode.makeIOs()
  }
}


trait HaveAXI4MMIOPort { this: BaseXSSoc =>

  val mmioAXI4SalveNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address = AddressSet(0x0, 0x7fffffff).subtract(AddressSet(0x38000000L, 0x0000ffffL)),
      regionType = RegionType.UNCACHED,
      supportsRead = TransferSizes(1, 8),
      supportsWrite = TransferSizes(1, 8),
      interleavedId = Some(0)
    )),
    beatBytes = 8
  )))

  mmioAXI4SalveNode :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    mmioXbar

  val mmioAXI4 = InModuleBody {
    mmioAXI4SalveNode.makeIOs()
  }

}


class MySoc()(implicit p: Parameters) extends BaseXSSoc()
  with HaveAXI4MemPort
  with HaveAXI4MMIOPort
  {

  println(s"My Soc cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3BusWidth")

  val l3_xbar = TLXbar()
  for (i <- 0 until NumCores) {
    val core = LazyModule(new XSCore())
    val l2 = LazyModule(new InclusiveCache(
      CacheParameters(
        level = 2,
        ways = L2NWays,
        sets = L2NSets,
        blockBytes = L2BlockSize,
        beatBytes = L1BusWidth / 8, // beatBytes = l1BusDataWidth / 8
        cacheName = s"L2"
      ),
      InclusiveCacheMicroParameters(
        writeBytes = 32
      )
    ))
    mmioXbar := TLBuffer() := core.frontend.instrUncache.clientNode
    mmioXbar := TLBuffer() := core.memBlock.uncache.clientNode
    val l2_xbar = TLXbar()
    l2_xbar := TLBuffer() := core.memBlock.dcache.clientNode
    l2_xbar := TLBuffer() := core.l1pluscache.clientNode
    l2_xbar := TLBuffer() := core.ptw.node
    l2_xbar := TLBuffer() := core.l2Prefetcher.clientNode
    l2.node := TLBuffer() := l2_xbar
    l3_xbar := TLBuffer() := l2.node
  }

  private val clint = LazyModule(new TLTimer(
    Seq(AddressSet(0x38000000L, 0x0000ffffL)),
    sim = !env.FPGAPlatform
  ))

  clint.node := mmioXbar

  val l3_node = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 3,
      ways = L3NWays,
      sets = L3NSets,
      blockBytes = L3BlockSize,
      beatBytes = L2BusWidth / 8,
      cacheName = "L3"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 32
    )
  )).node

  bankedNode :*= l3_node :*= TLBuffer() :*= l3_xbar

  lazy val module = new LazyModuleImp(this){

  }
}

object MySoc extends App {
  override def main(args: Array[String]): Unit = {
    implicit val p = Parameters.empty
    (new XiangShanStage).execute(Array(), Seq(
      ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new MySoc())
        soc.module
      })
    ))
  }
}
