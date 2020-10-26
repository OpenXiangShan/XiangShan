package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLMessages, TLToAXI4, TLXbar}
import org.scalatest.{FlatSpec, Matchers}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan.cache.{DCache, DCacheLineReq, DCacheWordReq, MemoryOpConstants}
import xiangshan.testutils.AddSinks

import scala.util.Random

case class TLCCacheTestParams
(
  ways: Int = 4,
  banks: Int = 1,
  capacityKB: Int = 4,
  blockBytes: Int = 64,
  beatBytes: Int = 32
) {
  require(blockBytes >= beatBytes)
}
case object TLCCacheTestKey extends Field[TLCCacheTestParams]

class TLCCacheTestTopIO extends Bundle {
  val mastersIO = Vec(2,new TLCTestMasterMMIO())
  val slaveIO = new TLCTestSlaveMMIO()
}

class TLCCacheTestTop()(implicit p: Parameters) extends LazyModule{

  val masters = Array.fill(2)(LazyModule(new TLCMasterMMIO()))

  val l2params = p(TLCCacheTestKey)

  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = l2params.ways,
      sets = l2params.capacityKB * 1024 / (l2params.blockBytes * l2params.ways * l2params.banks),
      blockBytes = l2params.blockBytes,
      beatBytes = l2params.beatBytes
    ),
    InclusiveCacheMicroParameters(
      writeBytes = l2params.beatBytes
    )
  ))

  val xbar = TLXbar()

  for(master <- masters){
    xbar := TLBuffer() := DebugIdentityNode() := master.node
  }
  l2.node := TLBuffer() := DebugIdentityNode() := xbar

  val slave = LazyModule(new TLCSlaveMMIO())
  slave.node := DebugIdentityNode() := l2.node

  lazy val module = new LazyModuleImp(this) with HasXSLog {

    val io = IO(new TLCCacheTestTopIO)

    slave.module.io <> io.slaveIO
    masters zip io.mastersIO map { case (m,i) =>
      m.module.io <> i
    }
  }
}

class TLCCacheTestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new TLCCacheTestTop())

  lazy val module = new LazyModuleImp(this){
    val io = IO(new TLCCacheTestTopIO)
    AddSinks()
    io <> testTop.module.io
  }
}

class TLCCacheTest extends FlatSpec with ChiselScalatestTester with Matchers{

  top.Parameters.set(top.Parameters.debugParameters)

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case TLCCacheTestKey =>
        TLCCacheTestParams()
    })

    test(LazyModule(new TLCCacheTestTopWrapper()).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)){ c =>
        c.io.mastersIO.foreach{ case mio =>
          mio.AChannel.initSource().setSourceClock(c.clock)
          mio.CChannel.initSource().setSourceClock(c.clock)
          mio.EChannel.initSource().setSourceClock(c.clock)
          mio.BChannel.initSink().setSinkClock(c.clock)
          mio.DChannel.initSink().setSinkClock(c.clock)
        }
        c.io.slaveIO.AChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.CChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.EChannel.initSink().setSinkClock(c.clock)
        c.io.slaveIO.BChannel.initSource().setSourceClock(c.clock)
        c.io.slaveIO.DChannel.initSource().setSourceClock(c.clock)

        fork{
          while (true){
            val AChannel_valid = c.io.mastersIO(0).AChannel.valid.litToBoolean
            if(AChannel_valid){
              
            }
          }
        }.fork{
          while (true){

          }
        }.fork{
          while (true){

          }
        }.join

        c.clock.setTimeout(100)
      }
  }

}