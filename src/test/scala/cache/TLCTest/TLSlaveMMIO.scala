package cache.TLCTest

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, SimpleDevice, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLManagerNode, TLSlaveParameters, TLSlavePortParameters}
import xiangshan.cache.{DCacheBundle, HasDCacheParameters}
 
class TLCTestSlaveMMIO extends DCacheBundle
{
  val AChannel = DecoupledIO(new TLCFakeABundle())
  val CChannel = DecoupledIO(new TLCFakeCBundle())
  val EChannel = DecoupledIO(new TLCFakeEBundle())
  val BChannel = Flipped(DecoupledIO(new TLCFakeBBundle()))
  val DChannel = Flipped(DecoupledIO(new TLCFakeDBundle()))
}


class TLCSlaveMMIO()(implicit p: Parameters) extends LazyModule
  with HasDCacheParameters{

  val l2params = p(TLCCacheTestKey)

  val device = new SimpleDevice("fake-llc", Seq())

  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(
      address            = List(AddressSet(0x0L, 0xffffffffffL)),
      resources          = device.reg,
      regionType         = RegionType.CACHED,
      supportsGet        = TransferSizes(1, blockBytes),
      supportsPutPartial = TransferSizes(1, blockBytes),
      supportsPutFull    = TransferSizes(1, blockBytes),
      supportsAcquireT   = TransferSizes(1, blockBytes),
      supportsAcquireB   = TransferSizes(1, blockBytes),

    )), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 0,
    endSinkId = 16,
  )))


  lazy val module = new LazyModuleImp(this) {

    val (bus,edge) = node.in.head

    val io = IO(new TLCTestSlaveMMIO)

    io.AChannel.bits.opcode := bus.a.bits.opcode
    io.AChannel.bits.param := bus.a.bits.param
    io.AChannel.bits.size := bus.a.bits.size
    io.AChannel.bits.source := bus.a.bits.source
    io.AChannel.bits.address := bus.a.bits.address
    io.AChannel.bits.mask := bus.a.bits.mask
    io.AChannel.bits.data := bus.a.bits.data
    io.AChannel.valid := bus.a.valid
    bus.a.ready := io.AChannel.ready

    io.CChannel.bits.opcode := bus.c.bits.opcode
    io.CChannel.bits.param := bus.c.bits.param
    io.CChannel.bits.size := bus.c.bits.size
    io.CChannel.bits.source := bus.c.bits.source
    io.CChannel.bits.address := bus.c.bits.address
    io.CChannel.bits.data := bus.c.bits.data
    io.CChannel.valid := bus.c.valid
    bus.c.ready := io.CChannel.ready

    io.EChannel.bits.sink := bus.e.bits.sink
    io.EChannel.valid := bus.e.valid
    bus.e.ready := io.EChannel.ready

    bus.b.bits.opcode := io.BChannel.bits.opcode
    bus.b.bits.param := io.BChannel.bits.param
    bus.b.bits.size := io.BChannel.bits.size
    bus.b.bits.source := io.BChannel.bits.source
    bus.b.bits.address := io.BChannel.bits.address
    bus.b.bits.mask := io.BChannel.bits.mask
    bus.b.bits.data := io.BChannel.bits.data
    bus.b.valid := io.BChannel.valid
    io.BChannel.ready := bus.b.ready

    bus.d.bits.opcode := io.DChannel.bits.opcode
    bus.d.bits.param := io.DChannel.bits.param
    bus.d.bits.size := io.DChannel.bits.size
    bus.d.bits.source := io.DChannel.bits.source
    bus.d.bits.sink := io.DChannel.bits.sink
    bus.d.bits.denied := io.DChannel.bits.denied
    bus.d.bits.data := io.DChannel.bits.data
    bus.d.valid := io.DChannel.valid
    io.DChannel.ready := bus.d.ready

  }

}
