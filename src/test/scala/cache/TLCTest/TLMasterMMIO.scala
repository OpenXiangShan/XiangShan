package cache.TLCTest

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters}
import xiangshan.cache.{DCacheBundle, HasDCacheParameters}

class TLCFakeBundle extends DCacheBundle
{
  val sourceBits = 5 //maybe parameterized later
}

class TLCFakeABundle extends TLCFakeBundle
{
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(3.W)
  val source = UInt(sourceBits.W)
  val address = UInt(64.W)
  val mask = UInt((l1BusDataWidth/8).W)
  val data = UInt(l1BusDataWidth.W)
}
class TLCFakeCBundle extends TLCFakeBundle
{
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(3.W)
  val source = UInt(sourceBits.W)
  val address = UInt(64.W)
  val data = UInt(l1BusDataWidth.W)
}
class TLCFakeEBundle extends TLCFakeBundle
{
  val sink = UInt(8.W) //maybe enough
}
class TLCFakeBBundle extends TLCFakeBundle
{
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(3.W)
  val source = UInt(sourceBits.W)
  val address = UInt(64.W)
  val mask = UInt((l1BusDataWidth/8).W)
  val data = UInt(l1BusDataWidth.W)
}
class TLCFakeDBundle extends TLCFakeBundle
{
  val opcode = UInt(3.W)
  val param = UInt(3.W)
  val size = UInt(3.W)
  val source = UInt(sourceBits.W)
  val sink = UInt(8.W)//maybe enough
  val denied = Bool()
  val data = UInt(l1BusDataWidth.W)
}

class TLCTestMasterMMIO extends DCacheBundle
{
  val AChannel = Flipped(DecoupledIO(new TLCFakeABundle()))
  val CChannel = Flipped(DecoupledIO(new TLCFakeCBundle()))
  val EChannel = Flipped(DecoupledIO(new TLCFakeEBundle()))
  val BChannel = DecoupledIO(new TLCFakeBBundle())
  val DChannel = DecoupledIO(new TLCFakeDBundle())
}


class TLCMasterMMIO()(implicit p: Parameters) extends LazyModule
  with HasDCacheParameters{

  val l2params = p(TLCCacheTestKey)
  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "fake_master",
      sourceId = IdRange(0, cfg.nMissEntries+1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    ))
  )

  val node = TLClientNode(Seq(clientParameters))


  lazy val module = new LazyModuleImp(this) {

    val (bus,edge) = node.out.head

    val io = IO(new TLCTestMasterMMIO)

    bus.a.bits.opcode := io.AChannel.bits.opcode
    bus.a.bits.param := io.AChannel.bits.param
    bus.a.bits.size := io.AChannel.bits.size
    bus.a.bits.source := io.AChannel.bits.source
    bus.a.bits.address := io.AChannel.bits.address
    bus.a.bits.mask := io.AChannel.bits.mask
    bus.a.bits.data := io.AChannel.bits.data
    bus.a.bits.corrupt := false.B
    bus.a.valid := io.AChannel.valid
    io.AChannel.ready := bus.a.ready

    bus.c.bits.opcode := io.CChannel.bits.opcode
    bus.c.bits.param := io.CChannel.bits.param
    bus.c.bits.size := io.CChannel.bits.size
    bus.c.bits.source := io.CChannel.bits.source
    bus.c.bits.address := io.CChannel.bits.address
    bus.c.bits.data := io.CChannel.bits.data
    bus.c.bits.corrupt := false.B
    bus.c.valid := io.CChannel.valid
    io.CChannel.ready := bus.c.ready

    bus.e.bits.sink := io.EChannel.bits.sink
    bus.e.valid := io.EChannel.valid
    io.EChannel.ready := bus.e.ready

    io.BChannel.bits.opcode := bus.b.bits.opcode
    io.BChannel.bits.param := bus.b.bits.param
    io.BChannel.bits.size := bus.b.bits.size
    io.BChannel.bits.source := bus.b.bits.source
    io.BChannel.bits.address := bus.b.bits.address
    io.BChannel.bits.mask := bus.b.bits.mask
    io.BChannel.bits.data := bus.b.bits.data
    io.BChannel.valid := bus.b.valid
    bus.b.ready := io.BChannel.ready

    io.DChannel.bits.opcode := bus.d.bits.opcode
    io.DChannel.bits.param := bus.d.bits.param
    io.DChannel.bits.size := bus.d.bits.size
    io.DChannel.bits.source := bus.d.bits.source
    io.DChannel.bits.sink := bus.d.bits.sink
    io.DChannel.bits.denied := bus.d.bits.denied
    io.DChannel.bits.data := bus.d.bits.data
    io.DChannel.valid := bus.d.valid
    bus.d.ready := io.DChannel.ready

  }

}
