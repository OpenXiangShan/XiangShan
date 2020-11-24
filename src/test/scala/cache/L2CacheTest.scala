package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.LineCoverageAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan.cache.{DCache, DCacheLineReq, DCacheWordReq, MemoryOpConstants}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random


case class L2CacheTestParams
(
  ways: Int = 4,
  banks: Int = 1,
  capacityKB: Int = 4,
  blockBytes: Int = 64,
  beatBytes: Int = 32,
  writeBytes: Int = 8
) {
  require(blockBytes >= beatBytes)
}

case object L2CacheTestKey extends Field[L2CacheTestParams]

case class L3CacheTestParams
(
  ways: Int = 4,
  banks: Int = 1,
  capacityKB: Int = 4,
  blockBytes: Int = 64,
  beatBytes: Int = 32,
  writeBytes: Int = 8
) {
  require(blockBytes >= beatBytes)
}

case object L3CacheTestKey extends Field[L3CacheTestParams]


class L2TestTopIO extends Bundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val wdata = Input(UInt(64.W))
    val waddr = Input(UInt(20.W))
    val hartId = Input(UInt(1.W))
  }))
  val out = DecoupledIO(new Bundle() {
    val rdata = Output(UInt(64.W))
  })
}

class L2TestTop()(implicit p: Parameters) extends LazyModule{

  val cores = Array.fill(2)(LazyModule(new DCache()))
  val l2params = p(L2CacheTestKey)
  val l2s = (0 until 2) map (i =>
    LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = l2params.ways,
      sets = l2params.capacityKB * 1024 / (l2params.blockBytes * l2params.ways * l2params.banks),
      blockBytes = l2params.blockBytes,
      beatBytes = l2params.beatBytes,
      cacheName = s"L2_$i"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = l2params.writeBytes
    )
  )))

  val l3params = p(L3CacheTestKey)
  val l3 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 3,
      ways = l3params.ways,
      sets = l3params.capacityKB * 1024 / (l3params.blockBytes * l3params.ways * l3params.banks),
      blockBytes = l3params.blockBytes,
      beatBytes = l3params.beatBytes,
      cacheName = "L3"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = l3params.writeBytes
    )
  ))

  val ram = LazyModule(new AXI4RAM(
    Seq(AddressSet(0x0L, 0xffffffffffL)),
    memByte = 128 * 1024 * 1024,
    useBlackBox = false
  ))

  val xbar = TLXbar()

  for(i <- 0 until 2) {
    val core = cores(i)
    val l2 = l2s(i)
    xbar := l2.node := core.clientNode
  }

  l3.node := xbar

  ram.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLBuffer() :=
    TLCacheCork() :=
    l3.node

  lazy val module = new LazyModuleImp(this) with HasXSLog {

    val io = IO(new L2TestTopIO)

    val in = HoldUnless(io.in.bits, io.in.fire())

    cores.foreach(_.module.io <> DontCare)

    val storePorts = cores.map(_.module.io.lsu.store)
    val loadPorts  = cores.map(_.module.io.lsu.atomics)

    def sendStoreReq(addr: UInt, data: UInt): DCacheLineReq = {
      val req = Wire(new DCacheLineReq)
      req.cmd := MemoryOpConstants.M_XWR
      req.addr := addr
      req.data := data
      req.mask := Fill(req.mask.getWidth, true.B)
      req.meta := DontCare
      req
    }

    def sendLoadReq(addr: UInt): DCacheWordReq = {
      val req = Wire(new DCacheWordReq)
      req.cmd := MemoryOpConstants.M_XA_ADD
      req.addr := addr
      req.data := 0.U
      req.mask := Fill(req.mask.getWidth, true.B)
      req.meta := DontCare
      req
    }

    val s_idle :: s_write_req :: s_write_resp :: s_read_req :: s_read_resp :: s_finish :: Nil = Enum(6)
    val state = RegInit(s_idle)

    switch(state){
      is(s_idle){
        when(io.in.fire()){
          state := s_write_req
        }
      }
      is(s_write_req){
        when(storePorts.map(_.req.fire()).reduce(_||_)){
          state := s_write_resp
        }
      }
      is(s_write_resp){
        when(storePorts.map(_.resp.fire()).reduce(_||_)){
          state := s_read_req
        }
      }
      is(s_read_req){
        when(loadPorts.map(_.req.fire()).reduce(_||_)){
          state := s_read_resp
        }
      }
      is(s_read_resp){
        when(loadPorts.map(_.resp.fire()).reduce(_||_)){
          state := s_finish
        }
      }
    }

    io.in.ready := state === s_idle

    val storeReq = Wire(new DCacheLineReq)

    storeReq := sendStoreReq(in.waddr, Fill(8, in.wdata))

    storePorts.zipWithIndex.foreach{
      case (port, i) =>
        port.req.bits := storeReq
        port.req.valid := state===s_write_req && i.U===in.hartId
        port.resp.ready := true.B
        XSDebug(
          port.req.fire(),
          "write data %x to dcache [%d]\n",
          port.req.bits.data,
          i.U
        )
    }

    XSDebug(p"state: $state\n")

    val loadReq = sendLoadReq(in.waddr)

    loadPorts.zipWithIndex.foreach{
      case (port, i) =>
        port.req.bits := loadReq
        port.req.valid := state===s_read_req && i.U=/=in.hartId
        port.resp.ready := true.B
        XSDebug(
          port.resp.fire(),
          "read data %x form dcache [%d]\n",
          port.resp.bits.data,
          i.U
        )
    }

    val rdata = Reg(UInt(64.W))

    when(loadPorts.map(_.resp.fire()).reduce(_||_)){
      state := s_finish
      rdata := PriorityMux(
        loadPorts.map(p => p.resp.fire() -> p.resp.bits.data)
      )
    }

    io.out.bits.rdata := rdata
    io.out.valid := state === s_finish

    when(io.out.fire()){
      state := s_idle
    }
  }

}

class L2TestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new L2TestTop())

  lazy val module = new LazyModuleImp(this){
    val io = IO(new L2TestTopIO)

    AddSinks()

    io <> testTop.module.io
  }
}

class L2CacheTest extends AnyFlatSpec with ChiselScalatestTester with Matchers{

  top.Parameters.set(top.Parameters.debugParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    LineCoverageAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L2CacheTestKey =>
        L2CacheTestParams()
      case L3CacheTestKey =>
        L3CacheTestParams()
    })

     test(LazyModule(new L2TestTopWrapper()).module)
      .withAnnotations(annos){ c =>

        c.io.in.initSource().setSourceClock(c.clock)
        c.io.out.initSink().setSinkClock(c.clock)

        c.clock.step(100)

        for(i <- 0 until 100000){
          val addr = Random.nextInt(0xfffff) & 0xffe00 // align to block size
          val data = Random.nextLong() & 0x7fffffffffffffffL
          c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
            _.waddr -> addr.U,
            _.wdata -> data.U,
            _.hartId -> Random.nextInt(2).U
          ))
          c.io.out.expectDequeue(chiselTypeOf(c.io.out.bits).Lit(
            _.rdata -> data.U
          ))
        }
    }
  }

}
