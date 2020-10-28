package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar}
import org.scalatest.{FlatSpec, Matchers}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan.cache.{DCache, L1plusCache, DCacheLineReq, L1plusCacheReq, MemoryOpConstants}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random

case object L1plusCacheTestKey extends Field[Long]

class L1plusTestTopIO extends Bundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val wdata = Input(UInt(512.W))
    val waddr = Input(UInt(20.W))
  }))
  val out = DecoupledIO(new Bundle() {
    val rdata = Output(UInt(512.W))
  })
}

class L1plusTestTop()(implicit p: Parameters) extends LazyModule{

  val dcache = LazyModule(new DCache())
  val l1plusCache = LazyModule(new L1plusCache())
  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 4,
      sets = 4 * 1024 / (64 * 4 * 4),
      blockBytes = 64,
      beatBytes = 32,
      cacheName = s"L2"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 8
    )
  ))

  val ram = LazyModule(new AXI4RAM(
    AddressSet(0x0L, 0xffffffffffL),
    memByte = 128 * 1024 * 1024,
    useBlackBox = false
  ))

  val xbar = TLXbar()

  xbar := TLBuffer() := dcache.clientNode
  xbar := TLBuffer() := l1plusCache.clientNode

  l2.node := xbar

  ram.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLBuffer() :=
    TLCacheCork() :=
    l2.node

  lazy val module = new LazyModuleImp(this) with HasXSLog {

    val io = IO(new L1plusTestTopIO)

    val in = HoldUnless(io.in.bits, io.in.fire())

    dcache.module.io <> DontCare

    val storePort = dcache.module.io.lsu.store
    val loadPort  = l1plusCache.module.io

    def sendStoreReq(addr: UInt, data: UInt): DCacheLineReq = {
      val req = Wire(new DCacheLineReq)
      req.cmd := MemoryOpConstants.M_XWR
      req.addr := addr
      req.data := data
      req.mask := Fill(req.mask.getWidth, true.B)
      req.meta := DontCare
      req
    }

    def sendLoadReq(addr: UInt): L1plusCacheReq = {
      val req = Wire(new L1plusCacheReq)
      req.cmd  := MemoryOpConstants.M_XRD
      req.addr := addr
      req.id   := 0.U
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
        when(storePort.req.fire()) {
          state := s_write_resp
        }
      }
      is(s_write_resp){
        when(storePort.resp.fire()) {
          state := s_read_req
        }
      }
      is(s_read_req){
        when(loadPort.req.fire()) {
          state := s_read_resp
        }
      }
      is(s_read_resp){
        when(loadPort.resp.fire()) {
          state := s_finish
        }
      }
    }

    io.in.ready := state === s_idle

    val storeReq = Wire(new DCacheLineReq)

    storeReq := sendStoreReq(in.waddr, in.wdata)

    storePort.req.bits := storeReq
    storePort.req.valid := state===s_write_req
    storePort.resp.ready := true.B
    XSDebug(
      storePort.req.fire(),
      "write data %x to dcache\n",
      storePort.req.bits.data,
    )

    XSDebug(p"state: $state\n")

    val loadReq = sendLoadReq(in.waddr)

    loadPort.req.bits := loadReq
    loadPort.req.valid := state===s_read_req
    loadPort.resp.ready := true.B
    XSDebug(
      loadPort.resp.fire(),
      "read data %x form l1plusCache\n",
      loadPort.resp.bits.data,
    )

    val rdata = Reg(UInt(512.W))

    when(loadPort.resp.fire()) {
      state := s_finish
      rdata := loadPort.resp.bits.data
    }

    io.out.bits.rdata := rdata
    io.out.valid := state === s_finish

    when(io.out.fire()){
      state := s_idle
    }
  }

}

class L1plusTestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new L1plusTestTop())

  lazy val module = new LazyModuleImp(this){
    val io = IO(new L1plusTestTopIO)

    AddSinks()

    io <> testTop.module.io
  }
}

class L1plusCacheTest extends FlatSpec with ChiselScalatestTester with Matchers{

  top.Parameters.set(top.Parameters.debugParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L1plusCacheTestKey => 0
    })


    test(LazyModule(new L1plusTestTopWrapper()).module)
      .withAnnotations(annos){ c =>


        c.io.in.initSource().setSourceClock(c.clock)
        c.io.out.initSink().setSinkClock(c.clock)

        c.clock.step(100)

        val mem_size = 128 * 1024 * 1024
        val block_size = 64
        val nblocks = mem_size / block_size
        for(i <- 0 until nblocks){
          // we do not support l1plus flush for now
          // so we could only scan the whole memory,
          // and write every block for only once.
          // if we rewrite the same block multiple times
          // L1plus could not give correct data since it hasn't been flushed
          // val addr = Random.nextInt(0xfffff) & 0xffe00 // align to block size
          val addr = i * 64
          val words = (0 until 8) map { _ =>
            (BigInt(Random.nextLong() & 0x7fffffffffffffffL))
          }

          val data = words.foldLeft(BigInt(0))((sum, i) => sum << 64 | i)

          c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
            _.waddr -> addr.U,
            _.wdata -> data.U
          ))
          c.io.out.expectDequeue(chiselTypeOf(c.io.out.bits).Lit(
            _.rdata -> data.U
          ))
        }
    }
  }

}
