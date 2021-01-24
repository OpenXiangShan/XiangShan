package cache

import scala.collection.mutable.ArrayBuffer
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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheControlParameters, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.{HasXSLog, MicroOp}
import xiangshan.cache.{DCache, DCacheLineIO, DCacheWordIO, L1plusCache, L1plusCacheIO, MemoryOpConstants, Uncache}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random

class L2NonInclusiveGetTestTopIO extends Bundle {
  val l1plus = new L1plusCacheIO()
  val dcacheStore = new DCacheLineIO()
  val l2Flush = new DCacheWordIO
}

class L2NonInclusiveGetTestTop()(implicit p: Parameters) extends LazyModule {
  val uncache = LazyModule(new Uncache())
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
    ),
    Some(InclusiveCacheControlParameters(
      address = 0x8000000L,
      beatBytes = 8))))

  val ram = LazyModule(new AXI4RAM(
    Seq(AddressSet(0x0L, 0x7ffffffL)),
    memByte = 128 * 1024 * 1024,
    useBlackBox = false
  ))

  val xbar = TLXbar()

  xbar := TLBuffer() := DebugIdentityNode() := dcache.clientNode
  xbar := TLBuffer() := DebugIdentityNode() := l1plusCache.clientNode

  l2.node := DebugIdentityNode() := xbar

  ram.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLBuffer() :=
    TLCacheCork() :=
    DebugIdentityNode() := 
    l2.node

  // connect uncache access to l2 control node
  l2.ctlnode.get := DebugIdentityNode() := uncache.clientNode

  lazy val module = new LazyModuleImp(this) with HasXSLog {

    val io = IO(Flipped(new L2NonInclusiveGetTestTopIO))

    AddSinks()

    dcache.module.io <> DontCare

    dcache.module.io.lsu.store <> io.dcacheStore
    l1plusCache.module.io <> io.l1plus
    uncache.module.io.lsq <> io.l2Flush
  }
}

class L2NonInclusiveGetTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "L2Cache"

  val mem_size = 128 * 1024 * 1024
  val block_size = 64
  val block_bits = log2Up(block_size)
  // val nblocks = mem_size / block_size
  val nblocks = 100

  // data structures
  // our golden version cache
  val cache_blocks = new Array[BigInt](nblocks)
  for (i <- 0 until nblocks) {
    cache_blocks(i) = BigInt(0)
  }

  // ----------------------------------------
  // useful request parameter values
  val CMD_READ = MemoryOpConstants.M_XRD
  val CMD_WRITE = MemoryOpConstants.M_XWR
  // 64bit full mask
  val FULL_MASK_64 = BigInt("ffffffffffffffff", 16).U
  val L2_FLUSH_BASE_ADDR = 0x8000000L
  val CONFIG_ADDR = L2_FLUSH_BASE_ADDR + 0x0
  val FLUSH64_ADDR = L2_FLUSH_BASE_ADDR + 0x200
  val FLUSH32_ADDR = L2_FLUSH_BASE_ADDR + 0x240

  val r = scala.util.Random

  top.Parameters.set(top.Parameters.debugParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case L1plusCacheTestKey => 0
    })


    test(LazyModule(new L2NonInclusiveGetTestTop()).module)
      .withAnnotations(annos){ c =>

        c.clock.step(100)

        val sq = new StoreQueue(8)
        val lq = new LoadQueue(8)

        def init() = {
          sq.init()
          lq.init()

          // initialize DUT inputs
          c.io.dcacheStore.req.valid.poke(false.B)
          c.io.dcacheStore.resp.ready.poke(false.B)
          c.io.l1plus.req.valid.poke(false.B)
          c.io.l1plus.resp.ready.poke(false.B)
          c.io.l1plus.flush.poke(false.B)
          c.io.l2Flush.req.valid.poke(false.B)
          c.io.l2Flush.resp.ready.poke(false.B)
        }

        def mmio_read(addr: BigInt): BigInt = {
          // send req
          val req = c.io.l2Flush.req
          req.valid.poke(true.B)
          req.bits.cmd.poke(CMD_READ)
          req.bits.addr.poke(addr.U)
          req.bits.data.poke(0.U)
          req.bits.mask.poke(FULL_MASK_64)
          req.bits.id.poke(0.U)

          while (!req.ready.peek().litToBoolean) {
            c.clock.step()
          }
          // actually send the req
          c.clock.step()

          // lower valid
          req.valid.poke(false.B)

          // recv resp
          val resp = c.io.l2Flush.resp
          resp.ready.poke(true.B)
          while (!resp.valid.peek().litToBoolean) {
            c.clock.step()
          }
          val data = resp.bits.data.peek().litValue
          // actually recv the response
          c.clock.step()

          // lower ready
          resp.ready.poke(false.B)

          return data
        }

        def mmio_write(addr: BigInt, data: BigInt) = {
          // send req
          val req = c.io.l2Flush.req
          req.valid.poke(true.B)
          req.bits.cmd.poke(CMD_WRITE)
          req.bits.addr.poke(addr.U)
          req.bits.data.poke(data.U)
          req.bits.mask.poke(FULL_MASK_64)
          req.bits.id.poke(0.U)

          while (!req.ready.peek().litToBoolean) {
            c.clock.step()
          }
          // actually send the req
          c.clock.step()

          // lower valid
          req.valid.poke(false.B) // recv resp
          val resp = c.io.l2Flush.resp
          resp.ready.poke(true.B)
          while (!resp.valid.peek().litToBoolean) {
            c.clock.step()
          }
          // actually recv the response
          c.clock.step()

          // lower ready
          resp.ready.poke(false.B)
        }

        def get_l2_configurations() = {
          val config = mmio_read(CONFIG_ADDR)
          val nBank = config & 0xf
          val nWay = config >> 8 & 0xf
          val nSet = 1 << (config.toInt >> 16 & 0xf)
          val nBlock = 1 << (config.toInt >> 24 & 0xf)
          println(f"L2 configuration: nBank: $nBank nWay: $nWay nSet: $nSet nBlock: $nBlock")
        }

        def flush_l2_block(addr: BigInt) = {
          mmio_write(FLUSH64_ADDR, addr)
          println(f"L2 flush block: $addr%x")
        }

        def flush_l1plus() = {
          c.io.l1plus.flush.poke(true.B)
          while (!c.io.l1plus.empty.peek().litToBoolean) {
            c.clock.step()
          }
          c.io.l1plus.flush.poke(false.B)
        }

        def flush_l2_range(begin: BigInt, end: BigInt) = {
          var addr = begin >> block_bits << block_bits
          while (addr < end) {
            flush_l2_block(addr)
            addr += block_size
          }
        }

        def evaluate() = {
          while (!sq.isFinished() || !lq.isFinished()) {
            sq.tick(c.io.dcacheStore)
            lq.tick(c.io.l1plus)
            c.clock.step()
          }
        }

        get_l2_configurations()

        // ----------------------------------------
        // scan test
        def populate_memory() = {
          println(s"scan test")
          init()
          // first, initialize every memory block with random numbers
          for (i <- 0 until nblocks) {
            val addr = i * 64
            val words = (0 until 8) map { _ =>
              (BigInt(r.nextLong() & 0x7fffffffffffffffL))
            }
            val data = words.foldLeft(BigInt(0))((sum, i) => sum << 64 | i)
            cache_blocks(i) = data
            println(f"enq store addr: $addr%x data: $data%x")
            sq.enq(Req(addr, data))
          }
          // execute reqs
          evaluate()
        }

        def flush_memory() = {
          flush_l2_range(0, (nblocks - 1)* block_size)
        }

        def read_memory() = {
          // read them out
          for (i <- 0 until nblocks) {
            val addr = i * 64
            val data = cache_blocks(i)
            println(f"enq load addr: $addr%x data: $data%x")
            lq.enq(Req(addr, data))
          }
          // execute reqs
          evaluate()
        }

        for (i <- 0 until 10) {
          populate_memory()
          flush_memory()
          // these loads should cause get miss
          flush_l1plus()
          read_memory()

          populate_memory()
          // these loads should not miss
          flush_l1plus()
          read_memory()
        }
      }
  }
}
