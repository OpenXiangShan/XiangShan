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
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.MicroOp
import xiangshan.cache.{DCache, DCacheLineIO, L1plusCache, L1plusCacheIO, MemoryOpConstants}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName

import scala.util.Random

case object L1plusCacheTestKey extends Field[Long]

class L1plusTestTopIO extends Bundle {
  val l1plus = new L1plusCacheIO()
  val dcacheStore = new DCacheLineIO()
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
    Seq(AddressSet(0x0L, 0xffffffffffL)),
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

  lazy val module = new LazyModuleImp(this) {

    val io = IO(Flipped(new L1plusTestTopIO))

    AddSinks()

    dcache.module.io <> DontCare

    dcache.module.io.lsu.store <> io.dcacheStore
    l1plusCache.module.io <> io.l1plus
  }

}

class L1plusCacheTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "L1plusCache"

  val mem_size = 128 * 1024 * 1024
  val block_size = 64
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


    test(LazyModule(new L1plusTestTop()).module)
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
        }

        def flush_l1plus() = {
          c.io.l1plus.flush.poke(true.B)
          while (!c.io.l1plus.empty.peek().litToBoolean) {
            c.clock.step()
          }
          c.io.l1plus.flush.poke(false.B)
        }

        def evaluate() = {
          while (!sq.isFinished() || !lq.isFinished()) {
            sq.tick(c.io.dcacheStore)
            lq.tick(c.io.l1plus)
            c.clock.step()
          }
        }

        // ----------------------------------------
        // scan test
        // write every memory block and then read out every memory cell
        def scan_test() = {
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

        scan_test()

        // self_modify_test
        def self_modify_test() = {
          println(s"self_modify_test")
          for (i <- 0 until 10) {
            flush_l1plus()
            scan_test()
          }
        }

        self_modify_test()
      }
  }
}

// emulated queue
class IdPool(val nReqIds: Int, name: String) {
  val freeIds = new Array[Boolean](nReqIds)

  def allocate(): Int = {
    for (i <- 0 until freeIds.size) {
      if (freeIds(i)) {
        println(f"$name allocate: $i")
        freeIds(i) = false
        return i
      }
    }
    // no free id to allocate
    println(f"$name allocate failed")
    return -1
  }

  def free(id: Int): Unit = {
    println(f"$name free: $id")
    assert(!freeIds(id))
    freeIds(id) = true
  }

  def init(): Unit = {
    for (i <- 0 until freeIds.size) {
      freeIds(i) = true
    }
  }
}

case class Req(
  addr: Long,
  data: BigInt
) {
  override def toString() : String = {
    return f"addr: $addr%x data: $data%x"
  }
}

case class QueueEntry(
  var id: Int, // it's transaction id
  req: Req
) {
  override def toString() : String = {
    return f"id: $id%d req: $req"
  }
}

class Queue(nEntries: Int, name: String) {
  // Queue
  // ---------------------------------------
  val idPool = new IdPool(nEntries, name + "IdPool")
  val queue = new ArrayBuffer[QueueEntry]()
  def enq(req: Req) = {
    // for unissued reqs, they have id = -1
    queue += new QueueEntry(-1, req)
  }

  // select a req to issue
  // req with id == -1 are not issued
  def select(): Int = {
    for (i <- 0 until queue.size) {
      if (queue(i).id == -1)
        return i
    }
    return -1
  }

  // retire the req with transaction id tId
  def retire(tId: Int): Unit = {
    println(f"$name retire transaction: $tId%d")
    for (i <- 0 until queue.size) {
      if (queue(i).id == tId) {
        // remove this request
        queue.remove(i)
        println(f"$name retire req: $i%d transaction: $tId%d")
        return
      }
    }
    assert(false)
  }

  def issue(idx: Int, tId: Int) = {
    println(f"$name issue req: $idx%d transaction: $tId%d")
    assert(queue(idx).id == -1)
    queue(idx).id = tId
  }

  // look up req by transaction id tId
  def lookUp(tId: Int): Req = {
    for (i <- 0 until queue.size) {
      if (queue(i).id == tId) {
        // remove this request
        return queue(i).req
      }
    }
    // we must return a value
    // just to make scala happy
    return Req(0, 0)
  }

  var reqWaiting = false

  def init(): Unit = {
    idPool.init()
    queue.clear()
    reqWaiting = false
  }

  def isFinished() = queue.isEmpty
}

class StoreQueue(nEntries: Int) extends Queue(nEntries, "StoreQueue") {
  def sendReq(port: DCacheLineIO): Unit = {
    val req = port.req
    // has last cycle's req been fired?
    // can we send a new request in this cycle
    if (!reqWaiting) {
      val reqIdx = select()
      if (reqIdx == -1) {
        // no more request to send
        req.valid.poke(false.B)
        return
      }

      val tId = idPool.allocate()
      if (tId == -1) {
        // no more request to send
        req.valid.poke(false.B)
        return
      }

      // try sending a new request in this cycle
      // select a  req to issue

      reqWaiting = true

      issue(reqIdx, tId)

      val CMD_WRITE = MemoryOpConstants.M_XWR
      val FULL_MASK = BigInt("ffffffffffffffff", 16).U

      val r = queue(reqIdx).req
      req.valid.poke(true.B)
      req.bits.cmd.poke(CMD_WRITE)
      req.bits.addr.poke(r.addr.U)
      req.bits.data.poke(r.data.U)
      req.bits.mask.poke(FULL_MASK)
      req.bits.id.poke(tId.U)
    }

    if (req.valid.peek().litToBoolean && req.ready.peek().litToBoolean) {
      reqWaiting = false
    }
  }

  def handleResp(port: DCacheLineIO) = {
    val resp = port.resp
    // always ready
    resp.ready.poke(true.B)
    if (resp.valid.peek().litToBoolean) {
      val id = resp.bits.id.peek().litValue.longValue.toInt
      idPool.free(id)
      retire(id)
    }
  }

  def tick(port: DCacheLineIO) = {
    // first, try to send reqs
    sendReq(port)
    // then, receive responses
    handleResp(port)
  }
}

class LoadQueue(nEntries: Int) extends Queue(nEntries, "LoadQueue") {
  def sendReq(port: L1plusCacheIO): Unit = {
    val req = port.req
    // has last cycle's req been fired?
    // can we send a new request in this cycle
    if (!reqWaiting) {
      val reqIdx = select()
      if (reqIdx == -1) {
        // no more request to send
        req.valid.poke(false.B)
        return
      }

      val tId = idPool.allocate()
      if (tId == -1) {
        // no more request to send
        req.valid.poke(false.B)
        return
      }

      // try sending a new request in this cycle
      // select a  req to issue

      reqWaiting = true
      issue(reqIdx, tId)

      val CMD_READ = MemoryOpConstants.M_XRD

      val r = queue(reqIdx).req
      req.valid.poke(true.B)
      req.bits.cmd.poke(CMD_READ)
      req.bits.addr.poke(r.addr.U)
      req.bits.id.poke(tId.U)
    }

    if (req.valid.peek().litToBoolean && req.ready.peek().litToBoolean) {
      reqWaiting = false
    }
  }

  def handleResp(port: L1plusCacheIO) = {
    val resp = port.resp
    // always ready
    resp.ready.poke(true.B)
    if (resp.valid.peek().litToBoolean) {
      val id = resp.bits.id.peek().litValue.longValue.toInt
      val rdata = resp.bits.data.peek().litValue
      val r = lookUp(id)
      assert(r.data == rdata)
      idPool.free(id)
      retire(id)
    }
  }

  def tick(port: L1plusCacheIO) = {
    // first, try to send reqs
    sendReq(port)
    // then, receive responses
    handleResp(port)
  }
}
