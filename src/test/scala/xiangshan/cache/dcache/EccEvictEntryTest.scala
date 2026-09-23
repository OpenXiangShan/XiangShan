package xiangshan.cache

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class EccEvictEntryTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val defaultConfig = new DefaultConfig
    defaultConfig.alterPartial({ case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy() })
  }

  private def init(d: EccEvictEntry): Unit = {
    d.io.req.valid.poke(false.B)
    d.io.req.bits.addr.poke(0.U)
    d.io.req.bits.vaddr.poke(0.U)
    d.io.req.bits.way_en.poke(0.U)
    d.io.pipe_req.ready.poke(false.B)
    d.io.pipe_done.poke(false.B)
    d.io.evict_complete.valid.poke(false.B)
    d.io.evict_complete.bits.addr.poke(0.U)
    d.io.evict_complete.bits.tag.poke(false.B)
    d.reset.poke(true.B)
    d.clock.step(2)
    d.reset.poke(false.B)
    d.clock.step()
  }

  private def submit(d: EccEvictEntry, addr: BigInt, vaddr: BigInt, way: BigInt): Unit = {
    d.io.req.bits.addr.poke(addr.U)
    d.io.req.bits.vaddr.poke(vaddr.U)
    d.io.req.bits.way_en.poke(way.U)
    d.io.req.valid.poke(true.B)
    assert(d.io.req.ready.peek().litToBoolean, "entry must accept an idle request")
    d.clock.step()
    d.io.req.valid.poke(false.B)
  }

  behavior of "ECC evict entry"

  it should "hold a tag evict entry until matching WBQ completion" in {
    implicit val p = config
    simulate(new EccEvictEntry(isTag = true)) { d =>
      val addr = BigInt("80004000", 16)
      val vaddr = BigInt("4000", 16)
      init(d)
      submit(d, addr, vaddr, 1)
      assert(d.io.pipe_req.valid.peek().litToBoolean)
      assert(d.io.pipe_req.bits.local_evict.peek().litToBoolean)
      assert(d.io.pipe_req.bits.local_evict_tag.peek().litToBoolean)
      assert(d.io.pipe_req.bits.addr.peek().litValue == addr)
      assert(d.io.pipe_req.bits.vaddr.peek().litValue == vaddr)
      assert(d.io.pipe_req.bits.local_evict_way_en.peek().litValue == 1)

      d.io.pipe_req.ready.poke(true.B)
      d.clock.step()
      d.io.pipe_req.ready.poke(false.B)
      assert(!d.io.req.ready.peek().litToBoolean, "entry must remain allocated after MainPipe accepts it")

      d.io.evict_complete.valid.poke(true.B)
      d.io.evict_complete.bits.addr.poke((addr + 64).U)
      d.io.evict_complete.bits.tag.poke(true.B)
      d.clock.step()
      d.io.evict_complete.valid.poke(false.B)
      assert(!d.io.req.ready.peek().litToBoolean, "different address must not complete entry")

      d.io.evict_complete.valid.poke(true.B)
      d.io.evict_complete.bits.addr.poke(addr.U)
      d.io.evict_complete.bits.tag.poke(false.B)
      d.clock.step()
      d.io.evict_complete.valid.poke(false.B)
      assert(!d.io.req.ready.peek().litToBoolean, "data completion must not complete tag entry")

      d.io.evict_complete.valid.poke(true.B)
      d.io.evict_complete.bits.addr.poke(addr.U)
      d.io.evict_complete.bits.tag.poke(true.B)
      d.clock.step()
      d.io.evict_complete.valid.poke(false.B)
      assert(d.io.req.ready.peek().litToBoolean, "matching ReleaseAck completion must free tag entry")
    }
  }

  it should "match data completion type and support no-WBQ completion" in {
    implicit val p = config
    simulate(new EccEvictEntry(isTag = false)) { d =>
      val addr = BigInt("80008000", 16)
      init(d)
      submit(d, addr, addr, 2)
      d.io.pipe_req.ready.poke(true.B)
      d.clock.step()
      d.io.pipe_req.ready.poke(false.B)

      d.io.evict_complete.valid.poke(true.B)
      d.io.evict_complete.bits.addr.poke(addr.U)
      d.io.evict_complete.bits.tag.poke(true.B)
      d.clock.step()
      d.io.evict_complete.valid.poke(false.B)
      assert(!d.io.req.ready.peek().litToBoolean, "tag completion must not complete data entry")

      d.io.pipe_done.poke(true.B)
      d.clock.step()
      d.io.pipe_done.poke(false.B)
      assert(d.io.req.ready.peek().litToBoolean, "MainPipe no-WBQ completion must free data entry")
    }
  }
}
