package xiangshan.cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.experimental.UnlocatableSourceInfo
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class ProbeQueueEccHarness(implicit p: Parameters) extends DCacheModule {
  private val transfer = TransferSizes(cfg.blockBytes)
  private val client = TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(
      name = "probeq-test",
      sourceId = IdRange(0, 16),
      supportsProbe = transfer,
      supportsGet = transfer,
      supportsPutFull = transfer,
      supportsPutPartial = transfer,
      supportsArithmetic = transfer,
      supportsLogical = transfer,
      supportsHint = transfer)))
  private val manager = TLSlavePortParameters.v1(Seq(
    TLSlaveParameters.v1(
      address = Seq(AddressSet(0, 0xffffffffffffL)),
      regionType = RegionType.UNCACHED,
      supportsGet = transfer,
      supportsPutFull = transfer,
      supportsPutPartial = transfer)),
    beatBytes = cfg.blockBytes)
  private val edge = new TLEdgeOut(client, manager, p, UnlocatableSourceInfo)

  val io = IO(new Bundle {
    val tagReq = Flipped(DecoupledIO(new EccEvictReq))
    val dataReq = Flipped(DecoupledIO(new EccEvictReq))
    val pipeReq = DecoupledIO(new MainPipeReq)
    val evictDone = Input(Valid(Bool()))
    val evictComplete = Input(Valid(new EccEvictComplete))
  })

  val dut = Module(new ProbeQueue(edge))
  dut.io.mem_probe.valid := false.B
  dut.io.mem_probe.bits := 0.U.asTypeOf(dut.io.mem_probe.bits)
  dut.io.tag_evict <> io.tagReq
  dut.io.data_evict <> io.dataReq
  io.pipeReq <> dut.io.pipe_req
  dut.io.lrsc_locked_block.valid := false.B
  dut.io.lrsc_locked_block.bits := 0.U
  dut.io.update_resv_set := false.B
  dut.io.evict_done := io.evictDone
  dut.io.evict_complete := io.evictComplete
}

class ProbeQueueEccTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val base = new DefaultConfig
    base.alterPartial({ case XSCoreParamsKey => base(XSTileKey).head.copy() }).alterPartial({
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    })
  }

  private def init(d: ProbeQueueEccHarness): Unit = {
    d.io.tagReq.valid.poke(false.B)
    d.io.tagReq.bits.poke(0.U.asTypeOf(d.io.tagReq.bits))
    d.io.dataReq.valid.poke(false.B)
    d.io.dataReq.bits.poke(0.U.asTypeOf(d.io.dataReq.bits))
    d.io.pipeReq.ready.poke(false.B)
    d.io.evictDone.valid.poke(false.B)
    d.io.evictDone.bits.poke(false.B)
    d.io.evictComplete.valid.poke(false.B)
    d.io.evictComplete.bits.poke(0.U.asTypeOf(d.io.evictComplete.bits))
    d.reset.poke(true.B)
    d.clock.step(2)
    d.reset.poke(false.B)
    d.clock.step()
  }

  private def submitData(d: ProbeQueueEccHarness, addr: BigInt): Unit = {
    d.io.dataReq.bits.addr.poke(addr.U)
    d.io.dataReq.bits.vaddr.poke(addr.U)
    d.io.dataReq.bits.way_en.poke(2.U)
    d.io.dataReq.valid.poke(true.B)
    assert(d.io.dataReq.ready.peek().litToBoolean)
    d.clock.step()
    d.io.dataReq.valid.poke(false.B)
  }

  behavior of "ProbeQueue ECC evict arbitration"

  it should "prioritize tag evict and issue it through MainPipe" in {
    implicit val p = config
    simulate(new ProbeQueueEccHarness) { d =>
      init(d)
      val addr = BigInt("80004000", 16)
      d.io.tagReq.bits.addr.poke(addr.U)
      d.io.tagReq.bits.vaddr.poke(addr.U)
      d.io.tagReq.bits.way_en.poke(1.U)
      d.io.dataReq.bits.addr.poke(addr.U)
      d.io.dataReq.bits.vaddr.poke(addr.U)
      d.io.dataReq.bits.way_en.poke(2.U)
      d.io.tagReq.valid.poke(true.B)
      d.io.dataReq.valid.poke(true.B)
      d.clock.step()
      d.io.tagReq.valid.poke(false.B)
      d.io.dataReq.valid.poke(false.B)
      d.clock.step()
      assert(d.io.pipeReq.valid.peek().litToBoolean)
      assert(d.io.pipeReq.bits.local_evict_tag.peek().litToBoolean, "tag ECC evict must win same-cycle arbitration")
      assert(d.io.pipeReq.bits.local_evict_way_en.peek().litValue == 1)
      d.io.pipeReq.ready.poke(true.B)
      d.clock.step()
      d.io.pipeReq.ready.poke(false.B)
      d.io.evictDone.valid.poke(true.B)
      d.io.evictDone.bits.poke(true.B)
      d.clock.step()
      d.io.evictDone.valid.poke(false.B)
      assert(d.io.pipeReq.valid.peek().litToBoolean, "data ECC evict must issue after tag completion")
      assert(!d.io.pipeReq.bits.local_evict_tag.peek().litToBoolean)
    }
  }

  it should "free data evict on no-WBQ completion" in {
    implicit val p = config
    simulate(new ProbeQueueEccHarness) { d =>
      init(d)
      val addr = BigInt("80008000", 16)
      submitData(d, addr)
      d.clock.step()
      assert(d.io.pipeReq.valid.peek().litToBoolean)
      assert(!d.io.pipeReq.bits.local_evict_tag.peek().litToBoolean)
      d.io.pipeReq.ready.poke(true.B)
      d.clock.step()
      d.io.pipeReq.ready.poke(false.B)
      d.io.evictDone.valid.poke(true.B)
      d.io.evictDone.bits.poke(false.B)
      d.clock.step()
      d.io.evictDone.valid.poke(false.B)
      assert(d.io.dataReq.ready.peek().litToBoolean, "data evict entry must be free after no-WBQ completion")
    }
  }
}
