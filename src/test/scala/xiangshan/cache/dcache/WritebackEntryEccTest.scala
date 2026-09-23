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

class WritebackEntryEccHarness(implicit p: Parameters) extends DCacheModule {
  private val tlBeatBytes = cfg.blockBytes
  private val transfer = TransferSizes(tlBeatBytes)
  private val client = TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(
      name = "wbq-test",
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
      regionType = RegionType.CACHED,
      supportsAcquireT = transfer,
      supportsAcquireB = transfer,
      supportsGet = transfer,
      supportsPutFull = transfer,
      supportsPutPartial = transfer)),
    beatBytes = tlBeatBytes,
    endSinkId = 1)
  private val edge = new TLEdgeOut(client, manager, p, UnlocatableSourceInfo)

  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReqWodata))
    val reqData = Input(new WritebackReqData)
    val primaryValid = Input(Bool())
    val release = DecoupledIO(new TLBundleC(edge.bundle))
    val grantValid = Input(Bool())
    val grantSource = Input(UInt(log2Up(16).W))
    val grantReady = Output(Bool())
    val expectedBeats = Output(UInt(8.W))
    val evictComplete = Output(Valid(new EccEvictComplete))
  })

  val dut = Module(new WritebackEntry(edge))
  dut.io.id := 9.U
  dut.io.req <> io.req
  dut.io.req_data := io.reqData
  dut.io.primary_valid := io.primaryValid
  io.release <> dut.io.mem_release
  dut.io.mem_grant.valid := io.grantValid
  dut.io.mem_grant.bits := 0.U.asTypeOf(dut.io.mem_grant.bits)
  dut.io.mem_grant.bits.source := io.grantSource
  io.grantReady := dut.io.mem_grant.ready
  io.expectedBeats := refillCycles.U
  io.evictComplete := dut.io.evict_complete
}

class WritebackEntryEccTest extends AnyFlatSpec with ChiselSim {
  private def config = {
    val base = new DefaultConfig
    base.alterPartial({ case XSCoreParamsKey => base(XSTileKey).head.copy() }).alterPartial({
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    })
  }

  private def init(d: WritebackEntryEccHarness): Unit = {
    d.io.req.valid.poke(false.B)
    d.io.req.bits.poke(0.U.asTypeOf(d.io.req.bits))
    d.io.reqData.data.poke(0.U)
    d.io.primaryValid.poke(false.B)
    d.io.release.ready.poke(true.B)
    d.io.grantValid.poke(false.B)
    d.io.grantSource.poke(9.U)
    d.reset.poke(true.B)
    d.clock.step(2)
    d.reset.poke(false.B)
    d.clock.step()
  }

  private def driveReq(d: WritebackEntryEccHarness, voluntary: Boolean, hasData: Boolean, local: Boolean): Unit = {
    d.io.req.bits.addr.poke("h80004000".U)
    d.io.req.bits.param.poke(0.U)
    d.io.req.bits.voluntary.poke(voluntary.B)
    d.io.req.bits.hasData.poke(hasData.B)
    d.io.req.bits.corrupt.poke(true.B)
    d.io.req.bits.dirty.poke(voluntary.B)
    d.io.req.bits.local_evict.poke(local.B)
    d.io.req.bits.local_evict_tag.poke(true.B)
    d.io.req.bits.delay_release.poke(false.B)
    d.io.req.bits.miss_id.poke(0.U)
    d.io.reqData.data.poke("h112233445566778899aabbccddeeff00".U)
    d.io.primaryValid.poke(true.B)
    d.io.req.valid.poke(true.B)
    assert(d.io.req.ready.peek().litToBoolean, "WritebackEntry must accept an idle request")
    d.clock.step()
    d.io.req.valid.poke(false.B)
    d.io.primaryValid.poke(false.B)
  }

  behavior of "WritebackEntry ECC protocol"

  it should "emit corrupt ProbeAckData beats with backpressure" in {
    implicit val p = config
    simulate(new WritebackEntryEccHarness) { d =>
      init(d)
      driveReq(d, voluntary = false, hasData = true, local = false)
      d.io.release.ready.poke(false.B)
      d.clock.step(2)
      assert(d.io.release.valid.peek().litToBoolean, "probe response must remain valid under C backpressure")
      assert(d.io.release.bits.corrupt.peek().litToBoolean)
      d.io.release.ready.poke(true.B)
      var beats = 0
      for (_ <- 0 until 8) {
        if (d.io.release.valid.peek().litToBoolean) beats += 1
        d.clock.step()
      }
      val expected = d.io.expectedBeats.peek().litValue.toInt
      assert(beats == expected, s"expected $expected ProbeAckData beats, got $beats")
      assert(!d.io.evictComplete.valid.peek().litToBoolean)
    }
  }

  it should "complete a dirty local evict only after matching ReleaseAck" in {
    implicit val p = config
    simulate(new WritebackEntryEccHarness) { d =>
      init(d)
      driveReq(d, voluntary = true, hasData = true, local = true)
      var beats = 0
      for (_ <- 0 until 8) {
        if (d.io.release.valid.peek().litToBoolean) beats += 1
        d.clock.step()
      }
      val expected = d.io.expectedBeats.peek().litValue.toInt
      assert(beats == expected, s"expected $expected ReleaseData beats, got $beats")
      assert(d.io.grantReady.peek().litToBoolean, "entry must wait for ReleaseAck")
      d.io.grantValid.poke(true.B)
      d.io.grantSource.poke(9.U)
      assert(d.io.grantReady.peek().litToBoolean)
      assert(d.io.evictComplete.valid.peek().litToBoolean, "matching ReleaseAck must complete local evict")
      assert(d.io.evictComplete.bits.addr.peek().litValue == BigInt("80004000", 16))
      assert(d.io.evictComplete.bits.tag.peek().litToBoolean)
      d.clock.step()
      d.io.grantValid.poke(false.B)
    }
  }
}
