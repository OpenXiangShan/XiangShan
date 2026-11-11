// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import utility._
import xscache.chi.{CHIAddrWidthKey, CHIIssue}
import xscache.common.BankBitsKey
import xscache.coupledL2._
import xscache.coupledL2.prefetch._

class CachePrefetchGateHarness extends Module {
  val io = IO(new Bundle {
    val enabled = Input(Bool())
    val request = Flipped(Decoupled(UInt(16.W)))
    val output = Decoupled(UInt(16.W))
  })
  io.output <> CachePrefetchControl.gate(io.request, io.enabled)
}

class CachePrefetchControlTest extends AnyFlatSpec {
  behavior of "cache prefetch control"
  it should "drain disabled requests and preserve enabled requests under backpressure" in {
    simulate(new CachePrefetchGateHarness) { dut =>
      for (enabled <- Seq(false, true, false, true); valid <- Seq(false, true); ready <- Seq(false, true)) {
        dut.io.enabled.poke(enabled.B)
        dut.io.request.valid.poke(valid.B)
        dut.io.request.bits.poke(0x1234.U)
        dut.io.output.ready.poke(ready.B)
        dut.io.output.valid.expect((enabled && valid).B)
        dut.io.request.ready.expect((!enabled || ready).B)
        dut.io.output.bits.expect(0x1234.U)
        dut.clock.step()
      }
    }
  }
}

/** Small real L2 receiver plus the gate used by core prefetch paths.
  * Emit once and run the same binary with different Constantin files.
  */
class CachePrefetchRuntimeHarness extends Module {
  private val clients = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "dcache", sourceId = IdRange(0, 4), supportsProbe = TransferSizes(64))))
  private val managers = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)), regionType = RegionType.TRACKED,
    supportsAcquireB = TransferSizes(64), supportsAcquireT = TransferSizes(64),
    supportsGet = TransferSizes(1, 64))), beatBytes = 32, endSinkId = 4)
  private val base = Parameters.empty.alterPartial {
    case L2ParamKey => L2Param(sets = 8, ways = 4, enablePrefetch = false,
      prefetch = Seq(BOPParameters(), PrefetchReceiverParams(4)),
      clientCaches = Seq(L1Param(vaddrBitsOpt = Some(39), pcBitOpt = Some(48))),
      enablePerf = false, enableMonitor = false, enableRollingDB = false, elaboratedTopDown = false)
    case BankBitsKey => 0
    case SliceIdKey => 0
    case MaxHartIdBits => 1
    case CHIAddrWidthKey => 48
    case CHIIssue => "E.b"
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }
  private implicit val p: Parameters = base.alterPartial {
    case EdgeInKey => new TLEdgeIn(clients, managers, base, UnlocatableSourceInfo)
    case EdgeOutKey => new TLEdgeOut(clients, managers, base, UnlocatableSourceInfo)
  }
  val io = IO(new Bundle {
    val csrEnable = Input(Bool())
    val enabled = Output(Bool())
    val otherHartEnabled = Output(Bool())
    val coreRequest = Flipped(Decoupled(UInt(16.W)))
    val coreOutput = Decoupled(UInt(16.W))
    val receive = Input(Bool())
    val address = Input(UInt(48.W))
    val train = Input(Bool())
    val trainReady = Output(Bool())
    val tlbValid = Output(Bool())
    val l2Ready = Input(Bool())
    val l2Valid = Output(Bool())
    val l2Address = Output(UInt(48.W))
  })
  private val enabled = CachePrefetchControl.enabled(0, defaultEnable = false)
  io.enabled := enabled
  io.otherHartEnabled := CachePrefetchControl.enabled(1, defaultEnable = true)
  io.coreOutput <> CachePrefetchControl.gate(io.coreRequest, enabled)
  private val prefetcher = Module(new Prefetcher)
  prefetcher.hartId := 0.U
  prefetcher.pfCtrlFromCore := 0.U.asTypeOf(prefetcher.pfCtrlFromCore)
  prefetcher.pfCtrlFromCore.l2_pf_master_en := io.csrEnable
  prefetcher.pfCtrlFromCore.l2_pf_recv_en := io.csrEnable
  prefetcher.io.train.foreach { request =>
    request.valid := io.train
    request.bits := 0.U.asTypeOf(request.bits)
    request.bits.tag := io.address(47, 9)
    request.bits.set := io.address(8, 6)
    request.bits.vaddr.foreach(_ := io.address(38, 6))
  }
  prefetcher.io.resp.foreach { response =>
    response.valid := false.B
    response.bits := 0.U.asTypeOf(response.bits)
  }
  prefetcher.io.recv_addr.valid := io.receive
  prefetcher.io.recv_addr.bits.addr := io.address
  prefetcher.io.recv_addr.bits.pfSource := MemReqSource.Prefetch2L2Stream.id.U
  prefetcher.io.tlb_req.req.ready := true.B
  prefetcher.io.tlb_req.resp.valid := false.B
  prefetcher.io.tlb_req.resp.bits := 0.U.asTypeOf(prefetcher.io.tlb_req.resp.bits)
  prefetcher.io.tlb_req.pmp_resp := 0.U.asTypeOf(prefetcher.io.tlb_req.pmp_resp)
  io.trainReady := prefetcher.io.train.head.ready
  io.tlbValid := prefetcher.io.tlb_req.req.valid
  prefetcher.io.req.head.ready := io.l2Ready
  io.l2Valid := prefetcher.io.req.head.valid
  io.l2Address := prefetcher.io.req.head.bits.addr
}

object CachePrefetchRuntimeMain extends App {
  val output = java.nio.file.Paths.get(args(0))
  java.nio.file.Files.createDirectories(output)
  Constantin.init(args.lift(1).forall(_ != "static"))
  ChiselDB.init(false)
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(new CachePrefetchRuntimeHarness,
    args = Array("--target-dir", output.toString),
    firtoolOpts = Array("-O=release", "--disable-annotation-unknown"))
  java.nio.file.Files.write(output.resolve("constantin.cpp"),
    (Constantin.getInitCpp + Constantin.getTXT.linesIterator.map(line =>
      Constantin.getCpp(line.takeWhile(_ != ' '))).mkString)
      .getBytes(java.nio.charset.StandardCharsets.UTF_8))
}
