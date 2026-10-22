package cache

import chisel3._
import chisel3.simulator.ChiselSim
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan._
import xiangshan.cache.mmu._

class PtwMptTransitionHarness(parallel: Boolean)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new Bundle {
    val request = Input(Bool())
    val enable = Input(Bool())
    val flush = Input(Bool())
    val acceptMemory = Input(Bool())
    val memoryResponse = Input(Bool())
    val acceptRecheck = Input(Bool())
    val ready = Output(Bool())
    val mptRequest = Output(Bool())
    val memoryRequest = Output(Bool())
    val response = Output(Bool())
    val recheck = Output(Bool())
    val recheckVpn = Output(UInt(36.W))
    val recheckSource = Output(UInt(2.W))
  })
  val ptw = Module(new PTW(parallel = parallel))
  ptw.io.req.valid := io.request
  ptw.io.req.bits := 0.U.asTypeOf(ptw.io.req.bits)
  ptw.io.req.bits.req_info.vpn := "h40000".U
  ptw.io.req.bits.req_info.source := 1.U
  ptw.io.req.bits.req_info.s2xlate := noS2xlate
  ptw.io.req.bits.l2Hit := true.B
  ptw.io.req.bits.ppn := "h80080".U
  ptw.io.csr := 0.U.asTypeOf(ptw.io.csr)
  ptw.io.csr.satp.mode := 8.U
  ptw.io.csr.mmpt.mode := io.enable.asUInt
  ptw.io.sfence := 0.U.asTypeOf(ptw.io.sfence)
  ptw.io.sfence.valid := io.flush
  ptw.io.resp.ready := true.B
  ptw.io.llptw.ready := io.acceptRecheck
  ptw.io.mem.mask := false.B
  ptw.io.mem.req.ready := io.acceptMemory
  ptw.io.mem.resp.valid := io.memoryResponse
  ptw.io.mem.resp.bits := ((BigInt("100000", 16) << 10) | 0xcf).U
  ptw.io.pmp.resp := 0.U.asTypeOf(ptw.io.pmp.resp)
  ptw.io.hptw.req.ready := false.B
  ptw.io.hptw.resp.valid := false.B
  ptw.io.hptw.resp.bits := 0.U.asTypeOf(ptw.io.hptw.resp.bits)
  ptw.io.mptCheck.get.req.ready := false.B
  ptw.io.mptCheck.get.resp.valid := false.B
  ptw.io.mptCheck.get.resp.bits := 0.U.asTypeOf(ptw.io.mptCheck.get.resp.bits)
  io.ready := ptw.io.req.ready
  io.mptRequest := ptw.io.mptCheck.get.req.valid
  io.memoryRequest := ptw.io.mem.req.valid
  io.response := ptw.io.resp.valid
  io.recheck := ptw.io.llptw.valid
  io.recheckVpn := ptw.io.llptw.bits.req_info.vpn
  io.recheckSource := ptw.io.llptw.bits.req_info.source
  if (parallel) assert(!ptw.io.mptCheck.get.req.valid, "SECOND_MPT_UNCONNECTED")
}

object PtwMptTransitionTest extends App with ChiselSim {
  val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head.copy(HasBitmapCheck = false, HasMptCheck = true)
    case DebugOptionsKey => base(DebugOptionsKey).copy(EnableDifftest = false,
      AlwaysBasicDiff = false, EnablePerfDebug = false)
    case DFTOptionsKey => base(DFTOptionsKey).copy(EnableMbist = false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, enableXMR = false)
  }
  ChiselDB.init(false)
  Constantin.init(false)
  for (parallel <- Seq(true, false)) simulate(new PtwMptTransitionHarness(parallel)) { dut =>
    dut.io.request.poke(false.B)
    dut.io.enable.poke(false.B)
    dut.io.flush.poke(false.B)
    dut.io.acceptMemory.poke(false.B)
    dut.io.memoryResponse.poke(false.B)
    dut.io.acceptRecheck.poke(false.B)
    dut.clock.step(8)
    def until(condition: => Boolean): Unit = {
      var remaining = 32
      while (!condition && remaining > 0) { dut.clock.step(); remaining -= 1 }
      assert(condition, "walker did not make progress")
    }
    def send(): Unit = {
      dut.io.ready.expect(true.B)
      dut.io.request.poke(true.B)
      dut.clock.step()
      dut.io.request.poke(false.B)
    }
    def holdRecheck(): Unit = for (_ <- 0 until 3) {
      dut.io.recheck.expect(true.B)
      dut.io.recheckVpn.expect("h40000".U)
      dut.io.recheckSource.expect(1.U)
      dut.io.response.expect(false.B)
      dut.io.memoryRequest.expect(false.B)
      dut.clock.step()
    }
    def acceptRecheck(): Unit = {
      dut.io.acceptRecheck.poke(true.B)
      dut.clock.step()
      dut.io.acceptRecheck.poke(false.B)
      dut.io.ready.expect(true.B)
      dut.io.recheck.expect(false.B)
    }

    // MODE changes with unchanged SDID, so mmpt.changed remains deasserted.
    send()
    dut.io.enable.poke(true.B)
    if (parallel) {
      until(dut.io.recheck.peek().litToBoolean)
      holdRecheck()
      dut.io.enable.poke(false.B)
      holdRecheck()
      acceptRecheck()

      // An already accepted read must drain before its VPN can be rechecked.
      send()
      until(dut.io.memoryRequest.peek().litToBoolean)
      dut.io.acceptMemory.poke(true.B)
      dut.clock.step()
      dut.io.acceptMemory.poke(false.B)
      dut.io.enable.poke(true.B)
      for (_ <- 0 until 3) {
        dut.io.recheck.expect(false.B)
        dut.io.response.expect(false.B)
        dut.io.memoryRequest.expect(false.B)
        dut.clock.step()
      }
      dut.io.memoryResponse.poke(true.B)
      dut.clock.step()
      dut.io.memoryResponse.poke(false.B)
      until(dut.io.recheck.peek().litToBoolean)
      holdRecheck()
      dut.io.enable.poke(false.B)
      holdRecheck()
      acceptRecheck()

      // Flush cancels a held recheck and clears its replay state.
      send()
      dut.io.enable.poke(true.B)
      until(dut.io.recheck.peek().litToBoolean)
      dut.io.enable.poke(false.B)
      dut.io.flush.poke(true.B)
      dut.clock.step()
      dut.io.flush.poke(false.B)
      dut.io.ready.expect(true.B)
      dut.io.recheck.expect(false.B)

      // Reusing the lane after replay/flush still completes a normal walk.
      send()
      until(dut.io.memoryRequest.peek().litToBoolean)
      dut.io.acceptMemory.poke(true.B)
      dut.clock.step()
      dut.io.acceptMemory.poke(false.B)
      dut.io.memoryResponse.poke(true.B)
      dut.clock.step()
      dut.io.memoryResponse.poke(false.B)
      until(dut.io.response.peek().litToBoolean)
      dut.clock.step()
      dut.io.ready.expect(true.B)
      println("MPT_PARALLEL_TRANSITION_PASS")
    } else {
      until(dut.io.mptRequest.peek().litToBoolean)
      dut.io.memoryRequest.expect(false.B)
      dut.io.recheck.expect(false.B)
      dut.io.response.expect(false.B)
      println("MPT_PRIMARY_CHECK_PRESERVED_PASS")
    }
  }
}
