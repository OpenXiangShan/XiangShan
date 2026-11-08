package xiangshan.mem

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import system.{CVMParamsKey, SoCParamsKey}
import top.KunminghuV2WithoutLLCConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.backend.fu.{PMP, PMPChecker, PMPRespBundle, PMPXSModule}
import xiangshan.cache.mmu.TlbCmd

private class SoCPmaHarness(implicit p: Parameters) extends PMPXSModule {
  val io = IO(new Bundle {
    val address = Input(UInt(PAddrBits.W))
    val command = Input(UInt(3.W))
    val result = Output(new PMPRespBundle)
  })
  val entries = Module(new PMP)
  entries.io.distribute_csr.w.valid := false.B
  entries.io.distribute_csr.w.bits := 0.U.asTypeOf(entries.io.distribute_csr.w.bits)
  val checker = Module(new PMPChecker(lgMaxSize = 3, sameCycle = true))
  checker.io.check_env.keyIDen := false.B // KEYIDEN reset value: check the complete PA.
  checker.io.check_env.cmode := false.B
  checker.io.check_env.mode := 3.U // PMA must also apply to M-mode accesses.
  checker.io.check_env.debug := false.B
  checker.io.check_env.pmp := entries.io.pmp
  checker.io.check_env.pma := entries.io.pma
  checker.io.req.valid := true.B
  checker.io.req.bits.addr := io.address
  checker.io.req.bits.size := 3.U
  checker.io.req.bits.cmd := io.command
  io.result := checker.io.resp
}

class SoCPmaTest extends AnyFlatSpec with ChiselScalatestTester {
  private val config = new KunminghuV2WithoutLLCConfig
  implicit private val p: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }

  behavior of "The 48-bit CPU's 37-bit SoC PMA map"

  it should "retain the upstream address widths and select one KeyID bit" in {
    assert(config(SoCParamsKey).PAddrBits == 48)
    assert(config(CVMParamsKey).KeyIDBits == 1)
    assert(config(XSTileKey).head.HasBitmapCheck)
    assert(!config(XSTileKey).head.HasBitmapCheckDefault)
    assert(config(XSTileKey).head.l2tlbParameters.bcReservedBits == 1)
  }

  it should "deny high addresses and their low-address aliases without changing DDR or QSPI permissions" in {
    test(new SoCPmaHarness) { c =>
      c.io.address.poke(0.U)
      c.io.command.poke(TlbCmd.read)
      c.clock.step(2)
      def check(address: BigInt, denied: Boolean, cacheable: Option[Boolean] = None): Unit = {
        c.io.address.poke(address.U)
        for ((command, fault) <- Seq(TlbCmd.read -> c.io.result.ld, TlbCmd.write -> c.io.result.st, TlbCmd.exec -> c.io.result.instr)) {
          c.io.command.poke(command)
          fault.expect(denied.B)
          cacheable.foreach(value => c.io.result.mmio.expect((!value).B))
        }
      }
      val limit = BigInt(1) << 37
      for (address <- Seq(limit - 8, limit, limit + BigInt("80000000", 16),
          (BigInt(1) << 47) | BigInt("80000000", 16), (BigInt(1) << 48) - 8)) {
        check(address, denied = true)
      }
      for (address <- Seq(BigInt("80000000", 16), BigInt("87ffffff8", 16))) {
        check(address, denied = false, cacheable = Some(true))
        c.io.result.atomic.expect(true.B)
      }
      check(BigInt("880000000", 16), denied = true)
      check(BigInt("02200000", 16), denied = false, cacheable = Some(true))
      check(BigInt("023ffff8", 16), denied = false, cacheable = Some(true))
      check(BigInt("02400000", 16), denied = false, cacheable = Some(false))
    }
  }
}
