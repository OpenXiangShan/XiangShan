package xiangshan.cache.mmu

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

private class LLPTWCoexistHarness(implicit p: Parameters) extends Module {
  val dut = Module(new LLPTW)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val vpn = UInt(dut.io.in.bits.req_info.vpn.getWidth.W)
      val cached = Bool()
    }))
    val memReq = Decoupled(chiselTypeOf(dut.io.mem.req.bits))
    val memResp = Flipped(Valid(chiselTypeOf(dut.io.mem.resp.bits)))
    val bitmapReq = Decoupled(new bitmapReqBundle)
    val bitmapResp = Flipped(Decoupled(new bitmapRespBundle))
    val out = Decoupled(new Bundle {
      val vpn = UInt(dut.io.out.bits.req_info.vpn.getWidth.W)
      val cached = Bool()
    })
  })

  dut.io.csr := 0.U.asTypeOf(dut.io.csr)
  dut.io.csr.mbmc.BME := 1.U
  dut.io.sfence := 0.U.asTypeOf(dut.io.sfence)
  dut.io.in.valid := io.in.valid
  io.in.ready := dut.io.in.ready
  dut.io.in.bits := 0.U.asTypeOf(dut.io.in.bits)
  dut.io.in.bits.req_info.vpn := io.in.bits.vpn
  dut.io.in.bits.ppn := "h20".U
  dut.io.in.bits.bitmapCheck.get.jmp_bitmap_check := io.in.bits.cached
  // Different sectors have distinct leaf PPNs, so bitmap completions cannot
  // accidentally satisfy the test by sharing the first request's result.
  for (i <- 0 until 8) {
    dut.io.in.bits.bitmapCheck.get.ptes(i) := ((BigInt(0x100 + i) << 10) | 0xc7).U
  }
  io.memReq <> dut.io.mem.req
  dut.io.mem.resp <> io.memResp
  dut.io.mem.req_mask.foreach(_ := false.B)
  dut.io.mem.flush_latch.foreach(_ := false.B)
  dut.io.pmp.foreach(_.resp := 0.U.asTypeOf(dut.io.pmp.head.resp))
  dut.io.hptw.req.ready := true.B
  dut.io.hptw.resp := 0.U.asTypeOf(dut.io.hptw.resp)
  dut.io.cache.ready := true.B
  dut.io.l0_way_info.get := 1.U
  io.bitmapReq <> dut.io.bitmap.get.req
  dut.io.bitmap.get.resp <> io.bitmapResp
  io.out.valid := dut.io.out.valid
  dut.io.out.ready := io.out.ready
  io.out.bits.vpn := dut.io.out.bits.req_info.vpn
  io.out.bits.cached := dut.io.out.bits.bitmapCheck.get.jmp_bitmap_check
}

class LLPTWCoexistTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val defaults = new DefaultConfig
  private implicit val p: Parameters = defaults.alterPartial({
    case XSCoreParamsKey => defaults(XSTileKey).head
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.NORMAL, perfDBHartID = 0)
  })

  behavior of "LLPTW admission with coexisting same-line requests"

  for (cachedInput <- Seq(false, true); completedBitmap <- Seq(false, true)) {
    it should s"accept cached=$cachedInput with bitmap completed=$completedBitmap" in {
      test(new LLPTWCoexistHarness) { dut =>
        def await(hint: String)(ready: => Boolean): Unit = {
          var cycles = 0
          while (!ready && cycles < 20) {
            dut.clock.step()
            cycles += 1
          }
          withClue(hint) { ready shouldBe true }
        }
        def enqueue(vpn: Int, cached: Boolean): Unit = {
          dut.io.in.bits.vpn.poke(vpn.U)
          dut.io.in.bits.cached.poke(cached.B)
          dut.io.in.valid.poke(true.B)
          await("input admission") { dut.io.in.ready.peek().litToBoolean }
          dut.clock.step()
          dut.io.in.valid.poke(false.B)
        }
        def bitmapResponse(id: BigInt): Unit = {
          dut.io.bitmapResp.bits.id.poke(id.U)
          dut.io.bitmapResp.valid.poke(true.B)
          await("bitmap response") { dut.io.bitmapResp.ready.peek().litToBoolean }
          dut.clock.step()
          dut.io.bitmapResp.valid.poke(false.B)
        }

        dut.io.in.valid.poke(false.B)
        dut.io.in.bits.vpn.poke(0.U)
        dut.io.in.bits.cached.poke(false.B)
        dut.io.memReq.ready.poke(false.B)
        dut.io.memResp.valid.poke(false.B)
        dut.io.memResp.bits.id.poke(0.U)
        dut.io.memResp.bits.value.poke(0.U)
        dut.io.bitmapReq.ready.poke(false.B)
        dut.io.bitmapResp.valid.poke(false.B)
        dut.io.bitmapResp.bits.id.poke(0.U)
        dut.io.bitmapResp.bits.cf.poke(false.B)
        dut.io.bitmapResp.bits.cfs.foreach(_.poke(false.B))
        dut.io.out.ready.poke(false.B)
        dut.clock.step(2)

        // Keep the ordinary PTE read outstanding throughout the bitmap path.
        enqueue(0x100, cached = false)
        await("ordinary PTE request") { dut.io.memReq.valid.peek().litToBoolean }
        val memId = dut.io.memReq.bits.id.peek().litValue
        dut.io.memReq.ready.poke(true.B)
        dut.clock.step()
        dut.io.memReq.ready.poke(false.B)

        enqueue(0x107, cached = true)
        await("cached PTE bitmap request") { dut.io.bitmapReq.valid.peek().litToBoolean }
        val bitmapId = dut.io.bitmapReq.bits.id.peek().litValue
        dut.io.bitmapReq.ready.poke(true.B)
        dut.clock.step()
        dut.io.bitmapReq.ready.poke(false.B)
        if (completedBitmap) {
          bitmapResponse(bitmapId)
          dut.io.out.valid.expect(true.B)
          dut.io.out.bits.vpn.expect(0x107.U)
        }

        // The old assertion fails here for both ordinary and cached inputs,
        // with either a bitmap response waiter or a completed output present.
        enqueue(0x102, cachedInput)
        if (!completedBitmap) bitmapResponse(bitmapId)
        dut.io.out.valid.expect(true.B)
        dut.io.out.bits.vpn.expect(0x107.U)
        dut.io.out.ready.poke(true.B)
        dut.clock.step()
        dut.io.out.ready.poke(false.B)

        if (cachedInput) {
          await("third request bitmap check") { dut.io.bitmapReq.valid.peek().litToBoolean }
          dut.io.bitmapReq.bits.vpn.expect(0x102.U)
          dut.io.bitmapReq.bits.bmppn.expect(0x102.U)
          val thirdId = dut.io.bitmapReq.bits.id.peek().litValue
          dut.io.bitmapReq.ready.poke(true.B)
          dut.clock.step()
          dut.io.bitmapReq.ready.poke(false.B)
          bitmapResponse(thirdId)
          dut.io.out.valid.expect(true.B)
          dut.io.out.bits.vpn.expect(0x102.U)
          dut.io.out.bits.cached.expect(true.B)
          dut.io.out.ready.poke(true.B)
          dut.clock.step()
          dut.io.out.ready.poke(false.B)
        } else {
          // An ordinary input joins memId; it must neither complete from the
          // unrelated bitmap response nor issue another PTE memory request.
          dut.io.out.valid.expect(false.B)
          dut.io.memReq.valid.expect(false.B)
        }

        // Return invalid PTEs (PF), avoiding another bitmap check. Both
        // ordinary requests must complete exactly once after this response.
        dut.io.memResp.bits.id.poke(memId.U)
        dut.io.memResp.valid.poke(true.B)
        dut.clock.step()
        dut.io.memResp.valid.poke(false.B)
        val expected = if (cachedInput) Set(BigInt(0x100)) else Set(BigInt(0x100), BigInt(0x102))
        var seen = Set.empty[BigInt]
        dut.io.out.ready.poke(true.B)
        for (_ <- 0 until expected.size) {
          await("ordinary request completion") { dut.io.out.valid.peek().litToBoolean }
          val vpn = dut.io.out.bits.vpn.peek().litValue
          expected should contain(vpn)
          seen should not contain vpn
          seen += vpn
          dut.io.out.bits.cached.expect(false.B)
          dut.clock.step()
        }
        seen shouldBe expected
        dut.io.out.valid.expect(false.B)
        dut.io.memReq.valid.expect(false.B)
      }
    }
  }
}
