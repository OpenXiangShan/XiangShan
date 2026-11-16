// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE.
package xiangshan

import chisel3._
import circt.stage.ChiselStage
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.ArgParser
import xiangshan.backend.Bundles.{DecodeInUop, DecodeOutUop, EnqRobUop, RenameOutUop}
import xiangshan.frontend.FetchToIBuffer
import xiangshan.frontend.ibuffer.IBufEntry

class TeaProfilingConfigTest extends AnyFlatSpec with Matchers {
  behavior of "TEA/DIP build configuration"

  it should "reject profiling without debug metadata and accept either supported debug mode" in {
    for (feature <- Seq("--enable-tea", "--enable-dip", "--enable-tea-stats")) {
      val error = intercept[IllegalArgumentException] {
        ArgParser.parse(Array(feature))
      }
      error.getMessage should include ("requires --enable-difftest or --full-basicdiff")
      for (debug <- Seq("--enable-difftest", "--full-basicdiff")) {
        noException should be thrownBy ArgParser.parse(Array(feature, debug))
      }
    }
  }

  it should "elaborate metadata only for the requested samplers across every flag combination" in {
    for (mask <- 0 until 8) {
      val flags = Seq("--enable-tea", "--enable-dip", "--enable-tea-stats")
        .zipWithIndex.collect { case (flag, bit) if (mask & (1 << bit)) != 0 => flag }
      val (parsed, _, _) = ArgParser.parse((Seq("--enable-difftest") ++ flags).toArray)
      implicit val p: Parameters = parsed.alterPartial {
        case XSCoreParamsKey => parsed(XSTileKey).head
      }
      val backend = p(XSCoreParamsKey).backendParams
      backend.teaTrackerEn shouldBe ((mask & 1) != 0)
      backend.dipEn shouldBe ((mask & 2) != 0)
      backend.teaCycleStatsEn shouldBe ((mask & 4) != 0)
      val psvWidth = if ((mask & 3) != 0) TeaEvent.width else 0
      ChiselStage.emitCHIRRTL(new Module {
        val io = IO(Output(new Bundle {
          val fetch = new FetchToIBuffer
          val entry = new IBufEntry
          val decodeIn = new DecodeInUop
          val decodeOut = new DecodeOutUop
          val rename = new RenameOutUop
          val rob = new EnqRobUop
        }))
        io := 0.U.asTypeOf(io)
        Seq(io.fetch.teaPsv, io.entry.teaPsv, io.decodeIn.teaPsv, io.decodeOut.teaPsv,
          io.rename.teaPsv, io.rob.teaPsv).foreach(_.getWidth shouldBe psvWidth)
      })
    }
  }
}
