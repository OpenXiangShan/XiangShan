// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// XiangShan is licensed under Mulan PSL v2.

package xiangshan.cache.mmu

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSBundle, XSCoreParamsKey, XSTileKey, XSModule}

import scala.collection.mutable

class LLPTWTestIO(implicit p: Parameters) extends XSBundle with HasPtwConst {
  val flush = Input(Bool())
  val inValid = Input(Bool())
  val inVpn = Input(UInt(vpnLen.W))
  val inStage = Input(UInt(2.W))
  val memReady = Input(Bool())
  val memRespValid = Input(Bool())
  val memRespId = Input(UInt(log2Ceil(l2tlbParams.llptwsize).W))
  val memRespValue = Input(UInt(blockBits.W))
  val hptwRespValid = Input(Bool())
  val hptwRespId = Input(UInt(log2Ceil(l2tlbParams.llptwsize).W))
  val hptwRespTag = Input(UInt(gvpnLen.W))
  val hptwRespPpn = Input(UInt(ppnLen.W))
  val hptwRespGpf = Input(Bool())
  val hptwRespGaf = Input(Bool())
  val hptwRespRead = Input(Bool())

  val inputFire = Output(Bool())
  val memReqFire = Output(Bool())
  val memReqValid = Output(Bool())
  val memReqId = Output(UInt(bMemID.W))
  val hptwReqFire = Output(Bool())
  val hptwReqId = Output(UInt(log2Ceil(l2tlbParams.llptwsize).W))
  val hptwReqVpn = Output(UInt(ptePPNLen.W))
  val outValid = Output(Bool())
  val outId = Output(UInt(bMemID.W))
  val outVpn = Output(UInt(vpnLen.W))
  val outTag = Output(UInt(gvpnLen.W))
  val outPpn = Output(UInt(ppnLen.W))
  val outGpf = Output(Bool())
  val outGaf = Output(Bool())
  val outFirstFault = Output(Bool())
}

class LLPTWTestHarness(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new LLPTWTestIO)
  val llptw = Module(new LLPTW)
  require(!HasMptCheck)

  llptw.io.sfence := 0.U.asTypeOf(llptw.io.sfence)
  llptw.io.sfence.valid := io.flush
  llptw.io.csr := 0.U.asTypeOf(llptw.io.csr)
  llptw.io.csr.hgatp.mode := Sv39x4
  llptw.io.in.valid := io.inValid
  llptw.io.in.bits := 0.U.asTypeOf(llptw.io.in.bits)
  llptw.io.in.bits.req_info.vpn := io.inVpn
  llptw.io.in.bits.req_info.s2xlate := io.inStage
  llptw.io.in.bits.ppn := "h456".U
  llptw.io.out.ready := true.B
  llptw.io.cache.ready := true.B
  llptw.io.mem.req.ready := io.memReady
  llptw.io.mem.resp.valid := io.memRespValid
  llptw.io.mem.resp.bits.id := io.memRespId
  llptw.io.mem.resp.bits.value := io.memRespValue
  llptw.io.mem.req_mask := VecInit.fill(l2tlbParams.llptwsize)(false.B)
  llptw.io.mem.flush_latch := VecInit.fill(l2tlbParams.llptwsize)(false.B)
  llptw.io.hptw.req.ready := true.B
  llptw.io.hptw.resp.valid := io.hptwRespValid
  llptw.io.hptw.resp.bits := 0.U.asTypeOf(llptw.io.hptw.resp.bits)
  llptw.io.hptw.resp.bits.id := io.hptwRespId
  llptw.io.hptw.resp.bits.h_resp.entry.tag := io.hptwRespTag
  llptw.io.hptw.resp.bits.h_resp.entry.ppn := io.hptwRespPpn
  llptw.io.hptw.resp.bits.h_resp.entry.v := true.B
  llptw.io.hptw.resp.bits.h_resp.entry.perm.get.r := io.hptwRespRead
  llptw.io.hptw.resp.bits.h_resp.entry.perm.get.x := !io.hptwRespRead
  llptw.io.hptw.resp.bits.h_resp.entry.perm.get.a := true.B
  llptw.io.hptw.resp.bits.h_resp.entry.perm.get.u := true.B
  llptw.io.hptw.resp.bits.h_resp.gpf := io.hptwRespGpf
  llptw.io.hptw.resp.bits.h_resp.gaf := io.hptwRespGaf
  llptw.io.pmp.foreach(pmp => pmp.resp := 0.U.asTypeOf(pmp.resp))
  llptw.io.l0_way_info.foreach(_ := 0.U)
  llptw.io.bitmap.foreach { bitmap =>
    bitmap.req.ready := true.B
    bitmap.resp.valid := false.B
    bitmap.resp.bits := 0.U.asTypeOf(bitmap.resp.bits)
  }

  io.inputFire := llptw.io.in.fire
  io.memReqFire := llptw.io.mem.req.fire
  io.memReqValid := llptw.io.mem.req.valid
  io.memReqId := llptw.io.mem.req.bits.id
  io.hptwReqFire := llptw.io.hptw.req.fire
  io.hptwReqId := llptw.io.hptw.req.bits.id
  io.hptwReqVpn := llptw.io.hptw.req.bits.gvpn
  io.outValid := llptw.io.out.valid
  io.outId := llptw.io.out.bits.id
  io.outVpn := llptw.io.out.bits.req_info.vpn
  io.outTag := llptw.io.out.bits.h_resp.entry.tag
  io.outPpn := llptw.io.out.bits.h_resp.entry.ppn
  io.outGpf := llptw.io.out.bits.h_resp.gpf
  io.outGaf := llptw.io.out.bits.h_resp.gaf
  io.outFirstFault := llptw.io.out.bits.first_s2xlate_fault
}

class LLPTWDuplicateRequestTest extends AnyFlatSpec with ChiselSim {
  behavior of "LLPTW duplicate memory requests"

  it should "complete overlapping walks without issuing duplicate memory transactions" in {
    val defaultConfig = new DefaultConfig
    implicit val config: Parameters = defaultConfig.alterPartial {
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy()
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
    }
    val allStage = 3
    val onlyStage1 = 1
    val firstVpn = BigInt(0x12340)
    val secondVpn = firstVpn + 1
    val tablePpn = BigInt(0x456)
    val leafPpn = BigInt(0x80000)
    val leafFlags = BigInt(0xd3) // D, A, U, R and V.
    val sectorEntries = 8
    val pteBits = 64
    val ptePpnShift = 10
    val ptes = (0 until sectorEntries).map { i =>
      (((leafPpn + i) << ptePpnShift) | leafFlags) << (i * pteBits)
    }.reduce(_ | _)
    def translatedPpn(vpn: BigInt): BigInt = BigInt(0x90000) + vpn

    case class Scenario(
      name: String,
      responseOffset: Int = 0,
      stallCycles: Int = 0,
      vpn: BigInt = secondVpn,
      firstStage: Int = allStage,
      fault: String = "",
      flush: Boolean = false
    )
    val scenarios = Seq(
      Scenario("same cycle"),
      Scenario("response before memory", responseOffset = -1),
      Scenario("response after memory", responseOffset = 1),
      Scenario("memory backpressure", responseOffset = -1, stallCycles = 2),
      Scenario("different sector", vpn = firstVpn + sectorEntries),
      Scenario("different stage", firstStage = onlyStage1),
      Scenario("guest page fault", fault = "gpf"),
      Scenario("guest access fault", fault = "gaf"),
      Scenario("read permission fault", fault = "read"),
      Scenario("flush overlap", flush = true)
    )

    simulate(new LLPTWTestHarness) { dut =>
      for (scenario <- scenarios) {
        withClue(scenario.name + ": ") {
          val memRequests = mutable.ArrayBuffer.empty[BigInt]
          val hptwRequests = mutable.Queue.empty[(BigInt, BigInt)]
          val completed = mutable.ArrayBuffer.empty[BigInt]

          def tick(): Unit = {
            if (dut.io.memReqFire.peek().litToBoolean) memRequests += dut.io.memReqId.peek().litValue
            if (dut.io.hptwReqFire.peek().litToBoolean) {
              hptwRequests.enqueue((dut.io.hptwReqId.peek().litValue, dut.io.hptwReqVpn.peek().litValue))
            }
            if (dut.io.outValid.peek().litToBoolean) {
              val id = dut.io.outId.peek().litValue
              assert(id == 0 || id == 1)
              val vpn = if (id == 0) firstVpn else scenario.vpn
              dut.io.outVpn.expect(vpn.U)
              val hasFault = id == 1 && scenario.fault.nonEmpty
              dut.io.outFirstFault.expect(hasFault.B)
              dut.io.outGpf.expect((hasFault && scenario.fault != "gaf").B)
              dut.io.outGaf.expect((hasFault && scenario.fault == "gaf").B)
              if (!hasFault && (id == 1 || scenario.firstStage == allStage)) {
                val expectedTag = leafPpn + (vpn % sectorEntries)
                dut.io.outTag.expect(expectedTag.U)
                dut.io.outPpn.expect(translatedPpn(expectedTag).U)
              }
              completed += id
            }
            dut.clock.step()
          }

          def respond(request: (BigInt, BigInt), fault: String = ""): Unit = {
            dut.io.hptwRespValid.poke(true.B)
            dut.io.hptwRespId.poke(request._1.U)
            dut.io.hptwRespTag.poke(request._2.U)
            dut.io.hptwRespPpn.poke(translatedPpn(request._2).U)
            dut.io.hptwRespGpf.poke((fault == "gpf").B)
            dut.io.hptwRespGaf.poke((fault == "gaf").B)
            dut.io.hptwRespRead.poke((fault != "read").B)
          }

          dut.reset.poke(true.B)
          dut.io.flush.poke(false.B)
          dut.io.inValid.poke(false.B)
          dut.io.inVpn.poke(0.U)
          dut.io.inStage.poke(allStage.U)
          dut.io.memReady.poke(false.B)
          dut.io.memRespValid.poke(false.B)
          dut.io.memRespId.poke(0.U)
          dut.io.memRespValue.poke(ptes.U)
          dut.io.hptwRespValid.poke(false.B)
          dut.io.hptwRespId.poke(0.U)
          dut.io.hptwRespTag.poke(tablePpn.U)
          dut.io.hptwRespPpn.poke(0.U)
          dut.io.hptwRespGpf.poke(false.B)
          dut.io.hptwRespGaf.poke(false.B)
          dut.io.hptwRespRead.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)

          dut.io.inValid.poke(true.B)
          dut.io.inVpn.poke(firstVpn.U)
          dut.io.inStage.poke(scenario.firstStage.U)
          dut.io.inputFire.expect(true.B)
          tick()
          dut.io.inVpn.poke(scenario.vpn.U)
          dut.io.inStage.poke(allStage.U)
          dut.io.inputFire.expect(true.B)
          tick()
          dut.io.inValid.poke(false.B)
          if (scenario.firstStage == allStage) {
            assert(hptwRequests.dequeue() == ((BigInt(0), tablePpn)))
            respond((BigInt(0), tablePpn))
            tick()
            dut.io.hptwRespValid.poke(false.B)
          }
          tick()
          assert(hptwRequests.dequeue() == ((BigInt(1), tablePpn)))
          dut.io.memReqValid.expect(true.B)
          dut.io.memReqId.expect(0.U)
          assert(memRequests.isEmpty)

          if (scenario.responseOffset > 0) {
            dut.io.memReady.poke(true.B)
            dut.io.memReqFire.expect(true.B)
            tick()
          }
          dut.io.memReady.poke((scenario.responseOffset >= 0).B)
          dut.io.flush.poke(scenario.flush.B)
          respond((BigInt(1), tablePpn), scenario.fault)
          if (scenario.responseOffset == 0 && !scenario.flush) dut.io.memReqFire.expect(true.B)
          tick()
          dut.io.hptwRespValid.poke(false.B)
          dut.io.flush.poke(false.B)
          for (_ <- 0 until scenario.stallCycles) tick()
          dut.io.memReady.poke(true.B)
          for (_ <- 0 until 4) tick()

          val separate = scenario.vpn / sectorEntries != firstVpn / sectorEntries ||
            scenario.firstStage != allStage
          val expectedMemRequests = if (scenario.flush) 0 else if (separate) 2 else 1
          assert(memRequests.size == expectedMemRequests, s"memory requests: $memRequests")
          assert(memRequests.distinct.size == memRequests.size)
          dut.io.memReqValid.expect(false.B)

          if (!scenario.flush) {
            for (id <- memRequests.toSeq) {
              dut.io.memRespValid.poke(true.B)
              dut.io.memRespId.poke(id.U)
              tick()
            }
            dut.io.memRespValid.poke(false.B)
            var cycles = 0
            while (completed.size < 2 && cycles < 20) {
              dut.io.hptwRespValid.poke(false.B)
              if (hptwRequests.nonEmpty) respond(hptwRequests.dequeue())
              tick()
              cycles += 1
            }
            dut.io.hptwRespValid.poke(false.B)
            assert(completed.sorted.toSeq == Seq(BigInt(0), BigInt(1)))
            assert(hptwRequests.isEmpty)
          } else {
            assert(completed.isEmpty)
          }
          for (_ <- 0 until 3) tick()
          assert(memRequests.size == expectedMemRequests)
          assert(completed.size == (if (scenario.flush) 0 else 2))
        }
      }
    }
  }
}
