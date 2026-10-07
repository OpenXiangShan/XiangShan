package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.{DefaultConfig, WithPDB}
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}

class PDBCapacityMainTop(implicit p: Parameters) extends PBMainTestTop(backgroundMove = false) {
  val capacity = IO(new Bundle {
    val allocAddr, useAddr = Input(UInt(PAddrBits.W))
    val fillByte = Input(UInt(8.W))
    val use, pressure, usedMove = Input(Bool())
    val issue = Output(Valid(UInt(PAddrBits.W)))
    val done, victim = Output(Bool())
    val victimUsed = Output(Bool())
    val installedUsed, accessed = Output(Bool())
    val installedSource = Output(UInt(L1PfSourceBits.W))
  })
  pb.io.mshr.allocReq.bits.addr := capacity.allocAddr
  pb.io.mshr.allocReq.bits.vaddr := capacity.allocAddr
  pb.io.mshr.allocReq.bits.prefetchSource := 3.U
  pb.io.mshr.refillReq.bits.data := Fill(cfg.blockBytes, capacity.fillByte)
  pb.io.mshr.refillWait := capacity.pressure
  pb.io.dcache.usedMove := capacity.usedMove
  pb.io.load(0).s1_paddr.valid := capacity.use
  pb.io.load(0).s1_paddr.bits := capacity.useAddr
  pb.io.load(0).s2_use := pb.io.load(0).s2_dataResp.valid && pb.io.load(0).s2_dataResp.bits.hit
  capacity.issue.valid := pb.io.pipe.s0_moveReq.fire
  capacity.issue.bits := pb.io.pipe.s0_moveReq.bits.paddr
  capacity.done := pb.io.pipe.s3_moveDone.valid
  capacity.victim := pb.io.dcache.perf.capacityVictim.valid
  capacity.victimUsed := pb.io.dcache.perf.capacityVictim.bits.used
  capacity.installedUsed := mp.io.prefetchUseInstall.bits.used
  capacity.installedSource := mp.io.prefetch_flag_write.bits.source
  capacity.accessed := mp.io.access_flag_write.bits.flag
}

class PDBCapacityMainPipeTest extends AnyFlatSpec with ChiselSim {
  private val base = new WithPDB(4, "lru") ++ new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "Capacity move through the real MainPipe"

  for (abort <- Seq(false, true)) {
    it should s"transfer the chosen used line with a stalled dirty DCache victim${if (abort) " after an abort" else ""}" in {
      simulate(new PDBCapacityMainTop) { c =>
        c.io.alloc.poke(false.B); c.io.fill.poke(false.B)
        c.io.store.poke(false.B); c.io.probe.poke(false.B)
        c.io.corrupt.poke(false.B); c.io.btot.poke(false.B); c.io.otherStore.poke(false.B)
        c.io.dirty.poke(true.B); c.io.block.poke(abort.B)
        c.io.readReady.poke(true.B); c.io.commit.poke(true.B); c.io.wbStall.poke(true.B)
        c.capacity.use.poke(false.B); c.capacity.pressure.poke(false.B)
        c.capacity.usedMove.poke(true.B); c.capacity.allocAddr.poke(0.U)
        c.capacity.useAddr.poke(0.U); c.capacity.fillByte.poke(0.U)
        c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B); c.clock.step()
        val addr = BigInt("80001000", 16)
        for (i <- 0 until 4) {
          c.capacity.allocAddr.poke((addr + 64 * i).U)
          c.io.alloc.poke(true.B); c.clock.step(); c.io.alloc.poke(false.B)
          c.capacity.fillByte.poke((0x30 + i).U)
          c.io.fill.poke(true.B); c.clock.step(); c.io.fill.poke(false.B)
          c.capacity.useAddr.poke((addr + 64 * i).U)
          c.capacity.use.poke(true.B); c.clock.step(); c.capacity.use.poke(false.B); c.clock.step(2)
        }
        c.capacity.pressure.poke(true.B)
        var aborted = false
        var completed = 0
        var victims = 0
        var writes = 0
        var dirtyWritebacks = 0
        for (cycle <- 0 until 50) {
          if (c.io.abort.peek().litToBoolean) aborted = true
          if (cycle == 12) c.io.block.poke(false.B)
          if (cycle == 28) c.io.wbStall.poke(false.B)
          if (c.capacity.issue.valid.peek().litToBoolean) c.capacity.issue.bits.expect(addr.U)
          if (cycle < 28) {
            c.io.write.expect(false.B); c.capacity.done.expect(false.B)
            c.capacity.victim.expect(false.B)
          }
          if (c.io.write.peek().litToBoolean) {
            c.io.data.expect(BigInt("30" * 64, 16).U)
            c.io.coh.expect(ClientStates.Trunk)
            c.capacity.installedUsed.expect(true.B); c.capacity.accessed.expect(true.B)
            c.capacity.installedSource.expect(1.U) // CLEAR, consumed Stream lifetime.
            writes += 1
          }
          if (c.io.wb.peek().litToBoolean) {
            c.io.victim.expect(BigInt("a5" * 64, 16).U)
            c.io.wbData.expect(true.B); c.io.wbCorrupt.expect(false.B)
            dirtyWritebacks += 1
          }
          if (c.capacity.done.peek().litToBoolean) completed += 1
          if (c.capacity.victim.peek().litToBoolean) {
            c.capacity.victimUsed.expect(true.B); victims += 1
          }
          c.clock.step()
        }
        assert(aborted == abort)
        assert(completed == 1 && victims == 1 && writes == 1 && dirtyWritebacks == 1)
      }
    }
  }
}
