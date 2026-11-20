// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import chisel3.experimental.UnlocatableSourceInfo
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.SDBPConfig
import utility.{MemReqSource, ReqSourceField, ReqSourceKey}
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache.{DCacheModule, MainPipeReq, MissQueuePC, MissReqPipeRegBundle}
import xiangshan.mem.L1PrefetchReq
import xscache.coupledL2.{PCField, PCKey, PCValidField, PCValidKey, SDBPSource, SDBPSourceField, SDBPSourceKey}

class MissQueuePCHarness extends Module {
  val io = IO(new Bundle {
    val valid = Input(Vec(4, Bool()))
    val pc = Input(Vec(4, UInt(50.W)))
    val selected = Output(Valid(UInt(50.W)))
    val canonical = Output(UInt(64.W))
  })
  val (valid, pc) = MissQueuePC.select(io.valid.toSeq, io.pc.toSeq)
  io.selected.valid := valid
  io.selected.bits := pc
  io.canonical := MissQueuePC.canonical(pc, 64)
}

class MissQueuePrefetchAcquireHarness(implicit p: Parameters) extends DCacheModule {
  private val clients = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "dcache", sourceId = IdRange(0, cfg.nMissEntries), supportsProbe = TransferSizes(64))),
    requestFields = Seq(PCField(64), PCValidField(), ReqSourceField(), SDBPSourceField()))
  private val managers = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)), regionType = RegionType.TRACKED,
    supportsAcquireB = TransferSizes(64), supportsAcquireT = TransferSizes(64))),
    beatBytes = 32, endSinkId = 4, requestKeys = Seq(PCKey, PCValidKey, ReqSourceKey, SDBPSourceKey))
  private val edge = new TLEdgeOut(clients, managers, p, UnlocatableSourceInfo)
  val io = IO(new Bundle {
    val triggerPC = Input(Valid(UInt(VAddrBits.W)))
    val demandPC = Input(Valid(UInt(VAddrBits.W)))
    val mergedDemand = Input(Bool())
    val acquirePC = Output(Valid(UInt(64.W)))
    val source = Output(UInt(MemReqSource.reqSourceBits.W))
    val sdbpSource = Output(SDBPSource())
  })
  val prefetch = WireInit(0.U.asTypeOf(new L1PrefetchReq))
  prefetch.triggerPC := io.triggerPC
  prefetch.pf_source.value := L1_HW_PREFETCH_STRIDE
  val mainPipe = (new MainPipeReq).convertPrefetchReq(prefetch)
  val pipe = WireInit(0.U.asTypeOf(new MissReqPipeRegBundle(edge)))
  pipe.req.source := mainPipe.source
  pipe.req.cmd := mainPipe.cmd
  pipe.req.pf_source := mainPipe.pf_source
  pipe.req.prefetchPC := mainPipe.prefetchPC
  pipe.req.pc := io.demandPC.bits
  pipe.req.pcValid := io.demandPC.valid
  pipe.mergedDemand := io.mergedDemand
  val acquire = pipe.get_acquire(false.B)
  io.acquirePC.valid := acquire.user(PCValidKey)
  io.acquirePC.bits := acquire.user(PCKey)
  io.source := acquire.user(ReqSourceKey)
  io.sdbpSource := acquire.user(SDBPSourceKey)
}

class SDBPMissQueuePCTest extends AnyFlatSpec {
  behavior of "MissQueue demand PC selection"
  it should "retain the first valid PC across compressed and same-cycle merges, including PC zero" in {
    simulate(new MissQueuePCHarness) { dut =>
      for (mask <- 0 until 16) {
        for (i <- 0 until 4) {
          dut.io.valid(i).poke(((mask & (1 << i)) != 0).B)
          dut.io.pc(i).poke((if (i == 0) 0 else 0x80000000L + i).U)
        }
        dut.io.selected.valid.expect((mask != 0).B)
        if (mask != 0) {
          val first = (0 until 4).find(i => (mask & (1 << i)) != 0).get
          dut.io.selected.bits.expect((if (first == 0) 0 else 0x80000000L + first).U)
        }
      }
      dut.io.valid.foreach(_.poke(false.B))
      dut.io.valid(0).poke(true.B)
      dut.io.pc(0).poke(((BigInt(1) << 50) - 1).U)
      dut.io.canonical.expect(((BigInt(1) << 64) - 1).U)
    }
  }

  it should "keep a prefetch trigger until an accepted demand takes over the Acquire" in {
    val config = new SDBPConfig
    implicit val p: Parameters = config.alterPartial {
      case XSCoreParamsKey => config(XSTileKey).head
    }
    simulate(new MissQueuePrefetchAcquireHarness) { dut =>
      dut.io.triggerPC.valid.poke(true.B)
      dut.io.triggerPC.bits.poke(0.U)
      dut.io.demandPC.bits.poke(0x8100.U)
      dut.io.demandPC.valid.poke(false.B)
      dut.io.mergedDemand.poke(false.B)
      dut.io.source.expect(MemReqSource.L1DataPrefetch.id.U)
      dut.io.sdbpSource.expect(SDBPSource.L1Stride)
      dut.io.acquirePC.valid.expect(true.B)
      dut.io.acquirePC.bits.expect(0.U)

      dut.io.mergedDemand.poke(true.B)
      dut.io.source.expect(MemReqSource.CPULoadData.id.U)
      dut.io.sdbpSource.expect(SDBPSource.Demand)
      dut.io.acquirePC.valid.expect(false.B)
      dut.io.demandPC.valid.poke(true.B)
      dut.io.acquirePC.valid.expect(true.B)
      dut.io.acquirePC.bits.expect(0x8100.U)
    }
  }
}
